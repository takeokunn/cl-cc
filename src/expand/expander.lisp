(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expander — Logic Layer
;;;
;;; Data lives in expander-data.lisp; defstruct helpers in expander-defstruct.lisp.
;;; This file: helper functions, define-expander-for registration macro,
;;; all handler registrations, and the short table-driven compiler-macroexpand-all.
;;;
;;; Design: *expander-head-table* is a Prolog-style clause database.
;;; Each (define-expander-for HEAD (form) body...) adds one clause.
;;; compiler-macroexpand-all is the inference engine — ~15 lines.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; Ensure cl-cc/type package exists at compile time so that qualified symbols
;;; like cl-cc/type:looks-like-type-specifier-p can be read before type/ loads.
;;; The full defpackage in src/type/package.lisp will extend this stub later.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-cc/type)
    (defpackage :cl-cc/type
      (:use :cl)
      (:export #:looks-like-type-specifier-p
               #:parse-type-specifier
               #:+type-unknown+
               #:register-type-alias))))

;;; ── Helper functions ─────────────────────────────────────────────────────

(defun chain-comparison-op (op args)
  "Chain a comparison (OP a b c) → (AND (OP a b) (OP b c)).
Uses gensyms for intermediate values to avoid double evaluation.
(OP) → error, (OP a) → T, (OP a b) → pass through, (OP a b c ...) → AND chain."
  (case (length args)
    (0 (error "~A requires at least one argument" op))
    (1 t)
    (2 (list op (first args) (second args)))
    (t (let* ((temps (loop for i from 0 below (length args)
                           collect (gensym (format nil "CMP~D-" i))))
              (bindings (mapcar #'list temps args))
              (pairs (loop for (a b) on temps
                           while b
                           collect (list op a b))))
         `(let ,bindings (and ,@pairs))))))

(defun reduce-variadic-op (op args identity)
  "Reduce a variadic arithmetic form (OP arg...) to nested binary forms.
(OP) => IDENTITY, (OP a) => a, (OP a b) => (OP a b), (OP a b c ...) => (OP (OP a b) c) ..."
  (case (length args)
    (0 identity)
    (1 (first args))
    (2 (list op (first args) (second args)))
    (t (reduce (lambda (acc x) (list op acc x)) (cddr args)
               :initial-value (list op (first args) (second args))))))

(defun register-defclass-accessors (class-name slot-specs)
  "Register ACCESSOR → (CLASS-NAME . SLOT-NAME) in *accessor-slot-map*.
Called at expand time so later (setf (accessor obj) val) can be lowered
to (setf (slot-value ...)) without runtime lookup."
  (when (listp slot-specs)
    (dolist (spec slot-specs)
      (when (listp spec)
        (let ((accessor (getf (rest spec) :accessor)))
          (when accessor
            (setf (gethash accessor *accessor-slot-map*)
                  (cons class-name (first spec)))))))))

(defun expand-defclass-slot-spec (spec)
  "Expand only the :initform value inside a slot SPEC, leaving all other
keys (:accessor, :initarg, :reader, :writer, :type) untouched."
  (if (listp spec)
      (list* (first spec)
             (loop for (k v) on (rest spec) by #'cddr
                   append (list k (if (eq k :initform)
                                      (compiler-macroexpand-all v)
                                      v))))
      spec))

(defun expand-typed-defun-or-lambda (head name params rest-forms)
  "Strip type annotations from PARAMS, register the type signature, and
rebuild a plain DEFUN or LAMBDA form with check-type assertions.

HEAD is 'defun or 'lambda; NAME is the function name (nil for lambda).

Side effects:
  - For DEFUN, calls register-function-type with the resolved param/return types.

Handles return-type annotation: if the first element of REST-FORMS is a type
specifier symbol, it is treated as the declared return type and wrapped in
(the RETURN-TYPE (progn ...)) in the output body."
  (multiple-value-bind (plain-params type-alist)
      (strip-typed-params params)
    (let* ((has-return-type  (and rest-forms
                                  (symbolp (first rest-forms))
                                  (cl-cc/type:looks-like-type-specifier-p (first rest-forms))))
           (return-type-spec (when has-return-type (first rest-forms)))
           (body-forms       (if has-return-type (cdr rest-forms) rest-forms))
           (param-types      (mapcar (lambda (e)
                                       (cl-cc/type:parse-type-specifier (cdr e)))
                                     type-alist))
           (return-type      (if return-type-spec
                                 (cl-cc/type:parse-type-specifier return-type-spec)
                                 cl-cc/type:+type-unknown+))
           (typed-body       (if return-type-spec
                                 `((the ,return-type-spec (progn ,@body-forms)))
                                 body-forms))
           (checks           (loop for (pname . ptype) in type-alist
                                   collect `(check-type ,pname ,ptype)))
           (full-body        (append checks typed-body)))
      (when (eq head 'defun)
        (register-function-type name param-types return-type))
      (compiler-macroexpand-all
       (if (eq head 'defun)
           `(defun ,name ,plain-params ,@full-body)
           `(lambda ,plain-params ,@full-body))))))

(defun make-macro-expander (lambda-list body)
  "Build a macro expander function for a LAMBDA-LIST and BODY.
Quasiquotes in BODY are pre-expanded so the host eval can handle them.
When *macro-eval-fn* is our-eval (self-hosting mode), the returned
function is a host CL closure delegating to our-eval at each invocation."
  (let ((expanded-body (mapcar #'our-macroexpand-all body)))
    (if (eq *macro-eval-fn* #'eval)
        (let ((form-var (gensym "FORM"))
              (env-var  (gensym "ENV")))
          (eval `(lambda (,form-var ,env-var)
                   (declare (ignore ,env-var))
                   (let* ,(generate-lambda-bindings lambda-list form-var)
                     ,@expanded-body))))
        (lambda (form env)
          (declare (ignore env))
          (let ((form-var (gensym "FORM")))
            (funcall *macro-eval-fn*
                     `(let ((,form-var ',form))
                        (let* ,(generate-lambda-bindings lambda-list form-var)
                          ,@expanded-body))))))))

(defun expand-macrolet-form (bindings body)
  "Register local macro BINDINGS, expand BODY under them, then restore.
Returns the expanded BODY wrapped in PROGN."
  (let ((saved nil))
    (dolist (b bindings)
      (let ((name        (first b))
            (lambda-list (second b))
            (macro-body  (cddr b)))
        (push (cons name (lookup-macro name)) saved)
        (register-macro name (make-macro-expander lambda-list macro-body))))
    (let ((result (compiler-macroexpand-all (cons 'progn body))))
      (dolist (s saved)
        (if (cdr s)
            (register-macro (car s) (cdr s))
            (remhash (car s) (macro-env-table *macro-environment*))))
      result)))

(defun expand-symbol-macrolet-form (bindings body)
  "Register local symbol macro BINDINGS, expand BODY under them, then restore.
Each binding is (symbol expansion). Returns the expanded BODY wrapped in PROGN."
  (let ((saved nil))
    (dolist (b bindings)
      (let ((name (first b))
            (expansion (second b)))
        (push (cons name (gethash name *symbol-macro-table*)) saved)
        (setf (gethash name *symbol-macro-table*) expansion)))
    (let ((result (compiler-macroexpand-all (cons 'progn body))))
      (dolist (s saved)
        (if (cdr s)
            (setf (gethash (car s) *symbol-macro-table*) (cdr s))
            (remhash (car s) *symbol-macro-table*)))
      result)))

(defun expand-progn-with-eager-defmacro (subforms)
  "Expand each form in SUBFORMS, eagerly registering DEFMACRO forms so
later siblings can immediately use the new macro."
  (let ((out nil))
    (dolist (sub subforms)
      (let ((exp (compiler-macroexpand-all sub)))
        (when (and (consp exp) (eq (car exp) 'defmacro))
          (register-macro (second exp)
                          (make-macro-expander (third exp) (cdddr exp))))
        (when (and (consp exp) (eq (car exp) 'our-defmacro))
          ;; Must use host eval — our-defmacro mutates the host macro environment.
          (eval exp))
        (push exp out)))
    (cons 'progn (nreverse out))))

(defun expand-function-builtin (name)
  "Wrap a known builtin NAME in a first-class lambda for higher-order use."
  (cond
    ((member name *variadic-fold-builtins*)
     (let ((args (gensym "ARGS")) (acc (gensym "ACC")) (x (gensym "X"))
           (id   (cdr (assoc name *variadic-fold-identities*))))
       (compiler-macroexpand-all
        `(lambda (&rest ,args)
           (let ((,acc ,id))
             (dolist (,x ,args ,acc)
               (setq ,acc (,name ,acc ,x))))))))
    ((eq name '-)
     (let ((args (gensym "ARGS")) (acc (gensym "ACC")) (x (gensym "X")))
       (compiler-macroexpand-all
        `(lambda (&rest ,args)
           (if (null (cdr ,args))
               (- 0 (car ,args))
               (let ((,acc (car ,args)))
                 (dolist (,x (cdr ,args) ,acc)
                   (setq ,acc (- ,acc ,x)))))))))
    ((eq name 'list)
     (let ((args (gensym "ARGS")))
       (compiler-macroexpand-all `(lambda (&rest ,args) ,args))))
    ((member name *binary-builtins*)
     (let ((a (gensym "A")) (b (gensym "B")))
       (compiler-macroexpand-all `(lambda (,a ,b) (,name ,a ,b)))))
    (t
     (let ((x (gensym "X")))
       (compiler-macroexpand-all `(lambda (,x) (,name ,x)))))))

(defun expand-apply-named-fn (fn-name args-form)
  "Expand (apply 'FN-NAME args-form) where FN-NAME is a known symbol.
Variadic builtins get a dolist fold; others normalise to (apply #'fn args)."
  (if (member fn-name (list* '- 'list *variadic-fold-builtins*))
      (let ((acc (gensym "ACC")) (x (gensym "X")) (lst (gensym "LST"))
            (id  (cdr (assoc fn-name *variadic-fold-identities*))))
        (if (eq fn-name '-)
            (compiler-macroexpand-all
             `(let ((,lst ,args-form))
                (if (null (cdr ,lst))
                    (- 0 (car ,lst))
                    (let ((,acc (car ,lst)))
                      (dolist (,x (cdr ,lst) ,acc)
                        (setq ,acc (- ,acc ,x)))))))
            (compiler-macroexpand-all
             `(let ((,acc ,id))
                (dolist (,x ,args-form ,acc)
                  (setq ,acc (,fn-name ,acc ,x)))))))
      (list 'apply (list 'function fn-name)
            (compiler-macroexpand-all args-form))))

(defun expand-eval-when-form (situations body)
  "Handle EVAL-WHEN phase control.
Evaluate BODY immediately for :compile-toplevel; include in output for :execute/:load-toplevel."
  (when (member :compile-toplevel situations)
    (dolist (b body)
      (handler-case
          (let ((expanded (compiler-macroexpand-all b)))
            (when (and (consp expanded)
                       (member (car expanded) '(defvar defparameter)))
              (handler-case (eval expanded) (error () nil)))
            (if (fboundp 'run-string-repl)
                (run-string-repl (write-to-string expanded))
                (our-eval expanded)))
        (error () nil))))
  (if (or (member :execute situations)
          (member :load-toplevel situations))
      (compiler-macroexpand-all (cons 'progn body))
      nil))

(defun expand-setf-cons-place (place value)
  "Expand (setf (ACCESSOR ARGS...) value) for cons-cell accessors to rplaca/rplacd."
  (let ((v (gensym "V")))
    (case (car place)
      ((car first)
       `(let ((,v ,value)) (rplaca ,(second place) ,v) ,v))
      ((cdr rest)
       `(let ((,v ,value)) (rplacd ,(second place) ,v) ,v))
      (nth
       `(let ((,v ,value)) (rplaca (nthcdr ,(second place) ,(third place)) ,v) ,v))
      (cadr
       `(let ((,v ,value)) (rplaca (cdr ,(second place)) ,v) ,v))
      (cddr
       `(let ((,v ,value)) (rplacd (cdr ,(second place)) ,v) ,v)))))

(defun expand-make-array-form (size rest-args)
  "Expand (make-array size &rest keyword-args).
Promotes to make-adjustable-vector when :fill-pointer or :adjustable is given.
Handles :initial-contents (FR-654) and :initial-element (FR-687) via loop expansion."
  (let (fp adj init-elem init-contents)
    (loop for (key val) on rest-args by #'cddr
          do (case key
               (:fill-pointer    (setf fp val))
               (:adjustable      (setf adj val))
               (:initial-element  (setf init-elem val))
               (:initial-contents (setf init-contents val))
               ;; :element-type, :displaced-to, :displaced-index-offset — silently ignored
               ))
    (cond
      ;; :initial-contents — build array from sequence
      (init-contents
       (let ((arr-g  (gensym "ARR"))
             (cont-g (gensym "CONT"))
             (i-g    (gensym "I")))
         (compiler-macroexpand-all
          `(let* ((,cont-g ,init-contents)
                  (,arr-g  (make-array (length ,cont-g))))
             (dotimes (,i-g (length ,cont-g) ,arr-g)
               (setf (aref ,arr-g ,i-g) (elt ,cont-g ,i-g)))))))
      ;; :fill-pointer / :adjustable — promote to adjustable vector
      ;; When :fill-pointer has a specific non-t value, set it after construction
      ((or fp adj)
       (if (and fp (not (eq fp t)))
           ;; Specific fill-pointer value: create vector then set fill-pointer
           (let ((arr-g (gensym "ARR"))
                 (fp-g  (gensym "FP")))
             (compiler-macroexpand-all
              `(let* ((,fp-g  ,fp)
                      (,arr-g (make-adjustable-vector ,size)))
                 (setf (fill-pointer ,arr-g) ,fp-g)
                 ,arr-g)))
           ;; Boolean fill-pointer (t) or :adjustable only
           (compiler-macroexpand-all `(make-adjustable-vector ,size))))
      ;; :initial-element — fill array with init value
      (init-elem
       (let ((arr-g (gensym "ARR"))
             (ie-g  (gensym "IE"))
             (i-g   (gensym "I")))
         (compiler-macroexpand-all
          `(let* ((,ie-g  ,init-elem)
                  (,arr-g (make-array ,size)))
             (dotimes (,i-g ,size ,arr-g)
               (setf (aref ,arr-g ,i-g) ,ie-g))))))
      ;; plain make-array
      (t
       (compiler-macroexpand-all `(make-array ,size))))))

(defun expand-setf-accessor (place value)
  "Expand (setf (ACCESSOR OBJ) VAL) via *accessor-slot-map* for known struct accessors,
or fall back to generic (setf (slot-value obj 'accessor-name) val)."
  (let ((mapping (gethash (car place) *accessor-slot-map*)))
    (if mapping
        (compiler-macroexpand-all
         `(setf (slot-value ,(second place) ',(cdr mapping)) ,value))
        (compiler-macroexpand-all
         `(setf (slot-value ,(second place) ',(car place)) ,value)))))

;;; ── setf compound-place dispatch table — fact registration ──────────────
;;;
;;; Each entry maps a known place-head symbol to its expansion rule.
;;; The setf handler does a single gethash; no cond branching needed.

(setf (gethash 'aref *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(aset ,(second place) ,(third place) ,value))))

(setf (gethash 'getf *setf-compound-place-handlers*)
      (lambda (place value)
        (let ((v (gensym "V")))
          (compiler-macroexpand-all
           `(let ((,v ,value))
              (setq ,(second place)
                    (rt-plist-put ,(second place) ,(third place) ,v))
              ,v)))))

;; All cons-cell accessors share the same expansion logic — hoist the closure once
(let ((cons-place-handler (lambda (place value)
                            (compiler-macroexpand-all (expand-setf-cons-place place value)))))
  (dolist (sym '(car first cdr rest nth cadr cddr))
    (setf (gethash sym *setf-compound-place-handlers*) cons-place-handler)))

;;; FR-593: (setf (subseq seq start end) new-seq) → (replace seq new-seq :start1 start :end1 end)
(setf (gethash 'subseq *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(replace ,(second place) ,value
                   :start1 ,(third place)
                   ,@(when (fourth place) `(:end1 ,(fourth place)))))))

;;; FR-614: (setf (char s i) v) and (setf (schar s i) v)
(let ((char-place-handler (lambda (place value)
                            (compiler-macroexpand-all
                             `(rt-string-set ,(second place) ,(third place) ,value)))))
  (dolist (sym '(char schar))
    (setf (gethash sym *setf-compound-place-handlers*) char-place-handler)))

;;; FR-636: (setf (bit bv i) x) and (setf (sbit bv i) x)
(let ((bit-place-handler (lambda (place value)
                           (compiler-macroexpand-all
                            `(rt-bit-set ,(second place) ,(third place) ,value)))))
  (dolist (sym '(bit sbit))
    (setf (gethash sym *setf-compound-place-handlers*) bit-place-handler)))

;;; FR-620: (setf (svref v i) x) and (setf (row-major-aref a i) x)
(setf (gethash 'svref *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(%svset ,(second place) ,(third place) ,value))))

(setf (gethash 'row-major-aref *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(aset ,(second place) ,(third place) ,value))))

;;; FR-552: (setf (find-class name) class) → register in class registry
(setf (gethash 'find-class *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(%set-find-class ,(second place) ,value))))

;;; FR-548: (setf (symbol-function name) fn) → host bridge
(setf (gethash 'symbol-function *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(set-fdefinition ,value ,(second place)))))

;;; FR-428: (setf (macro-function name) fn) → host bridge
(setf (gethash 'macro-function *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(%set-macro-function ,(second place) ,value))))

;;; FR-603: (setf (values a b ...) expr) → multiple-value-bind + setq chain
(setf (gethash 'values *setf-compound-place-handlers*)
      (lambda (place value)
        (let* ((vars (cdr place))
               (temps (mapcar (lambda (v) (declare (ignore v)) (gensym "V")) vars)))
          (compiler-macroexpand-all
           `(multiple-value-bind ,temps ,value
              ,@(mapcar (lambda (var temp) `(setq ,var ,temp)) vars temps)
              (values ,@vars))))))

(defun expand-let-binding (b)
  "Macro-expand the value in a LET binding, leaving the binding name untouched."
  (if (and (consp b) (symbolp (car b)))
      (list (car b) (compiler-macroexpand-all (cadr b)))
      b))

(defun expand-flet-labels-binding (binding)
  "Macro-expand only the body forms of an FLET/LABELS binding; leave params untouched."
  (if (and (consp binding) (>= (length binding) 3))
      (list* (first binding) (second binding)
             (mapcar #'compiler-macroexpand-all (cddr binding)))
      binding))

(defun expand-lambda-list-defaults (params)
  "Expand macro calls in &optional/&key default value positions within PARAMS.
Leaves required params, lambda-list keywords, and supplied-p vars untouched."
  (let (in-extended)
    (mapcar (lambda (p)
               (cond
                 ((member p '(&optional &rest &key &allow-other-keys &aux &body &whole))
                  (setf in-extended t) p)
                 ((and in-extended (consp p) (cdr p))
                  (list* (first p)
                         (compiler-macroexpand-all (second p))
                         (cddr p)))
                 (t p)))
             params)))

;;; ── Registration macro ───────────────────────────────────────────────────

(defmacro define-expander-for (head (form) &body body)
  "Register a handler in *expander-head-table* for forms whose head is HEAD.
Contract: handler receives the full form and returns a fully-expanded form."
  `(setf (gethash ',head *expander-head-table*)
         (lambda (,form) ,@body)))

;;; ── Expander handler registrations ──────────────────────────────────────
;;;
;;; Each (define-expander-for HEAD ...) corresponds to one clause in the
;;; Prolog sense: head(Form) :- body(Form).  The inference engine
;;; (compiler-macroexpand-all) queries this database by head symbol.

;; quote — never recurse into quoted forms
(define-expander-for quote (form) form)

;; backquote — expand (cl-cc::backquote template) into list/cons/append
(define-expander-for cl-cc::backquote (form)
  (compiler-macroexpand-all (%expand-quasiquote (second form))))

;; funcall — (funcall 'name ...) with quoted symbol → direct call
(define-expander-for funcall (form)
  (if (and (>= (length form) 2)
           (consp (second form))
           (eq (car (second form)) 'quote)
           (symbolp (second (second form))))
      (compiler-macroexpand-all (cons (second (second form)) (cddr form)))
      (cons 'funcall (mapcar #'compiler-macroexpand-all (cdr form)))))

;; apply — spread-args normalisation + variadic builtin fold
(define-expander-for apply (form)
  (cond
    ;; (apply fn a1 a2 ... list) with spread args → cons-fold
    ((> (length form) 3)
     (let* ((fn         (second form))
            (spread-args (butlast (cddr form)))
            (last-arg    (car (last form)))
            (combined    (reduce (lambda (a rest) `(cons ,a ,rest))
                                 spread-args :from-end t :initial-value last-arg)))
       (compiler-macroexpand-all `(apply ,fn ,combined))))
    ;; (apply 'name list) or (apply #'name list)
    ((and (= (length form) 3)
          (consp (second form))
          (or (and (eq (car (second form)) 'quote)    (symbolp (second (second form))))
              (and (eq (car (second form)) 'function) (symbolp (second (second form))))))
     (expand-apply-named-fn (second (second form)) (third form)))
    ;; default: expand args
    (t (cons 'apply (mapcar #'compiler-macroexpand-all (cdr form))))))

;; make-hash-table — :test #'fn → :test 'fn normalisation
(define-expander-for make-hash-table (form)
  (if (and (>= (length form) 3)
           (eq (second form) :test)
           (consp (third form))
           (eq (car (third form)) 'function)
           (symbolp (second (third form))))
      (compiler-macroexpand-all
       `(make-hash-table :test ',(second (third form)) ,@(cdddr form)))
      (cons 'make-hash-table (mapcar #'compiler-macroexpand-all (cdr form)))))

;; function — wrap builtins in first-class lambda
(define-expander-for function (form)
  (let ((name (second form)))
    (if (and (symbolp name) (member name *all-builtin-names*))
        (expand-function-builtin name)
        form)))

;; multiple-value-list — must be here (not our-defmacro): %values-to-list is
;; position-sensitive and must follow the multi-valued form with no gap.
(define-expander-for multiple-value-list (form)
  (let ((tmp (gensym "MVL")))
    (compiler-macroexpand-all
     `(let ((,tmp ,(second form)))
        (declare (ignore ,tmp))
        (%values-to-list)))))

;; Variadic builtins: (OP a b c) → (OP (OP a b) c); each handler covers all arities.
(dolist (entry '((+ 0) (* 1) (append nil) (nconc nil)))
  (let ((op (first entry)) (id (second entry)))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (if (/= (length (cdr form)) 2)
                (compiler-macroexpand-all (reduce-variadic-op op (cdr form) id))
                (list op
                      (compiler-macroexpand-all (second form))
                      (compiler-macroexpand-all (third form))))))))

;; - (subtraction / negation): unary → (- 0 x), 0-arg → error, N-arg → left fold
(define-expander-for - (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 0) (error "- requires at least one argument"))
      ((= nargs 1) (list '- 0 (compiler-macroexpand-all (second form))))
      ((= nargs 2) (list '- (compiler-macroexpand-all (second form))
                            (compiler-macroexpand-all (third form))))
      (t (compiler-macroexpand-all (reduce-variadic-op '- (cdr form) 0))))))

;; / (FR-661): unary → reciprocal (/ 1 x), 0-arg → error, N-arg → left fold
(define-expander-for / (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 0) (error "/ requires at least one argument"))
      ((= nargs 1) (list '/ 1 (compiler-macroexpand-all (second form))))
      ((= nargs 2) (list '/ (compiler-macroexpand-all (second form))
                            (compiler-macroexpand-all (third form))))
      (t (compiler-macroexpand-all (reduce-variadic-op '/ (cdr form) 1))))))

;; log (FR-476): 1-arg → natural log, 2-arg → change-of-base formula
(define-expander-for log (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 1) (list 'log (compiler-macroexpand-all (second form))))
      ((= nargs 2) (compiler-macroexpand-all
                    `(/ (log ,(second form)) (log ,(third form)))))
      (t (error "log takes 1 or 2 arguments")))))

;; FR-662: min/max — 1-arg → identity, 2-arg → builtin, N-arg → left fold
(define-expander-for min (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 0) (error "min requires at least one argument"))
      ((= nargs 1) (compiler-macroexpand-all (second form)))
      ((= nargs 2) (list 'min (compiler-macroexpand-all (second form))
                              (compiler-macroexpand-all (third form))))
      (t (compiler-macroexpand-all (reduce-variadic-op 'min (cdr form) nil))))))

(define-expander-for max (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 0) (error "max requires at least one argument"))
      ((= nargs 1) (compiler-macroexpand-all (second form)))
      ((= nargs 2) (list 'max (compiler-macroexpand-all (second form))
                              (compiler-macroexpand-all (third form))))
      (t (compiler-macroexpand-all (reduce-variadic-op 'max (cdr form) nil))))))

;; FR-662: gcd/lcm — 0-arg → identity, 1-arg → abs, 2-arg → builtin, N-arg → fold
(define-expander-for gcd (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 0) 0)
      ((= nargs 1) (list 'abs (compiler-macroexpand-all (second form))))
      ((= nargs 2) (list 'gcd (compiler-macroexpand-all (second form))
                               (compiler-macroexpand-all (third form))))
      (t (compiler-macroexpand-all (reduce-variadic-op 'gcd (cdr form) 0))))))

(define-expander-for lcm (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 0) 1)
      ((= nargs 1) (list 'abs (compiler-macroexpand-all (second form))))
      ((= nargs 2) (list 'lcm (compiler-macroexpand-all (second form))
                               (compiler-macroexpand-all (third form))))
      (t (compiler-macroexpand-all (reduce-variadic-op 'lcm (cdr form) 1))))))

;; float-sign (FR-685): 1-arg → builtin, 2-arg → (* (float-sign x) (abs y))
(define-expander-for float-sign (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 1) (list 'float-sign (compiler-macroexpand-all (second form))))
      ((= nargs 2) (compiler-macroexpand-all
                    `(* (float-sign ,(second form)) (abs ,(third form)))))
      (t (error "float-sign takes 1 or 2 arguments")))))

;; float (FR-604): 1-arg → builtin, 2-arg → ignore prototype, just convert
(define-expander-for float (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 1) (list 'float (compiler-macroexpand-all (second form))))
      ((= nargs 2) (list 'float (compiler-macroexpand-all (second form))))
      (t (error "float takes 1 or 2 arguments")))))

;; FR-667: logand/logior/logxor/logeqv — 0-arg → identity, 1-arg → value, N-arg → left fold
;; Identity elements: logand → -1 (all bits set), logior → 0, logxor → 0, logeqv → -1
(dolist (entry '((logand -1) (logior 0) (logxor 0) (logeqv -1)))
  (let ((op (first entry)) (id (second entry)))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (cond
                ((= nargs 0) id)
                ((= nargs 1) (compiler-macroexpand-all (second form)))
                ((= nargs 2) (list op
                                   (compiler-macroexpand-all (second form))
                                   (compiler-macroexpand-all (third form))))
                (t (compiler-macroexpand-all
                    (reduce-variadic-op op (cdr form) id)))))))))

;; FR-663: =/</>/<=/>= — 1-arg → T, 2-arg → builtin, N-arg → AND chain with gensyms
(dolist (op '(= < > <= >=))
  (let ((op op))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (cond
                ((= nargs 0) (error "~A requires at least one argument" op))
                ((= nargs 1) t)
                ((= nargs 2) (list op
                                   (compiler-macroexpand-all (second form))
                                   (compiler-macroexpand-all (third form))))
                (t (compiler-macroexpand-all
                    (chain-comparison-op op (cdr form))))))))))

;; FR-645: char=/char</char>/char<=/char>=/char/= — variadic comparison chaining
;; Also case-insensitive: char-equal/char-lessp/char-greaterp/char-not-greaterp/char-not-lessp
(dolist (op '(char= char< char> char<= char>= char/=
              char-equal char-lessp char-greaterp char-not-greaterp char-not-lessp))
  (let ((op op))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (cond
                ((= nargs 0) (error "~A requires at least one argument" op))
                ((= nargs 1) t)
                ((= nargs 2) (list op
                                   (compiler-macroexpand-all (second form))
                                   (compiler-macroexpand-all (third form))))
                (t (compiler-macroexpand-all
                    (chain-comparison-op op (cdr form))))))))))

;; /= (not-equal): 1-arg → T, 2-arg → (not (= a b)), N-arg → all-pairs distinct
(define-expander-for /= (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 0) (error "/= requires at least one argument"))
      ((= nargs 1) t)
      ((= nargs 2) (compiler-macroexpand-all
                     `(not (= ,(second form) ,(third form)))))
      (t (let* ((args (cdr form))
                (temps (loop for i from 0 below nargs
                             collect (gensym (format nil "NE~D-" i))))
                (bindings (mapcar #'list temps args))
                (pairs (loop for (a . rest) on temps
                             nconc (loop for b in rest
                                         collect `(not (= ,a ,b))))))
           (compiler-macroexpand-all
            `(let ,bindings (and ,@pairs))))))))

;; FR-665: mapcar/mapc/mapcan — multi-sequence support
;; 2-arg → inline single-list expansion, 3+ args → labels recursion over N lists
(dolist (entry '((mapcar :collect) (mapc :side-effect) (mapcan :nconc)))
  (let ((op (first entry)) (mode (second entry)))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (cond
                ;; 2-arg: inline the single-list expansion (replaces our-defmacro)
                ((= nargs 2)
                 (let ((fn-var (gensym "FN"))
                       (x (gensym "X"))
                       (acc (gensym "ACC"))
                       (lst (gensym "LST"))
                       (fn-arg (second form))
                       (list-arg (third form)))
                   (compiler-macroexpand-all
                    (ecase mode
                      (:collect
                       `(let ((,fn-var ,fn-arg) (,acc nil))
                          (dolist (,x ,list-arg (nreverse ,acc))
                            (setq ,acc (cons (funcall ,fn-var ,x) ,acc)))))
                      (:side-effect
                       `(let ((,fn-var ,fn-arg) (,lst ,list-arg))
                          (dolist (,x ,lst ,lst)
                            (funcall ,fn-var ,x))))
                      (:nconc
                       `(let ((,fn-var ,fn-arg) (,acc nil))
                          (dolist (,x ,list-arg ,acc)
                            (setq ,acc (nconc ,acc (funcall ,fn-var ,x))))))))))
                ;; 3+ args: multi-list via labels recursion
                ((>= nargs 3)
                 (let* ((fn-arg (second form))
                        (list-args (cddr form))
                        (nlists (length list-args))
                        (fn-var (gensym "FN"))
                        (list-vars (loop for i from 0 below nlists
                                         collect (gensym (format nil "L~D-" i))))
                        (helper (gensym "MAP"))
                        (null-test `(or ,@(mapcar (lambda (v) `(null ,v)) list-vars)))
                        (car-args (mapcar (lambda (v) `(car ,v)) list-vars))
                        (cdr-args (mapcar (lambda (v) `(cdr ,v)) list-vars))
                        (call `(funcall ,fn-var ,@car-args))
                        (recurse `(,helper ,@cdr-args))
                        (body (ecase mode
                                (:collect `(if ,null-test nil
                                             (cons ,call ,recurse)))
                                (:side-effect `(if ,null-test nil
                                                 (progn ,call ,recurse)))
                                (:nconc `(if ,null-test nil
                                            (nconc ,call ,recurse))))))
                   (compiler-macroexpand-all
                    `(let ((,fn-var ,fn-arg))
                       (labels ((,helper ,list-vars ,body))
                         (,helper ,@list-args))))))
                (t (error "~A requires at least 2 arguments" op))))))))

;; FR-650: every/some — multi-sequence support
;; 2-arg → inline single-list expansion, 3+ args → labels recursion over N lists
(dolist (entry '((every :every) (some :some)))
  (let ((op (first entry)) (mode (second entry)))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (cond
                ;; 2-arg: inline the single-list expansion (replaces our-defmacro)
                ((= nargs 2)
                 (let ((fn-var (gensym "FN"))
                       (x (gensym "X"))
                       (result (gensym "R"))
                       (pred-arg (second form))
                       (list-arg (third form)))
                   (compiler-macroexpand-all
                    (ecase mode
                      (:every
                       `(let ((,fn-var ,pred-arg))
                          (block nil
                            (dolist (,x ,list-arg t)
                              (unless (funcall ,fn-var ,x)
                                (return nil))))))
                      (:some
                       `(let ((,fn-var ,pred-arg))
                          (block nil
                            (dolist (,x ,list-arg nil)
                              (let ((,result (funcall ,fn-var ,x)))
                                (when ,result (return ,result)))))))))))
                ;; 3+ args: multi-list via labels recursion
                ((>= nargs 3)
                 (let* ((fn-arg (second form))
                        (list-args (cddr form))
                        (nlists (length list-args))
                        (fn-var (gensym "FN"))
                        (list-vars (loop for i from 0 below nlists
                                         collect (gensym (format nil "L~D-" i))))
                        (helper (gensym "QNT"))
                        (null-test `(or ,@(mapcar (lambda (v) `(null ,v)) list-vars)))
                        (car-args (mapcar (lambda (v) `(car ,v)) list-vars))
                        (cdr-args (mapcar (lambda (v) `(cdr ,v)) list-vars))
                        (call `(funcall ,fn-var ,@car-args))
                        (recurse `(,helper ,@cdr-args))
                        (body (ecase mode
                                (:every `(if ,null-test t
                                            (if ,call ,recurse nil)))
                                (:some `(if ,null-test nil
                                           (let ((r ,call))
                                             (if r r ,recurse)))))))
                   (compiler-macroexpand-all
                    `(let ((,fn-var ,fn-arg))
                       (labels ((,helper ,list-vars ,body))
                         (,helper ,@list-args))))))
                (t (error "~A requires at least 2 arguments" op))))))))

;; FR-478: parse-integer — 1-arg → builtin, N-arg → extract keyword args → stdlib impl
(define-expander-for parse-integer (form)
  (let ((nargs (length (cdr form))))
    (if (= nargs 1)
        (list 'parse-integer (compiler-macroexpand-all (second form)))
        (let ((str (second form))
              (start 0) (end nil) (radix 10) (junk nil)
              (rest (cddr form)))
          (loop while rest do
            (let ((key (car rest)) (val (cadr rest)))
              (cond ((eq key :start)        (setf start val))
                    ((eq key :end)          (setf end val))
                    ((eq key :radix)        (setf radix val))
                    ((eq key :junk-allowed) (setf junk val)))
              (setf rest (cddr rest))))
          (compiler-macroexpand-all
           `(%parse-integer-impl ,str ,start ,end ,radix ,junk))))))

;; FR-668: digit-char-p — 1-arg → builtin, 2-arg → inline radix check via char-code
(define-expander-for digit-char-p (form)
  (let ((nargs (length (cdr form))))
    (cond
      ((= nargs 1) (list 'digit-char-p (compiler-macroexpand-all (second form))))
      ((= nargs 2)
       (let ((ch (gensym "CH")) (radix (gensym "RADIX")) (code (gensym "CODE")))
         (compiler-macroexpand-all
          `(let ((,ch ,(second form)) (,radix ,(third form)))
             (let ((,code (char-code ,ch)))
               (cond
                 ((and (>= ,code 48) (< ,code (min 58 (+ 48 ,radix))))
                  (- ,code 48))
                 ((and (>= ,radix 11) (>= ,code 65) (< ,code (+ 55 ,radix)))
                  (- ,code 55))
                 ((and (>= ,radix 11) (>= ,code 97) (< ,code (+ 87 ,radix)))
                  (- ,code 87))
                 (t nil)))))))
      (t (error "digit-char-p takes 1 or 2 arguments")))))

;; deftype — register type alias at expand time; returns (quote name)
;; Supports: (deftype name expansion) — simple alias (legacy 2-arg)
;;           (deftype name () body...) — ANSI form with empty lambda-list
;;           (deftype name (params) body...) — ANSI form with parameters (FR-430)
(define-expander-for deftype (form)
  (let ((name (second form)))
    (cond
      ;; (deftype name (params...) body...) — ANSI form with lambda-list + body
      ((and (>= (length form) 4) (symbolp name) (listp (third form))
            (every #'symbolp (third form)))
       (let* ((lambda-list (third form))
              (body (cdddr form))
              (expansion (if (= (length body) 1) (first body) `(progn ,@body))))
         (if (null lambda-list)
             ;; No params: evaluate body at expand time for simple type alias
             (let ((result (eval expansion)))
               (cl-cc/type:register-type-alias name result))
             ;; With params: register expander function
             (let ((expander (eval `(lambda ,lambda-list ,@body))))
               (cl-cc/type:register-type-alias name expander)))))
      ;; (deftype name expansion) — legacy 2-arg form
      ((and (= (length form) 3) (symbolp name))
       (cl-cc/type:register-type-alias name (third form))))
    `(quote ,name)))

;; defsetf — register a setf expander for an accessor (FR-355)
;; Short form: (defsetf accessor updater) → (updater args... value)
;; Long form: (defsetf accessor lambda-list (store-var) body...) — partial support
(define-expander-for defsetf (form)
  (let ((accessor (second form)))
    (cond
      ;; Short form: (defsetf accessor updater)
      ((and (= (length form) 3) (symbolp (third form)))
       (let ((updater (third form)))
         (setf (gethash accessor *setf-compound-place-handlers*)
               (lambda (place value)
                 (compiler-macroexpand-all
                  `(,updater ,@(cdr place) ,value))))))
      ;; Long form: (defsetf accessor (args...) (store-var) body...)
      ((and (>= (length form) 5) (listp (third form)) (listp (fourth form)))
       (let* ((lambda-list (third form))
              (store-vars (fourth form))
              (body (cddddr form))
              (store-var (first store-vars)))
         (setf (gethash accessor *setf-compound-place-handlers*)
               (let ((ll lambda-list) (sv store-var) (bd body))
                 (lambda (place value)
                   (let* ((arg-bindings (mapcar #'list ll (cdr place)))
                          (store-binding `((,sv ,value))))
                     (compiler-macroexpand-all
                      `(let (,@arg-bindings ,@store-binding)
                         ,@bd)))))))))
    `(quote ,accessor)))

;; define-setf-expander — register setf expansion function (FR-355)
;; (define-setf-expander accessor (lambda-list) body...)
(define-expander-for define-setf-expander (form)
  (let* ((accessor (second form))
         (lambda-list (third form))
         (body (cdddr form))
         (expander-fn (eval `(lambda ,lambda-list ,@body))))
    (setf (gethash accessor *setf-compound-place-handlers*)
          (let ((fn expander-fn))
            (lambda (place value)
              (multiple-value-bind (temps vals stores store-form access-form)
                  (funcall fn place)
                (declare (ignore access-form))
                (compiler-macroexpand-all
                 `(let (,@(mapcar #'list temps vals)
                        ,@(mapcar (lambda (s) (list s value)) stores))
                    ,store-form)))))))
  `(quote ,(second form)))

;; defconstant — compile-time constants treated as defparameter
(define-expander-for defconstant (form)
  (compiler-macroexpand-all `(defparameter ,(second form) ,(third form))))

;; FR-651: vector — variadic vector constructor (vector a b c) → #(a b c)
;; Expands to (coerce-to-vector (list ...)) so the VM list→vector path handles it.
(define-expander-for vector (form)
  (compiler-macroexpand-all `(coerce-to-vector (list ,@(cdr form)))))

;; FR-679: get-decoded-time — 0-arg form, expands to decode-universal-time call
(define-expander-for get-decoded-time (form)
  (declare (ignore form))
  '(decode-universal-time (get-universal-time)))

;; setf — unified place dispatcher
(define-expander-for setf (form)
  (let ((len (length form))
        (expand-args (lambda () (cons 'setf (mapcar #'compiler-macroexpand-all (cdr form))))))
    (cond
      ;; (setf a 1 b 2 ...) → (progn (setf a 1) (setf b 2) ...)
      ((and (> len 3) (evenp (1- len)))
       (compiler-macroexpand-all
        `(progn ,@(loop for (place val) on (cdr form) by #'cddr
                        collect `(setf ,place ,val)))))
      ;; (setf var val) → (setq var val)
      ((and (= len 3) (symbolp (second form)))
       (compiler-macroexpand-all `(setq ,(second form) ,(third form))))
      ;; compound place — table lookup first, then generic accessor, then passthrough
      ((and (= len 3) (consp (second form)))
       (let* ((place   (second form))
              (value   (third form))
              (handler (gethash (car place) *setf-compound-place-handlers*)))
         (cond
           (handler (funcall handler place value))
           ((and (symbolp (car place)) (= (length place) 2))
            (expand-setf-accessor place value))
           (t (funcall expand-args)))))
      (t (funcall expand-args)))))

;; make-array — promote to make-adjustable-vector when :fill-pointer/:adjustable given
(define-expander-for make-array (form)
  (if (>= (length form) 4)
      (expand-make-array-form (second form) (cddr form))
      (cons 'make-array (mapcar #'compiler-macroexpand-all (cdr form)))))

;; defstruct — expand to defclass + constructor + predicate
(define-expander-for defstruct (form)
  (compiler-macroexpand-all (expand-defstruct form)))

;; eval-when — phase control (compile-toplevel vs execute/load-toplevel)
(define-expander-for eval-when (form)
  (expand-eval-when-form (second form) (cddr form)))

;; macrolet — local macro bindings scoped to body
(define-expander-for macrolet (form)
  (expand-macrolet-form (second form) (cddr form)))

;; symbol-macrolet — local symbol macro bindings scoped to body (FR-398)
(define-expander-for symbol-macrolet (form)
  (expand-symbol-macrolet-form (second form) (cddr form)))

;; define-symbol-macro — global symbol macro definition (FR-398)
(define-expander-for define-symbol-macro (form)
  (let ((name (second form))
        (expansion (third form)))
    (setf (gethash name *symbol-macro-table*) expansion)
    `(quote ,name)))

;; FR-607: Strip leading docstring from body (string + more forms → skip string)
;; Returns (stripped-body . docstring-or-nil)
(defun %strip-docstring (body)
  (if (and (stringp (car body)) (cdr body))
      (cdr body)
      body))

(defun %extract-docstring (body)
  "Return the docstring if BODY starts with one (and has more forms), else nil."
  (if (and (stringp (car body)) (cdr body))
      (car body)
      nil))

;; defun — typed params get check-type assertions; untyped: expand defaults + body
(define-expander-for defun (form)
  (let* ((fn-name   (second form))
         (raw-body  (cdddr form))
         (docstring (%extract-docstring raw-body))
         (body      (%strip-docstring raw-body)))
    ;; FR-607: register docstring at compile time in CL-level table
    (when (and docstring (symbolp fn-name))
      (setf (gethash (list fn-name 'function) *documentation-table*) docstring))
    (if (and (>= (length form) 4)
             (symbolp fn-name)
             (listp (third form))
             (lambda-list-has-typed-p (third form)))
        (expand-typed-defun-or-lambda 'defun fn-name (third form) body)
        (list* 'defun fn-name
               (expand-lambda-list-defaults (third form))
               (mapcar #'compiler-macroexpand-all body)))))

;; lambda — typed params get check-type assertions; untyped: expand defaults + body
(define-expander-for lambda (form)
  (let ((body (%strip-docstring (cddr form))))
    (if (and (>= (length form) 3)
             (listp (second form))
             (lambda-list-has-typed-p (second form)))
        (expand-typed-defun-or-lambda 'lambda nil (second form) body)
        (list* 'lambda
               (expand-lambda-list-defaults (second form))
               (mapcar #'compiler-macroexpand-all body)))))

;; defclass — register accessor mappings; expand :initform values in slot specs
;; Passes class options (5th+ elements like :default-initargs) through to parser
(define-expander-for defclass (form)
  (let ((class-name (second form))
        (slot-specs (fourth form))
        (class-options (nthcdr 4 form)))
    (register-defclass-accessors class-name slot-specs)
    (append (list 'defclass class-name
                  (mapcar #'compiler-macroexpand-all (third form))
                  (when (listp slot-specs)
                    (mapcar #'expand-defclass-slot-spec slot-specs)))
            class-options)))

;; progn — process forms sequentially, eagerly registering any defmacro siblings
(define-expander-for progn (form)
  (expand-progn-with-eager-defmacro (cdr form)))

;; let — destructuring bindings desugar to destructuring-bind chains; plain let expands values
;; FR-623: (let () body) → (progn body)
(define-expander-for let (form)
  (cond
    ((and (>= (length form) 2) (listp (second form)) (null (second form)))
     (compiler-macroexpand-all (cons 'progn (cddr form))))
    ((and (>= (length form) 2) (listp (second form))
          (some (lambda (b) (and (consp b) (>= (length b) 2) (consp (first b))))
                (second form)))
     (let ((simple nil) (destructuring nil))
       (dolist (b (second form))
         (if (and (consp b) (>= (length b) 2) (consp (first b)))
             (push b destructuring)
             (push b simple)))
       (setf simple (nreverse simple) destructuring (nreverse destructuring))
       (let ((inner (if simple
                        (list* 'let simple (cddr form))
                        (cons 'progn (cddr form)))))
         (dolist (d (reverse destructuring))
           (setf inner (list 'destructuring-bind (first d) (second d) inner)))
         (compiler-macroexpand-all inner))))
    ((and (>= (length form) 2) (listp (second form)))
     (list* 'let
            (mapcar #'expand-let-binding (second form))
            (mapcar #'compiler-macroexpand-all (cddr form))))
    (t (cons 'let (mapcar #'compiler-macroexpand-all (cdr form))))))

;; flet/labels — expand only function bodies; head symbol tells them apart
;; FR-623: (flet () body) / (labels () body) → (progn body)
(defun %expand-flet-or-labels (head form)
  (if (and (>= (length form) 3) (listp (second form)))
      (if (null (second form))
          (compiler-macroexpand-all (cons 'progn (cddr form)))
          (list* head
                 (mapcar #'expand-flet-labels-binding (second form))
                 (mapcar #'compiler-macroexpand-all (cddr form))))
      (cons head (mapcar #'compiler-macroexpand-all (cdr form)))))

(define-expander-for flet   (form) (%expand-flet-or-labels 'flet   form))
(define-expander-for labels (form) (%expand-flet-or-labels 'labels form))

;; FR-585: handler-case :no-error clause
;; (handler-case form (type (v) ...) (:no-error (x) success-body))
;; → (block #:tag
;;     (let ((#:r (handler-case form (type (v) (return-from #:tag ...)))))
;;       success-body-with-x-bound))
(define-expander-for handler-case (form)
  (let* ((protected (second form))
         (all-clauses (cddr form))
         (no-error-clause (find :no-error all-clauses :key #'car))
         (error-clauses (remove :no-error all-clauses :key #'car)))
    (if no-error-clause
        (let ((tag (gensym "NO-ERROR-"))
              (ne-vars (second no-error-clause))
              (ne-body (cddr no-error-clause)))
          (compiler-macroexpand-all
           `(block ,tag
              (let ((,(if (and ne-vars (car ne-vars)) (car ne-vars) (gensym "R-"))
                     (handler-case ,protected
                       ,@(mapcar (lambda (c)
                                   `(,(first c) ,(second c)
                                     (return-from ,tag (progn ,@(cddr c)))))
                                 error-clauses))))
                ,@(or ne-body '(nil))))))
        ;; No :no-error clause — recurse into subforms normally
        (cons 'handler-case (mapcar #'compiler-macroexpand-all (cdr form))))))

;; FR-301: normalise 1-arg rounding ops to 2-arg form with divisor 1.
;; Registered via dolist for all *rounding-ops* at once (data-driven).
(dolist (op *rounding-ops*)
  (setf (gethash op *expander-head-table*)
        (lambda (form)
          (if (= (length form) 2)
              (compiler-macroexpand-all `(,(car form) ,(second form) 1))
              (list (car form)
                    (compiler-macroexpand-all (second form))
                    (compiler-macroexpand-all (third form)))))))

;;; ── FR-626: error/warn format-string desugaring ──────────────────────────
;;;
;;; (error fmt arg ...) → (error (format nil fmt arg ...)) when 2+ args.
;;; Single-arg form passes through to builtin dispatch unchanged.
;; Only desugar when datum is a string and extra format args are present.
;; Handler-case clauses (error (var) body) have a list as second element —
;; those must pass through to the parser unchanged.
(dolist (op '(error warn))
  (setf (gethash op *expander-head-table*)
        (lambda (form)
          (if (and (> (length form) 2) (stringp (second form)))
              ;; (error "fmt" arg...) → (error (format nil "fmt" arg...))
              (compiler-macroexpand-all
               `(,(car form) (format nil ,@(cdr form))))
              ;; 1-arg form or handler-case clause: pass through unchanged
              (mapcar #'compiler-macroexpand-all form)))))

;;; ── Main dispatcher ──────────────────────────────────────────────────────
;;;
;;; The inference engine: 4 clauses, ~15 lines.
;;; Handlers in *expander-head-table* take priority over *compiler-special-forms*.

(defun compiler-macroexpand-all (form)
  "Expand macros in FORM for the compiler pipeline.
Dispatch order: (1) atoms — symbol macros expanded, others pass through;
(2) *expander-head-table* handlers; (3) *compiler-special-forms* recurse-fallback;
(4) our-macroexpand-1."
  (cond
    ((atom form)
     (if (and (symbolp form)
              (not (keywordp form))
              (gethash form *symbol-macro-table*))
         (compiler-macroexpand-all (gethash form *symbol-macro-table*))
         form))
    (t
     (let ((handler (gethash (car form) *expander-head-table*)))
       (cond
         (handler
          (funcall handler form))
         ((member (car form) *compiler-special-forms*)
          (cons (car form) (mapcar #'compiler-macroexpand-all (cdr form))))
         (t
          (multiple-value-bind (exp expanded-p) (our-macroexpand-1 form)
            (if expanded-p
                (compiler-macroexpand-all exp)
                (mapcar #'compiler-macroexpand-all form)))))))))
