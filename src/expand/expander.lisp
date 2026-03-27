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
Promotes to make-adjustable-vector when :fill-pointer or :adjustable is given."
  (let (fp adj)
    (loop for (key val) on rest-args by #'cddr
          do (case key
               (:fill-pointer (setf fp val))
               (:adjustable   (setf adj val))))
    (if (or fp adj)
        (compiler-macroexpand-all `(make-adjustable-vector ,size))
        (compiler-macroexpand-all `(make-array ,size)))))

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

;;; FR-620: (setf (svref v i) x) and (setf (row-major-aref a i) x)
(setf (gethash 'svref *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(%svset ,(second place) ,(third place) ,value))))

(setf (gethash 'row-major-aref *setf-compound-place-handlers*)
      (lambda (place value)
        (compiler-macroexpand-all
         `(aset ,(second place) ,(third place) ,value))))

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

;; deftype — register type alias at expand time; returns (quote name)
(define-expander-for deftype (form)
  (when (and (= (length form) 3) (symbolp (second form)))
    (cl-cc/type:register-type-alias (second form) (third form)))
  `(quote ,(second form)))

;; defconstant — compile-time constants treated as defparameter
(define-expander-for defconstant (form)
  (compiler-macroexpand-all `(defparameter ,(second form) ,(third form))))

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

;; defun — typed params get check-type assertions; untyped: expand defaults + body
(define-expander-for defun (form)
  (if (and (>= (length form) 4)
           (symbolp (second form))
           (listp (third form))
           (lambda-list-has-typed-p (third form)))
      (expand-typed-defun-or-lambda 'defun (second form) (third form) (cdddr form))
      (list* 'defun (second form)
             (expand-lambda-list-defaults (third form))
             (mapcar #'compiler-macroexpand-all (cdddr form)))))

;; lambda — typed params get check-type assertions; untyped: expand defaults + body
(define-expander-for lambda (form)
  (if (and (>= (length form) 3)
           (listp (second form))
           (lambda-list-has-typed-p (second form)))
      (expand-typed-defun-or-lambda 'lambda nil (second form) (cddr form))
      (list* 'lambda
             (expand-lambda-list-defaults (second form))
             (mapcar #'compiler-macroexpand-all (cddr form)))))

;; defclass — register accessor mappings; expand :initform values in slot specs
(define-expander-for defclass (form)
  (let ((class-name (second form))
        (slot-specs (fourth form)))
    (register-defclass-accessors class-name slot-specs)
    (list 'defclass class-name
          (mapcar #'compiler-macroexpand-all (third form))
          (when (listp slot-specs)
            (mapcar #'expand-defclass-slot-spec slot-specs)))))

;; progn — process forms sequentially, eagerly registering any defmacro siblings
(define-expander-for progn (form)
  (expand-progn-with-eager-defmacro (cdr form)))

;; let — destructuring bindings desugar to destructuring-bind chains; plain let expands values
(define-expander-for let (form)
  (cond
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
(defun %expand-flet-or-labels (head form)
  (if (and (>= (length form) 3) (listp (second form)))
      (list* head
             (mapcar #'expand-flet-labels-binding (second form))
             (mapcar #'compiler-macroexpand-all (cddr form)))
      (cons head (mapcar #'compiler-macroexpand-all (cdr form)))))

(define-expander-for flet   (form) (%expand-flet-or-labels 'flet   form))
(define-expander-for labels (form) (%expand-flet-or-labels 'labels form))

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
Dispatch order: (1) atoms pass through; (2) *expander-head-table* handlers;
(3) *compiler-special-forms* recurse-fallback; (4) our-macroexpand-1."
  (cond
    ((atom form) form)
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
