;;;; expand/expander.lisp - Compiler Macro Expander

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

(in-package :cl-cc)

;;; Accessor-to-slot mapping for setf expansion (populated by defstruct)
(defvar *accessor-slot-map* (make-hash-table :test #'eq)
  "Maps accessor function names to (class-name . slot-name) for setf expansion.")

(defparameter *compiler-special-forms*
  '(if progn lambda quote setq setf
    defun defvar defparameter defmacro defclass defgeneric defmethod
    make-instance slot-value
    block return-from tagbody go
    flet labels function funcall
    the print
    catch throw unwind-protect
    handler-case eval-when defstruct
    macrolet symbol-macrolet
    multiple-value-call multiple-value-prog1
    values multiple-value-bind apply)
  "Forms handled directly by the parser/compiler — not subject to macro expansion.
   Note: declare, in-package, defpackage, export, locally, warn, coerce,
   with-open-file, copy-hash-table are registered as our-defmacro in macro.lisp.")

(defun expand-defstruct (form)
  "Expand (defstruct name-or-options slot...) to progn of defclass + defuns.
Supports :conc-name, :constructor with boa-lambda-list, slot defaults."
  (let* ((name-and-options (second form))
         (slots-raw (cddr form))
         (name (if (listp name-and-options) (first name-and-options) name-and-options))
         (options (when (listp name-and-options) (rest name-and-options)))
         (conc-name-opt (find :conc-name options :key (lambda (o) (when (listp o) (first o)))))
         (conc-name (if conc-name-opt
                        (second conc-name-opt)
                        (intern (concatenate 'string (symbol-name name) "-"))))
         (constructor-opt (find :constructor options :key (lambda (o) (when (listp o) (first o)))))
         (constructor-name (if constructor-opt
                               (second constructor-opt)
                               (intern (concatenate 'string "MAKE-" (symbol-name name)))))
         (boa-args (when (and constructor-opt (cddr constructor-opt))
                     (third constructor-opt)))
         (parsed-slots (mapcar (lambda (s)
                                 (if (listp s)
                                     (list (first s) (second s))
                                     (list s nil)))
                               (remove-if #'stringp slots-raw)))
         (predicate-name (intern (concatenate 'string (symbol-name name) "-P"))))
    (flet ((slot-accessor (slot-name)
             "Compute the accessor symbol for SLOT-NAME under CONC-NAME."
             (if conc-name
                 (intern (concatenate 'string (symbol-name conc-name) (symbol-name slot-name)))
                 slot-name)))
      ;; Register accessor→slot mappings at macro-expansion time
      (dolist (slot parsed-slots)
        (setf (gethash (slot-accessor (first slot)) *accessor-slot-map*)
              (cons name (first slot))))
      `(progn
         (defclass ,name ()
           ,(mapcar (lambda (slot)
                      `(,(first slot)
                        :initarg ,(intern (symbol-name (first slot)) "KEYWORD")
                        :initform ,(second slot)
                        :accessor ,(slot-accessor (first slot))))
                    parsed-slots))
         ,(if boa-args
              `(defun ,constructor-name ,boa-args
                 (make-instance ',name
                                ,@(mapcan (lambda (arg)
                                            (list (intern (symbol-name arg) "KEYWORD") arg))
                                          boa-args)))
              `(defun ,constructor-name (&key ,@(mapcar (lambda (slot)
                                                          (list (first slot) (second slot)))
                                                        parsed-slots))
                 (make-instance ',name
                                ,@(mapcan (lambda (slot)
                                            (list (intern (symbol-name (first slot)) "KEYWORD")
                                                  (first slot)))
                                          parsed-slots))))
         (defun ,predicate-name (obj) (typep obj ',name))
         ',name))))

(defun reduce-variadic-op (op args identity)
  "Reduce a variadic arithmetic form (OP arg...) to nested binary forms.
(OP) => IDENTITY, (OP a) => a, (OP a b) => (OP a b), (OP a b c ...) => (OP (OP a b) c) ..."
  (case (length args)
    (0 identity)
    (1 (first args))
    (2 (list op (first args) (second args)))
    (t (reduce (lambda (acc x) (list op acc x)) (cddr args)
               :initial-value (list op (first args) (second args))))))

;;; ------------------------------------------------------------
;;; Builtin Arity Classification (data layer)
;;; ------------------------------------------------------------
;;; These tables drive both the #'name → lambda wrapping and the
;;; (apply #'name list) → dolist-fold expansion in compiler-macroexpand-all.

(defparameter *variadic-fold-builtins*
  '(+ * append nconc)
  "Builtins that fold over a list with an identity: (OP a b c) = (OP (OP a b) c).")

(defparameter *binary-builtins*
  '(cons = < > <= >= mod rem eq eql equal
    nth nthcdr member assoc acons
    string= string< string> string<= string>= string-equal
    string-lessp string-greaterp string/=
    string-not-equal string-not-greaterp string-not-lessp string-concat
    char min max floor ceiling truncate round ffloor fceiling ftruncate fround
    ash logand logior logxor logeqv logtest logbitp
    expt scale-float gcd lcm complex
    array-dimension row-major-aref svref vector-push
    bit sbit bit-and bit-or bit-xor adjust-array)
  "Builtins that take exactly 2 arguments.")

(defparameter *unary-builtins*
  '(car cdr not null consp symbolp numberp integerp stringp
    atom listp characterp functionp
    first second third fourth fifth rest last
    nreverse butlast endp reverse length copy-list copy-tree
    symbol-name make-symbol intern gensym keywordp
    string-length string-upcase string-downcase
    char-code code-char
    typep hash-table-p hash-table-count
    hash-table-test hash-table-keys hash-table-values
    zerop plusp minusp evenp oddp abs lognot logcount integer-length
    sqrt exp log sin cos tan asin acos atan sinh cosh tanh float float-sign
    rational rationalize numerator denominator realpart imagpart conjugate phase
    boundp fboundp makunbound fmakunbound
    array-rank array-total-size array-dimensions
    fill-pointer array-has-fill-pointer-p array-adjustable-p vector-pop
    bit-not array-displacement char-int
    princ prin1 print write-to-string prin1-to-string princ-to-string
    type-of make-list alphanumericp eval identity)
  "Builtins that take exactly 1 argument.")

(defparameter *cxr-builtins*
  '(caar cadr cdar cddr
    caaar cdaar cadar cddar
    caadr cdadr caddr cdddr
    caaaar cadaar caadar caddar
    cdaaar cddaar cdadar cdddar
    caaadr cadadr caaddr cadddr
    cdaadr cddadr cdaddr cddddr)
  "CXR accessor builtins — all unary.")

(defparameter *all-builtin-names*
  (append *variadic-fold-builtins* '(- list) *binary-builtins* *unary-builtins* *cxr-builtins*)
  "Union of all known builtin names — used to decide whether #'name needs a lambda wrapper.")

;;; ------------------------------------------------------------
;;; compiler-macroexpand-all helper functions
;;; ------------------------------------------------------------

(defun register-defclass-accessors (class-name slot-specs)
  "Register ACCESSOR → (CLASS-NAME . SLOT-NAME) mappings in *accessor-slot-map*.
   Called during macro expansion so later (setf (accessor obj) val) forms
   can be lowered to (setf (slot-value ...)) without runtime lookup."
  (when (listp slot-specs)
    (dolist (spec slot-specs)
      (when (listp spec)
        (let ((accessor (getf (rest spec) :accessor)))
          (when accessor
            (setf (gethash accessor *accessor-slot-map*)
                  (cons class-name (first spec)))))))))

(defun expand-defclass-slot-spec (spec)
  "Expand only the :initform value inside a slot SPEC list, leaving all other
   keys (slot name, :accessor, :initarg, :reader, :writer, :type) untouched."
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
   HEAD is 'defun or 'lambda; NAME is the function name (nil for lambda)."
  (multiple-value-bind (plain-params type-alist)
      (strip-typed-params params)
    (let* ((has-return-type (and rest-forms
                                 (symbolp (first rest-forms))
                                 (cl-cc/type:looks-like-type-specifier-p (first rest-forms))))
           (return-type-spec (when has-return-type (first rest-forms)))
           (body-forms       (if has-return-type (cdr rest-forms) rest-forms)))
      ;; Register the function's type signature for the type checker
      (let ((param-types (mapcar (lambda (e)
                                   (cl-cc/type:parse-type-specifier (cdr e)))
                                 type-alist))
            (return-type  (if return-type-spec
                              (cl-cc/type:parse-type-specifier return-type-spec)
                              cl-cc/type:+type-unknown+)))
        (when (eq head 'defun)
          (register-function-type name param-types return-type)))
      ;; Build the annotated body
      (let* ((typed-body (if return-type-spec
                             `((the ,return-type-spec (progn ,@body-forms)))
                             body-forms))
             (checks (loop for (pname . ptype) in type-alist
                           collect `(check-type ,pname ,ptype)))
             (full-body (append checks typed-body)))
        (compiler-macroexpand-all
         (if (eq head 'defun)
             `(defun ,name ,plain-params ,@full-body)
             `(lambda ,plain-params ,@full-body)))))))

(defun make-macro-expander (lambda-list body)
  "Build a macro expander function for a LAMBDA-LIST and BODY.
   The expander destructures the macro call form and evaluates BODY."
  (let ((form-var (gensym "FORM"))
        (env-var  (gensym "ENV")))
    (eval `(lambda (,form-var ,env-var)
             (declare (ignore ,env-var))
             (let* ,(generate-lambda-bindings lambda-list form-var)
               ,@body)))))

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
  "Expand each form in SUBFORMS, eagerly registering any DEFMACRO/OUR-DEFMACRO forms so
   later sibling forms can immediately use the new macro."
  (let ((out nil))
    (dolist (sub subforms)
      (let ((exp (compiler-macroexpand-all sub)))
        (when (and (consp exp) (eq (car exp) 'defmacro))
          (register-macro (second exp)
                          (make-macro-expander (third exp) (cdddr exp))))
        ;; our-defmacro forms (from define-modify-macro, etc.) need to be evaluated
        ;; eagerly so that sibling forms can use the newly registered macro.
        (when (and (consp exp) (eq (car exp) 'our-defmacro))
          (eval exp))
        (push exp out)))
    (cons 'progn (nreverse out))))

(defun expand-function-builtin (name)
  "Wrap a known builtin NAME in a first-class lambda for higher-order use."
  (cond
    ((member name *variadic-fold-builtins*)
     (let ((args (gensym "ARGS")) (acc (gensym "ACC")) (x (gensym "X"))
           (id   (case name ((+) 0) ((*) 1) (t nil))))
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
            (id  (case fn-name ((+) 0) ((*) 1) (t nil))))
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
   Evaluate BODY immediately if :compile-toplevel is listed; include in output
   if :execute or :load-toplevel is listed."
  (when (member :compile-toplevel situations)
    (dolist (b body)
      (our-eval (compiler-macroexpand-all b))))
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
                  ;; (name default ...) — expand only the default (second element)
                  (list* (first p)
                         (compiler-macroexpand-all (second p))
                         (cddr p)))
                 (t p)))
             params)))

(defun compiler-macroexpand-all (form)
  "Expand macros in FORM for the compiler pipeline.
   Skips forms the compiler handles as special forms."
  (cond
    ;; Atoms pass through
    ((atom form) form)
    ;; Quote — never recurse into quoted forms
    ((eq (car form) 'quote) form)
    ;; (funcall 'name ...) with quoted symbol => (name ...) direct call
    ((and (consp form) (eq (car form) 'funcall)
          (>= (length form) 2)
          (consp (second form))
          (eq (car (second form)) 'quote)
          (symbolp (second (second form))))
     (compiler-macroexpand-all (cons (second (second form)) (cddr form))))
    ;; (apply fn a1 a2 ... list-form) with spread args => (apply fn (cons a1 (cons a2 ... list-form)))
    ((and (consp form) (eq (car form) 'apply)
          (> (length form) 3))
     (let* ((fn (second form))
            (spread-args (butlast (cddr form)))
            (last-arg (car (last form)))
            (combined (reduce (lambda (a rest) `(cons ,a ,rest))
                              spread-args :from-end t :initial-value last-arg)))
       (compiler-macroexpand-all `(apply ,fn ,combined))))
    ;; (apply 'name/function-ref list-form) — variadic builtins get dolist fold; others normalize to #'
    ((and (consp form) (eq (car form) 'apply)
          (= (length form) 3)
          (consp (second form))
          (or (and (eq (car (second form)) 'quote)    (symbolp (second (second form))))
              (and (eq (car (second form)) 'function) (symbolp (second (second form))))))
     (expand-apply-named-fn (second (second form)) (third form)))
    ;; (make-hash-table :test #'fn) → (make-hash-table :test 'fn)
    ((and (eq (car form) 'make-hash-table)
          (>= (length form) 3)
          (eq (second form) :test)
          (consp (third form))
          (eq (car (third form)) 'function)
          (symbolp (second (third form))))
     (compiler-macroexpand-all
      `(make-hash-table :test ',(second (third form)) ,@(cdddr form))))
    ;; (function builtin) — wrap builtins in lambda for first-class use
    ((eq (car form) 'function)
     (let ((name (second form)))
       (if (and (symbolp name) (member name *all-builtin-names*))
           (expand-function-builtin name)
           form)))
    ;; (multiple-value-list expr) — must live here (not our-defmacro) because the
    ;; %values-to-list VM intrinsic is position-sensitive: no other instruction
    ;; must execute between the multi-valued form and the capture call.
    ;; declare-ignore in the body is intentional — the binder just triggers the
    ;; VM's values-buffer fill; the primary-value binding itself is discarded.
    ((and (consp form) (eq (car form) 'multiple-value-list)
          (= (length form) 2))
     (let ((tmp (gensym "MVL")))
       (compiler-macroexpand-all
        `(let ((,tmp ,(second form)))
           (declare (ignore ,tmp))
           (%values-to-list)))))
    ;; Variadic builtins: (+ a b c) => (+ (+ a b) c), (append a b c) => (append (append a b) c)
    ((and (consp form) (member (car form) (cons '- *variadic-fold-builtins*))
          (/= (length (cdr form)) 2))
     (let ((op (car form))
           (identity (case (car form) ((+ -) 0) (* 1) (t nil))))
       (compiler-macroexpand-all
        (reduce-variadic-op op (cdr form) identity))))
    ;; (deftype name type-spec) — register type alias at expand time
    ;; (kept here rather than our-defmacro because cl-cc/type is loaded after macro.lisp)
    ((and (consp form) (eq (car form) 'deftype)
          (= (length form) 3)
          (symbolp (second form)))
     (cl-cc/type:register-type-alias (second form) (third form))
     `(quote ,(second form)))
    ;; (setf var val) => (setq var val) — plain variable assignment
    ((and (consp form) (eq (car form) 'setf)
          (= (length form) 3)
          (symbolp (second form)))
     (compiler-macroexpand-all `(setq ,(second form) ,(third form))))
    ;; (setf (aref arr idx) val) => (aset arr idx val)
    ((and (consp form) (eq (car form) 'setf)
          (= (length form) 3)
          (consp (second form))
          (eq (car (second form)) 'aref))
     (let ((arr (second (second form)))
           (idx (third (second form)))
           (val (third form)))
       (compiler-macroexpand-all `(aset ,arr ,idx ,val))))
    ;; (make-array size :fill-pointer fp :adjustable adj ...) => make-adjustable-vector
    ((and (consp form) (eq (car form) 'make-array)
          (>= (length form) 4))
     (expand-make-array-form (second form) (cddr form)))
    ;; (setf (car/cdr/first/rest/nth ...) val) — expand to rplaca/rplacd
    ((and (consp form) (eq (car form) 'setf)
          (= (length form) 3)
          (consp (second form))
          (member (car (second form)) '(car cdr first rest nth cadr cddr)))
     (compiler-macroexpand-all
      (expand-setf-cons-place (second form) (third form))))
    ;; (setf (accessor obj) val) — expand via accessor-slot-map or to slot-value
    ((and (consp form) (eq (car form) 'setf)
          (= (length form) 3)
          (consp (second form))
          (symbolp (car (second form)))
          (= (length (second form)) 2))
     (expand-setf-accessor (second form) (third form)))
    ;; (defstruct ...) — expand to defclass + constructor + predicate
    ((and (consp form) (eq (car form) 'defstruct))
     (compiler-macroexpand-all (expand-defstruct form)))
    ;; (eval-when (situations...) body...) — phase control
    ((and (consp form) (eq (car form) 'eval-when))
     (expand-eval-when-form (second form) (cddr form)))
    ;; (macrolet ((name lambda-list body)...) forms...) — local macros
    ((and (consp form) (eq (car form) 'macrolet))
     (expand-macrolet-form (second form) (cddr form)))
    ;; Typed defun: (defun name ((x fixnum) ...) return-type body...)
    ((and (consp form) (eq (car form) 'defun)
          (>= (length form) 4)
          (symbolp (second form))
          (listp (third form))
          (lambda-list-has-typed-p (third form)))
     (expand-typed-defun-or-lambda 'defun (second form) (third form) (cdddr form)))
    ;; Typed lambda: (lambda ((x fixnum) ...) return-type body...)
    ((and (consp form) (eq (car form) 'lambda)
          (>= (length form) 3)
          (listp (second form))
          (lambda-list-has-typed-p (second form)))
     (expand-typed-defun-or-lambda 'lambda nil (second form) (cddr form)))
    ;; (defclass name supers (slot-specs...)) — extract accessor mappings for setf expansion
    ((and (consp form) (eq (car form) 'defclass))
     (let ((class-name (second form))
           (slot-specs (fourth form)))
       (register-defclass-accessors class-name slot-specs)
       (list 'defclass class-name
             (mapcar #'compiler-macroexpand-all (third form))
             (when (listp slot-specs)
               (mapcar #'expand-defclass-slot-spec slot-specs)))))
    ;; progn — process forms sequentially so defmacro takes effect for later forms
    ((and (consp form) (eq (car form) 'progn))
     (expand-progn-with-eager-defmacro (cdr form)))
    ;; defun/lambda (untyped) — protect required params from expansion; expand default values and body
    ((and (consp form) (member (car form) '(defun lambda))
          (>= (length form) 3)
          (listp (second (if (eq (car form) 'lambda) form (cdr form)))))
     (if (eq (car form) 'defun)
         ;; (defun name params body...)
         (list* 'defun (second form) (expand-lambda-list-defaults (third form))
                (mapcar #'compiler-macroexpand-all (cdddr form)))
         ;; (lambda params body...)
         (list* 'lambda (expand-lambda-list-defaults (second form))
                (mapcar #'compiler-macroexpand-all (cddr form)))))
    ;; let — expand only binding VALUES, not binding names (let* is a macro, handled by our-macroexpand-1)
    ((and (consp form) (eq (car form) 'let)
          (>= (length form) 2) (listp (second form)))
     (list* 'let
            (mapcar #'expand-let-binding (second form))
            (mapcar #'compiler-macroexpand-all (cddr form))))
    ;; flet/labels — expand only function bodies, not binding structure
    ((and (consp form) (member (car form) '(flet labels))
          (>= (length form) 3) (listp (second form)))
     (list* (car form)
            (mapcar #'expand-flet-labels-binding (second form))
            (mapcar #'compiler-macroexpand-all (cddr form))))
    ;; FR-301: normalize 1-arg rounding forms to 2-arg with divisor 1
    ((and (consp form) (= (length form) 2)
          (member (car form) '(floor ceiling truncate round)))
     (compiler-macroexpand-all `(,(car form) ,(second form) 1)))
    ;; Special forms — recurse into subforms but don't expand the head
    ((and (symbolp (car form))
          (member (car form) *compiler-special-forms*))
     (cons (car form)
           (mapcar #'compiler-macroexpand-all (cdr form))))
    ;; Try macro expansion
    (t
     (multiple-value-bind (exp expanded-p)
         (our-macroexpand-1 form)
       (if expanded-p
           (compiler-macroexpand-all exp)
           (mapcar #'compiler-macroexpand-all form))))))

