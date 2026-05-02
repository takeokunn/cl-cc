(in-package :cl-cc/expand)

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
           (typed-return-spec (and return-type-spec
                                   (or (cl-cc/type:lookup-type-alias return-type-spec)
                                       return-type-spec)))
           (typed-body       (if typed-return-spec
                                 `((the ,typed-return-spec (progn ,@body-forms)))
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
When *macro-eval-fn* is our-eval (self-hosting mode), the returned value is
a data descriptor interpreted through the selfhost evaluator instead of a
host CL closure."
  (list :kind :macro-expander
        :lambda-list lambda-list
        :body (mapcar #'our-macroexpand-all body)))

(defun make-compiler-macro-expander (lambda-list body)
  "Build a compiler-macro expander for a function LAMBDA-LIST and BODY."
  (list :kind :compiler-macro-expander
        :lambda-list lambda-list
        :body (mapcar #'our-macroexpand-all body)))

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
      (when (and (consp sub)
                 (symbolp (car sub))
                 (string= (symbol-name (car sub)) "OUR-DEFMACRO"))
        ;; Register OUR-DEFMACRO eagerly using the same host macro environment as
        ;; DEFMACRO, but via MAKE-MACRO-EXPANDER so bootstrap quasiquote forms in
        ;; the body are normalized before the expander is stored.
        (register-macro (second sub)
                        (make-macro-expander (third sub) (cdddr sub))))
      (let ((exp (compiler-macroexpand-all sub)))
        (when (and (consp exp) (eq (car exp) 'defmacro))
          (register-macro (second exp)
                          (make-macro-expander (third exp) (cdddr exp))))
        (push exp out)))
    (cons 'progn (nreverse out))))

(defun expand-eval-when-form (situations body)
  "Handle EVAL-WHEN phase control.
Evaluate BODY immediately for :compile-toplevel; include in output for :execute/:load-toplevel."
  (when (member :compile-toplevel situations)
    (dolist (b body)
      (handler-case
          (let ((expanded (compiler-macroexpand-all b)))
            (if (fboundp 'run-string-repl)
                (run-string-repl (write-to-string expanded))
                (our-eval expanded)))
        (error () nil))))
  (if (or (member :execute situations)
          (member :load-toplevel situations))
      (compiler-macroexpand-all (cons 'progn body))
      nil))

(defun %build-variadic-fold-lambda (name)
  (let ((args (gensym "ARGS")) (acc (gensym "ACC")) (x (gensym "X"))
        (id   (variadic-fold-identity name)))
    `(lambda (&rest ,args)
       (let ((,acc ,id))
         (dolist (,x ,args ,acc)
           (setq ,acc (,name ,acc ,x)))))))

(defun %build-subtract-lambda ()
  (let ((args (gensym "ARGS")) (acc (gensym "ACC")) (x (gensym "X")))
    `(lambda (&rest ,args)
       (if (null (cdr ,args))
           (- 0 (car ,args))
           (let ((,acc (car ,args)))
             (dolist (,x (cdr ,args) ,acc)
               (setq ,acc (- ,acc ,x))))))))

(defun expand-function-builtin (name)
  "Wrap a known builtin NAME in a first-class lambda for higher-order use."
  (compiler-macroexpand-all
   (cond
     ((member name *variadic-fold-builtins*) (%build-variadic-fold-lambda name))
     ((eq name '-)                           (%build-subtract-lambda))
     ((eq name 'list)
      (let ((args (gensym "ARGS")))
        `(lambda (&rest ,args) ,args)))
     ((member name *binary-builtins*)
      (let ((a (gensym "A")) (b (gensym "B")))
        `(lambda (,a ,b) (,name ,a ,b))))
     (t
      (let ((x (gensym "X")))
        `(lambda (,x) (,name ,x)))))))

(defun expand-apply-named-fn (fn-name args-form)
  "Expand (apply 'FN-NAME args-form) where FN-NAME is a known symbol.
Variadic builtins get a dolist fold; others normalise to (apply #'fn args)."
  (if (member fn-name (list* '- 'list *variadic-fold-builtins*))
      (let ((acc (gensym "ACC")) (x (gensym "X")) (lst (gensym "LST")))
        (if (eq fn-name '-)
            (compiler-macroexpand-all
             `(let ((,lst ,args-form))
                (if (null (cdr ,lst))
                    (- 0 (car ,lst))
                    (let ((,acc (car ,lst)))
                      (dolist (,x (cdr ,lst) ,acc)
                        (setq ,acc (- ,acc ,x)))))))
            (compiler-macroexpand-all
             (let ((id (variadic-fold-identity fn-name)))
               `(let ((,acc ,id))
                  (dolist (,x ,args-form ,acc)
                    (setq ,acc (,fn-name ,acc ,x))))))))
      (list 'apply (list 'function fn-name)
            (compiler-macroexpand-all args-form))))
