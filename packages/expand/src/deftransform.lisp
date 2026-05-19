(in-package :cl-cc/expand)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Deftransform — compile-time method specialization transforms
;;;
;;; Mirrors the compiler-macro registry pattern, but keys entries by both the
;;; function name and a required-argument type pattern.  Transform functions
;;; receive (FORM ENVIRONMENT) and return either a replacement form or NIL to
;;; decline the transform.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar *deftransform-table* (make-hash-table :test #'equal)
  "Global deftransform environment: maps (function-name . type-pattern) to transform functions.")

(defun register-deftransform (name type-pattern transform-fn)
  "Register TRANSFORM-FN for NAME specialized by TYPE-PATTERN.
TYPE-PATTERN is a list of type specifiers such as (INTEGER STRING), (LIST), or
T wildcards.  TRANSFORM-FN receives (FORM ENVIRONMENT) and returns a replacement
form, or NIL when the transform does not apply."
  (setf (gethash (cons name type-pattern) *deftransform-table*) transform-fn)
  (%reset-macroexpansion-caches)
  name)

(defun %deftransform-candidate-names (name)
  "Return possible registry names for NAME, following compiler-macro local lookup conventions."
  (if (symbolp name)
      (let ((local (intern (symbol-name name) :cl-cc/expand)))
        (if (eq local name) (list name) (list name local)))
      (list name)))

(defun %deftransform-pattern-match-p (pattern arg-types)
  "Return T when PATTERN matches ARG-TYPES.  T in PATTERN is a wildcard."
  (and (= (length pattern) (length arg-types))
       (loop for expected in pattern
             for actual in arg-types
             always (or (eq expected 't)
                        (equal expected actual)))))

(defun %deftransform-pattern-specificity (pattern)
  "Return a numeric specificity score for PATTERN; higher is more specific."
  (count-if-not (lambda (type) (eq type 't)) pattern))

(defun lookup-deftransform (name &optional arg-types)
  "Look up a deftransform for NAME and optional ARG-TYPES.
When ARG-TYPES is supplied, exact matches are preferred and T in registered
patterns acts as a wildcard.  Without ARG-TYPES, only zero-argument transform
patterns are considered."
  (let ((names (%deftransform-candidate-names name)))
    (or (loop for candidate in names
              for exact = (gethash (cons candidate arg-types) *deftransform-table*)
              when exact return exact)
        (let ((best nil)
              (best-score -1))
          (maphash
           (lambda (key value)
             (let ((candidate-name (car key))
                   (pattern (cdr key)))
               (when (and (member candidate-name names :test #'eq)
                          (%deftransform-pattern-match-p pattern arg-types))
                 (let ((score (%deftransform-pattern-specificity pattern)))
                   (when (> score best-score)
                     (setf best value
                           best-score score))))))
           *deftransform-table*)
          best))))

(defun deftransform (name &optional arg-types)
  "Return the registered deftransform for NAME and optional ARG-TYPES, or NIL."
  (lookup-deftransform name arg-types))

(defun make-deftransform-expander (arg-names body)
  "Create a data-backed deftransform expander for source-level DEFINE-DEFTRANSFORM."
  (list :kind :deftransform-expander
        :arg-names arg-names
        :body body))

(defun %deftransform-expander-p (object)
  "Return T when OBJECT is a data-backed deftransform expander."
  (and (listp object)
       (eq (getf object :kind) :deftransform-expander)))

(defun invoke-deftransform (transform form env)
  "Invoke TRANSFORM on FORM and ENV.
Supports both host functions and descriptor-backed transform expanders."
  (cond
    ((functionp transform)
     (funcall transform form env))
    ((%deftransform-expander-p transform)
     (let* ((form-var (gensym "FORM"))
            (env-var  (gensym "ENV"))
            (arg-names (getf transform :arg-names))
            (bindings (loop for arg in arg-names
                            for index from 0
                            collect `(,arg (nth ,index (cdr ,form-var)))))
            (eval-form `(let ((,form-var ',form)
                              (,env-var ',env))
                          (declare (ignorable ,env-var))
                          (let ,bindings
                            ,@(getf transform :body)))))
       (funcall *macro-eval-fn* eval-form)))
    (t
     (error "Unsupported deftransform representation: ~S" transform))))

(defun %literal-deftransform-type (form)
  "Return a conservative type symbol for literal FORM, or NIL when unknown."
  (cond
    ((and (consp form) (eq (car form) 'quote))
     (let ((value (second form)))
       (cond
         ((listp value) 'list)
         ((integerp value) 'integer)
         ((stringp value) 'string)
         ((symbolp value) 'symbol)
         ((characterp value) 'character)
         (t nil))))
    ((and (consp form) (eq (car form) 'the) (third form))
     (second form))
    ((integerp form) 'integer)
    ((stringp form) 'string)
    ((characterp form) 'character)
    ((null form) 'list)
    (t nil)))

(defun deftransform-arg-types-known (args)
  "Return (VALUES TYPES T) when all ARGS have conservative compile-time types.
This intentionally does not call or mutate the type inference pipeline; it only
recognizes explicit THE annotations and literal forms."
  (let ((types nil))
    (dolist (arg args (values (nreverse types) t))
      (let ((type (%literal-deftransform-type arg)))
        (unless type
          (return (values nil nil)))
        (push type types)))))

(defun deftransform-expand-1 (form env)
  "Apply one deftransform to FORM when all argument types are known.
Returns (VALUES EXPANDED-FORM T) when a transform produced a replacement;
otherwise returns (VALUES FORM NIL)."
  (if (and (consp form) (symbolp (car form)))
      (multiple-value-bind (arg-types known-p)
          (deftransform-arg-types-known (cdr form))
        (if known-p
            (let* ((transform (lookup-deftransform (car form) arg-types))
                   (expanded (and transform (invoke-deftransform transform form env))))
              (if (and expanded (not (equal expanded form)))
                  (values expanded t)
                  (values form nil)))
            (values form nil)))
      (values form nil)))

(defmacro define-deftransform (name typed-args &body body)
  "Define a deftransform for NAME.
Syntax: (DEFINE-DEFTRANSFORM name ((arg type) ...) body...).  The BODY may use
the argument names, plus FORM and ENVIRONMENT bindings, and should return a
replacement form or NIL."
  (let ((form-var (gensym "FORM"))
        (env-var  (gensym "ENV"))
        (arg-names (mapcar #'first typed-args))
        (type-pattern (mapcar #'second typed-args)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (register-deftransform
        ',name
        ',type-pattern
        (lambda (,form-var ,env-var)
          (let ((form ,form-var)
                (environment ,env-var))
            (declare (ignorable form environment))
            (destructuring-bind ,arg-names (cdr ,form-var)
              (declare (ignorable ,@arg-names))
              ,@body))))
       ',name)))

(register-macro 'define-deftransform
  (lambda (form env)
    (declare (ignore env))
    (let* ((name (second form))
           (typed-args (third form))
           (body (cdddr form))
           (arg-names (mapcar #'first typed-args))
           (type-pattern (mapcar #'second typed-args)))
      (register-deftransform name type-pattern (make-deftransform-expander arg-names body))
      `(quote ,name))))

;;; Example transform: `(identity x)` is replaced by `x` when x is known to be a list.
(eval-when (:load-toplevel :execute)
  (register-deftransform
   'identity
   '(list)
   (lambda (form env)
     (declare (ignore env))
     (second form))))
