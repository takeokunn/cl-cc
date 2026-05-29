;;;; packages/php/src/unsupported.lisp — PHP unsupported-form validation

(in-package :cl-cc/php)

(defun %php-unsupported-call-p (ast)
  "Return true when AST is a PHP unsupported-form marker call."
  (and (ast-call-p ast)
       (let ((func (ast-call-func ast)))
         (and (ast-var-p func)
              (eq (ast-var-name func) '%php-unsupported)))))

(defun %php-unsupported-message (ast)
  "Return the compile-time diagnostic message stored in unsupported marker AST."
  (let ((message-arg (first (ast-call-args ast))))
    (if (and (ast-quote-p message-arg)
             (stringp (ast-quote-value message-arg)))
         (ast-quote-value message-arg)
         "PHP form is not yet supported in cl-cc")))

(defun %php-unsupported-classlike-message (kind)
  "Return a diagnostic for unsupported PHP class-like declaration KIND.
Returns NIL when the kind is supported (:trait, :interface are both supported)."
  (case kind
    (:trait     nil)                    ; traits are now supported via parser-trait.lisp
    (:interface nil)                    ; interfaces are now supported
    (otherwise nil)))

(defun php-check-supported-forms (forms)
  "Signal an error if FORMS contains PHP AST markers for unsupported constructs."
  (labels ((walk (form)
             (cond
               ((null form) nil)
               ((%php-unsupported-call-p form)
                 (ast-error form "~A" (%php-unsupported-message form)))
                ((and (ast-defclass-p form)
                      (%php-unsupported-classlike-message (ast-defclass-php-kind form)))
                 (ast-error form "~A"
                            (%php-unsupported-classlike-message
                             (ast-defclass-php-kind form))))
                ((typep form 'ast-node)
                 (dolist (child (ast-children form))
                   (walk child)))
               ((consp form)
                (walk (car form))
                (walk (cdr form)))
               (t nil))))
    (walk forms)
    forms))
