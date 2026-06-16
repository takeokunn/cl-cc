;;;; packages/compile/src/reflection.lisp — FR-657 Runtime Reflection API
;;;; Programmatic access to compiler internals at runtime.
;;;; Java Reflection / .NET Reflection API equivalent.

(in-package :cl-cc/compile)

(defvar *reflection-enabled* t)

;;; ──── Type reflection ────
(defun reflect-type-of (object)
  "Return compile-time type info for OBJECT."
  (type-of object))

(defun reflect-type-name (type)
  "Return the name (symbol) of TYPE."
  (typecase type
    (symbol type)
    (class (class-name type))
    (t (type-of type))))

;;; ──── Function reflection ────
(defun %sb-kernel-fbound-symbol (name)
  "Return the SB-KERNEL symbol named NAME when available and fbound."
  (let ((package (find-package "SB-KERNEL")))
    (when package
      (let ((symbol (find-symbol name package)))
        (when (and symbol (fboundp symbol))
          symbol)))))

(defun %sb-introspect-function-lambda-list (fn)
  "Return FN's lambda list when SB-INTROSPECT is available."
  (let ((package (find-package "SB-INTROSPECT")))
    (when package
      (let ((symbol (find-symbol "FUNCTION-LAMBDA-LIST" package)))
        (when (and symbol (fboundp symbol))
          (funcall symbol fn))))))

(defun reflect-function-arity (fn)
  "Return the arity of function FN."
  (let ((info (%sb-introspect-function-lambda-list fn)))
    (if info
        (length (remove-if (lambda (x) (member x '(&optional &rest &key &allow-other-keys)))
                           info))
        (multiple-value-bind (lambda-expression closure-p name)
            (function-lambda-expression fn)
          (declare (ignore closure-p name))
          (if (and (consp lambda-expression)
                   (eq (first lambda-expression) 'lambda))
              (let ((lambda-list (second lambda-expression)))
                (length (remove-if (lambda (x) (member x '(&optional &rest &key &allow-other-keys)))
                                   lambda-list)))
              0)))))

(defun reflect-function-name (fn)
  "Return the name of function FN."
  (let ((symbol (%sb-kernel-fbound-symbol "%FUN-NAME")))
    (if symbol
        (funcall symbol fn)
        (multiple-value-bind (lambda-expression closure-p name)
            (function-lambda-expression fn)
          (declare (ignore lambda-expression closure-p))
          name))))

;;; ──── Slot reflection ────
(defun reflect-class-slots (class-name)
  "Return list of slot names for CLASS-NAME."
  (mapcar #'cl-cc/vm:slot-definition-name
          (cl-cc/vm::class-direct-slots (find-class class-name))))

(defun reflect-slot-value (object slot-name)
  "Get SLOT-NAME of OBJECT via reflection."
  (slot-value object slot-name))

;;; ──── Compiler reflection ────
(defun reflect-optimization-settings ()
  "Return current optimization settings as an alist."
  (let* ((policy (symbol-value (find-symbol "*POLICY*" "SB-C")))
         (policy-quality (find-symbol "POLICY-QUALITY" "SB-C")))
    (loop for quality in '(speed safety debug compilation-speed space)
          collect (cons quality (funcall policy-quality policy quality)))))

(defun reflect-compiled-function-p (fn)
  "Return T if FN has been compiled to native code."
  (let ((symbol (%sb-kernel-fbound-symbol "%SIMPLE-FUN-P")))
    (if symbol
        (funcall symbol fn)
        (typep fn 'compiled-function))))

;;; ──── Meta-object protocol extension ────
(defun reflect-generic-function-methods (gf-name)
  "Return list of method specializers for GF-NAME."
  (mapcar #'cl-cc/vm::method-specializers
          (cl-cc/vm:generic-function-methods (fdefinition gf-name))))
