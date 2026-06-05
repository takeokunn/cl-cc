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
(defun reflect-function-arity (fn)
  "Return the arity of function FN."
  (let ((info (sb-introspect:function-lambda-list fn)))
    (length (remove-if (lambda (x) (member x '(&optional &rest &key &allow-other-keys)))
                       info))))

(defun reflect-function-name (fn)
  "Return the name of function FN."
  (sb-kernel:%fun-name fn))

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
  (warn "reflect-optimization-settings returns hard-coded values; TODO: query *optimize-qualities*")
  `((speed . 3) (safety . 0) (debug . 0)
    (compilation-speed . 0) (space . 0)))

(defun reflect-compiled-function-p (fn)
  "Return T if FN has been compiled to native code."
  (sb-kernel:%simple-fun-p fn))

;;; ──── Meta-object protocol extension ────
(defun reflect-generic-function-methods (gf-name)
  "Return list of method specializers for GF-NAME."
  (mapcar #'cl-cc/vm::method-specializers
          (cl-cc/vm:generic-function-methods (fdefinition gf-name))))
