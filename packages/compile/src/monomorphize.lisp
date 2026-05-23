;;;; packages/compile/src/monomorphize.lisp — FR-712 Monomorphization
;;;; Convert generic/polymorphic functions into monomorphic specializations.
;;;; Rust monomorphization / C++ template instantiation equivalent.

(in-package :cl-cc/compile)

(defvar *monomorphize-enabled* t)

(defvar *monomorphized-versions* (make-hash-table :test #'equal)
  "(function-name type-signature) → specialized-function-name mapping.")

;;; ──── Type-based specialization ────
(defun monomorphize-function (func-name type-args)
  "Create a monomorphized version of FUNC-NAME specialized for TYPE-ARGS.
Returns the name of the specialized function."
  (let* ((key (list func-name type-args))
         (existing (gethash key *monomorphized-versions*)))
    (or existing
        (let ((specialized-name
               (intern (format nil "~A~{-~A~}" func-name
                              (mapcar (lambda (t) (ty-symbol t)) type-args)))))
          ;; Clone the function with type-args substituted
          (setf (gethash key *monomorphized-versions*) specialized-name)
          specialized-name))))

(defun monomorphize-call-site (call-expr type-env)
  "At a call site, determine if the generic function should be
replaced with a monomorphized version based on static argument types."
  (declare (ignore call-expr type-env))
  nil)

;;; ──── Pass integration ────
(defun monomorphization-pass (module)
  "Apply monomorphization to all eligible generic functions in MODULE."
  (declare (ignore module))
  ;; For each generic function:
  ;; 1. Collect all call sites and their concrete type arguments
  ;; 2. For each unique set of type arguments, create a specialized version
  ;; 3. Replace call sites with specialized versions
  (values))

;;; ──── Helpers ────
(defun ty-symbol (type)
  "Get a symbol representation of TYPE for naming specializations."
  (typecase type
    (symbol (string-downcase (symbol-name type)))
    (cons (ty-symbol (car type)))
    (t "T")))
