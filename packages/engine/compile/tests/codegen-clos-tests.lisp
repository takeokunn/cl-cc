;;;; tests/unit/compile/codegen-clos-tests.lisp — Codegen CLOS Unit Tests

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defun make-test-slot (name &key initarg)
  "Build a minimal ast-slot-def for use in codegen tests."
  (cl-cc/ast::make-ast-slot-def :name name :initarg initarg))

;;; ─── compile-ast: ast-defclass ───────────────────────────────────────────────

(deftest codegen-defclass-compilation
  "Compiling defclass: emits vm-class-def with correct slot names, registers globally, returns register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast::make-ast-defclass
                             :name 'my-rect
                             :superclasses nil
                             :slots (list (make-test-slot 'w :initarg :w)
                                          (make-test-slot 'h :initarg :h)))
                           ctx))
         (inst (codegen-find-inst ctx 'cl-cc/vm::vm-class-def)))
    (assert-true inst)
    (assert-equal '(w h) (cl-cc/vm::vm-slot-names inst))
    (assert-true (gethash 'my-rect (cl-cc/compile::ctx-global-classes ctx)))
    (assert-true (keywordp reg))))

;;; ─── compile-ast: ast-defgeneric ─────────────────────────────────────────────

(deftest codegen-defgeneric-compilation
  "Compiling defgeneric emits vm-class-def dispatch table and registers in global-generics."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc/ast::make-ast-defgeneric :name 'my-speak :params '(animal))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-class-def))
    (assert-true (gethash 'my-speak (cl-cc/compile::ctx-global-generics ctx)))))

(deftest codegen-defgeneric-idempotent
  "Compiling the same defgeneric twice reuses the existing dispatch register."
  (let ((ctx (make-codegen-ctx)))
    (let ((r1 (compile-ast (cl-cc/ast::make-ast-defgeneric :name 'my-compute :params '(x)) ctx))
          (r2 (compile-ast (cl-cc/ast::make-ast-defgeneric :name 'my-compute :params '(x)) ctx)))
      (assert-eq r1 r2))))

;;; ─── compile-ast: ast-defmethod ──────────────────────────────────────────────

(deftest codegen-defmethod-compilation
  "Compiling defmethod emits vm-register-method (with correct specializer) and vm-closure."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc/ast::make-ast-defgeneric :name 'my-greet :params '(obj)) ctx)
    (compile-ast (cl-cc/ast::make-ast-defmethod
                  :name 'my-greet
                  :specializers (list '(obj . dog))
                  :params '(obj)
                  :body (list (cl-cc/ast::make-ast-int :value 99)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-register-method)))
      (assert-true inst)
      (assert-equal '(dog) (cl-cc/vm::vm-method-specializer inst)))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-closure))))

;;; ─── compile-ast: ast-make-instance ─────────────────────────────────────────

(deftest-each codegen-make-instance-emits-vm-make-obj
  "make-instance emits vm-make-obj regardless of static vs dynamic class reference."
  :cases (("static"  (cl-cc/ast::make-ast-make-instance
                       :class (cl-cc/ast::make-ast-quote :value 'my-dog)
                       :initargs (list (cons :name (cl-cc/ast::make-ast-quote :value 'rex))))
                     nil)
          ("dynamic" (cl-cc/ast::make-ast-make-instance
                       :class (cl-cc/ast::make-ast-var :name 'cls)
                       :initargs nil)
                     (list (cons 'cls :R50))))
  (ast env-setup)
  (let ((ctx (make-codegen-ctx)))
    (when env-setup (setf (cl-cc/compile::ctx-env ctx) env-setup))
    (compile-ast ast ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj))))

(deftest codegen-make-instance-static-loads-class-globally
  "Static make-instance emits vm-get-global to load the class descriptor."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc/ast::make-ast-make-instance
                  :class (cl-cc/ast::make-ast-quote :value 'my-cat)
                  :initargs nil)
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-get-global))))

