;;;; tests/unit/compile/codegen-control-tests.lisp — Codegen control-flow tests

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── compile-ast: ast-block / ast-tagbody / ast-go ───────────────────────────

(deftest codegen-block-compilation
  "Compiling a block returns a register and emits an exit label."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-block :name 'my-block
                                            :body (list (make-ast-int :value 42)))
                           ctx)))
    (assert-true (keywordp reg))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-label))))

(deftest codegen-tagbody-compilation
  "Compiling a tagbody emits a label per tag and returns a register."
  (let* ((ctx  (make-codegen-ctx))
         (reg  (compile-ast (make-ast-tagbody
                              :tags (list (cons 'tag1 (list (make-ast-int :value 1)))
                                          (cons 'tag2 (list (make-ast-int :value 2)))))
                            ctx))
         (labels (remove-if-not (lambda (i) (typep i 'cl-cc::vm-label))
                                (codegen-instructions ctx))))
    (assert-true (>= (length labels) 2))
    (assert-true (keywordp reg))))

(deftest codegen-catch-compilation
  "Compiling catch emits establish-catch, labels, and returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-catch
                             :tag  (make-ast-quote :value 'my-tag)
                             :body (list (make-ast-int :value 42)))
                           ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-establish-catch))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-label))
    (assert-true (keywordp reg))))

(deftest codegen-throw-compiles-tag-and-value
  "Compiling throw emits vm-throw with tag and value registers."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-throw
                   :tag (make-ast-quote :value 'my-tag)
                   :value (make-ast-int :value 42))
                  ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-throw))
    (let* ((insts (codegen-instructions ctx))
           (consts (remove-if-not (lambda (i) (typep i 'cl-cc::vm-const)) insts)))
      (assert-true (>= (length consts) 2)))))
