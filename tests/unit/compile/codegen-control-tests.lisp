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

;;; ─── lookup-block ────────────────────────────────────────────────────────────

(deftest lookup-block-returns-exit-label-and-result-reg
  "lookup-block returns the (exit-label . result-reg) pair when the block is found."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (setf (cl-cc::ctx-block-env ctx)
          (list (cons 'my-block (cons "exit_0" :R3))))
    (let ((info (cl-cc::lookup-block ctx 'my-block)))
      (assert-equal "exit_0" (car info))
      (assert-eq :R3 (cdr info)))))

(deftest lookup-block-multiple-blocks-returns-correct-one
  "lookup-block finds the right entry when multiple blocks are registered."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (setf (cl-cc::ctx-block-env ctx)
          (list (cons 'outer (cons "outer_exit" :R0))
                (cons 'inner (cons "inner_exit" :R1))))
    (let ((info (cl-cc::lookup-block ctx 'inner)))
      (assert-equal "inner_exit" (car info))
      (assert-eq :R1 (cdr info)))))

(deftest lookup-block-signals-error-for-unknown-block
  "lookup-block signals an error when the block name is not in block-env."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-signals error
      (cl-cc::lookup-block ctx 'nonexistent-block))))

;;; ─── lookup-tag ──────────────────────────────────────────────────────────────

(deftest lookup-tag-returns-label-when-found
  "lookup-tag returns the label string for a registered tag."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (setf (cl-cc::ctx-tagbody-env ctx)
          (list (cons 'loop-start "tag_0")
                (cons 'loop-end   "tag_1")))
    (assert-equal "tag_0" (cl-cc::lookup-tag ctx 'loop-start))
    (assert-equal "tag_1" (cl-cc::lookup-tag ctx 'loop-end))))

(deftest lookup-tag-signals-error-for-unknown-tag
  "lookup-tag signals an error when the tag is not in tagbody-env."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-signals error
      (cl-cc::lookup-tag ctx 'missing-tag))))

(deftest lookup-tag-shadowed-returns-innermost
  "lookup-tag returns the first (innermost) binding when tags share a name."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (setf (cl-cc::ctx-tagbody-env ctx)
          (list (cons 'retry "inner_tag_5")
                (cons 'retry "outer_tag_1")))
    (assert-equal "inner_tag_5" (cl-cc::lookup-tag ctx 'retry))))

;;; ─── type-error-message-from-mismatch ────────────────────────────────────────

(deftest type-error-message-contains-expected-and-got
  "type-error-message-from-mismatch formats a string containing 'expected' and 'got'."
  (let* ((expected-type (cl-cc/type:parse-type-specifier 'integer))
         (actual-type   (cl-cc/type:parse-type-specifier 'string))
         (err (make-condition 'cl-cc/type:type-mismatch-error
                              :expected expected-type
                              :actual   actual-type))
         (msg (cl-cc::type-error-message-from-mismatch err)))
    (assert-true (stringp msg))
    (assert-true (search "expected" msg))
    (assert-true (search "got" msg))))
