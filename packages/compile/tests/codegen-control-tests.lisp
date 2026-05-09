;;;; tests/unit/compile/codegen-control-tests.lisp — Codegen control-flow tests

(in-package :cl-cc/test)
(in-suite cl-cc-codegen-unit-serial-suite)

;;; ─── compile-ast: ast-block / ast-tagbody / ast-go ───────────────────────────

(deftest codegen-block-compilation
  "Compiling a block returns a register and emits an exit label."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-block :name 'my-block
                                            :body (list (make-ast-int :value 42)))
                           ctx)))
    (assert-true (keywordp reg))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-label))))

(deftest codegen-tagbody-compilation
  "Compiling a tagbody emits a label per tag and returns a register."
  (let* ((ctx  (make-codegen-ctx))
         (reg  (compile-ast (make-ast-tagbody
                              :tags (list (cons 'tag1 (list (make-ast-int :value 1)))
                                          (cons 'tag2 (list (make-ast-int :value 2)))))
                            ctx))
         (labels (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-label))
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
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-establish-catch))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-label))
    (assert-true (keywordp reg))))

(deftest codegen-throw-compiles-tag-and-value
  "Compiling throw emits vm-throw with tag and value registers."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-throw
                   :tag (make-ast-quote :value 'my-tag)
                   :value (make-ast-int :value 42))
                  ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-throw))
    (let* ((insts (codegen-instructions ctx))
           (consts (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-const)) insts)))
      (assert-true (>= (length consts) 2)))))

;;; ─── lookup-block ────────────────────────────────────────────────────────────

(deftest lookup-block-finds-single-block
  "lookup-block returns exit-label and register for a single registered block."
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (setf (cl-cc/compile:ctx-block-env ctx)
          (list (cons 'my-block (cons "exit_0" :R3))))
    (let ((info (cl-cc/compile::lookup-block ctx 'my-block)))
      (assert-equal "exit_0" (car info))
      (assert-eq :R3 (cdr info)))))

(deftest lookup-block-finds-correct-in-multi-block-env
  "lookup-block finds the inner block by name in a multi-block environment."
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (setf (cl-cc/compile:ctx-block-env ctx)
          (list (cons 'outer (cons "outer_exit" :R0))
                (cons 'inner (cons "inner_exit" :R1))))
    (let ((info (cl-cc/compile::lookup-block ctx 'inner)))
      (assert-equal "inner_exit" (car info))
      (assert-eq :R1 (cdr info)))))

(deftest lookup-block-signals-for-unknown-name
  "lookup-block signals an error when no block with the given name is in the environment."
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (assert-signals error
      (cl-cc/compile::lookup-block ctx 'nonexistent-block))))

;;; ─── lookup-tag ──────────────────────────────────────────────────────────────

(deftest lookup-tag-returns-label-for-known-tag
  "lookup-tag returns the label string for each known tag name."
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (setf (cl-cc/compile:ctx-tagbody-env ctx)
          (list (cons 'loop-start "tag_0")
                (cons 'loop-end   "tag_1")))
    (assert-equal "tag_0" (cl-cc/compile::lookup-tag ctx 'loop-start))
    (assert-equal "tag_1" (cl-cc/compile::lookup-tag ctx 'loop-end))))

(deftest lookup-tag-signals-for-unknown-tag
  "lookup-tag signals an error when the tag name is not in the environment."
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (assert-signals error
      (cl-cc/compile::lookup-tag ctx 'missing-tag))))

(deftest lookup-tag-shadowed-returns-innermost
  "lookup-tag returns the innermost (first) label when the same tag name appears multiple times."
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (setf (cl-cc/compile:ctx-tagbody-env ctx)
          (list (cons 'retry "inner_tag_5")
                (cons 'retry "outer_tag_1")))
    (assert-equal "inner_tag_5" (cl-cc/compile::lookup-tag ctx 'retry))))

;;; ─── type-error-message-from-mismatch ────────────────────────────────────────

(deftest type-error-message-contains-expected-and-got
  "type-error-message-from-mismatch formats a string containing 'expected' and 'got'."
  (let* ((expected-type (cl-cc/type:parse-type-specifier 'integer))
         (actual-type   (cl-cc/type:parse-type-specifier 'string))
         (err (make-condition 'cl-cc/type:type-mismatch-error
                              :expected expected-type
                              :actual   actual-type))
         (msg (cl-cc/compile::type-error-message-from-mismatch err)))
    (assert-true (stringp msg))
    (assert-true (search "expected" msg))
    (assert-true (search "got" msg))))

;;; ─── compile-ast: ast-return-from ────────────────────────────────────────────

(deftest codegen-return-from-emits-move-and-jump
  "return-from compiles to a vm-move followed by a vm-jump to the block's exit label."
  (let* ((ctx (make-codegen-ctx))
         (exit-label "exit_test_0")
         (result-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-block-env ctx)
          (list (cons 'outer (cons exit-label result-reg))))
    (compile-ast (make-ast-return-from :name 'outer
                                       :value (make-ast-int :value 99))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-move))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-jump))))

(deftest codegen-return-from-inside-block-exits-at-correct-label
  "return-from inside an ast-block emits at least one jump to the block exit."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-block
                :name 'exit-test
                :body (list (make-ast-return-from :name 'exit-test
                                                   :value (make-ast-int :value 42))))
               ctx)))
    (assert-true (keywordp reg))
    (let ((jumps (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-jump))
                                (codegen-instructions ctx))))
      (assert-true (>= (length jumps) 1)))))

;;; ─── compile-ast: ast-go ─────────────────────────────────────────────────────

(deftest codegen-go-emits-vm-jump-to-named-tag
  "compile-ast of ast-go emits a vm-jump targeting the registered tag label."
  (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile:ctx-tagbody-env ctx)
          (list (cons 'loop-start "tag_loop_start_0")))
    (compile-ast (make-ast-go :tag 'loop-start) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-jump))))

(deftest codegen-go-inside-tagbody-emits-multiple-jumps
  "compile-ast of a tagbody with a go emits at least two vm-jumps (one for the go, one for the fallthrough)."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-tagbody
                :tags (list (cons 'start (list (make-ast-go :tag 'end)))
                            (cons 'end   (list (make-ast-int :value 0)))))
               ctx)))
    (assert-true (keywordp reg))
    (let ((jumps (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-jump))
                                (codegen-instructions ctx))))
      (assert-true (>= (length jumps) 2)))))

;;; ─── %emit-the-runtime-assertion ─────────────────────────────────────────────

(deftest emit-the-runtime-assertion-emits-vm-typep-for-non-trivial-type
  "%emit-the-runtime-assertion emits vm-typep when type is non-trivial and safety > 0."
  (let* ((ctx (make-codegen-ctx))  ; ctx-safety defaults to 1
         (value-reg (cl-cc/compile:make-register ctx)))
    (cl-cc/compile::%emit-the-runtime-assertion ctx value-reg 'integer)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-typep))))

(deftest-each emit-the-runtime-assertion-skip-cases
  "%emit-the-runtime-assertion skips vm-typep for T type, nil type, or when safety=0."
  :cases (("t-type"      't       nil)
          ("nil-type"    nil      nil)
          ("safety-zero" 'integer 0))
  (ty safety-override)
  (let* ((ctx (make-codegen-ctx))
         (value-reg (cl-cc/compile:make-register ctx)))
    (when safety-override (setf (cl-cc/compile:ctx-safety ctx) safety-override))
    (cl-cc/compile::%emit-the-runtime-assertion ctx value-reg ty)
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-typep))))

(deftest emit-the-runtime-assertion-no-failure-p-emits-typep-only
  "%emit-the-runtime-assertion with :emit-failure-p nil emits vm-typep but not vm-signal-error."
  (let* ((ctx (make-codegen-ctx))
         (value-reg (cl-cc/compile:make-register ctx)))
    (cl-cc/compile::%emit-the-runtime-assertion ctx value-reg 'string :emit-failure-p nil)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-typep))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-type-error-condition))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-signal-error))))

(deftest emit-the-runtime-assertion-failure-p-emits-type-error-condition-and-signal-error
  "%emit-the-runtime-assertion with :emit-failure-p t emits vm-typep, vm-type-error-condition, and vm-signal-error."
  (let* ((ctx (make-codegen-ctx))
         (value-reg (cl-cc/compile:make-register ctx)))
    (cl-cc/compile::%emit-the-runtime-assertion ctx value-reg 'integer :emit-failure-p t)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-typep))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-type-error-condition))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-signal-error))))

;;; ─── compile-ast: ast-the ────────────────────────────────────────────────────

(deftest codegen-the-with-declared-integer-type-emits-typep
  "Compiling (the integer val) emits vm-typep (runtime type assertion) when safety > 0."
  (let* ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc:make-ast-the :type 'integer :value (make-ast-int :value 42)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-typep))))

(deftest codegen-the-with-local-defun-safety-zero-skips-typep
  "A defun-local (optimize (safety 0)) suppresses runtime vm-typep assertions."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc/ast:make-ast-defun :name 'safe-zero-defun
                                 :params nil
                                 :declarations '((optimize (safety 0)))
                                 :body (list (make-ast-the :type 'integer
                                                           :value (make-ast-int :value 42))))
                 ctx)
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-typep))))

(deftest codegen-the-with-local-let-safety-zero-skips-typep
  "A let-local (optimize (safety 0)) suppresses runtime vm-typep assertions inside the body."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-let :bindings nil
                               :declarations '((optimize (safety 0)))
                               :body (list (make-ast-the :type 'integer
                                                         :value (make-ast-int :value 42))))
                 ctx)
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-typep))))

(deftest-each codegen-let-optimize-inline-policy-propagates-to-lambda-closure
  "Let-local optimize qualities map onto existing closure inline policy metadata."
  :cases (("speed-three" '((optimize (speed 3))) :inline)
          ("debug-three" '((optimize (debug 3))) :notinline)
          ("space-two" '((optimize (space 2))) :notinline))
  (declarations expected-policy)
  (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-let
               :bindings (list (cons 'f (make-ast-lambda :params '(x)
                                                         :body (list (make-ast-var :name 'x)))))
               :declarations declarations
               :body (list (make-ast-var :name 'f)))))
    (compile-ast ast ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-closure)))
      (assert-true inst)
      (assert-eq expected-policy (cl-cc/vm:vm-closure-inline-policy inst)))))
