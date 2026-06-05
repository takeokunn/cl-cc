;;;; tests/unit/compile/codegen-hash-table-tests.lisp — Hash-table codegen tests

(in-package :cl-cc/test)
(in-suite cl-cc-codegen-unit-suite)

(deftest-each codegen-make-hash-table-test-designators
  "make-hash-table statically accepts quoted and function test designators."
  :cases (("quoted"   (make-call 'make-hash-table (make-var :test) (make-quoted 'equal)))
          ("function" (make-call 'make-hash-table (make-var :test) (make-fn 'equalp))))
  (form)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast form ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
      (assert-true inst)
      (assert-true (cl-cc::vm-make-hash-table-test inst)))))

(deftest codegen-make-hash-table-emits-size-option
  "make-hash-table compiles :size into the VM make-hash-table instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-hash-table
                            (make-var :test)
                            (make-quoted 'eql)
                            (make-var :size)
                            (make-int 100))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
      (assert-true inst)
      (assert-true (cl-cc/vm::vm-hash-size inst)))))

(deftest codegen-gethash-emits-default-register
  "gethash emits vm-gethash and preserves the optional default register."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash
                            (make-quoted 'key)
                            (make-quoted 'table)
                            (make-int 0))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-gethash)))
      (assert-true inst)
      (assert-true (cl-cc::vm-gethash-default inst)))))

(deftest-each codegen-gethash-specializes-direct-make-hash-table-test
  "gethash specializes direct make-hash-table forms with static EQ/EQL/EQUAL tests."
  :cases (("eq" 'eq 'cl-cc/vm::vm-gethash-eq)
          ("eql" 'eql 'cl-cc/vm::vm-gethash-eql)
          ("equal" 'equal 'cl-cc/vm::vm-gethash-equal))
  (test-sym inst-type)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash
                            (make-quoted 'key)
                            (make-call 'make-hash-table
                                       (make-var :test)
                                       (make-quoted test-sym)))
                 ctx)
    (assert-true (codegen-find-inst ctx inst-type))))

(deftest codegen-gethash-specializes-let-bound-static-hash-table
  "gethash specializes a lexical table whose make-hash-table test is static."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-let
                  :bindings (list (cons 'ht
                                        (make-call 'make-hash-table
                                                   (make-var :test)
                                                   (make-quoted 'equal))))
                  :body (list (make-call 'gethash
                                         (make-quoted "k")
                                         (make-var 'ht))))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-equal))))

(deftest codegen-gethash-keeps-generic-path-for-equalp
  "gethash keeps unsupported static tests on the generic vm-gethash path."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash
                            (make-quoted "k")
                            (make-call 'make-hash-table
                                       (make-var :test)
                                       (make-quoted 'equalp)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-gethash))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-equal))))

(deftest codegen-make-hash-table-dynamic-test-is-evaluated
  "make-hash-table evaluates dynamic :test expressions while gethash remains generic."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-let
                  :bindings (list (cons 'test (make-quoted 'equal)))
                  :body (list (make-ast-let
                               :bindings (list (cons 'ht
                                                     (make-call 'make-hash-table
                                                                (make-var :test)
                                                                (make-var 'test))))
                               :body (list (make-call 'gethash
                                                      (make-quoted "k")
                                                      (make-var 'ht))))))
                 ctx)
    (let ((make-inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
      (assert-true make-inst)
      (assert-true (cl-cc::vm-make-hash-table-test make-inst)))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-gethash))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-equal))))

(deftest codegen-make-hash-table-test-symbol-variable-is-dynamic
  "make-hash-table treats variable names like EQL as dynamic lexical references."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-let
                  :bindings (list (cons 'eql (make-quoted 'equal)))
                  :body (list (make-ast-let
                               :bindings (list (cons 'ht
                                                     (make-call 'make-hash-table
                                                                (make-var :test)
                                                                (make-var 'eql))))
                               :body (list (make-call 'gethash
                                                      (make-quoted "k")
                                                      (make-var 'ht))))))
                 ctx)
    (let ((make-inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
      (assert-true make-inst)
      (assert-true (cl-cc::vm-make-hash-table-test make-inst)))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-gethash))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-eql))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-equal))))

(deftest codegen-gethash-masks-shadowed-static-hash-binding
  "inner dynamic hash-table bindings shadow outer static hash tracking."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-let
                  :bindings (list (cons 'test (make-quoted 'equal))
                                  (cons 'ht
                                        (make-call 'make-hash-table
                                                   (make-var :test)
                                                   (make-quoted 'equal))))
                  :body (list (make-ast-let
                               :bindings (list (cons 'ht
                                                     (make-call 'make-hash-table
                                                                (make-var :test)
                                                                (make-var 'test))))
                               :body (list (make-call 'gethash
                                                      (make-quoted "k")
                                                      (make-var 'ht))))))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-gethash))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-eq))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-eql))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-equal))))

(deftest codegen-gethash-masks-shadowed-non-hash-binding
  "inner non-hash bindings shadow outer static hash tracking."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-let
                  :bindings (list (cons 'ht
                                        (make-call 'make-hash-table
                                                   (make-var :test)
                                                   (make-quoted 'equal))))
                  :body (list (make-ast-let
                               :bindings (list (cons 'ht (make-quoted 'not-a-table)))
                               :body (list (make-call 'gethash
                                                      (make-quoted "k")
                                                      (make-var 'ht))))))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-gethash))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-eq))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-eql))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-equal))))

(deftest codegen-maphash-emits-loop-support
  "maphash emits table snapshot, call, and nil return value."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'maphash
                            (make-quoted 'fn)
                            (make-quoted 'ht))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-hash-table-keys))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-call))
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (null (cl-cc::vm-const-value i))))
                       (codegen-instructions ctx)))))
