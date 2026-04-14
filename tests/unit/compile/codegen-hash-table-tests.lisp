;;;; tests/unit/compile/codegen-hash-table-tests.lisp — Hash-table codegen tests

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest-each codegen-make-hash-table-test-designators
  "make-hash-table accepts quoted, variable, and function test designators."
  :cases (("quoted"   (make-call 'make-hash-table (make-var :test) (make-quoted 'equal)))
          ("variable" (make-call 'make-hash-table (make-var :test) (make-var 'eql)))
          ("function" (make-call 'make-hash-table (make-var :test) (make-fn 'equalp))))
  (form)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast form ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-hash-table)))
      (assert-true inst)
      (assert-true (cl-cc::vm-make-hash-table-test inst)))))

(deftest codegen-gethash-emits-default-register
  "gethash emits vm-gethash and preserves the optional default register."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash
                            (make-quoted 'key)
                            (make-quoted 'table)
                            (make-int 0))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-gethash)))
      (assert-true inst)
      (assert-true (cl-cc::vm-gethash-default inst)))))

(deftest codegen-maphash-emits-loop-support
  "maphash emits table snapshot, call, and nil return value."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'maphash
                            (make-quoted 'fn)
                            (make-quoted 'ht))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-hash-table-keys))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-call))
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc::vm-const)
                              (null (cl-cc::vm-const-value i))))
                       (codegen-instructions ctx)))))
