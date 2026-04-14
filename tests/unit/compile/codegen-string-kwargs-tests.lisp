;;;; tests/unit/compile/codegen-string-kwargs-tests.lisp — String keyword-argument codegen tests

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest codegen-string-comparison-keywords-use-subseq
  "Keyworded string comparisons compile substring slices before the comparison."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'string=
                            (make-quoted "hello")
                            (make-quoted "yellow")
                            (make-var :start1) (make-int 1)
                            (make-var :end1)   (make-int 4)
                            (make-var :start2) (make-int 1)
                            (make-var :end2)   (make-int 4))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-subseq))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-string=))))

(deftest codegen-string-case-keywords-reconstruct-string
  "Keyworded string-upcase compiles prefix/slice/suffix reconstruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'string-upcase
                            (make-quoted "hello")
                            (make-var :start) (make-int 1)
                            (make-var :end)   (make-int 4))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-subseq))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-concatenate))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-string-upcase))))
