;;;; tests/unit/compile/codegen-type-predicate-tests.lisp — Type predicate codegen tests

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── %proven-fixnum-type-p / %proven-float-type-p / %float-literal-node-p ───

(deftest-each proven-type-p-cases
  "%proven-fixnum/float-type-p: true for matching specifier; false for mismatched or nil."
  :cases (("fixnum-proven"   #'cl-cc/compile::%proven-fixnum-type-p 'fixnum t)
          ("fixnum-nil"      #'cl-cc/compile::%proven-fixnum-type-p nil     nil)
          ("float-proven"    #'cl-cc/compile::%proven-float-type-p  'float  t)
          ("float-not-fixnum"#'cl-cc/compile::%proven-float-type-p  'fixnum nil))
  (pred-fn spec expected)
  (let ((ty (when spec (cl-cc/type:parse-type-specifier spec))))
    (if expected
        (assert-true  (funcall pred-fn ty))
        (assert-false (funcall pred-fn ty)))))

(deftest-each float-literal-node-p-cases
  "%float-literal-node-p identifies quoted float literals."
  :cases (("float-quote"   (make-ast-quote :value 3.14)  t)
          ("int-quote"     (make-ast-quote :value 42)    nil)
          ("non-quote-ast" (make-ast-int :value 1)       nil))
  (node expected)
  (if expected
      (assert-true  (cl-cc/compile::%float-literal-node-p node))
      (assert-false (cl-cc/compile::%float-literal-node-p node))))
