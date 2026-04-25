;;;; tests/integration/compiler-tests-runtime-hof-tests.lisp — HOF and Control Tests
;;;;
;;;; Continuation of compiler-tests-runtime-string-tests.lisp.
;;;; Tests for higher-order functions, ignore-errors, unwind-protect,
;;;; self-host CLOS, copy-hash-table, and cxr compositions.

(in-package :cl-cc/test)
(in-suite cl-cc-integration-suite)

(deftest-each stdlib-hof-basic
  "mapcar, reduce, and remove-if with stdlib loaded return correct values."
  :cases (("mapcar"    '(2 4 6) "(mapcar (lambda (x) (* x 2)) '(1 2 3))")
          ("reduce"    15       "(reduce #'+ '(1 2 3 4 5) :initial-value 0)")
          ("remove-if" '(2 4)   "(remove-if #'oddp '(1 2 3 4 5))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

;;; Let Alias Fix and Prog1/Prog2 Tests

(deftest-each let-no-alias-and-prog1-prog2
  "LET bindings are independent copies; prog1 returns first value; prog2 returns second."
  :cases (("let-simple"      0  "(let ((x 0)) (let ((y x)) (setq x 10) y))")
          ("let-nested"      5  "(let ((a 5)) (let ((b a)) (let ((c b)) (setq a 99) c)))")
          ("prog1-basic"     42 "(prog1 42 (+ 1 2))")
          ("prog1-side-eff"   0 "(let ((x 0)) (prog1 x (setq x 10)))")
          ("prog2"           42 "(prog2 1 42 3)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-ignore-errors
  "ignore-errors returns the value on success and nil on error."
  :cases (("success" 3   "(ignore-errors (+ 1 2))")
          ("failure" nil "(ignore-errors (error \"boom\"))"))
  (expected form)
  (assert-true (equal expected (run-string form))))

;;; Unwind-Protect Integration Tests

(deftest unwind-protect-cleanup-visible
  "unwind-protect cleanup side effects visible in handler-case"
  (assert-eq t (run-string "
(let ((cleaned nil)) (handler-case (unwind-protect (error \"boom\") (setf cleaned t)) (error (e) cleaned)))")))

;;; Self-Hosting CLOS Compiler Test

(deftest self-host-clos-compiler
  "Self-hosting: CLOS-based AST compiler with defgeneric/defmethod dispatch"
  (assert-equal '(:R2 3) (run-string *self-host-clos-compiler-program* :stdlib t)))

;;; Hash Table Extended Builtins Tests

(deftest compile-hash-table-values
  "hash-table-values returns list of values"
  (let ((result (run-string "(let ((ht (make-hash-table)))
  (setf (gethash :a ht) 1)
  (setf (gethash :b ht) 2)
  (hash-table-values ht))")))
    (assert-= 2 (length result))
    (assert-true (null (set-difference result '(1 2))))))

(deftest-each compile-hash-table-test
  "hash-table-test returns the test function name the table was created with."
  :cases (("default-eql"    'eql   "(hash-table-test (make-hash-table))")
          ("explicit-equal" 'equal "(hash-table-test (make-hash-table :test 'equal))"))
  (expected form)
  (assert-true (eq expected (run-string form))))

(deftest compile-copy-hash-table
  "copy-hash-table creates independent copy"
  (assert-= 42 (run-string "
(let ((ht (make-hash-table))) (setf (gethash :a ht) 42) (let ((ht2 (copy-hash-table ht))) (setf (gethash :a ht) 99) (gethash :a ht2)))")))

;;; Car/Cdr Composition and List Accessor Tests

(deftest-each cxr-compositions
  "c*r composition functions access deeply nested list elements correctly."
  :cases (("cadddr" "d" "(string-downcase (symbol-name (cadddr '(a b c d e))))")
          ("caadr"  "x" "(string-downcase (symbol-name (caadr '(a (x y) c))))")
          ("caddar" 3   "(caddar '((1 2 3) b c))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

;;; Self-hosting runtime-heavy integration tests moved to compiler-tests-runtime-selfhost.lisp.
