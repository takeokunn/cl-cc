(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest-each framework-meta-tree-map-cases
  "%tree-map applies leaf-fn to every atom, rebuilding cons structure unchanged."
  :cases (("atom-identity"    42                   :MAPPED)
          ("atom-transform"   'x                   :MAPPED)
          ("flat-list"        '(a b c)             '(:MAPPED :MAPPED :MAPPED))
          ("nested-list"      '(a (b c))           '(:MAPPED (:MAPPED :MAPPED)))
          ("dotted-pair"      '(a . b)             '(:MAPPED . :MAPPED)))
  (input expected)
  (assert-equal expected (%tree-map input (lambda (atom) (declare (ignore atom)) :MAPPED))))

(deftest framework-meta-substitute-symbol-rewrites-occurrences
  "%substitute-symbol rewrites all occurrences of a symbol without touching unrelated atoms."
  (assert-equal '(let ((y 1)) (+ y z))
                (%substitute-symbol '(let ((x 1)) (+ x z)) 'x 'y)))

(deftest framework-meta-substitute-constant-rewrites-occurrences
  "%substitute-constant rewrites all occurrences of a constant without touching other atoms."
  (assert-equal '(if (= x 1) 1 2)
                (%substitute-constant '(if (= x 0) 0 2) 0 1)))

(deftest-each framework-meta-negate-first-if-condition
  "%negate-first-if-condition wraps the test in NOT, handles nested IF, passes non-IF through."
  :cases (("simple-if"  '(if test then else)         '(if (not test) then else))
          ("nested-if"  '((if inner-test then else)) '((if (not inner-test) then else)))
          ("no-if"      '(+ 1 2)                     '(+ 1 2)))
  (input expected)
  (assert-equal expected (%negate-first-if-condition input)))

(deftest-each framework-meta-return-nil-body
  "%return-nil-body rewrites defun/defmethod body to nil; passes non-binding forms through."
  :cases (("defun-rewrites"     '(defun sample (x) (+ x 1))         '(defun sample (x) nil))
          ("defmethod-rewrites"  '(defmethod draw ((s shape)) (print s)) '(defmethod draw ((s shape)) nil))
          ("non-defun-passthrough" '(if test then else)               '(if test then else)))
  (input expected)
  (assert-equal expected (%return-nil-body input)))

(deftest-each framework-meta-apply-mutation-produces-mutants
  "Mutation operators emit concrete mutant forms for representative inputs."
  :cases (("arithmetic-swap" '(defun sample (x) (+ x 1)) :arithmetic-swap)
          ("condition-negate" '(if test then else) :condition-negate)
          ("boundary-shift" '(< x 10) :boundary-shift)
          ("constant-replace" '(list 0 1) :constant-replace)
          ("return-nil" '(defun sample (x) (+ x 1)) :return-nil))
  (form mutation-type)
  (assert-true (consp (%apply-mutation form mutation-type))))

(deftest framework-meta-read-all-forms-collects-top-level-forms
  "%read-all-forms parses a string into multiple top-level form plists."
  (let ((forms (%read-all-forms "(defun foo () 1) (defun bar () 2)")))
    (assert-= 2 (length forms))
    (assert-eq 'defun (caar forms))
    (assert-string= "FOO" (symbol-name (cadar forms)))
    (assert-eq 'defun (caadr forms))
    (assert-string= "BAR" (symbol-name (cadadr forms)))))

(deftest framework-meta-eval-form-safely-returns-t-or-nil
  "%eval-form-safely returns T for successful evaluation and NIL for signaled errors."
  (assert-true  (%eval-form-safely '(+ 1 2)))
  (assert-false (%eval-form-safely '(error "boom"))))

(deftest framework-meta-binary-assertion-failure-message-has-name-expected-actual
  "A failing assert-equal produces a message with the assertion name, expected, and actual values."
  (handler-case
      (progn (assert-equal 1 2) (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-equal failed" message))
        (assert-true (search "expected: 1" message))
        (assert-true (search "actual: 2" message))))))

(deftest framework-meta-binary-assertion-evaluates-each-operand-once
  "assert-equal evaluates both the expected and actual operands exactly once."
  (let ((calls 0))
    (handler-case
        (progn
          (assert-equal (progn (incf calls) 1)
                        (progn (incf calls) 2))
          (assert-false t))
      (test-failure ()
        (assert-= 2 calls)))))

(deftest framework-meta-unary-assertion-failure-message-has-name-and-actual
  "A failing assert-null produces a message with the assertion name and actual value."
  (handler-case
      (progn (assert-null :not-nil) (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-null failed" message))
        (assert-true (search "actual: :NOT-NIL" message))))))

(deftest framework-meta-unary-assertion-macroexpands-through-assert-unary
  "assert-null macroexpands to %assert-unary with the null predicate."
  (let ((expanded (macroexpand-1 '(assert-null sample-form))))
    (assert-eq '%assert-unary (first expanded))
    (assert-eq 'null (second expanded))))

(deftest framework-meta-assert-run=-failure-message-is-readable
  "assert-run= produces a readable failure message including the expected value and source form."
  (handler-case
      (progn (assert-run= 7 "(+ 1 2)") (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-run=: expected 7" message))
        (assert-true (search "(+ 1 2)" message))))))

(deftest framework-meta-assert-run=-macroexpands-through-with-run-string-assertion
  "assert-run= macroexpands to %with-run-string-assertion."
  (let ((expanded (macroexpand-1 '(assert-run= 7 "(+ 1 2)"))))
    (assert-eq '%with-run-string-assertion (first expanded))))

(deftest framework-meta-assert-run=-reports-host-errors-in-failure-message
  "assert-run= reports host-signaled errors as a readable TAP YAML failure."
  (flet ((run-string (expr) (declare (ignore expr)) (error "synthetic host failure")))
    (handler-case
        (progn (assert-run= 1 "(boom)") (assert-false t))
      (test-failure (c)
        (let ((message (test-failure-message c)))
          (assert-true (search "run-string signaled" message))
          (assert-true (search "form: (ASSERT-RUN= 1 (boom))" message)))))))


(deftest framework-meta-assert-compiles-to-behavior
  "assert-compiles-to succeeds when the requested VM op is present in the compiled stream; gives readable failure when absent."
  (assert-compiles-to "(+ 1 2)" :contains 'vm-const)
  (handler-case
      (progn
        (assert-compiles-to "(+ 1 2)" :contains 'vm-sub)
        (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-compiles-to" message))
        (assert-true (search "VM-SUB" message))))))

(deftest framework-meta-assert-evaluates-to-reports-mismatch
  "assert-evaluates-to emits a readable failure when runtime results differ."
  (handler-case
      (progn
        (assert-evaluates-to "(+ 1 2)" 99)
        (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-evaluates-to" message))
        (assert-true (search "expected 99" message))))))

(deftest framework-meta-assert-run-string=-failure-on-non-string-result
  "assert-run-string= fails with a readable message when the result is not a string."
  (handler-case
      (progn
        (assert-run-string= "3" "(+ 1 2)")
        (assert-false t))
    (test-failure (c)
      (assert-true (search "assert-run-string=" (test-failure-message c))))))

(deftest framework-meta-assert-macro-expands-to-reports-form-and-mismatch
  "assert-macro-expands-to failure message includes the original form and mismatch detail."
  (handler-case
      (progn
        (assert-macro-expands-to '(when t 1) '(if nil 1 nil))
        (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-macro-expands-to" message))
        (assert-true (search "(WHEN T 1)" message))))))

(deftest framework-meta-assert-infers-type-succeeds-for-fixnum
  "assert-infers-type passes when the inferred type matches fixnum."
  (assert-infers-type "42" fixnum))

(deftest framework-meta-assert-infers-type-failure-reports-mismatch
  "assert-infers-type failure message includes the assertion name and the expected type."
  (handler-case
      (progn (assert-infers-type "42" string) (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-infers-type" message))
        (assert-true (search "STRING" message))))))

(deftest-each framework-meta-assertion-name-in-failure-message
  "Failing assertions include their own name in the failure message."
  :cases (("run-true"       (lambda () (assert-run-true "nil"))               "assert-run-true"       nil)
          ("run-false"      (lambda () (assert-run-false "42"))               "assert-run-false"      nil)
          ("run-signals"    (lambda () (assert-run-signals error "42"))       "assert-run-signals"    nil)
          ("output-contains" (lambda () (assert-output-contains "abcdef" "zzz")) "assert-output-contains" "zzz"))
  (thunk name-fragment extra-fragment)
  (handler-case
      (progn (funcall thunk) (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search name-fragment message))
        (when extra-fragment (assert-true (search extra-fragment message)))))))

(deftest framework-meta-defmetamorphic-registers-relation
  "defmetamorphic adds a relation entry to *metamorphic-relations* with the given name."
  (let ((*metamorphic-relations* nil))
    (defmetamorphic test-relation
      :transform (lambda (expr) expr)
      :relation #'equal
      :applicable-when (lambda (expr) (declare (ignore expr)) t))
    (assert-= 1 (length *metamorphic-relations*))
    (assert-eq 'test-relation (getf (first *metamorphic-relations*) :name))))

(deftest framework-meta-mutant-killed-p-returns-true-for-eval-failure
  "%mutant-killed-p returns T when the mutant form signals an error during evaluation."
  (assert-true (%mutant-killed-p '(error "synthetic eval failure")
                                 'cl-cc-unit-suite)))

(deftest framework-meta-coverage-helpers-are-callable
  "enable-coverage and disable-coverage are callable; %print-coverage-report signals with readable message."
  (enable-coverage)
  (disable-coverage)
  (handler-bind ((warning #'muffle-warning))
    (handler-case
        (assert-equal nil (%print-coverage-report nil))
      (error (e)
        (assert-true (search "Coverage report" (princ-to-string e)))))))

(deftest framework-meta-assert-evaluates-to-stdlib-path
  "assert-evaluates-to supports the stdlib execution path through a shared high-level assertion."
  (assert-evaluates-to "(+ 1 2)" 3 :stdlib t))

(deftest framework-meta-with-reset-repl-state-restores-package
  "with-reset-repl-state restores the caller package after BODY mutates *package*."
  (let ((original-package *package*))
    (with-reset-repl-state
      (setf *package* (find-package :cl-cc/compile))
      (assert-eq (find-package :cl-cc/compile) *package*))
    (assert-eq original-package *package*)))

(deftest framework-meta-deftest-compile-stdlib-expands-to-shared-assertion
  "deftest-compile stdlib cases macroexpand into assert-evaluates-to instead of duplicating boilerplate."
  (let ((expanded (macroexpand-1
                   '(deftest-compile sample "doc"
                      :cases (("basic" 3 "(+ 1 2)"))
                      :stdlib t))))
    (assert-eq 'progn (car expanded))
    (assert-true (search "ASSERT-EVALUATES-TO" (prin1-to-string expanded)))))
