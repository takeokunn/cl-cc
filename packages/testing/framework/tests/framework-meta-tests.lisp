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

(deftest framework-meta-substitute-symbol-rewrites-recursively
  "Symbol substitution rewrites nested occurrences without touching unrelated atoms."
  (assert-equal '(let ((y 1)) (+ y z))
                (%substitute-symbol '(let ((x 1)) (+ x z)) 'x 'y)))

(deftest framework-meta-substitute-constant-rewrites-literals
  "Constant substitution rewrites matching literals recursively."
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
  "The source reader collects multiple top-level forms from a string."
  (let ((forms (%read-all-forms "(defun foo () 1) (defun bar () 2)")))
    (assert-= 2 (length forms))
    (assert-eq 'defun (caar forms))
    (assert-string= "FOO" (symbol-name (cadar forms)))
    (assert-eq 'defun (caadr forms))
    (assert-string= "BAR" (symbol-name (cadadr forms)))))

(deftest framework-meta-eval-form-safely
  "%eval-form-safely returns T for evaluable forms and NIL for errors."
  (assert-true (%eval-form-safely '(+ 1 2)))
  (assert-false (%eval-form-safely '(error "boom"))))

(deftest framework-meta-generated-binary-assertion-preserves-failure-payload
  "Generated binary assertions keep the same readable failure payload."
  (handler-case
      (progn
        (assert-equal 1 2)
        (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-equal failed" message))
        (assert-true (search "expected: 1" message))
        (assert-true (search "actual: 2" message))))))

(deftest framework-meta-generated-binary-assertion-single-evaluation
  "Generated binary assertions evaluate each operand exactly once."
  (let ((calls 0))
    (handler-case
        (progn
          (assert-equal (progn (incf calls) 1)
                        (progn (incf calls) 2))
          (assert-false t))
      (test-failure ()
        (assert-= 2 calls)))))

(deftest framework-meta-generated-unary-assertion-preserves-failure-payload
  "Generated unary assertions keep their original failure wording and actual value."
  (handler-case
      (progn
        (assert-null :not-nil)
        (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-null failed" message))
        (assert-true (search "actual: :NOT-NIL" message))))))

(deftest framework-meta-generated-unary-assertion-expands-through-shared-helper
  "Generated unary assertions macroexpand through %assert-unary for shared semantics."
  (let ((expanded (macroexpand-1 '(assert-null sample-form))))
    (assert-eq '%assert-unary (first expanded))
    (assert-eq 'null (second expanded))))

(deftest framework-meta-assert-run=-reports-readable-failure
  "assert-run= still reports a readable mismatch after helper extraction."
  (handler-case
      (progn
        (assert-run= 7 "(+ 1 2)")
        (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-run=: expected 7" message))
        (assert-true (search "(+ 1 2)" message))))))

(deftest framework-meta-assert-run=-swallows-host-errors-into-readable-failure
  "assert-run= keeps converting host RUN-STRING errors into assertion failures."
  (flet ((run-string (expr)
           (declare (ignore expr))
           (error "synthetic host failure")))
    (handler-case
        (progn
          (assert-run= 1 "(boom)")
          (assert-false t))
      (test-failure (c)
        (let ((message (test-failure-message c)))
          (assert-true (search "assert-run=: expected 1" message))
          (assert-true (search "form: (ASSERT-RUN= 1 (boom))" message)))))))

(deftest framework-meta-assert-run-string=-guards-non-string-results
  "assert-run-string= fails cleanly when the evaluated form is not a string."
  (handler-case
      (progn
        (assert-run-string= "3" "(+ 1 2)")
        (assert-false t))
    (test-failure ()
      t)))

(deftest framework-meta-assert-compiles-to-accepts-matching-instruction
  "assert-compiles-to succeeds when the compiled instruction stream contains the requested VM op." 
  (assert-compiles-to "(defun typed-add ((x fixnum) (y fixnum)) fixnum (+ x y))" :contains 'vm-add))

(deftest framework-meta-assert-compiles-to-reports-mismatch
  "assert-compiles-to raises a readable failure when the requested VM op is absent."
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

(deftest framework-meta-assert-macro-expands-to-reports-mismatch
  "assert-macro-expands-to reports both the form and mismatch cleanly."
  (handler-case
      (progn
        (assert-macro-expands-to '(when t 1) '(if nil 1 nil))
        (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-macro-expands-to" message))
        (assert-true (search "(WHEN T 1)" message))))))

(deftest framework-meta-assert-infers-type-accepts-primitive-type-name
  "assert-infers-type succeeds for a straightforward primitive inference result."
  (assert-infers-type "42" fixnum))

(deftest framework-meta-assert-infers-type-reports-mismatch
  "assert-infers-type emits a readable failure when the inferred type differs."
  (handler-case
      (progn
        (assert-infers-type "42" string)
        (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-infers-type" message))
        (assert-true (search "STRING" message))))))

(deftest framework-meta-assert-run-true-reports-nil-result
  "assert-run-true fails readably when the expression evaluates to NIL."
  (handler-case
      (progn
        (assert-run-true "nil")
        (assert-false t))
    (test-failure (c)
      (assert-true (search "assert-run-true" (test-failure-message c))))))

(deftest framework-meta-assert-run-false-reports-truthy-result
  "assert-run-false fails readably when the expression evaluates truthy."
  (handler-case
      (progn
        (assert-run-false "42")
        (assert-false t))
    (test-failure (c)
      (assert-true (search "assert-run-false" (test-failure-message c))))))

(deftest framework-meta-assert-run-signals-fails-when-no-condition-occurs
  "assert-run-signals fails readably when the expected condition is not signaled."
  (handler-case
      (progn
        (assert-run-signals error "42")
        (assert-false t))
    (test-failure (c)
      (assert-true (search "assert-run-signals" (test-failure-message c))))))

(deftest framework-meta-assert-output-contains-reports-missing-substring
  "assert-output-contains emits a readable failure when the substring is absent."
  (handler-case
      (progn
        (assert-output-contains "abcdef" "zzz")
        (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-output-contains" message))
        (assert-true (search "zzz" message))))))

(deftest framework-meta-defmetamorphic-registers-relation
  "defmetamorphic appends a relation descriptor to *metamorphic-relations*."
  (let ((*metamorphic-relations* nil))
    (defmetamorphic test-relation
      :transform (lambda (expr) expr)
      :relation #'equal
      :applicable-when (lambda (expr) (declare (ignore expr)) t))
    (assert-= 1 (length *metamorphic-relations*))
    (assert-eq 'test-relation (getf (first *metamorphic-relations*) :name))))

(deftest framework-meta-enable-disable-coverage-smoke
  "Coverage toggles are callable in the current image."
  (enable-coverage)
  (disable-coverage)
  (assert-true t))

(deftest framework-meta-print-coverage-report-smoke
  "%print-coverage-report emits the expected report-path banner."
  (let ((*standard-output* (make-string-output-stream)))
    (%print-coverage-report nil)
    (let ((output (get-output-stream-string *standard-output*)))
      (assert-true (search "cl-cc-coverage" output)))))

(deftest framework-meta-mutant-killed-by-compile-error
  "A mutant that fails to eval is counted as killed immediately.
   Uses a form that signals at top-level eval so %eval-form-safely
   returns NIL; otherwise %mutant-killed-p would fall through to
   re-running every test in the suite — including this one —
   causing unbounded recursion."
  (assert-true (%mutant-killed-p '(error "synthetic eval failure")
                                 'cl-cc-unit-suite)))
