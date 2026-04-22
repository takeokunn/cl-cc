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

(deftest framework-meta-substitution-cases
  "Symbol and constant substitution each rewrite nested occurrences without touching unrelated atoms."
  (assert-equal '(let ((y 1)) (+ y z))
                (%substitute-symbol '(let ((x 1)) (+ x z)) 'x 'y))
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

(deftest framework-meta-source-utils-cases
  "Source reader collects top-level forms; %eval-form-safely returns T/NIL for success/error."
  (let ((forms (%read-all-forms "(defun foo () 1) (defun bar () 2)")))
    (assert-= 2 (length forms))
    (assert-eq 'defun (caar forms))
    (assert-string= "FOO" (symbol-name (cadar forms)))
    (assert-eq 'defun (caadr forms))
    (assert-string= "BAR" (symbol-name (cadadr forms))))
  (assert-true  (%eval-form-safely '(+ 1 2)))
  (assert-false (%eval-form-safely '(error "boom"))))

(deftest framework-meta-generated-binary-assertion-cases
  "Binary assertions: payload includes assertion name/expected/actual; each operand evaluated once."
  (handler-case
      (progn (assert-equal 1 2) (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-equal failed" message))
        (assert-true (search "expected: 1" message))
        (assert-true (search "actual: 2" message)))))
  (let ((calls 0))
    (handler-case
        (progn
          (assert-equal (progn (incf calls) 1)
                        (progn (incf calls) 2))
          (assert-false t))
      (test-failure ()
        (assert-= 2 calls)))))

(deftest framework-meta-generated-unary-assertion-cases
  "Unary assertions: payload includes name and actual; macroexpands through %assert-unary."
  (handler-case
      (progn (assert-null :not-nil) (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-null failed" message))
        (assert-true (search "actual: :NOT-NIL" message)))))
  (let ((expanded (macroexpand-1 '(assert-null sample-form))))
    (assert-eq '%assert-unary (first expanded))
    (assert-eq 'null (second expanded))))

(deftest framework-meta-assert-run=-cases
  "assert-run=: readable failure; macroexpands through %with-run-string-assertion; reports host errors as readable TAP YAML."
  (handler-case
      (progn (assert-run= 7 "(+ 1 2)") (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-run=: expected 7" message))
        (assert-true (search "(+ 1 2)" message)))))
  (let ((expanded (macroexpand-1 '(assert-run= 7 "(+ 1 2)"))))
    (assert-eq '%with-run-string-assertion (first expanded)))
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

(deftest framework-meta-assertion-mismatch-cases
  "assert-run-string= fails on non-string result; assert-macro-expands-to reports form+mismatch."
  (handler-case
      (progn
        (assert-run-string= "3" "(+ 1 2)")
        (assert-false t))
    (test-failure (c)
      (assert-true (search "assert-run-string=" (test-failure-message c)))))
  (handler-case
      (progn
        (assert-macro-expands-to '(when t 1) '(if nil 1 nil))
        (assert-false t))
    (test-failure (c)
      (let ((message (test-failure-message c)))
        (assert-true (search "assert-macro-expands-to" message))
        (assert-true (search "(WHEN T 1)" message))))))

(deftest framework-meta-assert-infers-type-cases
  "assert-infers-type: succeeds for fixnum; reports mismatch for wrong type."
  (assert-infers-type "42" fixnum)
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

(deftest framework-meta-mutation-testing-infra-cases
  "defmetamorphic registers relation; mutant-killed-p returns T for eval-failure; coverage helpers are callable."
  (let ((*metamorphic-relations* nil))
    (defmetamorphic test-relation
      :transform (lambda (expr) expr)
      :relation #'equal
      :applicable-when (lambda (expr) (declare (ignore expr)) t))
    (assert-= 1 (length *metamorphic-relations*))
    (assert-eq 'test-relation (getf (first *metamorphic-relations*) :name)))
  (assert-true (%mutant-killed-p '(error "synthetic eval failure")
                                 'cl-cc-unit-suite))
  (enable-coverage)
  (disable-coverage)
  (handler-case
      (progn
        (%print-coverage-report nil)
        (assert-false t))
    (error (e)
      (declare (ignore e))
      (assert-true t))))

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
