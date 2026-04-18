(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest framework-meta-substitute-symbol-rewrites-recursively
  "Symbol substitution rewrites nested occurrences without touching unrelated atoms."
  (assert-equal '(let ((y 1)) (+ y z))
                (%substitute-symbol '(let ((x 1)) (+ x z)) 'x 'y)))

(deftest framework-meta-substitute-constant-rewrites-literals
  "Constant substitution rewrites matching literals recursively."
  (assert-equal '(if (= x 1) 1 2)
                (%substitute-constant '(if (= x 0) 0 2) 0 1)))

(deftest framework-meta-negate-first-if-condition-wraps-not
  "The first IF condition is negated while the rest of the form is preserved."
  (assert-equal '(if (not test) then else)
                (%negate-first-if-condition '(if test then else))))

(deftest framework-meta-return-nil-body-rewrites-defun
  "Return-nil mutation preserves the defun header and replaces the body with NIL."
  (assert-equal '(defun sample (x) nil)
                (%return-nil-body '(defun sample (x) (+ x 1)))))

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

;;; P7: %print-mutation-report — unit test for the report printer

(deftest framework-meta-print-mutation-report-empty
  "%print-mutation-report with no records reports 100% mutation score."
  (let ((*standard-output* (make-string-output-stream)))
    (multiple-value-bind (score killed total)
        (%print-mutation-report nil)
      (assert-= 100.0 score)
      (assert-= 0 killed)
      (assert-= 0 total)
      (let ((output (get-output-stream-string *standard-output*)))
        (assert-true (search "Mutation Testing Report" output))))))

(deftest framework-meta-print-mutation-report-killed
  "%print-mutation-report with a killed mutant reports 100% score."
  (let* ((rec (make-mutation-record
                :source-location "test.lisp:1"
                :mutation-type :constant-replace
                :original-form '(+ 1 2)
                :mutant-form '(+ 0 2)
                :killed t))
         (*standard-output* (make-string-output-stream)))
    (multiple-value-bind (score killed total)
        (%print-mutation-report (list rec))
      (assert-= 100.0 score)
      (assert-= 1 killed)
      (assert-= 1 total))))

(deftest framework-meta-print-mutation-report-survivor
  "%print-mutation-report with a surviving mutant reports 0% score and lists survivors."
  (let* ((rec (make-mutation-record
                :source-location "test.lisp:1"
                :mutation-type :condition-negate
                :original-form '(if t 1 2)
                :mutant-form '(if (not t) 1 2)
                :killed nil))
         (*standard-output* (make-string-output-stream)))
    (multiple-value-bind (score killed total)
        (%print-mutation-report (list rec))
      (declare (ignore score))
      (assert-= 0 killed)
      (assert-= 1 total)
      (let ((output (get-output-stream-string *standard-output*)))
        (assert-true (search "Survivors" output))))))

;;; P8: %verify-metamorphic-relations — calls through the relation pipeline

(deftest framework-meta-verify-metamorphic-no-violation
  "%verify-metamorphic-relations does not fail when relations hold."
  (let ((*metamorphic-relations*
          (list (list :name 'commutativity-check
                      :transform (lambda (expr) `(,(car expr) ,(caddr expr) ,(cadr expr)))
                      :relation #'=
                      :applicable-when (lambda (expr) (eq (car expr) '+))))))
    ;; Should not signal any failure
    (%verify-metamorphic-relations (list '(+ 1 2)))))

(deftest framework-meta-verify-metamorphic-violation-signals-failure
  "%verify-metamorphic-relations signals TEST-FAILURE when a relation is violated."
  (let ((*metamorphic-relations*
          (list (list :name 'bad-relation
                      :transform (lambda (expr) expr)
                      :relation (lambda (lhs rhs)
                                  (declare (ignore lhs rhs))
                                  nil)
                      :applicable-when (lambda (expr) (eq (car expr) '+))))))
    (handler-case
      (progn
        (%verify-metamorphic-relations (list '(+ 1 2)))
        (assert-false t))
      (test-failure (c)
        (assert-true (search "BAD-RELATION"
                             (string-upcase (test-failure-message c))))))))

;;; P9: nested negate + passthrough branches

(deftest framework-meta-negate-nested-if
  "Negate recurses into car position; a nested IF inside a list element is found."
  (assert-equal '((if (not inner-test) then else))
                (%negate-first-if-condition '((if inner-test then else)))))

(deftest framework-meta-negate-no-if
  "Negate returns the form unchanged when no IF is found."
  (assert-equal '(+ 1 2)
                (%negate-first-if-condition '(+ 1 2))))

(deftest framework-meta-return-nil-body-non-defun-passthrough
  "%return-nil-body returns non-defun/defmethod/defgeneric forms unchanged."
  (assert-equal '(if test then else)
                (%return-nil-body '(if test then else))))

(deftest framework-meta-return-nil-body-defmethod
  "%return-nil-body rewrites defmethod body to nil."
  (assert-equal '(defmethod draw ((s shape)) nil)
                (%return-nil-body '(defmethod draw ((s shape)) (print s)))))

(deftest framework-meta-run-mutation-test-validates-required-args
  "run-mutation-test rejects missing target/suite arguments."
  (handler-case
      (progn
        (run-mutation-test :suite 'cl-cc-unit-suite)
        (assert-false t))
    (error (e)
      (assert-true (search ":target" (princ-to-string e)))))
  (handler-case
      (progn
        (run-mutation-test :target "/tmp/does-not-matter.lisp")
        (assert-false t))
    (error (e)
      (assert-true (search ":suite" (princ-to-string e))))))

(deftest framework-meta-run-mutation-test-missing-file
  "run-mutation-test rejects nonexistent files before reading forms."
  (handler-case
      (progn
        (run-mutation-test :target "/tmp/definitely-missing-cl-cc-mutation.lisp"
                           :suite 'cl-cc-unit-suite)
        (assert-false t))
    (error (e)
      (assert-true (search "target file not found" (princ-to-string e))))))

(deftest framework-meta-run-mutation-test-empty-operator-set
  "run-mutation-test can execute its full reporting path with an empty mutation set."
  (let ((path "/tmp/cl-cc-mutation-smoke.lisp")
        (*standard-output* (make-string-output-stream)))
    (unwind-protect
         (progn
           (with-open-file (stream path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-line "(defun cl-cc-mutation-smoke (x) (+ x 1))" stream))
           (multiple-value-bind (score killed total)
               (run-mutation-test :target path
                                  :suite 'cl-cc-unit-suite
                                  :mutations '())
             (assert-= 100.0 score)
             (assert-= 0 killed)
             (assert-= 0 total)
             (assert-true (search "Mutation Testing Report"
                                  (get-output-stream-string *standard-output*)))))
      (ignore-errors (delete-file path)))))
