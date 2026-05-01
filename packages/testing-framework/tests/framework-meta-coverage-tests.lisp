(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

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

;;; P9: additional mutation operator coverage

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
