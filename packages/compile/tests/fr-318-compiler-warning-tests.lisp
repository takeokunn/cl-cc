;;;; packages/compile/tests/fr-318-compiler-warning-tests.lisp
;;;; FR-318: Compiler Warning System.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defun fr-318-warning-messages (warnings)
  (mapcar #'cl-cc/parse:diagnostic-message warnings))

(deftest fr-318-unused-let-binding-emits-structured-warning
  "FR-318: codegen records an unused lexical variable as a structured warning."
  :tags '(:fr-318)
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((let ((unused 1)) 42))
                    :target :vm))
           (warnings (cl-cc/compile:compilation-result-warnings result))
           (warning (first warnings)))
      (assert-= 1 (length warnings))
      (assert-eq :warning (cl-cc/parse:diagnostic-severity warning))
      (assert-equal "W0001" (cl-cc/parse:diagnostic-error-code warning))
      (assert-true (search "UNUSED" (cl-cc/parse:diagnostic-message warning)))
      (assert-null (cl-cc/compile:compilation-result-errors result)))))

(deftest fr-318-ignore-declaration-suppresses-unused-warning
  "FR-318: `(declare (ignore ...))` suppresses the unused binding warning."
  :tags '(:fr-318)
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((let ((ignored 1)) (declare (ignore ignored)) 42))
                    :target :vm))
           (messages (fr-318-warning-messages
                      (cl-cc/compile:compilation-result-warnings result))))
      (assert-false (some (lambda (message)
                            (search "IGNORED" message))
                          messages))
      (assert-null (cl-cc/compile:compilation-result-errors result)))))

(deftest fr-318-cps-toplevel-exposes-unused-warning
  "FR-318: CPS top-level compilation propagates nested codegen warnings."
  :tags '(:fr-318)
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* t))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((let ((unused 1)) 42))
                    :target :vm))
           (warnings (cl-cc/compile:compilation-result-warnings result)))
      (assert-true (some (lambda (warning)
                           (equal (cl-cc/parse:diagnostic-error-code warning) "W0001"))
                         warnings))
      (assert-null (cl-cc/compile:compilation-result-errors result)))))

(deftest fr-318-used-let-binding-does-not-warn
  "FR-318: used lexical variables do not produce unused-variable warnings."
  :tags '(:fr-318)
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (let ((result (cl-cc/compile:compile-toplevel-forms
                   '((let ((used 1)) used))
                   :target :vm)))
      (assert-null (cl-cc/compile:compilation-result-warnings result))
      (assert-null (cl-cc/compile:compilation-result-errors result)))))

(deftest fr-318-type-check-warning-is-structured
  "FR-318: non-strict type-check failures are recorded as structured warnings."
  :tags '(:fr-318)
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((+ "x" 1))
                    :target :vm
                    :type-check t))
           (warnings (cl-cc/compile:compilation-result-warnings result)))
      (assert-true (some (lambda (warning)
                           (and (eq (cl-cc/parse:diagnostic-severity warning) :warning)
                                (equal (cl-cc/parse:diagnostic-error-code warning) "W0002")))
                         warnings)))))

(deftest fr-318-compile-expression-exposes-unused-warning
  "FR-318: public single-form compilation exposes codegen warnings."
  :tags '(:fr-318)
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (let* ((result (cl-cc:compile-expression '(let ((unused 1)) 42) :target :vm))
           (warnings (cl-cc/compile:compilation-result-warnings result)))
      (assert-true (some (lambda (warning)
                           (equal (cl-cc/parse:diagnostic-error-code warning) "W0001"))
                         warnings)))))

(deftest fr-318-compile-expression-exposes-type-check-warning
  "FR-318: public single-form type-check warnings are structured diagnostics."
  :tags '(:fr-318)
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (let* ((result (cl-cc:compile-expression '(+ "x" 1)
                                             :target :vm
                                             :type-check t))
           (warnings (cl-cc/compile:compilation-result-warnings result)))
      (assert-true (some (lambda (warning)
                           (and (eq (cl-cc/parse:diagnostic-severity warning) :warning)
                                (equal (cl-cc/parse:diagnostic-error-code warning) "W0002")))
                         warnings)))))
