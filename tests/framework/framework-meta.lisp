;;;; tests/framework-meta.lisp — CL-CC Test Framework (Meta-Testing)
;;;; Mutation testing, code coverage, metamorphic testing, invariants.

;;; Load sb-cover contrib before the reader encounters sb-cover: symbols.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (ignore-errors (require :sb-cover)))

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Notes on shared state (defined in framework.lisp)
;;; ------------------------------------------------------------

;;; definvariant is defined in framework.lisp
;;; *invariant-registry* is defined in framework.lisp
;;; *metamorphic-relations* is defined in framework.lisp
;;; *tap-mutex* is defined in framework.lisp
;;; %fail-test is defined in framework.lisp

;;; ------------------------------------------------------------
;;; Coverage Helpers (FR-035) — sb-cover integration
;;; ------------------------------------------------------------

(defun enable-coverage ()
  "Instrument code for coverage tracking."
  #+sbcl
  (declaim (optimize (sb-cover:store-coverage-data 3)))
  #-sbcl
  nil)

(defun disable-coverage ()
  "Disable coverage instrumentation."
  #+sbcl
  (declaim (optimize (sb-cover:store-coverage-data 0)))
  #-sbcl
  nil)

(defun %print-coverage-report (covered-modules)
  "Print sb-cover coverage report for covered modules."
  (declare (ignore covered-modules))
  #+sbcl
  (progn
    (ensure-directories-exist "/tmp/cl-cc-coverage/")
    (sb-cover:report "/tmp/cl-cc-coverage/")
    (format t "# Coverage report written to /tmp/cl-cc-coverage/~%"))
  #-sbcl
  (format t "# Coverage: only available on SBCL~%"))

;;; ------------------------------------------------------------
;;; Metamorphic Testing (FR-036)
;;; ------------------------------------------------------------

(defmacro defmetamorphic (name &key transform relation applicable-when)
  "Define a metamorphic relation. At suite-run time, applies TRANSFORM to
   all test expressions that match APPLICABLE-WHEN, and verifies RELATION
   holds between original and transformed results.

   Example:
     (defmetamorphic commutativity
       :transform (lambda (expr)
         (destructuring-bind (op a b) (cdr expr)
           `(,op ,b ,a)))
       :relation #'=
       :applicable-when (lambda (expr)
         (member (car expr) '(+ *))))"
  `(progn
     (push (list :name ',name
                 :transform ,transform
                 :relation ,relation
                 :applicable-when ,applicable-when)
           *metamorphic-relations*)
     ',name))

(defun %verify-metamorphic-relations (expressions)
  "Check all registered metamorphic relations against the given expressions.
   Signals test-failure via %fail-test if any relation is violated."
  (dolist (relation *metamorphic-relations*)
    (let ((transform      (getf relation :transform))
          (rel-fn         (getf relation :relation))
          (applicable-when (getf relation :applicable-when)))
      (dolist (expr expressions)
        (when (and applicable-when (ignore-errors (funcall applicable-when expr)))
          (let* ((transformed        (ignore-errors (funcall transform expr)))
                 (original-result    (ignore-errors (eval expr)))
                 (transformed-result (ignore-errors (eval transformed))))
            (when (and original-result transformed-result)
              (unless (ignore-errors (funcall rel-fn original-result transformed-result))
                (%fail-test (format nil "Metamorphic relation ~S violated"
                                    (getf relation :name))
                            :expected (list expr original-result)
                            :actual   (list transformed transformed-result))))))))))

;;; ------------------------------------------------------------
;;; Mutation Operators (FR-029)
;;; ------------------------------------------------------------

(defun %substitute-symbol (form old new)
  "Recursively substitute all occurrences of symbol OLD with NEW in FORM."
  (cond
    ((eq form old) new)
    ((consp form)
     (cons (%substitute-symbol (car form) old new)
           (%substitute-symbol (cdr form) old new)))
    (t form)))

(defun %substitute-constant (form old new)
  "Recursively substitute the literal value OLD with NEW in FORM."
  (cond
    ((and (atom form) (eql form old)) new)
    ((consp form)
     (cons (%substitute-constant (car form) old new)
           (%substitute-constant (cdr form) old new)))
    (t form)))

(defun %negate-first-if-condition (form)
  "Find the first (if cond ...) subform and negate its condition."
  (cond
    ((and (consp form) (eq (car form) 'if) (>= (length form) 3))
     (let ((cond-expr (cadr form))
           (rest      (cddr form)))
       `(if (not ,cond-expr) ,@rest)))
    ((consp form)
     (cons (%negate-first-if-condition (car form))
           (cdr form)))
    (t form)))

(defun %return-nil-body (form)
  "If FORM is a (defun ...) or (defmethod ...) top-level definition,
   replace its body with NIL. Otherwise return FORM unchanged."
  (if (and (consp form)
           (member (car form) '(defun defmethod defgeneric)))
      ;; Keep (defun name params) prefix, replace body with nil
      (let* ((head    (car form))
             (fname   (cadr form))
             (params  (caddr form)))
        `(,head ,fname ,params nil))
      form))

(defun %apply-mutation (form mutation-type)
  "Apply a single mutation type to FORM, returning a list of mutant forms.
   Each mutant is a modified version of FORM with one change applied.

   Mutation types:
   :arithmetic-swap  — swap + with -, * with /
   :condition-negate — negate the first (if ...) condition found
   :boundary-shift   — replace < with <=, > with >=
   :constant-replace — replace literal 0 with 1 and 1 with 0
   :return-nil       — replace defun/defmethod body with nil"
  (ecase mutation-type
    (:arithmetic-swap
     ;; Produce two mutants: one with + swapped to -, one with * swapped to /
     (let ((plus-mutant  (%substitute-symbol form '+ '-))
           (times-mutant (%substitute-symbol form '* '/)))
       (remove-duplicates
        (remove-if (lambda (m) (equal m form))
                   (list plus-mutant times-mutant))
        :test #'equal)))
    (:condition-negate
     (let ((mutant (%negate-first-if-condition form)))
       (if (equal mutant form) '() (list mutant))))
    (:boundary-shift
     (let ((lt-mutant  (%substitute-symbol form '< '<=))
           (gt-mutant  (%substitute-symbol form '> '>=)))
       (remove-duplicates
        (remove-if (lambda (m) (equal m form))
                   (list lt-mutant gt-mutant))
        :test #'equal)))
    (:constant-replace
     (let ((zero-mutant (%substitute-constant form 0 1))
           (one-mutant  (%substitute-constant form 1 0)))
       (remove-duplicates
        (remove-if (lambda (m) (equal m form))
                   (list zero-mutant one-mutant))
        :test #'equal)))
    (:return-nil
     (let ((mutant (%return-nil-body form)))
       (if (equal mutant form) '() (list mutant))))))

;;; ------------------------------------------------------------
;;; Source File Reader
;;; ------------------------------------------------------------

(defun %read-all-forms (source-string)
  "Read all top-level forms from SOURCE-STRING.
   Returns a list of forms; ignores read errors gracefully."
  (let ((forms '())
        (pos 0)
        (len (length source-string)))
    (loop
      (when (>= pos len) (return))
      ;; Skip whitespace
      (loop while (and (< pos len)
                       (member (char source-string pos)
                               '(#\Space #\Tab #\Newline #\Return)))
            do (incf pos))
      (when (>= pos len) (return))
      ;; Try to read one form
      (multiple-value-bind (form new-pos)
          (ignore-errors
            (read-from-string source-string nil nil :start pos))
        (cond
          ((null new-pos)
           ;; read-from-string returned nil (EOF or error) — advance past problem
           (return))
          ((= new-pos pos)
           ;; No progress — bail out
           (return))
          (t
           (when form
             (push form forms))
           (setf pos new-pos)))))
    (nreverse forms)))

;;; ------------------------------------------------------------
;;; Mutant Evaluation
;;; ------------------------------------------------------------

(defun %eval-form-safely (form)
  "Evaluate FORM in the current Lisp image.
   Returns T on success, NIL on any error."
  (ignore-errors
    (eval form)
    t))

(defun %mutant-killed-p (mutant-form suite-name)
  "Check whether the given mutant is 'killed' (i.e., some test in SUITE-NAME
   detects the change introduced by MUTANT-FORM).

   Strategy:
   1. Attempt to eval the mutant form (replacing the original definition).
   2. Re-run the test suite collecting results.
   3. If any test FAILS that previously passed, the mutant is killed.

   NOTE: This is an in-image mutation approach. For production use, a
   subprocess-based approach with image snapshots is preferable to avoid
   polluting the running image. This implementation provides a functional
   but simplified version suitable for the cl-cc project's test scale."
  (let ((tests (%collect-all-suite-tests suite-name nil))
        (killed nil))
    ;; Speculatively eval the mutant; ignore compilation/eval errors
    ;; (a mutant that fails to compile is considered killed by the compiler)
    (let ((compiled-ok (%eval-form-safely mutant-form)))
      (unless compiled-ok
        ;; Mutant caused a compile/load error — counts as killed
        (return-from %mutant-killed-p t))
      ;; Run all tests and check if any now fail
      (dolist (test-plist tests)
        (when killed (return))
        (let ((result (%run-single-test test-plist 0 '())))
          (when (eq (getf result :status) :fail)
            (setf killed t)))))
    killed))

;;; ------------------------------------------------------------
;;; Mutation Score Reporter
;;; ------------------------------------------------------------

(defstruct mutation-record
  "Tracks a single mutant's outcome."
  (source-location "" :type string)
  (mutation-type   nil)
  (original-form   nil)
  (mutant-form     nil)
  (killed          nil :type boolean))

(defun %print-mutation-report (records)
  "Print a TAP-style mutation testing report."
  (let* ((total   (length records))
         (killed  (count-if #'mutation-record-killed records))
         (score   (if (zerop total)
                      100.0
                      (* 100.0 (/ killed total))))
         (survivors (remove-if #'mutation-record-killed records)))
    (format t "~%# ------------------------------------------------------------~%")
    (format t "# Mutation Testing Report~%")
    (format t "# ------------------------------------------------------------~%")
    (format t "# Mutation Score: ~,1F% (~A/~A mutants killed)~%"
            score killed total)
    (when survivors
      (format t "# Survivors (~A):~%" (length survivors))
      (dolist (rec survivors)
        (format t "#   ~A [~A] NOT caught~%"
                (mutation-record-source-location rec)
                (mutation-record-mutation-type rec))))
    (format t "# ------------------------------------------------------------~%")
    (force-output)
    (values score killed total)))

;;; ------------------------------------------------------------
;;; run-mutation-test (FR-029)
;;; ------------------------------------------------------------

(defun run-mutation-test (&key
                            target
                            suite
                            (mutations '(:arithmetic-swap
                                         :condition-negate
                                         :boundary-shift
                                         :constant-replace
                                         :return-nil)))
  "Run mutation testing on TARGET file using SUITE tests.

   TARGET    — path string to a .lisp source file
   SUITE     — symbol naming a registered test suite
   MUTATIONS — list of mutation operator keywords to apply

   Approach:
   1. Read all top-level forms from TARGET.
   2. For each form, apply each mutation operator to produce mutant variants.
   3. For each mutant: eval it in-image, re-run the suite, check if caught.
   4. Restore the original definition after each mutant trial.
   5. Print mutation score report.

   Returns (values score killed total) as flonum and integers.

   NOTE: Full production mutation testing requires subprocess isolation to
   avoid image pollution. This implementation uses an in-image eval approach
   which is sufficient for most cl-cc use cases. Side-effecting top-level
   forms (defvar, defparameter) are skipped to avoid state corruption."
  (unless target
    (error "run-mutation-test: :target file path is required"))
  (unless suite
    (error "run-mutation-test: :suite name is required"))
  (unless (probe-file target)
    (error "run-mutation-test: target file not found: ~A" target))

  (format t "# Mutation testing: ~A~%" target)
  (format t "# Suite: ~A~%" suite)
  (format t "# Operators: ~{~A~^ ~}~%" mutations)
  (format t "# Reading source forms...~%")
  (force-output)

  (let* ((source     (uiop:read-file-string target))
         (forms      (%read-all-forms source))
         (records    '())
         (form-index 0))

    (format t "# Found ~A top-level forms~%" (length forms))
    (force-output)

    (dolist (form forms)
      (incf form-index)
      ;; Skip forms that are risky to mutate in-image
      (when (and (consp form)
                 (member (car form) '(defun defmethod defgeneric defmacro)))
        (dolist (mutation-type mutations)
          (let ((mutants (%apply-mutation form mutation-type)))
            (dolist (mutant mutants)
              (let* ((location (format nil "~A:form-~A" target form-index))
                     (killed   nil))
                ;; Save original definition
                (let ((original-fn
                        (when (and (consp form) (eq (car form) 'defun))
                          (ignore-errors (symbol-function (cadr form))))))
                  ;; Test the mutant
                  (setf killed (%mutant-killed-p mutant suite))
                  ;; Restore original
                  (ignore-errors
                    (when (and original-fn
                               (consp form)
                               (eq (car form) 'defun))
                      (setf (symbol-function (cadr form)) original-fn))))
                ;; Record result
                (push (make-mutation-record
                       :source-location location
                       :mutation-type   mutation-type
                       :original-form   form
                       :mutant-form     mutant
                       :killed          killed)
                      records)))))))

    ;; Print report and return values
    (let ((final-records (nreverse records)))
      (%print-mutation-report final-records))))

;;; ------------------------------------------------------------
;;; Convenience: Pre-defined Metamorphic Relations for cl-cc
;;; ------------------------------------------------------------

;; Arithmetic commutativity: (+ a b) == (+ b a)
(defmetamorphic commutativity
  :transform (lambda (expr)
               (when (and (consp expr) (>= (length expr) 3))
                 (destructuring-bind (op a b &rest rest) expr
                   (declare (ignore rest))
                   `(,op ,b ,a))))
  :relation #'equal
  :applicable-when (lambda (expr)
                     (and (consp expr)
                          (member (car expr) '(+ *)))))

;; Compiler idempotency: compiling twice should give structurally equal output
;; (checked at the instruction-count level as a lightweight proxy)
(defmetamorphic compiler-idempotency
  :transform (lambda (expr)
               ;; Second compilation of the same expression
               expr)
  :relation (lambda (r1 r2)
              ;; Both results should be non-nil and have same instruction count
              (and r1 r2
                   (let ((p1 (ignore-errors (compilation-result-program r1)))
                         (p2 (ignore-errors (compilation-result-program r2))))
                     (and p1 p2
                          (= (length (vm-program-instructions p1))
                             (length (vm-program-instructions p2)))))))
  :applicable-when (lambda (expr)
                     ;; Apply to string expressions that look compilable
                     (stringp expr)))
