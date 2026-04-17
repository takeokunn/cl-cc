;;;; tests/framework-advanced.lisp — CL-CC Test Framework (Advanced Features)
;;;; Parameterized tests, nesting, snapshots, pipeline testing, combinatorial, flaky.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Dynamic Variables for Advanced Features
;;; ------------------------------------------------------------

(defvar *testing-context* nil
  "Stack of nested testing labels, accumulated as a string path.")

(defvar *update-snapshots* nil
  "When t, overwrite snapshot files on assert-snapshot mismatch.")

;;; ------------------------------------------------------------
;;; FR-015 — testing: Nested Sub-Cases
;;; ------------------------------------------------------------

(defmacro testing (label &body body)
  "Run BODY as a named sub-case within the current test.
TAP output will include the context path: outer > inner.
Uses *testing-context* to accumulate nesting depth."
  (let ((ctx-var (gensym "CTX")))
    `(let* ((,ctx-var (if *testing-context*
                          (format nil "~A > ~A" *testing-context* ,label)
                          ,label))
            (*testing-context* ,ctx-var))
       (handler-case
           (progn ,@body)
         (test-failure (c)
           ;; Re-signal with context prepended to the message
           (let ((orig-msg (test-failure-message c)))
             (error 'test-failure
                    :message (format nil "~A~%  context: ~A"
                                     orig-msg ,ctx-var))))))))

;;; ------------------------------------------------------------
;;; FR-016 — assert-snapshot: Snapshot Testing
;;; ------------------------------------------------------------

(defun %snapshot-path (name)
  "Return the full path for snapshot NAME."
  (concatenate 'string *snapshot-dir* name ".snap"))

(defun %read-snapshot (path)
  "Read and return the saved snapshot value from PATH, or return the
 symbol :snapshot-not-found if the file does not exist."
  (handler-case
      (with-open-file (stream path :direction :input)
        (read stream))
    (file-error () :snapshot-not-found)))

(defun %write-snapshot (path value)
  "Write VALUE to the snapshot file at PATH, creating directories as needed."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write value :stream stream)))

(defmacro assert-snapshot (name form)
  "Assert that FORM evaluates to the same value as the saved snapshot NAME.
On first run (no snapshot file), saves the result.
When *update-snapshots* is t, overwrites the saved file with the current value."
  (let ((actual-var  (gensym "ACTUAL"))
        (path-var    (gensym "PATH"))
        (saved-var   (gensym "SAVED")))
    `(let* ((,actual-var ,form)
            (,path-var   (%snapshot-path ,name))
            (,saved-var  (%read-snapshot ,path-var)))
       (cond
         ;; First run: no snapshot yet — save and pass
         ((eq ,saved-var :snapshot-not-found)
          (%write-snapshot ,path-var ,actual-var)
          t)
         ;; Update mode: overwrite snapshot unconditionally
         (*update-snapshots*
          (%write-snapshot ,path-var ,actual-var)
          t)
          ;; Normal run: compare
          ((not (equal ,saved-var ,actual-var))
           (%fail-test (format nil "assert-snapshot ~S mismatch" ,name)
                       :expected ,saved-var
                       :actual   ,actual-var
                       :form     ',form))
          (t t)))))

;;; ------------------------------------------------------------
;;; FR-014 — deftest-each: Parameterized Tests
;;; ------------------------------------------------------------

(defmacro deftest-each (base-name docstring &rest args)
  "Define one test per entry in CASES.
Each case is a list whose first element is the case label string and
whose remaining elements are bound to the variables in the VARS list.

Syntax:
  (deftest-each base-name
    \"docstring\"
    :cases ((\"label\" val ...) ...)
    (var ...)
    body...)

Generates tests named BASE-NAME [label] for each case."
  ;; Extract :cases keyword and the trailing body (var-list + body forms).
  (let* ((cases-pos (position :cases args))
         (cases     (if cases-pos (nth (1+ cases-pos) args) nil))
         (body      (if cases-pos (nthcdr (+ 2 cases-pos) args) args)))
    ;; BODY starts with a variable-list form, followed by the actual body forms.
    (destructuring-bind (vars &rest body-forms) body
      (let ((expansions
              (loop for case-entry in cases
                    collect
                    (let* ((case-label (first case-entry))
                           (case-vals  (rest case-entry))
                           (test-name  (intern
                                        (format nil "~A [~A]"
                                                (symbol-name base-name)
                                                case-label)))
                           (bindings   (mapcar #'list vars case-vals)))
                      `(deftest ,test-name
                         ,docstring
                         (let ,bindings
                           ,@body-forms))))))
        `(progn ,@expansions)))))

;;; ------------------------------------------------------------
;;; FR-027 — deftest-combinatorial: All-Combinations Testing
;;; ------------------------------------------------------------

(defun %cross-product (lists)
  "Return the cross-product of a list of lists as a list of lists."
  (if (null lists)
      '(())
      (let ((head (car lists))
            (rest-product (%cross-product (cdr lists))))
        (loop for item in head
              nconc (loop for combo in rest-product
                          collect (cons item combo))))))

(defmacro deftest-combinatorial (base-name &rest args)
  "Define one test per combination of all parameter values.
PARAMS is a list of (var form) pairs where form evaluates to a list of values.
Each combination gets its own deftest named BASE-NAME [v1 v2 ...].

Syntax:
  (deftest-combinatorial base-name
    :params ((var1 '(val...)) (var2 '(val...)) ...)
    body...)

Evaluates all param value forms at macro-expansion time (quoted lists)
or generates runtime expansion."
  ;; Extract :params keyword and the trailing body forms.
  (let* ((params-pos  (position :params args))
         (params      (if params-pos (nth (1+ params-pos) args) nil))
         (body        (if params-pos (nthcdr (+ 2 params-pos) args) args))
         (param-vars  (mapcar #'first  params))
         (param-forms (mapcar #'second params))
         (combo-gensym (gensym "COMBOS")))
    `(let ((,combo-gensym (%cross-product (list ,@param-forms))))
       (dolist (combo ,combo-gensym)
         (let* ,(loop for var in param-vars
                      for idx from 0
                      collect `(,var (nth ,idx combo)))
           (let* ((label-str (format nil "~{~A~^ ~}" combo))
                  (test-name (intern (format nil "~A [~A]"
                                             ',(symbol-name base-name)
                                             label-str))))
             (setf (gethash test-name *test-registry*)
                   (let ((captured-combo combo))
                     (list :name test-name
                           :fn (let ,(loop for var in param-vars
                                           for idx from 0
                                           collect `(,var (nth ,idx captured-combo)))
                                 (lambda () ,@body))
                           :suite *current-suite*
                           :timeout nil
                           :depends-on nil
                           :tags nil
                           :docstring (format nil "~A combination ~A"
                                              ',base-name label-str))))))))))

;;; ------------------------------------------------------------
;;; FR-021 — deftest-pipeline: Pipeline Stage Testing
;;; ------------------------------------------------------------

;; The _ symbol used in pipeline stage checks is simply bound to the
;; stage output before each check form is evaluated.

(defmacro deftest-pipeline (expr &rest stage-checks)
  "Define a pipeline test that runs EXPR through compiler stages.
Available stage keys: :parse :expand :compile :execute
In each stage body, _ is bound to the output of that stage.

  :parse    lower-sexp-to-ast
  :expand   our-macroexpand
  :compile  compile-expression
  :execute  run-string

Syntax:
  (deftest-pipeline expr-string
    :parse   check-form...
    :compile check-form...
    :execute check-form...)"
  (let ((test-name (gensym "PIPELINE-TEST"))
        (forms '()))
    ;; Build up the stage forms in order of appearance.
    (do ((rest stage-checks rest))
        ((null rest))
      (let ((key  (pop rest))
            (chk  (pop rest)))
        (push (list key chk) forms)))
    (let ((stage-list (nreverse forms)))
      `(deftest ,test-name
         ,(format nil "pipeline test for ~S" expr)
         ,@(loop for (stage-key check-form) in stage-list
                 collect
                 (ecase stage-key
                   (:parse
                    `(let ((_ (lower-sexp-to-ast
                               (read-from-string ,expr))))
                       ,check-form))
                   (:expand
                    `(let ((_ (our-macroexpand
                               (read-from-string ,expr))))
                       ,check-form))
                   (:compile
                    `(let ((_ (compile-expression
                               (lower-sexp-to-ast
                                (read-from-string ,expr)))))
                       ,check-form))
                   (:execute
                    `(let ((_ (run-string ,expr)))
                       ,check-form))))))))

;;; ------------------------------------------------------------
;;; Convenience: assert-snapshot update helper
;;; ------------------------------------------------------------

(defun update-snapshots! ()
  "Bind *update-snapshots* to t for the dynamic extent of this call.
Intended to be called from the REPL when you want to refresh all snapshots."
  (setf *update-snapshots* t))

(defun reset-snapshots! ()
  "Reset snapshot update mode to nil."
  (setf *update-snapshots* nil))

;;; ------------------------------------------------------------
;;; Runner Regression Tests
;;; ------------------------------------------------------------

(defsuite runner-regression-suite
  :description "Serial regression tests for test runner orchestration"
  :parallel nil
  :parent cl-cc-suite)

(in-suite runner-regression-suite)

(deftest suite-parallel-policy-inherits-from-parent
  "Child suites inherit serial execution policy from ancestors."
  (let ((parent (gensym "ULW-PARENT-"))
        (child (gensym "ULW-CHILD-")))
    (unwind-protect
         (progn
           (setf (gethash parent *suite-registry*)
                 (list :name parent :description "tmp" :parent nil :parallel nil
                       :before-each '() :after-each '()))
           (setf (gethash child *suite-registry*)
                 (list :name child :description "tmp" :parent parent :parallel t
                       :before-each '() :after-each '()))
           (assert-false (%suite-parallel-p child))
           (assert-false (%test-parallel-safe-p (list :suite child :depends-on nil))))
      (remhash child *suite-registry*)
      (remhash parent *suite-registry*))))

(deftest mixed-runner-reorders-internal-dependencies
  "Dependent tests are reordered behind their in-list prerequisite."
  (let* ((dependency (list :name 'ulw-dependency :depends-on nil))
         (dependent (list :name 'ulw-dependent :depends-on 'ulw-dependency))
         (ordered (%order-tests-for-dependencies (list dependent dependency))))
    (assert-equal '(ulw-dependency ulw-dependent)
                  (mapcar (lambda (test) (getf test :name)) ordered))))

(deftest dependency-ordering-fallback-preserves-confirmed-prefix
  "If dependency ordering gets stuck, already-confirmed prefix order is preserved."
  (let* ((ordered (%order-tests-for-dependencies
                   (list (list :name 'ulw-a :depends-on 'ulw-b)
                         (list :name 'ulw-x :depends-on nil)
                         (list :name 'ulw-b :depends-on 'ulw-a)))))
    (assert-equal '(ulw-x ulw-a ulw-b)
                  (mapcar (lambda (test) (getf test :name)) ordered))))

(deftest mixed-runner-keeps-serial-suites-out-of-parallel-pool
  "Serial suites and dependent tests are excluded from the parallel worker batch."
  (let ((serial-suite (gensym "ULW-SERIAL-SUITE-"))
        (parallel-suite (gensym "ULW-PARALLEL-SUITE-")))
    (unwind-protect
         (progn
           (setf (gethash serial-suite *suite-registry*)
                 (list :name serial-suite :description "tmp" :parent nil :parallel nil
                       :before-each '() :after-each '()))
           (setf (gethash parallel-suite *suite-registry*)
                 (list :name parallel-suite :description "tmp" :parent nil :parallel t
                       :before-each '() :after-each '()))
           (assert-false (%test-parallel-safe-p
                          (list :name 'serial-test :suite serial-suite :depends-on nil)))
           (assert-false (%test-parallel-safe-p
                          (list :name 'dependent-test :suite parallel-suite
                                :depends-on 'other-test)))
            (assert-true (%test-parallel-safe-p
                         (list :name 'parallel-test :suite parallel-suite :depends-on nil))))
      (remhash parallel-suite *suite-registry*)
      (remhash serial-suite *suite-registry*))))

(deftest effective-worker-count-falls-back-to-one-for-serial-batches
  "Worker reporting collapses to 1 when no test in the batch may run in parallel."
  (let ((serial-suite (gensym "ULW-SERIAL-SUITE-")))
    (unwind-protect
         (progn
           (setf (gethash serial-suite *suite-registry*)
                 (list :name serial-suite :description "tmp" :parent nil :parallel nil
                       :before-each '() :after-each '()))
           (assert-= 1
                     (%effective-worker-count
                      (list (list :name 'serial-test :suite serial-suite :depends-on nil))
                      t
                      4))
           (assert-= 1
                     (%effective-worker-count
                      (list (list :name 'serial-test :suite serial-suite :depends-on nil))
                      nil
                      4)))
      (remhash serial-suite *suite-registry*))))

(deftest effective-worker-count-keeps-requested-workers-for-parallel-safe-batches
  "Worker reporting preserves the requested worker count when at least one test can run in parallel."
  (let ((parallel-suite (gensym "ULW-PARALLEL-SUITE-")))
    (unwind-protect
         (progn
           (setf (gethash parallel-suite *suite-registry*)
                 (list :name parallel-suite :description "tmp" :parent nil :parallel t
                       :before-each '() :after-each '()))
           (assert-= 4
                     (%effective-worker-count
                      (list (list :name 'parallel-test :suite parallel-suite :depends-on nil))
                      t
                      4))
            (assert-= 2
                     (%effective-worker-count
                      (list (list :name 'parallel-test :suite parallel-suite :depends-on nil))
                      t
                      2)))
      (remhash parallel-suite *suite-registry*))))

(deftest run-suite-reports-one-worker-for-serial-only-batch
  "run-suite reports one worker when the selected batch has no parallel-safe tests."
  (let ((root (gensym "ULW-ROOT-"))
        (serial-suite (gensym "ULW-SERIAL-"))
        (test-name (gensym "ULW-TEST-"))
        (original-warm-stdlib-cache (symbol-function 'cl-cc::warm-stdlib-cache))
        (original-quit (symbol-function 'uiop:quit)))
    (unwind-protect
         (progn
           (setf (gethash root *suite-registry*)
                 (list :name root :description "tmp" :parent nil :parallel t
                       :before-each '() :after-each '()))
           (setf (gethash serial-suite *suite-registry*)
                 (list :name serial-suite :description "tmp" :parent root :parallel nil
                       :before-each '() :after-each '()))
           (setf (gethash test-name *test-registry*)
                 (list :name test-name
                       :suite serial-suite
                       :fn (lambda () t)
                       :skip nil
                       :xfail nil
                       :depends-on nil
                       :timeout nil
                       :tags nil))
           (setf (symbol-function 'cl-cc::warm-stdlib-cache) (lambda () nil))
           (setf (symbol-function 'uiop:quit) (lambda (&optional code) code))
           (let ((output (with-output-to-string (s)
                           (let ((*standard-output* s))
                             (assert-equal 0
                                           (run-suite root :parallel t :random nil :workers 4))))))
             (assert-true (search "# Workers: 1" output))))
      (setf (symbol-function 'cl-cc::warm-stdlib-cache) original-warm-stdlib-cache)
      (setf (symbol-function 'uiop:quit) original-quit)
      (remhash test-name *test-registry*)
      (remhash serial-suite *suite-registry*)
      (remhash root *suite-registry*))))

(deftest canonical-suite-taxonomy-matches-runner-contract
  "The canonical runner exposes unit, integration, and e2e suites under the root taxonomy."
  (assert-eq 'cl-cc-suite
             (getf (gethash 'cl-cc-unit-suite *suite-registry*) :parent))
  (assert-eq 'cl-cc-suite
             (getf (gethash 'cl-cc-integration-suite *suite-registry*) :parent))
  (assert-eq 'cl-cc-suite
             (getf (gethash 'cl-cc-e2e-suite *suite-registry*) :parent)))

(deftest run-single-test-skips-when-dependency-failed
  "A test with a failed dependency is reported as skipped without executing its body."
  (let* ((called nil)
         (test-plist (list :name 'needs-dep
                           :fn (lambda () (setf called t))
                           :suite 'cl-cc-unit-suite
                           :timeout nil
                           :depends-on 'upstream
                           :tags nil))
         (result (%run-single-test test-plist 1 (list (list :name 'upstream :status :fail)))))
    (assert-false called)
    (assert-eq :skip (getf result :status))))

(deftest run-single-test-handles-pending-condition
  "A pending condition becomes a :pending result rather than a failure."
  (let* ((test-plist (list :name 'pending-demo
                           :fn (lambda () (error 'pending-condition :reason "later"))
                           :suite 'cl-cc-unit-suite
                           :timeout nil
                           :depends-on nil
                           :tags nil))
         (result (%run-single-test test-plist 1 '())))
    (assert-eq :pending (getf result :status))
    (assert-true (search "later" (getf result :detail)))))

(deftest effective-test-timeout-cases
  "%effective-test-timeout normalizes nil, :none, and positive integers."
  (assert-null (%effective-test-timeout (list :timeout :none)))
  (assert-equal 7 (%effective-test-timeout (list :timeout 7)))
  (assert-equal (%default-test-timeout)
                (%effective-test-timeout (list :timeout nil))))

(deftest detect-flaky-reports-inconsistent-statuses
  "%detect-flaky prints a summary when a test passes in only some repeated runs."
  (let ((*standard-output* (make-string-output-stream)))
    (%detect-flaky (list (list (list :name 'sometimes :status :pass)
                               (list :name 'always :status :pass))
                         (list (list :name 'sometimes :status :fail)
                               (list :name 'always :status :pass)))
                   2)
    (let ((output (get-output-stream-string *standard-output*)))
      (assert-true (search "Flaky tests detected" output))
      (assert-true (search "SOMETIMES" (string-upcase output))))))

(deftest detect-flaky-is-silent-for-consistent-results
  "%detect-flaky emits nothing when every test is consistently pass or fail."
  (let ((*standard-output* (make-string-output-stream)))
    (%detect-flaky (list (list (list :name 'always-pass :status :pass)
                               (list :name 'always-fail :status :fail))
                         (list (list :name 'always-pass :status :pass)
                               (list :name 'always-fail :status :fail)))
                   2)
    (assert-string= "" (get-output-stream-string *standard-output*))))

(deftest run-single-test-handles-skip-condition
  "A skip condition becomes a :skip result rather than a failure."
  (let* ((test-plist (list :name 'skip-demo
                           :fn (lambda () (error 'skip-condition :reason "not today"))
                           :suite 'cl-cc-unit-suite
                           :timeout nil
                           :depends-on nil
                           :tags nil))
         (result (%run-single-test test-plist 1 '())))
    (assert-eq :skip (getf result :status))
    (assert-true (search "not today" (getf result :detail)))))

(deftest run-single-test-reports-fixture-setup-errors
  "A before-each fixture error is surfaced as a fixture error result."
  (let ((fixture-suite (gensym "ULW-FIXTURE-SUITE-")))
    (unwind-protect
         (progn
           (setf (gethash fixture-suite *suite-registry*)
                 (list :name fixture-suite
                       :description "tmp"
                       :parent nil
                       :parallel t
                       :before-each (list (lambda () (error "fixture boom")))
                       :after-each '()))
           (let* ((test-plist (list :name 'fixture-demo
                                    :fn (lambda () t)
                                    :suite fixture-suite
                                    :timeout nil
                                    :depends-on nil
                                    :tags nil))
                  (result (%run-single-test test-plist 1 '())))
             (assert-eq :fail (getf result :status))
             (assert-true (search "fixture error" (getf result :detail)))
             (assert-true (search "fixture boom" (getf result :detail)))))
      (remhash fixture-suite *suite-registry*))))

(deftest run-single-test-times-out-sequentially
  "Sequential runner reports timeout failures when a test exceeds its budget."
  (let* ((test-plist (list :name 'slow-demo
                           :fn (lambda () (sleep 2))
                           :suite 'cl-cc-unit-suite
                           :timeout 1
                           :depends-on nil
                           :tags nil))
         (result (%run-single-test test-plist 1 '())))
    (assert-eq :fail (getf result :status))
    (assert-true (search "timeout after 1 seconds" (getf result :detail)))))

(deftest run-tests-sequential-returns-ordered-results
  "%run-tests-sequential preserves input order and records pass statuses."
  (let ((results (%run-tests-sequential
                  (list (list :name 'first
                              :fn (lambda () t)
                              :suite 'cl-cc-unit-suite
                              :timeout nil
                              :depends-on nil
                              :tags nil
                              :number 1)
                        (list :name 'second
                              :fn (lambda () t)
                              :suite 'cl-cc-unit-suite
                              :timeout nil
                              :depends-on nil
                              :tags nil
                              :number 2)))))
    (assert-equal '(first second)
                  (mapcar (lambda (result) (getf result :name)) results))
    (assert-true (every (lambda (result) (eq :pass (getf result :status))) results))))

(deftest resolve-suite-returns-symbol-and-signals-for-missing-suite
  "%resolve-suite returns existing suites and errors on missing ones."
  (assert-eq 'cl-cc-unit-suite (%resolve-suite :cl-cc/test "CL-CC-UNIT-SUITE"))
  (handler-case
      (progn
        (%resolve-suite :cl-cc/test "MISSING-SUITE")
        (assert-false t))
    (error (e)
      (assert-true (search "Suite CL-CC/TEST::MISSING-SUITE not found"
                           (princ-to-string e))))))
