;;;; tests/framework.lisp — CL-CC Custom Test Framework (Core)
;;;; Zero external dependencies. Replaces FiveAM.
;;;; TAP version 13 output, parallel execution, fixtures, skip/pending.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Conditions
;;; ------------------------------------------------------------

(define-condition test-failure (error)
  ((message :initarg :message :reader test-failure-message))
  (:report (lambda (c s)
             (format s "Test failure: ~A" (test-failure-message c)))))

(define-condition skip-condition (condition)
  ((reason :initarg :reason :reader skip-reason))
  (:report (lambda (c s)
             (format s "SKIP: ~A" (skip-reason c)))))

(define-condition pending-condition (condition)
  ((reason :initarg :reason :reader pending-reason))
  (:report (lambda (c s)
             (format s "TODO: ~A" (pending-reason c)))))

;;; ------------------------------------------------------------
;;; Internal State
;;; ------------------------------------------------------------

(defvar *suite-registry* (make-hash-table)
  "symbol -> plist (:name :description :parent :before-each :after-each)")

(defvar *test-registry* (make-hash-table)
  "symbol -> plist (:name :fn :suite :timeout :depends-on :tags :docstring)")

(defvar *current-suite* nil
  "Currently active suite symbol.")

(defvar *invariant-registry* '()
  "List of invariant functions called after every test.")

(defvar *test-results* nil
  "List of result plists: (:name :status :detail :number)")

(defvar *test-counter* 0
  "Total test count for plan line.")

(defvar *tap-mutex* (sb-thread:make-mutex :name "tap-output")
  "Mutex for thread-safe TAP output.")

(defvar *snapshot-dir* "tests/snapshots/"
  "Directory for snapshot files.")

(defvar *metamorphic-relations* '()
  "List of metamorphic relation plists.")

;;; ------------------------------------------------------------
;;; Suite Definition
;;; ------------------------------------------------------------

(defmacro defsuite (name &key description parent)
  "Define a test suite. Stores metadata in *suite-registry*."
  `(progn
     (setf (gethash ',name *suite-registry*)
           (list :name ',name
                 :description ,description
                 :parent ',parent
                 :before-each '()
                 :after-each '()))
     ',name))

(defmacro in-suite (name)
  "Set the current active suite."
  `(setf *current-suite* ',name))

;;; ------------------------------------------------------------
;;; Test Definition
;;; ------------------------------------------------------------

(defun %parse-deftest-body (forms)
  "Parse body forms, extracting optional docstring and keyword args.
Returns (values docstring timeout depends-on tags body-forms)."
  (let ((docstring nil)
        (timeout nil)
        (depends-on nil)
        (tags nil)
        (rest forms))
    (when (and rest (stringp (car rest)))
      (setf docstring (car rest)
            rest (cdr rest)))
    (loop while (and rest (keywordp (car rest)))
          do (let ((key (pop rest))
                   (val (pop rest)))
               (case key
                 (:timeout   (setf timeout val))
                 (:depends-on (setf depends-on val))
                 (:tags      (setf tags val)))))
    (values docstring timeout depends-on tags rest)))

(defmacro deftest (name &body body)
  "Define a test. Syntax:
     (deftest name
       \"optional docstring\"
       :timeout 5
       :depends-on other-test
       :tags '(:tag1)
       body-form...)"
  (multiple-value-bind (docstring timeout depends-on tags body-forms)
      (%parse-deftest-body body)
    `(progn
       (setf (gethash ',name *test-registry*)
             (list :name ',name
                   :fn (lambda () ,@body-forms)
                   :suite *current-suite*
                   :timeout ,timeout
                   :depends-on ',depends-on
                   :tags ,tags
                   :docstring ,docstring))
       ',name)))

;;; ------------------------------------------------------------
;;; Fixtures
;;; ------------------------------------------------------------

(defmacro defbefore (when-spec suite-spec &body body)
  "Register a before-each fixture for the given suite.
   (defbefore :each (suite-name) body...)"
  (declare (ignore when-spec))
  (let ((suite-name (if (listp suite-spec) (car suite-spec) suite-spec)))
    `(let ((entry (gethash ',suite-name *suite-registry*)))
       (when entry
         (setf (getf entry :before-each)
               (append (getf entry :before-each)
                       (list (lambda () ,@body))))
         (setf (gethash ',suite-name *suite-registry*) entry)))))

(defmacro defafter (when-spec suite-spec &body body)
  "Register an after-each fixture for the given suite.
   (defafter :each (suite-name) body...)"
  (declare (ignore when-spec))
  (let ((suite-name (if (listp suite-spec) (car suite-spec) suite-spec)))
    `(let ((entry (gethash ',suite-name *suite-registry*)))
       (when entry
         (setf (getf entry :after-each)
               (append (getf entry :after-each)
                       (list (lambda () ,@body))))
         (setf (gethash ',suite-name *suite-registry*) entry)))))

;;; ------------------------------------------------------------
;;; Skip / Pending
;;; ------------------------------------------------------------

(defun skip (reason)
  "Signal a skip condition with the given reason."
  (signal 'skip-condition :reason reason))

(defun pending (reason)
  "Signal a pending condition with the given reason."
  (signal 'pending-condition :reason reason))

(defmacro with-reset-repl-state (&body body)
  "Run BODY with a clean REPL state, and always restore the REPL to empty."
  `(unwind-protect
       (progn
         (reset-repl-state)
         ,@body)
     (reset-repl-state)))

(defun make-test-vm ()
  "Create a fresh VM state for instruction-level testing."
  (make-instance 'cl-cc:vm-state))

(defmacro with-test-vm ((state &rest bindings) &body body)
  "Create a fresh VM STATE, preload register BINDINGS, then run BODY.

Each binding has the shape (register value), for example:
  (with-test-vm (vm (1 42) (2 '(a b))) ...)
This keeps instruction-level tests focused on behavior instead of setup noise."
  `(let ((,state (make-test-vm)))
     ,@(mapcar (lambda (binding)
                 (destructuring-bind (register value) binding
                   `(cl-cc:vm-reg-set ,state ,register ,value)))
               bindings)
     ,@body))

(defun vm-exec (inst state &optional (pc 0) (labels (make-hash-table :test #'equal)))
  "Execute one VM instruction and return the next program counter."
  (cl-cc:execute-instruction inst state pc labels))

(defun exec1 (inst state &optional (pc 0))
  "Execute one VM instruction with a fresh label table."
  (vm-exec inst state pc (make-hash-table)))

;;; ------------------------------------------------------------
;;; Invariants
;;; ------------------------------------------------------------

(defmacro definvariant (name &rest args)
  "Register an invariant checked after every test.
   Syntax: (definvariant name [:after-each t] body...)"
  (let ((body args))
    ;; Strip optional :after-each BOOL prefix
    (when (and body (eq (first body) :after-each))
      (setf body (cddr body)))
    `(progn
       (push (cons ',name (lambda () ,@body)) *invariant-registry*)
       ',name)))

(defun %run-invariants ()
  "Run all registered invariants, signaling test-failure with invariant name on violation."
  (dolist (inv *invariant-registry*)
    (handler-case
        (funcall (cdr inv))
      (test-failure (c)
        (error 'test-failure
               :message (format nil "Invariant ~S violated: ~A"
                                (car inv) (test-failure-message c)))))))

;;; ------------------------------------------------------------
;;; Assertion Helpers
;;; ------------------------------------------------------------

(defun %format-value (val)
  "Format a value for diagnostic output."
  (let ((*print-length* 20)
        (*print-level* 5))
    (format nil "~S" val)))

(defun %fail-test (message &key expected actual form at)
  "Record a test failure and signal test-failure condition."
  (let ((yaml (with-output-to-string (s)
                (format s "  ---~%")
                (format s "  message: ~S~%" message)
                (when expected
                  (format s "  expected: ~A~%" (%format-value expected)))
                (when actual
                  (format s "  actual: ~A~%" (%format-value actual)))
                (when form
                  (format s "  form: ~A~%" form))
                (when at
                  (format s "  at: ~A~%" at))
                (format s "  ..."))))
    (error 'test-failure :message yaml)))

;;; ------------------------------------------------------------
;;; Core Assertion Macros
;;; ------------------------------------------------------------

(defmacro assert-= (expected actual)
  "Assert numeric equality."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (= ,e ,a)
         (%fail-test "assert-= failed"
                     :expected ,e
                     :actual ,a
                     :form '(= ,expected ,actual))))))

(defmacro assert-eq (expected actual)
  "Assert pointer equality (eq)."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (eq ,e ,a)
         (%fail-test "assert-eq failed"
                     :expected ,e
                     :actual ,a
                     :form '(eq ,expected ,actual))))))

(defmacro assert-eql (expected actual)
  "Assert eql equality."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (eql ,e ,a)
         (%fail-test "assert-eql failed"
                     :expected ,e
                     :actual ,a
                     :form '(eql ,expected ,actual))))))

(defmacro assert-equal (expected actual)
  "Assert structural equality (equal)."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (equal ,e ,a)
         (%fail-test "assert-equal failed"
                     :expected ,e
                     :actual ,a
                     :form '(equal ,expected ,actual))))))

(defmacro assert-string= (expected actual)
  "Assert string equality."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (string= ,e ,a)
         (%fail-test "assert-string= failed"
                     :expected ,e
                     :actual ,a
                     :form '(string= ,expected ,actual))))))

(defmacro assert-null (form)
  "Assert form evaluates to nil."
  (let ((v (gensym "V")))
    `(let ((,v ,form))
       (unless (null ,v)
         (%fail-test "assert-null failed"
                     :expected nil
                     :actual ,v
                     :form ',form)))))

(defmacro assert-true (form)
  "Assert form evaluates to a truthy value."
  (let ((v (gensym "V")))
    `(let ((,v ,form))
       (unless ,v
         (%fail-test "assert-true failed"
                     :expected t
                     :actual nil
                     :form ',form)))))

(defmacro assert-false (form)
  "Assert form evaluates to a falsy value."
  (let ((v (gensym "V")))
    `(let ((,v ,form))
       (when ,v
         (%fail-test "assert-false failed"
                     :expected nil
                     :actual ,v
                     :form ',form)))))

(defmacro assert-type (type-name object)
  "Assert object is of type type-name. Note: type-name comes first."
  (let ((o (gensym "O")))
    `(let ((,o ,object))
       (unless (typep ,o ',type-name)
         (%fail-test "assert-type failed"
                     :expected ',type-name
                     :actual (type-of ,o)
                     :form '(typep ,object ,type-name))))))

(defmacro assert-signals (condition-type form)
  "Assert that form signals a condition of condition-type."
  `(handler-case
       (progn
         ,form
         (%fail-test (format nil "assert-signals: expected ~S to be signaled, but no condition was raised"
                             ',condition-type)
                     :form ',form))
     (,condition-type () t)
     (error (c)
       (%fail-test (format nil "assert-signals: expected ~S but got ~S: ~A"
                           ',condition-type (type-of c) c)
                   :form ',form))))

(defmacro assert-values (form &rest expected-values)
  "Assert multiple return values of form."
  (let ((actuals (gensym "ACTUALS")))
    `(let ((,actuals (multiple-value-list ,form)))
       (let ((expected-list (list ,@expected-values)))
         (unless (equal ,actuals expected-list)
           (%fail-test "assert-values failed"
                       :expected expected-list
                       :actual ,actuals
                       :form ',form))))))

(defmacro assert-type-equal (expected actual)
  "Assert that two type-nodes are structurally equal via type-equal-p.
Produces a human-readable message using type-to-string on failure."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (type-equal-p ,e ,a)
         (%fail-test "assert-type-equal: types not equal"
                     :expected (type-to-string ,e)
                     :actual   (type-to-string ,a)
                     :form '(type-equal-p ,expected ,actual))))))

(defmacro assert-unifies (t1 t2)
  "Assert that types T1 and T2 unify successfully."
  (let ((s (gensym "S")) (ok (gensym "OK")))
    `(multiple-value-bind (,s ,ok) (type-unify ,t1 ,t2)
       (declare (ignore ,s))
       (unless ,ok
         (%fail-test "assert-unifies: types failed to unify"
                     :expected "unification success"
                     :actual   "unification failure"
                     :form '(type-unify ,t1 ,t2))))))

(defmacro assert-not-unifies (t1 t2)
  "Assert that types T1 and T2 fail to unify."
  (let ((s (gensym "S")) (ok (gensym "OK")))
    `(multiple-value-bind (,s ,ok) (type-unify ,t1 ,t2)
       (declare (ignore ,s))
       (when ,ok
         (%fail-test "assert-not-unifies: types unexpectedly unified"
                     :expected "unification failure"
                     :actual   "unification success"
                     :form '(type-unify ,t1 ,t2))))))

;;; ------------------------------------------------------------
;;; TAP Output
;;; ------------------------------------------------------------

(defun %tap-print (line)
  "Print a TAP line with mutex protection."
  (sb-thread:with-mutex (*tap-mutex*)
    (format t "~A~%" line)
    (force-output)))

(defun %tap-print-result (result)
  "Print a single TAP test result line plus optional YAML block."
  (let* ((number (getf result :number))
         (name   (getf result :name))
         (status (getf result :status))
         (detail (getf result :detail)))
    (sb-thread:with-mutex (*tap-mutex*)
      (ecase status
        (:pass
         (format t "ok ~A - ~A~%" number name))
        (:fail
         (format t "not ok ~A - ~A~%" number name)
         (when detail
           (format t "~A~%" detail)))
        (:skip
         (format t "ok ~A - ~A # SKIP ~A~%" number name (or detail "")))
        (:pending
         (format t "not ok ~A - ~A # TODO ~A~%" number name (or detail ""))))
      (force-output))))

;;; ------------------------------------------------------------
;;; Test Execution
;;; ------------------------------------------------------------

(defun %collect-suite-tests (suite-name tags)
  "Collect all tests belonging to suite-name (and child suites), filtered by tags."
  (let ((result '()))
    (maphash (lambda (test-sym plist)
               (declare (ignore test-sym))
               (let ((suite (getf plist :suite))
                     (test-tags (getf plist :tags)))
                 (when (and (eq suite suite-name)
                            (or (null tags)
                                (and test-tags
                                     (some (lambda (tag) (member tag test-tags)) tags))))
                   (push plist result))))
             *test-registry*)
    result))

(defun %collect-all-suite-tests (suite-name tags)
  "Collect tests from suite-name and all descendant suites."
  (let ((result (%collect-suite-tests suite-name tags)))
    ;; find child suites
    (maphash (lambda (child-name child-plist)
               (when (eq (getf child-plist :parent) suite-name)
                 (setf result (append result
                                      (%collect-all-suite-tests child-name tags)))))
             *suite-registry*)
    result))

(defun %fisher-yates-shuffle (vec)
  "In-place Fisher-Yates shuffle of a vector."
  (let ((n (length vec)))
    (loop for i from (1- n) downto 1
          do (let ((j (random (1+ i))))
               (rotatef (aref vec i) (aref vec j)))))
  vec)

(defun %get-suite-fixtures (suite-name)
  "Return (before-each-fns after-each-fns) for a suite."
  (let ((entry (gethash suite-name *suite-registry*)))
    (if entry
        (values (getf entry :before-each) (getf entry :after-each))
        (values nil nil))))

(defun %check-dependency (test-plist all-results)
  "Return nil if dependency failed (test should be skipped), t otherwise."
  (let ((dep (getf test-plist :depends-on)))
    (if (null dep)
        t
        (let ((dep-result (find dep all-results :key (lambda (r) (getf r :name)))))
          (if dep-result
              (eq (getf dep-result :status) :pass)
              t)))))  ; dependency not yet run — allow

(defun %run-single-test (test-plist number results-so-far)
  "Run one test and return a result plist."
  (let* ((name     (getf test-plist :name))
         (fn       (getf test-plist :fn))
         (timeout  (getf test-plist :timeout))
         (suite    (getf test-plist :suite)))
    ;; Check dependency
    (unless (%check-dependency test-plist results-so-far)
      (return-from %run-single-test
        (list :name name :status :skip :detail "dependency failed" :number number)))
    ;; Run before-each fixtures
    (multiple-value-bind (before-fns after-fns)
        (%get-suite-fixtures suite)
      (handler-case
          (progn
            (dolist (bf before-fns) (funcall bf))
            (handler-case
                (handler-bind
                    ((skip-condition
                       (lambda (c)
                         (return-from %run-single-test
                           (list :name name :status :skip
                                 :detail (skip-reason c) :number number))))
                     (pending-condition
                       (lambda (c)
                         (return-from %run-single-test
                           (list :name name :status :pending
                                 :detail (pending-reason c) :number number)))))
                  (sb-ext:with-timeout (or timeout 5)
                    (funcall fn))
                  ;; Run after-each fixtures
                  (dolist (af after-fns) (funcall af))
                  ;; Run invariants
                  (%run-invariants)
                  (list :name name :status :pass :detail nil :number number))
              (test-failure (c)
                (dolist (af after-fns) (ignore-errors (funcall af)))
                (list :name name :status :fail
                      :detail (test-failure-message c) :number number))
              (sb-ext:timeout ()
                (dolist (af after-fns) (ignore-errors (funcall af)))
                (list :name name :status :fail
                      :detail (format nil "  ---~%  message: \"timeout after ~A seconds\"~%  ..."
                                      (or timeout 5))
                      :number number))
              (error (e)
                (dolist (af after-fns) (ignore-errors (funcall af)))
                (list :name name :status :fail
                      :detail (format nil "  ---~%  message: ~S~%  ..."
                                      (princ-to-string e))
                      :number number))))
        (error (e)
          ;; fixture setup error
          (list :name name :status :fail
                :detail (format nil "  ---~%  message: \"fixture error: ~A\"~%  ..."
                                (princ-to-string e))
                :number number))))))

;;; ------------------------------------------------------------
;;; Parallel Worker Pool
;;; ------------------------------------------------------------

(defun %run-tests-parallel (tests workers)
  "Run tests in parallel using sb-thread worker threads.
   Returns list of result plists in test-number order."
  (let* ((n (length tests))
         (results (make-array n :initial-element nil))
         (results-lock (sb-thread:make-mutex :name "results-lock"))
         (work-queue (coerce tests 'vector))
         (queue-index 0)
         (queue-lock (sb-thread:make-mutex :name "queue-lock"))
         (threads '()))
    (flet ((worker ()
             (loop
               (let ((task nil)
                     (idx nil))
                 ;; Grab next task
                 (sb-thread:with-mutex (queue-lock)
                   (when (< queue-index n)
                     (setf idx queue-index
                           task (aref work-queue queue-index))
                     (incf queue-index)))
                 (unless task (return))
                 ;; Run test (pass results seen so far for dependency checks)
                 (let* ((test-plist task)
                        (number (getf test-plist :number))
                        ;; snapshot of results so far
                        (results-snapshot
                          (sb-thread:with-mutex (results-lock)
                            (remove nil (coerce results 'list))))
                        (result (%run-single-test test-plist number results-snapshot)))
                   (sb-thread:with-mutex (results-lock)
                     (setf (aref results idx) result))
                   (%tap-print-result result))))))
      ;; Spawn workers
      (dotimes (i workers)
        (push (sb-thread:make-thread #'worker :name (format nil "test-worker-~A" i))
              threads))
      ;; Wait for all
      (dolist (th threads)
        (sb-thread:join-thread th))
      (coerce results 'list))))

(defun %run-tests-sequential (tests)
  "Run tests sequentially and return result plists."
  (let ((results '()))
    (dolist (test tests)
      (let* ((number (getf test :number))
             (result (%run-single-test test number results)))
        (push result results)
        (%tap-print-result result)))
    (nreverse results)))

;;; ------------------------------------------------------------
;;; Flaky Detection
;;; ------------------------------------------------------------

(defun %detect-flaky (all-run-results repeat)
  "Given a list of repeat result-lists, find tests with inconsistent status."
  (let ((by-name (make-hash-table)))
    (dolist (run-results all-run-results)
      (dolist (r run-results)
        (let ((name (getf r :name))
              (status (getf r :status)))
          (push status (gethash name by-name)))))
    (let ((flaky '()))
      (maphash (lambda (name statuses)
                 (let ((pass-count (count :pass statuses))
                       (total (length statuses)))
                   (when (and (< pass-count total) (> pass-count 0))
                     (push (list name pass-count total) flaky))))
               by-name)
      (when flaky
        (format t "# Flaky tests detected (inconsistent across ~A runs):~%" repeat)
        (dolist (f flaky)
          (format t "#   ~A: passed ~A/~A runs~%"
                  (first f) (second f) (third f)))))))

;;; ------------------------------------------------------------
;;; run-suite
;;; ------------------------------------------------------------

(defun run-suite (suite-name &key
                               (parallel t)
                               (random t)
                               (seed nil)
                               (workers nil)
                               (repeat 1)
                               (update-snapshots nil)
                               (tags nil)
                               (coverage nil))
  "Run all tests in suite-name (and children).
   Returns t if all tests passed, nil otherwise."
  ;; Coverage forces sequential
  (when coverage
    (setf parallel nil)
    (format t "# Coverage mode: parallel disabled~%"))

  ;; Seed random state
  (let* ((actual-seed (or seed (random most-positive-fixnum)))
         (*random-state* (sb-ext:seed-random-state actual-seed)))

    ;; Collect tests
    (let* ((tests-plists (%collect-all-suite-tests suite-name tags))
           (n (length tests-plists)))

      ;; Assign numbers in definition order
      (let ((numbered (loop for p in tests-plists
                            for i from 1
                            collect (append p (list :number i)))))

        ;; Shuffle if random
        (let ((test-vec (coerce numbered 'vector)))
          (when random
            (%fisher-yates-shuffle test-vec))
          (let ((ordered-tests (coerce test-vec 'list)))

            ;; Print TAP header
            (let ((nworkers (or workers
                                (if parallel
                                    4
                                    1))))
              (format t "# Seed: ~A~%" actual-seed)
              (format t "# Workers: ~A~%" (if parallel nworkers 1))
              (format t "TAP version 13~%")
              (format t "1..~A~%" (* n repeat))
              (force-output)

              ;; Run (with optional repeat for flaky detection)
              (let ((all-run-results '()))
                (dotimes (r repeat)
                  (when (> repeat 1)
                    (format t "# Run ~A/~A~%" (1+ r) repeat))
                  (let ((run-results
                          (if (and parallel (> nworkers 1))
                              (%run-tests-parallel ordered-tests nworkers)
                              (%run-tests-sequential ordered-tests))))
                    (push run-results all-run-results)))

                ;; Flaky detection
                (when (> repeat 1)
                  (%detect-flaky (reverse all-run-results) repeat))

                ;; Reproducibility hint
                (format t "# To reproduce this run: (run-suite '~A :seed ~A)~%"
                        suite-name actual-seed)

                ;; Compute pass/fail
                (let* ((flat-results (apply #'append (reverse all-run-results)))
                       (any-fail (some (lambda (r) (eq (getf r :status) :fail))
                                       flat-results)))

                  ;; Snapshot update mode
                  (when update-snapshots
                    (format t "# Snapshot update mode enabled~%"))

                  ;; Coverage report placeholder
                  (when coverage
                    (format t "# Coverage report: use sb-cover directly for full report~%"))

                  ;; Print result summary
                  (let* ((pass-count (count :pass flat-results :key (lambda (r) (getf r :status))))
                         (fail-count (count :fail flat-results :key (lambda (r) (getf r :status))))
                         (skip-count (count :skip flat-results :key (lambda (r) (getf r :status))))
                         (total (length flat-results))
                         (bar "#  ---------------------------------------------------"))
                    (format t "#~%")
                    (format t "~A~%" bar)
                    (format t "#  Test Results~%")
                    (format t "~A~%" bar)
                    (format t "#    PASS  ~4D~%" pass-count)
                    (format t "#    FAIL  ~4D~%" fail-count)
                    (when (> skip-count 0)
                      (format t "#    SKIP  ~4D~%" skip-count))
                    (format t "#   -------~%")
                    (format t "#   TOTAL  ~4D~%" total)
                    (format t "~A~%" bar)
                    (when any-fail
                      (format t "#~%")
                      (format t "#  Failed tests:~%")
                      (dolist (r (sort (remove-if-not (lambda (r) (eq (getf r :status) :fail))
                                                      flat-results)
                                       #'< :key (lambda (r) (getf r :number))))
                        (format t "#    [~4D] ~A~%" (getf r :number) (getf r :name)))
                      (format t "~A~%" bar))
                    (format t "#~%"))

                  (if any-fail
                      (uiop:quit 1)
                      (uiop:quit 0)))))))))))

(defun run-tests ()
  "Run all tests in the main cl-cc-suite."
  (run-suite 'cl-cc-suite :parallel t :random nil))

;;; ------------------------------------------------------------
;;; Suite Definitions (bottom of file — other files use in-suite)
;;; ------------------------------------------------------------

(defsuite cl-cc-suite :description "CL-CC Test - Main suite for all tests")
(in-suite cl-cc-suite)
