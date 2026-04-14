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

(defun %copy-hash-table-shallow (table)
  "Return a shallow copy of TABLE preserving its test function."
  (let ((copy (make-hash-table :test (hash-table-test table)
                               :size (hash-table-count table))))
    (maphash (lambda (k v) (setf (gethash k copy) v)) table)
    copy))

(defun %copy-macro-environment ()
  "Return a fresh macro-env instance populated from the current global macro table."
  (let* ((copy (make-instance 'cl-cc::macro-env))
         (src  (cl-cc::macro-env-table cl-cc::*macro-environment*))
         (dst  (cl-cc::macro-env-table copy)))
    (maphash (lambda (k v) (setf (gethash k dst) v)) src)
    copy))

;;; ------------------------------------------------------------
;;; Suite Definition
;;; ------------------------------------------------------------

(defmacro defsuite (name &key description parent (parallel t))
  "Define a test suite. Stores metadata in *suite-registry*.
Use :parallel NIL for suites that must always run sequentially."
  `(progn
     (setf (gethash ',name *suite-registry*)
           (list :name ',name
                 :description ,description
                 :parent ',parent
                 :parallel ,parallel
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
       :timeout :none
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

(defmacro with-fresh-prolog (&body body)
  "Run BODY with an isolated Prolog rule database and restore the prior state.
Shared by unit-level DCG tests and integration-level Prolog tests."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (make-hash-table :test 'eq)))
       (maphash (lambda (k v)
                  (setf (gethash k ,saved) v))
                cl-cc:*prolog-rules*)
       (cl-cc:clear-prolog-database)
       (unwind-protect
           (progn ,@body)
         (cl-cc:clear-prolog-database)
         (maphash (lambda (k v)
                    (setf (gethash k cl-cc:*prolog-rules*) v))
                  ,saved)))))

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
                     :form '(= ,expected ,actual)))
       t)))

(defmacro assert-eq (expected actual)
  "Assert pointer equality (eq)."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (eq ,e ,a)
         (%fail-test "assert-eq failed"
                     :expected ,e
                     :actual ,a
                     :form '(eq ,expected ,actual)))
       t)))

(defmacro assert-eql (expected actual)
  "Assert eql equality."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (eql ,e ,a)
         (%fail-test "assert-eql failed"
                     :expected ,e
                     :actual ,a
                     :form '(eql ,expected ,actual)))
       t)))

(defmacro assert-equal (expected actual)
  "Assert structural equality (equal)."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (equal ,e ,a)
         (%fail-test "assert-equal failed"
                     :expected ,e
                     :actual ,a
                     :form '(equal ,expected ,actual)))
       t)))

(defmacro assert-string= (expected actual)
  "Assert string equality."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (string= ,e ,a)
         (%fail-test "assert-string= failed"
                     :expected ,e
                     :actual ,a
                     :form '(string= ,expected ,actual)))
       t)))

(defmacro assert-null (form)
  "Assert form evaluates to nil."
  (let ((v (gensym "V")))
    `(let ((,v ,form))
       (unless (null ,v)
         (%fail-test "assert-null failed"
                     :expected nil
                     :actual ,v
                     :form ',form))
       t)))

(defmacro assert-true (form)
  "Assert form evaluates to a truthy value."
  (let ((v (gensym "V")))
    `(let ((,v ,form))
       (unless ,v
         (%fail-test "assert-true failed"
                     :expected t
                     :actual nil
                     :form ',form))
       t)))

(defmacro assert-false (form)
  "Assert form evaluates to a falsy value."
  (let ((v (gensym "V")))
    `(let ((,v ,form))
       (when ,v
         (%fail-test "assert-false failed"
                     :expected nil
                     :actual ,v
                     :form ',form))
       t)))

(defmacro assert-type (type-name object)
  "Assert object is of type type-name. Note: type-name comes first."
  (let ((o (gensym "O")))
    `(let ((,o ,object))
       (unless (typep ,o ',type-name)
         (%fail-test "assert-type failed"
                     :expected ',type-name
                     :actual (type-of ,o)
                     :form '(typep ,object ,type-name)))
       t)))

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
                        :form ',form))
          t))))

(defmacro assert-type-equal (expected actual)
  "Assert that two type-nodes are structurally equal via type-equal-p.
Produces a human-readable message using type-to-string on failure."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (type-equal-p ,e ,a)
         (%fail-test "assert-type-equal: types not equal"
                     :expected (type-to-string ,e)
                     :actual   (type-to-string ,a)
                     :form '(type-equal-p ,expected ,actual)))
       t)))

(defmacro assert-unifies (t1 t2)
  "Assert that types T1 and T2 unify successfully."
  (let ((s (gensym "S")) (ok (gensym "OK")))
    `(multiple-value-bind (,s ,ok) (type-unify ,t1 ,t2)
       (declare (ignore ,s))
       (unless ,ok
         (%fail-test "assert-unifies: types failed to unify"
                     :expected "unification success"
                     :actual   "unification failure"
                     :form '(type-unify ,t1 ,t2)))
       t)))

(defmacro assert-not-unifies (t1 t2)
  "Assert that types T1 and T2 fail to unify."
  (let ((s (gensym "S")) (ok (gensym "OK")))
    `(multiple-value-bind (,s ,ok) (type-unify ,t1 ,t2)
       (declare (ignore ,s))
       (when ,ok
         (%fail-test "assert-not-unifies: types unexpectedly unified"
                     :expected "unification failure"
                     :actual   "unification success"
                     :form '(type-unify ,t1 ,t2)))
       t)))

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

(defun %test-tags-match-p (test-tags tags)
  "Return t if TEST-TAGS intersects TAGS (inclusion filter).
NIL tags means 'no inclusion filter' → matches everything."
  (or (null tags)
      (and test-tags (some (lambda (tag) (member tag test-tags)) tags))))

(defun %test-tags-excluded-p (test-tags exclude-tags)
  "Return t if TEST-TAGS intersects EXCLUDE-TAGS."
  (and exclude-tags test-tags
       (some (lambda (tag) (member tag exclude-tags)) test-tags)))

(defun %collect-suite-tests (suite-name tags &optional exclude-tags)
  "Collect all tests belonging to suite-name (not children), filtered by tags."
  (let ((result '()))
    (maphash (lambda (test-sym plist)
               (declare (ignore test-sym))
               (let ((suite (getf plist :suite))
                     (test-tags (getf plist :tags)))
                 (when (and (eq suite suite-name)
                            (%test-tags-match-p test-tags tags)
                            (not (%test-tags-excluded-p test-tags exclude-tags)))
                   (push plist result))))
             *test-registry*)
    result))

(defun %collect-all-suite-tests (suite-name tags &optional exclude-tags exclude-suites)
  "Collect tests from suite-name and all descendant suites.
Prunes any subtree rooted at a suite named in EXCLUDE-SUITES."
  (if (member suite-name exclude-suites)
      '()
      (let ((result (%collect-suite-tests suite-name tags exclude-tags)))
        (maphash (lambda (child-name child-plist)
                   (when (eq (getf child-plist :parent) suite-name)
                     (setf result
                           (append result
                                   (%collect-all-suite-tests
                                    child-name tags exclude-tags exclude-suites)))))
                 *suite-registry*)
        result)))

(defun %fisher-yates-shuffle (vec)
  "In-place Fisher-Yates shuffle of a vector."
  (let ((n (length vec)))
    (loop for i from (1- n) downto 1
          do (let ((j (random (1+ i))))
               (rotatef (aref vec i) (aref vec j)))))
  vec)

(defun %get-suite-fixtures (suite-name)
  "Return (before-each-fns after-each-fns) for a suite, inheriting from all ancestors.
Parent fixtures run BEFORE child before-each, and AFTER child after-each (nested).
This matches standard xUnit lifecycle semantics."
  (let ((before-chain '())
        (after-chain '())
        (current suite-name))
    (loop while current
          for entry = (gethash current *suite-registry*)
          while entry
          do (setf before-chain (append (getf entry :before-each) before-chain))
             (setf after-chain  (append after-chain (getf entry :after-each)))
             (setf current (getf entry :parent)))
    (values before-chain after-chain)))

(defun %suite-parallel-p (suite-name)
  "Return T when SUITE-NAME and all of its ancestors allow parallel execution."
  (loop with current = suite-name
        while current
        for entry = (gethash current *suite-registry*)
        while entry
        do (when (null (getf entry :parallel))
             (return-from %suite-parallel-p nil))
           (setf current (getf entry :parent))
        finally (return t)))

(defun %test-parallel-safe-p (test-plist)
  "Return T when TEST-PLIST can safely run in the parallel worker pool."
  (and (null (getf test-plist :depends-on))
       (%suite-parallel-p (getf test-plist :suite))))

(defun %order-tests-for-dependencies (tests)
  "Return TESTS reordered so in-list dependencies run before dependents.
Preserves relative order whenever possible. Cycles or unresolved internal
dependency chains fall back to the original remaining order."
  (let* ((all-names (mapcar (lambda (test) (getf test :name)) tests))
         (emitted-names '())
         (pending tests)
         (ordered '()))
    (labels ((dependency-ready-p (test)
               (let ((dep (getf test :depends-on)))
                 (or (null dep)
                     (member dep emitted-names :test #'eq)
                     (not (member dep all-names :test #'eq))))))
      (loop while pending
            for ready = (find-if #'dependency-ready-p pending)
            do (if ready
                   (progn
                     (setf ordered (append ordered (list ready)))
                     (push (getf ready :name) emitted-names)
                     (setf pending (remove ready pending :count 1 :test #'eq)))
                   (progn
                     (setf ordered (append ordered pending))
                     (return))))
      ordered)))

(defun %check-dependency (test-plist all-results)
  "Return nil if dependency failed (test should be skipped), t otherwise."
  (let ((dep (getf test-plist :depends-on)))
    (if (null dep)
        t
        (let ((dep-result (find dep all-results :key (lambda (r) (getf r :name)))))
          (if dep-result
              (eq (getf dep-result :status) :pass)
              t)))))  ; dependency not yet run — allow

(defun %default-test-timeout ()
  "Return the default per-test timeout in seconds.
10s is the team-chosen target: every test is expected to finish within this
budget, otherwise the test or the underlying logic is expected to be
refactored (rather than the timeout being relaxed). Pipeline-heavy tests
that still need stdlib compilation rely on the *stdlib-expanded-cache*
in src/compile/pipeline.lisp to stay under budget. Individual tests
may still override via an explicit `:timeout N' in the deftest form, but
such overrides are discouraged and flagged during review. Override the
global default via environment variable `timeout' if needed."
  (let ((raw (uiop:getenv "timeout")))
    (or (and raw
             (ignore-errors
               (let ((parsed (parse-integer raw)))
                 (and (plusp parsed) parsed))))
        10)))

(defun %run-single-test (test-plist number results-so-far)
  "Run one test and return a result plist."
  (let* ((name     (getf test-plist :name))
         (fn       (getf test-plist :fn))
         (timeout  (getf test-plist :timeout))
         (suite    (getf test-plist :suite)))
    ;; Optional hang-diagnosis tracing (opt-in via CLCC_TEST_TRACE=1)
    (when (uiop:getenv "CLCC_TEST_TRACE")
      (format *error-output* "# [trace] running ~A~%" name)
      (force-output *error-output*))
    ;; Check dependency
    (unless (%check-dependency test-plist results-so-far)
      (return-from %run-single-test
        (list :name name :status :skip :detail "dependency failed" :number number)))
    ;; Run before-each fixtures
    (multiple-value-bind (before-fns after-fns)
        (%get-suite-fixtures suite)
      (handler-case
          (progn
            (let ((cl-cc::*macro-environment* (%copy-macro-environment))
                  (cl-cc::*symbol-macro-table*
                    (%copy-hash-table-shallow cl-cc::*symbol-macro-table*))
                  (cl-cc::*compiler-macro-table*
                    (%copy-hash-table-shallow cl-cc::*compiler-macro-table*))
                  (cl-cc::*macroexpand-step-cache*
                    (make-hash-table :test #'eq :weakness :key))
                  (cl-cc::*macroexpand-all-cache*
                    (make-hash-table :test #'eq :weakness :key)))
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
                    ;; NOTE: sb-ext:with-timeout relies on SIGALRM delivery
                    ;; which is unreliable inside sb-thread worker threads —
                    ;; parallel runs used to hang intermittently because the
                    ;; alarm never fired. Sequential runs still use it below;
                    ;; parallel runs enforce timeouts via join-thread :timeout
                    ;; in %run-tests-parallel, so this branch is the
                    ;; sequential-only fallback and is always safe.
                    (if (or (eq timeout :none)
                            (eq *test-runner-mode* :parallel))
                        (funcall fn)
                        (sb-ext:with-timeout (or timeout (%default-test-timeout))
                          (funcall fn)))
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
                                        (if (eq timeout :none) "disabled" (or timeout (%default-test-timeout))))
                        :number number))
                (error (e)
                  (dolist (af after-fns) (ignore-errors (funcall af)))
                  (list :name name :status :fail
                        :detail (format nil "  ---~%  message: ~S~%  ..."
                                        (princ-to-string e))
                        :number number)))))
        (error (e)
          ;; fixture setup error
          (list :name name :status :fail
                :detail (format nil "  ---~%  message: \"fixture error: ~A\"~%  ..."
                                (princ-to-string e))
                :number number))))))

;;; ------------------------------------------------------------
;;; Parallel Worker Pool
;;; ------------------------------------------------------------

(defvar *test-runner-mode* :sequential
  "Dynamic indicator of the current runner mode (:sequential or :parallel).
%run-single-test consults this to decide whether sb-ext:with-timeout is safe
to use (sequential) or whether the parallel runner will enforce the timeout
via sb-thread:join-thread :timeout instead.")

(defun %effective-test-timeout (test-plist)
  (let ((tm (getf test-plist :timeout)))
    (cond
      ((eq tm :none) nil)        ; no timeout
      ((and (integerp tm) (plusp tm)) tm)
      (t (%default-test-timeout)))))

(defun %run-tests-parallel (tests workers)
  "Run tests in parallel using sb-thread worker threads.
Each task is executed in its own short-lived sub-thread, and the worker
waits for it with sb-thread:join-thread :timeout. If the sub-thread does
not finish in time it is terminated and the test is recorded as a
timeout fail, guaranteeing the runner can never wedge on a hanging test."
  (let* ((*test-runner-mode* :parallel)
         (n (length tests))
         (results (make-array n :initial-element nil))
         (results-lock (sb-thread:make-mutex :name "results-lock"))
         (work-queue (coerce tests 'vector))
         (queue-index 0)
         (queue-lock (sb-thread:make-mutex :name "queue-lock"))
         (threads '()))
    (labels ((run-with-timeout (test-plist number snapshot)
               (let* ((timeout (%effective-test-timeout test-plist))
                      (result-cell (list nil))
                      (done-lock (sb-thread:make-mutex :name "done"))
                      (done-cv (sb-thread:make-waitqueue :name "done-cv"))
                      (done nil)
                      (runner-thread
                        (sb-thread:make-thread
                         (lambda ()
                           ;; CRITICAL: SBCL sub-threads do NOT inherit the
                           ;; parent's dynamic bindings, so we must rebind
                           ;; *test-runner-mode* here explicitly. Without this,
                           ;; %run-single-test would see the global default
                           ;; (:sequential) and re-enter sb-ext:with-timeout,
                           ;; defeating the parallel timeout enforcement.
                           (let ((*test-runner-mode* :parallel))
                             (let ((r (%run-single-test test-plist number snapshot)))
                               (sb-thread:with-mutex (done-lock)
                                 (setf (car result-cell) r
                                       done t)
                                 (sb-thread:condition-notify done-cv)))))
                         :name (format nil "test-case-~A" number))))
                 (sb-thread:with-mutex (done-lock)
                   (if timeout
                       (loop until done
                             do (unless (sb-thread:condition-wait
                                         done-cv done-lock :timeout timeout)
                                  (return)))
                       (loop until done
                             do (sb-thread:condition-wait done-cv done-lock))))
                 (cond
                   (done
                    (ignore-errors (sb-thread:join-thread runner-thread))
                    (car result-cell))
                   (t
                    (ignore-errors
                     (sb-thread:terminate-thread runner-thread))
                    (ignore-errors (sb-thread:join-thread runner-thread))
                    (list :name (getf test-plist :name)
                          :status :fail
                          :detail (format nil "  ---~%  message: \"timeout after ~A seconds (parallel runner killed thread)\"~%  ..."
                                          timeout)
                          :number number)))))
             (worker ()
               (loop
                 (let ((task nil)
                       (idx nil))
                   (sb-thread:with-mutex (queue-lock)
                     (when (< queue-index n)
                       (setf idx queue-index
                             task (aref work-queue queue-index))
                       (incf queue-index)))
                   (unless task (return))
                   (let* ((test-plist task)
                          (number (getf test-plist :number))
                          (results-snapshot
                            (sb-thread:with-mutex (results-lock)
                              (remove nil (coerce results 'list))))
                          (result (run-with-timeout test-plist number
                                                    results-snapshot)))
                     (sb-thread:with-mutex (results-lock)
                       (setf (aref results idx) result))
                     (%tap-print-result result))))))
      (dotimes (i workers)
        (push (sb-thread:make-thread #'worker :name (format nil "test-worker-~A" i))
              threads))
      (dolist (th threads)
        (sb-thread:join-thread th))
      (coerce results 'list))))

(defun %run-tests-sequential (tests &optional results-so-far)
  "Run tests sequentially and return result plists.
RESULTS-SO-FAR is consulted for dependency checks but is not included in the
returned list."
  (let ((results '()))
    (dolist (test tests)
      (let* ((number (getf test :number))
             (result (%run-single-test test number
                                       (append results-so-far (reverse results)))))
        (push result results)
        (%tap-print-result result)))
    (nreverse results)))

(defun %run-tests-mixed (tests workers)
  "Run TESTS with a mixed strategy: parallel-safe tests use the worker pool,
while tests from explicitly serial suites and tests with dependencies run
sequentially in dependency-safe input order."
  (let ((results '())
        (tests (%order-tests-for-dependencies tests))
        (parallel-batch '()))
    (flet ((flush-parallel-batch ()
             (when parallel-batch
               (setf results
                     (append results
                             (%run-tests-parallel (nreverse parallel-batch)
                                                  workers)))
               (setf parallel-batch '()))))
      (dolist (test tests)
        (if (%test-parallel-safe-p test)
            (push test parallel-batch)
            (progn
              (flush-parallel-batch)
              (setf results
                    (append results
                            (%run-tests-sequential (list test) results))))))
      (flush-parallel-batch)
      results)))

(defun %effective-worker-count (ordered-tests parallel workers)
  "Return the effective worker count for ORDERED-TESTS.
Serial runs, serial-only batches, and single-worker requests all collapse to 1."
  (if (and parallel
           (> (or workers 4) 1)
           (some #'%test-parallel-safe-p ordered-tests))
      (or workers 4)
      1))

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
                               (exclude-tags nil)
                               (exclude-suites nil)
                               (coverage nil)
                               (warm-stdlib t))
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
    (let* ((tests-plists (%collect-all-suite-tests suite-name tags
                                                    exclude-tags exclude-suites))
           (n (length tests-plists)))
      (when (or exclude-tags exclude-suites)
        (format t "# Excluding tags: ~S~%# Excluding suites: ~S~%"
                exclude-tags exclude-suites))

      ;; Assign numbers in definition order
      (let ((numbered (loop for p in tests-plists
                            for i from 1
                            collect (append p (list :number i)))))

        ;; Shuffle if random
        (let ((test-vec (coerce numbered 'vector)))
          (when random
            (%fisher-yates-shuffle test-vec))
          (let* ((ordered-tests (%order-tests-for-dependencies
                                 (coerce test-vec 'list)))
                 (effective-workers
                   (%effective-worker-count ordered-tests parallel workers)))

            ;; Print TAP header
            (let ((nworkers effective-workers))
              (format t "# Seed: ~A~%" actual-seed)
              (format t "# Workers: ~A~%" nworkers)
              (format t "TAP version 13~%")
              (format t "1..~A~%" (* n repeat))
              (force-output)

               ;; Optionally warm the stdlib AST cache before any tests run.
               ;; This is valuable for broad/heavy runs, but far too expensive
               ;; for the default fast-path targets.
               (when warm-stdlib
                 (ignore-errors (cl-cc::warm-stdlib-cache)))

              ;; Run (with optional repeat for flaky detection)
              (let ((all-run-results '()))
                (dotimes (r repeat)
                  (when (> repeat 1)
                    (format t "# Run ~A/~A~%" (1+ r) repeat))
                  (let ((run-results
                          (if (and parallel (> nworkers 1))
                              (%run-tests-mixed ordered-tests nworkers)
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

(defun run-tests (&key
                    (tags nil)
                    (exclude-tags nil)
                    (exclude-suites nil)
                    (parallel t)
                    (random nil))
  "Run the canonical CL-CC test plan.

This single entry point executes unit, integration, property-based, and e2e suites.
Use the filtering keywords for focused debugging from the REPL, but the public
automation workflow is always `make test`."
  (run-suite 'cl-cc-suite
             :parallel parallel
             :random random
             :warm-stdlib t
             :tags tags
             :exclude-tags exclude-tags
             :exclude-suites exclude-suites))

(defun run-all-tests ()
  "Backward-compatible alias for the canonical test plan."
  (run-tests))

(defun run-tests-extended ()
  "Backward-compatible alias for the canonical test plan."
  (run-tests))

(defun %resolve-suite (package-name symbol-name)
  (let* ((pkg (find-package package-name))
         (sym (and pkg (find-symbol symbol-name pkg))))
    (unless sym
      (error "Suite ~A::~A not found (package not loaded?)"
             package-name symbol-name))
    sym))

(defun run-selfhost-tests ()
  "Focused helper for running only the self-hosting e2e suite from the REPL."
  (run-suite (%resolve-suite "CL-CC/TEST" "CL-CC-E2E-SUITE")
             :parallel t :random nil :warm-stdlib nil))

(defun run-pbt-tests ()
  "Focused helper for running only the property-based integration suites.
Honors CLCC_PBT_COUNT to bound per-property case counts."
  (let* ((pbt (%resolve-suite "CL-CC/PBT" "CL-CC-PBT-SUITE")))
    (run-suite pbt :parallel t :random nil :warm-stdlib nil)))

;;; ------------------------------------------------------------
;;; Suite Definitions (bottom of file — other files use in-suite)
;;; ------------------------------------------------------------

(defsuite cl-cc-suite :description "CL-CC test root suite")

(defsuite cl-cc-unit-suite
  :description "Unit tests"
  :parent cl-cc-suite)

;; Integration tests that exercise the full compile pipeline via run-string.
;; Each case invokes parse → expand → cps → optimize → codegen → execute.
;; Heavy tests rely on *stdlib-expanded-cache* being pre-warmed by run-suite
;; before the parallel workers start.
(defsuite cl-cc-integration-suite
  :description "Full-pipeline integration tests"
  :parent cl-cc-suite)

(defsuite cl-cc-integration-serial-suite
  :description "Sequential-only integration tests"
  :parent cl-cc-integration-suite
  :parallel nil)

(defsuite cl-cc-e2e-suite
  :description "End-to-end tests"
  :parent cl-cc-suite)

(defsuite cl-cc-serial-suite
  :description "Sequential-only unit tests"
  :parent cl-cc-unit-suite
  :parallel nil)

(in-suite cl-cc-suite)
