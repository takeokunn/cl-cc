(in-package :cl-cc/test)

;;; ── Result constructors ──────────────────────────────────────────────────
;;; All test outcomes share the same plist shape. Named constructors eliminate
;;; repeated inline `(list :name ... :status ... :detail ... :number ... :suite ...)`.

(defun %make-test-result (name number suite status detail)
  (list :name name :status status :detail detail :number number :suite suite))

(defun %format-timeout-detail (timeout)
  (format nil "  ---~%  message: \"timeout after ~A seconds\"~%  ..."
          (or timeout (%default-test-timeout))))

(defun %fail-result (name number suite detail)
  (%make-test-result name number suite :fail detail))

(defun %run-after-fns-safely (after-fns)
  (dolist (af after-fns) (ignore-errors (funcall af))))

(defun %format-backtrace-lines (n)
  "Capture N SBCL backtrace frames, each line indented for YAML embedding."
  (let ((raw (with-output-to-string (s) (sb-debug:backtrace n s))))
    (with-output-to-string (out)
      (loop with start = 0
            while (< start (length raw))
            do (let ((nl (position #\Newline raw :start start)))
                 (let ((line (subseq raw start (or nl (length raw)))))
                   (when (plusp (length line))
                     (format out "    ~A~%" line)))
                 (if nl (setf start (1+ nl)) (return)))))))

(defun %format-error-detail (e)
  "Format an unhandled error into a YAML block with condition_type and backtrace."
  (format nil "  ---~%  condition_type: ~A~%  message: ~S~%  backtrace: |~%~A  ..."
          (type-of e)
          (princ-to-string e)
          (%format-backtrace-lines 10)))

;;; ── Macro-environment isolation ──────────────────────────────────────────
;;; Each test runs in a fresh copy of the macro/expander tables so that
;;; defmacro side-effects inside one test do not bleed into the next.

(defmacro %with-isolated-macro-environment (&body body)
  `(let ((cl-cc/expand:*macro-environment*      (%copy-macro-environment))
         (cl-cc/expand:*symbol-macro-table*
           (%copy-hash-table-shallow cl-cc/expand:*symbol-macro-table*))
         (cl-cc/expand:*compiler-macro-table*
           (%copy-hash-table-shallow cl-cc/expand:*compiler-macro-table*))
         (cl-cc/expand:*macroexpand-step-cache*  (make-hash-table :test #'eq))
         (cl-cc/expand:*macroexpand-all-cache*   (make-hash-table :test #'eq))
         (cl-cc/expand:*macro-eval-fn*            cl-cc/expand:*macro-eval-fn*)
         (cl-cc/vm:*vm-hash-cons-table*           (make-hash-table :test #'equal)))
     ,@body))

(defvar *test-runner-mode* :sequential
  "Dynamic indicator of the current runner mode (:sequential or :parallel).
%run-single-test consults this to decide whether sb-ext:with-timeout is safe
to use (sequential) or whether the parallel runner will enforce the timeout
via sb-thread:join-thread :timeout instead.")

;;; ── Test body execution ───────────────────────────────────────────────────
;;; Runs FN with skip/pending/timeout/error condition handling.

(defun %run-with-sequential-timeout (fn timeout-seconds)
  "Run FN with a per-test timeout enforced by a watchdog thread that interrupts
the CURRENT thread (preserving its dynamic bindings — unlike running FN in a
sub-thread). Reliable even inside parallel worker threads, where sb-ext:with-
timeout's SIGALRM is undelivered on macOS ARM64. The watchdog exits without
interrupting when FN completes first; on timeout it signals SB-EXT:TIMEOUT in the
running thread, which %run-test-body's handler-case turns into a :fail."
  (let* ((runner   sb-thread:*current-thread*)
         (done     (sb-thread:make-semaphore))
         (watchdog (sb-thread:make-thread
                    (lambda ()
                      (unless (sb-thread:wait-on-semaphore done :timeout timeout-seconds)
                        (ignore-errors
                         (sb-thread:interrupt-thread
                          runner (lambda () (error 'sb-ext:timeout))))))
                    :name "test-timeout-watchdog")))
    (unwind-protect (funcall fn)
      (sb-thread:signal-semaphore done)
      (ignore-errors (sb-thread:join-thread watchdog)))))

;;; NOTE: the sequential timeout uses %run-with-sequential-timeout (thread-
;;; interrupt watchdog) rather than sb-ext:with-timeout, whose SIGALRM is
;;; unreliable inside sb-thread worker threads on macOS ARM64.

(defun %run-test-body (fn name number suite timeout after-fns)
  (block %run-body
    (handler-case
        (handler-bind
            ((skip-condition
               (lambda (c)
                  (return-from %run-body
                    (%make-test-result name number suite :skip (skip-reason c)))))
              (pending-condition
               (lambda (c)
                  (return-from %run-body
                    (%make-test-result name number suite :pending (pending-reason c)))))
              (expected-fail-condition
               (lambda (c)
                  (return-from %run-body
                    (%make-test-result name number suite :xfail (expected-fail-reason c)))))
             (sb-kernel:redefinition-with-defun
              (lambda (c)
                (declare (ignore c))
                (muffle-warning))))
           (if (eq *test-runner-mode* :parallel)
                (funcall fn)
                (%run-with-sequential-timeout fn (or timeout (%default-test-timeout))))
          (dolist (af after-fns) (funcall af))
          (%run-invariants)
          (%make-test-result name number suite :pass nil))
      (test-failure (c)
        (%run-after-fns-safely after-fns)
        (%fail-result name number suite (test-failure-message c)))
      (sb-ext:timeout ()
        (%run-after-fns-safely after-fns)
        (%fail-result name number suite (%format-timeout-detail timeout)))
      (error (e)
        (%run-after-fns-safely after-fns)
        (%fail-result name number suite (%format-error-detail e))))))

(defun %compute-duration-ns (start-time end-time)
  "Convert internal-time-units difference to integer nanoseconds.
LC_ALL-independent integer arithmetic, never scientific notation."
  (floor (* (- end-time start-time) 1000000000)
         internal-time-units-per-second))

(defun %run-single-test (test-plist number results-so-far)
  "Run one test and return a result plist with :duration-ns and :source-file attached.
Tests tagged :expected-fail have their status converted: pass → :xpass, fail → handled by %run-test-body as :xfail."
  (let* ((name        (getf test-plist :name))
          (fn          (getf test-plist :fn))
          (timeout     (%effective-test-timeout test-plist))
          (suite       (getf test-plist :suite))
          (source-file (getf test-plist :source-file))
          (tags        (getf test-plist :tags))
          (start-time  (get-internal-real-time))
          (duration-ns 0)
          (result nil))
    (when (uiop:getenv "CLCC_TEST_TRACE")
      (format *error-output* "# [trace] running ~A~%" name)
      (force-output *error-output*))
    (unwind-protect
         (setf result
               (block %run-body
                 (unless (%check-dependency test-plist results-so-far)
                   (return-from %run-body
                     (%make-test-result name number suite :skip "dependency failed")))
                 (multiple-value-bind (before-fns after-fns)
                     (%get-suite-fixtures suite)
                   (handler-case
                       (%with-isolated-macro-environment
                         (dolist (bf before-fns) (funcall bf))
                         (%run-test-body fn name number suite timeout after-fns))
                     (error (e)
                       (%fail-result name number suite
                                     (format nil "  ---~%  message: \"fixture error: ~A\"~%  ..."
                                             (princ-to-string e))))))))
      (setf duration-ns (%compute-duration-ns start-time (get-internal-real-time))))
    ;; Convert unexpected passes on expected-fail tests to :xpass
    (when (and result
               (eq (getf result :status) :pass)
               (member :expected-fail tags))
      (setf result (%make-test-result name number suite :xpass
                                       "UNEXPECTED PASS: test tagged :expected-fail passed. The feature may be fixed!")))
    (if result
        (append result (list :duration-ns duration-ns :source-file source-file))
        (append (%fail-result name number suite
                              "  ---~%  message: \"aborted before producing result\"~%  ...")
                (list :duration-ns duration-ns :source-file source-file)))))
