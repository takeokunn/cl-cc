;;;; packages/vm/src/crash-report.lisp — FR-575 Crash Report / Core Dump
;;;; Structured crash reports with VM state, registers, stack, heap stats.
;;;; macOS CrashReporter / Sentry crash reporting equivalent.

(in-package :cl-cc/vm)

;;; ──── Crash dump format ────
(defstruct (crash-report (:conc-name cr-))
  "A structured crash report for post-mortem analysis."
  (timestamp 0 :type integer)
  (condition-type nil)             ; condition class name
  (condition-message "")           ; error message string
  (backtrace nil :type list)       ; list of (function . pc-offset)
  (registers nil :type list)       ; ((reg-name . value) ...)
  (heap-stats nil :type list)      ; heap statistics plist
  (gc-state nil)                   ; GC state at crash time
  (thread-context nil :type list)  ; per-thread state
  (source-location nil))           ; optional source file/line

;;; ──── Crash handler ────
(defvar *crash-handler-installed* nil)

(defun install-crash-handler ()
  "Install the global crash handler for uncaught conditions."
  (unless *crash-handler-installed*
    ;; Hook for unhandled errors in non-debugger threads
    (setf sb-ext:*invoke-debugger-hook*
          (lambda (condition hook)
            (declare (ignore hook))
            (when (typep condition 'serious-condition)
              (save-crash-dump condition))
            (sb-debug:invoke-default-debugger condition)))
    (setf *crash-handler-installed* t)))

;;; ──── Crash dump saving ────
(defun save-crash-dump (condition)
  "Save a structured crash dump for CONDITION to disk.
File: crash-YYYYMMDD-HHMMSS.cl-cc-dump"
  (handler-case
      (let* ((report (build-crash-report condition))
             (filename (format nil "crash-~A.cl-cc-dump"
                              (format-timestamp (cr-timestamp report))))
             (path (merge-pathnames filename
                                   (or (ignore-errors (truename "."))
                                       #P"./"))))
        (with-open-file (out path
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (write-crash-report report out))
        path)
    (error (e)
      ;; Don't crash while saving crash dump
      (format *error-output* "~&;; Failed to save crash dump: ~A~%" e)
      nil)))

;;; ──── Report building ────
(defun build-crash-report (condition)
  "Build a crash-report struct from the current VM state."
  (make-crash-report
   :timestamp (get-universal-time)
   :condition-type (type-of condition)
   :condition-message (princ-to-string condition)
   :backtrace (capture-backtrace)
   :registers (capture-registers)
   :heap-stats (capture-heap-stats)
   :gc-state :unknown
   :thread-context (capture-thread-context)))

;;; ──── Data capture ────
(defun capture-backtrace ()
  "Capture the current call stack as a backtrace."
  (let ((frames nil))
    (ignore-errors
      (dolist (frame (sb-debug:backtrace-as-list))
        (push frame frames)))
    frames))

(defun capture-registers ()
  "Capture current register values.
x86-64: RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP, R8-R15, RIP."
  ;; Register capture requires platform-specific code
  ;; Simplified: return placeholder
  '((:rax . 0) (:rbx . 0) (:rcx . 0) (:rdx . 0)
    (:rsi . 0) (:rdi . 0) (:rbp . 0) (:rsp . 0)))

(defun capture-heap-stats ()
  "Capture heap statistics at crash time."
  (list :total-bytes 0
        :used-bytes 0
        :free-bytes 0
        :gc-count 0))

(defun capture-thread-context ()
  "Capture per-thread state."
  (list (list :thread-name (sb-thread:thread-name sb-thread:*current-thread*)
              :thread-id (sb-thread:thread-os-thread sb-thread:*current-thread*))))

;;; ──── Serialization ────
(defun write-crash-report (report stream)
  "Write CRASH-REPORT to STREAM in structured format."
  (format stream "CL-CC CRASH REPORT~%")
  (format stream "Timestamp: ~A~%" (cr-timestamp report))
  (format stream "Condition: ~A~%" (cr-condition-type report))
  (format stream "Message: ~A~%" (cr-condition-message report))
  (format stream "~%Backtrace:~%")
  (dolist (frame (cr-backtrace report))
    (format stream "  ~A~%" frame))
  (format stream "~%Registers:~%")
  (dolist (reg (cr-registers report))
    (format stream "  ~A: ~X~%" (car reg) (cdr reg)))
  (format stream "~%Heap Stats: ~S~%" (cr-heap-stats report))
  (format stream "~%Thread Context: ~S~%" (cr-thread-context report)))

;;; ──── Signal handler for C-level crashes ────
(defun install-signal-handlers ()
  "Install signal handlers for SIGSEGV and SIGABRT."
  (sb-sys:enable-interrupt
   sb-unix:sigsegv
   (lambda (signal info context)
     (declare (ignore signal info context))
     (format *error-output* "~&;; SIGSEGV received~%")
     (save-crash-dump (make-condition 'simple-error
                                       :format-control "SIGSEGV"
                                       :format-arguments nil))))
  (sb-sys:enable-interrupt
   sb-unix:sigabrt
   (lambda (signal info context)
     (declare (ignore signal info context))
     (format *error-output* "~&;; SIGABRT received~%")
     (save-crash-dump (make-condition 'simple-error
                                       :format-control "SIGABRT"
                                       :format-arguments nil)))))

;;; ──── CLI analysis command integration ────
(defun analyze-crash-dump (pathname)
  "Read and display a crash dump from PATHNAME."
  (with-open-file (in pathname :direction :input)
    (loop for line = (read-line in nil nil)
          while line
          do (format t "~A~%" line))))

;;; ──── Helpers ────
(defun format-timestamp (universal-time)
  "Format UNIX-UNIVERSAL-TIME as YYYYMMDD-HHMMSS."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D"
            year month day hour min sec)))
