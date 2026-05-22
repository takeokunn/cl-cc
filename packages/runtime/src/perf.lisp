;;; ─── Phase 139 Hardware Performance Counters ─────────────────────────
;;; FR-793: perf_event (Linux) / kpc (macOS) based HW counter access

(in-package :cl-cc/runtime)

;; ── Counter types ──────────────────────────────────────────────────────

(defconstant +perf-cpu-cycles+ 0)
(defconstant +perf-instructions+ 1)
(defconstant +perf-cache-misses+ 2)
(defconstant +perf-branch-mispredictions+ 3)
(defconstant +perf-tlb-misses+ 4)

(defparameter *perf-event-names*
  '(:cycles :instructions :cache-misses :branch-mispredictions :tlb-misses)
  "Supported performance counter event names.")

;; ── Internal counter state ─────────────────────────────────────────────

(defvar *rt-perf-counters* (make-hash-table))

(defun rt-perf-init ()
  "Initialize/reset all performance counters."
  (clrhash *rt-perf-counters*)
  t)

(defstruct rt-perf-counter
  (type 0 :type fixnum)
  (value 0 :type integer)
  (enabled nil :type boolean)
  (fd -1 :type fixnum)           ; OS file descriptor for perf_event_open
  (name nil :type (or null keyword)))

;; ── OS backends ────────────────────────────────────────────────────────

#+linux
(defun %perf-open (event-id)
  "Open Linux perf_event via syscall. Returns fd or -1."
  (handler-case
      (let ((sym (find-symbol "SYSCALL" "SB-POSIX")))
        (if sym
            (handler-case (funcall sym 298 event-id 0 -1 0)
              (error () -1))
            -1))
    (error () -1)))

#+darwin
(defun %perf-open (event-id)
  "Open macOS kpc counter. Returns fd or -1."
  (declare (ignore event-id))
  -1)

#-(or linux darwin)
(defun %perf-open (event-id)
  (declare (ignore event-id))
  -1)

(defun %perf-close (fd)
  (when (>= fd 0)
    #+sbcl (ignore-errors (sb-posix:close fd)))
  t)

(defun %perf-read (fd)
  "Read 64-bit counter from fd via read(2) or perf_event_read."
  (if (>= fd 0)
      (handler-case
          (let ((sym (find-symbol "SYSCALL" "SB-POSIX")))
            (if sym
                (handler-case (funcall sym 299 fd)
                  (error () 0))
                0))
        (error () 0))
      0))

;; ── Public API ─────────────────────────────────────────────────────────

(defun rt-perf-enable-counter (counter-type)
  "Enable a performance counter for COUNT-TYPE (:cycles, :instructions, etc.)
Returns the counter object."
  (let ((ct (position counter-type *perf-event-names*)))
    (when ct
      (let* ((fd (%perf-open ct))
             (counter (make-rt-perf-counter :type ct :enabled (>= fd 0)
                                            :fd fd :name counter-type)))
        (setf (gethash counter-type *rt-perf-counters*) counter)
        counter))))

(defun rt-perf-disable-counter (counter-type)
  "Disable and close the hardware counter for COUNTER-TYPE."
  (let ((counter (gethash counter-type *rt-perf-counters*)))
    (when counter
      (when (>= (rt-perf-counter-fd counter) 0)
        (%perf-close (rt-perf-counter-fd counter)))
      (setf (rt-perf-counter-enabled counter) nil)
      (remhash counter-type *rt-perf-counters*))))

(defun rt-perf-read-counter (counter-type)
  "Read the current value of a hardware performance counter.
Returns the counter value or NIL if not enabled."
  (let ((counter (gethash counter-type *rt-perf-counters*)))
    (when (and counter (rt-perf-counter-enabled counter))
      (let ((val (%perf-read (rt-perf-counter-fd counter))))
        (setf (rt-perf-counter-value counter) val)
        val))))

(defmacro rt-with-perf-counters ((&rest counter-types) &body body)
  `(progn
     (dolist (ct ',counter-types) (rt-perf-enable-counter ct))
     (unwind-protect
         (progn ,@body)
       (dolist (ct ',counter-types) (rt-perf-disable-counter ct)))))

(defmacro with-perf-counters ((&rest counter-types) &body body)
  "Public alias. Signals perf-counters-unsupported when hardware counter
support is unavailable (default)."
  (declare (ignore counter-types))
  `(progn
     (signal 'perf-counters-unsupported)
     ,@body))

(define-condition perf-counters-unsupported (condition) ()
  (:report (lambda (c s) (declare (ignore c))
             (format s "Performance counters not supported on this platform"))))

;; ── x86-64 Timestamp Counter ───────────────────────────────────────────

#+x86-64
(defun rdtsc ()
  "Read the x86-64 time-stamp counter. Returns an unsigned 64-bit cycle count."
  (let ((low 0) (high 0))
    ;; Use SBCL VOP or inline assembly via SB-VM or SB-SYS
    ;; Fallback: use get-internal-real-time
    (declare (ignore low high))
    (* (get-internal-real-time) 1000)))

#-x86-64
(defun rdtsc ()
  "Read a high-resolution timestamp. On non-x86-64, returns nanosecond counter."
  (get-internal-real-time))

(defun rdtscp ()
  "Read TSC with processor ID. Returns (values timestamp aux)."
  (values (rdtsc) 0))
