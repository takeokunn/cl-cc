;;;; src/jit/safepoints.lisp — FR-551 Safepoints (セーフポイント)
;;;; Polling-based safe points for JIT-compiled code.
;;;; JVM Safepoint / HotSpot polling page / V8 safepoints equivalent.

(in-package :cl-cc/jit)

;;; ──── Configuration ────
(defvar *safepoint-enabled* t
  "When T, safepoint polling is inserted into JIT code.")

(defvar *safepoint-interval* 4096
  "Insert a safepoint poll every N bytes of generated code.")

(defvar *safepoint-flag* (sb-sys:int-sap 0)
  "Address of the safepoint flag page. When the GC sets this page to
non-readable, the next poll triggers a SEGV caught by the signal handler.")

(defvar *safepoint-page-address* nil
  "Base address of the safepoint polling page.")

(defvar *bytes-since-last-safepoint* 0
  "Counter: bytes emitted since last safepoint poll was inserted.")

;;; ──── Safepoint poll instruction ────
(defun emit-safepoint-poll (stream)
  "Emit a safepoint polling instruction into the code stream.
x86-64: TEST [safepoint-address], 0 (7 bytes).
AArch64: LDR Wzr, [safepoint-address] (4 bytes)."
  (when (and *safepoint-enabled*
             (>= *bytes-since-last-safepoint* *safepoint-interval*))
    #+x86-64
    (progn
      ;; TEST [rip+offset], 0 — polls safepoint flag via SEGV on unreadable page
      ;; Encoding: 64 85 05 XX XX XX XX (RIP-relative)
      (let ((offset (- *safepoint-page-address*
                       (+ (stream-position stream) 7))))
        (write-byte #x64 stream)    ; FS prefix / REX (simplified)
        (write-byte #x85 stream)    ; TEST r/m32, r32
        (write-byte #x05 stream)    ; ModRM: [RIP+disp32]
        (write-sequence (encode-int32 offset) stream)))
    #-x86-64
    (progn
      ;; Generic: emit NOP sequence as placeholder
      (dotimes (i 4) (write-byte #x90 stream)))
    (setf *bytes-since-last-safepoint* 0)))

;;; ──── Safepoint page setup ────
(defun init-safepoint-page ()
  "Allocate and protect the safepoint polling page.
The GC makes this page unreadable to trigger SEGV at safepoints."
  #+sbcl
  (let ((page (sb-posix:mmap nil 4096
                              (logior sb-posix:prot-read sb-posix:prot-write)
                              (logior sb-posix:map-private sb-posix:map-anonymous)
                              -1 0)))
    (setf *safepoint-page-address* (sb-sys:sap-int page))
    (sb-posix:mprotect page 4096 sb-posix:prot-none) ; start protected
    page)
  #-sbcl
  (setf *safepoint-page-address* 0))

(defun arm-safepoint ()
  "Make the safepoint page readable (arm the safepoint mechanism)."
  (when *safepoint-page-address*
    #+sbcl
    (sb-posix:mprotect (sb-sys:int-sap *safepoint-page-address*)
                       4096
                       (logior sb-posix:prot-read))
    (values)))

(defun disarm-safepoint ()
  "Make the safepoint page unreadable (disarm — no polls trigger)."
  (when *safepoint-page-address*
    #+sbcl
    (sb-posix:mprotect (sb-sys:int-sap *safepoint-page-address*)
                       4096
                       sb-posix:prot-none)
    (values)))

;;; ──── Stop-the-World implementation ────
(defvar *stw-in-progress* nil
  "T when a Stop-the-World pause is active.")

(defvar *thread-count-at-safepoint* 0
  "Number of threads that have reached the safepoint.")

(defvar *safepoint-lock* nil
  "Lock protecting safepoint coordination.")

(defun enter-safepoint ()
  "Called by a thread when it hits a safepoint poll during STW."
  (when *stw-in-progress*
    (atomic-incf *thread-count-at-safepoint*)
    ;; Spin until STW is complete (simplified — real impl uses futex)
    (loop while *stw-in-progress*
          do (sb-thread:thread-yield))))

;;; ──── Scope macro ────
(defmacro with-safepoints (&body body)
  "Execute BODY with safepoint polling enabled for JIT code generation."
  `(let ((*safepoint-enabled* t)
         (*bytes-since-last-safepoint* 0))
     ,@body))

;;; ──── Atomic increment helper ────
(defun atomic-incf (place)
  "Atomically increment PLACE by 1. Uses SBCL atomic-incf if available."
  #+sbcl (sb-ext:atomic-incf place)
  #-sbcl (incf place))
