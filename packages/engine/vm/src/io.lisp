(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — I/O State, Constants, and Stream Helpers
;;;
;;; Contains:
;;;   - vm-io-state CLOS class (open-files, file-counter, std I/O, string-streams)
;;;   - +stdin-handle+, +stdout-handle+, +eof-value+ constants
;;;   - vm-get-stream      — resolve handle → CL stream
;;;   - vm-allocate-file-handle — allocate a fresh file handle integer
;;;   - vm-stream-open-p   — check if a handle is currently open
;;;
;;; All define-vm-instruction forms for I/O operations are in io-instructions.lisp
;;; (loads immediately after this file).
;;; execute-instruction methods are in io-execute.lisp.
;;; run-compiled-with-io / run-string-with-io are in io-runners.lisp.
;;;
;;; Load order: after vm-run.lisp, before io-instructions.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── VM I/O State ────────────────────────────────────────────────────────────

(defclass vm-io-state (vm-state)
  ((open-files :initform (make-hash-table :test #'eql)
               :reader vm-open-files
               :documentation "Hash table mapping file handles to streams")
   (file-counter :initform 0 :accessor vm-file-counter
                 :documentation "Counter for generating unique file handles")
   (standard-input :initform *standard-input* :accessor vm-standard-input
                   :documentation "Standard input stream")
   (standard-output :initform *standard-output* :accessor vm-standard-output
                    :documentation "Standard output stream")
   (string-streams :initform (make-hash-table :test #'eql)
                   :reader vm-string-streams
                   :documentation "Hash table for in-memory string streams"))
  (:documentation "Extended VM state with file I/O capabilities."))

;;; ─── VM State Clone ──────────────────────────────────────────────────────────

(defun clone-vm-state (source &key (output-stream *standard-output*))
  "Create a new vm-io-state seeded with the runtime state from SOURCE.
Copies function-registry, class-registry, global-vars, heap, heap-counter,
and symbol-plists so user code can call stdlib functions and access stdlib
globals without recompiling the stdlib.
Registers, call-stack, handler-stack, method-call-stack start fresh so
each test begins with a clean execution context."
  (let ((clone (make-instance 'vm-io-state :output-stream output-stream)))
    (flet ((copy-ht-into (src dst)
             (clrhash dst)
             (maphash (lambda (k v) (setf (gethash k dst) v)) src)))
      (copy-ht-into (vm-function-registry source) (vm-function-registry clone))
      (copy-ht-into (vm-class-registry    source) (vm-class-registry    clone))
      (copy-ht-into (vm-global-vars       source) (vm-global-vars       clone))
      (copy-ht-into (vm-state-heap        source) (vm-state-heap        clone))
      (copy-ht-into (vm-symbol-plists     source) (vm-symbol-plists     clone)))
    (setf (vm-heap-counter clone) (vm-heap-counter source))
    (setf (vm-standard-output clone) output-stream)
    clone))

;;; ─── File Handle Constants ───────────────────────────────────────────────────

(defconstant +stdin-handle+  0    "File handle for standard input")
(defconstant +stdout-handle+ 1    "File handle for standard output")
(defconstant +eof-value+     :eof "Special value returned at end of file")

;;; ─── Stream Helper Functions ─────────────────────────────────────────────────

(defun vm-get-stream (state handle)
  "Resolve HANDLE to a CL stream from STATE.
Accepts: direct CL stream objects, +stdin-handle+ (0), +stdout-handle+ (1),
or any handle allocated by vm-allocate-file-handle."
  (cond
    ((streamp handle)              handle)
    ((eql handle +stdin-handle+)   (vm-standard-input state))
    ((eql handle +stdout-handle+)  (vm-standard-output state))
    (t (or (gethash handle (vm-open-files state))
           (gethash handle (vm-string-streams state))
           (error "Invalid file handle: ~A" handle)))))

(defun vm-allocate-file-handle (state)
  "Allocate and return a new unique integer file handle (>= 2 to avoid stdin/stdout)."
  (let ((handle (max 2 (1+ (vm-file-counter state)))))
    (setf (vm-file-counter state) handle)
    handle))

(defun vm-stream-open-p (state handle)
  "Return true if HANDLE refers to a currently open stream in STATE."
  (or (streamp handle)
      (and (eql handle +stdin-handle+)  (vm-standard-input state))
      (and (eql handle +stdout-handle+) (vm-standard-output state))
      (gethash handle (vm-open-files state))
      (gethash handle (vm-string-streams state))))

;;; I/O instruction class declarations are in io-instructions.lisp (loads next).
