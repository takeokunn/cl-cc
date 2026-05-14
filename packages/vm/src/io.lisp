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
   (standard-output :initform *standard-output* :initarg :output-stream
                    :accessor vm-standard-output
                    :documentation "Standard output stream")
   (string-streams :initform (make-hash-table :test #'eql)
                   :reader vm-string-streams
                   :documentation "Hash table for in-memory string streams"))
  (:documentation "Extended VM state with file I/O capabilities."))

;;; ─── VM State Clone ──────────────────────────────────────────────────────────

(defun %copy-ht-into (src dst)
  "Replace DST's contents with a shallow copy of SRC."
  (clrhash dst)
  (maphash (lambda (k v) (setf (gethash k dst) v)) src))

(defun %copy-vm-symbol-property-table-into (src dst)
  "Replace DST's contents with a copy of SRC that preserves plist isolation."
  (clrhash dst)
  (maphash (lambda (symbol entry)
             (setf (gethash symbol dst)
                   (cl-cc/vm::%vm-copy-symbol-property-entry entry)))
           src))

(defun clone-vm-state (source &key (output-stream *standard-output*))
  "Create a new vm-io-state seeded with the runtime state from SOURCE.
 Copies function-registry, class-registry, global-vars, heap, heap-counter,
 user/system symbol property state, and related metadata so user code can call
 stdlib functions and access stdlib globals without recompiling the stdlib.
Registers, call-stack, handler-stack, method-call-stack start fresh so
each test begins with a clean execution context."
  (let ((clone (make-vm-state :output-stream output-stream)))
    (%copy-ht-into (vm-function-registry source) (vm-function-registry clone))
    (%copy-ht-into (vm-class-registry    source) (vm-class-registry    clone))
    (%copy-ht-into (vm-global-vars       source) (vm-global-vars       clone))
    (%copy-ht-into (vm-state-heap        source) (vm-state-heap        clone))
    (%copy-vm-symbol-property-table-into (vm-symbol-plists source)
                                         (vm-symbol-plists clone))
    (%copy-vm-symbol-property-table-into (vm-system-symbol-plists source)
                                         (vm-system-symbol-plists clone))
    (setf (vm-heap-counter clone) (vm-heap-counter source))
    (setf (vm-symbol-plist-read-barrier clone)
          (vm-symbol-plist-read-barrier source))
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

(defun %vm-bridge-stream-arg (value)
  "Resolve a VM stream handle passed through a host bridge callable."
  (if *vm-current-state*
      (vm-get-stream *vm-current-state* value)
      value))

(defun %vm-stream-handle-for-stream (state stream)
  "Return the VM handle for STREAM in STATE, or NIL when STREAM is direct."
  (let ((found nil))
    (maphash (lambda (handle candidate)
               (when (eq candidate stream)
                 (setf found handle)))
             (vm-open-files state))
    (unless found
      (maphash (lambda (handle candidate)
                 (when (eq candidate stream)
                   (setf found handle)))
               (vm-string-streams state)))
    found))

(defun %vm-bridge-stream-result (stream)
  "Convert host STREAM back to its VM handle when one exists."
  (if *vm-current-state*
      (or (%vm-stream-handle-for-stream *vm-current-state* stream) stream)
      stream))

(defun %vm-bridge-symbol-stream-value (symbol)
  "Return SYMBOL's current stream value, preferring the active VM global store."
  (if *vm-current-state*
      (multiple-value-bind (value found-p)
          (gethash symbol (vm-global-vars *vm-current-state*))
        (if found-p
            (%vm-bridge-stream-arg value)
            (symbol-value symbol)))
      (symbol-value symbol)))

(defun %vm-bridge-make-synonym-stream (symbol)
  "Construct a usable stream for VM synonym-stream calls.

Host synonym streams follow host dynamic bindings, not VM global bindings, so
the VM bridge resolves the current VM symbol value to its underlying stream."
  (if *vm-current-state*
      (%vm-bridge-symbol-stream-value symbol)
      (make-synonym-stream symbol)))

(defun %vm-bridge-make-broadcast-stream (&rest streams)
  (apply #'make-broadcast-stream (mapcar #'%vm-bridge-stream-arg streams)))

(defun %vm-bridge-make-two-way-stream (input-stream output-stream)
  (make-two-way-stream (%vm-bridge-stream-arg input-stream)
                       (%vm-bridge-stream-arg output-stream)))

(defun %vm-bridge-make-echo-stream (input-stream output-stream)
  (make-echo-stream (%vm-bridge-stream-arg input-stream)
                    (%vm-bridge-stream-arg output-stream)))

(defun %vm-bridge-make-concatenated-stream (&rest streams)
  (apply #'make-concatenated-stream (mapcar #'%vm-bridge-stream-arg streams)))

(defun %vm-bridge-broadcast-stream-streams (stream)
  (mapcar #'%vm-bridge-stream-result (broadcast-stream-streams stream)))

(defun %vm-bridge-two-way-stream-input-stream (stream)
  (%vm-bridge-stream-result (two-way-stream-input-stream stream)))

(defun %vm-bridge-two-way-stream-output-stream (stream)
  (%vm-bridge-stream-result (two-way-stream-output-stream stream)))

(defun %vm-bridge-echo-stream-input-stream (stream)
  (%vm-bridge-stream-result (echo-stream-input-stream stream)))

(defun %vm-bridge-echo-stream-output-stream (stream)
  (%vm-bridge-stream-result (echo-stream-output-stream stream)))

(defun %vm-bridge-concatenated-stream-streams (stream)
  (mapcar #'%vm-bridge-stream-result (concatenated-stream-streams stream)))

(eval-when (:load-toplevel :execute)
  (vm-register-host-bridge 'make-synonym-stream #'%vm-bridge-make-synonym-stream)
  (vm-register-host-bridge 'make-broadcast-stream #'%vm-bridge-make-broadcast-stream)
  (vm-register-host-bridge 'make-two-way-stream #'%vm-bridge-make-two-way-stream)
  (vm-register-host-bridge 'make-echo-stream #'%vm-bridge-make-echo-stream)
  (vm-register-host-bridge 'make-concatenated-stream #'%vm-bridge-make-concatenated-stream)
  (vm-register-host-bridge 'broadcast-stream-streams #'%vm-bridge-broadcast-stream-streams)
  (vm-register-host-bridge 'two-way-stream-input-stream #'%vm-bridge-two-way-stream-input-stream)
  (vm-register-host-bridge 'two-way-stream-output-stream #'%vm-bridge-two-way-stream-output-stream)
  (vm-register-host-bridge 'echo-stream-input-stream #'%vm-bridge-echo-stream-input-stream)
  (vm-register-host-bridge 'echo-stream-output-stream #'%vm-bridge-echo-stream-output-stream)
  (vm-register-host-bridge 'concatenated-stream-streams #'%vm-bridge-concatenated-stream-streams))

;;; I/O instruction class declarations are in io-instructions.lisp (loads next).
