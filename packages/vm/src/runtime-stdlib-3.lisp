;;; runtime-stdlib-3.lisp — Missing Easy/Medium runtime stdlib-3 APIs
;;;
;;; This file intentionally fills only true gaps from docs/runtime-stdlib-3.md.
;;; Implementations already present in topic files are reused and exported rather
;;; than duplicated.

(in-package :cl-cc/vm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow '(open
            *features*
            *read-base*
            *read-eval*
            *read-suppress*
            *read-default-float-format*
            *load-pathname*
            *load-truename*)))

;;; ── FR-959: External formats ──────────────────────────────────────────────

(defparameter *default-external-format* :utf-8
  "Default external format used by the VM OPEN wrapper.")

(defvar *vm-stream-external-formats* (make-hash-table :test #'eq)
  "Weakly maps streams opened by VM OPEN to their requested external format.")

(defun %normalize-external-format (external-format)
  (or external-format *default-external-format*))

(defun open (filespec &rest args &key external-format &allow-other-keys)
  "Open FILESPEC with VM default :EXTERNAL-FORMAT handling.
The host CL OPEN does the concrete I/O; this wrapper records the requested format
for STREAM-EXTERNAL-FORMAT and supplies *DEFAULT-EXTERNAL-FORMAT* when omitted."
  (let* ((format (%normalize-external-format external-format))
         (stream (apply #'cl:open filespec :external-format format args)))
    (when stream
      (setf (gethash stream *vm-stream-external-formats*) format))
    stream))

(defun stream-external-format (stream)
  "Return STREAM's external format when known, otherwise NIL."
  (or (gethash stream *vm-stream-external-formats*)
      #+sbcl (ignore-errors (sb-impl::fd-stream-external-format stream))
      #-sbcl nil))

(defun encode-external-format (string &key (external-format *default-external-format*))
  "Encode STRING to a vector of octets using EXTERNAL-FORMAT."
  (declare (ignore external-format))
  (string-to-utf8-bytes string))

(defun decode-external-format (octets &key (external-format *default-external-format*))
  "Decode OCTETS to a string using EXTERNAL-FORMAT."
  (declare (ignore external-format))
  (utf8-bytes-to-string octets))

;;; ── FR-986: Runtime type checking surface ─────────────────────────────────
;;; TYPE-ERROR and its readers are already supplied by conditions.lisp through
;;; VM-TYPE-ERROR inheriting from CL:TYPE-ERROR.  CHECK-TYPE remains the host CL
;;; macro inherited into CL-CC/VM so existing source files keep compiling.

(defun vm-check-type (datum expected-type &optional place)
  "Return DATUM when it satisfies EXPECTED-TYPE, otherwise signal VM-TYPE-ERROR."
  (declare (ignore place))
  (unless (typep datum expected-type)
    (error 'vm-type-error :datum datum :expected-type expected-type))
  datum)

;;; ── FR-1006: Environment variables and process exit helpers ───────────────

(defparameter *command-line-args*
  #+sbcl (copy-list (cdr sb-ext:*posix-argv*))
  #-sbcl nil
  "Command-line arguments visible to VM stdlib code.")

(defun getenv (name)
  "Return environment variable NAME, preserving vm-getenv taint marking."
  (vm-getenv name))

(defun setenv (name value &key overwrite)
  "Set environment variable NAME to VALUE.  Returns VALUE on success."
  (declare (ignore overwrite))
  (setf (uiop:getenv (string name)) (string value))
  value)

(defun exit (&optional (code 0))
  "Exit the hosting process with CODE."
  (uiop:quit code))

(defun quit (&optional (code 0))
  "Compatibility alias for EXIT."
  (exit code))

;;; ── FR-1066: SBCL-compatible implementation surface ───────────────────────

(defparameter *features*
  (remove-duplicates (append '(:cl-cc :cl-cc-vm :ansi-cl :common-lisp)
                             cl:*features*)
                     :test #'eq)
  "VM-visible feature list with CL-CC identifiers and host features.")

;;; LISP-IMPLEMENTATION-TYPE/VERSION are implemented in vm-environment.lisp.

;;; ── FR-1054/1082: Reader/load state variables ─────────────────────────────

(defparameter *read-base* 10 "Default radix used by VM reader helpers.")
(defparameter *read-eval* t "Whether #. reader evaluation is enabled.")
(defparameter *read-suppress* nil "When true, reader suppresses object construction where supported.")
(defparameter *read-default-float-format* 'single-float "Default float format for the reader.")
(defparameter *load-pathname* nil "Pathname currently being loaded by VM stdlib code.")
(defparameter *load-truename* nil "Truename currently being loaded by VM stdlib code.")

(defstruct source-location
  "Source location object for parser/load annotations."
  pathname file line column form)

;;; ── FR-1112: Numeric helpers ──────────────────────────────────────────────

(defun clamp (value min max)
  "Clamp VALUE into the inclusive range [MIN, MAX]."
  (min max (max min value)))

(defun wrap (value min max)
  "Wrap VALUE into the half-open numeric interval [MIN, MAX)."
  (let ((width (- max min)))
    (when (zerop width)
      (error "Cannot wrap with identical bounds: ~S" min))
    (+ min (mod (- value min) width))))

(defun lerp (start end amount)
  "Linearly interpolate between START and END by AMOUNT."
  (+ start (* amount (- end start))))

(defun vm-clamp (value min max) (clamp value min max))
(defun vm-wrap (value min max) (wrap value min max))
(defun vm-lerp (start end amount) (lerp start end amount))
(defun vm-signum (value) (signum value))

(eval-when (:load-toplevel :execute)
  (vm-register-host-bridge 'open #'open)
  (vm-register-host-bridge 'stream-external-format #'stream-external-format)
  (vm-register-host-bridge 'encode-external-format #'encode-external-format)
  (vm-register-host-bridge 'decode-external-format #'decode-external-format)
  (vm-register-host-bridge 'vm-check-type #'vm-check-type)
  (vm-register-host-bridge 'getenv #'getenv)
  (vm-register-host-bridge 'setenv #'setenv)
  (vm-register-host-bridge 'clamp #'clamp)
  (vm-register-host-bridge 'wrap #'wrap)
  (vm-register-host-bridge 'lerp #'lerp)
  (vm-register-host-bridge 'signum #'signum))

(export '(*default-external-format*
          open
          stream-external-format
          encode-external-format
          decode-external-format
          vm-check-type
          check-type
          type-error
          type-error-datum
          type-error-expected-type
          getenv
          setenv
          *command-line-args*
          exit
          quit
          *features*
          lisp-implementation-type
          lisp-implementation-version
          *read-base*
          *read-eval*
          *read-suppress*
          *read-default-float-format*
          *load-pathname*
          *load-truename*
          source-location
          source-location-p
          make-source-location
          source-location-pathname
          source-location-file
          source-location-line
          source-location-column
          source-location-form
          clamp
          wrap
          lerp
          signum
          vm-clamp
          vm-wrap
          vm-lerp
          vm-signum))
