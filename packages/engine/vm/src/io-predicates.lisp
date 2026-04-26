;;;; io-predicates.lisp — VM Stream Predicate, Binary I/O, and Load-File Execution
;;;;
;;;; Data-driven dispatch for stream predicates (define-stream-predicate-instruction),
;;;; binary I/O (read-byte/write-byte), stream control, vm-write-line, vm-load-file,
;;;; run-compiled-with-io, run-string-with-io.
;;;;
;;;; Core stream I/O (open/close/read-char/write-char etc.) are in io-execute.lisp.
;;;; Load order: after io-execute.lisp.

(in-package :cl-cc/vm)

;;; Stream Predicate Execution
;;;
;;; Values can be either direct CL stream objects or integer handles.
;;; We resolve handles to streams before applying predicates.

(defun %resolve-integer-stream-handle (handle state)
  "Resolve integer HANDLE to a CL stream via STATE's stream tables."
  (cond
    ((eql handle +stdin-handle+)  (vm-standard-input state))
    ((eql handle +stdout-handle+) (vm-standard-output state))
    ((typep state 'vm-io-state)
     (or (gethash handle (vm-open-files state))
         (gethash handle (vm-string-streams state))))
    (t nil)))

(defun %resolve-stream-val (state val)
  "Resolve VAL to a CL stream: if it's already a stream, return it;
if it's an integer handle, look it up in STATE's stream tables."
  (or (and (streamp val) val)
      (and (integerp val) (%resolve-integer-stream-handle val state))))

;;; Stream predicate dispatch — data table drives code generation.
;;; Each entry maps an instruction type to its CL predicate (or nil = just test existence).
(defmacro define-stream-predicate-instruction (inst-type pred-fn)
  "Generate an execute-instruction that resolves a stream and applies PRED-FN.
PRED-FN nil means test stream existence only."
  `(defmethod execute-instruction ((inst ,inst-type) state pc labels)
     (declare (ignore labels))
     (let* ((val    (vm-reg-get state (vm-src inst)))
            (stream (%resolve-stream-val state val)))
       (vm-reg-set state (vm-dst inst)
                   ,(if pred-fn
                        `(if (and stream (,pred-fn stream)) t nil)
                        `(if stream t nil)))
       (values (1+ pc) nil nil))))

(define-stream-predicate-instruction vm-streamp              nil)
(define-stream-predicate-instruction vm-input-stream-p       input-stream-p)
(define-stream-predicate-instruction vm-output-stream-p      output-stream-p)
(define-stream-predicate-instruction vm-open-stream-p        open-stream-p)
(define-stream-predicate-instruction vm-interactive-stream-p interactive-stream-p)

(defmethod execute-instruction ((inst vm-stream-element-type-inst) state pc labels)
  (declare (ignore labels))
  (let* ((val (vm-reg-get state (vm-src inst)))
         (stream (%resolve-stream-val state val)))
    (vm-reg-set state (vm-dst inst) (if stream (stream-element-type stream) nil))
    (values (1+ pc) nil nil)))

;;; Binary I/O Execution

(defmethod execute-instruction ((inst vm-read-byte) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle))
         (byte (read-byte stream nil +eof-value+)))
    (vm-reg-set state (vm-dst inst) byte)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-write-byte) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (byte (vm-reg-get state (vm-byte-val inst)))
         (stream (vm-get-stream state handle)))
    (write-byte byte stream)
    (values (1+ pc) nil nil)))

;;; Stream control dispatch — data table drives code generation.
(defmacro define-stream-control-instruction (inst-type cl-fn)
  "Generate an execute-instruction that resolves a stream handle and calls CL-FN on it."
  `(defmethod execute-instruction ((inst ,inst-type) state pc labels)
     (declare (ignore labels))
     (let* ((handle (vm-reg-get state (vm-file-handle inst)))
            (stream (vm-get-stream state handle)))
       (,cl-fn stream)
       (values (1+ pc) nil nil))))

(define-stream-control-instruction vm-force-output  force-output)
(define-stream-control-instruction vm-finish-output finish-output)
(define-stream-control-instruction vm-clear-input   clear-input)
(define-stream-control-instruction vm-clear-output  clear-output)

(defmethod execute-instruction ((inst vm-listen-inst) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle)))
    (vm-reg-set state (vm-dst inst) (if (listen stream) t nil))
    (values (1+ pc) nil nil)))

;;; write-line Execution

(defmethod execute-instruction ((inst vm-write-line) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (str (vm-reg-get state (vm-str-reg inst)))
         (stream (vm-get-stream state handle)))
    (write-line str stream)
    (values (1+ pc) nil nil)))

;;; Load File Execution
;;; our-load is defined in pipeline.lisp (loaded after this file).
;;; Use find-symbol + symbol-function to avoid forward reference and
;;; to resolve the canonical symbol from :cl-cc/compile.

(defmethod execute-instruction ((inst vm-load-file) state pc labels)
  (declare (ignore labels))
  (let* ((path (vm-reg-get state (vm-src inst)))
         (result (cl-cc/bootstrap:our-load path)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; run-compiled-with-io and run-string-with-io are in io-runners.lisp (loads next).
