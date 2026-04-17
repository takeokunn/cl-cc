(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — I/O Instruction Execution
;;;
;;; Contains: execute-instruction methods for all I/O instruction types
;;; (vm-open-file, vm-close-file, vm-read-char, vm-read-line, vm-write-char,
;;; vm-write-string, vm-peek-char, vm-unread-char, vm-file-position,
;;; vm-file-length, vm-eof-p, vm-make-string-stream, vm-get-string-from-stream,
;;; stream predicates via define-stream-predicate-instruction,
;;; vm-stream-element-type-inst, binary I/O, stream control instructions,
;;; vm-write-line, vm-load-file), run-compiled-with-io, run-string-with-io.
;;;
;;; Helper: %resolve-stream-val, define-stream-predicate-instruction,
;;;         define-stream-control-instruction.
;;;
;;; Load order: after io.lisp, before format.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defmethod execute-instruction ((inst vm-open-file) state pc labels)
  (declare (ignore labels))
  (handler-case
      (let* ((path-str (vm-reg-get state (vm-path inst)))
             (direction (vm-file-direction inst))
             ;; Use user-specified if-exists/if-not-exists, falling back to defaults
             (if-exists (or (vm-if-exists inst) :supersede))
             (if-not-exists (or (vm-if-not-exists inst)
                                (if (eq direction :output) :create :error)))
             (handle (vm-allocate-file-handle state))
             (stream (open path-str
                          :direction direction
                          :if-exists if-exists
                          :if-does-not-exist if-not-exists)))
        (setf (gethash handle (vm-open-files state)) stream)
        (vm-reg-set state (vm-dst inst) handle)
        (values (1+ pc) nil nil))
    (file-error (e)
      (error "vm-open-file: Failed to open file: ~A" e))))

(defmethod execute-instruction ((inst vm-close-file) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (gethash handle (vm-open-files state))))
    (cond
      ;; Don't close stdin/stdout
      ((or (eql handle +stdin-handle+) (eql handle +stdout-handle+))
       (values (1+ pc) nil nil))
      ;; Close regular file stream (integer handle)
      (stream
       (close stream)
       (remhash handle (vm-open-files state))
       (values (1+ pc) nil nil))
      ;; Check string streams
      ((gethash handle (vm-string-streams state))
       (remhash handle (vm-string-streams state))
       (values (1+ pc) nil nil))
      ;; Direct CL stream object (from host-bridge make-string-output-stream etc.)
      ((streamp handle)
       (close handle)
       (values (1+ pc) nil nil))
      (t
       (error "vm-close-file: Invalid file handle: ~A" handle)))))

(defmethod execute-instruction ((inst vm-read-char) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle))
         (char (read-char stream nil +eof-value+)))
    (vm-reg-set state (vm-dst inst) char)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-read-line) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle)))
    (multiple-value-bind (line missing-newline-p)
        (read-line stream nil +eof-value+)
      (declare (ignore missing-newline-p))
      ;; read-line returns the line even when EOF terminates it.
      ;; Only return :eof when the stream was already at EOF (line = +eof-value+).
      (vm-reg-set state (vm-dst inst) line)
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-write-char) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (char (vm-reg-get state (vm-char-reg inst)))
         (stream (vm-get-stream state handle)))
    (write-char char stream)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-write-string) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (str (vm-reg-get state (vm-str-reg inst)))
         (stream (vm-get-stream state handle)))
    (write-string str stream)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-peek-char) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle))
         (char (peek-char nil stream nil +eof-value+)))
    (vm-reg-set state (vm-dst inst) char)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-unread-char) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (char (vm-reg-get state (vm-char-reg inst)))
         (stream (vm-get-stream state handle)))
    (unread-char char stream)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-file-position) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle))
         (position-reg (vm-position-reg inst)))
    (if position-reg
        ;; Set position
        (let ((new-pos (vm-reg-get state position-reg)))
          (file-position stream new-pos)
          (vm-reg-set state (vm-dst inst) new-pos)
          (values (1+ pc) nil nil))
        ;; Get position
        (let ((current-pos (file-position stream)))
          (vm-reg-set state (vm-dst inst) current-pos)
          (values (1+ pc) nil nil)))))

(defmethod execute-instruction ((inst vm-file-length) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle))
         (length (file-length stream)))
    (vm-reg-set state (vm-dst inst) length)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-eof-p) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-value inst)))
         (is-eof (if (eq value +eof-value+) 1 0)))
    (vm-reg-set state (vm-dst inst) is-eof)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-make-string-stream) state pc labels)
  (declare (ignore labels))
  (let* ((direction (vm-stream-direction inst))
         (initial-str (when (vm-initial-string inst)
                        (vm-reg-get state (vm-initial-string inst))))
         (handle (vm-allocate-file-handle state))
         (stream (if (eq direction :input)
                     (make-string-input-stream (or initial-str ""))
                     (make-string-output-stream))))
    (setf (gethash handle (vm-string-streams state)) stream)
    (vm-reg-set state (vm-dst inst) handle)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-get-string-from-stream) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (gethash handle (vm-string-streams state))))
     (if stream
        (progn
          (vm-reg-set state (vm-dst inst) (get-output-stream-string stream))
          (values (1+ pc) nil nil))
        (error "vm-get-string-from-stream: Handle ~A is not an output string stream" handle))))

;;; Stream Predicate Execution
;;;
;;; Values can be either direct CL stream objects or integer handles.
;;; We resolve handles to streams before applying predicates.

(defun %resolve-stream-val (state val)
  "Resolve VAL to a CL stream: if it's already a stream, return it;
if it's an integer handle, look it up in STATE's stream tables."
  (cond
    ((streamp val) val)
    ((integerp val)
     (cond
       ((eql val +stdin-handle+) (vm-standard-input state))
       ((eql val +stdout-handle+) (vm-standard-output state))
       ((typep state 'vm-io-state)
        (or (gethash val (vm-open-files state))
            (gethash val (vm-string-streams state))))
       (t nil)))
    (t nil)))

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
         (our-load-sym (or (find-symbol "OUR-LOAD" :cl-cc/compile)
                           (find-symbol "OUR-LOAD" :cl-cc)))
         (result (funcall (symbol-function our-load-sym) path)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; run-compiled-with-io and run-string-with-io are in io-runners.lisp (loads next).
