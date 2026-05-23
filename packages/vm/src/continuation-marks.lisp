;;;; packages/vm/src/continuation-marks.lisp — FR-577 Continuation Marks
;;;;
;;;; Two new VM instructions: VM-WITH-CONTINUATION-MARK and
;;;; VM-CURRENT-CONTINUATION-MARKS.  Continuation marks are key-value metadata
;;;; stored on VM call frames as a list of (key . value) conses.  Marks follow
;;;; the current continuation: when a tail call occurs the mark list is replaced
;;;; (not appended) so the space is O(1).
;;;;
;;;; A mark stored on a frame is visible to the current function and to any
;;;; function it calls (until/unless the callee replaces it with a tail-call
;;;; mark of the same key).  VM-CURRENT-CONTINUATION-MARKS walks the mark chain
;;;; from the current frame upward, collecting every entry whose key matches.
;;;;
;;;; Typically loaded after vm-instructions.lisp (which defines vm-call/vm-ret)
;;;; and vm-dispatch.lisp (which defines vm-push-call-frame).

(in-package :cl-cc/vm)

;;; ── Frame mark accessors ─────────────────────────────────────────────────

(defun vm-frame-marks (frame)
  "Return the continuation-marks list stored in FRAME, or nil."
  (fifth frame))

(defun (setf vm-frame-marks) (marks frame)
  "Set FRAME's continuation-marks list to MARKS."
  (setf (fifth frame) marks))

(defun vm-frame-push-mark (frame key value)
  "Push a (KEY . VALUE) cons onto FRAME's continuation marks."
  (push (cons key value) (fifth frame))
  value)

;;; ── VM instruction: vm-with-continuation-mark ────────────────────────────

(define-vm-instruction vm-with-continuation-mark (vm-instruction)
  "Push a continuation mark (KEY-REG . VALUE-REG) onto the current call frame.
The mark is visible to the current function and to any called function.
Tail calls replace the mark list, so the space cost is O(1)."
  (key-reg nil)
  (value-reg nil)
  (:sexp-tag :with-continuation-mark)
  (:sexp-slots key-reg value-reg))

(defmethod execute-instruction ((inst vm-with-continuation-mark) state pc labels)
  (declare (ignore labels))
  (let* ((key   (vm-reg-get state (vm-with-continuation-mark-key-reg inst)))
         (value (vm-reg-get state (vm-with-continuation-mark-value-reg inst)))
         (frame (car (vm-call-stack state))))
    (when frame
      (vm-frame-push-mark frame key value))
    (values (1+ pc) nil nil)))

;;; ── VM instruction: vm-current-continuation-marks ────────────────────────

(define-vm-instruction vm-current-continuation-marks (vm-instruction)
  "Search the continuation mark chain from the current frame upward,
collecting every mark whose key matches KEY-REG into DEST-REG as a list.
The list is in mark order (most recent first)."
  (key-reg nil)
  (dest-reg nil)
  (:sexp-tag :current-continuation-marks)
  (:sexp-slots key-reg dest-reg))

(defmethod execute-instruction ((inst vm-current-continuation-marks) state pc labels)
  (declare (ignore labels))
  (let* ((search-key (vm-reg-get state (vm-current-continuation-marks-key-reg inst)))
         (result nil))
    ;; Walk the call stack from top (current) to bottom
    (dolist (frame (vm-call-stack state))
      (dolist (mark (vm-frame-marks frame))
        (when (equal (car mark) search-key)
          (push (cdr mark) result))))
    (vm-reg-set state (vm-current-continuation-marks-dest-reg inst) (nreverse result))
    (values (1+ pc) nil nil)))

;;; ── CL-level macro ──────────────────────────────────────────────────────
;;;
;;; with-continuation-mark is a CL-CC extension that the compiler recognizes.
;;; The CPS/codegen transform emits vm-with-continuation-mark instructions.
;;; At the CL expander level, this is just a passthrough that ensures the
;;; form is recognized as a special construct by the compiler pipeline.

(defmacro with-continuation-mark ((key value) &body body)
  "Execute BODY with KEY-VALUE mark on the current continuation.
The mark is visible to called functions and is automatically cleaned
up when the current call frame returns.
Tail calls from BODY replace (not append) the mark list."
  (declare (ignore key value))
  `(progn ,@body))
