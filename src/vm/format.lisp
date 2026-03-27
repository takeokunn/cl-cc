(in-package :cl-cc)

;;; VM Formatted Output and Reader Instructions
;;;
;;; This file provides VM instructions for formatted output (princ, prin1,
;;; print, terpri, format, write-to-string) and reader operations
;;; (read-from-string, read-sexp) using cl-cc's own lexer/parser.
;;;

;;; ─── Print/Format Instructions ───────────────────────────────────────────

(define-vm-instruction vm-princ (vm-instruction)
  "Print object readably (no escaping) to *standard-output*."
  (src nil :reader vm-src)
  (:sexp-tag :princ)
  (:sexp-slots src))

(define-vm-instruction vm-prin1 (vm-instruction)
  "Print object with escaping to *standard-output*."
  (src nil :reader vm-src)
  (:sexp-tag :prin1)
  (:sexp-slots src))

(define-vm-instruction vm-print-inst (vm-instruction)
  "Print object with newline prefix and space suffix to *standard-output*."
  (src nil :reader vm-src)
  (:sexp-tag :print)
  (:sexp-slots src))

(define-vm-instruction vm-terpri-inst (vm-instruction)
  "Output a newline to *standard-output*."
  (:sexp-tag :terpri))

(define-vm-instruction vm-fresh-line-inst (vm-instruction)
  "Output a newline if not at start of line to *standard-output*."
  (:sexp-tag :fresh-line))

(define-vm-instruction vm-write-to-string-inst (vm-instruction)
  "Convert object to its printed representation as a string."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :write-to-string)
  (:sexp-slots dst src))

;; Custom sexp: uses list* with variadic arg-regs
(define-vm-instruction vm-format-inst (vm-instruction)
  "Format string with arguments. Result string stored in DST."
  (dst nil :reader vm-dst)
  (fmt nil :reader vm-fmt)
  (arg-regs nil :reader vm-arg-regs))

(defmethod instruction->sexp ((inst vm-format-inst))
  (list* :format (vm-dst inst) (vm-fmt inst) (vm-arg-regs inst)))

(setf (gethash :format *instruction-constructors*)
      (lambda (sexp)
        (make-vm-format-inst :dst (second sexp)
                             :fmt (third sexp)
                             :arg-regs (cdddr sexp))))

;;; ─── String Stream Instructions ──────────────────────────────────────────

(define-vm-instruction vm-make-string-output-stream-inst (vm-instruction)
  "Create a string output stream, store in DST."
  (dst nil :reader vm-dst)
  (:sexp-tag :make-string-output-stream)
  (:sexp-slots dst))

(define-vm-instruction vm-get-output-stream-string-inst (vm-instruction)
  "Extract accumulated string from string output stream in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :get-output-stream-string)
  (:sexp-slots dst src))

(define-vm-instruction vm-stream-write-string-inst (vm-instruction)
  "Write string in SRC to stream in STREAM-REG."
  (stream-reg nil :reader vm-stream-reg)
  (src nil :reader vm-src)
  (:sexp-tag :stream-write-string)
  (:sexp-slots stream-reg src))

;;; ─── Execute print/format instructions ──────────────────────────────────

(defmethod execute-instruction ((inst vm-princ) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (princ val (vm-output-stream state))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-prin1) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (prin1 val (vm-output-stream state))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-print-inst) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (print val (vm-output-stream state))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-terpri-inst) state pc labels)
  (declare (ignore labels))
  (terpri (vm-output-stream state))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-fresh-line-inst) state pc labels)
  (declare (ignore labels))
  (fresh-line (vm-output-stream state))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-write-to-string-inst) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (write-to-string val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-format-inst) state pc labels)
  (declare (ignore labels))
  (let* ((fmt-str (vm-reg-get state (vm-fmt inst)))
         (arg-vals (mapcar (lambda (r) (vm-reg-get state r)) (vm-arg-regs inst)))
         (result (apply #'format nil fmt-str arg-vals)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-make-string-output-stream-inst) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (make-string-output-stream))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-get-output-stream-string-inst) state pc labels)
  (declare (ignore labels))
  (let ((stream (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (get-output-stream-string stream))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-stream-write-string-inst) state pc labels)
  (declare (ignore labels))
  (let* ((stream-val (vm-reg-get state (vm-stream-reg inst)))
         (stream (if (streamp stream-val)
                     stream-val
                     (vm-get-stream state stream-val)))
         (str (vm-reg-get state (vm-src inst))))
    (write-string str stream)
    (values (1+ pc) nil nil)))

;;; ─── Reader Instructions (use cl-cc's own lexer/parser) ─────────────────

(define-vm-instruction vm-read-from-string-inst (vm-instruction)
  "Read an S-expression from a string using cl-cc's own parser."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :read-from-string)
  (:sexp-slots dst src))

(define-vm-instruction vm-read-sexp-inst (vm-instruction)
  "Read an S-expression from a stream handle using cl-cc's own parser."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :read-sexp)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-read-from-string-inst) state pc labels)
  (declare (ignore labels))
  (let* ((str (vm-reg-get state (vm-src inst)))
         (forms (parse-all-forms str))
         (value (if forms (first forms) nil)))
    (vm-reg-set state (vm-dst inst) value)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-read-sexp-inst) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-src inst)))
         (stream (vm-get-stream state handle))
         (line (read-line stream nil nil))
         (value (when line (first (parse-all-forms line)))))
    (vm-reg-set state (vm-dst inst) value)
    (values (1+ pc) nil nil)))
