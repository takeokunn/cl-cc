(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Phase 2: Stream / Reader / Printer Handlers
;;;
;;; Extracted from codegen-phase2.lisp to keep the stream-oriented handlers
;;; cohesive and separate from the remaining array/string builtins.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; Helper: emit the eof-value substitution pattern.
;;; After emitting the raw read into RAW-REG, if eof-value is supplied (3+ args),
;;; check for :eof sentinel and substitute eof-value. Returns result-reg.
(defun %emit-eof-value-check (raw-reg eof-val-reg result-reg ctx)
  "If raw-reg == :eof, write eof-val-reg into result-reg; else copy raw-reg."
  (let ((sentinel-reg (make-register ctx))
        (eq-reg (make-register ctx))
        (not-eof-label (make-label ctx "not_eof"))
        (end-label (make-label ctx "eof_end")))
    (emit ctx (make-vm-const :dst sentinel-reg :value :eof))
    (emit ctx (make-vm-eq :dst eq-reg :lhs raw-reg :rhs sentinel-reg))
    (emit ctx (make-vm-jump-zero :reg eq-reg :label not-eof-label))
    ;; EOF branch: use eof-value
    (emit ctx (make-vm-move :dst result-reg :src eof-val-reg))
    (emit ctx (make-vm-jump :label end-label))
    (emit ctx (make-vm-label :name not-eof-label))
    ;; Non-EOF branch: use the read value
    (emit ctx (make-vm-move :dst result-reg :src raw-reg))
    (emit ctx (make-vm-label :name end-label))
    result-reg))

(defun %compile-and-discard-extra-args (args ctx)
  "Compile trailing ARGS for side effects only."
  (dolist (extra (cdr args))
    (compile-ast extra ctx)))

(defun %compile-stream-read (args result-reg ctx emit-raw-fn)
  "Common logic for stream reader handlers (read-char, read-line, read).
   Acquires a handle register (arg 0 or default stdin handle 0), then:
     3+ args → eof-value substitution path (args: stream eof-error-p eof-value [recursive-p])
     0-2 args → simple path, returns :eof sentinel on EOF.
   EMIT-RAW-FN is called as (emit-raw-fn dst-reg handle-reg ctx) to emit the VM read."
  (let ((handle-reg (if args
                        (compile-ast (first args) ctx)
                        (let ((r (make-register ctx)))
                          (emit ctx (make-vm-const :dst r :value 0)) r))))
    (if (>= (length args) 3)
        (progn
          (compile-ast (second args) ctx)   ; eof-error-p — compile for side effects, discard
          (let ((eof-val-reg (compile-ast (third args) ctx))
                (raw-reg (make-register ctx)))
            (when (fourth args) (compile-ast (fourth args) ctx)) ; recursive-p — discard
            (funcall emit-raw-fn raw-reg handle-reg ctx)
            (%emit-eof-value-check raw-reg eof-val-reg result-reg ctx)))
        (progn
          (%compile-and-discard-extra-args args ctx)
          (funcall emit-raw-fn result-reg handle-reg ctx)
          result-reg))))

;; read-char / read-line / read — FR-612
;; (fn &optional stream eof-error-p eof-value recursive-p)
;; All three share identical argument structure; only the emitted VM instruction differs.

(define-phase2-handler "READ-CHAR" (args result-reg ctx)
  (%compile-stream-read args result-reg ctx
    (lambda (dst handle ctx)
      (emit ctx (make-vm-read-char :dst dst :handle handle)))))

(define-phase2-handler "READ-LINE" (args result-reg ctx)
  (%compile-stream-read args result-reg ctx
    (lambda (dst handle ctx)
      (emit ctx (make-vm-read-line :dst dst :handle handle)))))

(define-phase2-handler "READ" (args result-reg ctx)
  (%compile-stream-read args result-reg ctx
    (lambda (dst handle ctx)
      (emit ctx (make-vm-read-sexp-inst :dst dst :src handle)))))

;; peek-char: (peek-char handle) or (peek-char nil handle)
(define-phase2-handler "PEEK-CHAR" (args result-reg ctx)
  (let ((handle-reg (if (>= (length args) 2)
                        (compile-ast (second args) ctx)
                        (compile-ast (first args) ctx))))
    (emit ctx (make-vm-peek-char :dst result-reg :handle handle-reg))
    result-reg))

;; unread-char: (unread-char char) — 1-arg form defaults to *standard-input* (handle 0)
;; Phase 1 handles the 2-arg form; this handler fires only for 1-arg.
(define-phase2-handler "UNREAD-CHAR" (args result-reg ctx)
  (when (= (length args) 1)
    (let* ((char-reg (compile-ast (first args) ctx))
           (handle-reg (make-register ctx)))
      (emit ctx (make-vm-const :dst handle-reg :value 0))
      (emit ctx (make-vm-unread-char :char char-reg :handle handle-reg))
      (emit ctx (make-vm-const :dst result-reg :value nil))
      result-reg)))

;; listen: (listen) — 0-arg form defaults to *standard-input* (handle 0)
;; Phase 1 handles the 1-arg form; this handler fires only for 0-arg.
(define-phase2-handler "LISTEN" (args result-reg ctx)
  (when (null args)
    (let ((handle-reg (make-register ctx)))
      (emit ctx (make-vm-const :dst handle-reg :value 0))
      (emit ctx (make-vm-listen-inst :dst result-reg :handle handle-reg))
      result-reg)))

;; read-byte: (read-byte stream &optional eof-error-p eof-value) — FR-672b
;; Phase 1 handles (read-byte handle) exactly; this fires for 3+ args (with eof-value).
(define-phase2-handler "READ-BYTE" (args result-reg ctx)
  (when (>= (length args) 3)
    (let ((handle-reg (compile-ast (first args) ctx)))
      (compile-ast (second args) ctx)   ; eof-error-p — discard
      (let ((eof-val-reg (compile-ast (third args) ctx))
            (raw-reg (make-register ctx)))
        (emit ctx (make-vm-read-byte :dst raw-reg :handle handle-reg))
        (%emit-eof-value-check raw-reg eof-val-reg result-reg ctx)))))

;; make-string-input-stream
(define-phase2-handler "MAKE-STRING-INPUT-STREAM" (args result-reg ctx)
  (let ((str-reg (compile-ast (first args) ctx)))
    (emit ctx (make-vm-make-string-stream :dst result-reg :direction :input
                                          :initial-string str-reg))
    result-reg))

;; FR-673: terpri / fresh-line — optional stream argument
;; 0-arg: existing vm-terpri-inst / vm-fresh-line-inst (default stdout)
;; 1-arg: emit vm-write-char #\Newline to given stream

(defun %compile-newline-handler (args result-reg ctx default-inst-thunk)
  "Common logic for TERPRI and FRESH-LINE.
   1-arg: write #\\Newline to given stream.
   0-arg: emit DEFAULT-INST-THUNK () → default-stdout instruction."
  (if args
      (let ((handle-reg (compile-ast (first args) ctx))
            (char-reg   (make-register ctx)))
        (emit ctx (make-vm-const :dst char-reg :value #\Newline))
        (emit ctx (make-vm-write-char :handle handle-reg :char char-reg))
        (emit ctx (make-vm-const :dst result-reg :value nil))
        result-reg)
      (progn
        (emit ctx (funcall default-inst-thunk))
        (emit ctx (make-vm-const :dst result-reg :value nil))
        result-reg)))

(define-phase2-handler "TERPRI"    (args result-reg ctx)
  (%compile-newline-handler args result-reg ctx #'make-vm-terpri-inst))

(define-phase2-handler "FRESH-LINE" (args result-reg ctx)
  (%compile-newline-handler args result-reg ctx #'make-vm-fresh-line-inst))

;; FR-666: print / prin1 / princ — optional stream argument
;; 1-arg: existing vm-print-inst / vm-prin1 / vm-princ (default stdout)
;; 2-arg: write-to-string then vm-stream-write-string-inst to given stream

(defparameter *print-mode-stdout-constructors*
  '((:print  . make-vm-print-inst)
    (:prin1  . make-vm-prin1)
    (:princ  . make-vm-princ))
  "Alist mapping print mode keyword to its 1-arg (stdout) VM instruction constructor.")

(defun %emit-print-to-stream (mode args result-reg ctx)
  "Emit print/prin1/princ to an optional stream.
MODE is :print, :prin1, or :princ.  ARGS is (obj) or (obj stream).
Returns register holding the printed object (ANSI: print returns its argument)."
  (declare (ignore result-reg))
  (let ((obj-reg (compile-ast (first args) ctx)))
    (if (>= (length args) 2)
        ;; 2-arg form: write-to-string then stream-write-string
        (let* ((raw-str-reg (make-register ctx))
               (stream-reg  (compile-ast (second args) ctx))
               (out-str-reg (if (eq mode :print)
                                ;; :print prepends newline before the repr
                                (let ((nl-reg   (make-register ctx))
                                      (full-reg (make-register ctx)))
                                  (emit ctx (make-vm-write-to-string-inst :dst raw-str-reg :src obj-reg))
                                  (emit ctx (make-vm-const :dst nl-reg :value (string #\Newline)))
                                  (emit ctx (make-vm-concatenate :dst full-reg :str1 nl-reg :str2 raw-str-reg))
                                  full-reg)
                                ;; :princ/:prin1: bare write-to-string repr
                                (progn
                                  (emit ctx (make-vm-write-to-string-inst :dst raw-str-reg :src obj-reg))
                                  raw-str-reg))))
          (emit ctx (make-vm-stream-write-string-inst :stream-reg stream-reg :src out-str-reg))
          obj-reg)   ; ANSI: print/prin1/princ return the object
        ;; 1-arg form: direct constructor dispatch; avoid funcall+keyword plist
        ;; during bootstrap self-compilation.
        (progn
          (if (eq mode :print)
              (emit ctx (make-vm-print-inst :src obj-reg))
              (if (eq mode :prin1)
                  (emit ctx (make-vm-prin1 :src obj-reg))
                  (emit ctx (make-vm-princ :src obj-reg))))
          obj-reg))))

(define-phase2-handler "PRINT" (args result-reg ctx)
  (%emit-print-to-stream :print args result-reg ctx))
(define-phase2-handler "PRIN1" (args result-reg ctx)
  (%emit-print-to-stream :prin1 args result-reg ctx))
(define-phase2-handler "PRINC" (args result-reg ctx)
  (%emit-print-to-stream :princ args result-reg ctx))

;; AREF (multi-dim), WRITE-TO-STRING, WRITE-STRING, FORMAT, OPEN, CLOSE,
;; and CONCATENATE handlers are in codegen-io-ext.lisp (loaded next).
