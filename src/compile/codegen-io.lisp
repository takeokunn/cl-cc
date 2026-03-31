(in-package :cl-cc)
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

;; read-char: (read-char &optional stream eof-error-p eof-value recursive-p) — FR-612
(define-phase2-handler "READ-CHAR" (args result-reg ctx)
  (let ((handle-reg (if args
                        (compile-ast (first args) ctx)
                        (let ((r (make-register ctx)))
                          (emit ctx (make-vm-const :dst r :value 0)) r))))
    (cond
      ;; 3+ args: eof-value substitution enabled
      ((>= (length args) 3)
       (compile-ast (second args) ctx)   ; eof-error-p — compile for side effects, discard
       (let ((eof-val-reg (compile-ast (third args) ctx))
             (raw-reg (make-register ctx)))
         (when (fourth args) (compile-ast (fourth args) ctx)) ; recursive-p — discard
         (emit ctx (make-vm-read-char :dst raw-reg :handle handle-reg))
         (%emit-eof-value-check raw-reg eof-val-reg result-reg ctx)))
      ;; 0-2 args: compile and discard extra args; return :eof sentinel on EOF
      (t
       (%compile-and-discard-extra-args args ctx)
       (emit ctx (make-vm-read-char :dst result-reg :handle handle-reg))
       result-reg))))

;; read-line: (read-line &optional stream eof-error-p eof-value recursive-p) — FR-612
(define-phase2-handler "READ-LINE" (args result-reg ctx)
  (let ((handle-reg (if args
                        (compile-ast (first args) ctx)
                        (let ((r (make-register ctx)))
                          (emit ctx (make-vm-const :dst r :value 0)) r))))
    (cond
      ;; 3+ args: eof-value substitution
      ((>= (length args) 3)
       (compile-ast (second args) ctx)   ; eof-error-p — discard
       (let ((eof-val-reg (compile-ast (third args) ctx))
             (raw-reg (make-register ctx)))
         (when (fourth args) (compile-ast (fourth args) ctx))
         (emit ctx (make-vm-read-line :dst raw-reg :handle handle-reg))
         (%emit-eof-value-check raw-reg eof-val-reg result-reg ctx)))
      (t
       (%compile-and-discard-extra-args args ctx)
       (emit ctx (make-vm-read-line :dst result-reg :handle handle-reg))
       result-reg))))

;; read: (read &optional stream eof-error-p eof-value recursive-p) — FR-612
(define-phase2-handler "READ" (args result-reg ctx)
  (let ((handle-reg (if args
                        (compile-ast (first args) ctx)
                        (let ((r (make-register ctx)))
                          (emit ctx (make-vm-const :dst r :value 0)) r))))
    (cond
      ;; 3+ args: eof-value substitution
      ((>= (length args) 3)
       (compile-ast (second args) ctx)   ; eof-error-p — discard
       (let ((eof-val-reg (compile-ast (third args) ctx))
             (raw-reg (make-register ctx)))
         (when (fourth args) (compile-ast (fourth args) ctx))
         (emit ctx (make-vm-read-sexp-inst :dst raw-reg :src handle-reg))
         (%emit-eof-value-check raw-reg eof-val-reg result-reg ctx)))
      (t
       (%compile-and-discard-extra-args args ctx)
       (emit ctx (make-vm-read-sexp-inst :dst result-reg :src handle-reg))
       result-reg))))

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
(define-phase2-handler "TERPRI" (args result-reg ctx)
  (if args
      (let ((handle-reg (compile-ast (first args) ctx))
            (char-reg   (make-register ctx)))
        (emit ctx (make-vm-const :dst char-reg :value #\Newline))
        (emit ctx (make-vm-write-char :handle handle-reg :char char-reg))
        (emit ctx (make-vm-const :dst result-reg :value nil))
        result-reg)
      (progn (emit ctx (make-vm-terpri-inst))
             (emit ctx (make-vm-const :dst result-reg :value nil))
             result-reg)))

(define-phase2-handler "FRESH-LINE" (args result-reg ctx)
  (if args
      (let ((handle-reg (compile-ast (first args) ctx))
            (char-reg   (make-register ctx)))
        (emit ctx (make-vm-const :dst char-reg :value #\Newline))
        (emit ctx (make-vm-write-char :handle handle-reg :char char-reg))
        (emit ctx (make-vm-const :dst result-reg :value nil))
        result-reg)
      (progn (emit ctx (make-vm-fresh-line-inst))
             (emit ctx (make-vm-const :dst result-reg :value nil))
             result-reg)))

;; FR-666: print / prin1 / princ — optional stream argument
;; 1-arg: existing vm-print-inst / vm-prin1 / vm-princ (default stdout)
;; 2-arg: write-to-string then vm-stream-write-string-inst to given stream
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
                                ;; :print prepends newline to the repr
                                (let ((nl-reg   (make-register ctx))
                                      (full-reg (make-register ctx)))
                                  (emit ctx (make-vm-write-to-string-inst :dst raw-str-reg :src obj-reg))
                                  (emit ctx (make-vm-const :dst nl-reg :value (string #\Newline)))
                                  (emit ctx (make-vm-concatenate :dst full-reg :str1 nl-reg :str2 raw-str-reg))
                                  full-reg)
                                ;; :princ/:prin1: just the write-to-string repr
                                (progn
                                  (emit ctx (make-vm-write-to-string-inst :dst raw-str-reg :src obj-reg))
                                  raw-str-reg))))
          (emit ctx (make-vm-stream-write-string-inst :stream-reg stream-reg :src out-str-reg))
          obj-reg)   ; ANSI: print/prin1/princ return the object
        ;; 1-arg form: existing instructions (return obj-reg)
        (progn (ecase mode
                 (:print  (emit ctx (make-vm-print-inst :src obj-reg)))
                 (:prin1  (emit ctx (make-vm-prin1 :src obj-reg)))
                 (:princ  (emit ctx (make-vm-princ :src obj-reg))))
               obj-reg))))

(define-phase2-handler "PRINT" (args result-reg ctx)
  (%emit-print-to-stream :print args result-reg ctx))
(define-phase2-handler "PRIN1" (args result-reg ctx)
  (%emit-print-to-stream :prin1 args result-reg ctx))
(define-phase2-handler "PRINC" (args result-reg ctx)
  (%emit-print-to-stream :princ args result-reg ctx))

;; Multi-dimensional aref: (aref arr i j ...) — Phase 1 handles (aref arr i) exactly
(define-phase2-handler "AREF" (args result-reg ctx)
  (when (>= (length args) 3)  ; array + 2+ indices
    (let ((arr-reg  (compile-ast (first args) ctx))
          (idx-regs (mapcar (lambda (a) (compile-ast a ctx)) (rest args))))
      (emit ctx (make-vm-aref-multi :dst result-reg
                                    :array-reg arr-reg
                                    :index-regs idx-regs))
      result-reg)))

;; FR-649: write-to-string — accept keyword args, use only the object
(define-phase2-handler "WRITE-TO-STRING" (args result-reg ctx)
  (when args
    (let ((obj-reg (compile-ast (first args) ctx)))
      ;; Evaluate keyword values for side effects, but ignore them.
      (loop for (key val) on (cdr args) by #'cddr
            when (and (typep key 'ast-var)
                      (keywordp (ast-var-name key)))
              do (compile-ast val ctx))
      (emit ctx (make-vm-write-to-string-inst :dst result-reg :src obj-reg))
      result-reg)))

;; write-string: optional stream arg
(define-phase2-handler "WRITE-STRING" (args result-reg ctx)
  (let ((str-reg (compile-ast (first args) ctx)))
    (if (>= (length args) 2)
        (let ((stream-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-stream-write-string-inst :stream-reg stream-reg :src str-reg))
          (emit ctx (make-vm-move :dst result-reg :src str-reg)))
        (emit ctx (make-vm-princ :src str-reg)))
    result-reg))

;; format: destination dispatch — nil (string), t (stdout), stream
(define-phase2-handler "FORMAT" (args result-reg ctx)
  (when (>= (length args) 2)
    (let* ((dest-arg        (first args))
           (fmt-reg         (compile-ast (second args) ctx))
           (format-arg-regs (mapcar (lambda (a) (compile-ast a ctx)) (cddr args)))
           (dest-sym (cond ((and (typep dest-arg 'ast-var)
                                  (member (ast-var-name dest-arg) '(nil t)))
                             (ast-var-name dest-arg))
                            ((typep dest-arg 'ast-quote)
                             (ast-quote-value dest-arg))
                            (t :stream))))
      (cond
        ((null dest-sym)
         ;; (format nil ...) → return the string
         (emit ctx (make-vm-format-inst :dst result-reg :fmt fmt-reg
                                        :arg-regs format-arg-regs)))
        ((eq dest-sym t)
         ;; (format t ...) → print to stdout, return nil
         (let ((str-reg (make-register ctx)))
           (emit ctx (make-vm-format-inst :dst str-reg :fmt fmt-reg
                                          :arg-regs format-arg-regs))
           (emit ctx (make-vm-princ :src str-reg))
           (emit ctx (make-vm-const :dst result-reg :value nil))))
        (t
         ;; (format stream ...) → write to stream, return nil
         (let ((str-reg    (make-register ctx))
               (stream-reg (compile-ast dest-arg ctx)))
           (emit ctx (make-vm-format-inst :dst str-reg :fmt fmt-reg
                                          :arg-regs format-arg-regs))
           (emit ctx (make-vm-stream-write-string-inst :stream-reg stream-reg :src str-reg))
           (emit ctx (make-vm-const :dst result-reg :value nil)))))
       result-reg)))

;; open: parse :direction keyword arg
(define-phase2-handler "OPEN" (args result-reg ctx)
  ;; Parse :direction, :if-exists, :if-does-not-exist from keyword args.
  ;; Other keyword args (:element-type, :external-format) are compiled but ignored at runtime.
  (let* ((path-reg  (compile-ast (first args) ctx))
         (direction :input)
         (if-exists :supersede)
         (if-not-exists nil))
    (loop for (key val) on (cdr args) by #'cddr
          when (and (typep key 'ast-var) (keywordp (ast-var-name key)))
            do (let ((k (ast-var-name key)))
                 (cond
                   ((eq k :direction)
                    (setf direction (if (typep val 'ast-var)
                                        (ast-var-name val) :input)))
                   ((eq k :if-exists)
                    (setf if-exists (if (typep val 'ast-var)
                                        (ast-var-name val) :supersede)))
                   ((eq k :if-does-not-exist)
                    (setf if-not-exists (if (typep val 'ast-var)
                                            (ast-var-name val) nil)))
                   ;; Compile but discard :element-type, :external-format args
                   (t (compile-ast val ctx)))))
    ;; if-not-exists default: :create for output, :error for input (computed in execute-instruction)
    (emit ctx (make-vm-open-file :dst result-reg :path path-reg :direction direction
                                 :if-exists if-exists :if-not-exists if-not-exists))
    result-reg))

;; close: (close stream &key abort) — :abort accepted but ignored (FR-589)
(define-phase2-handler "CLOSE" (args result-reg ctx)
  (when args
    (let ((handle-reg (compile-ast (first args) ctx)))
      ;; Compile and discard :abort keyword arg value
      (loop for (key val) on (cdr args) by #'cddr
            when (and (typep key 'ast-var) (keywordp (ast-var-name key)))
              do (compile-ast val ctx))
      (emit ctx (make-vm-close-file :handle handle-reg))
      ;; close returns t per ANSI CL
      (emit ctx (make-vm-const :dst result-reg :value t))
      result-reg)))

;; concatenate: only (concatenate 'string a b) → vm-concatenate
(define-phase2-handler "CONCATENATE" (args result-reg ctx)
  (when (and (>= (length args) 3)
             (typep (first args) 'ast-quote)
             (string= (symbol-name (ast-quote-value (first args))) "STRING"))
    (let ((str1-reg (compile-ast (second args) ctx))
          (str2-reg (compile-ast (third args)  ctx)))
      (emit ctx (make-vm-concatenate :dst result-reg :str1 str1-reg :str2 str2-reg))
      result-reg)))
