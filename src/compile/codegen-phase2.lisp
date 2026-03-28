(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Phase 2: AST-Introspecting Builtin Handlers
;;;
;;; Builtins in this file cannot be handled by the data-driven Phase 1
;;; *builtin-registry* because they need to inspect AST node types at
;;; compile time: quote guards, keyword arg parsing, variadic cons-chains,
;;; multi-destination paths (e.g. FORMAT's nil/t/stream branches).
;;;
;;; Design: *phase2-builtin-handlers* is a Prolog-style clause database.
;;; Each (define-phase2-handler "NAME" ...) registers one clause.
;;; Returning result-reg signals success; nil signals fallthrough to the
;;; generic function-call path in codegen.lisp.
;;;
;;; Load order: after codegen-functions, before codegen.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar *phase2-builtin-handlers* (make-hash-table :test 'equal)
  "Maps uppercase builtin name strings to Phase 2 handler functions.
Handler lambda-list: (args result-reg ctx) → result-reg-or-nil.")

(defmacro define-phase2-handler (name (args result-reg ctx) &body body)
  "Register a Phase 2 handler for the builtin named NAME (uppercase string)."
  `(setf (gethash ,name *phase2-builtin-handlers*)
         (lambda (,args ,result-reg ,ctx) ,@body)))

;;; ── Handler registrations ──────────────────────────────────────────────────

;; make-hash-table: extract :test keyword and convert #'fn → 'fn
(define-phase2-handler "MAKE-HASH-TABLE" (args result-reg ctx)
  (let ((test-reg nil))
    (when (>= (length args) 2)
      (let ((kw-arg (first args)))
        (when (and (typep kw-arg 'ast-var) (eq (ast-var-name kw-arg) :test))
          (let* ((test-arg (second args))
                 (test-sym (cond
                              ((and (typep test-arg 'ast-quote)
                                    (symbolp (ast-quote-value test-arg)))
                               (ast-quote-value test-arg))
                              ((and (typep test-arg 'ast-var)
                                    (member (ast-var-name test-arg) '(eq eql equal equalp)))
                               (ast-var-name test-arg))
                              ((and (typep test-arg 'ast-function)
                                    (symbolp (ast-function-name test-arg))
                                    (member (ast-function-name test-arg) '(eq eql equal equalp)))
                               (ast-function-name test-arg))
                              (t nil))))
            (when test-sym
              (setf test-reg (make-register ctx))
              (emit ctx (make-vm-const :dst test-reg :value test-sym)))))))
    (emit ctx (make-vm-make-hash-table :dst result-reg :test test-reg))
    result-reg))

;; gethash: 2-arg (key table) or 3-arg (key table default)
(define-phase2-handler "GETHASH" (args result-reg ctx)
  (let ((key-reg     (compile-ast (first args)  ctx))
        (table-reg   (compile-ast (second args) ctx))
        (default-reg (when (third args) (compile-ast (third args) ctx))))
    (emit ctx (make-vm-gethash :dst result-reg :found-dst nil
                               :key key-reg :table table-reg :default default-reg))
    result-reg))

;; maphash: inline loop — keys snapshot → iterate with fn call
(define-phase2-handler "MAPHASH" (args result-reg ctx)
  (let* ((fn-reg     (compile-ast (first args)  ctx))
         (table-reg  (compile-ast (second args) ctx))
         (keys-reg   (make-register ctx))
         (loop-start (make-label ctx "MAPHASH_START"))
         (loop-end   (make-label ctx "MAPHASH_END"))
         (key-reg    (make-register ctx))
         (val-reg    (make-register ctx)))
    (emit ctx (make-vm-hash-table-keys :dst keys-reg :table table-reg))
    (emit ctx (make-vm-label :name loop-start))
    (emit ctx (make-vm-jump-zero :reg keys-reg :label loop-end))
    (emit ctx (make-vm-car :dst key-reg :src keys-reg))
    (emit ctx (make-vm-gethash :dst val-reg :key key-reg :table table-reg))
    (let ((call-dst (make-register ctx)))
      (emit ctx (make-vm-call :dst call-dst :func fn-reg :args (list key-reg val-reg))))
    (emit ctx (make-vm-cdr :dst keys-reg :src keys-reg))
    (emit ctx (make-vm-jump :label loop-start))
    (emit ctx (make-vm-label :name loop-end))
    (emit ctx (make-vm-const :dst result-reg :value nil))
    result-reg))

;; make-array: fixed-size array
(define-phase2-handler "MAKE-ARRAY" (args result-reg ctx)
  (let ((size-reg (compile-ast (first args) ctx)))
    (emit ctx (make-vm-make-array :dst result-reg :size-reg size-reg
                                  :fill-pointer nil :adjustable nil))
    result-reg))

;; make-adjustable-vector: array with fill-pointer
(define-phase2-handler "MAKE-ADJUSTABLE-VECTOR" (args result-reg ctx)
  (let ((size-reg (compile-ast (first args) ctx)))
    (emit ctx (make-vm-make-array :dst result-reg :size-reg size-reg
                                  :fill-pointer t :adjustable t))
    result-reg))

;; array-row-major-index: variadic subscripts → cons-chain
(define-phase2-handler "ARRAY-ROW-MAJOR-INDEX" (args result-reg ctx)
  (when (>= (length args) 1)
    (let ((arr-reg  (compile-ast (first args) ctx))
          (subs-reg (make-register ctx)))
      (emit ctx (make-vm-const :dst subs-reg :value nil))
      (dolist (sub (reverse (rest args)))
        (let ((sub-reg (compile-ast sub ctx))
              (new-reg (make-register ctx)))
          (emit ctx (make-vm-cons :dst new-reg :car-src sub-reg :cdr-src subs-reg))
          (setf subs-reg new-reg)))
      (emit ctx (make-vm-array-row-major-index :dst result-reg :arr arr-reg :subs subs-reg))
      result-reg)))

;; encode-universal-time: 6 or 7 args → cons-list → single instruction
(define-phase2-handler "ENCODE-UNIVERSAL-TIME" (args result-reg ctx)
  (when (and (>= (length args) 6) (<= (length args) 7))
    (let ((arg-regs (mapcar (lambda (a) (compile-ast a ctx)) args))
          (list-reg (make-register ctx)))
      (emit ctx (make-vm-const :dst list-reg :value nil))
      (dolist (r (reverse arg-regs))
        (let ((new-reg (make-register ctx)))
          (emit ctx (make-vm-cons :dst new-reg :car-src r :cdr-src list-reg))
          (setf list-reg new-reg)))
      (emit ctx (make-vm-encode-universal-time :dst result-reg :args-reg list-reg))
      result-reg)))

;; make-string: optional :initial-element and/or :element-type keywords
;; :element-type is accepted but ignored (we only support character strings)
(define-phase2-handler "MAKE-STRING" (args result-reg ctx)
  (let* ((size-reg (compile-ast (first args) ctx))
         ;; Scan keyword args for :initial-element; skip :element-type value
         (init-char-ast
          (loop for (k v) on (rest args) by #'cddr
                when (and (typep k 'ast-var)
                          (eq (ast-var-name k) :initial-element))
                return v)))
    (if init-char-ast
        (let ((char-reg (compile-ast init-char-ast ctx)))
          (emit ctx (make-vm-make-string :dst result-reg :src size-reg :char char-reg)))
        (emit ctx (make-vm-make-string :dst result-reg :src size-reg)))
    result-reg))

;; FR-604: float 2-arg form — (float number prototype) drops prototype (all nums are double)
(define-phase2-handler "FLOAT" (args result-reg ctx)
  (when (= (length args) 2)
    ;; Evaluate and discard prototype (for side effects); convert number to float
    (compile-ast (second args) ctx)
    (let ((num-reg (compile-ast (first args) ctx)))
      (emit ctx (make-vm-float-inst :dst result-reg :src num-reg))
      result-reg)))

;; typep: fast path when type is a quoted symbol
(define-phase2-handler "TYPEP" (args result-reg ctx)
  (when (and (= (length args) 2) (typep (second args) 'ast-quote))
    (let ((val-reg  (compile-ast (first args) ctx))
          (type-sym (ast-quote-value (second args))))
      (emit ctx (make-vm-typep :dst result-reg :src val-reg :type-name type-sym))
      result-reg)))

;; CLOS slot predicates: (pred obj 'slot-name) with quoted slot name
(dolist (entry '(("SLOT-BOUNDP"    . make-vm-slot-boundp)
                 ("SLOT-EXISTS-P"  . make-vm-slot-exists-p)
                 ("SLOT-MAKUNBOUND". make-vm-slot-makunbound)))
  (let ((name-str (car entry)) (inst-ctor (cdr entry)))
    (setf (gethash name-str *phase2-builtin-handlers*)
          (lambda (args result-reg ctx)
            (when (and (= (length args) 2) (typep (second args) 'ast-quote))
              (let ((obj-reg  (compile-ast (first args) ctx))
                    (slot-sym (ast-quote-value (second args))))
                (emit ctx (funcall (symbol-function inst-ctor)
                                   :dst result-reg :obj-reg obj-reg :slot-name-sym slot-sym))
                result-reg))))))

;; call-next-method: 0 args or variadic args → cons-list
(define-phase2-handler "CALL-NEXT-METHOD" (args result-reg ctx)
  (let ((args-reg (when args
                    (let* ((arg-regs (mapcar (lambda (a) (compile-ast a ctx)) args))
                           (list-reg (make-register ctx)))
                      (emit ctx (make-vm-const :dst list-reg :value nil))
                      (dolist (r (reverse arg-regs))
                        (let ((new-reg (make-register ctx)))
                          (emit ctx (make-vm-cons :dst new-reg :car-src r :cdr-src list-reg))
                          (setf list-reg new-reg)))
                      list-reg))))
    (emit ctx (make-vm-call-next-method :dst result-reg :args-reg args-reg))
    result-reg))

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

;; Helper: emit the eof-value substitution pattern.
;; After emitting the raw read into RAW-REG, if eof-value is supplied (3+ args),
;; check for :eof sentinel and substitute eof-value. Returns result-reg.
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
       (dolist (extra (cdr args)) (compile-ast extra ctx))
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
       (dolist (extra (cdr args)) (compile-ast extra ctx))
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
       (dolist (extra (cdr args)) (compile-ast extra ctx))
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
      (emit ctx (make-vm-write-to-string-inst :dst result-reg :src obj-reg))
      result-reg)))

;; FR-637: string comparisons with :start1/:end1/:start2/:end2 keyword args
;; When args > 2 (keywords present), Phase 1 arity check rejects them and falls here.
;; Strategy: use vm-subseq to extract substrings, then emit the 2-arg comparison.

(defun %compile-maybe-subseq (str-ast start-ast end-ast ctx)
  "Compile str-ast, wrapping in vm-subseq when start-ast or end-ast is non-nil."
  (let ((str-reg (compile-ast str-ast ctx)))
    (if (or start-ast end-ast)
        (let* ((start-reg (if start-ast
                              (compile-ast start-ast ctx)
                              (let ((r (make-register ctx)))
                                (emit ctx (make-vm-const :dst r :value 0))
                                r)))
               (end-reg (when end-ast (compile-ast end-ast ctx)))
               (sub-reg (make-register ctx)))
          (emit ctx (make-vm-subseq :dst sub-reg :string str-reg
                                    :start start-reg :end end-reg))
          sub-reg)
        str-reg)))

(defun %parse-string-cmp-kwargs (args)
  "Return (values str1 str2 start1 end1 start2 end2) from an args list with keywords."
  (let (start1 end1 start2 end2)
    (loop for (key val) on (cddr args) by #'cddr
          when (and (typep key 'ast-var) (keywordp (ast-var-name key)))
            do (case (ast-var-name key)
                 (:start1 (setf start1 val))
                 (:end1   (setf end1   val))
                 (:start2 (setf start2 val))
                 (:end2   (setf end2   val))))
    (values (first args) (second args) start1 end1 start2 end2)))

(defmacro define-string-cmp-handler (upper-name ctor)
  `(define-phase2-handler ,upper-name (args result-reg ctx)
     (when (>= (length args) 2)
       (multiple-value-bind (str1 str2 start1 end1 start2 end2)
           (%parse-string-cmp-kwargs args)
         (let ((r1 (%compile-maybe-subseq str1 start1 end1 ctx))
               (r2 (%compile-maybe-subseq str2 start2 end2 ctx)))
           (emit ctx (,ctor :dst result-reg :str1 r1 :str2 r2))
           result-reg)))))

(define-string-cmp-handler "STRING="       make-vm-string=)
(define-string-cmp-handler "STRING<"       make-vm-string<)
(define-string-cmp-handler "STRING>"       make-vm-string>)
(define-string-cmp-handler "STRING<="      make-vm-string<=)
(define-string-cmp-handler "STRING>="      make-vm-string>=)
(define-string-cmp-handler "STRING-EQUAL"  make-vm-string-equal)

;; FR-627: string-upcase/downcase/capitalize with :start/:end keyword args
;; When keywords present, Phase 1 rejects (arity mismatch) and falls here.
;; ANSI semantics: only the substring [start, end) is changed; prefix and suffix stay intact.
;; Strategy: concatenate prefix + case(slice) + suffix.
(defun %parse-string-case-kwargs (args)
  "Return (values str-ast start-ast end-ast) from args with keywords."
  (let (start end)
    (loop for (key val) on (cdr args) by #'cddr
          when (and (typep key 'ast-var) (keywordp (ast-var-name key)))
            do (case (ast-var-name key)
                 (:start (setf start val))
                 (:end   (setf end   val))))
    (values (first args) start end)))

(defmacro define-string-case-handler (upper-name ctor)
  `(define-phase2-handler ,upper-name (args result-reg ctx)
     (when args
       (multiple-value-bind (str-ast start-ast end-ast)
           (%parse-string-case-kwargs args)
         (if (or start-ast end-ast)
             ;; Keyword-arg form: reconstruct full string = prefix + case(slice) + suffix
             ;; (ANSI CL: only the [start,end) region is modified)
             (let* ((str-reg    (compile-ast str-ast ctx))
                    ;; start register (default 0)
                    (start-reg  (if start-ast
                                    (compile-ast start-ast ctx)
                                    (let ((r (make-register ctx)))
                                      (emit ctx (make-vm-const :dst r :value 0)) r)))
                    ;; end register (default length of string)
                    (len-reg    (make-register ctx))
                    (end-reg    (progn
                                  (emit ctx (make-vm-length :dst len-reg :src str-reg))
                                  (if end-ast (compile-ast end-ast ctx) len-reg)))
                    ;; prefix = str[0..start)
                    (zero-reg   (let ((r (make-register ctx)))
                                  (emit ctx (make-vm-const :dst r :value 0)) r))
                    (pre-reg    (make-register ctx))
                    ;; slice = str[start..end)
                    (slice-reg  (make-register ctx))
                    ;; suffix = str[end..length)
                    (suf-reg    (make-register ctx))
                    (case-reg   (make-register ctx))
                    (tmp-reg    (make-register ctx)))
               (emit ctx (make-vm-subseq :dst pre-reg   :string str-reg
                                         :start zero-reg :end start-reg))
               (emit ctx (make-vm-subseq :dst slice-reg  :string str-reg
                                         :start start-reg :end end-reg))
               (emit ctx (make-vm-subseq :dst suf-reg    :string str-reg
                                         :start end-reg   :end len-reg))
               (emit ctx (,ctor :dst case-reg :src slice-reg))
               (emit ctx (make-vm-concatenate :dst tmp-reg      :str1 pre-reg  :str2 case-reg))
               (emit ctx (make-vm-concatenate :dst result-reg   :str1 tmp-reg  :str2 suf-reg))
               result-reg)
             ;; No keyword args: simple 1-arg case op
             (let ((str-reg (compile-ast str-ast ctx)))
               (emit ctx (,ctor :dst result-reg :src str-reg))
               result-reg))))))

(define-string-case-handler "STRING-UPCASE"     make-vm-string-upcase)
(define-string-case-handler "STRING-DOWNCASE"   make-vm-string-downcase)
(define-string-case-handler "STRING-CAPITALIZE" make-vm-string-capitalize)
(define-string-case-handler "NSTRING-UPCASE"    make-vm-nstring-upcase)
(define-string-case-handler "NSTRING-DOWNCASE"  make-vm-nstring-downcase)
(define-string-case-handler "NSTRING-CAPITALIZE" make-vm-nstring-capitalize)

;; concatenate: only (concatenate 'string a b) → vm-concatenate
(define-phase2-handler "CONCATENATE" (args result-reg ctx)
  (when (and (>= (length args) 3)
             (typep (first args) 'ast-quote)
             (string= (symbol-name (ast-quote-value (first args))) "STRING"))
    (let ((str1-reg (compile-ast (second args) ctx))
          (str2-reg (compile-ast (third args)  ctx)))
      (emit ctx (make-vm-concatenate :dst result-reg :str1 str1-reg :str2 str2-reg))
      result-reg)))
