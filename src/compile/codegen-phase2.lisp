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

;; make-string: optional :initial-element keyword
(define-phase2-handler "MAKE-STRING" (args result-reg ctx)
  (let ((size-reg (compile-ast (first args) ctx)))
    (if (and (= (length args) 3)
             (typep (second args) 'ast-var)
             (eq (ast-var-name (second args)) :initial-element))
        (let ((char-reg (compile-ast (third args) ctx)))
          (emit ctx (make-vm-make-string :dst result-reg :src size-reg :char char-reg)))
        (emit ctx (make-vm-make-string :dst result-reg :src size-reg)))
    result-reg))

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
  (let* ((path-reg  (compile-ast (first args) ctx))
         (direction (if (and (>= (length args) 3)
                             (typep (second args) 'ast-var)
                             (eq (ast-var-name (second args)) :direction)
                             (typep (third args) 'ast-var)
                             (keywordp (ast-var-name (third args))))
                        (ast-var-name (third args))
                        :input)))
    (emit ctx (make-vm-open-file :dst result-reg :path path-reg :direction direction))
    result-reg))

;; peek-char: (peek-char handle) or (peek-char nil handle)
(define-phase2-handler "PEEK-CHAR" (args result-reg ctx)
  (let ((handle-reg (if (>= (length args) 2)
                        (compile-ast (second args) ctx)
                        (compile-ast (first args) ctx))))
    (emit ctx (make-vm-peek-char :dst result-reg :handle handle-reg))
    result-reg))

;; make-string-input-stream
(define-phase2-handler "MAKE-STRING-INPUT-STREAM" (args result-reg ctx)
  (let ((str-reg (compile-ast (first args) ctx)))
    (emit ctx (make-vm-make-string-stream :dst result-reg :direction :input
                                          :initial-string str-reg))
    result-reg))

;; concatenate: only (concatenate 'string a b) → vm-concatenate
(define-phase2-handler "CONCATENATE" (args result-reg ctx)
  (when (and (>= (length args) 3)
             (typep (first args) 'ast-quote)
             (string= (symbol-name (ast-quote-value (first args))) "STRING"))
    (let ((str1-reg (compile-ast (second args) ctx))
          (str2-reg (compile-ast (third args)  ctx)))
      (emit ctx (make-vm-concatenate :dst result-reg :str1 str1-reg :str2 str2-reg))
      result-reg)))
