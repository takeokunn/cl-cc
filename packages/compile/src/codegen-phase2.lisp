(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Phase 2: AST-Introspecting Builtin Handlers
;;;
;;; Builtins in this file cannot be handled by the data-driven Phase 1
;;; *builtin-registry* because they need to inspect AST node types at
;;; compile time.
;;;
;;; The stream/reader/printer, hash-table, slot-predicate, and string-keyword
;;; clusters live in dedicated files so this file can stay focused on the
;;; remaining array / numeric / control handlers.
;;;
;;; Load order: after codegen-functions, before codegen.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar *phase2-builtin-handlers* (make-hash-table :test 'equal)
  "Maps uppercase builtin name strings to Phase 2 handler functions.
Handler lambda-list: (args result-reg ctx) → result-reg-or-nil.")

(defmacro define-phase2-handler (name (args result-reg ctx) &body body)
  "Register a Phase 2 handler for the builtin named NAME (uppercase string)."
  (list 'setf
        (list 'gethash name '*phase2-builtin-handlers*)
        (cons 'lambda (cons (list args result-reg ctx) body))))

;;; ── Handler registrations ──────────────────────────────────────────────────

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

;; list: variadic constructor → cons chain ending in NIL
(define-phase2-handler "LIST" (args result-reg ctx)
  (emit ctx (make-vm-const :dst result-reg :value nil))
  (dolist (arg (reverse args))
    (let ((arg-reg (compile-ast arg ctx))
          (new-reg (make-register ctx)))
      (emit ctx (make-vm-cons :dst new-reg :car-src arg-reg :cdr-src result-reg))
      (setf result-reg new-reg)))
  result-reg)

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

;; set-fdefinition: register a compiled closure under a symbol name and return the closure
(define-phase2-handler "SET-FDEFINITION" (args result-reg ctx)
  (when (= (length args) 2)
    (let* ((fn-reg (compile-ast (first args) ctx))
           (name-ast (second args))
           (name (cond
                   ((typep name-ast 'ast-quote)
                    (ast-quote-value name-ast))
                   ((typep name-ast 'ast-var)
                    (ast-var-name name-ast))
                   (t nil))))
      (when (symbolp name)
        (emit ctx (make-vm-register-function :name name :src fn-reg))
        (if (eq result-reg fn-reg)
            fn-reg
            (progn
              (emit ctx (make-vm-move :dst result-reg :src fn-reg))
              result-reg))))))
