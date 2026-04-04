(in-package :cl-cc)

;;; VM Instructions

(define-vm-instruction vm-const (vm-instruction)
  (dst nil :reader vm-dst)
  (value nil :reader vm-value)
  (:sexp-tag :const))

(define-vm-instruction vm-move (vm-instruction)
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :move))

(define-vm-instruction vm-binop (vm-instruction)
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs))

(define-vm-instruction vm-add (vm-binop)
  (:sexp-tag :add)
  (:sexp-slots dst lhs rhs))
(define-vm-instruction vm-sub (vm-binop)
  (:sexp-tag :sub)
  (:sexp-slots dst lhs rhs))
(define-vm-instruction vm-mul (vm-binop)
  (:sexp-tag :mul)
  (:sexp-slots dst lhs rhs))

;;; Specialized arithmetic instructions used by type-directed codegen.
(define-vm-instruction vm-integer-add (vm-add)
  (:sexp-tag :iadd)
  (:sexp-slots dst lhs rhs))
(define-vm-instruction vm-integer-sub (vm-sub)
  (:sexp-tag :isub)
  (:sexp-slots dst lhs rhs))
(define-vm-instruction vm-integer-mul (vm-mul)
  (:sexp-tag :imul)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-float-add (vm-add)
  (:sexp-tag :fadd)
  (:sexp-slots dst lhs rhs))
(define-vm-instruction vm-float-sub (vm-sub)
  (:sexp-tag :fsub)
  (:sexp-slots dst lhs rhs))
(define-vm-instruction vm-float-mul (vm-mul)
  (:sexp-tag :fmul)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-label (vm-instruction)
  (name nil :reader vm-name)
  (:sexp-tag :label)
  (:conc-name vm-lbl-))

(define-vm-instruction vm-jump (vm-instruction)
  (label nil :reader vm-label-name)
  (:sexp-tag :jump))

(define-vm-instruction vm-jump-zero (vm-instruction)
  (reg nil :reader vm-reg)
  (label nil :reader vm-label-name)
  (:sexp-tag :jump-zero))

(define-vm-instruction vm-select (vm-instruction)
  "Select one of two values based on COND. DST = THEN if COND is non-NIL, else ELSE."
  (dst nil :reader vm-dst)
  (cond-reg nil :reader vm-select-cond-reg)
  (then-reg nil :reader vm-select-then-reg)
  (else-reg nil :reader vm-select-else-reg)
  (:sexp-tag :select)
  (:sexp-slots dst cond-reg then-reg else-reg))

(define-vm-instruction vm-print (vm-instruction)
  (reg nil :reader vm-reg)
  (:sexp-tag :print))

(define-vm-instruction vm-halt (vm-instruction)
  (reg nil :reader vm-reg)
  (:sexp-tag :halt))

;;; Function support instructions
(define-vm-instruction vm-closure (vm-instruction)
  (dst nil :reader vm-dst)
  (label nil :reader vm-label-name)
  (params nil :reader vm-closure-params)
  (optional-params nil :reader vm-closure-optional-params)
  (rest-param nil :reader vm-closure-rest-param)
  (key-params nil :reader vm-closure-key-params)
  (rest-stack-alloc-p nil :reader vm-closure-rest-stack-alloc-p)
  (captured nil :reader vm-captured-vars)
  (:sexp-tag :closure)
  (:sexp-slots dst label params optional-params rest-param key-params rest-stack-alloc-p captured)
  (:conc-name vm-closure-inst-))

;; vm-make-closure uses list* for instruction->sexp (custom sexp handling)
(define-vm-instruction vm-make-closure (vm-instruction)
  "Create a closure from LABEL with PARAMS, capturing ENV-REGS values on heap."
  (dst nil :reader vm-dst)
  (label nil :reader vm-label-name)
  (params nil :reader vm-make-closure-params)
  (env-regs nil :reader vm-env-regs))

(defmethod instruction->sexp ((inst vm-make-closure))
  (list* :make-closure (vm-make-closure-dst inst) (vm-make-closure-label inst)
         (vm-make-closure-params inst) (vm-make-closure-env-regs inst)))

(setf (gethash :make-closure *instruction-constructors*)
      (lambda (sexp)
        (make-vm-make-closure :dst (second sexp) :label (third sexp)
                              :params (fourth sexp) :env-regs (nthcdr 4 sexp))))

(define-vm-instruction vm-closure-ref-idx (vm-instruction)
  "Access captured value at INDEX from closure in CLOSURE register."
  (dst nil :reader vm-dst)
  (closure nil :reader vm-closure-reg)
  (index nil :reader vm-closure-index)
  (:sexp-tag :closure-ref-idx))

(define-vm-instruction vm-call (vm-instruction)
  (dst nil :reader vm-dst)
  (func nil :reader vm-func-reg)
  (args nil :reader vm-args)
  (:sexp-tag :call))

(define-vm-instruction vm-tail-call (vm-instruction)
  "Tail call: reuses current call frame instead of pushing a new one."
  (dst nil :reader vm-dst)
  (func nil :reader vm-func-reg)
  (args nil :reader vm-args)
  (:sexp-tag :tail-call))

(define-vm-instruction vm-ret (vm-instruction)
  (reg nil :reader vm-reg)
  (:sexp-tag :ret))

(define-vm-instruction vm-func-ref (vm-instruction)
  (dst nil :reader vm-dst)
  (label nil :reader vm-label-name)
  (:sexp-tag :func-ref))

;;; Multiple values and apply instructions
(define-vm-instruction vm-values (vm-instruction)
  "Multiple values. DST gets primary value, values-list stores all."
  (dst nil :reader vm-dst)
  (src-regs nil :reader vm-src-regs)
  (:sexp-tag :values))

(define-vm-instruction vm-mv-bind (vm-instruction)
  "Bind multiple values from values-list to registers."
  (dst-regs nil :reader vm-dst-regs)
  (:sexp-tag :mv-bind))

(define-vm-instruction vm-values-to-list (vm-instruction)
  "Convert vm-values-list to a list in DST register."
  (dst nil :reader vm-dst)
  (:sexp-tag :values-to-list))

(define-vm-instruction vm-spread-values (vm-instruction)
  "Spread a list from SRC register as multiple values. DST gets primary value."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :spread-values))

(define-vm-instruction vm-clear-values (vm-instruction)
  "Clear vm-values-list to nil (reset multiple-values buffer before a form)."
  (:sexp-tag :clear-values))

(define-vm-instruction vm-ensure-values (vm-instruction)
  "If vm-values-list is nil, set it to (list (reg-get SRC)).
   Used after compiling a form for multiple-value-call to normalise
   plain expressions that do not emit vm-values."
  (src nil :reader vm-src)
  (:sexp-tag :ensure-values))

(define-vm-instruction vm-call-next-method (vm-instruction)
  "Call the next applicable method from the current method dispatch context.
   ARGS-REG holds the args list (nil means use original args).
   DST receives the return value."
  (dst nil :reader vm-dst)
  (args-reg nil :reader vm-cnm-args-reg)
  (:sexp-tag :call-next-method))

(define-vm-instruction vm-next-method-p (vm-instruction)
  "Return T if there is a next applicable method in the current dispatch context."
  (dst nil :reader vm-dst)
  (:sexp-tag :next-method-p))

(define-vm-instruction vm-apply (vm-instruction)
  "Apply function with spread arguments. Last arg is a list to spread."
  (dst nil :reader vm-dst)
  (func nil :reader vm-func-reg)
  (args nil :reader vm-args)
  (:sexp-tag :apply))

(define-vm-instruction vm-register-function (vm-instruction)
  "Register a closure in the global function registry by name."
  (name nil :reader vm-func-name)
  (src nil :reader vm-src)
  (:sexp-tag :register-function))

(define-vm-instruction vm-set-global (vm-instruction)
  "Store a value in the global variable store."
  (name nil :reader vm-global-name)
  (src nil :reader vm-src)
  (:sexp-tag :set-global))

(define-vm-instruction vm-get-global (vm-instruction)
  "Load a value from the global variable store into a register."
  (dst nil :reader vm-dst)
  (name nil :reader vm-global-name)
  (:sexp-tag :get-global))
