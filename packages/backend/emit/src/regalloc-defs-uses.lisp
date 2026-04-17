;;;; packages/backend/emit/src/regalloc-defs-uses.lisp — instruction-defs / instruction-uses methods
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Extracted from regalloc.lisp so that the protocol implementations for
;;; every VM instruction subsystem live in one focused file.
;;; Depends on regalloc.lisp (live-interval/regalloc-result structs,
;;;   instruction-defs/instruction-uses generic function declarations).
;;; Load order: immediately after regalloc.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/emit)

;;; Default catch-all (base vm-instruction): no defs, no uses
(defmethod instruction-defs ((inst vm-instruction)) nil)
(defmethod instruction-uses ((inst vm-instruction)) nil)

;; dst-only: vm-const, vm-func-ref, vm-get-global
(defmethod instruction-defs ((inst vm-const)) (list (vm-dst inst)))
(defmethod instruction-defs ((inst vm-func-ref)) (list (vm-dst inst)))
(defmethod instruction-defs ((inst vm-get-global)) (list (vm-dst inst)))

;; src-only: vm-set-global
(defmethod instruction-uses ((inst vm-set-global)) (list (vm-src inst)))

;; dst+src: vm-move and unary type predicates
(defmethod instruction-defs ((inst vm-move)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-move)) (list (vm-src inst)))

;; vm-binop: dst = lhs op rhs (covers add, sub, mul, and all comparison ops)
(defmethod instruction-defs ((inst vm-binop)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-binop)) (list (vm-lhs inst) (vm-rhs inst)))

;; vm-select: dst = cond ? then : else
(defmethod instruction-defs ((inst vm-select)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-select))
  (list (vm-select-cond-reg inst)
        (vm-select-then-reg inst)
        (vm-select-else-reg inst)))

;; Jump-zero: uses reg
(defmethod instruction-uses ((inst vm-jump-zero)) (list (vm-reg inst)))

;; Print: uses reg
(defmethod instruction-uses ((inst vm-print)) (list (vm-reg inst)))

;; Halt: uses reg
(defmethod instruction-uses ((inst vm-halt)) (list (vm-reg inst)))

;; Return: uses reg
(defmethod instruction-uses ((inst vm-ret)) (list (vm-reg inst)))

;; Closure: defs dst, uses captured register values
(defmethod instruction-defs ((inst vm-closure)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-closure))
  (mapcar #'cdr (vm-captured-vars inst)))

;; Make-closure: defs dst, uses env-regs
(defmethod instruction-defs ((inst vm-make-closure)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-make-closure))
  (copy-list (vm-env-regs inst)))

;; Closure-ref-idx: defs dst, uses closure reg
(defmethod instruction-defs ((inst vm-closure-ref-idx)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-closure-ref-idx))
  (list (vm-closure-reg inst)))

;; Call: defs dst, uses func + args
(defmethod instruction-defs ((inst vm-call)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-call))
  (cons (vm-func-reg inst) (copy-list (vm-args inst))))

;; Tail-call: does not define a live destination because control never returns.
(defmethod instruction-defs ((inst vm-tail-call)) nil)
(defmethod instruction-uses ((inst vm-tail-call))
  (cons (vm-func-reg inst) (copy-list (vm-args inst))))

;;; Primitive instructions (vm-primitives.lisp)

;; Comparison ops inherit from vm-instruction, have dst/lhs/rhs pattern
;; vm-eq, vm-lt, vm-gt, vm-le, vm-ge, vm-num-eq, vm-div, vm-mod, vm-and, vm-or
(macrolet ((def-binop-like (class)
             `(progn
                (defmethod instruction-defs ((inst ,class)) (list (vm-dst inst)))
                (defmethod instruction-uses ((inst ,class)) (list (vm-lhs inst) (vm-rhs inst))))))
  (def-binop-like vm-eq)
  (def-binop-like vm-lt)
  (def-binop-like vm-gt)
  (def-binop-like vm-le)
  (def-binop-like vm-ge)
  (def-binop-like vm-num-eq)
  (def-binop-like vm-div)
  (def-binop-like vm-cl-div)
  (def-binop-like vm-mod)
  (def-binop-like vm-and)
  (def-binop-like vm-or)
  ;; Bitwise logical ops (inherit vm-instruction directly, not vm-binop)
  (def-binop-like vm-logand)
  (def-binop-like vm-logior)
  (def-binop-like vm-logxor)
  (def-binop-like vm-logeqv)
  (def-binop-like vm-logtest)
  (def-binop-like vm-logbitp)
  ;; Min/max (inherit vm-instruction directly)
  (def-binop-like vm-min)
  (def-binop-like vm-max)
  ;; Arithmetic shift (inherit vm-instruction directly)
  (def-binop-like vm-ash)
  (def-binop-like vm-rotate)
  ;; Integer division ops (inherit vm-instruction directly, not vm-binop)
  (def-binop-like vm-truncate)
  (def-binop-like vm-rem)
  ;; FR-640: nreconc
  (def-binop-like vm-nreconc)
  )

;; Unary ops: dst+src pattern
(macrolet ((def-unary-like (class)
             `(progn
                (defmethod instruction-defs ((inst ,class)) (list (vm-dst inst)))
                (defmethod instruction-uses ((inst ,class)) (list (vm-src inst))))))
  (def-unary-like vm-cons-p)
  (def-unary-like vm-null-p)
  (def-unary-like vm-symbol-p)
  (def-unary-like vm-number-p)
  (def-unary-like vm-integer-p)
  (def-unary-like vm-function-p)
  (def-unary-like vm-neg)
  (def-unary-like vm-abs)
  (def-unary-like vm-inc)
  (def-unary-like vm-dec)
  (def-unary-like vm-not)
  ;; Bitwise complement (vm-instruction directly, not vm-binop)
  (def-unary-like vm-lognot)
  (def-unary-like vm-bswap)
  ;; FR-648: simple-vector-p predicate
  (def-unary-like vm-simple-vector-p)
  ;; FR-563: sixth–tenth list accessors
  (def-unary-like vm-sixth)
  (def-unary-like vm-seventh)
  (def-unary-like vm-eighth)
  (def-unary-like vm-ninth)
  (def-unary-like vm-tenth)
  ;; FR-596: nbutlast
  (def-unary-like vm-nbutlast)
  ;; FR-597: function combinators
  (def-unary-like vm-identity)
  (def-unary-like vm-constantly)
  (def-unary-like vm-complement)
  ;; FR-631: macro expansion
  (def-unary-like vm-macroexpand-1-inst)
  (def-unary-like vm-macroexpand-inst)
  ;; FR-498: hash code
  (def-unary-like vm-sxhash)
  ;; FR-677: CLOS introspection
  (def-unary-like vm-class-name-fn)
  (def-unary-like vm-class-of-fn))

;;; List instructions (vm-list.lisp)

;; vm-cons: defs dst, uses car-reg + cdr-reg
(defmethod instruction-defs ((inst vm-cons)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-cons))
  (list (vm-car-reg inst) (vm-cdr-reg inst)))

;; vm-car, vm-cdr: defs dst, uses cons-reg
(defmethod instruction-defs ((inst vm-car)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-car)) (list (vm-cons-reg inst)))
(defmethod instruction-defs ((inst vm-cdr)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-cdr)) (list (vm-cons-reg inst)))

;; vm-rplaca, vm-rplacd: uses cons-reg + val-reg
(defmethod instruction-uses ((inst vm-rplaca))
  (list (vm-cons-reg inst) (vm-val-reg inst)))
(defmethod instruction-uses ((inst vm-rplacd))
  (list (vm-cons-reg inst) (vm-val-reg inst)))

;; vm-push: defs dst, uses src + list-reg
(defmethod instruction-defs ((inst vm-push)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-push))
  (list (vm-src inst) (vm-cons-reg inst)))

;; vm-pop: defs dst, uses cons-reg
(defmethod instruction-defs ((inst vm-pop)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-pop)) (list (vm-cons-reg inst)))

;;; CLOS instructions (vm.lisp)

(defmethod instruction-defs ((inst vm-class-def)) (list (vm-dst inst)))
(defmethod instruction-defs ((inst vm-make-obj)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-make-obj))
  (cons (vm-class-reg inst)
        (mapcar #'cdr (vm-initarg-regs inst))))
(defmethod instruction-defs ((inst vm-slot-read)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-slot-read)) (list (vm-obj-reg inst)))
(defmethod instruction-uses ((inst vm-slot-write))
  (list (vm-obj-reg inst) (vm-value-reg inst)))
(defmethod instruction-uses ((inst vm-register-method))
  (list (vm-gf-reg inst) (vm-method-reg inst)))
(defmethod instruction-defs ((inst vm-generic-call)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-generic-call))
  (cons (vm-gf-reg inst) (copy-list (vm-args inst))))

;; vm-values: defs dst, uses all src-regs
(defmethod instruction-defs ((inst vm-values)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-values))
  (copy-list (vm-src-regs inst)))

;; vm-mv-bind: defs all dst-regs
(defmethod instruction-defs ((inst vm-mv-bind))
  (copy-list (vm-dst-regs inst)))

;; vm-apply: defs dst, uses func + args
(defmethod instruction-defs ((inst vm-apply)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-apply))
  (cons (vm-func-reg inst) (copy-list (vm-args inst))))

;; Handler-case instructions
(defmethod instruction-defs ((inst vm-establish-handler))
  (list (vm-handler-result-reg inst)))
(defmethod instruction-uses ((inst vm-signal-error))
  (list (vm-error-reg inst)))
;; Catch/throw instructions
(defmethod instruction-defs ((inst vm-establish-catch))
  (list (vm-catch-result-reg inst)))
(defmethod instruction-uses ((inst vm-establish-catch))
  (list (vm-catch-tag-reg inst)))
(defmethod instruction-uses ((inst vm-throw))
  (list (vm-throw-tag-reg inst) (vm-throw-value-reg inst)))
