(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer Data Tables
;;;
;;; All data-driven dispatch tables for the instruction optimizer.
;;; Separated from the pass logic so tables can be read and extended
;;; independently of the algorithm code.
;;;
;;; Load order: before optimizer.lisp (tables must exist before passes).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── Classification Helpers ──────────────────────────────────────────────

(defun opt-falsep (value)
  "Compile-time analog of vm-falsep: T if VALUE is falsy (nil or 0)."
  (or (null value) (eql value 0)))

(defun opt-register-keyword-p (x)
  "T if X is a VM register keyword of the form :Rn (e.g. :R0, :R15).
   Used by the sexp-reflection fallback in opt-inst-read-regs."
  (and (keywordp x)
       (let ((name (symbol-name x)))
         (and (>= (length name) 2)
              (char= (char name 0) #\R)
              (every #'digit-char-p (subseq name 1))))))

(defun opt-inst-dst (inst)
  "Return the single destination register written by INST, or NIL.
   Uses the shared vm-dst generic function; returns NIL for instructions that
   do not write a destination (jump, halt, ret, print, etc.) or for unknown types."
  (ignore-errors (vm-dst inst)))

;;; opt-inst-pure-p is defined in effects.lisp (loaded first in the optimize module).
;;; It replaces the former 2-type whitelist with a 100+-type data-driven table.

;;; ─── Fold Tables ─────────────────────────────────────────────────────────
;;;
;;; Each table maps a VM instruction struct-type symbol to a CL function
;;; that performs the compile-time fold.  Adding a new foldable instruction
;;; requires only ONE entry here — opt-fold-binop-value, opt-pass-fold,
;;; and opt-pass-cse all derive their behavior from these tables.

(defparameter *opt-binary-fold-table*
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash 'vm-add ht) #'+)
    (setf (gethash 'vm-sub ht) #'-)
    (setf (gethash 'vm-mul ht) #'*)
    (setf (gethash 'vm-mod ht) #'mod)
    (setf (gethash 'vm-rem ht) #'rem)
    (setf (gethash 'vm-min ht) #'min)
    (setf (gethash 'vm-max ht) #'max)
    (setf (gethash 'vm-logand ht) #'logand)
    (setf (gethash 'vm-logior ht) #'logior)
    (setf (gethash 'vm-logxor ht) #'logxor)
    (setf (gethash 'vm-logeqv ht) #'logeqv)
    (setf (gethash 'vm-ash ht) #'ash)
    ht)
  "Maps binary VM instruction types to their CL fold functions.
   Used by opt-fold-binop-value for constant folding.")

(defparameter *opt-binary-cmp-fold-table*
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash 'vm-lt ht) #'<)
    (setf (gethash 'vm-gt ht) #'>)
    (setf (gethash 'vm-le ht) #'<=)
    (setf (gethash 'vm-ge ht) #'>=)
    (setf (gethash 'vm-num-eq ht) #'=)
    ht)
  "Maps comparison VM instruction types to their CL predicate functions.
   Comparisons fold to 1/0 (not t/nil) for VM register semantics.")

(defparameter *opt-binary-zero-guard-types*
  '(vm-div vm-cl-div vm-mod vm-rem)
  "Binary instruction types that must guard against zero divisor before folding.")

(defparameter *opt-binary-no-fold-types*
  '(vm-floor-inst vm-ceiling-inst vm-truncate vm-round-inst)
  "Binary instruction types that set vm-values-list side-channel and must NOT be folded.")

(defparameter *opt-unary-fold-table*
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash 'vm-neg ht) #'-)
    (setf (gethash 'vm-abs ht) #'abs)
    (setf (gethash 'vm-inc ht) #'1+)
    (setf (gethash 'vm-dec ht) #'1-)
    (setf (gethash 'vm-lognot ht) #'lognot)
    (setf (gethash 'vm-not ht) (lambda (x) (if (or (null x) (eql x 0)) t nil)))
    ht)
  "Maps unary VM instruction types to their CL fold functions.")

(defparameter *opt-type-pred-fold-table*
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash 'vm-null-p ht) #'null)
    (setf (gethash 'vm-cons-p ht) #'consp)
    (setf (gethash 'vm-symbol-p ht) #'symbolp)
    (setf (gethash 'vm-number-p ht) #'numberp)
    (setf (gethash 'vm-integer-p ht) #'integerp)
    (setf (gethash 'vm-function-p ht) (lambda (x) (declare (ignore x)) nil))
    ht)
  "Maps type-predicate VM instruction types to their CL predicate functions.
   Predicates fold to 1/0.  vm-function-p always returns 0 for constants.")

(defparameter *opt-foldable-binary-types*
  (let ((types nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k types)) *opt-binary-fold-table*)
    (maphash (lambda (k v) (declare (ignore v)) (push k types)) *opt-binary-cmp-fold-table*)
    types)
  "All binary instruction types that participate in constant folding or CSE.
   Derived from the fold tables — single source of truth.")

(defparameter *opt-foldable-unary-types*
  (let ((types nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k types)) *opt-unary-fold-table*)
    (maphash (lambda (k v) (declare (ignore v)) (push k types)) *opt-type-pred-fold-table*)
    types)
  "All unary instruction types that participate in constant folding or CSE.
   Derived from the fold tables — single source of truth.")

(defparameter *opt-binary-lhs-rhs-types*
  '(vm-lt vm-gt vm-le vm-ge vm-num-eq vm-eq
    vm-mod vm-rem vm-min vm-max
    vm-truncate vm-floor-inst vm-ceiling-inst vm-round-inst
    vm-logand vm-logior vm-logxor vm-logeqv vm-ash
    vm-div vm-cl-div vm-ffloor vm-fceiling vm-ftruncate vm-fround)
  "Non-binop-subclass instruction types that have vm-lhs/vm-rhs accessors.
   vm-binop (parent of vm-add/vm-sub/vm-mul) is handled by typecase inheritance.
   Used by opt-inst-read-regs, opt-pass-fold, opt-pass-cse, and WASM register collection.")

(defparameter *opt-unary-src-types*
  '(vm-neg vm-abs vm-inc vm-dec vm-lognot vm-not
    vm-cons-p vm-null-p vm-symbol-p vm-number-p
    vm-integer-p vm-function-p)
  "Instruction types that have vm-src/vm-dst unary accessors.
   Used by opt-inst-read-regs, opt-pass-fold, opt-pass-cse, and WASM register collection.")

(defparameter *opt-commutative-inst-types*
  '(vm-add vm-mul vm-logand vm-logior vm-logxor vm-logeqv
    vm-num-eq vm-eq vm-min vm-max)
  "VM binary instruction struct-type symbols where operand order is irrelevant.
   Used by opt-pass-cse to produce a canonical key for commutative expressions,
   enabling (+ a b) and (+ b a) to share the same CSE memo entry.")

(defparameter *opt-read-regs-table*
  (let ((ht (make-hash-table :test #'eq)))
    ;; Single vm-reg accessor
    (dolist (tp '(vm-jump-zero vm-print vm-halt vm-ret))
      (setf (gethash tp ht) (lambda (inst) (list (vm-reg inst)))))
    ;; Single vm-src accessor
    (dolist (tp '(vm-set-global vm-register-function vm-ensure-values vm-spread-values))
      (setf (gethash tp ht) (lambda (inst) (list (vm-src inst)))))
    ;; Call-family: func/gf register + args
    (dolist (tp '(vm-call vm-apply))
      (setf (gethash tp ht) (lambda (inst) (cons (vm-func-reg inst) (vm-args inst)))))
    (setf (gethash 'vm-generic-call ht)
          (lambda (inst) (cons (vm-gf-reg inst) (vm-args inst))))
    ;; Multiple values
    (setf (gethash 'vm-values ht)       (lambda (inst) (vm-src-regs inst)))
    ;; Object operations
    (setf (gethash 'vm-closure-ref-idx ht) (lambda (inst) (list (vm-closure-reg inst))))
    (setf (gethash 'vm-slot-read ht)    (lambda (inst) (list (vm-obj-reg inst))))
    (setf (gethash 'vm-slot-write ht)   (lambda (inst) (list (vm-obj-reg inst) (vm-value-reg inst))))
    (setf (gethash 'vm-register-method ht)
          (lambda (inst) (list (vm-gf-reg inst) (vm-method-reg inst))))
    (setf (gethash 'vm-make-obj ht)
          (lambda (inst) (cons (vm-class-reg inst) (mapcar #'cdr (vm-initarg-regs inst)))))
    ;; Compound instructions with optional registers
    (setf (gethash 'vm-make-string ht)
          (lambda (inst) (remove nil (list (vm-src inst) (vm-char inst)))))
    (setf (gethash 'vm-intern-symbol ht)
          (lambda (inst) (remove nil (list (vm-src inst) (vm-intern-pkg inst)))))
    (setf (gethash 'vm-make-array ht)
          (lambda (inst) (remove nil (list (vm-size-reg inst) (vm-initial-element inst)
                                           (vm-fill-pointer inst) (vm-adjustable inst)))))
    ht)
  "Maps VM instruction type symbols to (lambda (inst) ...) read-reg extractors.
   Used by opt-inst-read-regs for types not covered by the bulk tables.")

(defun opt-inst-read-regs (inst)
  "Return a list of all register names read by INST.
   Dispatch: zero-read types → nil, move → src, binop → lhs/rhs,
   binary/unary tables, then *opt-read-regs-table* for specific types,
   finally sexp-reflection fallback."
  (let ((tp (type-of inst)))
    (cond
      ;; Zero-read instructions
      ((member tp '(vm-const vm-func-ref vm-get-global vm-values-to-list) :test #'eq)
       nil)
      ;; Move: single source
      ((eq tp 'vm-move) (list (vm-src inst)))
      ;; vm-binop subclasses (vm-add, vm-sub, vm-mul) — handled by typep
      ((typep inst 'vm-binop) (list (vm-lhs inst) (vm-rhs inst)))
      ;; Non-binop binary: data-driven from *opt-binary-lhs-rhs-types*
      ((member tp *opt-binary-lhs-rhs-types* :test #'eq)
       (list (vm-lhs inst) (vm-rhs inst)))
      ;; Unary: data-driven from *opt-unary-src-types*
      ((member tp *opt-unary-src-types* :test #'eq)
       (list (vm-src inst)))
      ;; Per-type table lookup
      (t (let ((handler (gethash tp *opt-read-regs-table*)))
           (if handler
               (funcall handler inst)
               ;; Fallback: serialize to sexp and collect all register-shaped keywords
               (let ((dst (opt-inst-dst inst))
                     (regs nil))
                 (labels ((collect (x)
                            (cond ((and (keywordp x)
                                        (opt-register-keyword-p x)
                                        (not (eq x dst)))
                                   (push x regs))
                                  ((consp x)
                                   (collect (car x))
                                   (collect (cdr x))))))
                   (handler-case (collect (instruction->sexp inst))
                     (error () nil)))
                 regs)))))))

;;; ─── Algebraic Identity Rules ────────────────────────────────────────────
;;;
;;; Each entry: (inst-type . ((condition . action) ...))
;;; Conditions: (:rconst N) = right operand is constant N
;;;             (:lconst N) = left operand is constant N
;;;             :same-reg   = both operands are the same register
;;; Actions:    :move-lhs / :move-rhs   = copy one operand
;;;             (:const V)              = produce constant V
;;;             (:neg :lhs) / (:neg :rhs) = negate one operand

(defparameter *opt-algebraic-identity-rules*
  (let ((ht (make-hash-table :test #'eq)))
    (flet ((reg (tp rules) (setf (gethash tp ht) rules)))
      ;; Arithmetic
      (reg 'vm-add       '(((:rconst 0) . :move-lhs) ((:lconst 0) . :move-rhs)))
      (reg 'vm-sub       '(((:rconst 0) . :move-lhs) (:same-reg . (:const 0))))
      (reg 'vm-mul       '(((:rconst 1) . :move-lhs) ((:lconst 1) . :move-rhs)
                            ((:rconst 0) . (:const 0)) ((:lconst 0) . (:const 0))
                            ((:rconst -1) . (:neg :lhs)) ((:lconst -1) . (:neg :rhs))))
      (reg 'vm-div       '(((:rconst 1) . :move-lhs)))
      (reg 'vm-cl-div    '(((:rconst 1) . :move-lhs)))
      (reg 'vm-floor-inst '(((:rconst 1) . :move-lhs)))
      (reg 'vm-mod       '(((:lconst 0) . (:const 0))))
      ;; Comparisons
      (dolist (tp '(vm-num-eq vm-eq vm-le vm-ge))
        (reg tp '((:same-reg . (:const 1)))))
      (dolist (tp '(vm-lt vm-gt))
        (reg tp '((:same-reg . (:const 0)))))
      ;; Bitwise
      (reg 'vm-logand    '(((:rconst 0) . (:const 0)) ((:lconst 0) . (:const 0))
                            ((:rconst -1) . :move-lhs) ((:lconst -1) . :move-rhs)
                            (:same-reg . :move-lhs)))
      (reg 'vm-logior    '(((:rconst 0) . :move-lhs) ((:lconst 0) . :move-rhs)
                            ((:rconst -1) . (:const -1)) ((:lconst -1) . (:const -1))
                            (:same-reg . :move-lhs)))
      (reg 'vm-logxor    '(((:rconst 0) . :move-lhs) ((:lconst 0) . :move-rhs)
                            (:same-reg . (:const 0))))
      (reg 'vm-ash       '(((:rconst 0) . :move-lhs) ((:lconst 0) . (:const 0)))))
    ht)
  "Maps VM binary instruction types to lists of algebraic identity rules.")

;;; ─── Classification Predicates ───────────────────────────────────────────
;;;
;;; These predicates are derived purely from the data tables above.
;;; Adding a new foldable instruction type to a table automatically extends
;;; the predicates — opt-pass-fold and opt-pass-cse never need hand-editing.

(defun opt-binary-lhs-rhs-p (inst)
  "T if INST is a binary instruction accessed via vm-lhs/vm-rhs.
   Covers vm-binop subclasses (vm-add/sub/mul) via class inheritance,
   plus all additional types listed in *opt-binary-lhs-rhs-types*."
  (or (typep inst 'vm-binop)
      (member (type-of inst) *opt-binary-lhs-rhs-types* :test #'eq)))

(defun opt-unary-src-p (inst)
  "T if INST is a unary instruction accessed via vm-src/vm-dst.
   Derived from *opt-unary-src-types* — the single source of truth."
  (member (type-of inst) *opt-unary-src-types* :test #'eq))

(defun opt-foldable-unary-arith-p (inst)
  "T if INST is a foldable unary arithmetic instruction (not a type predicate).
   Derived from *opt-unary-fold-table*."
  (not (null (gethash (type-of inst) *opt-unary-fold-table*))))

(defun opt-foldable-type-pred-p (inst)
  "T if INST is a foldable type predicate (cons-p, null-p, etc.).
   Derived from *opt-type-pred-fold-table*."
  (not (null (gethash (type-of inst) *opt-type-pred-fold-table*))))

;;; ─── Dead-Label Reference Table ──────────────────────────────────────────

(defparameter *opt-label-ref-table*
  (let ((ht (make-hash-table :test #'eq)))
    ;; These all use vm-label-name to extract the referenced label
    (dolist (tp '(vm-jump vm-jump-zero vm-closure vm-make-closure vm-func-ref))
      (setf (gethash tp ht) #'vm-label-name))
    ;; Handler labels use a distinct accessor
    (setf (gethash 'vm-establish-handler ht) #'vm-handler-label)
    ;; Catch labels use a distinct accessor
    (setf (gethash 'vm-establish-catch ht) #'vm-catch-handler-label)
    ht)
  "Maps instruction type symbols to accessor functions that extract referenced label names.
   Used by opt-pass-dead-labels to collect all live labels without typecase dispatch.")
