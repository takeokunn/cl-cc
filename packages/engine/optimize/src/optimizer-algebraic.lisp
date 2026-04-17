;;;; packages/engine/optimize/src/optimizer-algebraic.lisp — Algebraic Identity Rules and Dead-Label Table
;;;
;;; Extracted from optimizer-tables.lisp.
;;; Contains:
;;;   - *opt-algebraic-identity-rules* — data-driven algebraic simplification rules
;;;   - Classification predicates (opt-binary-lhs-rhs-p, opt-unary-src-p,
;;;     opt-foldable-unary-arith-p, opt-foldable-type-pred-p)
;;;   - *opt-label-ref-table* — dead-label reference extraction
;;;
;;; Depends on optimizer-tables.lisp (*opt-binary-lhs-rhs-types*,
;;;   *opt-unary-src-types*, *opt-unary-fold-table*, *opt-type-pred-fold-table*).
;;; Load order: immediately after optimizer-tables.lisp.

(in-package :cl-cc/optimize)

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
    (flet ((reg (tp rules) (setf (gethash tp ht) rules))
           (reg-variants (types rules) (dolist (tp types) (setf (gethash tp ht) rules))))
      ;; Arithmetic — variant groups share identical rules across generic/integer/float specializations
      (reg-variants '(vm-add vm-integer-add vm-float-add)
                    '(((:rconst 0) . :move-lhs) ((:lconst 0) . :move-rhs)))
      (reg-variants '(vm-sub vm-integer-sub vm-float-sub)
                    '(((:rconst 0) . :move-lhs) (:same-reg . (:const 0))))
      (reg-variants '(vm-mul vm-integer-mul vm-float-mul)
                    '(((:rconst 1) . :move-lhs) ((:lconst 1) . :move-rhs)
                      ((:rconst 0) . (:const 0)) ((:lconst 0) . (:const 0))
                      ((:rconst -1) . (:neg :lhs)) ((:lconst -1) . (:neg :rhs))))
      (reg-variants '(vm-div vm-cl-div vm-float-div vm-floor-inst)
                    '(((:rconst 1) . :move-lhs)))
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
      (reg 'vm-rem       '(((:lconst 0) . (:const 0))))
      (reg 'vm-ash       '(((:rconst 0) . :move-lhs) ((:lconst 0) . (:const 0))))
      (reg 'vm-rotate    '(((:rconst 0) . :move-lhs) ((:lconst 0) . (:const 0))))
    ht))
  "Maps VM binary instruction types to lists of algebraic identity rules.")

;;; ─── Classification Predicates ───────────────────────────────────────────
;;;
;;; These predicates are derived purely from the data tables in optimizer-tables.lisp.
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
