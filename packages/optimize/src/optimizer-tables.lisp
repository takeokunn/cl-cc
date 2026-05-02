(in-package :cl-cc/optimize)
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
  "Compile-time analog of vm-falsep: T if VALUE is falsy.

The optimizer uses the same language-level truthiness as the VM and CPS layers:
both NIL and numeric zero are false."
  (or (null value)
      (and (numberp value)
           (zerop value))))

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

(defun %alist->eq-hash-table (alist)
  "Build an EQ hash-table from ALIST of (key . value) pairs."
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (pair alist ht)
      (setf (gethash (car pair) ht) (cdr pair)))))

(defun %hash-table-keys (ht)
  "Return all keys of HT as a list."
  (loop for k being the hash-keys of ht collect k))

(defparameter *opt-binary-fold-table*
  (%alist->eq-hash-table
   `((vm-add         . ,#'+)
     (vm-integer-add . ,#'+)
     (vm-float-add   . ,#'+)
     (vm-sub         . ,#'-)
     (vm-integer-sub . ,#'-)
     (vm-float-sub   . ,#'-)
     (vm-mul         . ,#'*)
     (vm-integer-mul . ,#'*)
     (vm-float-mul   . ,#'*)
     (vm-mod         . ,#'mod)
     (vm-rem         . ,#'rem)
     (vm-min         . ,#'min)
     (vm-max         . ,#'max)
     (vm-logand      . ,#'logand)
     (vm-logior      . ,#'logior)
     (vm-logxor      . ,#'logxor)
     (vm-logeqv      . ,#'logeqv)
     (vm-ash         . ,#'ash)
     (vm-rotate      . ,#'rotate-right)
     (vm-bswap       . ,#'bswap)
     (vm-float-div   . ,#'/)
     (vm-gcd         . ,#'gcd)
     (vm-lcm         . ,#'lcm)))
  "Maps binary VM instruction types to their CL fold functions.
   Used by opt-fold-binop-value for constant folding.")

(defparameter *opt-binary-cmp-fold-table*
  (%alist->eq-hash-table
   `((vm-lt     . ,#'<)
     (vm-gt     . ,#'>)
     (vm-le     . ,#'<=)
     (vm-ge     . ,#'>=)
     (vm-num-eq . ,#'=)))
  "Maps comparison VM instruction types to their CL predicate functions.
   Comparisons fold to 1/0 (not t/nil) for VM register semantics.")

(defparameter *opt-binary-zero-guard-types*
  '(vm-div vm-cl-div vm-float-div vm-mod vm-rem)
  "Binary instruction types that must guard against zero divisor before folding.")

(defparameter *opt-binary-no-fold-types*
  '(vm-floor-inst vm-ceiling-inst vm-truncate vm-round-inst)
  "Binary instruction types that set vm-values-list side-channel and must NOT be folded.")

(defparameter *opt-unary-fold-table*
  (%alist->eq-hash-table
   `((vm-neg         . ,#'-)
     (vm-abs         . ,#'abs)
     (vm-inc         . ,#'1+)
     (vm-dec         . ,#'1-)
     (vm-lognot      . ,#'lognot)
     (vm-rational    . ,#'rational)
     (vm-rationalize . ,#'rationalize)
     (vm-numerator   . ,#'numerator)
     (vm-denominator . ,#'denominator)
     (vm-not         . ,(lambda (x) (if (null x) t nil)))))
  "Maps unary VM instruction types to their CL fold functions.")

(defparameter *opt-type-pred-fold-table*
  (%alist->eq-hash-table
   `((vm-null-p     . ,#'null)
     (vm-cons-p     . ,#'consp)
     (vm-symbol-p   . ,#'symbolp)
     (vm-number-p   . ,#'numberp)
     (vm-integer-p  . ,#'integerp)
     (vm-function-p . ,(lambda (x) (declare (ignore x)) nil))))
  "Maps type-predicate VM instruction types to their CL predicate functions.
   Predicates fold to 1/0.  vm-function-p always returns 0 for constants.")

(defparameter *opt-foldable-binary-types*
  (append (%hash-table-keys *opt-binary-fold-table*)
          (%hash-table-keys *opt-binary-cmp-fold-table*))
  "All binary instruction types that participate in constant folding or CSE.
   Derived from the fold tables — single source of truth.")

(defparameter *opt-foldable-unary-types*
  (append (%hash-table-keys *opt-unary-fold-table*)
          (%hash-table-keys *opt-type-pred-fold-table*))
  "All unary instruction types that participate in constant folding or CSE.
   Derived from the fold tables — single source of truth.")

(defparameter *opt-binary-lhs-rhs-types*
  '(vm-lt vm-gt vm-le vm-ge vm-num-eq vm-eq
    vm-gcd vm-lcm
    vm-mod vm-rem vm-min vm-max
    vm-truncate vm-floor-inst vm-ceiling-inst vm-round-inst
    vm-logand vm-logior vm-logxor vm-logeqv vm-ash vm-rotate
    vm-div vm-cl-div vm-ffloor vm-fceiling vm-ftruncate vm-fround)
  "Non-binop-subclass instruction types that have vm-lhs/vm-rhs accessors.
   vm-binop (parent of vm-add/vm-sub/vm-mul) is handled by typecase inheritance.
   Used by opt-inst-read-regs, opt-pass-fold, opt-pass-cse, and WASM register collection.")

(defparameter *opt-unary-src-types*
  '(vm-neg vm-abs vm-inc vm-dec vm-lognot vm-bswap vm-not
    vm-rational vm-rationalize vm-numerator vm-denominator
    vm-cons-p vm-null-p vm-symbol-p vm-number-p
    vm-integer-p vm-function-p)
  "Instruction types that have vm-src/vm-dst unary accessors.
   Used by opt-inst-read-regs, opt-pass-fold, opt-pass-cse, and WASM register collection.")

(defparameter *opt-commutative-inst-types*
  '(vm-add vm-integer-add vm-float-add
    vm-mul vm-integer-mul vm-float-mul
    vm-logand vm-logior vm-logxor vm-logeqv
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
    ;; Variadic-ish concatenation packed by optimizer
    (setf (gethash 'vm-concatenate ht)
          (lambda (inst)
            (remove nil (or (vm-parts inst)
                            (list (vm-str1 inst) (vm-str2 inst))))))
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
          (lambda (inst)
            (remove nil (list (vm-src inst) (cl-cc/vm:vm-intern-pkg inst)))))
    (setf (gethash 'vm-make-array ht)
          (lambda (inst) (remove nil (list (vm-size-reg inst) (vm-initial-element inst)
                                           (vm-fill-pointer inst) (vm-adjustable inst)))))
    ht)
  "Maps VM instruction type symbols to (lambda (inst) ...) read-reg extractors.
   Used by opt-inst-read-regs for types not covered by the bulk tables.")

(defun %opt-collect-sexp-regs (x dst acc)
  "Recursively collect register-shaped keywords from sexp X into ACC, excluding DST."
  (cond
    ((and (keywordp x) (opt-register-keyword-p x) (not (eq x dst))) (cons x acc))
    ((consp x) (%opt-collect-sexp-regs (cdr x) dst (%opt-collect-sexp-regs (car x) dst acc)))
    (t acc)))

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
               (let ((dst (opt-inst-dst inst)))
                 (handler-case
                     (%opt-collect-sexp-regs (instruction->sexp inst) dst nil)
                   (error () nil)))))))))

(defun %opt-commutative-inst-p (inst)
  "Return T if INST is a commutative binary instruction."
  (member (type-of inst) *opt-commutative-inst-types* :test #'eq))

;;; *opt-algebraic-identity-rules*, classification predicates
;;; (opt-binary-lhs-rhs-p, opt-unary-src-p, opt-foldable-unary-arith-p,
;;; opt-foldable-type-pred-p), and *opt-label-ref-table*
;;; are in optimizer-algebraic.lisp (loaded next).
