(in-package :cl-cc)

;;; ─── CL-CC Instruction Optimizer ────────────────────────────────────────
;;;
;;; Multi-pass optimizer for the CL-CC VM instruction sequence.
;;;
;;; Pipeline (two iterations for convergence):
;;;   opt-pass-fold        constant folding + algebraic simplification
;;;                        + unary folding + type predicate folding
;;;                        + constant branch elimination
;;;   opt-pass-dce         global-usedness dead code elimination (pure insts only)
;;;   opt-pass-jump        jump threading + dead jump elimination
;;;   opt-pass-unreachable remove instructions after unconditional transfers

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

;;; ─── Data-Driven Fold Tables ───────────────────────────────────────────
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
  '(vm-div vm-mod vm-rem)
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
    vm-div vm-ffloor vm-fceiling vm-ftruncate vm-fround)
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

(defun opt-inst-read-regs (inst)
  "Return a list of all register names read by INST.
   Uses *opt-binary-lhs-rhs-types* and *opt-unary-src-types* data tables
   for lhs/rhs and src classification — single source of truth."
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
      ;; Single-reg instructions
      ((eq tp 'vm-jump-zero) (list (vm-reg inst)))
      ((member tp '(vm-print vm-halt vm-ret) :test #'eq) (list (vm-reg inst)))
      ((member tp '(vm-set-global vm-register-function vm-ensure-values vm-spread-values)
               :test #'eq)
       (list (vm-src inst)))
      ;; Call-family: func/gf register + args
      ((eq tp 'vm-call)         (cons (vm-func-reg inst) (vm-args inst)))
      ((eq tp 'vm-apply)        (cons (vm-func-reg inst) (vm-args inst)))
      ((eq tp 'vm-generic-call) (cons (vm-gf-reg inst) (vm-args inst)))
      ;; Multiple values
      ((eq tp 'vm-values) (vm-src-regs inst))
      ;; Object operations
      ((eq tp 'vm-closure-ref-idx) (list (vm-closure-reg inst)))
      ((eq tp 'vm-slot-read)  (list (vm-obj-reg inst)))
      ((eq tp 'vm-slot-write) (list (vm-obj-reg inst) (vm-value-reg inst)))
      ((eq tp 'vm-register-method) (list (vm-gf-reg inst) (vm-method-reg inst)))
      ((eq tp 'vm-make-obj) (cons (vm-class-reg inst) (mapcar #'cdr (vm-initarg-regs inst))))
      ;; Compound instructions with optional registers
      ((eq tp 'vm-make-string) (remove nil (list (vm-src inst) (vm-char inst))))
      ((eq tp 'vm-intern-symbol) (remove nil (list (vm-src inst) (vm-intern-pkg inst))))
      ((eq tp 'vm-make-array) (remove nil (list (vm-size-reg inst) (vm-initial-element inst)
                                                 (vm-fill-pointer inst) (vm-adjustable inst))))
      (t
       ;; Fallback: serialize to sexp and collect all register-shaped keywords.
       ;; Every VM instruction has an instruction->sexp method; registers are
       ;; keywords named Rn (e.g. :R0, :R15).  We exclude the destination register
       ;; to avoid marking DST as "used by its own definition".
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
         regs)))))

;;; ─── Pass 1: Constant Folding + Algebraic Simplification ─────────────────

(defun opt-fold-binop-value (inst lval rval)
  "Fold binary INST with numeric constants LVAL and RVAL.
   Returns (values folded-value t) on success, or (values nil nil) if not foldable.
   Dispatch is data-driven via *opt-binary-fold-table* and *opt-binary-cmp-fold-table*."
  (let* ((tp (type-of inst))
         (arith-fn (gethash tp *opt-binary-fold-table*))
         (cmp-fn   (gethash tp *opt-binary-cmp-fold-table*)))
    (cond
      ;; Instructions that must not be folded (values-list side-channel)
      ((member tp *opt-binary-no-fold-types* :test #'eq)
       (values nil nil))
      ;; Zero-guarded arithmetic (div/mod/rem)
      ((and arith-fn (member tp *opt-binary-zero-guard-types* :test #'eq))
       (if (zerop rval)
           (values nil nil)
           (values (funcall arith-fn lval rval) t)))
      ;; Normal arithmetic fold
      (arith-fn
       (values (funcall arith-fn lval rval) t))
      ;; Comparison fold → 1/0
      (cmp-fn
       (values (if (funcall cmp-fn lval rval) 1 0) t))
      (t (values nil nil)))))

;;; Algebraic identity rules table.
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

(defun opt-simplify-binop (inst dst lhs-reg rhs-reg lval rval)
  "Algebraic simplification of binary INST via rule table lookup.
   LVAL/RVAL are known constant values or :unknown.
   Returns a simplified instruction, or NIL if no simplification applies."
  (let ((rules (gethash (type-of inst) *opt-algebraic-identity-rules*)))
    (flet ((const-p (v) (and (not (eq v :unknown)) (numberp v)))
           (apply-action (action)
             (case action
               (:move-lhs (make-vm-move :dst dst :src lhs-reg))
               (:move-rhs (make-vm-move :dst dst :src rhs-reg))
               (otherwise
                (if (consp action)
                    (case (car action)
                      (:const (make-vm-const :dst dst :value (cadr action)))
                      (:neg   (if (eq (cadr action) :lhs)
                                  (make-vm-neg :dst dst :src lhs-reg)
                                  (make-vm-neg :dst dst :src rhs-reg))))
                    nil)))))
      (dolist (rule rules nil)
        (let ((cond (car rule))
              (action (cdr rule)))
          (when (cond
                  ((eq cond :same-reg) (eq lhs-reg rhs-reg))
                  ((and (consp cond) (eq (car cond) :rconst))
                   (and (const-p rval) (eql rval (cadr cond))))
                  ((and (consp cond) (eq (car cond) :lconst))
                   (and (const-p lval) (eql lval (cadr cond)))))
            (return (apply-action action))))))))

(defun opt-pass-fold (instructions)
  "Forward pass: constant folding, algebraic simplification, constant branch elimination."
  (let ((env (make-hash-table :test #'eq)) ; reg → known constant value
        (result nil))
    (flet ((emit (inst) (push inst result))
           (emit-const (dst val)
             (setf (gethash dst env) val)
             (push (make-vm-const :dst dst :value val) result))
           (clear (reg) (remhash reg env)))
      (dolist (inst instructions)
        (typecase inst

          ;; Labels: any path may arrive here → conservatively flush constant knowledge
          (vm-label
           (clrhash env)
           (emit inst))

          ;; vm-const: record constant value in env
          (vm-const
           (setf (gethash (vm-dst inst) env) (vm-value inst))
           (emit inst))

          ;; vm-move: propagate constant if src is known; eliminate self-moves
          (vm-move
           (let ((src (vm-src inst)) (dst (vm-dst inst)))
             (cond
               ((eq src dst)) ; self-move: drop silently
               (t
                (multiple-value-bind (sval found) (gethash src env)
                  (if found
                      (emit-const dst sval)
                      (progn (clear dst) (emit inst))))))))

          ;; Binary arithmetic/comparison: full fold or algebraic simplification
          ;; Dispatch is data-driven via *opt-foldable-binary-types* (derived from fold tables).
          ;; vm-floor-inst/vm-ceiling-inst/vm-truncate are excluded (values-list side-channel).
          ((or vm-binop vm-lt vm-gt vm-le vm-ge vm-num-eq vm-eq
               vm-mod vm-rem vm-min vm-max
               vm-logand vm-logior vm-logxor vm-logeqv vm-ash)
           (let* ((dst (vm-dst inst))
                  (lhs (vm-lhs inst))
                  (rhs (vm-rhs inst)))
             (multiple-value-bind (lval lfound) (gethash lhs env)
               (multiple-value-bind (rval rfound) (gethash rhs env)
                 (cond
                   ;; Both operands are known numeric constants → full fold
                   ((and lfound rfound (numberp lval) (numberp rval))
                    (multiple-value-bind (folded ok)
                        (opt-fold-binop-value inst lval rval)
                      (if ok
                          (emit-const dst folded)
                          (progn (clear dst) (emit inst)))))
                   ;; Try algebraic identity simplification
                   (t
                    (let ((simp (opt-simplify-binop inst dst lhs rhs
                                                    (if lfound lval :unknown)
                                                    (if rfound rval :unknown))))
                      (cond
                        (simp
                         (when (vm-const-p simp)
                           (setf (gethash dst env) (vm-const-value simp)))
                         (unless (vm-const-p simp) (clear dst))
                         (emit simp))
                        (t (clear dst) (emit inst))))))))))

          ;; Unary arithmetic: data-driven via *opt-unary-fold-table*
          ((or vm-neg vm-abs vm-inc vm-dec vm-lognot vm-not)
           (let* ((dst (vm-dst inst)) (src (vm-src inst))
                  (fold-fn (gethash (type-of inst) *opt-unary-fold-table*)))
             (multiple-value-bind (sval found) (gethash src env)
               (if (and found fold-fn
                        (or (numberp sval)
                            ;; vm-not handles non-numeric values (nil → t)
                            (eq (type-of inst) 'vm-not)))
                   (emit-const dst (funcall fold-fn sval))
                   (progn (clear dst) (emit inst))))))

          ;; Type predicates: data-driven via *opt-type-pred-fold-table*
          ((or vm-cons-p vm-null-p vm-symbol-p vm-number-p
               vm-integer-p vm-function-p)
           (let* ((dst (vm-dst inst)) (src (vm-src inst))
                  (pred-fn (gethash (type-of inst) *opt-type-pred-fold-table*)))
             (multiple-value-bind (sval found) (gethash src env)
               (if (and found pred-fn)
                   (emit-const dst (if (funcall pred-fn sval) 1 0))
                   (progn (clear dst) (emit inst))))))

          ;; Constant branch folding: known-false → unconditional jump; known-true → drop
          (vm-jump-zero
           (multiple-value-bind (val found) (gethash (vm-reg inst) env)
             (if found
                 (if (opt-falsep val)
                     (emit (make-vm-jump :label (vm-label-name inst)))
                     nil) ; condition is always true → branch never taken → drop
                 (emit inst))))

          ;; Default: invalidate any written destination register
          (t
           (let ((dst (opt-inst-dst inst)))
             (when dst (clear dst)))
           (emit inst)))))
    (nreverse result)))

;;; ─── Tree Walker (shared utility) ────────────────────────────────────────

(defun opt-map-tree (fn tree)
  "Apply FN to every leaf of TREE (a possibly-improper nested cons tree)."
  (if (consp tree)
      (cons (opt-map-tree fn (car tree))
            (opt-map-tree fn (cdr tree)))
      (funcall fn tree)))

;;; ─── Pass 1b: Copy Propagation ───────────────────────────────────────────

(defun opt-pass-copy-prop (instructions)
  "Forward pass: propagate register copies so downstream uses reference the
   original register, enabling later DCE to remove the now-redundant moves.
   At labels, conservatively flush copy knowledge (multiple paths may arrive)."
  (let ((copies (make-hash-table :test #'eq)) ; reg → canonical reg
        (result nil))
    (flet ((canonical (reg)
             ;; Follow the copy chain to its root
             (loop for r = reg then (gethash r copies)
                   while (gethash r copies)
                   finally (return r)))
           (kill (reg)
             ;; When REG is written, invalidate: its own entry AND any entry
             ;; pointing to REG (forward aliases of REG are now stale)
             (remhash reg copies)
             (maphash (lambda (k v) (when (eq v reg) (remhash k copies)))
                      copies)))
      (dolist (inst instructions)
        (typecase inst
          ;; Labels: flush all copy knowledge
          (vm-label
           (clrhash copies)
           (push inst result))
          ;; vm-move: record copy relationship (after resolving canonical)
          (vm-move
           (let* ((src (canonical (vm-move-src inst)))
                  (dst (vm-move-dst inst)))
             (cond
               ((eq src dst) nil) ; self-move after resolution: drop
               (t
                (kill dst)
                (setf (gethash dst copies) src)
                ;; Rewrite src to canonical in the emitted instruction
                (push (if (eq src (vm-move-src inst))
                          inst
                          (make-vm-move :dst dst :src src))
                      result)))))
          ;; For all other instructions: rewrite read registers to canonical,
          ;; then kill any written register
          (t
           (let* ((rewritten (opt-rewrite-inst-regs inst copies))
                  (dst (opt-inst-dst rewritten)))
             (when dst (kill dst))
             (push rewritten result)))))
      (nreverse result))))

(defun opt-rewrite-inst-regs (inst copies)
  "Return INST with all source registers replaced by their canonical copies.
   Uses sexp roundtrip: instruction->sexp rewrites all register-keyword leaves
   except the destination slot (position 1 for instructions with a dst), then
   reconstructs via sexp->instruction.  Falls back to INST unchanged on error."
  (flet ((c (x) (if (opt-register-keyword-p x) (or (gethash x copies) x) x)))
    (handler-case
        (let* ((sexp      (instruction->sexp inst))
               (has-dst   (not (null (opt-inst-dst inst))))
               ;; Rewrite all leaves; for instructions with a dst, the dst sits at
               ;; position 1 (immediately after the opcode tag) — leave it intact.
               (new-sexp  (if has-dst
                              (list* (first sexp) (second sexp)
                                     (opt-map-tree #'c (cddr sexp)))
                              (cons  (first sexp)
                                     (opt-map-tree #'c (cdr sexp))))))
          (if (equal sexp new-sexp) inst (sexp->instruction new-sexp)))
      (error () inst))))

;;; ─── Pass 2: Dead Code Elimination ──────────────────────────────────────

(defun opt-pass-dce (instructions)
  "Dead code elimination via global usedness analysis.
   Pass 1: collect every register that appears as a source operand anywhere
   in the program into a 'used' set (ignoring control flow).
   Pass 2: remove pure instructions (vm-const, vm-move) whose destination
   register never appears in the used set.
   This is safe across branches/labels: a register defined in both branches
   is preserved as long as it is read anywhere -- the linear-order issue that
   plagued the previous backward-liveness DCE is entirely avoided."
  (let ((used (make-hash-table :test #'eq)))
    ;; Pass 1: mark every register that is read by any instruction
    (dolist (inst instructions)
      (dolist (reg (opt-inst-read-regs inst))
        (setf (gethash reg used) t)))
    ;; Pass 2: drop DCE-eligible instructions (pure + alloc) whose dst is never read
    (remove-if (lambda (inst)
                 (and (opt-inst-dce-eligible-p inst)
                      (let ((dst (ignore-errors (vm-dst inst))))
                        (and dst (not (gethash dst used))))))
               instructions)))

;;; ─── Pass 3: Jump Threading + Dead Jump Elimination ─────────────────────

(defun opt-build-label-index (instructions)
  "Return (values vector label→index-ht) for threading analysis."
  (let ((vec (coerce instructions 'vector))
        (idx (make-hash-table :test #'equal)))
    (loop for i from 0 below (length vec)
          when (vm-label-p (aref vec i))
          do (setf (gethash (vm-name (aref vec i)) idx) i))
    (values vec idx)))

(defun opt-thread-label (label idx vec &optional (depth 0))
  "Follow jump chains starting at LABEL. Returns the ultimate jump target label.
   DEPTH guards against infinite loops in pathological (cyclic) code."
  (when (> depth 20) (return-from opt-thread-label label))
  (let ((pos (gethash label idx)))
    (unless pos (return-from opt-thread-label label))
    ;; Scan forward past any labels to find the first real instruction
    (loop for i from pos below (length vec)
          for inst = (aref vec i)
          do (typecase inst
               (vm-label nil) ; skip label markers
               (vm-jump ; found a chained jump → follow it
                (let ((next (vm-label-name inst)))
                  (return-from opt-thread-label
                    (opt-thread-label next idx vec (1+ depth)))))
               (t (return-from opt-thread-label label)))))
  label)

(defun opt-falls-through-to-p (vec i target)
  "T if scanning forward from position I+1 we reach TARGET before any non-label."
  (loop for j from (1+ i) below (length vec)
        for inst = (aref vec j)
        if (not (vm-label-p inst)) return nil
        if (equal (vm-name inst) target) return t
        finally (return nil)))

(defun opt-pass-jump (instructions)
  "Thread jump chains and remove jumps to the immediately following label."
  (multiple-value-bind (vec idx) (opt-build-label-index instructions)
    (let ((result nil)
          (n (length vec)))
      (loop for i from 0 below n
            for inst = (aref vec i)
            do (typecase inst
                 (vm-jump
                  (let ((threaded (opt-thread-label (vm-label-name inst) idx vec)))
                    (unless (opt-falls-through-to-p vec i threaded)
                      (push (if (equal threaded (vm-label-name inst))
                                inst
                                (make-vm-jump :label threaded))
                            result))))
                 (vm-jump-zero
                  (let ((threaded (opt-thread-label (vm-label-name inst) idx vec)))
                    (push (if (equal threaded (vm-label-name inst))
                              inst
                              (make-vm-jump-zero :reg (vm-reg inst) :label threaded))
                          result)))
                 (t (push inst result))))
      (nreverse result))))

;;; ─── Pass 4: Unreachable Code Elimination ────────────────────────────────

(defun opt-pass-unreachable (instructions)
  "Remove instructions that follow unconditional control transfers (jump/ret)
   and precede the next label — they can never be executed."
  (let ((result nil)
        (dead nil))
    (dolist (inst instructions)
      (typecase inst
        (vm-label
         (setf dead nil)
         (push inst result))
        (t
         (unless dead
           (push inst result))
         ;; Mark subsequent instructions as unreachable after unconditional transfer
         (when (or (vm-jump-p inst) (vm-ret-p inst))
           (setf dead t)))))
    (nreverse result)))

;;; ─── Pass 0: Function Inlining ───────────────────────────────────────────
;;;
;;; Inlines small, non-capturing, linear functions at call sites.
;;; Eligibility: vm-closure with no captured vars, body ≤ threshold insts,
;;;              no internal jumps (linear body ending in vm-ret).
;;;
;;; Algorithm:
;;;   1. Scan once to map label → (params, linear-body).
;;;   2. Forward scan: when a known small function's register is called,
;;;      replace vm-call with: arg-moves + renamed body + result-move.
;;;   Register renaming uses fresh :R<N> indices above the program's max.

(defun opt-max-reg-index (instructions)
  "Return the maximum integer N where :RN is used in INSTRUCTIONS, or -1."
  (let ((max-idx -1))
    (dolist (inst instructions max-idx)
      (dolist (reg (cons (opt-inst-dst inst) (opt-inst-read-regs inst)))
        (when (and reg (opt-register-keyword-p reg))
          (let* ((name (symbol-name reg))
                 (idx (ignore-errors (parse-integer name :start 1))))
            (when (and idx (> idx max-idx))
              (setf max-idx idx))))))))

(defun opt-make-renaming (body-instructions base-index)
  "Build renaming table: existing-register → fresh :R<N> (starting at BASE-INDEX).
   Uses opt-inst-dst + opt-inst-read-regs to discover ALL registers, including
   those not serialized by instruction->sexp (e.g., vm-make-obj initarg-regs)."
  (let ((seen (make-hash-table :test #'eq))
        (counter base-index)
        (renaming (make-hash-table :test #'eq)))
    (flet ((add (r)
             (when (and r (opt-register-keyword-p r))
               (unless (gethash r seen)
                 (setf (gethash r seen) t)
                 (setf (gethash r renaming)
                       (intern (format nil "R~A" counter) :keyword))
                 (incf counter)))))
      (dolist (inst body-instructions)
        (add (opt-inst-dst inst))
        (dolist (r (opt-inst-read-regs inst)) (add r))))
    renaming))

(defun opt-can-safely-rename-p (body-instructions)
  "T if all instructions in BODY can be safely register-renamed via sexp roundtrip.
   An instruction is safe when instruction->sexp captures all its registers —
   i.e., opt-inst-read-regs reports the same registers as appear in the sexp.
   Instructions with custom sexp methods (vm-make-obj, vm-slot-read, etc.) that
   omit registers from their sexp representation will cause this to return NIL."
  (dolist (inst body-instructions t)
    (let ((explicit-regs (remove nil
                                 (cons (opt-inst-dst inst)
                                       (opt-inst-read-regs inst))))
          (sexp-regs nil))
      (labels ((visit (x)
                 (cond ((and (keywordp x) (opt-register-keyword-p x)) (push x sexp-regs))
                       ((consp x) (visit (car x)) (visit (cdr x))))))
        (handler-case (visit (instruction->sexp inst))
          (error () (return-from opt-can-safely-rename-p nil))))
      (unless (every (lambda (r) (member r sexp-regs)) explicit-regs)
        (return-from opt-can-safely-rename-p nil)))))

(defun opt-rename-regs-in-inst (inst renaming)
  "Return INST with all VM register keywords substituted per RENAMING.
   Uses instruction→sexp roundtrip; returns INST unchanged on any error."
  (flet ((sub (x)
           (if (and (keywordp x) (opt-register-keyword-p x))
               (or (gethash x renaming) x)
               x)))
    (handler-case
        (sexp->instruction (opt-map-tree #'sub (instruction->sexp inst)))
      (error () inst))))

(defun opt-collect-function-defs (instructions)
  "Return hash-table label → (:params params :body body-insts).
   Only includes functions that are:
   - Registered via vm-closure with known params
   - Linear: no internal jumps; body ends with exactly one vm-ret"
  (let ((label-to-params (make-hash-table :test #'equal))
        (label-to-body   (make-hash-table :test #'equal))
        (in-fn nil) (cur-label nil) (cur-body nil) (has-jump nil))
    ;; Collect params from vm-closure instructions
    (dolist (inst instructions)
      (when (and (vm-closure-p inst) (vm-closure-params inst))
        (setf (gethash (vm-label-name inst) label-to-params)
              (vm-closure-params inst))))
    ;; Collect linear bodies (label → instructions ending in vm-ret)
    (dolist (inst instructions)
      (typecase inst
        (vm-label
         ;; Nested label: abandon any in-progress body (non-linear)
         (when in-fn
           (setf in-fn nil cur-label nil cur-body nil has-jump nil))
         (setf in-fn t cur-label (vm-name inst) cur-body nil has-jump nil))
        ((or vm-jump vm-jump-zero)
         (setf has-jump t)
         (when in-fn (push inst cur-body)))
        (vm-ret
         (when (and in-fn (not has-jump))
           (push inst cur-body)
           (setf (gethash cur-label label-to-body) (nreverse cur-body)))
         (setf in-fn nil cur-label nil cur-body nil has-jump nil))
        (t (when in-fn (push inst cur-body)))))
    ;; Combine: label must appear in both tables
    ;; Also build reverse map: label → the vm-closure instruction (for captured-var check)
    (let ((label-to-closure (make-hash-table :test #'equal))
          (result (make-hash-table :test #'equal)))
      (dolist (inst instructions)
        (when (vm-closure-p inst)
          (setf (gethash (vm-label-name inst) label-to-closure) inst)))
      (maphash (lambda (lbl params)
                 (let ((body    (gethash lbl label-to-body))
                       (closure (gethash lbl label-to-closure)))
                   (when (and body closure)
                     (setf (gethash lbl result)
                           (list :closure closure :params params :body body)))))
               label-to-params)
      result)))

(defun opt-body-has-global-refs-p (body-instructions params)
  "Return T if BODY-INSTRUCTIONS read any register that is neither in PARAMS
   nor defined as a DST by a prior body instruction.  Such 'global registers'
   (e.g., class descriptors set by defclass at the top level) would be renamed
   to fresh uninitialized registers if the function were inlined, breaking
   correctness.  Functions with global refs must not be inlined."
  (let ((safe (make-hash-table :test #'eq)))
    (dolist (p params) (setf (gethash p safe) t))
    (dolist (inst body-instructions nil)
      (dolist (r (opt-inst-read-regs inst))
        (unless (gethash r safe)
          (return-from opt-body-has-global-refs-p t)))
      (let ((dst (opt-inst-dst inst)))
        (when dst (setf (gethash dst safe) t))))))

(defun opt-pass-inline (instructions &key (threshold 15))
  "Replace vm-call of a known small function with the inlined body.
   The function must be: captured-var-free, linear, and have ≤ THRESHOLD
   body instructions (not counting the final vm-ret)."
  (let* ((func-defs (opt-collect-function-defs instructions))
         (base-idx  (1+ (opt-max-reg-index instructions)))
         (reg-track (make-hash-table :test #'eq)) ; reg → label of known function
         ;; Build name→label mapping from vm-register-function instructions
         (name-to-label (let ((ht (make-hash-table :test #'eq)))
                          (dolist (inst instructions ht)
                            (when (vm-register-function-p inst)
                              (let ((label (gethash (vm-src inst) reg-track)))
                                ;; reg-track may not be populated yet; scan closures first
                                (when (null label)
                                  ;; Find the closure that the register came from
                                  (dolist (i instructions)
                                    (when (and (vm-closure-p i)
                                               (eq (vm-dst i) (vm-src inst)))
                                      (setf label (vm-label-name i))
                                      (return))))
                                (when label
                                  (setf (gethash (vm-func-name inst) ht) label)))))))
         (result nil))
    (dolist (inst instructions)
      (typecase inst
        ;; Record which register now holds which function
        ((or vm-closure vm-func-ref)
         (let ((label (vm-label-name inst)))
           (when (gethash label func-defs)
             (setf (gethash (vm-dst inst) reg-track) label)))
         (push inst result))
        ;; Track vm-const loading a function name symbol
        (vm-const
         (let* ((val (vm-value inst))
                (label (when (symbolp val) (gethash val name-to-label))))
           (when (and label (gethash label func-defs))
             (setf (gethash (vm-dst inst) reg-track) label)))
         (push inst result))
        ;; Attempt inlining
        (vm-call
         (let* ((label  (gethash (vm-func-reg inst) reg-track))
                (def    (and label (gethash label func-defs))))
           (if (and def
                    ;; Only inline zero-capture, required-only-param closures
                    (let ((ci (getf def :closure)))
                      (and (vm-closure-p ci)
                           (null (vm-captured-vars ci))
                           ;; Must not have optional/rest/key params — those require
                           ;; runtime argument-count processing that can't be inlined
                           (null (vm-closure-optional-params ci))
                           (null (vm-closure-rest-param ci))
                           (null (vm-closure-key-params ci))))
                    (<= (1- (length (getf def :body))) threshold)
                    ;; All body instructions must support lossless register renaming
                    ;; via instruction->sexp roundtrip (guards against vm-make-obj etc.)
                    (opt-can-safely-rename-p (getf def :body))
                    ;; No implicit global register refs — only params + body-defined
                    ;; registers may appear as sources.  Renaming a global class or
                    ;; function register would give a fresh (0-valued) register.
                    (not (opt-body-has-global-refs-p
                           (getf def :body)
                           (vm-closure-params (getf def :closure)))))
               ;; ── Inline ──
               (let* ((body    (getf def :body))
                      (ci      (getf def :closure))
                      (params  (vm-closure-params ci))
                      (rename  (opt-make-renaming body base-idx)))
                 (incf base-idx (hash-table-count rename))
                 ;; 1. Move call arguments into renamed parameter registers
                 (loop for param in params
                       for arg  in (vm-args inst)
                       do (push (make-vm-move :dst (or (gethash param rename) param)
                                              :src arg)
                                result))
                 ;; 2. Emit renamed body (all but the last vm-ret)
                 (dolist (bi (butlast body))
                   (push (opt-rename-regs-in-inst bi rename) result))
                 ;; 3. Move renamed return register to call's dst
                 (let* ((ret-inst (car (last body)))
                        (ret-src  (or (gethash (vm-reg ret-inst) rename)
                                      (vm-reg ret-inst))))
                   (push (make-vm-move :dst (vm-dst inst) :src ret-src) result))
                 ;; The func-reg binding remains in reg-track (safe: reg still points to same fn)
                 )
               ;; ── Keep call as-is ──
               (push inst result))))
        ;; Invalidate tracking for any overwritten register
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst (remhash dst reg-track)))
         (push inst result))))
    (nreverse result)))

;;; ─── Pass: Strength Reduction ────────────────────────────────────────────

(defun opt-power-of-2-p (n)
  "T if N is a positive integer that is a power of 2 (>= 2)."
  (and (integerp n) (>= n 2) (zerop (logand n (1- n)))))

(defun opt-pass-strength-reduce (instructions)
  "Forward pass: replace multiply/divide by powers of 2 with arithmetic shifts.
   - (* x 2^k) → (ash x k)
   - (* 2^k x) → (ash x k)   [commutative]
   - (/ x 2^k) → (ash x -k)  [floor semantics: (floor x 2^k) = (ash x -k)]
   At vm-label boundaries, flush the constant environment."
  (let* ((env     (make-hash-table :test #'eq))
         (base    (1+ (opt-max-reg-index instructions)))
         (counter base)
         (result  nil))
    (flet ((new-shift-reg ()
             (prog1 (intern (format nil "R~A" counter) :keyword)
               (incf counter)))
           (const-val (reg) (gethash reg env))
           (emit (i) (push i result)))
      (dolist (inst instructions)
        (typecase inst
          (vm-label
           (clrhash env)
           (emit inst))
          (vm-const
           (setf (gethash (vm-dst inst) env) (vm-value inst))
           (emit inst))
          (vm-mul
           (let* ((dst (vm-dst inst))
                  (lhs (vm-lhs inst))
                  (rhs (vm-rhs inst))
                  (rv  (const-val rhs))
                  (lv  (const-val lhs)))
             (cond
               ((and rv (opt-power-of-2-p rv))
                (let* ((k         (1- (integer-length rv)))
                       (shift-reg (new-shift-reg)))
                  (emit (make-vm-const :dst shift-reg :value k))
                  (emit (make-vm-ash   :dst dst :lhs lhs :rhs shift-reg))))
               ((and lv (opt-power-of-2-p lv))
                (let* ((k         (1- (integer-length lv)))
                       (shift-reg (new-shift-reg)))
                  (emit (make-vm-const :dst shift-reg :value k))
                  (emit (make-vm-ash   :dst dst :lhs rhs :rhs shift-reg))))
               (t (emit inst)))))
          (t
           (let ((dst (opt-inst-dst inst)))
             (when dst (remhash dst env)))
           (emit inst)))))
    (nreverse result)))

;;; ─── Pass: Common Subexpression Elimination ──────────────────────────────

(defun opt-pass-cse (instructions)
  "Common subexpression elimination via generation-numbered value numbering.
   Replaces redundant computations with vm-move from the first computing register.
   At vm-label boundaries, all knowledge is conservatively flushed."
  (let ((gen      (make-hash-table :test #'eq))    ; reg → generation count
        (val-env  (make-hash-table :test #'eq))    ; reg → canonical-value
        (memo     (make-hash-table :test #'equal)) ; canonical-value → canonical-reg
        (result   nil))
    (labels ((get-val (reg)
               ;; Canonical value: from val-env if known, else (reg . gen)
               (or (gethash reg val-env)
                   (cons reg (or (gethash reg gen) 0))))
             (bump-gen (dst)
               ;; Overwriting dst: remove stale memo entry if dst was canonical
               (let ((old-val (gethash dst val-env)))
                 (when old-val
                   (when (eq (gethash old-val memo) dst)
                     (remhash old-val memo))
                   (remhash dst val-env)))
               (setf (gethash dst gen) (1+ (or (gethash dst gen) 0))))
             (record (dst key)
               (setf (gethash dst val-env) key)
               (unless (gethash key memo)
                 (setf (gethash key memo) dst)))
             (val< (a b)
               (string< (format nil "~S" a) (format nil "~S" b)))
             (commutative-p (inst)
               (member (type-of inst) *opt-commutative-inst-types* :test #'eq))
             (try-cse (key)
               ;; Return existing reg if key is memoized, else nil
               (gethash key memo))
             (emit-or-cse (inst dst key)
               (let ((existing (try-cse key)))
                 (if existing
                     (progn
                       (bump-gen dst)
                       (setf (gethash dst val-env) key)
                       (push (make-vm-move :dst dst :src existing) result))
                     (progn
                       (bump-gen dst)
                       (record dst key)
                       (push inst result))))))
      (dolist (inst instructions)
        (typecase inst
          (vm-label
           (clrhash gen) (clrhash val-env) (clrhash memo)
           (push inst result))
          (vm-const
           ;; Never replace vm-const with vm-move: doing so creates a
           ;; fold<->CSE oscillation that DCE can turn into a dangling
           ;; register reference (first canonical reg gets removed by
           ;; DCE while later moves still point to it, leaving the
           ;; register uninitialized = 0 instead of NIL).
           ;; The fold pass already propagates constant values; CSE is
           ;; only needed for computed expressions.
           (let ((dst (vm-dst inst))
                 (key (list :const (vm-value inst))))
             (bump-gen dst)
             (record dst key)
             (push inst result)))
          (vm-move
           (let* ((dst     (vm-move-dst inst))
                  (src-val (get-val (vm-move-src inst))))
             (bump-gen dst)
             (setf (gethash dst val-env) src-val)
             (push inst result)))
          ;; CSE for binary ops: data-driven via *opt-foldable-binary-types*
          ((or vm-binop vm-lt vm-gt vm-le vm-ge vm-num-eq vm-eq
               vm-mod vm-rem vm-min vm-max
               vm-logand vm-logior vm-logxor vm-logeqv vm-ash)
           (let* ((dst (vm-dst inst))
                  (lv  (get-val (vm-lhs inst)))
                  (rv  (get-val (vm-rhs inst)))
                  (op  (type-of inst))
                  (key (if (commutative-p inst)
                           (list op (if (val< lv rv) lv rv)
                                    (if (val< lv rv) rv lv))
                           (list op lv rv))))
             (emit-or-cse inst dst key)))
          ;; CSE for unary ops: data-driven via *opt-foldable-unary-types*
          ((or vm-neg vm-abs vm-inc vm-dec vm-lognot vm-not
               vm-cons-p vm-null-p vm-symbol-p vm-number-p
               vm-integer-p vm-function-p)
           (let* ((dst (vm-dst inst))
                  (sv  (get-val (vm-src inst)))
                  (key (list (type-of inst) sv)))
             (emit-or-cse inst dst key)))
          (t
           (let ((dst (opt-inst-dst inst)))
             (when dst (bump-gen dst)))
           (push inst result)))))
    (nreverse result)))

;;; ─── Unused Label Elimination ────────────────────────────────────────────

(defun opt-pass-dead-labels (instructions)
  "Remove labels that are not referenced by any jump or closure instruction."
  (let ((used (make-hash-table :test #'equal)))
    ;; Collect all label references: jumps AND closure body labels
    (dolist (inst instructions)
      (typecase inst
        (vm-jump              (setf (gethash (vm-label-name inst) used) t))
        (vm-jump-zero         (setf (gethash (vm-label-name inst) used) t))
        (vm-closure           (setf (gethash (vm-label-name inst) used) t))
        (vm-make-closure      (setf (gethash (vm-label-name inst) used) t))
        (vm-func-ref          (setf (gethash (vm-label-name inst) used) t))
        ;; Handler labels must be kept — vm-signal-error branches to them at runtime
        (vm-establish-handler (setf (gethash (vm-handler-label inst) used) t))))
    ;; Filter out unreferenced labels
    (remove-if (lambda (inst)
                 (and (vm-label-p inst)
                      (not (gethash (vm-name inst) used))))
               instructions)))

;;; ─── Top-Level Optimizer ─────────────────────────────────────────────────

(defparameter *opt-convergence-passes*
  (list #'opt-pass-fold
        #'opt-pass-strength-reduce
        #'opt-pass-copy-prop
        #'opt-pass-cse
        #'opt-pass-jump
        #'opt-pass-dead-labels
        #'opt-pass-unreachable
        #'opt-pass-dce)
  "Ordered list of passes run to convergence in optimize-instructions.
   Each pass is a function (instructions) -> instructions.
   Add new passes here; the convergence loop requires no other changes.")

(defun opt-run-passes-once (prog)
  "Apply every convergence pass in *opt-convergence-passes* once, left to right."
  (reduce (lambda (p f) (funcall f p)) *opt-convergence-passes* :initial-value prog))

(defun opt-converged-p (prev next)
  "T if a pass-cycle produced no change (same length and all instructions eq)."
  (and (= (length prev) (length next))
       (every #'eq prev next)))

(defun optimize-instructions (instructions &key (max-iterations 20))
  "Run the full multi-pass optimization pipeline on a VM instruction sequence.
   Runs until no changes or MAX-ITERATIONS reached."
  (let ((prog (opt-pass-inline instructions :threshold 15)))
    (loop for _ from 0 below max-iterations
          for prev = prog
          do (setf prog (opt-run-passes-once prog))
          when (opt-converged-p prev prog)
          return prog)
    (if *enable-prolog-peephole*
        (mapcar #'sexp->instruction
                (apply-prolog-peephole (mapcar #'instruction->sexp prog)))
        prog)))
