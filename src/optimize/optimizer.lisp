(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer — Pass Implementations
;;;
;;; All optimization passes over the VM instruction sequence.
;;; Data tables and derived predicates live in optimizer-tables.lisp.
;;;
;;; Pipeline (two iterations for convergence):
;;;   opt-pass-fold        constant folding + algebraic simplification
;;;                        + unary folding + type predicate folding
;;;                        + constant branch elimination
;;;   opt-pass-dce         global-usedness dead code elimination (pure insts only)
;;;   opt-pass-jump        jump threading + dead jump elimination
;;;   opt-pass-unreachable remove instructions after unconditional transfers
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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

(defun %fold-vm-label (inst env emit)
  "Flush all constant knowledge at a label: any path may arrive here."
  (clrhash env)
  (funcall emit inst))

(defun %fold-vm-const (inst env emit)
  "Record the constant value of INST's destination in ENV, then emit."
  (setf (gethash (vm-dst inst) env) (vm-value inst))
  (funcall emit inst))

(defun %fold-vm-move (inst env emit emit-const clear)
  "Propagate constant if src is known in ENV; eliminate self-moves silently."
  (let ((src (vm-src inst)) (dst (vm-dst inst)))
    (cond
      ((eq src dst)) ; self-move: drop silently
      (t
       (multiple-value-bind (sval found) (gethash src env)
         (if found
             (funcall emit-const dst sval)
             (progn (funcall clear dst) (funcall emit inst))))))))

(defun %fold-binary-inst (inst env emit emit-const clear)
  "Binary arithmetic/comparison: full fold or algebraic simplification.
   Dispatch derived from opt-binary-lhs-rhs-p (covers *opt-binary-lhs-rhs-types*).
   vm-floor-inst/vm-ceiling-inst/vm-truncate are excluded (values-list side-channel)."
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
                 (funcall emit-const dst folded)
                 (progn (funcall clear dst) (funcall emit inst)))))
          ;; Try algebraic identity simplification
          (t
           (let ((simp (opt-simplify-binop inst dst lhs rhs
                                           (if lfound lval :unknown)
                                           (if rfound rval :unknown))))
             (cond
               (simp
                (when (vm-const-p simp)
                  (setf (gethash dst env) (vm-const-value simp)))
                (unless (vm-const-p simp) (funcall clear dst))
                (funcall emit simp))
               (t (funcall clear dst) (funcall emit inst))))))))))

(defun %fold-unary-inst (inst env emit emit-const clear)
  "Unary arithmetic: data-driven via opt-foldable-unary-arith-p → *opt-unary-fold-table*."
  (let* ((dst (vm-dst inst)) (src (vm-src inst))
         (fold-fn (gethash (type-of inst) *opt-unary-fold-table*)))
    (multiple-value-bind (sval found) (gethash src env)
      (if (and found fold-fn
               (or (numberp sval)
                   ;; vm-not handles non-numeric values (nil → t)
                   (eq (type-of inst) 'vm-not)))
          (funcall emit-const dst (funcall fold-fn sval))
          (progn (funcall clear dst) (funcall emit inst))))))

(defun %fold-type-pred-inst (inst env emit emit-const clear)
  "Type predicates: data-driven via opt-foldable-type-pred-p → *opt-type-pred-fold-table*."
  (let* ((dst (vm-dst inst)) (src (vm-src inst))
         (pred-fn (gethash (type-of inst) *opt-type-pred-fold-table*)))
    (multiple-value-bind (sval found) (gethash src env)
      (if (and found pred-fn)
          (funcall emit-const dst (if (funcall pred-fn sval) 1 0))
          (progn (funcall clear dst) (funcall emit inst))))))

(defun %fold-vm-jump-zero (inst env emit)
  "Constant branch folding: known-false → unconditional jump; known-true → drop."
  (multiple-value-bind (val found) (gethash (vm-reg inst) env)
    (if found
        (if (opt-falsep val)
            (funcall emit (make-vm-jump :label (vm-label-name inst)))
            nil) ; condition is always true → branch never taken → drop
        (funcall emit inst))))

(defun %fold-default-inst (inst env emit clear)
  "Default: invalidate any written destination register, then emit."
  (declare (ignore env))
  (let ((dst (opt-inst-dst inst)))
    (when dst (funcall clear dst)))
  (funcall emit inst))

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
        (cond
          ((typep inst 'vm-label)         (%fold-vm-label      inst env #'emit))
          ((typep inst 'vm-const)         (%fold-vm-const      inst env #'emit))
          ((typep inst 'vm-move)          (%fold-vm-move       inst env #'emit #'emit-const #'clear))
          ((opt-binary-lhs-rhs-p inst)    (%fold-binary-inst   inst env #'emit #'emit-const #'clear))
          ((opt-foldable-unary-arith-p inst) (%fold-unary-inst inst env #'emit #'emit-const #'clear))
          ((opt-foldable-type-pred-p inst)(%fold-type-pred-inst inst env #'emit #'emit-const #'clear))
          ((typep inst 'vm-jump-zero)     (%fold-vm-jump-zero  inst env #'emit))
          (t                              (%fold-default-inst  inst env #'emit #'clear)))))
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
        (cond
          ((typep inst 'vm-label)
           (clrhash gen) (clrhash val-env) (clrhash memo)
           (push inst result))
          ((typep inst 'vm-const)
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
          ((typep inst 'vm-move)
           (let* ((dst     (vm-move-dst inst))
                  (src-val (get-val (vm-move-src inst))))
             (bump-gen dst)
             (setf (gethash dst val-env) src-val)
             (push inst result)))
          ;; CSE for binary ops: derived from opt-binary-lhs-rhs-p
          ((opt-binary-lhs-rhs-p inst)
           (let* ((dst (vm-dst inst))
                  (lv  (get-val (vm-lhs inst)))
                  (rv  (get-val (vm-rhs inst)))
                  (op  (type-of inst))
                  (key (if (commutative-p inst)
                           (list op (if (val< lv rv) lv rv)
                                    (if (val< lv rv) rv lv))
                           (list op lv rv))))
             (emit-or-cse inst dst key)))
          ;; CSE for unary ops + type predicates: derived from opt-unary-src-p
          ((opt-unary-src-p inst)
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
      (let ((accessor (gethash (type-of inst) *opt-label-ref-table*)))
        (when accessor
          (setf (gethash (funcall accessor inst) used) t))))
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
