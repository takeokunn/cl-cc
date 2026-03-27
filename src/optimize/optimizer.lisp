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

          ;; Labels: any path may arrive here → conservatively flush constant knowledge
          ((typep inst 'vm-label)
           (clrhash env)
           (emit inst))

          ;; vm-const: record constant value in env
          ((typep inst 'vm-const)
           (setf (gethash (vm-dst inst) env) (vm-value inst))
           (emit inst))

          ;; vm-move: propagate constant if src is known; eliminate self-moves
          ((typep inst 'vm-move)
           (let ((src (vm-src inst)) (dst (vm-dst inst)))
             (cond
               ((eq src dst)) ; self-move: drop silently
               (t
                (multiple-value-bind (sval found) (gethash src env)
                  (if found
                      (emit-const dst sval)
                      (progn (clear dst) (emit inst))))))))

          ;; Binary arithmetic/comparison: full fold or algebraic simplification.
          ;; Dispatch derived from opt-binary-lhs-rhs-p (covers *opt-binary-lhs-rhs-types*).
          ;; vm-floor-inst/vm-ceiling-inst/vm-truncate are excluded (values-list side-channel).
          ((opt-binary-lhs-rhs-p inst)
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

          ;; Unary arithmetic: data-driven via opt-foldable-unary-arith-p → *opt-unary-fold-table*
          ((opt-foldable-unary-arith-p inst)
           (let* ((dst (vm-dst inst)) (src (vm-src inst))
                  (fold-fn (gethash (type-of inst) *opt-unary-fold-table*)))
             (multiple-value-bind (sval found) (gethash src env)
               (if (and found fold-fn
                        (or (numberp sval)
                            ;; vm-not handles non-numeric values (nil → t)
                            (eq (type-of inst) 'vm-not)))
                   (emit-const dst (funcall fold-fn sval))
                   (progn (clear dst) (emit inst))))))

          ;; Type predicates: data-driven via opt-foldable-type-pred-p → *opt-type-pred-fold-table*
          ((opt-foldable-type-pred-p inst)
           (let* ((dst (vm-dst inst)) (src (vm-src inst))
                  (pred-fn (gethash (type-of inst) *opt-type-pred-fold-table*)))
             (multiple-value-bind (sval found) (gethash src env)
               (if (and found pred-fn)
                   (emit-const dst (if (funcall pred-fn sval) 1 0))
                   (progn (clear dst) (emit inst))))))

          ;; Constant branch folding: known-false → unconditional jump; known-true → drop
          ((typep inst 'vm-jump-zero)
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

(defun opt-inline-eligible-p (def threshold)
  "Return T if the function definition DEF (a plist from opt-collect-function-defs)
   satisfies all inlining preconditions:
   1. Has a vm-closure with zero captured variables
   2. Has only required params (no &optional/&rest/&key)
   3. Body size ≤ THRESHOLD (excluding the final vm-ret)
   4. All body instructions support lossless register renaming
   5. Body reads no 'global' registers (ones not defined by params or body)"
  (let ((ci   (getf def :closure))
        (body (getf def :body)))
    (and (vm-closure-p ci)
         (null (vm-captured-vars ci))
         (null (vm-closure-optional-params ci))
         (null (vm-closure-rest-param ci))
         (null (vm-closure-key-params ci))
         (<= (1- (length body)) threshold)
         (opt-can-safely-rename-p body)
         (not (opt-body-has-global-refs-p body (vm-closure-params ci))))))

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
           (if (and def (opt-inline-eligible-p def threshold))
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
