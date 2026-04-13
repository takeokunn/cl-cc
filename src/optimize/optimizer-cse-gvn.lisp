(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer — CSE, GVN, Dead Label Elimination, Leaf Function Detection
;;;
;;; Contains: opt-pass-cse (common subexpression elimination via generation-
;;; numbered value numbering), %gvn-process-block / opt-pass-gvn (global
;;; value numbering over the dominator tree), opt-pass-dead-labels (removes
;;; unreferenced labels), *opt-leaf-call-types* + opt-pass-leaf-detect.
;;;
;;; Load order: after optimizer-strength.lisp, before optimizer-licm.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── Pass: Common Subexpression Elimination ──────────────────────────────

(defun opt-pass-cse (instructions)
  "Common subexpression elimination via generation-numbered value numbering.
   Replaces redundant computations with vm-move from the first computing register.
   At vm-label boundaries, all knowledge is conservatively flushed."
  (let ((gen      (make-hash-table :test #'eq))    ; reg → generation count
        (val-env  (make-hash-table :test #'eq))    ; reg → canonical-value
        (memo     (make-hash-table :test #'equal)) ; canonical-value → canonical-reg
        (target-labels (%opt-branch-target-labels instructions))
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
            (when (gethash (vm-name inst) target-labels)
              (clrhash gen) (clrhash val-env) (clrhash memo))
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
                           (list op (if (%opt-value< lv rv) lv rv)
                                    (if (%opt-value< lv rv) rv lv))
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

;;; ─── Pass: Global Value Numbering ────────────────────────────────────────

(defun %gvn-process-block (block gen val-env memo)
  "Process BLOCK for global value numbering using GEN/VAL-ENV/MEMO state."
  (labels ((copy-env (env &key (test #'eq))
            (let ((copy (make-hash-table :test test)))
             (maphash (lambda (k v) (setf (gethash k copy) v)) env)
             copy))
         (kill (reg gen val-env memo)
           (remhash reg val-env)
           (maphash (lambda (k v)
                      (when (eq v reg)
                        (remhash k memo)))
                    memo)
           (setf (gethash reg gen) (1+ (or (gethash reg gen) 0))))
         (commutative-p (inst)
            (member (type-of inst) *opt-commutative-inst-types* :test #'eq))
         (get-val (reg gen val-env)
           (or (gethash reg val-env)
               (cons reg (or (gethash reg gen) 0))))
         (record (dst key gen val-env memo)
           (setf (gethash dst val-env) key)
           (unless (gethash key memo)
             (setf (gethash key memo) dst))
           (setf (gethash dst gen) (1+ (or (gethash dst gen) 0))))
         (gvn-key (inst gen val-env)
           (let ((reads (opt-inst-read-regs inst)))
             (cond
               ((typep inst 'vm-const)
                (list :const (vm-value inst)))
               ((typep inst 'vm-move)
                (get-val (vm-src inst) gen val-env))
               ((and (opt-inst-pure-p inst) reads)
                (let ((vals (mapcar (lambda (reg) (get-val reg gen val-env)) reads)))
                   (if (commutative-p inst)
                       (destructuring-bind (a b) vals
                        (if (%opt-value< a b)
                            (list (type-of inst) a b)
                            (list (type-of inst) b a)))
                       (cons (type-of inst) vals))))
                (t nil)))))
    (let ((local-gen (copy-env gen))
          (local-val-env (copy-env val-env))
          (local-memo (copy-env memo :test #'equal))
          (new-insts nil))
      (dolist (inst (bb-instructions block))
        (let ((dst (opt-inst-dst inst)))
          (when dst
            (kill dst local-gen local-val-env local-memo)))
        (cond
          ((typep inst 'vm-const)
           (let* ((dst (vm-dst inst))
                  (key (gvn-key inst local-gen local-val-env)))
             (record dst key local-gen local-val-env local-memo)
             (push inst new-insts)))
          ((typep inst 'vm-move)
           (let* ((dst (vm-dst inst))
                  (key (gvn-key inst local-gen local-val-env))
                  (existing (gethash key local-memo)))
             (if existing
                 (progn
                   (record dst key local-gen local-val-env local-memo)
                   (push (make-vm-move :dst dst :src existing) new-insts))
                 (progn
                   (record dst key local-gen local-val-env local-memo)
                   (push inst new-insts)))))
          ((and (opt-inst-pure-p inst) (opt-inst-dst inst))
           (let* ((dst (opt-inst-dst inst))
                  (key (gvn-key inst local-gen local-val-env))
                  (existing (and key (gethash key local-memo))))
             (if existing
                 (progn
                   (record dst key local-gen local-val-env local-memo)
                   (push (make-vm-move :dst dst :src existing) new-insts))
                 (progn
                   (when key
                     (record dst key local-gen local-val-env local-memo))
                   (push inst new-insts)))))
          (t
           (push inst new-insts))))
      (setf (bb-instructions block) (nreverse new-insts))
      (dolist (child (bb-dom-children block))
        (%gvn-process-block child local-gen local-val-env local-memo)))))

(defun opt-pass-gvn (instructions)
  "Conservative global value numbering over the dominator tree.

   Unlike opt-pass-cse, knowledge is propagated into dominated basic blocks
   instead of being flushed at every label. This lets a computation in a
   dominator block replace an equivalent computation in a dominated block,
   while still avoiding cross-join speculation."
  (let ((cfg (cfg-build instructions)))
    (when (cfg-entry cfg)
      (cfg-compute-dominators cfg)
      (%gvn-process-block (cfg-entry cfg)
                          (make-hash-table :test #'eq)
                          (make-hash-table :test #'eq)
                          (make-hash-table :test #'eq))
      (cfg-flatten-hot-cold cfg))))

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

;;; ─── Leaf Function Detection ────────────────────────────────────────────

(defparameter *opt-leaf-call-types*
  '(vm-call vm-generic-call vm-tail-call vm-apply vm-call-next-method)
  "Instruction types that disqualify a function from being a leaf function.")

(defun opt-pass-leaf-detect (instructions)
  "Return INSTRUCTIONS unchanged and a leaf-function flag.

   A program is a leaf function when it contains no call-like instructions."
  (let ((leaf-p (not (some (lambda (inst)
                             (member (type-of inst) *opt-leaf-call-types* :test #'eq))
                           instructions))))
    (values instructions leaf-p)))
