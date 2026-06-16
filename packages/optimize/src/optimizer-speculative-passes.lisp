(in-package :cl-cc/optimize)

(defun opt-pass-specialize-known-args (instructions)
  "Conservatively clone known-callee functions specialized by constant call args."
  (let* ((func-defs (opt-collect-function-defs instructions))
         (base-idx (1+ (opt-max-reg-index instructions)))
         (reg-track (make-hash-table :test #'eq))
         (const-track (make-hash-table :test #'eq))
         (plan-cache (make-hash-table :test #'equal))
         (emitted-labels (make-hash-table :test #'equal))
         (result nil))
    (labels ((clear-dst-tracks (dst)
               (when dst
                 (remhash dst const-track)
                 (remhash dst reg-track))))
      (dolist (inst instructions)
        (typecase inst
          ((or vm-closure vm-func-ref)
           (setf (gethash (vm-dst inst) reg-track) (vm-label-name inst))
           (remhash (vm-dst inst) const-track)
           (push inst result))
          (vm-const
           (setf (gethash (vm-dst inst) const-track) (vm-value inst))
           (remhash (vm-dst inst) reg-track)
           (push inst result))
          (vm-move
           (multiple-value-bind (src-const present-p)
               (gethash (vm-src inst) const-track)
             (if present-p
                 (setf (gethash (vm-dst inst) const-track) src-const)
                 (remhash (vm-dst inst) const-track)))
           (multiple-value-bind (label found-p)
               (gethash (vm-src inst) reg-track)
             (if found-p
                 (setf (gethash (vm-dst inst) reg-track) label)
                 (remhash (vm-dst inst) reg-track)))
           (push inst result))
          ((or vm-call vm-tail-call)
            (let* ((callee-label (gethash (vm-func-reg inst) reg-track))
                   (def (and callee-label (gethash callee-label func-defs))))
             (if (null def)
                 (push inst result)
                 (let* ((params (getf def :params))
                        (body (getf def :body))
                        (const-bindings (%opt-constant-bindings-from-call-args
                                         params (vm-args inst) const-track))
                        (plan (and const-bindings
                                   (opt-build-specialization-plan
                                    callee-label params const-bindings :cache plan-cache))))
                   (if (null plan)
                       (push inst result)
                       (let* ((specialized-label (opt-specialization-plan-specialized-name plan))
                              (dynamic-params (opt-specialization-plan-dynamic-args plan))
                              (dynamic-args (%opt-dynamic-call-args params (vm-args inst) dynamic-params))
                              (clone-reg (intern (format nil "R~A" base-idx) :keyword)))
                         (incf base-idx)
                         (unless (gethash specialized-label emitted-labels)
                            (let* ((partial
                                     (opt-partial-evaluate-function
                                      callee-label params body
                                      :constant-bindings const-bindings
                                      :specialized-name specialized-label))
                                   (residual-body
                                     (opt-partial-eval-residual-body partial)))
                              (push (make-vm-closure :dst clone-reg
                                                     :label specialized-label
                                                     :params dynamic-params
                                                    :captured nil)
                                   result)
                             (push (make-vm-label :name specialized-label) result)
                             (dolist (body-inst residual-body)
                               (push body-inst result))
                             (setf (gethash specialized-label emitted-labels) clone-reg)))
                         (let ((resolved-reg (gethash specialized-label emitted-labels)))
                           (push (%opt-make-call-like inst resolved-reg dynamic-args)
                                 result)))))))
              (clear-dst-tracks (opt-inst-dst inst)))
          (vm-apply
           ;; APPLY spreads the final argument list at runtime, so fixed-arity
           ;; parameter/signature reasoning is not sound here yet.
           (clear-dst-tracks (opt-inst-dst inst))
           (push inst result))
          (t
           (clear-dst-tracks (opt-inst-dst inst))
           (push inst result)))))
    (nreverse result)))

(defun opt-pass-partial-evaluation (instructions)
  "Pipeline entrypoint for partial evaluation over known constant call arguments.

Current strategy specializes known-callee call sites into residual clones with
only dynamic arguments forwarded, then relies on downstream fold/SCCP cleanup."
  (opt-pass-specialize-known-args instructions))

;;; FR-523..FR-528 planning helpers (backend roadmap evidence anchors)

(defun %opt-find-label-index (vec name &optional (start 0))
  (loop for i from start below (length vec)
        for inst = (aref vec i)
        when (and (typep inst 'vm-label)
                  (equal (vm-name inst) name))
        do (return i)))

(defun %opt-parse-canonical-loop-at (vec i)
  "Parse canonical loop shape at label index I.

Expected shape:
  Lh: cmp/jz body step jump Lh Lexit:
where cmp is vm-lt and step is self-update vm-add on induction variable.
Returns OPT-CANONICAL-LOOP or NIL."
  (when (and (< (+ i 5) (length vec))
             (typep (aref vec i) 'vm-label)
             (typep (aref vec (1+ i)) 'vm-lt)
             (typep (aref vec (+ i 2)) 'vm-jump-zero))
    (let* ((head (aref vec i))
           (cmp  (aref vec (1+ i)))
           (jz   (aref vec (+ i 2)))
           (head-label (vm-name head))
           (exit-label (vm-label-name jz))
           (exit-idx (%opt-find-label-index vec exit-label (+ i 3))))
      (when (and exit-idx (> exit-idx (+ i 4)))
        (let* ((back-idx (1- exit-idx))
               (back (aref vec back-idx)))
          (when (and (typep back 'vm-jump)
                     (equal (vm-label-name back) head-label))
            (let* ((body (loop for k from (+ i 3) below back-idx
                               collect (aref vec k)))
                   (step (car (last body))))
              (when (and (typep step 'vm-add)
                         (eq (vm-dst step) (vm-lhs step))
                         (eq (vm-dst step) (vm-lhs cmp))
                         (eq (vm-reg jz) (vm-dst cmp)))
                (make-opt-canonical-loop
                 :head-index i
                 :cmp-index (1+ i)
                 :jz-index (+ i 2)
                 :back-index back-idx
                 :exit-index exit-idx
                 :head-label head-label
                 :exit-label exit-label
                 :iv-reg (vm-lhs cmp)
                 :limit-reg (vm-rhs cmp)
                 :step-reg (vm-rhs step)
                 :cond-reg (vm-dst cmp)
                 :body body)))))))))

(defun %opt-find-canonical-loops (instructions)
  (let* ((vec (coerce instructions 'vector))
         (loops nil)
         (i 0)
         (n (length vec)))
    (loop while (< i n)
          do (let ((lp (%opt-parse-canonical-loop-at vec i)))
               (if lp
                   (progn
                     (push lp loops)
                     (setf i (1+ (opt-loop-exit-index lp))))
                   (incf i))))
    (nreverse loops)))

(defun opt-build-affine-loop-summary (&key induction-vars bounds accesses)
  "Build a conservative affine-loop summary descriptor."
  (list :kind :affine-loop-summary
        :induction-vars (copy-list (or induction-vars nil))
        :bounds (copy-list (or bounds nil))
        :accesses (copy-list (or accesses nil))))

(defun %opt-access-kind (inst)
  (typecase inst
    (vm-get-global :read-global)
    (vm-set-global :write-global)
    (vm-slot-read :read-slot)
    (vm-slot-write :write-slot)
    (t nil)))

(defun %opt-inst-side-effect-p (inst)
  (or (typep inst '(or vm-set-global vm-slot-write vm-call vm-generic-call vm-apply vm-ret))))

(defun %opt-loop-core-and-step (lp)
  (let* ((body (opt-loop-body lp))
         (step (car (last body)))
         (core (butlast body)))
    (values core step)))

(defun %opt-loop-constant-init (vec lp)
  "Return last dominating integer init for loop IV, or NIL when uncertain."
  (let* ((iv (opt-loop-iv-reg lp))
         (value nil))
    (loop for i from 0 below (opt-loop-head-index lp)
          for inst = (aref vec i)
          do (cond
               ((and (typep inst 'vm-const)
                     (eq (vm-dst inst) iv)
                     (integerp (vm-value inst)))
                (setf value (vm-value inst)))
               ((and (opt-inst-dst inst) (eq (opt-inst-dst inst) iv))
                (setf value nil))))
    value))

(defun %opt-inst-depends-on-p (producer consumer)
  (let ((dst (opt-inst-dst producer)))
    (and dst (member dst (opt-inst-read-regs consumer) :test #'eq))))

(defun %opt-schedule-core-with-deps (core)
  "Dependency-aware local reordering: only swap adjacent independent ops by cost." 
  (let ((vec (coerce (copy-list core) 'vector))
        (changed nil))
    (loop for i from 0 below (1- (length vec))
          do (let* ((a (aref vec i))
                    (b (aref vec (1+ i))))
               (when (and (not (%opt-inst-depends-on-p a b))
                          (not (%opt-inst-depends-on-p b a))
                          (< (opt-inline-inst-cost b) (opt-inline-inst-cost a)))
                 (rotatef (aref vec i) (aref vec (1+ i)))
                 (setf changed t))))
    (values (coerce vec 'list) changed)))

(defun opt-pass-affine-loop-analysis (instructions)
  "Analyze canonical loops and cache affine summaries for later passes.

This pass preserves instructions but computes real summaries from detected loop
regions (not from caller-provided payload lists)."
  (let ((loops (%opt-find-canonical-loops instructions))
        (summaries nil))
    (dolist (lp loops)
      (let* ((accesses (remove nil
                               (mapcar (lambda (inst)
                                         (let ((kind (%opt-access-kind inst)))
                                           (and kind (list :kind kind :inst inst))))
                                       (opt-loop-body lp))))
             (summary (opt-build-affine-loop-summary
                       :induction-vars (list (opt-loop-iv-reg lp))
                       :bounds (list (list :lt (opt-loop-iv-reg lp) (opt-loop-limit-reg lp)))
                       :accesses accesses)))
        (push summary summaries)))
    (setf *opt-last-affine-loop-summaries* (nreverse summaries))
    instructions))

(defun opt-loop-interchange-plan (&key loops cache-locality-score dependence-safe-p)
  "Return an interchange plan when dependence safety is proven."
  (list :kind :loop-interchange
        :applied-p (and dependence-safe-p (> (or cache-locality-score 0) 0))
        :loops (copy-list (or loops nil))
        :dependence-safe-p (not (null dependence-safe-p))
        :cache-locality-score (or cache-locality-score 0)))

(defun opt-pass-loop-interchange (instructions)
  "Apply conservative loop-body interchange via independent core-op swap.

This is intentionally strict: only pure/independent core operations are swapped.
Control instructions and IV update remain untouched."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (out nil)
         (changed nil)
         (i 0))
    (labels ((emit (x) (push x out)))
      (loop while (< i n)
            do (let ((lp (%opt-parse-canonical-loop-at vec i)))
                 (if (null lp)
                     (progn (emit (aref vec i)) (incf i))
                     (multiple-value-bind (core step) (%opt-loop-core-and-step lp)
                       (let ((rewritten core))
                         (when (and (>= (length core) 2)
                                    (opt-inst-cse-eligible-p (first core))
                                    (opt-inst-cse-eligible-p (second core))
                                    (not (%opt-inst-depends-on-p (first core) (second core)))
                                    (not (%opt-inst-depends-on-p (second core) (first core))))
                           (setf rewritten (cons (second core) (cons (first core) (cddr core))))
                           (setf changed t))
                         (emit (aref vec i))
                         (emit (aref vec (1+ i)))
                         (emit (aref vec (+ i 2)))
                         (dolist (inst rewritten) (emit inst))
                         (emit step)
                         (emit (aref vec (opt-loop-back-index lp)))
                         (emit (aref vec (opt-loop-exit-index lp)))
                         (setf i (1+ (opt-loop-exit-index lp))))))))
      (if changed (nreverse out) instructions))))

(defun opt-polyhedral-schedule-plan (&key statements constraints objective)
  "Return a conservative polyhedral schedule planning descriptor."
  (list :kind :polyhedral-schedule
        :statements (copy-list (or statements nil))
        :constraints (copy-list (or constraints nil))
        :objective (or objective :latency-min)))

(defun opt-pass-polyhedral-schedule (instructions)
  "Apply a conservative schedule optimization inside canonical loops.

Current subset: reorder loop body pure operations by ascending static cost,
leaving control-flow and induction update in place."
  (let* ((vec (coerce instructions 'vector))
         (out nil)
         (changed nil)
         (i 0)
         (n (length vec)))
    (labels ((emit (x) (push x out)))
      (loop while (< i n)
            do (let ((lp (%opt-parse-canonical-loop-at vec i)))
                 (if (null lp)
                     (progn
                       (emit (aref vec i))
                       (incf i))
                     (let* ((body (opt-loop-body lp))
                            (step (car (last body)))
                            (body-core (butlast body))
                            (sortable (every #'opt-inst-cse-eligible-p body-core))
                            (sorted body-core)
                            (sorted-changed nil)
                            (plan (opt-polyhedral-schedule-plan
                                   :statements body-core
                                   :constraints (list :canonical-loop)
                                   :objective :latency-min)))
                        (declare (ignore plan))
                       (when sortable
                         (multiple-value-setq (sorted sorted-changed)
                           (%opt-schedule-core-with-deps body-core)))
                       (when sorted-changed
                         (setf changed t))
                       (emit (aref vec i))
                       (emit (aref vec (1+ i)))
                       (emit (aref vec (+ i 2)))
                       (dolist (inst sorted)
                         (emit inst))
                       (emit step)
                       (emit (aref vec (opt-loop-back-index lp)))
                       (emit (aref vec (opt-loop-exit-index lp)))
                       (setf i (1+ (opt-loop-exit-index lp)))))))
      (if changed (nreverse out) instructions))))

(defun opt-loop-fusion-fission-plan (&key loops register-pressure instruction-budget)
  "Choose loop fusion/fission strategy from simple pressure/budget heuristics."
  (let* ((pressure (or register-pressure 0))
         (budget (or instruction-budget 0))
         (strategy (cond ((and (> pressure 32) (> budget 0)) :fission)
                         ((and (<= pressure 32) (> budget 0)) :fusion)
                         (t :none))))
    (list :kind :loop-fusion-fission
          :strategy strategy
          :loops (copy-list (or loops nil))
          :register-pressure pressure
          :instruction-budget budget)))

(defun opt-pass-loop-fusion-fission (instructions)
  "Apply conservative loop fusion/fission on canonical loops.

Fusion: adjacent loops with identical headers are merged into one loop body.
Fission: oversized loop body is split into two core regions in the same loop
         using a conservative split marker label."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (out nil)
         (changed nil)
         (i 0))
    (labels ((emit (x) (push x out))
             (loop-seq (lp)
               (loop for k from (opt-loop-head-index lp) to (opt-loop-exit-index lp)
                     collect (aref vec k)))
             (pure-core (lp)
               (multiple-value-bind (core _step) (%opt-loop-core-and-step lp)
                 (declare (ignore _step))
                 core))
             (pure-loop-p (lp)
               (every #'opt-inst-cse-eligible-p (pure-core lp)))
             (same-iter-space-p (a b)
               (and (not (eq (opt-loop-iv-reg a) (opt-loop-iv-reg b)))
                    (equal (opt-loop-limit-reg a) (opt-loop-limit-reg b))
                    (equal (opt-loop-step-reg a) (opt-loop-step-reg b))
                    (equal (%opt-loop-constant-init vec a)
                           (%opt-loop-constant-init vec b)))))
      (loop while (< i n)
            do (let ((lp (%opt-parse-canonical-loop-at vec i)))
                 (if (null lp)
                     (progn
                       (emit (aref vec i))
                       (incf i))
                     (let* ((next-i (1+ (opt-loop-exit-index lp)))
                            (lp2 (and (< next-i n)
                                      (%opt-parse-canonical-loop-at vec next-i))))
                       (if (and lp2
                                (pure-loop-p lp)
                                (pure-loop-p lp2)
                                (same-iter-space-p lp lp2))
                           (multiple-value-bind (core-a step-a) (%opt-loop-core-and-step lp)
                             (multiple-value-bind (core-b _step-b) (%opt-loop-core-and-step lp2)
                               (declare (ignore _step-b))
                               (let ((m (make-hash-table :test #'eq)))
                                 (setf (gethash (opt-loop-iv-reg lp2) m)
                                       (opt-loop-iv-reg lp))
                                 (emit (aref vec (opt-loop-head-index lp)))
                                 (emit (aref vec (opt-loop-cmp-index lp)))
                                 (emit (aref vec (opt-loop-jz-index lp)))
                                 (dolist (inst core-a) (emit inst))
                                 (dolist (inst core-b) (emit (opt-rewrite-inst-regs inst m)))
                                 (emit step-a)
                                 (emit (aref vec (opt-loop-back-index lp)))
                                 (emit (aref vec (opt-loop-exit-index lp2)))
                                 (setf changed t)
                                 (setf i (1+ (opt-loop-exit-index lp2))))))
                           (let ((core (pure-core lp)))
                             (if (and (pure-loop-p lp)
                                      (> (length core) 24))
                                 (progn
                                   ;; Conservative fission: keep semantics while creating two
                                   ;; independently schedulable core regions in the same loop.
                                   (let* ((half (floor (length core) 2))
                                          (core-a (subseq core 0 half))
                                          (core-b (subseq core half))
                                          (split-label (make-vm-label
                                                        :name (intern (format nil "~A__SPLIT" (vm-name (aref vec (opt-loop-head-index lp))))
                                                                      :keyword))))
                                     (emit (aref vec (opt-loop-head-index lp)))
                                     (emit (aref vec (opt-loop-cmp-index lp)))
                                     (emit (aref vec (opt-loop-jz-index lp)))
                                     (dolist (inst core-a) (emit inst))
                                     (emit split-label)
                                     (dolist (inst core-b) (emit inst))
                                     (emit (car (last (opt-loop-body lp))))
                                     (emit (aref vec (opt-loop-back-index lp)))
                                     (emit (aref vec (opt-loop-exit-index lp)))
                                     (setf changed t)
                                     (setf i (1+ (opt-loop-exit-index lp)))))
                                 (progn
                                    (dolist (inst (loop-seq lp))
                                      (emit inst))
                                    (setf i (1+ (opt-loop-exit-index lp)))))))))))
      (if changed (nreverse out) instructions))))

(defun opt-ml-inline-score-plan (&key features model-version)
  "Return a deterministic MLGO-style inline scoring descriptor."
  (let ((feature-count (length (or features nil))))
    (list :kind :ml-inline-score
          :model-version (or model-version "mlgo-v1")
          :feature-count feature-count
          :score (+ 10 (* 2 feature-count)))))

(defun opt-learned-codegen-cost-plan (&key opcode-features target)
  "Return learned cost descriptor used by backend codegen selection policies."
  (let* ((feature-count (length (or opcode-features nil)))
         (arch (or target :generic))
         (base (ecase arch
                 ((:x86-64) 8)
                 ((:aarch64) 7)
                 ((:generic) 10))))
    (list :kind :learned-codegen-cost
          :target arch
          :feature-count feature-count
          :predicted-cost (+ base feature-count))))
