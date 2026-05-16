;;;; optimizer-inline-cost.lisp — Inlining Cost Model and Main Pass
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;
;;; Contains: opt-inline-inst-cost, opt-inline-body-cost,
;;; opt-adaptive-inline-threshold, opt-inline-eligible-p, opt-pass-inline.
;;;
;;; Infrastructure (memo tables, DCE pass) lives in optimizer-inline-pass.lisp.
;;;
;;; Load order: after optimizer-inline-pass.lisp, before optimizer-dataflow.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/optimize)

(defparameter *opt-inline-threshold-scale* 1
  "PGO-guided multiplier for adaptive inline thresholds.
1 means no change; values >1 make inlining more aggressive for hot profiles.")

(defparameter *opt-enable-ml-inline-score* t
  "When non-NIL, adaptive inlining also consults ML-guided score helpers.")

(defparameter *opt-inline-ml-model-version* "mlgo-v2"
  "Model version tag passed to `opt-ml-inline-score-plan`.")

(defparameter *opt-learned-cost-target* :generic
  "Target architecture hint for learned inline cost adjustment.")

(defun %opt-inline-score-features (def)
  "Build coarse feature tags for ML-guided inline score estimation."
  (let* ((body (butlast (getf def :body)))
         (inst-count (length body))
         (call-heavy-p (some (lambda (inst)
                               (typep inst '(or vm-call vm-generic-call vm-apply)))
                             body))
         (cheap-count (count-if (lambda (inst)
                                  (<= (egraph-default-cost (vm-inst-to-enode-op inst) nil) 1))
                                body))
         (cheap-ratio (if (zerop inst-count) 1.0 (/ cheap-count inst-count))))
    (remove nil
            (list (if (<= inst-count 6) :small-body :large-body)
                  (when (>= cheap-ratio 0.75) :cheap-body)
                  (when call-heavy-p :call-heavy)))))

(defun %opt-ml-inline-threshold-bonus (def)
  "Translate ML inline score into a small threshold bonus."
  (if (and *opt-enable-ml-inline-score*
           (fboundp 'opt-ml-inline-score-plan))
      (let* ((plan (opt-ml-inline-score-plan
                    :features (%opt-inline-score-features def)
                    :model-version *opt-inline-ml-model-version*))
             (score (or (getf plan :score) 0)))
        (cond ((>= score 16) 6)
              ((>= score 12) 3)
              (t 0)))
      0))

(defun opt-inline-inst-cost (inst)
  "Return the inline cost of INST using the shared e-graph opcode table.
This keeps inlining policy aligned with the optimizer's existing cost model
instead of relying on raw instruction count."
  (let* ((base (egraph-default-cost (vm-inst-to-enode-op inst) nil))
         (learned (and (fboundp 'opt-learned-codegen-cost-plan)
                       (opt-learned-codegen-cost-plan
                        :opcode-features (list (vm-inst-to-enode-op inst))
                        :target *opt-learned-cost-target*)))
         (predicted (or (and learned (getf learned :predicted-cost)) 10))
         (normalized (max 1 (ceiling predicted 4))))
    (+ base normalized)))

(defun opt-inline-body-cost (body)
  "Return the total inline cost of BODY, excluding the final vm-ret."
  (reduce #'+ (mapcar #'opt-inline-inst-cost (butlast body)) :initial-value 0))

(defun opt-adaptive-inline-threshold (def &key (base-threshold 15) (max-threshold 50))
  "Compute a conservative adaptive inline threshold for DEF.
Cheap bodies dominated by low-cost instructions get a larger threshold, while
call-heavy bodies are kept near the base threshold."
  (let* ((body (butlast (getf def :body)))
         (inst-count (length body))
         (cheap-count (count-if (lambda (inst)
                                  (<= (egraph-default-cost (vm-inst-to-enode-op inst) nil) 1))
                                body))
         (call-heavy-p (some (lambda (inst)
                               (typep inst '(or vm-call vm-generic-call vm-apply)))
                             body))
         (cheap-ratio (if (zerop inst-count) 1.0 (/ cheap-count inst-count))))
    (let* ((raw (max 8
                     (+ base-threshold
                        (if call-heavy-p -5 0)
                        (cond
                          ((>= cheap-ratio 0.90) 35)
                          ((>= cheap-ratio 0.75) 20)
                          ((>= cheap-ratio 0.50) 8)
                          (t 0)))))
            (scaled (if (and (integerp *opt-inline-threshold-scale*)
                             (> *opt-inline-threshold-scale* 1))
                        (* raw *opt-inline-threshold-scale*)
                        raw))
            (with-ml (+ scaled (%opt-ml-inline-threshold-bonus def))))
      (min max-threshold with-ml))))

(defun opt-inline-eligible-p (def threshold)
  "Return T if DEF satisfies all inlining preconditions:
1. Has a vm-closure with zero captured variables
2. Has only required params (no &optional/&rest/&key)
3. Body cost ≤ THRESHOLD (excluding the final vm-ret), unless forced INLINE
4. All body instructions support lossless register renaming
5. Body reads no global registers not defined by params or body.
NOTINLINE always blocks inlining; INLINE only bypasses the cost threshold and
does not weaken any structural safety checks."
  (let ((ci   (getf def :closure))
         (body (getf def :body)))
    (and (vm-closure-p ci)
         (not (eq (vm-closure-inline-policy ci) :notinline))
         (null (vm-captured-vars ci))
         (null (vm-closure-optional-params ci))
         (null (vm-closure-rest-param ci))
         (null (vm-closure-key-params ci))
         (or (eq (vm-closure-inline-policy ci) :inline)
             (<= (opt-inline-body-cost body) threshold))
         (opt-can-safely-rename-p body)
         (not (opt-body-has-global-refs-p body (vm-closure-params ci))))))

(defun opt-pass-inline (instructions &key (threshold 15))
  "Replace vm-call of a known small function with the inlined body.
The function must be: captured-var-free, linear, and have body cost
≤ THRESHOLD (not counting the final vm-ret)."
  (let* ((func-defs (opt-collect-function-defs instructions))
         (name-to-label (opt-build-function-name-map instructions))
         (recursive-labels (opt-call-graph-recursive-labels
                            (opt-build-call-graph instructions func-defs name-to-label)))
         (base-idx  (1+ (opt-max-reg-index instructions)))
         (reg-track (make-hash-table :test #'eq))
         (const-track (make-hash-table :test #'eq))
         (result nil))
    (dolist (inst instructions)
      (typecase inst
        ((or vm-closure vm-func-ref)
         (let ((label (vm-label-name inst)))
           (when (gethash label func-defs)
             (setf (gethash (vm-dst inst) reg-track) label)))
         (let ((dst (opt-inst-dst inst)))
           (when dst (remhash dst const-track)))
         (push inst result))
        (vm-const
         (let* ((val (vm-value inst))
                (label (when (symbolp val) (gethash val name-to-label))))
           (when (and label (gethash label func-defs))
             (setf (gethash (vm-dst inst) reg-track) label)))
          (setf (gethash (vm-dst inst) const-track) (vm-value inst))
          (push inst result))
        (vm-move
         (multiple-value-bind (src-const present-p)
             (gethash (vm-src inst) const-track)
           (if present-p
               (setf (gethash (vm-dst inst) const-track) src-const)
               (remhash (vm-dst inst) const-track)))
         (push inst result))
        (vm-call
         (let* ((label (gethash (vm-func-reg inst) reg-track))
                (def   (and label (gethash label func-defs)))
                (effective-threshold (if (and def (eq threshold :adaptive))
                                         (opt-adaptive-inline-threshold def)
                                         threshold)))
           (if (and def
                    (not (gethash label recursive-labels))
                    (opt-inline-eligible-p def effective-threshold))
               (let* ((body   (getf def :body))
                      (ci     (getf def :closure))
                      (params (vm-closure-params ci))
                      (rename (opt-make-renaming body base-idx)))
                 (incf base-idx (hash-table-count rename))
                 (loop for param in params
                       for arg in (vm-args inst)
                       do (let ((dst (or (gethash param rename) param))
                                (const-present nil)
                                (const-value nil))
                            (multiple-value-setq (const-value const-present)
                              (gethash arg const-track))
                            (push (if const-present
                                      (make-vm-const :dst dst :value const-value)
                                      (make-vm-move :dst dst :src arg))
                                  result)))
                 (dolist (bi (butlast body))
                   (push (opt-rename-regs-in-inst bi rename) result))
                 (let* ((ret-inst (car (last body)))
                        (ret-src (or (gethash (vm-reg ret-inst) rename)
                                     (vm-reg ret-inst))))
                   (push (make-vm-move :dst (vm-dst inst) :src ret-src) result)))
               (push inst result))))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst
             (remhash dst reg-track)
             (remhash dst const-track)))
         (push inst result))))
    (nreverse result)))
