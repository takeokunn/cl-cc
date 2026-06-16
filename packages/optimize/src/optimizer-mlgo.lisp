;;;; optimizer-mlgo.lisp — ML-guided inline benefit predictor

(in-package :cl-cc/optimize)

(defparameter *mlgo-enabled* nil
  "When non-NIL, enable MLGO-inspired inline benefit prediction.")

(defparameter *mlgo-inline-weights*
  '(:bias 4 :instruction-count -0.35 :call-count -1.25 :loop-depth 4.0
    :arg-count -0.75)
  "Hardcoded linear-model weights for inline benefit prediction.")

(defun %mlgo-call-instruction-p (inst)
  (typep inst '(or vm-call vm-generic-call vm-tail-call vm-apply)))

(defun %mlgo-loop-depth (instructions)
  "Return a coarse loop-depth feature from backward branches in INSTRUCTIONS."
  (let ((label-pos (make-hash-table :test #'equal))
        (max-depth 0))
    (loop for inst in instructions
          for i from 0
          when (typep inst 'vm-label)
            do (setf (gethash (vm-name inst) label-pos) i))
    (loop for inst in instructions
          for i from 0
          when (typep inst '(or vm-jump vm-jump-zero))
            do (let ((target (gethash (vm-label-name inst) label-pos)))
                 (when (and target (< target i))
                   (incf max-depth))))
    max-depth))

(defun %mlgo-function-body (def)
  (cond ((and (listp def) (getf def :body)) (butlast (getf def :body)))
        ((listp def) def)
        (t nil)))

(defun opt-mlgo-function-features (def &key profile-data)
  "Return a feature vector for MLGO inline decisions.
Features are instruction count, call count, loop depth, and argument count."
  (declare (ignore profile-data))
  (let* ((body (%mlgo-function-body def))
         (ci (and (listp def) (getf def :closure)))
         (inst-count (length body))
         (call-count (count-if #'%mlgo-call-instruction-p body))
         (loop-depth (%mlgo-loop-depth body))
         (arg-count (cond ((typep ci '(or vm-closure vm-func-ref))
                           (length (vm-closure-params ci)))
                          ((and (listp def) (getf def :params))
                           (length (getf def :params)))
                          (t 0))))
    (list :instruction-count inst-count
          :call-count call-count
          :loop-depth loop-depth
          :arg-count arg-count)))

(defun %mlgo-weight (key)
  (getf *mlgo-inline-weights* key 0))

(defun opt-mlgo-inline-benefit (features)
  "Predict inline benefit from FEATURES using a weighted linear model."
  (+ (%mlgo-weight :bias)
     (* (%mlgo-weight :instruction-count) (getf features :instruction-count 0))
     (* (%mlgo-weight :call-count) (getf features :call-count 0))
     (* (%mlgo-weight :loop-depth) (getf features :loop-depth 0))
     (* (%mlgo-weight :arg-count) (getf features :arg-count 0))))

(defun opt-mlgo-inline-threshold (def &key profile-data (base-threshold 15) (max-threshold 80))
  "Return an ML-predicted inline threshold for DEF, replacing a static cutoff."
  (let* ((features (opt-mlgo-function-features def :profile-data profile-data))
         (benefit (opt-mlgo-inline-benefit features)))
    (max 1 (min max-threshold (+ base-threshold (round benefit))))))

(defun opt-pass-mlgo-inline (instructions)
  "Run inlining with the MLGO benefit model when explicitly enabled."
  (if *mlgo-enabled*
      (let ((*opt-enable-ml-inline-score* t))
        (opt-pass-inline instructions :threshold :mlgo))
      instructions))
