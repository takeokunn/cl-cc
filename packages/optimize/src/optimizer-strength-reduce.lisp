(in-package :cl-cc/optimize)

;;; ─── FR-681: Strength Reduction for Induction Variables ─────────────────

(defun %opt-sr-const-env-before (vec end)
  "Return a register->integer constant environment for instructions before END."
  (%opt-build-const-env-up-to vec end))

(defun %opt-sr-const-value (env reg)
  "Return REG's known integer constant in ENV, or NIL."
  (let ((value (gethash reg env)))
    (and (integerp value) value)))

(defun %opt-sr-mul-by-iv-constant-p (inst iv-reg const-env)
  "Return (DST OTHER-REG CONSTANT) when INST computes IV-REG * CONSTANT."
  (when (typep inst 'vm-mul)
    (let* ((lhs (vm-lhs inst))
           (rhs (vm-rhs inst))
           (lhs-c (%opt-sr-const-value const-env lhs))
           (rhs-c (%opt-sr-const-value const-env rhs)))
      (cond
        ((and (eq lhs iv-reg) rhs-c)
         (list (vm-dst inst) lhs rhs-c))
        ((and (eq rhs iv-reg) lhs-c)
         (list (vm-dst inst) rhs lhs-c))))))

(defun %opt-sr-rewrite-body-mul (body target-mul accumulator)
  "Replace TARGET-MUL in BODY with a move from ACCUMULATOR."
  (mapcar (lambda (inst)
            (if (eq inst target-mul)
                (make-vm-move :dst (vm-dst inst) :src accumulator)
                inst))
          body))

(defun %opt-sr-counted-loop-at (vec i)
  "Return a conservative counted-loop candidate at VEC[I], or NIL."
  (%opt-counted-loop-candidate-at vec i))

(defun %opt-counted-loop-candidate-at (vec index)
  "Return a conservative single-latch counted-loop candidate at VEC[INDEX]."
  (let ((n (length vec)))
    (when (and (<= (+ index 5) (1- n))
               (vm-label-p (aref vec index)))
      (let* ((header (aref vec index))
             (cmp-inst (aref vec (+ index 1)))
             (jz-inst (aref vec (+ index 2)))
             (header-name (vm-name header)))
        (when (and (%opt-loop-unroll-cmp-inst-p cmp-inst)
                   (typep jz-inst 'vm-jump-zero)
                   (eq (vm-reg jz-inst) (vm-dst cmp-inst)))
          (let* ((exit-name (vm-label-name jz-inst))
                 (exit-pos (cfg-find-label-position vec n exit-name))
                 (back-pos (and exit-pos (1- exit-pos)))
                 (back-inst (and back-pos (>= back-pos 0) (aref vec back-pos))))
            (when (and exit-pos
                       (> exit-pos (+ index 4))
                       (typep back-inst 'vm-jump)
                       (equal (vm-label-name back-inst) header-name)
                       (not (%opt-has-external-jump-to-label-p vec header-name index exit-pos)))
              (let* ((body (loop for j from (+ index 3) below back-pos
                                 collect (aref vec j)))
                     (step-inst (car (last body))))
                (when (and body (%loop-unroll-final-step-p step-inst cmp-inst))
                  (list :header-pos index
                        :exit-pos exit-pos
                        :back-pos back-pos
                        :header header
                        :cmp cmp-inst
                        :jump-zero jz-inst
                        :body body
                        :step step-inst
                        :iv-reg (vm-lhs cmp-inst)
                        :limit-reg (vm-rhs cmp-inst)
                        :step-reg (vm-rhs step-inst)))))))))))

(defun %opt-sr-emit-iv-reduction (vec start candidate const-env new-reg-fn result)
  "Emit IV strength reduction for CANDIDATE, returning (VALUES RESULT NEXT-POS DONE-P)."
  (let* ((body (getf candidate :body))
         (iv-reg (getf candidate :iv-reg))
         (step-reg (getf candidate :step-reg))
         (step-value (%opt-sr-const-value const-env step-reg))
         (mul-inst (find-if (lambda (inst)
                              (%opt-sr-mul-by-iv-constant-p inst iv-reg const-env))
                            body)))
    (if (and step-value mul-inst)
        (destructuring-bind (mul-dst ignored constant)
            (%opt-sr-mul-by-iv-constant-p mul-inst iv-reg const-env)
          (declare (ignore mul-dst ignored))
          (let* ((acc-reg (funcall new-reg-fn))
                 (inc-reg (funcall new-reg-fn))
                 (header-pos start)
                 (exit-pos (getf candidate :exit-pos))
                 (back-pos (getf candidate :back-pos))
                 (step-inst (getf candidate :step))
                 (rewritten-body (%opt-sr-rewrite-body-mul body mul-inst acc-reg)))
            ;; Preserve original loop entry and compute the derived induction value once.
            (push (make-vm-mul :dst acc-reg :lhs iv-reg :rhs (if (eq (vm-lhs mul-inst) iv-reg)
                                                                 (vm-rhs mul-inst)
                                                                 (vm-lhs mul-inst)))
                  result)
            (loop for j from header-pos to (+ header-pos 2)
                  do (push (aref vec j) result))
            (dolist (inst rewritten-body)
              (if (eq inst step-inst)
                  (progn
                    (push (make-vm-const :dst inc-reg :value (* step-value constant)) result)
                    (push (make-vm-add :dst acc-reg :lhs acc-reg :rhs inc-reg) result)
                    (push inst result))
                  (push inst result)))
            (loop for j from back-pos to exit-pos
                  do (push (aref vec j) result))
            (values result (1+ exit-pos) t)))
        (values result start nil))))

(defun opt-pass-iv-strength-reduce (instructions)
  "FR-681 induction-variable strength reduction.

Recognizes simple counted loops and converts loop-body `i * constant` uses into
a derived induction variable initialized before the loop and advanced by
`step * constant` at the latch.  The pass is deliberately conservative: it only
handles single-latch label/cmp/jump-zero/body/step/jump loops with known integer
step and multiplier constants, and leaves other loops unchanged."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (counter (1+ (opt-max-reg-index instructions)))
         (result nil)
         (i 0))
    (flet ((new-reg ()
             (prog1 (intern (format nil "R~A" counter) :keyword)
               (incf counter))))
      (loop while (< i n) do
        (let ((candidate (%opt-sr-counted-loop-at vec i)))
          (if candidate
              (multiple-value-bind (new-result next-pos done-p)
                  (%opt-sr-emit-iv-reduction vec i candidate
                                             (%opt-sr-const-env-before vec i)
                                             #'new-reg result)
                (setf result new-result)
                (if done-p
                    (setf i next-pos)
                    (progn (push (aref vec i) result) (incf i))))
              (progn (push (aref vec i) result) (incf i))))))
    (nreverse result)))

(defun opt-pass-div-by-const (instructions)
  "FR-681 division-by-constant lowering for non-power-of-two integer divisors.

This pass exposes the non-power-of-two subset separately from
`opt-pass-strength-reduce`.  Divisors that are powers of two are intentionally
left unchanged here because the existing shift lowering owns that case.  The
implementation delegates to the FR-685 helper, which also handles modulo by
constant through the same quotient sequence."
  (%opt-pass-div-by-const/fr685 instructions))
