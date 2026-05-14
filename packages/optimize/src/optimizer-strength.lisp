(in-package :cl-cc/optimize)
;;; ─── Pass: Strength Reduction ────────────────────────────────────────────

(defun opt-power-of-2-p (n)
  "T if N is a positive integer that is a power of 2 (>= 2)."
  (and (integerp n) (>= n 2) (zerop (logand n (1- n)))))

(defparameter +opt-strength-reduce-max-verified-dividend-hi+ 65535
  "Largest dividend upper bound FR-282 verifies exhaustively for reciprocal division.

Non-power-of-two division strength reduction stays intentionally conservative:
the pass only searches exact multiply/shift reciprocals for small, finite,
non-negative dividend intervals. Larger or unknown ranges keep vm-div.")

(defun %opt-verified-reciprocal-div-exact-p (divisor multiplier shift lo hi)
  "Return T when floor(X / DIVISOR) == floor((X * MULTIPLIER) / 2^SHIFT) on [LO, HI]."
  (loop for x from lo to hi
        always (= (floor x divisor)
                  (ash (* x multiplier) (- shift)))))

(defun %opt-find-verified-reciprocal-div-params (divisor interval)
  "Return (MULTIPLIER SHIFT) when an exact reciprocal exists for INTERVAL, else NIL.

The search is verified over the proved dividend interval instead of assuming a
fixed machine-word formula. Only non-negative bounded intervals are eligible."
  (when (and interval
             (integerp divisor)
             (> divisor 1)
             (not (opt-power-of-2-p divisor))
             (opt-interval-nonnegative-p interval))
    (let ((lo (opt-interval-lo interval))
          (hi (opt-interval-hi interval)))
      (when (and (integerp lo)
                 (integerp hi)
                 (<= lo hi)
                 (<= hi +opt-strength-reduce-max-verified-dividend-hi+))
        (let ((max-shift
                (max 1
                     (1+ (integer-length (* (max 1 hi)
                                            (1- divisor)))))))
          (loop for shift from 1 to max-shift
                for scale = (ash 1 shift)
                for multiplier = (ceiling scale divisor)
                 when (%opt-verified-reciprocal-div-exact-p divisor multiplier shift lo hi)
                   return (list multiplier shift)))))))

(defun %opt-find-verified-reciprocal-div-params-with-bias (divisor interval)
  "Return (MULTIPLIER SHIFT BIAS) when exact affine reciprocal exists, else NIL.

Searches exact solutions for:
  floor(x / DIVISOR) == floor((x * MULTIPLIER + BIAS) / 2^SHIFT)
over the proved integer interval [LO, HI]. Unlike the no-bias form, this also
supports intervals that include negative dividends."
  (when (and interval
             (integerp divisor)
             (> divisor 1)
             (not (opt-power-of-2-p divisor)))
    (let ((lo (opt-interval-lo interval))
          (hi (opt-interval-hi interval)))
      (when (and (integerp lo)
                 (integerp hi)
                 (<= lo hi)
                 (<= (abs lo) +opt-strength-reduce-max-verified-dividend-hi+)
                 (<= (abs hi) +opt-strength-reduce-max-verified-dividend-hi+))
        (let* ((max-abs (max (abs lo) (abs hi)))
               (max-shift (max 1
                               (1+ (integer-length (* (max 1 max-abs)
                                                      (1- divisor)))))))
          (loop for shift from 1 to max-shift do
            (let* ((scale (ash 1 shift))
                   (m-floor (floor scale divisor))
                   (m-ceil (ceiling scale divisor)))
              (loop for multiplier in (remove-duplicates
                                       (list m-floor m-ceil (1- m-ceil) (1+ m-floor))) do
                (let ((b-lo nil)
                      (b-hi nil)
                      (ok t))
                  (loop for x from lo to hi while ok do
                    (let* ((q (floor x divisor))
                           (lower (- (* q scale) (* x multiplier)))
                           (upper (1- (- (* (1+ q) scale) (* x multiplier)))))
                      (when (> lower upper)
                        (setf ok nil))
                      (when ok
                        (setf b-lo (if b-lo (max b-lo lower) lower)
                              b-hi (if b-hi (min b-hi upper) upper))
                        (when (> b-lo b-hi)
                          (setf ok nil)))))
                  (when (and ok b-lo b-hi (<= b-lo b-hi))
                    (return-from %opt-find-verified-reciprocal-div-params-with-bias
                      (list multiplier shift b-lo))))))))))))

(defun %opt-div-by-verified-reciprocal-seq (dst src divisor interval new-reg-fn)
  "Build a verified reciprocal multiply/shift sequence for DST ← floor(SRC / DIVISOR).

Returns NIL when no exact sequence is known for INTERVAL. Emitted instructions
are limited to vm-const, vm-mul, and vm-ash as required by FR-282."
  (let ((params (%opt-find-verified-reciprocal-div-params divisor interval)))
    (when params
      (destructuring-bind (multiplier shift) params
        (let ((mult-reg  (funcall new-reg-fn))
              (tmp-reg   (funcall new-reg-fn))
              (shift-reg (funcall new-reg-fn)))
          (list (make-vm-const :dst mult-reg :value multiplier)
                (make-vm-mul   :dst tmp-reg :lhs src :rhs mult-reg)
                (make-vm-const :dst shift-reg :value (- shift))
                (make-vm-ash   :dst dst :lhs tmp-reg :rhs shift-reg)))))))

(defun %opt-div-by-verified-reciprocal-seq-with-bias (dst src divisor interval new-reg-fn)
  "Build exact reciprocal multiply/add/shift sequence for bounded signed ranges.

Returns NIL when no exact affine reciprocal exists for INTERVAL. Emitted
instructions are limited to vm-const, vm-mul, vm-add, and vm-ash."
  (let ((params (%opt-find-verified-reciprocal-div-params-with-bias divisor interval)))
    (when params
      (destructuring-bind (multiplier shift bias) params
        (let ((mult-reg  (funcall new-reg-fn))
              (tmp-reg   (funcall new-reg-fn))
              (bias-reg  (funcall new-reg-fn))
              (sum-reg   (funcall new-reg-fn))
              (shift-reg (funcall new-reg-fn)))
          (list (make-vm-const :dst mult-reg :value multiplier)
                (make-vm-mul   :dst tmp-reg :lhs src :rhs mult-reg)
                (make-vm-const :dst bias-reg :value bias)
                (make-vm-add   :dst sum-reg :lhs tmp-reg :rhs bias-reg)
                (make-vm-const :dst shift-reg :value (- shift))
                (make-vm-ash   :dst dst :lhs sum-reg :rhs shift-reg)))))))

(defun %opt-mul-by-const-seq (dst src n new-reg-fn)
  "Build a shift/add instruction sequence computing DST ← SRC * N.
NEW-REG-FN is a thunk that allocates fresh temporary register keywords.
Returns a list of vm instructions (may include vm-const, vm-ash, vm-add, vm-move, vm-neg)."
  (let* ((negp (minusp n))
         (absn (abs n)))
    (cond
      ((zerop absn)
       (list (make-vm-const :dst dst :value 0)))
      ((= absn 1)
       (append (list (make-vm-move :dst dst :src src))
               (when negp (list (make-vm-neg :dst dst :src dst)))))
      (t
       (let ((bits  (loop for bit from 0 below (integer-length absn)
                          when (logbitp bit absn) collect bit))
             (seq   nil)
             (terms nil))
         (dolist (bit bits)
           (if (zerop bit)
               (push src terms)
               (let ((shift-count (funcall new-reg-fn))
                     (shifted     (funcall new-reg-fn)))
                 (push (make-vm-const :dst shift-count :value bit) seq)
                 (push (make-vm-ash   :dst shifted :lhs src :rhs shift-count) seq)
                 (push shifted terms))))
         (setf terms (nreverse terms))
         (let ((first (car terms)))
           (unless (eq first dst)
             (push (make-vm-move :dst dst :src first) seq))
           (dolist (term (cdr terms))
             (push (make-vm-add :dst dst :lhs dst :rhs term) seq))
           (when negp
             (push (make-vm-neg :dst dst :src dst) seq))
           (nreverse seq)))))))

(defun opt-pass-strength-reduce (instructions)
  "Forward pass: replace multiply/divide by powers of 2 with arithmetic shifts.
   - (* x 2^k) → (ash x k)
   - (* 2^k x) → (ash x k)   [commutative]
   - (/ x 2^k) → (ash x -k)  [floor semantics: (floor x 2^k) = (ash x -k)]
   - (/ x D)   → verified reciprocal multiply/shift when D is a positive
                 non-power-of-2 constant and x is proved non-negative + bounded
   - (mod x 2^k) → (logand x (2^k - 1))
   - (* x N)   → shift/add decomposition for small integer constants N
   At vm-label boundaries, flush the constant environment."
  (let* ((env       (make-hash-table :test #'eq))
         (intervals (make-hash-table :test #'eq))
         (base      (1+ (opt-max-reg-index instructions)))
         (counter   base)
         (result    nil))
    (flet ((new-reg ()
             (prog1 (intern (format nil "R~A" counter) :keyword)
               (incf counter)))
           (const-val (reg)
             (gethash reg env))
           (emit (inst)
             (push inst result))
           (emit-seq (insts)
             (dolist (inst insts)
               (push inst result)))
           (advance-intervals (inst)
             (%opt-transfer-interval-inst inst intervals)))
      (dolist (inst instructions)
        (typecase inst
          (vm-label
           (clrhash env)
           (clrhash intervals)
           (emit inst))

          (vm-const
           (setf (gethash (vm-dst inst) env) (vm-value inst))
           (emit inst)
           (advance-intervals inst))

          (vm-mul
           (let* ((dst (vm-dst inst))
                  (lhs (vm-lhs inst))
                  (rhs (vm-rhs inst))
                  (rv  (const-val rhs))
                  (lv  (const-val lhs)))
             (cond
               ((and rv (opt-power-of-2-p rv))
                (let* ((k         (1- (integer-length rv)))
                       (shift-reg (new-reg)))
                  (remhash dst env)
                  (emit (make-vm-const :dst shift-reg :value k))
                  (emit (make-vm-ash   :dst dst :lhs lhs :rhs shift-reg))))

               ((and lv (opt-power-of-2-p lv))
                (let* ((k         (1- (integer-length lv)))
                       (shift-reg (new-reg)))
                  (remhash dst env)
                  (emit (make-vm-const :dst shift-reg :value k))
                  (emit (make-vm-ash   :dst dst :lhs rhs :rhs shift-reg))))

               ((and rv (integerp rv) (not (zerop rv)) (<= (logcount (abs rv)) 2))
                (remhash dst env)
                (emit-seq (%opt-mul-by-const-seq dst lhs rv #'new-reg)))

               ((and lv (integerp lv) (not (zerop lv)) (<= (logcount (abs lv)) 2))
                (remhash dst env)
                (emit-seq (%opt-mul-by-const-seq dst rhs lv #'new-reg)))

               (t
                (emit inst))))
           (advance-intervals inst))

          (vm-div
           (let* ((dst          (vm-dst inst))
                  (lhs          (vm-lhs inst))
                  (rhs          (vm-rhs inst))
                  (rv           (const-val rhs))
                  (lhs-interval (gethash lhs intervals)))
             (cond
               ((and rv (opt-power-of-2-p rv))
                (let* ((k         (- (1- (integer-length rv))))
                       (shift-reg (new-reg)))
                  (remhash dst env)
                  (emit (make-vm-const :dst shift-reg :value k))
                  (emit (make-vm-ash   :dst dst :lhs lhs :rhs shift-reg))))

               ((and rv
                      (integerp rv)
                      (> rv 1)
                      (not (opt-power-of-2-p rv)))
                 (let ((seq (or (%opt-div-by-verified-reciprocal-seq dst lhs rv lhs-interval #'new-reg)
                                (%opt-div-by-verified-reciprocal-seq-with-bias dst lhs rv lhs-interval #'new-reg))))
                   (if seq
                       (progn
                         (remhash dst env)
                         (emit-seq seq))
                      (progn
                        (let ((dstreg (opt-inst-dst inst)))
                          (when dstreg
                            (remhash dstreg env)))
                        (emit inst)))))

               (t
                (let ((dstreg (opt-inst-dst inst)))
                  (when dstreg
                    (remhash dstreg env)))
                (emit inst))))
           (advance-intervals inst))

          (vm-mod
           (let* ((dst (vm-dst inst))
                  (lhs (vm-lhs inst))
                  (rhs (vm-rhs inst))
                  (rv  (const-val rhs)))
             (cond
               ((and rv (opt-power-of-2-p rv))
                (let ((mask-reg (new-reg)))
                  (remhash dst env)
                  (emit (make-vm-const  :dst mask-reg :value (1- rv)))
                  (emit (make-vm-logand :dst dst :lhs lhs :rhs mask-reg))))

               (t
                (let ((dstreg (opt-inst-dst inst)))
                  (when dstreg
                    (remhash dstreg env)))
                (emit inst))))
           (advance-intervals inst))

          (t
           (let ((dst (opt-inst-dst inst)))
             (when dst
               (remhash dst env)))
           (emit inst)
           (advance-intervals inst)))))
    (nreverse result)))

;;; Bswap and rotate recognition passes are in optimizer-recognition.lisp.
