(in-package :cl-cc/optimize)
;;; ─── Pass: Strength Reduction ────────────────────────────────────────────

(defun opt-power-of-2-p (n)
  "T if N is a positive integer that is a power of 2 (>= 2)."
  (and (integerp n) (>= n 2) (zerop (logand n (1- n)))))

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
   - (mod x 2^k) → (logand x (2^k - 1))
   - (* x N)   → shift/add decomposition for small integer constants N
   At vm-label boundaries, flush the constant environment."
  (let* ((env     (make-hash-table :test #'eq))
         (base    (1+ (opt-max-reg-index instructions)))
         (counter base)
         (result  nil))
    (flet ((new-reg  () (prog1 (intern (format nil "R~A" counter) :keyword) (incf counter)))
           (const-val (reg) (gethash reg env))
           (emit      (i) (push i result))
           (emit-seq  (insts) (dolist (i insts) (push i result))))
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
               (t (emit inst)))))
          (vm-div
           (let* ((dst (vm-dst inst))
                  (lhs (vm-lhs inst))
                  (rhs (vm-rhs inst))
                  (rv  (const-val rhs)))
             (cond
               ((and rv (opt-power-of-2-p rv))
                (let* ((k         (- (1- (integer-length rv))))
                       (shift-reg (new-reg)))
                  (remhash dst env)
                  (emit (make-vm-const :dst shift-reg :value k))
                  (emit (make-vm-ash   :dst dst :lhs lhs :rhs shift-reg))))
               (t
                (let ((dstreg (opt-inst-dst inst)))
                  (when dstreg (remhash dstreg env)))
                (emit inst)))))
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
                  (when dstreg (remhash dstreg env)))
                (emit inst)))))
           (t
            (let ((dst (opt-inst-dst inst)))
              (when dst (remhash dst env)))
            (emit inst)))))
    (nreverse result)))

;;; Bswap and rotate recognition passes are in optimizer-recognition.lisp.
