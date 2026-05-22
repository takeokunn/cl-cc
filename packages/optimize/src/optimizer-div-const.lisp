;;;; packages/optimize/src/optimizer-div-const.lisp — FR-685 division by constant

(in-package :cl-cc/optimize)

(defun opt-div-const-unsigned-magic (divisor &key (bits 64) (extra-bits bits))
  "Return the ceil(2^(BITS+EXTRA-BITS)/DIVISOR) unsigned reciprocal magic."
  (when (and (integerp divisor) (> divisor 1))
    (ceiling (ash 1 (+ bits extra-bits)) divisor)))

(defun opt-div-const-signed-magic (divisor &key (bits 64))
  "Return Hacker's Delight style signed magic parameters for positive DIVISOR.

Values are MAGIC, SHIFT, and ADD-P.  The optimizer only emits signed sequences
when interval verification proves Common Lisp FLOOR semantics for the concrete
range being optimized; this helper records the fixed-width parameters for tests
and future native lowering hooks."
  (when (and (integerp divisor) (> divisor 1) (not (opt-power-of-2-p divisor)))
    (let* ((two-to-w-1 (ash 1 (1- bits)))
           (anc (- (1- two-to-w-1)
                   (mod (1- two-to-w-1) divisor)))
           (p (1- bits))
           (q1 (floor two-to-w-1 anc))
           (r1 (- two-to-w-1 (* q1 anc)))
           (q2 (floor two-to-w-1 divisor))
           (r2 (- two-to-w-1 (* q2 divisor))))
      (loop
        (incf p)
        (setf q1 (* 2 q1)
              r1 (* 2 r1))
        (when (>= r1 anc)
          (incf q1)
          (decf r1 anc))
        (setf q2 (* 2 q2)
              r2 (* 2 r2))
        (when (>= r2 divisor)
          (incf q2)
          (decf r2 divisor))
        (let ((delta (- divisor 1 r2)))
          (unless (and (< p (* 2 bits))
                       (or (< q1 delta)
                           (and (= q1 delta) (zerop r1))))
            (return (values (1+ q2) (- p bits) nil))))))))

(defun %opt-div-const-quotient-seq (dst src divisor interval new-reg-fn)
  "Build a multiply+shift quotient sequence for SRC/DIVISOR when safe."
  (or (%opt-div-by-verified-reciprocal-seq dst src divisor interval new-reg-fn)
      (%opt-div-by-verified-reciprocal-seq-with-bias dst src divisor interval new-reg-fn)
      (%opt-div-by-unsigned-magic-seq dst src divisor interval new-reg-fn)))

(defun %opt-mod-by-const-seq (dst src divisor interval new-reg-fn)
  "Build mod via q = div-const(src), dst = src - divisor*q."
  (let ((q-reg (funcall new-reg-fn)))
    (let ((q-seq (%opt-div-const-quotient-seq q-reg src divisor interval new-reg-fn)))
      (when q-seq
        (let ((divisor-reg (funcall new-reg-fn))
              (prod-reg (funcall new-reg-fn)))
          (append q-seq
                  (list (make-vm-const :dst divisor-reg :value divisor)
                        (make-vm-mul :dst prod-reg :lhs q-reg :rhs divisor-reg)
                        (make-vm-sub :dst dst :lhs src :rhs prod-reg))))))))

(defun %opt-pass-div-by-const/fr685 (instructions)
  "FR-685: lower safe integer division/modulo by known non-zero constants.

Power-of-two divisors are intentionally left for opt-pass-strength-reduce's
vm-ash lowering.  Non-power-of-two lowering uses verified reciprocal sequences
for bounded signed/unsigned intervals and unsigned 64-bit magic multiply-high
when range facts prove the dividend is an unsigned word."
  (let* ((env (make-hash-table :test #'eq))
         (intervals (make-hash-table :test #'eq))
         (counter (1+ (opt-max-reg-index instructions)))
         (result nil))
    (flet ((new-reg ()
             (prog1 (intern (format nil "R~A" counter) :keyword)
               (incf counter)))
           (const-val (reg)
             (gethash reg env))
           (emit-seq (seq)
             (dolist (inst seq) (push inst result)))
           (kill-dst (inst)
             (let ((dst (opt-inst-dst inst)))
               (when dst (remhash dst env))))
           (advance (inst)
             (%opt-transfer-interval-inst inst intervals)))
      (dolist (inst instructions)
        (typecase inst
          (vm-label
           (clrhash env)
           (clrhash intervals)
           (push inst result))
          (vm-const
           (setf (gethash (vm-dst inst) env) (vm-value inst))
           (push inst result)
           (advance inst))
          (vm-div
           (let* ((dst (vm-dst inst))
                  (lhs (vm-lhs inst))
                  (divisor (const-val (vm-rhs inst)))
                  (interval (gethash lhs intervals))
                  (seq (and (integerp divisor)
                            (not (zerop divisor))
                            (> divisor 1)
                            (not (opt-power-of-2-p divisor))
                            (%opt-div-const-quotient-seq dst lhs divisor interval #'new-reg))))
             (if seq
                 (progn (remhash dst env) (emit-seq seq))
                 (progn (kill-dst inst) (push inst result)))
             (advance inst)))
          (vm-mod
           (let* ((dst (vm-dst inst))
                  (lhs (vm-lhs inst))
                  (divisor (const-val (vm-rhs inst)))
                  (interval (gethash lhs intervals))
                  (seq (and (integerp divisor)
                            (not (zerop divisor))
                            (> divisor 1)
                            (not (opt-power-of-2-p divisor))
                            (%opt-mod-by-const-seq dst lhs divisor interval #'new-reg))))
             (if seq
                 (progn (remhash dst env) (emit-seq seq))
                 (progn (kill-dst inst) (push inst result)))
             (advance inst)))
          (t
           (kill-dst inst)
           (push inst result)
           (advance inst)))))
    (nreverse result)))
