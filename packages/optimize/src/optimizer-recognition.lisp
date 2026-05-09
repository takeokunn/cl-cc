;;;; optimizer-recognition.lisp — Tree Pattern Recognition Passes
;;;;
;;;; Bswap (byte-swap) and rotate instruction recognition.
;;;; Scans the instruction stream for multi-instruction idioms and replaces
;;;; them with single specialized instructions (vm-bswap, vm-rotate).
;;;;
;;;; Load order: after optimizer-strength.lisp.

(in-package :cl-cc/optimize)

(defun opt-bswap-recognition-match-at (instructions pos)
  (let ((end (+ pos 19)))
    (when (<= end (length instructions))
      (let* ((c0 (nth (+ pos 0) instructions))
             (a0 (nth (+ pos 1) instructions))
             (s0 (nth (+ pos 2) instructions))
             (b0 (nth (+ pos 3) instructions))
             (c1 (nth (+ pos 4) instructions))
             (a1 (nth (+ pos 5) instructions))
             (s1 (nth (+ pos 6) instructions))
             (b1 (nth (+ pos 7) instructions))
             (c2 (nth (+ pos 8) instructions))
             (a2 (nth (+ pos 9) instructions))
             (s2 (nth (+ pos 10) instructions))
             (b2 (nth (+ pos 11) instructions))
             (c3 (nth (+ pos 12) instructions))
             (a3 (nth (+ pos 13) instructions))
             (s3 (nth (+ pos 14) instructions))
             (b3 (nth (+ pos 15) instructions))
             (o0 (nth (+ pos 16) instructions))
             (o1 (nth (+ pos 17) instructions))
             (o2 (nth (+ pos 18) instructions)))
        (when (and (typep c0 'vm-const)
                   (eql (vm-value c0) #xFF)
                   (typep a0 'vm-logand)
                   (typep s0 'vm-const)
                   (typep b0 'vm-ash)
                   (typep c1 'vm-const)
                   (eql (vm-value c1) #xFF00)
                   (typep a1 'vm-logand)
                   (typep s1 'vm-const)
                   (typep b1 'vm-ash)
                   (typep c2 'vm-const)
                   (eql (vm-value c2) #xFF0000)
                   (typep a2 'vm-logand)
                   (typep s2 'vm-const)
                   (typep b2 'vm-ash)
                   (typep c3 'vm-const)
                   (eql (vm-value c3) #xFF000000)
                   (typep a3 'vm-logand)
                   (typep s3 'vm-const)
                   (typep b3 'vm-ash)
                   (typep o0 'vm-logior)
                   (typep o1 'vm-logior)
                   (typep o2 'vm-logior))
            (let* ((src (vm-lhs a0))
                   (dst (vm-dst o2)))
              (when (and (eq (vm-lhs a0) src)
                         (eq (vm-rhs a0) (vm-dst c0))
                         (eq (vm-lhs a1) src)
                         (eq (vm-rhs a1) (vm-dst c1))
                         (eq (vm-lhs a2) src)
                         (eq (vm-rhs a2) (vm-dst c2))
                         (eq (vm-lhs a3) src)
                         (eq (vm-rhs a3) (vm-dst c3))
                         (equal (mapcar #'vm-value (list s0 s1 s2 s3)) '(24 8 -8 -24))
                         (eq (vm-lhs b0) (vm-dst a0))
                         (eq (vm-rhs b0) (vm-dst s0))
                         (eq (vm-lhs b1) (vm-dst a1))
                         (eq (vm-rhs b1) (vm-dst s1))
                         (eq (vm-lhs b2) (vm-dst a2))
                         (eq (vm-rhs b2) (vm-dst s2))
                         (eq (vm-lhs b3) (vm-dst a3))
                         (eq (vm-rhs b3) (vm-dst s3))
                         (eq (vm-lhs o0) (vm-dst b0))
                         (eq (vm-rhs o0) (vm-dst b1))
                         (eq (vm-lhs o1) (vm-dst b2))
                         (eq (vm-rhs o1) (vm-dst b3))
                         (eq (vm-lhs o2) (vm-dst o0))
                         (eq (vm-rhs o2) (vm-dst o1)))
                (values (make-vm-bswap :dst dst :src src) 19))))))))

;;; Shared skeleton for single-pass recognition passes.
;;; MATCH-FN: (instructions pos) → (values rewritten consumed) or (nil 0)
;;; PUSH-FN:  (rewritten result) → new result with rewritten prepended
(defun %opt-recognition-pass (instructions match-fn push-fn)
  (loop with n      = (length instructions)
        with result = nil
        with i      = 0
        while (< i n)
        do (multiple-value-bind (rewritten consumed)
               (funcall match-fn instructions i)
             (if rewritten
                 (progn (setf result (funcall push-fn rewritten result))
                        (incf i consumed))
                 (progn (push (nth i instructions) result)
                        (incf i 1))))
        finally (return (nreverse result))))

(defun opt-pass-bswap-recognition (instructions)
  "Collapse explicit byte-swap bit-manipulation trees into vm-bswap.

   Recognizes the canonical 32-bit reverse-bytes tree built from four masked
   extracts, four shifts, and three logior nodes, matching the bswap pattern
   documented for the backend."
  (%opt-recognition-pass instructions
                         #'opt-bswap-recognition-match-at
                         (lambda (rewritten result) (cons rewritten result))))

(defun opt-rotate-recognition-match-at (instructions pos)
  (let ((end (+ pos 5)))
    (when (<= end (length instructions))
      (let* ((c0 (nth (+ pos 0) instructions))
             (a0 (nth (+ pos 1) instructions))
             (c1 (nth (+ pos 2) instructions))
             (a1 (nth (+ pos 3) instructions))
             (o0 (nth (+ pos 4) instructions)))
        (when (and (typep c0 'vm-const)
                   (typep a0 'vm-ash)
                   (typep c1 'vm-const)
                   (typep a1 'vm-ash)
                   (typep o0 'vm-logior))
          (let* ((k0 (vm-value c0))
                 (k1 (vm-value c1))
                 (src0 (vm-lhs a0))
                 (src1 (vm-lhs a1))
                 (count0 (vm-rhs a0))
                 (count1 (vm-rhs a1))
                 (dst0 (vm-dst a0))
                 (dst1 (vm-dst a1))
                 (out-dst (vm-dst o0)))
            (when (and (integerp k0)
                       (integerp k1)
                       (eq src0 src1)
                       (eq count0 (vm-dst c0))
                       (eq count1 (vm-dst c1))
                       (or (and (eq (vm-lhs o0) dst0) (eq (vm-rhs o0) dst1))
                           (and (eq (vm-lhs o0) dst1) (eq (vm-rhs o0) dst0))))
              (cond
                ((and (plusp k0) (= k1 (- k0 64)))
                 (values (list (make-vm-const :dst (vm-dst c1)
                                              :value (mod (- 64 k0) 64))
                               (make-vm-rotate :dst out-dst
                                               :lhs src0
                                               :rhs (vm-dst c1)))
                         5))
                ((and (plusp k1) (= k0 (- k1 64)))
                 (values (list (make-vm-const :dst (vm-dst c0)
                                              :value (mod (- 64 k1) 64))
                               (make-vm-rotate :dst out-dst
                                               :lhs src0
                                               :rhs (vm-dst c0)))
                         5))))))))))

(defun opt-pass-rotate-recognition (instructions)
  "Collapse rotate idioms into vm-rotate.

   Recognizes the classic two-shift + OR tree that implements a 64-bit rotate
   and replaces it with a single vm-rotate plus the normalized count constant."
  (%opt-recognition-pass instructions
                          #'opt-rotate-recognition-match-at
                          (lambda (rewritten result)
                            (loop for inst in rewritten
                                  do (push inst result)
                                  finally (return result)))))

(defun %opt-branch-target-counts (instructions)
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (inst instructions counts)
      (when (or (typep inst 'vm-jump)
                (typep inst 'vm-jump-zero))
        (incf (gethash (vm-label-name inst) counts 0))))))

(defun %opt-distinct-registers-p (&rest regs)
  (loop for tail on (remove nil regs)
        always (not (member (car tail) (cdr tail) :test #'eq))))

(defun %opt-fill-loop-replacement (array-reg val-reg len-reg idx-reg next-reg cond-reg one-reg)
  (list (make-vm-fill :array-reg array-reg :val-reg val-reg)
        (make-vm-move :dst next-reg :src len-reg)
        (make-vm-move :dst idx-reg :src len-reg)
        (make-vm-const :dst cond-reg :value 0)
        (make-vm-const :dst one-reg :value 1)))

(defun opt-fill-recognition-match-at (instructions pos)
  "Recognize a canonical full-vector fill loop and replace it with vm-fill.

   The recognized form is intentionally narrow: ARRAY-LENGTH immediately before
   the induction initialization, zero-based index, `< length` guard, exactly one
   ASET store, increment by one, and private loop/exit labels."
  (let ((end (+ pos 10)))
    (when (and (> pos 0)
               (<= end (length instructions)))
      (let* ((len-inst   (nth (1- pos) instructions))
             (init       (nth (+ pos 0) instructions))
             (header     (nth (+ pos 1) instructions))
             (cmp        (nth (+ pos 2) instructions))
             (exit-jump  (nth (+ pos 3) instructions))
             (store      (nth (+ pos 4) instructions))
             (one        (nth (+ pos 5) instructions))
             (inc        (nth (+ pos 6) instructions))
             (step       (nth (+ pos 7) instructions))
             (back-jump  (nth (+ pos 8) instructions))
             (exit-label (nth (+ pos 9) instructions)))
        (when (and (typep len-inst 'vm-array-length)
                   (typep init 'vm-const)
                   (eql (vm-value init) 0)
                   (typep header 'vm-label)
                   (typep cmp 'vm-lt)
                   (typep exit-jump 'vm-jump-zero)
                   (typep store 'vm-aset)
                   (typep one 'vm-const)
                   (eql (vm-value one) 1)
                   (typep inc 'vm-add)
                   (typep step 'vm-move)
                   (typep back-jump 'vm-jump)
                   (typep exit-label 'vm-label))
          (let* ((array-reg (vm-src len-inst))
                 (len-reg   (vm-dst len-inst))
                 (idx-reg   (vm-dst init))
                 (cond-reg  (vm-dst cmp))
                 (one-reg   (vm-dst one))
                 (next-reg  (vm-dst inc))
                 (val-reg   (vm-val-reg store))
                 (header-name (vm-name header))
                 (exit-name   (vm-name exit-label))
                 (target-counts (%opt-branch-target-counts instructions)))
            (when (and (eq (vm-lhs cmp) idx-reg)
                       (eq (vm-rhs cmp) len-reg)
                       (eq (vm-reg exit-jump) cond-reg)
                       (equal (vm-label-name exit-jump) exit-name)
                       (eq (vm-array-reg store) array-reg)
                       (eq (vm-index-reg store) idx-reg)
                       (or (and (eq (vm-lhs inc) idx-reg)
                                (eq (vm-rhs inc) one-reg))
                           (and (eq (vm-lhs inc) one-reg)
                                (eq (vm-rhs inc) idx-reg)))
                       (eq (vm-dst step) idx-reg)
                       (eq (vm-src step) next-reg)
                       (equal (vm-label-name back-jump) header-name)
                       (= (gethash header-name target-counts 0) 1)
                       (= (gethash exit-name target-counts 0) 1)
                       (%opt-distinct-registers-p len-reg idx-reg cond-reg one-reg next-reg)
                       (not (member array-reg (list idx-reg cond-reg one-reg next-reg) :test #'eq))
                       (not (member val-reg (list idx-reg cond-reg one-reg next-reg) :test #'eq)))
              (values (%opt-fill-loop-replacement array-reg val-reg len-reg idx-reg next-reg cond-reg one-reg)
                      10))))))))

(defun opt-pass-fill-recognition (instructions)
  "Collapse a canonical zero-based array fill loop into a side-effecting vm-fill.

   This is the conservative FR-055 slice for full-vector fill idioms.  The pass
   preserves final loop temporaries after the bulk fill so later code observing
   the induction or condition registers keeps the same values."
  (loop with n      = (length instructions)
        with result = nil
        with i      = 0
        while (< i n)
        do (multiple-value-bind (rewritten consumed)
               (opt-fill-recognition-match-at instructions i)
             (if (and rewritten (integerp consumed) (plusp consumed))
                 (progn
                   (dolist (inst rewritten)
                     (push inst result))
                   (incf i consumed))
                 (progn
                   (push (nth i instructions) result)
                   (incf i 1))))
        finally (return (nreverse result))))
