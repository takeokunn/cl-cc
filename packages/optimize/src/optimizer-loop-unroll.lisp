;;;; optimizer-loop-unroll.lisp — FR-601 loop unrolling

(in-package :cl-cc/optimize)

(defparameter *opt-loop-unroll-fr601-factor* 4
  "Partial unroll factor for FR-601 unknown or large counted loops.")

(defparameter *opt-loop-unroll-fr601-max-full-trip* 8
  "Maximum compile-time trip count eligible for full FR-601 unrolling.")

(defun %loop-unroll-copy-inst (inst)
  "Return a structural copy of INST when the VM sexp codec supports it."
  (handler-case
      (sexp->instruction (instruction->sexp inst))
    (error () inst)))

(defun %loop-unroll-safe-body-p (body)
  "Return T when BODY has no internal labels or control-flow transfers."
  (notany (lambda (inst)
            (typep inst '(or vm-label vm-jump vm-jump-zero vm-ret vm-halt)))
          body))

(defun %loop-unroll-final-step-p (inst cmp-inst)
  "Return T when INST is the canonical induction update for CMP-INST."
  (and (typep inst 'vm-add)
       (eq (vm-dst inst) (vm-lhs inst))
       (eq (vm-dst inst) (vm-lhs cmp-inst))))

(defun %loop-unroll-optimize-expanded-body (instructions)
  "Run local cleanup requested by FR-601 after code expansion."
  (opt-pass-cse (opt-pass-fold instructions)))

(defun %loop-unroll-full (body trip result)
  "Append TRIP copies of BODY to RESULT and return the updated reversed list."
  (dotimes (_ trip result)
    (dolist (inst body)
      (push (%loop-unroll-copy-inst inst) result))))

(defun %loop-unroll-partial (body cmp-inst jz-inst result)
  "Append a guarded factor-4 unrolled prefix to RESULT."
  (dotimes (_ *opt-loop-unroll-fr601-factor* result)
    (push (%loop-unroll-copy-inst cmp-inst) result)
    (push (%loop-unroll-copy-inst jz-inst) result)
    (dolist (inst body)
      (push (%loop-unroll-copy-inst inst) result))))

(defun %loop-unroll-candidate-at (vec index)
  "Return a conservative counted-loop candidate beginning at VEC[INDEX]."
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
                (when (and body
                           (%loop-unroll-safe-body-p body)
                           (%loop-unroll-final-step-p step-inst cmp-inst))
                  (list :header header
                        :cmp cmp-inst
                        :jump-zero jz-inst
                        :body body
                        :exit-pos exit-pos
                        :iv-reg (vm-lhs cmp-inst)
                        :limit-reg (vm-rhs cmp-inst)
                        :step-reg (vm-rhs step-inst)))))))))))

(defun opt-pass-loop-unroll (instructions)
  "FR-601: unroll conservative counted loops.

Full unroll is applied when the trip count is known at compile time and is in
the small range 1..8. Unknown or larger loops get a factor-4 guarded prefix plus
the original loop as the residual loop. Expanded bodies are cleaned with constant
folding and CSE before returning."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (result nil)
         (changed nil)
         (i 0))
    (loop while (< i n)
          do (let ((candidate (%loop-unroll-candidate-at vec i)))
               (if candidate
                   (let* ((cmp-inst (getf candidate :cmp))
                          (jz-inst (getf candidate :jump-zero))
                          (body (getf candidate :body))
                          (exit-pos (getf candidate :exit-pos))
                          (const-env (%opt-build-const-env-up-to vec i))
                          (init (gethash (getf candidate :iv-reg) const-env))
                          (limit (gethash (getf candidate :limit-reg) const-env))
                          (step (gethash (getf candidate :step-reg) const-env))
                          (trip (and init limit step
                                     (%opt-loop-unroll-trip-count cmp-inst init limit step))))
                     (cond
                       ((and trip
                             (> trip 0)
                             (<= trip *opt-loop-unroll-fr601-max-full-trip*))
                        (setf result (%loop-unroll-full body trip result)
                              changed t
                              i exit-pos)
                        (push (%loop-unroll-copy-inst (aref vec i)) result)
                        (incf i))
                       ((plusp *opt-loop-unroll-fr601-factor*)
                        (setf result (%loop-unroll-partial body cmp-inst jz-inst result)
                              changed t)
                        (loop for j from i to exit-pos
                              do (push (%loop-unroll-copy-inst (aref vec j)) result))
                        (setf i (1+ exit-pos)))
                       (t
                        (push (aref vec i) result)
                        (incf i))))
                   (progn
                     (push (aref vec i) result)
                     (incf i)))))
    (let ((out (nreverse result)))
      (if changed
          (%loop-unroll-optimize-expanded-body out)
          instructions))))
