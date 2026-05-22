(in-package :cl-cc/optimize)

;;; ─── FR-682: Loop Peeling ────────────────────────────────────────────────

(defparameter *opt-loop-peel-default-count* 1
  "Default FR-682 peel count for loops with boundary checks.")

(defparameter *opt-loop-peel-simd-count* 4
  "Conservative peel count used when a loop body already contains SIMD markers.")

(defun %opt-loop-peel-boundary-check-inst-p (inst iv-reg)
  "Return T when INST is a first-iteration boundary/null/bounds check candidate."
  (or (and (typep inst '(or vm-aref vm-aset))
           (eq (vm-index-reg inst) iv-reg))
      (and (typep inst '(or vm-null-p vm-cons-p vm-symbol-p vm-number-p vm-integer-p vm-function-p))
           (vm-src inst))))

(defun %opt-loop-peel-body-needs-peeling-p (body iv-reg)
  "Return T when BODY contains a check that benefits from first-iteration peeling."
  (some (lambda (inst) (%opt-loop-peel-boundary-check-inst-p inst iv-reg)) body))

(defun %opt-loop-peel-safe-body-p (body)
  "Return T when BODY is safe to duplicate before the loop header."
  (and (notany #'vm-label-p body)
       ;; Skip loops with internal control transfer; preserving those requires CFG cloning.
       (notany (lambda (inst) (typep inst '(or vm-jump vm-jump-zero vm-ret))) body)
       ;; Skip stateful calls whose first iteration may have special observable behavior.
       (notany (lambda (inst) (typep inst '(or vm-call vm-tail-call vm-trampoline))) body)))

(defun %opt-loop-peel-count (body)
  "Return a conservative peel count for BODY."
  (if (some (lambda (inst) (typep inst 'vm-simd-vector-op)) body)
      *opt-loop-peel-simd-count*
      *opt-loop-peel-default-count*))

(defun %opt-loop-peel-mark-bce (body iv-reg)
  "Mark peeled array accesses as BCE-eligible and return BODY."
  (dolist (inst body body)
    (when (and (typep inst '(or vm-aref vm-aset))
               (eq (vm-index-reg inst) iv-reg))
      (opt-mark-bounds-check-eliminable inst
                                        :array-reg (vm-array-reg inst)
                                        :index-reg (vm-index-reg inst)
                                        :block :peeled-first-iteration))))

(defun %opt-loop-peel-emit-copies (cond-inst jz-inst body count result)
  "Emit COUNT guarded peeled copies into RESULT."
  (dotimes (_ count result)
    (push cond-inst result)
    (push jz-inst result)
    (dolist (inst body)
      (push inst result))))

(defun %opt-loop-peel-apply-candidate (instructions candidate)
  "Apply FR-682 peeling to a counted-loop CANDIDATE, or return NIL if unsafe."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (header-pos (getf candidate :header-pos))
         (exit-pos (getf candidate :exit-pos))
         (cond-inst (getf candidate :cmp))
         (jz-inst (getf candidate :jump-zero))
         (body (getf candidate :body))
         (iv-reg (getf candidate :iv-reg)))
    (when (and (%opt-loop-peel-safe-body-p body)
               (%opt-loop-peel-body-needs-peeling-p body iv-reg))
      (let ((result nil)
            (peeled-body (%opt-loop-peel-mark-bce (copy-list body) iv-reg)))
        (loop for i from 0 below header-pos
              do (push (aref vec i) result))
        (setf result (%opt-loop-peel-emit-copies cond-inst jz-inst peeled-body
                                                 (%opt-loop-peel-count body)
                                                 result))
        (loop for i from header-pos below n
              do (push (aref vec i) result))
        (nreverse result)))))

(defun opt-pass-loop-peel (instructions)
  "FR-682 loop peeling pass.

Peels the first iteration of conservative single-latch counted loops whose body
contains array boundary checks or null/type checks.  The peeled copy is emitted
before the original header with the original guard, and array accesses in the
peeled copy are annotated for BCE.  Loops with labels, calls, internal branches,
or other shapes that could make the first iteration semantically distinct are
left unchanged."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (result nil)
         (i 0))
    (loop while (< i n) do
      (let ((candidate (%opt-counted-loop-candidate-at vec i)))
        (if candidate
            (let ((replacement (%opt-loop-peel-apply-candidate instructions candidate)))
              (if replacement
                  (return-from opt-pass-loop-peel replacement)
                  (progn (push (aref vec i) result) (incf i))))
            (progn (push (aref vec i) result) (incf i)))))
    (nreverse result)))

(unless (fboundp 'opt-pass-loop-peeling)
  (defun opt-pass-loop-peeling (instructions)
    "Compatibility alias for the FR-682 loop peeling pass."
    (opt-pass-loop-peel instructions)))
