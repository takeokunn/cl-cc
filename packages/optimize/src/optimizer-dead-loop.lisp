(in-package :cl-cc/optimize)

;;; ─── FR-686: Dead Loop Elimination ───────────────────────────────────────
;;;
;;; Remove conservative while-shaped loops whose loop-local computations are
;;; pure, whose values are unused outside the loop, and whose body contains no
;;; volatility marker.  This pass is intentionally narrower than DCE: it removes
;;; the loop control skeleton only after proving the enclosed work is dead.

(defun %dead-loop-volatile-marker-p (inst)
  "Return T when INST carries an explicit volatility marker.

Volatility is represented today by optimizer metadata encoded as constants in
tests/frontends, or by any non-pure effectful VM instruction.  Treat these as
hard barriers so volatile loops are never deleted."
  (or (and (typep inst 'vm-const)
           (member (vm-value inst) '(:volatile :volatile-write volatile volatile-write)
                   :test #'eq))
      (member (vm-inst-effect-kind inst) '(:write-global :io :control :unknown)
              :test #'eq)))

(defun %dead-loop-removable-inst-p (inst)
  "Return T when INST may be removed as part of a dead loop body."
  (and (not (%dead-loop-volatile-marker-p inst))
       (or (opt-inst-pure-p inst)
           (member (vm-inst-effect-kind inst) '(:alloc) :test #'eq))))

(defun %dead-loop-read-regs-after (vec start)
  "Return an EQ set of registers read by VEC from START to end."
  (let ((reads (make-hash-table :test #'eq)))
    (loop for i from start below (length vec)
          do (dolist (reg (opt-inst-read-regs (aref vec i)))
               (setf (gethash reg reads) t)))
    reads))

(defun %dead-loop-defined-regs (insts)
  "Return an EQ set of destination registers defined by INSTS."
  (let ((defs (make-hash-table :test #'eq)))
    (dolist (inst insts defs)
      (let ((dst (opt-inst-dst inst)))
        (when dst (setf (gethash dst defs) t))))))

(defun %dead-loop-defs-unused-after-p (insts read-after)
  "Return T when no destination defined by INSTS is read after the loop."
  (let ((defs (%dead-loop-defined-regs insts)))
    (loop for reg being the hash-keys of defs
          never (gethash reg read-after))))

(defun %dead-loop-body-pure-p (insts)
  "Return T when every instruction in INSTS is removable and non-volatile."
  (every #'%dead-loop-removable-inst-p insts))

(defun %dead-loop-linear-candidate-at (vec i)
  "Return a removable loop plist beginning at I, or NIL.

Matches:
  label, pure-cond-inst, jump-zero exit, pure-body*, jump header, exit-label"
  (let ((n (length vec)))
    (when (and (< (+ i 4) n)
               (vm-label-p (aref vec i)))
      (let* ((header (aref vec i))
             (header-name (vm-name header))
             (cond-inst (aref vec (+ i 1)))
             (branch-inst (aref vec (+ i 2))))
        (when (and (%dead-loop-removable-inst-p cond-inst)
                   (typep branch-inst 'vm-jump-zero))
          (let* ((exit-name (vm-label-name branch-inst))
                 (exit-pos (cfg-find-label-position vec n exit-name))
                 (back-pos (and exit-pos (1- exit-pos)))
                 (back-inst (and back-pos (>= back-pos 0) (aref vec back-pos))))
            (when (and exit-pos
                       (> exit-pos (+ i 3))
                       (typep back-inst 'vm-jump)
                       (equal (vm-label-name back-inst) header-name))
              (let ((loop-work (cons cond-inst
                                     (loop for j from (+ i 3) below back-pos
                                           collect (aref vec j)))))
                (when (and (%dead-loop-body-pure-p loop-work)
                           (%dead-loop-defs-unused-after-p
                            loop-work
                            (%dead-loop-read-regs-after vec exit-pos)))
                  (list :start i :exit-pos exit-pos))))))))))

(defun %dead-loop-remove-linear (instructions)
  "Remove all conservative dead loops found by linear matching."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (result nil)
         (i 0)
         (changed nil))
    (loop while (< i n)
          do (let ((candidate (%dead-loop-linear-candidate-at vec i)))
               (if candidate
                   (progn
                     (setf changed t
                           i (getf candidate :exit-pos)))
                   (progn
                     (push (aref vec i) result)
                     (incf i)))))
    (values (nreverse result) changed)))

(defun opt-pass-dead-loop-elimination (instructions)
  "FR-686 dead loop elimination.

Runs after ordinary DCE in the default pipeline and removes pure loops whose
loop-defined values are unused after the loop. Any volatile marker or side
effect keeps the loop intact."
  (multiple-value-bind (result changed-p)
      (%dead-loop-remove-linear instructions)
    (declare (ignore changed-p))
    result))
