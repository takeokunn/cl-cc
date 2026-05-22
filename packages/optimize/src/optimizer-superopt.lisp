;;;; packages/optimize/src/optimizer-superopt.lisp — FR-750 peephole superoptimizer

(in-package :cl-cc/optimize)

(defparameter *opt-superopt-max-length* 3
  "Maximum peephole window length enumerated by OPT-PASS-SUPEROPT.")

(defparameter *opt-superopt-input-space* '(-1 0 1 2 nil t)
  "Exhaustive scalar input values used for peephole equivalence testing.")

(defun %opt-superopt-move-window-p (window)
  "Return true when WINDOW contains only VM-MOVE instructions."
  (every (lambda (inst) (typep inst 'vm-move)) window))

(defun %opt-superopt-window-registers (window)
  "Collect registers mentioned by WINDOW in stable first-seen order."
  (let ((regs nil))
    (dolist (inst window (nreverse regs))
      (dolist (reg (remove nil (cons (opt-inst-dst inst)
                                     (opt-inst-read-regs inst))))
        (pushnew reg regs :test #'eq)))))

(defun %opt-superopt-input-states (registers)
  "Enumerate all input register environments over *OPT-SUPEROPT-INPUT-SPACE*."
  (labels ((rec (remaining)
             (if (null remaining)
                 (list nil)
                 (loop for state in (rec (cdr remaining)) append
                   (loop for value in *opt-superopt-input-space*
                         collect (acons (car remaining) value state))))))
    (rec registers)))

(defun %opt-superopt-exec-move-sequence (sequence state)
  "Interpret move-only SEQUENCE over STATE and return a fresh final state alist."
  (let ((env (copy-alist state)))
    (dolist (inst sequence env)
      (let ((dst (vm-dst inst))
            (src (vm-src inst)))
        (setf (cdr (or (assoc dst env :test #'eq)
                       (car (push (cons dst nil) env))))
              (cdr (assoc src env :test #'eq)))))))

(defun %opt-superopt-states-equivalent-p (left right registers)
  "Return true when LEFT and RIGHT agree for every observed REGISTER."
  (every (lambda (reg)
           (equal (cdr (assoc reg left :test #'eq))
                  (cdr (assoc reg right :test #'eq))))
         registers))

(defun %opt-superopt-equivalent-p (original candidate registers)
  "Exhaustively test ORIGINAL and CANDIDATE against all register input states."
  (every (lambda (state)
           (%opt-superopt-states-equivalent-p
            (%opt-superopt-exec-move-sequence original state)
            (%opt-superopt-exec-move-sequence candidate state)
            registers))
         (%opt-superopt-input-states registers)))

(defun %opt-superopt-enumerate-move-candidates (registers max-length)
  "Enumerate all VM-MOVE instruction sequences up to MAX-LENGTH."
  (labels ((moves ()
             (loop for dst in registers append
               (loop for src in registers
                     collect (make-vm-move :dst dst :src src))))
           (seqs (length)
             (if (zerop length)
                 (list nil)
                 (loop for prefix in (seqs (1- length)) append
                   (loop for move in (moves)
                         collect (append prefix (list move)))))))
    (loop for length from 0 to max-length append (seqs length))))

(defun %opt-superopt-find-shorter-equivalent (window)
  "Return a shorter equivalent replacement for WINDOW, or NIL."
  (when (%opt-superopt-move-window-p window)
    (let ((registers (%opt-superopt-window-registers window)))
      (when registers
        (find-if (lambda (candidate)
                   (%opt-superopt-equivalent-p window candidate registers))
                 (%opt-superopt-enumerate-move-candidates
                  registers (1- (length window))))))))

(defun %opt-superopt-rewrite-once (instructions)
  "Apply the first discovered FR-750 peephole rewrite."
  (let ((max-window (min *opt-superopt-max-length* (length instructions))))
    (loop for prefix on instructions
          for index from 0 do
            (loop for width from max-window downto 1
                  when (<= width (length prefix)) do
                    (let* ((window (subseq prefix 0 width))
                           (replacement (%opt-superopt-find-shorter-equivalent window)))
                      (when replacement
                        (return-from %opt-superopt-rewrite-once
                          (values (append (subseq instructions 0 index)
                                          replacement
                                          (nthcdr (+ index width) instructions))
                                  t))))))
    (values instructions nil)))

(defun opt-pass-superopt (instructions)
  "FR-750 peephole superoptimizer.

Enumerates move-instruction sequences up to *OPT-SUPEROPT-MAX-LENGTH* (default
N=3), exhaustively tests them over *OPT-SUPEROPT-INPUT-SPACE*, and replaces a
window with the shortest equivalent sequence found.  The implementation is
deliberately infrastructure-focused and side-effect-safe: only move-only windows
are interpreted, so labels, jumps, calls, allocations, and VM semantics outside
register copies are never changed."
  (loop with current = instructions
        for changed = nil do
          (multiple-value-setq (current changed)
            (%opt-superopt-rewrite-once current))
        while changed
        finally (return current)))
