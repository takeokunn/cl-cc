;;;; packages/emit/src/regalloc.lisp - Linear Scan Register Allocator
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Regalloc — Data Structures, Def/Use Analysis, and Liveness Computation
;;;
;;; Maps unlimited virtual registers (VM :R0, :R1, ...) to physical
;;; machine registers. Uses linear scan with liveness analysis.
;;; Spills to stack when registers are exhausted.
;;;
;;; Contains: live-interval/regalloc-result defstructs, instruction-defs/uses
;;; methods, def-binop-like/def-unary-like macros, compute-live-intervals.
;;;
;;; Linear scan allocation, spill code insertion, and public API are in
;;; regalloc-allocate.lisp (loads immediately after this file).
;;;
;;; Load order: before regalloc-allocate.lisp. Consumes target-desc from cl-cc/mir.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/regalloc)

;;; Data Structures

(defstruct (live-interval (:conc-name interval-))
  "Live interval for register allocation."
  (vreg nil)
  (start nil)
  (end nil)
  (use-positions nil)
  (parameter-index nil)
  (coalesce-with nil)
  (crosses-call-p nil)
  (fp-p nil)
  (remat-const nil)
  (return-value-p nil)
  (phys-reg nil)
  (spill-slot nil))

(defstruct (regalloc-result (:conc-name regalloc-))
  (assignment nil)
  (spill-map nil)
  (spill-count 0 :type fixnum)
  (instructions nil :type list))

;;; Instruction Def/Use Analysis

(defgeneric instruction-defs (inst)
  (:documentation "Returns list of virtual registers defined (written) by INST."))

(defgeneric instruction-uses (inst)
  (:documentation "Returns list of virtual registers used (read) by INST."))

;; All instruction-defs/instruction-uses methods are in regalloc-defs-uses.lisp (loaded next).

;;; Liveness Analysis

(defun build-label-map (instructions)
  "Build a hash table mapping label names to instruction indices."
  (let ((map (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for i from 0
          when (typep inst 'vm-label)
          do (setf (gethash (vm-name inst) map) i))
    map))

(defun compute-live-intervals (instructions &optional float-vregs)
  "Compute live intervals for all virtual registers.
   Returns a list of live-interval objects sorted by start point."
  (let ((intervals (make-hash-table :test #'eq))
        (label-map (build-label-map instructions))
        (inst-vec (coerce instructions 'vector))
        (parameter-counter 0))
    (declare (ignore inst-vec))
    ;; Forward pass to collect initial def/use info
    (loop for inst in instructions
          for i from 0
          do (dolist (vreg (instruction-defs inst))
               (when vreg
                  (unless (gethash vreg intervals)
                    (setf (gethash vreg intervals)
                           (make-live-interval
                            :vreg vreg :start i :end i
                            :fp-p (and float-vregs (gethash vreg float-vregs))
                            :coalesce-with (when (typep inst 'vm-move)
                                             (vm-src inst)))))))
              (dolist (vreg (instruction-uses inst))
                (when vreg
                  (let ((interval (gethash vreg intervals)))
                    (if interval
                        (progn
                          (setf (interval-end interval) (max (interval-end interval) i))
                          (push i (interval-use-positions interval))
                          (when (typep inst 'vm-ret)
                            (setf (interval-return-value-p interval) t)))
                        (setf (gethash vreg intervals)
                              (make-live-interval
                                             :vreg vreg :start 0 :end i
                                             :use-positions (list i)
                                             :fp-p (and float-vregs (gethash vreg float-vregs))
                                             :parameter-index (prog1 parameter-counter
                                                                (incf parameter-counter))
                                             :return-value-p (typep inst 'vm-ret))))))))
    ;; Extend intervals across jumps (conservative)
    (loop for inst in instructions
          for i from 0
          when (and (typep inst 'vm-jump)
                    (gethash (vm-label-name inst) label-map))
           do (let ((target (gethash (vm-label-name inst) label-map)))
                (maphash (lambda (vreg interval)
                           (declare (ignore vreg))
                           (when (and (<= (interval-start interval) i)
                                      (>= (interval-end interval) target))
                             (setf (interval-start interval)
                                   (min (interval-start interval) target))
                             (setf (interval-end interval)
                                   (max (interval-end interval) i))))
                         intervals))
           when (and (typep inst 'vm-jump-zero)
                     (gethash (vm-label-name inst) label-map))
           do (let ((target (gethash (vm-label-name inst) label-map)))
                (maphash (lambda (vreg interval)
                           (declare (ignore vreg))
                           (when (and (<= (interval-start interval) i)
                                      (>= (interval-end interval) target))
                             (setf (interval-start interval)
                                   (min (interval-start interval) target))
                             (setf (interval-end interval)
                                   (max (interval-end interval) i))))
                         intervals)))
    ;; Mark intervals that span over a call site.
    (loop for inst in instructions
          for i from 0
          when (typep inst '(or vm-call vm-tail-call))
          do (maphash (lambda (vreg interval)
                        (declare (ignore vreg))
                        (when (and (< (interval-start interval) i)
                                   (> (interval-end interval) i))
                          (setf (interval-crosses-call-p interval) t)))
                      intervals))

    ;; Sort by start point
    (let ((result nil))
      (maphash (lambda (vreg interval)
                  (declare (ignore vreg))
                  (setf (interval-use-positions interval)
                        (sort (copy-list (interval-use-positions interval)) #'<))
                  (push interval result))
                intervals)
      (sort result #'< :key #'interval-start))))

