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
  (remat-inst nil)
  (return-value-p nil)
  (phys-reg nil)
  (spill-slot nil))

(defstruct (live-range-split-boundary (:conc-name split-boundary-))
  "Internal edge connecting two child intervals across a live-range hole."
  (after-position nil)
  (before-position nil)
  (from-vreg nil)
  (to-vreg nil)
  (slot nil))

(defstruct (regalloc-result (:conc-name regalloc-))
  (assignment nil)
  (spill-map nil)
  (spill-count 0 :type fixnum)
  (gpr-pressure 0 :type fixnum)
  (fp-pressure 0 :type fixnum)
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

(defun regalloc-collect-linear-functions (instructions)
  "Collect linear function bodies from INSTRUCTIONS.

Returns an EQUAL hash-table label -> list-of-instructions (excluding vm-label).
Collection starts at each vm-label and ends before the next vm-label."
  (let ((table (make-hash-table :test #'equal))
        (current-label nil)
        (current-body nil))
    (labels ((flush-current ()
               (when current-label
                 (setf (gethash current-label table) (nreverse current-body)))))
      (dolist (inst instructions)
        (if (typep inst 'vm-label)
            (progn
              (flush-current)
              (setf current-label (vm-name inst)
                    current-body nil))
            (when current-label
              (push inst current-body))))
      (flush-current))
    table))

(defun regalloc-function-leaf-p (body)
  "Return T when BODY has no direct or tail calls."
  (not (some (lambda (inst) (typep inst '(or vm-call vm-tail-call vm-apply))) body)))

(defun regalloc-build-direct-call-graph (instructions)
  "Build conservative label->callees graph from linearized instructions.

Tracks vm-func-ref into registers and resolves vm-call/vm-tail-call targets when
the callee register has a known direct label in the same body."
  (let* ((bodies (regalloc-collect-linear-functions instructions))
         (graph (make-hash-table :test #'equal)))
    (maphash
     (lambda (label body)
       (let ((reg->label (make-hash-table :test #'eq))
             (callees nil))
         (dolist (inst body)
           (typecase inst
             (vm-func-ref
              (setf (gethash (vm-dst inst) reg->label) (vm-label-name inst)))
             (vm-move
              (multiple-value-bind (target found-p)
                  (gethash (vm-src inst) reg->label)
                (if found-p
                    (setf (gethash (vm-dst inst) reg->label) target)
                    (remhash (vm-dst inst) reg->label))))
             ((or vm-call vm-tail-call)
              (multiple-value-bind (target found-p)
                  (gethash (vm-func-reg inst) reg->label)
                (when (and found-p target)
                  (pushnew target callees :test #'equal)))
              (let ((dst (opt-inst-dst inst)))
                (when dst (remhash dst reg->label))))
             (t
              (let ((dst (opt-inst-dst inst)))
                (when dst (remhash dst reg->label))))))
         (setf (gethash label graph) (nreverse callees))))
     bodies)
    graph))

(defun regalloc-compute-interprocedural-hints (instructions)
  "Compute conservative per-function interprocedural register-allocation hints.

Returns EQUAL hash-table label -> plist with:
  :leaf-p              function has no direct calls
  :leaf-callee-chain-p all direct callees are leaf functions

This is a planning oracle for FR-252 and does not mutate allocation policy yet."
  (let* ((bodies (regalloc-collect-linear-functions instructions))
         (graph (regalloc-build-direct-call-graph instructions))
         (hints (make-hash-table :test #'equal))
         (leafs (make-hash-table :test #'equal)))
    (maphash (lambda (label body)
               (setf (gethash label leafs) (regalloc-function-leaf-p body)))
             bodies)
    (maphash
     (lambda (label callees)
       (let ((leaf-p (gethash label leafs))
             (leaf-callee-chain-p t))
         (dolist (callee callees)
           (unless (gethash callee leafs)
             (setf leaf-callee-chain-p nil)
             (return)))
         (setf (gethash label hints)
               (list :leaf-p leaf-p
                     :leaf-callee-chain-p leaf-callee-chain-p))))
     graph)
    hints))

(defun regalloc-build-allocation-policy-from-hints (hints label)
  "Build a conservative allocation policy plist for LABEL from HINTS.

Returned plist keys:
  :prefer-callee-saved-p  -- keep long-lived values in callee-saved regs
  :prefer-caller-saved-p  -- bias toward caller-saved regs for leaf chains

This hook is intentionally side-effect free and can be consumed by allocator
entry points without changing interval semantics."
  (let* ((entry (and hints (gethash label hints)))
         (leaf-p (and entry (getf entry :leaf-p)))
         (leaf-callee-chain-p (and entry (getf entry :leaf-callee-chain-p))))
    (list :prefer-callee-saved-p (not leaf-callee-chain-p)
          :prefer-caller-saved-p (and leaf-p leaf-callee-chain-p))))

(defun %compute-live-intervals-raw (instructions &optional float-vregs)
  "Compute unsplit live intervals for all virtual registers."
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

    ;; FR-296: mark cheap rematerializable definitions. Constants can be
    ;; recreated directly; single pure no-input computations can be cloned at
    ;; reload points when spilling would otherwise force memory traffic.
    (loop for inst in instructions
          for dst = (first (instruction-defs inst))
          when (and dst (gethash dst intervals))
            do (let ((interval (gethash dst intervals)))
                 (cond
                   ((typep inst 'vm-const)
                    (setf (interval-remat-const interval) (vm-value inst)))
                   ((and (opt-inst-pure-p inst)
                         (null (instruction-uses inst))
                         (= (length (instruction-defs inst)) 1))
                    (setf (interval-remat-inst interval) inst)))))

    ;; Sort by start point
    (let ((result nil))
      (maphash (lambda (vreg interval)
                  (declare (ignore vreg))
                  (setf (interval-use-positions interval)
                        (sort (copy-list (interval-use-positions interval)) #'<))
                  (push interval result))
                intervals)
      (sort result #'< :key #'interval-start))))

(defun %split-vreg-name (vreg child-index)
  "Return a deterministic keyword for child CHILD-INDEX of VREG."
  (intern (format nil "~A/SPLIT~D" (symbol-name vreg) child-index) :keyword))

(defun %copy-live-interval-segment (interval vreg start end uses)
  "Copy INTERVAL metadata into a child segment for VREG."
  (make-live-interval
   :vreg vreg
   :start start
   :end end
   :use-positions uses
   :parameter-index (and (= start (interval-start interval))
                         (interval-parameter-index interval))
   :coalesce-with (and (= start (interval-start interval))
                       (interval-coalesce-with interval))
   :crosses-call-p (interval-crosses-call-p interval)
   :fp-p (interval-fp-p interval)
   :remat-const (and (= start (interval-start interval))
                     (interval-remat-const interval))
   :remat-inst (and (= start (interval-start interval))
                    (interval-remat-inst interval))
   :return-value-p (and (= end (interval-end interval))
                        (interval-return-value-p interval))))

(defun split-live-interval (interval minimum-hole-size)
  "Split INTERVAL at use-position holes larger than MINIMUM-HOLE-SIZE.

Returns the child live intervals as the primary value.  A secondary value carries
internal split-boundary records used by the allocator to insert fixed spill
stores/loads at split edges.  The live-interval structure itself stays unchanged
for external callers."
  (let ((uses (sort (copy-list (interval-use-positions interval)) #'<)))
    (if (or (null uses) (null (cdr uses)))
        (values (list interval) nil)
        (let ((segments nil)
              (boundaries nil)
              (segment-start (interval-start interval))
              (segment-uses nil)
              (child-index 0)
              (previous-child-vreg (interval-vreg interval)))
          (labels ((child-vreg (index)
                     (if (zerop index)
                         (interval-vreg interval)
                         (%split-vreg-name (interval-vreg interval) index)))
                   (finish-segment (end)
                     (let ((vreg (child-vreg child-index)))
                       (push (%copy-live-interval-segment
                              interval vreg segment-start end
                              (sort (copy-list segment-uses) #'<))
                             segments)
                       (setf previous-child-vreg vreg))))
            (loop for rest on uses
                  for use = (car rest)
                  for next-use = (cadr rest)
                  do (push use segment-uses)
                     (when (and next-use
                                (> (- next-use use) minimum-hole-size))
                       (finish-segment use)
                       (incf child-index)
                       (let ((next-vreg (child-vreg child-index)))
                         (push (make-live-range-split-boundary
                                :after-position use
                                :before-position next-use
                                :from-vreg previous-child-vreg
                                :to-vreg next-vreg)
                               boundaries))
                       (setf segment-start next-use
                             segment-uses nil)))
            (finish-segment (interval-end interval))
            (values (nreverse segments) (nreverse boundaries)))))))

(defun compute-live-intervals (instructions &optional float-vregs)
  "Compute live intervals for all virtual registers.
   Returns a list of live-interval objects sorted by start point."
  (%compute-live-intervals-raw instructions float-vregs))

(defun regalloc-register-pressure (intervals &key fp-p)
  "Return the peak number of simultaneously live intervals in one register class."
  (let ((events nil))
    (dolist (interval intervals)
      (when (eq (and (interval-fp-p interval) t) (and fp-p t))
        ;; End events sort before start events at the same position, matching the
        ;; linear-scan expiry rule where a register ending at I is reusable at I.
        (push (cons (interval-start interval) 1) events)
        (push (cons (1+ (interval-end interval)) -1) events)))
    (let ((live 0)
          (peak 0))
      (dolist (event (sort events (lambda (a b)
                                   (if (= (car a) (car b))
                                       (< (cdr a) (cdr b))
                                       (< (car a) (car b))))))
        (incf live (cdr event))
        (setf peak (max peak live)))
      peak)))
