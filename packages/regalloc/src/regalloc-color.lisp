(in-package :cl-cc/regalloc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Regalloc — Graph Coloring Allocation (FR-061) + Spill Slot Sharing (FR-199)
;;;
;;; Contains: %intervals-overlap-p, %vreg<, %build-interference-graph-body,
;;; %color-build-interference-graph,
;;; %copy-interference-graph, %graph-nodes, %graph-degree, %graph-remove-node,
;;; %color-spill-priority, %color-spill-candidate, %color-simplify,
;;; %color-ordered-registers-for-interval, %color-select-register,
;;; %color-assign-spill-slots, %interval-map, color-allocate,
;;; %copy-hash-into, color-allocate-for-target,
;;; %spill-weight,
;;; %build-spill-interference-matrix, color-spill-slots,
;;; regalloc-color-spill-slots, %maybe-color-spill-slots.
;;;
;;; Depends on: regalloc-allocate.lisp (linear-scan-allocate,
;;;   %preferred-register-for-interval, regalloc-target-fp-registers).
;;;
;;; Load order: after regalloc-allocate.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; FR-061: Graph Coloring Allocation (Chaitin-Briggs)
;;;
;;; This allocator builds an interval interference graph, runs Briggs-style
;;; optimistic simplification with K physical colors, then selects colors in
;;; reverse removal order.  Nodes that cannot be colored after optimistic
;;; simplification are real spills and receive stack slots through the existing
;;; spill-slot coloring path.

(defun %intervals-overlap-p (a b)
  "Return T when live intervals A and B interfere."
  (and (<= (interval-start a) (interval-end b))
       (<= (interval-start b) (interval-end a))))

(defun %vreg< (a b)
  "Deterministic ordering predicate for virtual-register symbols."
  (string< (symbol-name a) (symbol-name b)))

(defun %build-interference-graph-body (intervals overlap-predicate)
  "Build a vreg -> neighbor-vregs hash-table for INTERVALS.
Two intervals become neighbors when OVERLAP-PREDICATE returns true for them.
Intervals without a vreg are silently skipped."
  (let ((graph (make-hash-table :test #'eq)))
    (dolist (interval intervals)
      (when (interval-vreg interval)
        (setf (gethash (interval-vreg interval) graph) nil)))
    (loop for rest on intervals
          for a = (car rest)
          do (dolist (b (cdr rest))
               (when (and (interval-vreg a)
                          (interval-vreg b)
                          (funcall overlap-predicate a b))
                 (pushnew (interval-vreg b) (gethash (interval-vreg a) graph) :test #'eq)
                 (pushnew (interval-vreg a) (gethash (interval-vreg b) graph) :test #'eq))))
    graph))

(defun %color-build-interference-graph (intervals)
  "Build a vreg -> neighbor-vregs interference graph for INTERVALS."
  (%build-interference-graph-body intervals #'%intervals-overlap-p))

(defun %copy-interference-graph (graph)
  "Return a shallow mutable copy of GRAPH with copied neighbor lists."
  (let ((copy (make-hash-table :test #'eq)))
    (maphash (lambda (vreg neighbors)
               (setf (gethash vreg copy) (copy-list neighbors)))
             graph)
    copy))

(defun %graph-nodes (graph)
  "Return GRAPH nodes in deterministic order."
  (let ((nodes nil))
    (maphash (lambda (vreg neighbors)
               (declare (ignore neighbors))
               (push vreg nodes))
             graph)
    (sort nodes #'%vreg<)))

(defun %graph-degree (graph vreg)
  "Return VREG degree within mutable GRAPH."
  (length (gethash vreg graph)))

(defun %graph-remove-node (graph vreg)
  "Remove VREG and incident edges from mutable GRAPH."
  (dolist (neighbor (copy-list (gethash vreg graph)))
    (setf (gethash neighbor graph)
          (remove vreg (gethash neighbor graph) :test #'eq)))
  (remhash vreg graph))

(defun %color-spill-priority (interval)
  "Return lower-is-better optimistic spill priority for INTERVAL."
  (if *ml-regalloc-enabled*
      (regalloc-ml-spill-cost interval)
      (let ((length (max 1 (- (interval-end interval) (interval-start interval))))
            (uses (length (interval-use-positions interval))))
        (+ (/ uses length)
           (if (interval-crosses-call-p interval) 1 0)
           (if (interval-return-value-p interval) 1 0)))))

(defun %color-spill-candidate (graph interval-map)
  "Pick a deterministic optimistic spill candidate from GRAPH."
  (let ((best nil)
        (best-score nil))
    (dolist (vreg (%graph-nodes graph) best)
      (let* ((interval (gethash vreg interval-map))
             (score (if interval (%color-spill-priority interval) 0)))
        (when (or (null best)
                  (< score best-score)
                  (and (= score best-score) (%vreg< vreg best)))
          (setf best vreg
                best-score score))))))

(defun %color-simplify (graph interval-map color-count)
  "Return Chaitin-Briggs simplification stack for GRAPH."
  (let ((work (%copy-interference-graph graph))
        (stack nil))
    (loop while (plusp (hash-table-count work))
          do (let ((node (find-if (lambda (vreg)
                                    (< (%graph-degree work vreg) color-count))
                                  (%graph-nodes work))))
               (unless node
                 (setf node (%color-spill-candidate work interval-map)))
               (push (cons node (>= (%graph-degree work node) color-count)) stack)
               (%graph-remove-node work node)))
    stack))

(defun %color-ordered-registers-for-interval (interval cc available-regs)
  "Return AVAILABLE-REGS with INTERVAL's preferred register first when possible."
  (let ((preferred (and cc interval
                        (%preferred-register-for-interval interval cc available-regs))))
    (if preferred
        (cons preferred (remove preferred available-regs :test #'eq :count 1))
        available-regs)))

(defun %color-select-register (vreg graph assignment available-regs)
  "Return the first register available for VREG, excluding colored neighbors."
  (let ((used nil))
    (dolist (neighbor (gethash vreg graph))
      (let ((phys (gethash neighbor assignment)))
        (when phys
          (pushnew phys used :test #'eq))))
    (find-if (lambda (reg) (not (member reg used :test #'eq)))
             available-regs)))

(defun %color-assign-spill-slots (spilled spill-slot-offset)
  "Assign stack slots to SPILLED intervals and return spill-map and max slot."
  (let ((raw-map (make-hash-table :test #'eq)))
    (loop for interval in spilled
          for slot from (1+ spill-slot-offset)
          do (setf (gethash (interval-vreg interval) raw-map) slot
                   (interval-spill-slot interval) slot))
    (let ((colored-map (%maybe-color-spill-slots spilled raw-map spill-slot-offset))
          (max-slot spill-slot-offset))
      (maphash (lambda (vreg slot)
                 (declare (ignore vreg))
                 (setf max-slot (max max-slot slot)))
               colored-map)
      (values colored-map max-slot))))

(defun %interval-map (intervals)
  "Return vreg -> interval map for INTERVALS."
  (let ((map (make-hash-table :test #'eq)))
    (dolist (interval intervals map)
      (when (interval-vreg interval)
        (setf (gethash (interval-vreg interval) map) interval)))))

(defun color-allocate (intervals available-regs &optional (spill-slot-offset 0) cc)
  "Perform Chaitin-Briggs graph-coloring allocation.

INTERVALS is a list of live-interval objects from one register class.
AVAILABLE-REGS is the physical color set.  Returns three values:
assignment hash-table, spill-map hash-table, and the maximum spill slot."
  (let* ((assignment (make-hash-table :test #'eq))
         (interval-map (%interval-map intervals))
         (graph (%color-build-interference-graph intervals))
         (stack (%color-simplify graph interval-map (length available-regs)))
         (spilled nil))
    (dolist (entry stack)
      (let* ((vreg (car entry))
             (interval (gethash vreg interval-map))
             (ordered-regs (%color-ordered-registers-for-interval interval cc available-regs))
             (phys (%color-select-register vreg graph assignment ordered-regs)))
        (if phys
            (progn
              (setf (gethash vreg assignment) phys)
              (when interval
                (setf (interval-phys-reg interval) phys)))
            (when interval
              (push interval spilled)))))
    (multiple-value-bind (spill-map spill-count)
        (%color-assign-spill-slots (nreverse spilled) spill-slot-offset)
      (values assignment spill-map spill-count))))

(defun %copy-hash-into (from to)
  "Copy all entries from hash-table FROM into TO."
  (maphash (lambda (key value)
             (setf (gethash key to) value))
           from)
  to)

(defun color-allocate-for-target (intervals cc &optional (spill-slot-offset 0))
  "Perform graph-coloring allocation for CC, preserving FP/GPR register classes."
  (let ((gpr-intervals (remove-if #'interval-fp-p intervals))
        (fp-intervals (remove-if-not #'interval-fp-p intervals))
        (assignment (make-hash-table :test #'eq))
        (spill-map (make-hash-table :test #'eq))
        (spill-count spill-slot-offset))
    (multiple-value-bind (gpr-assignment gpr-spills gpr-count)
        (color-allocate gpr-intervals
                        (remove (first (target-scratch-regs cc))
                                 (copy-list (target-allocatable-regs cc)))
                        spill-slot-offset
                        cc)
      (%copy-hash-into gpr-assignment assignment)
      (%copy-hash-into gpr-spills spill-map)
      (setf spill-count gpr-count))
    (multiple-value-bind (fp-assignment fp-spills fp-count)
        (color-allocate fp-intervals (regalloc-target-fp-registers cc) spill-count cc)
      (%copy-hash-into fp-assignment assignment)
      (%copy-hash-into fp-spills spill-map)
      (setf spill-count fp-count))
    (values assignment spill-map spill-count)))

;;; FR-199: Spill Slot Sharing / Stack Coloring
;;;
;;; After linear scan, spilled vregs whose live ranges do not overlap are assigned
;;; the same stack slot via greedy interval-graph coloring.  This reduces O(N)
;;; stack slots to O(χ(G)), where χ(G) is the chromatic number of the interference
;;; graph of spilled intervals.
;;;
;;; Note: %intervals-overlap-p (defined above) is reused here; the former
;;; %spill-intervals-overlap-p duplicate has been removed.

(defun %spill-weight (interval)
  "Return a deterministic greedy-coloring priority for spilled INTERVAL."
  (+ (length (interval-use-positions interval))
     (if (interval-crosses-call-p interval) 1 0)
     (if (interval-return-value-p interval) 1 0)))

(defun %build-spill-interference-matrix (spilled)
  "Build vreg → neighbor-vregs matrix for spilled live intervals."
  (%build-interference-graph-body spilled #'%intervals-overlap-p))

(defun color-spill-slots (spilled &optional (spill-slot-offset 0))
  "Color SPILLED live intervals and return a vreg → shared slot hash-table.

Two spilled virtual registers interfere when their live ranges overlap.  The
greedy coloring order prioritizes higher spill weight, then longer intervals,
then earlier starts for deterministic output.  Slot numbers are 1-origin to keep
the existing [RBP - slot*8] spill/restore instruction format unchanged."
  (let* ((ordered (sort (copy-list spilled)
                        (lambda (a b)
                          (let ((aw (%spill-weight a))
                                (bw (%spill-weight b)))
                            (cond ((/= aw bw) (> aw bw))
                                  ((/= (- (interval-end a) (interval-start a))
                                       (- (interval-end b) (interval-start b)))
                                   (> (- (interval-end a) (interval-start a))
                                      (- (interval-end b) (interval-start b))))
                                  (t (< (interval-start a) (interval-start b))))))))
         (matrix (%build-spill-interference-matrix ordered))
         (color-map (make-hash-table :test #'eq)))
    (dolist (interval ordered color-map)
      (let ((used-slots nil))
        (dolist (neighbor (gethash (interval-vreg interval) matrix))
          (let ((slot (gethash neighbor color-map)))
            (when slot
              (pushnew slot used-slots))))
        (loop for color from 1
              for slot = (+ spill-slot-offset color)
              unless (member slot used-slots)
                do (setf (gethash (interval-vreg interval) color-map) slot
                         (interval-spill-slot interval) slot)
                   (return))))))

(defun regalloc-color-spill-slots (intervals spill-map &optional (spill-slot-offset 0))
  "Return a spill-map with shared slots for non-overlapping spilled vregs."
  (let ((spilled (remove-if-not (lambda (interval)
                                  (gethash (interval-vreg interval) spill-map))
                                intervals)))
    (if spilled
        (color-spill-slots spilled spill-slot-offset)
        spill-map)))

(defun %maybe-color-spill-slots (intervals spill-map &optional (spill-slot-offset 0))
  "Optionally apply spill slot coloring when both INTERVALS and SPILL-MAP are non-nil."
  (if (and spill-map intervals)
      (regalloc-color-spill-slots intervals spill-map spill-slot-offset)
      spill-map))


