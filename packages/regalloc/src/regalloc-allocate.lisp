(in-package :cl-cc/regalloc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Regalloc — Linear Scan Allocation, Spill Code Insertion, Public API
;;;
;;; Contains: %interval-next-use-after, %preferred-register-for-interval,
;;; linear-scan-allocate, vm-spill-store/vm-spill-load instruction structs,
;;; %regalloc-map-tree, %regalloc-rewrite-inst, %regalloc-scratch-candidates,
;;; insert-spill-code, allocate-registers (public API), regalloc-lookup.
;;;
;;; Data structures (live-interval, regalloc-result), def/use analysis, and
;;; liveness computation are in regalloc.lisp (loads before this file).
;;;
;;; Load order: after regalloc.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; Linear Scan Allocation State

(defstruct (lsa-state (:conc-name lsa-))
  "Mutable state shared by the linear-scan allocation helpers."
  (assignment   (make-hash-table :test 'eq) :type hash-table)
  (spill-map    (make-hash-table :test 'eq) :type hash-table)
  (spill-count  0                           :type integer)
  (spill-offset 0                           :type integer)
  (free-regs    nil                         :type list)
  (free-fp-regs nil                         :type list)
  (active       nil                         :type list)
  (interval-map (make-hash-table :test 'eq) :type hash-table))

(defvar *current-allocation-policy* nil
  "Hint-derived allocation policy plist bound during allocation.
Recognized keys:
  :prefer-callee-saved-p
  :prefer-caller-saved-p")

(defparameter *regalloc-allocation-strategy* :linear-scan
  "Default register allocation strategy.

Valid values are :LINEAR-SCAN and :COLOR.  Linear scan remains the default;
graph coloring can be selected by binding this variable or by passing
:ALLOCATOR :COLOR in ALLOCATION-POLICY to ALLOCATE-REGISTERS.")

(defparameter *ml-regalloc-enabled* nil
  "When non-NIL, use FR-581 ML-style spill-cost prediction for spill choices.")

(defvar *current-regalloc-loop-depths* nil
  "Position -> loop-depth table bound while FR-581 spill decisions are made.")

(defun regalloc-loop-depths (instructions)
  "Return a position -> loop-depth table from backward branches in INSTRUCTIONS."
  (let ((label-pos (make-hash-table :test #'equal))
        (depths (make-hash-table :test #'eql)))
    (loop for inst in instructions
          for i from 0
          when (typep inst 'vm-label)
            do (setf (gethash (vm-name inst) label-pos) i))
    (loop for inst in instructions
          for i from 0
          when (typep inst '(or vm-jump vm-jump-zero))
            do (let ((target (gethash (vm-label-name inst) label-pos)))
                 (when (and target (< target i))
                   (loop for j from target to i do (incf (gethash j depths 0))))))
    depths))

(defun regalloc-ml-spill-cost (interval &optional (loop-depths *current-regalloc-loop-depths*))
  "Predict spill cost for INTERVAL using frequency × loop-depth weighted uses.
Higher scores mean the interval should be kept in a register when possible." 
  (let ((weighted-uses
          (loop for pos in (interval-use-positions interval)
                sum (+ 1 (* 8 (if loop-depths (gethash pos loop-depths 0) 0))))))
    (+ weighted-uses
       (if (interval-crosses-call-p interval) 2 0)
       (if (interval-return-value-p interval) 4 0)
       (if (interval-remat-const interval) -6 0)
       (if (interval-remat-inst interval) -3 0))))

(defun %lsa-interval-pool (state interval)
  "Return the free-register pool for INTERVAL's register class."
  (if (interval-fp-p interval) (lsa-free-fp-regs state) (lsa-free-regs state)))

(defun %lsa-set-interval-pool (state interval new-pool)
  "Replace the free-register pool for INTERVAL's register class."
  (if (interval-fp-p interval)
      (setf (lsa-free-fp-regs state) new-pool)
      (setf (lsa-free-regs state) new-pool)))

(defun %lsa-expire-old (state interval)
  "Return expired intervals to their register pools; remove from active list."
  (setf (lsa-active state)
        (remove-if (lambda (a)
                     (when (< (interval-end a) (interval-start interval))
                       (%lsa-set-interval-pool state a
                                               (cons (interval-phys-reg a)
                                                     (%lsa-interval-pool state a)))
                       t))
                   (lsa-active state))))

(defun %lsa-spill-current (state interval)
  "Assign a provisional spill slot to INTERVAL and record it in spill-map."
  (incf (lsa-spill-count state))
  (let ((slot (+ (lsa-spill-offset state) (lsa-spill-count state))))
    (setf (interval-spill-slot interval) slot)
    (setf (gethash (interval-vreg interval) (lsa-spill-map state)) slot)))

(defun %lsa-best-spill-candidate (state interval)
  "Return the active interval (or INTERVAL itself) with the farthest next use."
  (let ((same-class (remove-if-not (lambda (cand)
                                      (eq (interval-fp-p cand) (interval-fp-p interval)))
                                    (lsa-active state))))
    (if *ml-regalloc-enabled*
        (reduce (lambda (best candidate)
                  (let ((best-cost (regalloc-ml-spill-cost best))
                        (cand-cost (regalloc-ml-spill-cost candidate)))
                    (cond ((< cand-cost best-cost) candidate)
                          ((and (= cand-cost best-cost)
                                (< (interval-end candidate) (interval-end best)))
                           candidate)
                          (t best))))
                same-class
                :initial-value interval)
        (reduce (lambda (best candidate)
                  (let ((best-next (%interval-next-use-after best (interval-start interval)))
                        (cand-next (%interval-next-use-after candidate (interval-start interval))))
                    (cond ((null best) candidate)
                          ((null cand-next) candidate)
                          ((null best-next) best)
                          ((> cand-next best-next) candidate)
                          (t best))))
                same-class
                :initial-value interval))))

;;; Linear Scan Allocation — named helpers

(defun %lsa-assign (state interval phys)
  "Assign PHYS to INTERVAL and insert it into the active set (sorted by end)."
  (setf (interval-phys-reg interval) phys
        (gethash (interval-vreg interval) (lsa-assignment state)) phys
        (lsa-active state) (merge 'list (list interval) (lsa-active state) #'< :key #'interval-end)))

(defun %lsa-try-coalesce (state interval)
  "Attempt register coalescing for INTERVAL.  Returns T on success."
  (let* ((src-vreg (interval-coalesce-with interval))
         (src-int (and src-vreg (gethash src-vreg (lsa-interval-map state)))))
    (when (and src-int
               (eq (interval-fp-p src-int) (interval-fp-p interval))
               (interval-phys-reg src-int)
               (<= (interval-end src-int) (interval-start interval)))
      (let ((phys (interval-phys-reg src-int)))
        (setf (lsa-active state) (remove src-int (lsa-active state) :test #'eq))
        (%lsa-assign state interval phys)
        t))))

(defun %lsa-allocate-from-pool (state interval cc pool)
  "Pick a physical register from POOL (preferred first) and assign it."
  (let* ((preferred (%preferred-register-for-interval interval cc pool))
         (phys (or preferred (car pool))))
    (%lsa-set-interval-pool state interval (remove phys pool :count 1 :test #'eq))
    (%lsa-assign state interval phys)))

(defun %lsa-evict-and-assign (state interval)
  "Spill the worst active interval and reassign its register to INTERVAL."
  (let ((spill-candidate (%lsa-best-spill-candidate state interval)))
    (if (eq spill-candidate interval)
        (%lsa-spill-current state interval)
        (let ((freed-reg (interval-phys-reg spill-candidate)))
          (%lsa-spill-current state spill-candidate)
          (remhash (interval-vreg spill-candidate) (lsa-assignment state))
          (setf (interval-phys-reg spill-candidate) nil
                (lsa-active state) (remove spill-candidate (lsa-active state)))
          (%lsa-assign state interval freed-reg)))))

(defun %interval-next-use-after (interval position)
  "Return the next use position of INTERVAL after POSITION, or NIL."
  (find-if (lambda (use-pos) (> use-pos position))
           (interval-use-positions interval)))

(defun %return-value-preferred-reg (interval cc free-regs)
  "Strategy: prefer the ABI return register for return-value intervals."
  (let ((preferred (and (interval-return-value-p interval)
                        (if (interval-fp-p interval)
                            (target-fp-ret-reg cc)
                            (target-ret-reg cc)))))
    (and preferred (member preferred free-regs) preferred)))

(defun %call-crossing-preferred-reg (interval cc free-regs)
  "Strategy: prefer a callee-saved register for intervals crossing a call."
  (when (and (interval-crosses-call-p interval) (not (interval-fp-p interval)))
    (find-if (lambda (reg) (member reg free-regs :test #'eq))
             (target-callee-saved cc))))

(defun %param-preferred-reg (interval cc free-regs)
  "Strategy: prefer the ABI argument register matching the parameter position."
  (let* ((param-index (interval-parameter-index interval))
         (arg-regs (if (interval-fp-p interval) (target-fp-arg-regs cc) (target-arg-regs cc)))
         (preferred (and (integerp param-index)
                          (< param-index (length arg-regs))
                          (nth param-index arg-regs))))
    (and preferred (member preferred free-regs) preferred)))

(defun %hint-policy-preferred-reg (interval cc free-regs)
  "Strategy: when policy prefers caller-saved regs, bias allocation accordingly.
Safety guard: do not force caller-saved for call-crossing intervals."
  (when (and (getf *current-allocation-policy* :prefer-caller-saved-p)
             (not (interval-crosses-call-p interval))
             (not (interval-fp-p interval)))
    (find-if (lambda (reg) (member reg free-regs :test #'eq))
             (target-caller-saved cc))))

(defparameter *preferred-register-strategies*
  (list #'%hint-policy-preferred-reg
        #'%return-value-preferred-reg
        #'%call-crossing-preferred-reg
        #'%param-preferred-reg)
  "Ordered list of preferred-register strategies tried left-to-right; first truthy result wins.")

(defun regalloc-target-fp-registers (cc)
  "Return the allocatable floating-point register pool for CC."
  (case (target-name cc)
    (:x86-64 '(:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7
               :xmm8 :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))
    (:aarch64 '(:v0 :v1 :v2 :v3 :v4 :v5 :v6 :v7
                :v8 :v9 :v10 :v11 :v12 :v13 :v14 :v15
                :v16 :v17 :v18 :v19 :v20 :v21 :v22 :v23
                :v24 :v25 :v26 :v27 :v28 :v29 :v30 :v31))
    (otherwise
     (remove-duplicates
      (append (copy-list (target-fp-arg-regs cc))
              (list (target-fp-ret-reg cc)))
      :test #'eq))))

(defun %derive-single-function-policy (instructions)
  "Best-effort default policy derivation for single-function instruction streams."
  (let* ((bodies (regalloc-collect-linear-functions instructions))
         (labels nil))
    (maphash (lambda (label body)
               (declare (ignore body))
               (push label labels))
             bodies)
    (when (= (length labels) 1)
      (let* ((label (first labels))
             (hints (regalloc-compute-interprocedural-hints instructions)))
         (regalloc-build-allocation-policy-from-hints hints label)))))

(defun %allocation-strategy (allocation-policy)
  "Return the requested allocation strategy, defaulting to linear scan."
  (or (getf allocation-policy :allocator)
      (getf allocation-policy :register-allocator)
      *regalloc-allocation-strategy*))

(defun %preferred-register-for-interval (interval cc free-regs)
  "Return a preferred free physical register for INTERVAL, or NIL."
  (some (lambda (strategy) (funcall strategy interval cc free-regs))
        *preferred-register-strategies*))

(defun linear-scan-allocate (intervals cc &optional (spill-slot-offset 0))
  "Perform linear scan register allocation.
   INTERVALS: sorted list of live-interval objects.
   CC: target-desc object.
   Returns (values assignment-ht spill-ht spill-count)."
  (let ((state (make-lsa-state
                :spill-offset spill-slot-offset
                :free-regs (remove (first (target-scratch-regs cc)) (copy-list (target-allocatable-regs cc)))
                :free-fp-regs (regalloc-target-fp-registers cc))))
    (dolist (int intervals)
      (setf (gethash (interval-vreg int) (lsa-interval-map state)) int))
    (dolist (interval intervals)
      (%lsa-expire-old state interval)
      (unless (%lsa-try-coalesce state interval)
        (let ((pool (%lsa-interval-pool state interval)))
          (if pool
              (%lsa-allocate-from-pool state interval cc pool)
              (%lsa-evict-and-assign state interval)))))
    ;; FR-199: Apply spill slot sharing / stack coloring.
    ;; Compute the actual spill count from the colored map's max slot index.
    (let ((colored-map (%maybe-color-spill-slots intervals (lsa-spill-map state) spill-slot-offset))
          (max-slot spill-slot-offset))
      (maphash (lambda (vreg slot)
                  (declare (ignore vreg))
                  (setf max-slot (max max-slot slot)))
                colored-map)
       ;; Slots are 1-indexed, so the maximum assigned slot is the frame slot count.
       (values (lsa-assignment state) colored-map max-slot))))

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

(defun %color-build-interference-graph (intervals)
  "Build a vreg -> neighbor-vregs interference graph for INTERVALS."
  (let ((graph (make-hash-table :test #'eq)))
    (dolist (interval intervals)
      (when (interval-vreg interval)
        (setf (gethash (interval-vreg interval) graph) nil)))
    (loop for rest on intervals
          for a = (car rest)
          do (dolist (b (cdr rest))
               (when (and (interval-vreg a)
                          (interval-vreg b)
                          (%intervals-overlap-p a b))
                 (pushnew (interval-vreg b) (gethash (interval-vreg a) graph) :test #'eq)
                 (pushnew (interval-vreg a) (gethash (interval-vreg b) graph) :test #'eq))))
    graph))

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

(defun %spill-intervals-overlap-p (a b)
  "Return T when live intervals A and B interfere for stack-slot sharing."
  (and (<= (interval-start a) (interval-end b))
       (<= (interval-start b) (interval-end a))))

(defun %spill-weight (interval)
  "Return a deterministic greedy-coloring priority for spilled INTERVAL."
  (+ (length (interval-use-positions interval))
     (if (interval-crosses-call-p interval) 1 0)
     (if (interval-return-value-p interval) 1 0)))

(defun %build-spill-interference-matrix (spilled)
  "Build vreg → neighbor-vregs matrix for spilled live intervals."
  (let ((matrix (make-hash-table :test #'eq)))
    (dolist (interval spilled)
      (setf (gethash (interval-vreg interval) matrix) nil))
    (loop for rest on spilled
          for a = (car rest)
          do (dolist (b (cdr rest))
               (when (%spill-intervals-overlap-p a b)
                 (pushnew (interval-vreg b) (gethash (interval-vreg a) matrix) :test #'eq)
                 (pushnew (interval-vreg a) (gethash (interval-vreg b) matrix) :test #'eq))))
    matrix))

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
  "Wrapper that optionally applies spill slot coloring.  Controlled by a special
   variable for safety during testing."
  (if (and spill-map intervals)
      (regalloc-color-spill-slots intervals spill-map spill-slot-offset)
      spill-map))

;;; Spill Code Insertion

(define-vm-instruction vm-spill-store (vm-instruction)
  "Store register to spill slot [RBP - slot*8]."
  (src-reg nil :reader vm-spill-src)
  (slot nil :reader vm-spill-slot))

(define-vm-instruction vm-spill-load (vm-instruction)
  "Load spill slot [RBP - slot*8] into register."
  (dst-reg nil :reader vm-spill-dst)
  (slot nil :reader vm-spill-slot))

(defparameter *live-range-split-minimum-hole-size* 8
  "Minimum instruction-position gap that triggers live range splitting.")

(defmethod instruction-uses ((inst vm-spill-store))
  (list (vm-spill-src inst)))

(defmethod instruction-defs ((inst vm-spill-load))
  (list (vm-spill-dst inst)))

(defun %split-vreg-at-position (children position)
  "Return the child vreg live at POSITION, or NIL."
  (let ((match (find-if (lambda (child)
                          (and (<= (interval-start child) position)
                               (<= position (interval-end child))))
                        children)))
    (and match (interval-vreg match))))

(defun %copy-float-vreg-map-with-splits (float-vregs child-groups)
  "Copy FLOAT-VREGS and mark split child virtual registers."
  (let ((result (make-hash-table :test #'eq)))
    (when float-vregs
      (maphash (lambda (vreg value)
                 (setf (gethash vreg result) value))
               float-vregs))
    (dolist (group child-groups result)
      (dolist (child (cdr group))
        (when (interval-fp-p child)
          (setf (gethash (interval-vreg child) result) t))))))

(defun %split-live-range-rewrite-map (child-groups position)
  "Build an original-vreg -> child-vreg map for POSITION."
  (let ((map (make-hash-table :test #'eq)))
    (dolist (group child-groups map)
      (destructuring-bind (original-vreg . children) group
        (let ((child-vreg (%split-vreg-at-position children position)))
          (when (and child-vreg (not (eq child-vreg original-vreg)))
            (setf (gethash original-vreg map) child-vreg)))))))

(defun %rewrite-inst-for-split-position (inst child-groups position)
  "Rewrite INST virtual registers to the split children live at POSITION."
  (let ((reg-map (%split-live-range-rewrite-map child-groups position)))
    (if (zerop (hash-table-count reg-map))
        inst
        (%regalloc-rewrite-inst inst reg-map))))

(defun %collect-live-range-splits (intervals minimum-hole-size)
  "Return child interval groups and split boundaries for INTERVALS."
  (let ((child-groups nil)
        (boundaries nil))
    (dolist (interval intervals)
      (multiple-value-bind (children interval-boundaries)
          (split-live-interval interval minimum-hole-size)
        (push (cons (interval-vreg interval) children) child-groups)
        (setf boundaries (append boundaries interval-boundaries))))
    (values (nreverse child-groups)
            (sort boundaries #'< :key #'split-boundary-before-position))))

(defun %assign-live-range-split-slots (boundaries)
  "Assign fixed stack slots to split BOUNDARIES."
  (loop for boundary in boundaries
        for slot from 1
        do (setf (split-boundary-slot boundary) slot)
        finally (return (1- slot))))

(defun %boundaries-by-position (boundaries keyfn)
  "Index BOUNDARIES by position returned by KEYFN."
  (let ((table (make-hash-table :test #'eql)))
    (dolist (boundary boundaries table)
      (push boundary (gethash (funcall keyfn boundary) table)))))

(defun split-live-ranges (instructions intervals float-vregs &optional
                          (minimum-hole-size *live-range-split-minimum-hole-size*))
  "Rewrite INSTRUCTIONS and intervals by splitting long live-range holes.

Returns four values: rewritten instructions, recomputed child intervals, the
number of fixed split spill slots reserved at the bottom of the frame, and the
float-vreg map updated with split children."
  (multiple-value-bind (child-groups boundaries)
      (%collect-live-range-splits intervals minimum-hole-size)
    (let ((split-slot-count (%assign-live-range-split-slots boundaries)))
      (if (zerop split-slot-count)
          (values instructions intervals 0 float-vregs)
          (let ((loads-by-position (%boundaries-by-position boundaries #'split-boundary-before-position))
                (stores-by-position (%boundaries-by-position boundaries #'split-boundary-after-position))
                (rewritten nil))
            (loop for inst in instructions
                  for position from 0
                  do (dolist (boundary (reverse (gethash position loads-by-position)))
                       (push (make-vm-spill-load :dst-reg (split-boundary-to-vreg boundary)
                                                 :slot (split-boundary-slot boundary))
                             rewritten))
                     (push (%rewrite-inst-for-split-position inst child-groups position) rewritten)
                     (dolist (boundary (reverse (gethash position stores-by-position)))
                       (push (make-vm-spill-store :src-reg (split-boundary-from-vreg boundary)
                                                  :slot (split-boundary-slot boundary))
                             rewritten)))
            (let* ((split-instructions (nreverse rewritten))
                   (split-float-vregs (%copy-float-vreg-map-with-splits float-vregs child-groups)))
              (values split-instructions
                      (%compute-live-intervals-raw split-instructions split-float-vregs)
                      split-slot-count
                      split-float-vregs)))))))

(defun %regalloc-map-tree (fn tree)
  (if (consp tree)
      (cons (%regalloc-map-tree fn (car tree))
            (%regalloc-map-tree fn (cdr tree)))
      (funcall fn tree)))

(defun %regalloc-rewrite-inst (inst reg-map)
  "Return INST with register keywords substituted per REG-MAP."
  (flet ((rewrite-reg (reg)
           (if (and (keywordp reg) (gethash reg reg-map))
               (gethash reg reg-map)
               reg)))
    (cond
      ((typep inst 'vm-spill-store)
       (make-vm-spill-store :src-reg (rewrite-reg (vm-spill-src inst))
                            :slot (vm-spill-slot inst)))
      ((typep inst 'vm-spill-load)
       (make-vm-spill-load :dst-reg (rewrite-reg (vm-spill-dst inst))
                           :slot (vm-spill-slot inst)))
      (t
       (handler-case
           (sexp->instruction
            (%regalloc-map-tree #'rewrite-reg (instruction->sexp inst)))
         (error () inst))))))

(defun %regalloc-rewrite-inst-dst (inst dst)
  "Return INST cloned with its single destination rewritten to DST."
  (let ((old-dst (opt-inst-dst inst))
        (reg-map (make-hash-table :test #'eq)))
    (when old-dst
      (setf (gethash old-dst reg-map) dst))
    (%regalloc-rewrite-inst inst reg-map)))

(defun %regalloc-rematerialize-inst (remat scratch)
  "Return instruction(s) that rematerialize REMAT into SCRATCH."
  (cond
    ((and (consp remat) (eq (car remat) :const))
     (list (make-vm-const :dst scratch :value (cadr remat))))
    ((and (consp remat) (eq (car remat) :inst))
     (list (%regalloc-rewrite-inst-dst (cadr remat) scratch)))
    ((or (integerp remat) (symbolp remat) (characterp remat) (stringp remat))
     (list (make-vm-const :dst scratch :value remat)))
    (t nil)))

(defun %regalloc-reserved-scratch-regs (inst cc)
  "Return scratch registers reserved internally by INST on CC, if any."
  (when (and (eq (target-name cc) :x86-64)
             (or (typep inst 'vm-integer-mul-high-u)
                 (typep inst 'vm-integer-mul-high-s)
                 (typep inst 'vm-truncate)
                 (typep inst 'vm-rem)
                 (typep inst 'vm-div)
                 (typep inst 'vm-mod)))
    (list (first (target-scratch-regs cc)))))

(defun %regalloc-scratch-candidates (cc used-phys &optional inst fp-p)
  (let ((reserved (remove nil (%regalloc-reserved-scratch-regs inst cc))))
    (remove-if
     (lambda (reg)
       (or (null reg)
           (member reg used-phys :test #'eq)
           (member reg reserved :test #'eq)))
     (if fp-p
         (regalloc-target-fp-registers cc)
         (remove-duplicates
          (append (list (first (target-scratch-regs cc))
                        (target-ret-reg cc))
                  (target-caller-saved cc)
                  (target-allocatable-regs cc))
          :test #'eq)))))

(defun insert-spill-code (instructions assignment spill-map cc &optional remat-map float-vregs)
  "Insert spill loads/stores around instructions that use spilled registers.
   Returns a new instruction list with spill code inserted."
  (let ((result nil))
    (dolist (inst instructions)
      (let* ((used-phys (remove nil
                                (mapcar (lambda (vreg)
                                          (and vreg
                                               (not (gethash vreg spill-map))
                                               (gethash vreg assignment)))
                                         (append (instruction-uses inst)
                                                 (instruction-defs inst)))))
             (available-gpr (%regalloc-scratch-candidates cc used-phys inst nil))
             (available-fp (%regalloc-scratch-candidates cc used-phys inst t))
             (reg-map (make-hash-table :test #'eq)))
        (flet ((float-vreg-p (vreg)
                 (and float-vregs (gethash vreg float-vregs)))
               (alloc-scratch (fp-p)
                 (or (if fp-p (pop available-fp) (pop available-gpr))
                     (error "insert-spill-code: no scratch register available for ~S" inst))))
          (flet ((ensure-scratch (vreg)
                   (or (gethash vreg reg-map)
                       (setf (gethash vreg reg-map)
                             (alloc-scratch (float-vreg-p vreg))))))
          ;; Load spilled uses before the instruction.
          (dolist (vreg (instruction-uses inst))
            (when (and vreg (gethash vreg spill-map))
              (let ((scratch (ensure-scratch vreg)))
                (let ((remat (and remat-map (gethash vreg remat-map))))
                  (if remat
                      (dolist (remat-inst (reverse (%regalloc-rematerialize-inst remat scratch)))
                        (push remat-inst result))
                      (push (make-vm-spill-load :dst-reg scratch
                                                :slot (gethash vreg spill-map))
                            result))))))
          ;; Ensure spilled defs have scratch registers unless the definition is
          ;; rematerializable and can be dropped instead of stored.
          (dolist (vreg (instruction-defs inst))
            (when (and vreg (gethash vreg spill-map)
                       (not (and remat-map (gethash vreg remat-map))))
              (ensure-scratch vreg)))
          ;; Rewrite instruction once with complete reg-map, then push.
          (unless (and (instruction-defs inst)
                       (every (lambda (vreg)
                                (and vreg
                                     (gethash vreg spill-map)
                                     remat-map
                                     (gethash vreg remat-map)))
                              (instruction-defs inst)))
            (push (if (zerop (hash-table-count reg-map))
                      inst
                      (%regalloc-rewrite-inst inst reg-map))
                  result))
          ;; Store spilled defs after the instruction.
          (dolist (vreg (instruction-defs inst))
            (when (and vreg (gethash vreg spill-map)
                       (not (and remat-map (gethash vreg remat-map))))
              (push (make-vm-spill-store :src-reg (gethash vreg reg-map)
                                          :slot (gethash vreg spill-map))
                    result)))))))
    (nreverse result)))

(defun %finalize-split-spill-registers (instructions assignment)
  "Rewrite split spill helper instructions from allocated vregs to physical regs."
  (mapcar (lambda (inst)
            (cond
              ((typep inst 'vm-spill-store)
               (let ((phys (gethash (vm-spill-src inst) assignment)))
                 (if phys
                     (make-vm-spill-store :src-reg phys :slot (vm-spill-slot inst))
                     inst)))
              ((typep inst 'vm-spill-load)
               (let ((phys (gethash (vm-spill-dst inst) assignment)))
                 (if phys
                     (make-vm-spill-load :dst-reg phys :slot (vm-spill-slot inst))
                     inst)))
              (t inst)))
          instructions))

;;; Public API

(defun allocate-registers (instructions cc &optional float-vregs allocation-policy)
  "Run register allocation on VM instruction list.
   CC is a target-desc object.
   ALLOCATION-POLICY is an optional plist (e.g. from
   REGALLOC-BUILD-ALLOCATION-POLICY-FROM-HINTS) used to bias preferred registers.
   Returns a regalloc-result."
  (let* ((raw-intervals (compute-live-intervals instructions float-vregs))
         (split-instructions instructions)
         (split-slot-count 0)
         (intervals raw-intervals)
           (effective-policy (or allocation-policy
                                 (%derive-single-function-policy instructions)))
           (*current-allocation-policy* effective-policy)
           (*current-regalloc-loop-depths* (and *ml-regalloc-enabled*
                                                (regalloc-loop-depths instructions))))
    (multiple-value-setq (split-instructions intervals split-slot-count float-vregs)
      (split-live-ranges instructions raw-intervals float-vregs))
    (multiple-value-bind (assignment spill-map spill-count)
        (case (%allocation-strategy effective-policy)
          (:color (color-allocate-for-target intervals cc split-slot-count))
          (:linear-scan (linear-scan-allocate intervals cc split-slot-count))
          (otherwise (linear-scan-allocate intervals cc split-slot-count)))
      (let* ((remat-map (let ((ht (make-hash-table :test #'eq)))
                           (dolist (interval intervals ht)
                             (cond
                               ((interval-remat-const interval)
                                (setf (gethash (interval-vreg interval) ht)
                                      (list :const (interval-remat-const interval))))
                               ((interval-remat-inst interval)
                                (setf (gethash (interval-vreg interval) ht)
                                      (list :inst (interval-remat-inst interval))))))))
              (final-instructions
               (%finalize-split-spill-registers
                (if (plusp (hash-table-count spill-map))
                    (insert-spill-code split-instructions assignment spill-map cc remat-map float-vregs)
                    split-instructions)
                assignment)))
        (make-regalloc-result
         :assignment assignment
         :spill-map spill-map
         :spill-count spill-count
         :gpr-pressure (regalloc-register-pressure intervals :fp-p nil)
         :fp-pressure (regalloc-register-pressure intervals :fp-p t)
         :instructions final-instructions)))))

(defun regalloc-lookup (result vreg)
  "Look up physical register for VREG in allocation result.
   Returns the physical register keyword or NIL if spilled."
  (gethash vreg (regalloc-assignment result)))
