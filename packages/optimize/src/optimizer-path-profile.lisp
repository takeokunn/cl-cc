;;;; packages/optimize/src/optimizer-path-profile.lisp — FR-662 Ball-Larus path profiling

(in-package :cl-cc/optimize)

;;; Ball and Larus path profiling assigns each edge in an acyclic CFG a small
;;; integer such that the sum of the edge integers along any entry→exit path is
;;; unique.  At run time the instrumented program keeps one accumulator register:
;;; edge instrumentation adds the edge value, and path-exit instrumentation records
;;; the final sum in a path counter table.  Backedges terminate the current
;;; acyclic path and reset the accumulator at the loop header, matching the
;;; original Ball-Larus treatment of cyclic CFGs as a stream of acyclic paths.

(defparameter +opt-bl-path-acc-reg+ :%bl-path-sum
  "Reserved VM register used by Ball-Larus path instrumentation.")

(defparameter +opt-bl-edge-reg+ :%bl-edge-value
  "Reserved VM register used to materialize Ball-Larus edge weights.")

(defparameter +opt-bl-one-reg+ :%bl-counter-one
  "Reserved VM register used to materialize counter increments.")

(defparameter +opt-bl-default-counter-table+ :*cl-cc-ball-larus-path-counters*
  "VM global variable name containing path-sum → execution-count tables.")

(define-vm-instruction opt-vm-path-profile-record (vm-instruction)
  "Record the current Ball-Larus path sum in a VM-global counter table."
  (path-reg +opt-bl-path-acc-reg+ :reader opt-vm-path-profile-record-path-reg)
  (table-name +opt-bl-default-counter-table+ :reader opt-vm-path-profile-record-table-name)
  (function-id nil :reader opt-vm-path-profile-record-function-id)
  (delta-reg +opt-bl-one-reg+ :reader opt-vm-path-profile-record-delta-reg)
  (:sexp-tag :path-profile-record)
  (:sexp-slots path-reg table-name function-id delta-reg))

(defmethod execute-instruction ((inst opt-vm-path-profile-record) state pc labels)
  "Increment (FUNCTION-ID PATH-SUM)'s counter in STATE and continue execution."
  (declare (ignore labels))
  (let* ((table-name (opt-vm-path-profile-record-table-name inst))
         (table (or (gethash table-name (vm-global-vars state))
                    (setf (gethash table-name (vm-global-vars state))
                          (make-hash-table :test #'equal))))
         (path-sum (vm-reg-get state (opt-vm-path-profile-record-path-reg inst)))
         (delta (or (vm-reg-get state (opt-vm-path-profile-record-delta-reg inst)) 1))
         (key (list (opt-vm-path-profile-record-function-id inst) path-sum)))
    (incf (gethash key table 0) delta)
    (values (1+ pc) nil nil)))

(defstruct opt-path-profile-block
  "Ball-Larus metadata for one basic block."
  (block-id 0 :type integer)
  (label nil)
  (path-id 0 :type integer)
  (successor-count 0 :type integer)
  (execution-count 0 :type integer)
  (path-count 1 :type integer))

(defstruct opt-ball-larus-edge
  "Instrumentable CFG edge with assigned Ball-Larus VALUE."
  from
  to
  (value 0 :type integer)
  (backedge-p nil :type boolean)
  (exit-p nil :type boolean))

(defstruct opt-ball-larus-profile
  "Complete Ball-Larus profile plan for one instruction stream."
  cfg
  (blocks nil :type list)
  (edges nil :type list)
  (paths nil :type list)
  (instrumented-instructions nil :type list)
  (function-id :anonymous))

(defstruct opt-block-version-plan
  "Basic block versioning plan derived from Ball-Larus path counters."
  (hot-threshold 1 :type integer)
  (versions nil :type list))

(defun %opt-bl-block-label-name (block)
  (and (bb-label block) (vm-name (bb-label block))))

(defun %opt-bl-terminator-p (inst)
  (typep inst '(or vm-jump vm-jump-zero vm-ret vm-halt)))

(defun %opt-bl-block-terminator (block)
  (find-if #'%opt-bl-terminator-p (reverse (bb-instructions block))))

(defun %opt-bl-successors (block)
  "Return successors in deterministic CFG order."
  (sort (copy-list (bb-successors block)) #'< :key #'bb-id))

(defun %opt-bl-backedge-p (from to)
  "A CFG edge is cyclic when its target dominates its source."
  (and (bb-idom to) (cfg-dominates-p to from)))

(defun %opt-bl-compute-path-counts (cfg)
  "Compute Ball-Larus NumPaths on the CFG DAG after removing backedges."
  (cfg-compute-dominators cfg)
  (let ((memo (make-hash-table :test #'eq)))
    (labels ((count-from (block visiting)
               (or (gethash block memo)
                   (setf (gethash block memo)
                         (let ((dag-successors
                                 (remove-if (lambda (succ)
                                              (or (%opt-bl-backedge-p block succ)
                                                  (member succ visiting :test #'eq)))
                                            (%opt-bl-successors block))))
                           (if dag-successors
                               (loop for succ in dag-successors
                                     sum (count-from succ (cons block visiting)))
                               1))))))
      (count-from (cfg-entry cfg) nil)
      memo)))

(defun %opt-bl-build-edge-table (cfg path-counts)
  "Assign edge values with the Ball-Larus cumulative-successor algorithm."
  (let ((table (make-hash-table :test #'equal))
        (edges nil))
    (loop for block across (cfg-blocks cfg) do
      (let ((offset 0))
        (dolist (succ (%opt-bl-successors block))
          (let* ((backedge-p (%opt-bl-backedge-p block succ))
                 (edge (make-opt-ball-larus-edge
                        :from block
                        :to succ
                        :value offset
                        :backedge-p backedge-p)))
            (setf (gethash (cons block succ) table) edge)
            (push edge edges)
            (unless backedge-p
              (incf offset (gethash succ path-counts 1)))))))
    (values table (nreverse edges))))

(defun %opt-bl-ensure-labels (cfg)
  "Give every CFG block a concrete VM label so inserted edge thunks can jump to it."
  (loop for block across (cfg-blocks cfg) do
    (unless (bb-label block)
      (let ((label (make-vm-label :name (format nil "BL_B~D" (bb-id block)))))
        (setf (bb-label block) label
              (gethash (vm-name label) (cfg-label->block cfg)) block))))
  cfg)

(defun %opt-bl-edge-instrumentation (edge &key (function-id :anonymous)
                                          (table-name +opt-bl-default-counter-table+))
  "Return VM instructions for traversing EDGE.

The required arithmetic is deliberately explicit: VM-CONST materializes the edge
weight, VM-ADD accumulates the path sum, VM-CONST materializes counter delta 1,
and OPT-VM-PATH-PROFILE-RECORD increments the path counter at exits/backedges."
  (let ((insts nil)
        (value (opt-ball-larus-edge-value edge)))
    (push (make-vm-const :dst +opt-bl-edge-reg+ :value value) insts)
    (push (make-vm-add :dst +opt-bl-path-acc-reg+
                       :lhs +opt-bl-path-acc-reg+
                       :rhs +opt-bl-edge-reg+)
          insts)
    (when (or (opt-ball-larus-edge-exit-p edge)
              (opt-ball-larus-edge-backedge-p edge))
      (push (make-vm-const :dst +opt-bl-one-reg+ :value 1) insts)
      (push (make-opt-vm-path-profile-record
             :path-reg +opt-bl-path-acc-reg+
             :table-name table-name
             :function-id function-id
             :delta-reg +opt-bl-one-reg+)
            insts)
      (push (make-vm-const :dst +opt-bl-path-acc-reg+ :value 0) insts))
    (nreverse insts)))

(defun %opt-bl-exit-instrumentation (block &key (function-id :anonymous))
  (%opt-bl-edge-instrumentation
   (make-opt-ball-larus-edge :from block :to nil :value 0 :exit-p t)
   :function-id function-id))

(defun %opt-bl-instrument-linear-edge (block succ edge-table function-id)
  (%opt-bl-edge-instrumentation (gethash (cons block succ) edge-table)
                                :function-id function-id))

(defun %opt-bl-instrument-jump (block target edge-table function-id)
  (append (%opt-bl-instrument-linear-edge block target edge-table function-id)
          (list (make-vm-jump :label (%opt-bl-block-label-name target)))))

(defun %opt-bl-instrument-jump-zero (block term target fallthrough edge-table function-id)
  "Lower a conditional CFG edge to explicit taken/fallthrough instrumentation pads."
  (let ((taken-label (format nil "BL_EDGE_~D_~D_T" (bb-id block) (bb-id target)))
        (fall-label (%opt-bl-block-label-name fallthrough))
        (target-label (%opt-bl-block-label-name target)))
    (append (list (make-vm-jump-zero :reg (vm-reg term) :label taken-label))
            (%opt-bl-instrument-linear-edge block fallthrough edge-table function-id)
            (list (make-vm-jump :label fall-label)
                  (make-vm-label :name taken-label))
            (%opt-bl-instrument-linear-edge block target edge-table function-id)
            (list (make-vm-jump :label target-label)))))

(defun %opt-bl-instrument-block (block next-block edge-table function-id &key entry-p)
  "Emit one CFG block plus Ball-Larus instrumentation for its outgoing edge(s)."
  (let* ((label (bb-label block))
         (term (%opt-bl-block-terminator block))
         (body (if term
                   (remove term (bb-instructions block) :count 1 :from-end t :test #'eq)
                   (copy-list (bb-instructions block))))
         (successors (%opt-bl-successors block)))
    (append (list label)
            (when entry-p (list (make-vm-const :dst +opt-bl-path-acc-reg+ :value 0)))
            body
            (cond
              ((null successors)
               (append (%opt-bl-exit-instrumentation block :function-id function-id)
                       (when term (list term))))
              ((typep term 'vm-jump)
               (%opt-bl-instrument-jump block (first successors) edge-table function-id))
              ((typep term 'vm-jump-zero)
               (let* ((jump-target (find (vm-label-name term) successors
                                         :key #'%opt-bl-block-label-name :test #'equal))
                      (fallthrough (or (and next-block (find next-block successors :test #'eq))
                                       (find-if-not (lambda (b) (eq b jump-target)) successors))))
                 (unless (and jump-target fallthrough)
                   (error "Cannot resolve conditional Ball-Larus edges for block ~D" (bb-id block)))
                 (%opt-bl-instrument-jump-zero block term jump-target fallthrough edge-table function-id)))
              (t
               (append (%opt-bl-instrument-linear-edge block (first successors) edge-table function-id)
                       (when (and term (not (typep term 'vm-jump-zero))) (list term))))))))

(defun opt-instrument-path-profile (instructions &key (function-id :anonymous))
  "Return INSTRUCTIONS instrumented with real Ball-Larus path profiling code."
  (let* ((cfg (%opt-bl-ensure-labels (cfg-build instructions)))
         (path-counts (%opt-bl-compute-path-counts cfg)))
    (multiple-value-bind (edge-table edges) (%opt-bl-build-edge-table cfg path-counts)
      (let ((blocks (coerce (cfg-blocks cfg) 'list)))
        (values
         (loop for (block . rest) on blocks
               append (%opt-bl-instrument-block block (car rest) edge-table function-id
                                                :entry-p (eq block (cfg-entry cfg))))
         cfg
         edges
         path-counts)))))

(defun %opt-bl-enumerate-paths (cfg edge-table)
  "Enumerate acyclic Ball-Larus paths as plists (:sum N :blocks (...))."
  (labels ((walk (block sum blocks visiting)
             (let ((successors (remove-if (lambda (succ)
                                            (or (%opt-bl-backedge-p block succ)
                                                (member succ visiting :test #'eq)))
                                          (%opt-bl-successors block))))
               (if successors
                   (loop for succ in successors append
                     (let ((edge (gethash (cons block succ) edge-table)))
                       (walk succ (+ sum (opt-ball-larus-edge-value edge))
                             (cons succ blocks)
                             (cons block visiting))))
                   (list (list :sum sum :blocks (nreverse blocks)))))))
    (when (cfg-entry cfg)
      (walk (cfg-entry cfg) 0 (list (cfg-entry cfg)) nil))))

(defun opt-build-ball-larus-profile (instructions &key (function-id :anonymous))
  "Build the Ball-Larus analysis and instrumented instruction stream."
  (multiple-value-bind (instrumented cfg edges path-counts)
      (opt-instrument-path-profile instructions :function-id function-id)
    (multiple-value-bind (edge-table ignored) (%opt-bl-build-edge-table cfg path-counts)
      (declare (ignore ignored))
      (make-opt-ball-larus-profile
       :cfg cfg
       :edges edges
       :paths (%opt-bl-enumerate-paths cfg edge-table)
       :instrumented-instructions instrumented
       :function-id function-id
       :blocks (loop for block across (cfg-blocks cfg)
                     collect (make-opt-path-profile-block
                              :block-id (bb-id block)
                              :label (%opt-bl-block-label-name block)
                              :path-id (gethash block path-counts 1)
                              :successor-count (length (bb-successors block))
                              :path-count (gethash block path-counts 1)))))))

(defun opt-compute-path-profile (instructions &key (counts nil))
  "Compute Ball-Larus path metadata for INSTRUCTIONS.

COUNTS may be keyed by block label/id for compatibility with older callers; path
counter tables are consumed by OPT-BUILD-BLOCK-VERSION-PLAN."
  (let ((profile (opt-build-ball-larus-profile instructions)))
    (loop for block in (opt-ball-larus-profile-blocks profile)
          for count = (or (and counts
                               (or (gethash (opt-path-profile-block-label block) counts)
                                   (gethash (opt-path-profile-block-block-id block) counts)))
                          0)
          do (setf (opt-path-profile-block-execution-count block) count)
          collect block)))

(defun %opt-bl-path-count (counts function-id path-sum)
  (cond
    ((hash-table-p counts)
     (or (gethash (list function-id path-sum) counts)
         (gethash path-sum counts)
         0))
    ((listp counts)
     (or (cdr (assoc (list function-id path-sum) counts :test #'equal))
         (cdr (assoc path-sum counts :test #'equal))
         0))
    (t 0)))

(defun opt-identify-hot-paths (profile counts &key (hot-threshold 1) (limit nil))
  "Return hot Ball-Larus paths sorted by decreasing execution count."
  (let* ((function-id (opt-ball-larus-profile-function-id profile))
         (paths (loop for path in (opt-ball-larus-profile-paths profile)
                      for count = (%opt-bl-path-count counts function-id (getf path :sum))
                      when (>= count hot-threshold)
                        collect (list* :count count path)))
         (sorted (sort paths (lambda (a b)
                               (or (> (getf a :count) (getf b :count))
                                   (and (= (getf a :count) (getf b :count))
                                        (< (getf a :sum) (getf b :sum))))))))
    (if limit (subseq sorted 0 (min limit (length sorted))) sorted)))

(defun opt-build-block-version-plan (path-profile &key (hot-threshold 1) counts limit)
  "Build a hot-path block-versioning plan from Ball-Larus counters."
  (make-opt-block-version-plan
   :hot-threshold hot-threshold
   :versions
   (cond
     ((typep path-profile 'opt-ball-larus-profile)
      (mapcar (lambda (path)
                (list :version :superblock
                      :path-sum (getf path :sum)
                      :count (getf path :count)
                      :blocks (mapcar #'bb-id (getf path :blocks))))
              (opt-identify-hot-paths path-profile counts
                                      :hot-threshold hot-threshold
                                      :limit limit)))
     (t
      ;; Compatibility path for older callers that pass OPT-COMPUTE-PATH-PROFILE's
      ;; block list rather than a complete Ball-Larus profile.
      (loop for block in path-profile
            when (>= (opt-path-profile-block-execution-count block) hot-threshold)
              collect (list :block-id (opt-path-profile-block-block-id block)
                            :label (opt-path-profile-block-label block)
                            :path-id (opt-path-profile-block-path-id block)
                            :version :hot))))))

(defun %opt-bl-clone-label (path-sum block)
  (format nil "BL_SUPER_~D_B~D" path-sum (bb-id block)))

(defun %opt-bl-remap-jump (inst label-map)
  (typecase inst
    (vm-jump (make-vm-jump :label (or (gethash (vm-label-name inst) label-map)
                                      (vm-label-name inst))))
    (vm-jump-zero (make-vm-jump-zero :reg (vm-reg inst)
                                     :label (or (gethash (vm-label-name inst) label-map)
                                                (vm-label-name inst))))
    (t inst)))

(defun %opt-bl-superblock-instructions (path)
  "Clone PATH's blocks into one fall-through superblock version."
  (let* ((path-sum (getf path :sum))
         (blocks (getf path :blocks))
         (label-map (make-hash-table :test #'equal)))
    (dolist (block blocks)
      (setf (gethash (%opt-bl-block-label-name block) label-map)
            (%opt-bl-clone-label path-sum block)))
    (loop for (block . rest) on blocks append
      (let* ((last-p (null rest))
             (term (%opt-bl-block-terminator block))
             (body (if (and term (not last-p))
                       (remove term (bb-instructions block) :count 1 :from-end t :test #'eq)
                       (bb-instructions block))))
        (append (list (make-vm-label :name (%opt-bl-clone-label path-sum block)))
                (mapcar (lambda (inst) (%opt-bl-remap-jump inst label-map)) body))))))

(defun opt-duplicate-hot-paths (instructions counts &key (hot-threshold 1)
                                             (function-id :anonymous) (limit 4))
  "Append path-specific superblock clones for hot Ball-Larus paths.

The clones are emitted as independent basic-block versions.  Later layout or tier
selection can redirect execution to them safely because each superblock preserves
the original instructions for its path while removing internal branch terminators
whose outcomes are known on that path."
  (let* ((profile (opt-build-ball-larus-profile instructions :function-id function-id))
         (hot-paths (opt-identify-hot-paths profile counts
                                            :hot-threshold hot-threshold
                                            :limit limit)))
    (append instructions (mapcan #'%opt-bl-superblock-instructions hot-paths))))

(defun opt-pass-path-profiling (instructions)
  "Optimizer pipeline hook: insert Ball-Larus path profiling instrumentation."
  (opt-instrument-path-profile instructions))
