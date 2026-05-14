(in-package :cl-cc/optimize)

;;; ─── Generic Dataflow Framework ───────────────────────────────────────────

(defstruct (opt-dataflow-result (:conc-name opt-dataflow-result-))
  "Stores per-block IN and OUT states for a dataflow analysis."
  cfg
  direction
  (in  (make-hash-table :test #'eq) :type hash-table)
  (out (make-hash-table :test #'eq) :type hash-table))

(defstruct (opt-abstract-domain (:conc-name opt-domain-))
  "Generic abstract interpretation domain descriptor.

JOIN/LEQ/WIDEN/NARROW are binary operators over abstract states.
TRANSFER maps (block in-state) -> out-state for the chosen domain."
  name
  top
  bottom
  join
  leq
  widen
  narrow
  transfer)

(defun %opt-ensure-cfg (cfg-or-instructions)
  "Return CFG-OR-INSTRUCTIONS as a CFG, building one when needed."
  (if (cfg-p cfg-or-instructions)
      cfg-or-instructions
      (cfg-build cfg-or-instructions)))

(defun %opt-dataflow-copy-state (state copy-state)
  "Copy STATE with COPY-STATE when provided, else return STATE unchanged."
  (if copy-state
      (funcall copy-state state)
      state))

(defun %opt-dataflow-state-set-equal-p (a b &key (test #'equal))
  "Return T when A and B contain the same set of elements under TEST."
  (and (= (length a) (length b))
       (subsetp a b :test test)
       (subsetp b a :test test)))

(defun %opt-dataflow-boundary-block (cfg direction)
  "Return the entry/exit boundary block for DIRECTION in CFG."
  (ecase direction
    (:forward  (cfg-entry cfg))
    (:backward (cfg-exit cfg))))

(defun %opt-dataflow-neighbors (block direction)
  "Return the predecessor/successor blocks that flow into BLOCK."
  (ecase direction
    (:forward  (bb-predecessors block))
    (:backward (bb-successors block))))

(defun %opt-dataflow-users (block direction)
  "Return the blocks whose input depends on BLOCK's output."
  (ecase direction
    (:forward  (bb-successors block))
    (:backward (bb-predecessors block))))

(defun %opt-dataflow-merge-states (states meet initial-state copy-state)
  "Merge STATES with MEET, or copy INITIAL-STATE when STATES is empty."
  (if states
      (funcall meet states)
      (%opt-dataflow-copy-state initial-state copy-state)))

(defun opt-run-dataflow (cfg &key
                               (direction :forward)
                               meet
                               transfer
                               (state-equal #'equal)
                               (initial-state nil)
                               (boundary-state initial-state)
                               (copy-state #'identity))
  "Run a generic worklist dataflow analysis over CFG.

MEET merges a list of predecessor/successor states. TRANSFER computes the new
output state for BLOCK from its incoming state. STATE-EQUAL compares states for
convergence. INITIAL-STATE seeds unreachable edges; BOUNDARY-STATE is used at
the entry block for forward analyses and exit block for backward analyses."
  (let* ((graph          (%opt-ensure-cfg cfg))
         (boundary-block (%opt-dataflow-boundary-block graph direction))
         (blocks         (loop for block across (cfg-blocks graph)
                               when block collect block))
         (result         (make-opt-dataflow-result :cfg graph :direction direction))
         (in-map         (opt-dataflow-result-in result))
         (out-map        (opt-dataflow-result-out result))
         (worklist       (copy-list blocks)))
    (dolist (block blocks)
      (setf (gethash block in-map)  (%opt-dataflow-copy-state initial-state copy-state)
            (gethash block out-map) (%opt-dataflow-copy-state initial-state copy-state)))
    (when boundary-block
      (ecase direction
        (:forward
         (setf (gethash boundary-block in-map)
               (%opt-dataflow-copy-state boundary-state copy-state)))
        (:backward
         (setf (gethash boundary-block out-map)
               (%opt-dataflow-copy-state boundary-state copy-state)))))
    (loop while worklist
          do (let* ((block     (pop worklist))
                    (incoming  (%opt-dataflow-neighbors block direction))
                    (state-in  (loop for other in incoming
                                     collect (ecase direction
                                               (:forward  (gethash other out-map))
                                               (:backward (gethash other in-map))))))
               (ecase direction
                 (:forward
                  (let* ((new-in (if (eq block boundary-block)
                                     (%opt-dataflow-copy-state boundary-state copy-state)
                                     (%opt-dataflow-merge-states state-in meet initial-state copy-state)))
                         (old-out (gethash block out-map))
                         (new-out (funcall transfer block new-in)))
                    (setf (gethash block in-map) new-in)
                    (unless (funcall state-equal old-out new-out)
                      (setf (gethash block out-map) new-out)
                      (dolist (user (%opt-dataflow-users block direction))
                        (pushnew user worklist :test #'eq)))))
                 (:backward
                  (let* ((new-out (if (eq block boundary-block)
                                      (%opt-dataflow-copy-state boundary-state copy-state)
                                      (%opt-dataflow-merge-states state-in meet initial-state copy-state)))
                         (old-in (gethash block in-map))
                         (new-in (funcall transfer block new-out)))
                    (setf (gethash block out-map) new-out)
                    (unless (funcall state-equal old-in new-in)
                      (setf (gethash block in-map) new-in)
                      (dolist (user (%opt-dataflow-users block direction))
                        (pushnew user worklist :test #'eq))))))))
    result))

(defmacro define-dataflow-pass (name lambda-list &rest options)
  "Define NAME as a thin wrapper around OPT-RUN-DATAFLOW."
  (let* ((cfg-arg        (car lambda-list))
         (missing        (gensym "MISSING"))
         (direction      (or (getf options :direction) :forward))
         (meet           (getf options :meet))
         (transfer       (getf options :transfer))
         (state-equal    (or (getf options :state-equal) '#'equal))
         (initial-state  (getf options :initial-state))
         (boundary-state (let ((value (getf options :boundary-state missing)))
                           (if (eq value missing)
                               initial-state
                               value)))
         (copy-state     (or (getf options :copy-state) '#'identity))
         (documentation  (or (getf options :documentation)
                             (format nil "Run the ~A dataflow analysis." name))))
    `(defun ,name ,lambda-list
       ,documentation
       (let ((cfg (%opt-ensure-cfg ,cfg-arg)))
          (opt-run-dataflow cfg
                            :direction ,direction
                           :meet ,meet
                           :transfer ,transfer
                           :state-equal ,state-equal
                           :initial-state ,initial-state
                            :boundary-state ,boundary-state
                            :copy-state ,copy-state)))))

(defun opt-run-abstract-interpretation (cfg-or-instructions domain
                                       &key
                                         (direction :forward)
                                         (widen-after 0)
                                         (copy-state #'identity))
  "Run abstract interpretation over CFG-OR-INSTRUCTIONS using DOMAIN.

This is a thin orchestration layer over OPT-RUN-DATAFLOW that adds a pluggable
domain API (join/leq/widen/narrow/transfer). Widening starts after WIDEN-AFTER
iterations to accelerate convergence on growing lattices."
  (let* ((cfg (%opt-ensure-cfg cfg-or-instructions))
         (join (opt-domain-join domain))
         (leq (or (opt-domain-leq domain) #'equal))
         (widen (opt-domain-widen domain))
         (narrow (opt-domain-narrow domain))
         (transfer (opt-domain-transfer domain))
         (boundary (opt-domain-bottom domain))
         (iteration 0))
    (declare (ignore narrow))
    (opt-run-dataflow cfg
                      :direction direction
                      :meet (lambda (states)
                              (cond
                                ((null states) (%opt-dataflow-copy-state boundary copy-state))
                                ((null (cdr states)) (%opt-dataflow-copy-state (car states) copy-state))
                                (t
                                 (let ((acc (%opt-dataflow-copy-state (car states) copy-state)))
                                   (dolist (state (cdr states) acc)
                                     (setf acc (funcall join acc state)
                                           acc (if (and widen (> iteration widen-after))
                                                   (funcall widen acc state)
                                                   acc)))))))
                      :transfer transfer
                      :state-equal (lambda (old new)
                                     (prog1 (funcall leq old new)
                                       (incf iteration)))
                      :initial-state (opt-domain-top domain)
                      :boundary-state boundary
                      :copy-state copy-state)))

;;; ─── Available Expressions ────────────────────────────────────────────────

(defun %available-expression-inst-p (inst)
  "Return T when INST contributes a conservative available expression."
  (let ((dst   (opt-inst-dst inst))
        (reads (opt-inst-read-regs inst)))
    (and dst reads (opt-inst-pure-p inst))))

(defun %available-expression-entry (inst block instruction-index)
  "Build the available-expression entry recorded for INST."
  (let ((reads (copy-list (opt-inst-read-regs inst))))
    (list (cons (type-of inst) reads)
          reads
          (opt-inst-dst inst)
          (bb-id block)
          instruction-index
          inst)))

(defun %available-expression-entry-key (entry)
  "Return the stable expression key stored in ENTRY."
  (first entry))

(defun %available-expression-entry-reads (entry)
  "Return the read-register list stored in ENTRY."
  (second entry))

(defun %available-expression-state-equal (a b)
  "Return T when A and B contain the same available-expression keys."
  (%opt-dataflow-state-set-equal-p
   (mapcar #'%available-expression-entry-key a)
   (mapcar #'%available-expression-entry-key b)
   :test #'equal))

(defun %available-expression-meet (states)
  "Intersect available expressions across STATES."
  (cond
    ((null states) nil)
    ((null (cdr states)) (copy-list (car states)))
    (t (let ((others (cdr states)))
         (loop for entry in (car states)
               for key = (%available-expression-entry-key entry)
               when (every (lambda (state)
                             (find key state
                                   :test #'equal
                                   :key #'%available-expression-entry-key))
                           others)
               collect entry)))))

(defun %available-expression-transfer (block in-state)
  "Transfer available expressions through BLOCK."
  (let ((state (copy-list in-state)))
    (loop for inst in (bb-instructions block)
          for instruction-index from 0
          do (let ((dst (opt-inst-dst inst)))
               (when dst
                 (setf state
                       (remove-if (lambda (entry)
                                    (member dst (%available-expression-entry-reads entry)
                                            :test #'eq))
                                  state)))
               (when (%available-expression-inst-p inst)
                 (let* ((entry (%available-expression-entry inst block instruction-index))
                        (key   (%available-expression-entry-key entry)))
                   (setf state
                         (cons entry
                               (remove key state
                                       :test #'equal
                                       :key #'%available-expression-entry-key)))))))
    state))

(define-dataflow-pass opt-compute-available-expressions (cfg-or-instructions)
  :direction :forward
  :meet #'%available-expression-meet
  :transfer #'%available-expression-transfer
  :state-equal #'%available-expression-state-equal
  :initial-state nil
  :boundary-state nil
  :copy-state #'copy-list
  :documentation "Compute conservative available expressions for CFG-OR-INSTRUCTIONS.")

;;; ─── Reaching Definitions ────────────────────────────────────────────────

(defun %reaching-definition-entry (reg block instruction-index inst)
  "Return the reaching-definition tuple for REG at BLOCK/INSTRUCTION-INDEX."
  (list reg (bb-id block) instruction-index inst))

(defun %reaching-definition-reg (definition)
  "Return the destination register described by DEFINITION."
  (first definition))

(defun %reaching-definitions-meet (states)
  "Union reaching definitions across STATES."
  (let ((merged nil))
    (dolist (state states)
      (dolist (definition state)
        (pushnew definition merged :test #'equal)))
    (nreverse merged)))

(defun %reaching-definitions-state-equal (a b)
  "Return T when A and B contain the same reaching-definition tuples."
  (%opt-dataflow-state-set-equal-p a b :test #'equal))

(defun %reaching-definitions-transfer (block in-state)
  "Transfer reaching definitions through BLOCK."
  (let ((state (copy-list in-state)))
    (loop for inst in (bb-instructions block)
          for instruction-index from 0
          do (let ((dst (opt-inst-dst inst)))
               (when dst
                 (setf state
                       (remove dst state :test #'eq :key #'%reaching-definition-reg))
                 (push (%reaching-definition-entry dst block instruction-index inst)
                       state))))
    state))

(define-dataflow-pass opt-compute-reaching-definitions (cfg-or-instructions)
  :direction :forward
  :meet #'%reaching-definitions-meet
  :transfer #'%reaching-definitions-transfer
  :state-equal #'%reaching-definitions-state-equal
  :initial-state nil
  :boundary-state nil
  :copy-state #'copy-list
  :documentation "Compute reaching definitions for CFG-OR-INSTRUCTIONS.")
