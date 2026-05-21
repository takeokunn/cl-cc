(in-package :cl-cc/runtime)

(defconstant +raft-follower+ 0)
(defconstant +raft-candidate+ 1)
(defconstant +raft-leader+ 2)
(defconstant +raft-election-timeout-min-ms+ 150)
(defconstant +raft-election-timeout-max-ms+ 300)
(defconstant +raft-heartbeat-timeout-ms+ 50)

(defstruct rt-raft-entry
  (term 0)
  (index 0)
  (command nil))

(defstruct rt-raft-node
  (id "")
  (state +raft-follower+)
  (current-term 0)
  (voted-for nil)
  (log nil)
  (commit-index 0)
  (last-applied 0)
  (election-timeout +raft-election-timeout-min-ms+)
  (election-timer 0)
  (heartbeat-timer 0)
  (next-index (make-hash-table :test #'equal))
  (match-index (make-hash-table :test #'equal))
  (votes-received nil)
  (state-machine nil))

(defstruct rt-raft-cluster
  (nodes (make-hash-table :test #'equal))
  (node-ids nil)
  (leader-id nil))

(defun %rt-raft-random-election-timeout ()
  (+ +raft-election-timeout-min-ms+
     (random (1+ (- +raft-election-timeout-max-ms+
                  +raft-election-timeout-min-ms+)))))

(defun %rt-raft-reset-election-timer (node)
  (setf (rt-raft-node-election-timeout node) (%rt-raft-random-election-timeout)
        (rt-raft-node-election-timer node) 0)
  node)

(defun %rt-raft-log-forward (node)
  (reverse (rt-raft-node-log node)))

(defun %rt-raft-last-log-index (node)
  (length (rt-raft-node-log node)))

(defun %rt-raft-last-log-term (node)
  (let ((entry (car (rt-raft-node-log node))))
    (if entry (rt-raft-entry-term entry) 0)))

(defun %rt-raft-entry-at (node index)
  (when (plusp index)
    (nth (1- index) (%rt-raft-log-forward node))))

(defun %rt-raft-log-term-at (node index)
  (if (zerop index)
      0
      (let ((entry (%rt-raft-entry-at node index)))
        (and entry (rt-raft-entry-term entry)))))

(defun %rt-raft-majority (cluster)
  (1+ (floor (length (rt-raft-cluster-node-ids cluster)) 2)))

(defun %rt-raft-step-down (node term cluster)
  (when (> term (rt-raft-node-current-term node))
    (setf (rt-raft-node-current-term node) term
          (rt-raft-node-voted-for node) nil))
  (setf (rt-raft-node-state node) +raft-follower+
        (rt-raft-cluster-leader-id cluster) nil
        (rt-raft-node-votes-received node) nil)
  (%rt-raft-reset-election-timer node))

(defun rt-make-raft-node (id &key (election-timeout nil))
  (let ((node (make-rt-raft-node :id id)))
    (setf (rt-raft-node-election-timeout node)
          (or election-timeout (%rt-raft-random-election-timeout))
          (rt-raft-node-election-timer node)
          (random (rt-raft-node-election-timeout node)))
    node))

(defun rt-make-raft-cluster (ids)
  (let ((cluster (make-rt-raft-cluster :node-ids ids)))
    (dolist (id ids)
      (setf (gethash id (rt-raft-cluster-nodes cluster))
            (rt-make-raft-node id)))
    cluster))

(defun rt-raft-become-leader (node cluster)
  (setf (rt-raft-node-state node) +raft-leader+
        (rt-raft-cluster-leader-id cluster) (rt-raft-node-id node)
        (rt-raft-node-heartbeat-timer node) 0)
  (clrhash (rt-raft-node-next-index node))
  (clrhash (rt-raft-node-match-index node))
  (let ((next (1+ (%rt-raft-last-log-index node))))
    (dolist (peer-id (rt-raft-cluster-node-ids cluster))
      (setf (gethash peer-id (rt-raft-node-next-index node)) next
            (gethash peer-id (rt-raft-node-match-index node)) 0))
    (setf (gethash (rt-raft-node-id node) (rt-raft-node-match-index node))
          (%rt-raft-last-log-index node)))
  node)

(defun rt-raft-request-vote (candidate voter)
  (let ((candidate-term (rt-raft-node-current-term candidate)))
    (cond
      ((< candidate-term (rt-raft-node-current-term voter)) nil)
      (t
       (when (> candidate-term (rt-raft-node-current-term voter))
         (setf (rt-raft-node-current-term voter) candidate-term
               (rt-raft-node-voted-for voter) nil
               (rt-raft-node-state voter) +raft-follower+))
       (let* ((voted-for (rt-raft-node-voted-for voter))
              (candidate-log-term (%rt-raft-last-log-term candidate))
              (voter-log-term (%rt-raft-last-log-term voter))
              (candidate-log-index (%rt-raft-last-log-index candidate))
              (voter-log-index (%rt-raft-last-log-index voter))
              (log-current-p (or (> candidate-log-term voter-log-term)
                                 (and (= candidate-log-term voter-log-term)
                                      (>= candidate-log-index voter-log-index)))))
         (when (and log-current-p
                    (or (null voted-for)
                        (equal voted-for (rt-raft-node-id candidate))))
           (setf (rt-raft-node-voted-for voter) (rt-raft-node-id candidate))
           (%rt-raft-reset-election-timer voter)
           t))))))

(defun rt-raft-start-election (node cluster)
  "Start a Raft election for NODE in CLUSTER and return true if it wins."
  (setf (rt-raft-node-state node) +raft-candidate+
        (rt-raft-node-current-term node) (1+ (rt-raft-node-current-term node))
        (rt-raft-node-voted-for node) (rt-raft-node-id node)
        (rt-raft-node-votes-received node) (list (rt-raft-node-id node)))
  (%rt-raft-reset-election-timer node)
  (dolist (peer-id (rt-raft-cluster-node-ids cluster))
    (unless (equal peer-id (rt-raft-node-id node))
      (let ((peer (gethash peer-id (rt-raft-cluster-nodes cluster))))
        (when peer
          (cond
            ((> (rt-raft-node-current-term peer) (rt-raft-node-current-term node))
             (%rt-raft-step-down node (rt-raft-node-current-term peer) cluster))
            ((rt-raft-request-vote node peer)
             (pushnew peer-id (rt-raft-node-votes-received node) :test #'equal)))))))
  (when (and (eq (rt-raft-node-state node) +raft-candidate+)
             (>= (length (rt-raft-node-votes-received node))
                 (%rt-raft-majority cluster)))
    (rt-raft-become-leader node cluster)
    t))

(defun %rt-raft-install-entries (follower entries prev-index)
  (let ((kept (loop for entry in (%rt-raft-log-forward follower)
                    when (<= (rt-raft-entry-index entry) prev-index)
                      collect entry)))
    (setf (rt-raft-node-log follower) (reverse (append kept entries)))))

(defun rt-raft-apply (node &optional apply-function)
  "Apply committed entries to NODE's state machine.
APPLY-FUNCTION receives (current-state command) and returns the new state.
Without APPLY-FUNCTION, commands are appended to the state-machine list."
  (loop while (< (rt-raft-node-last-applied node)
                 (rt-raft-node-commit-index node))
        for next-index = (1+ (rt-raft-node-last-applied node))
        for entry = (%rt-raft-entry-at node next-index)
        while entry
        do (setf (rt-raft-node-state-machine node)
                 (if apply-function
                     (funcall apply-function
                              (rt-raft-node-state-machine node)
                              (rt-raft-entry-command entry))
                     (append (rt-raft-node-state-machine node)
                             (list (rt-raft-entry-command entry))))
                 (rt-raft-node-last-applied node) next-index))
  (rt-raft-node-state-machine node))

(defun %rt-raft-handle-append-entries (leader follower cluster entries prev-index prev-term leader-commit)
  (cond
    ((< (rt-raft-node-current-term leader) (rt-raft-node-current-term follower)) nil)
    ((not (eql (%rt-raft-log-term-at follower prev-index) prev-term)) nil)
    (t
     (when (> (rt-raft-node-current-term leader) (rt-raft-node-current-term follower))
       (setf (rt-raft-node-current-term follower) (rt-raft-node-current-term leader)
             (rt-raft-node-voted-for follower) nil))
     (setf (rt-raft-node-state follower) +raft-follower+
           (rt-raft-cluster-leader-id cluster) (rt-raft-node-id leader))
     (%rt-raft-reset-election-timer follower)
     (when entries
       (%rt-raft-install-entries follower entries prev-index))
     (setf (rt-raft-node-commit-index follower)
           (min leader-commit (%rt-raft-last-log-index follower)))
     (rt-raft-apply follower)
     t)))

(defun rt-raft-advance-commit (leader cluster)
  "Advance LEADER's commit index when a majority has replicated an entry."
  (when (eq (rt-raft-node-state leader) +raft-leader+)
    (loop for index from (1+ (rt-raft-node-commit-index leader))
            to (%rt-raft-last-log-index leader)
          for entry = (%rt-raft-entry-at leader index)
          when (and entry
                    (= (rt-raft-entry-term entry) (rt-raft-node-current-term leader))
                    (>= (loop for peer-id in (rt-raft-cluster-node-ids cluster)
                              count (>= (gethash peer-id (rt-raft-node-match-index leader) 0)
                                        index))
                        (%rt-raft-majority cluster)))
            do (setf (rt-raft-node-commit-index leader) index)))
  (rt-raft-node-commit-index leader))

(defun rt-raft-append-entries (leader cluster &optional peer-id)
  "Send AppendEntries heartbeats/log entries from LEADER to followers."
  (unless (eq (rt-raft-node-state leader) +raft-leader+)
    (return-from rt-raft-append-entries nil))
  (let ((replicated 1)
        (old-commit (rt-raft-node-commit-index leader))
        (targets (if peer-id
                     (list peer-id)
                     (remove (rt-raft-node-id leader)
                             (rt-raft-cluster-node-ids cluster)
                             :test #'equal))))
    (dolist (id targets)
      (let ((follower (gethash id (rt-raft-cluster-nodes cluster))))
        (when follower
          (let* ((next-index (max 1 (gethash id (rt-raft-node-next-index leader)
                                             (1+ (%rt-raft-last-log-index leader)))))
                 (prev-index (1- next-index))
                 (prev-term (%rt-raft-log-term-at leader prev-index))
                 (entries (loop for entry in (%rt-raft-log-forward leader)
                                when (>= (rt-raft-entry-index entry) next-index)
                                  collect entry)))
            (if (%rt-raft-handle-append-entries
                 leader follower cluster entries prev-index prev-term
                 (rt-raft-node-commit-index leader))
                (let ((match-index (if entries
                                       (rt-raft-entry-index (car (last entries)))
                                       prev-index)))
                  (setf (gethash id (rt-raft-node-match-index leader)) match-index
                        (gethash id (rt-raft-node-next-index leader)) (1+ match-index))
                  (incf replicated))
                (setf (gethash id (rt-raft-node-next-index leader))
                      (max 1 (1- next-index))))))))
    (setf (gethash (rt-raft-node-id leader) (rt-raft-node-match-index leader))
          (%rt-raft-last-log-index leader))
    (rt-raft-advance-commit leader cluster)
    (when (> (rt-raft-node-commit-index leader) old-commit)
      (dolist (id targets)
        (let ((follower (gethash id (rt-raft-cluster-nodes cluster))))
          (when follower
            (let ((prev-index (%rt-raft-last-log-index leader)))
              (%rt-raft-handle-append-entries
               leader follower cluster nil prev-index
               (%rt-raft-log-term-at leader prev-index)
               (rt-raft-node-commit-index leader)))))))
    (rt-raft-apply leader)
    replicated))

(defun rt-raft-tick (node cluster &optional (elapsed-ms 1))
  "Advance Raft timers. Followers/candidates may start elections; leaders send heartbeats."
  (cond
    ((eq (rt-raft-node-state node) +raft-leader+)
     (incf (rt-raft-node-heartbeat-timer node) elapsed-ms)
     (when (>= (rt-raft-node-heartbeat-timer node) +raft-heartbeat-timeout-ms+)
       (setf (rt-raft-node-heartbeat-timer node) 0)
       (rt-raft-append-entries node cluster)))
    (t
     (incf (rt-raft-node-election-timer node) elapsed-ms)
     (when (>= (rt-raft-node-election-timer node)
               (rt-raft-node-election-timeout node))
       (rt-raft-start-election node cluster)))))

(defun rt-raft-propose (cluster value)
  (let* ((leader-id (rt-raft-cluster-leader-id cluster))
         (leader (and leader-id
                      (gethash leader-id (rt-raft-cluster-nodes cluster)))))
    (unless (and leader (eq (rt-raft-node-state leader) +raft-leader+))
      (error "No leader"))
    (let ((entry (make-rt-raft-entry
                  :term (rt-raft-node-current-term leader)
                  :index (1+ (%rt-raft-last-log-index leader))
                  :command value)))
      (push entry (rt-raft-node-log leader))
      (setf (gethash (rt-raft-node-id leader) (rt-raft-node-match-index leader))
            (rt-raft-entry-index entry))
      (rt-raft-append-entries leader cluster)
      value)))

(defun rt-raft-snapshot (node)
  (%rt-raft-log-forward node))

(defun rt-consensus-init () t)

(export '(+raft-follower+ +raft-candidate+ +raft-leader+
          rt-raft-entry make-rt-raft-entry rt-raft-entry-term
          rt-raft-entry-index rt-raft-entry-command
          rt-raft-node make-rt-raft-node rt-raft-node-id
          rt-raft-node-state rt-raft-node-current-term
          rt-raft-node-voted-for rt-raft-node-log
          rt-raft-node-commit-index rt-raft-node-last-applied
          rt-raft-node-election-timeout rt-raft-node-election-timer
          rt-raft-node-state-machine
          rt-raft-cluster make-rt-raft-cluster rt-raft-cluster-nodes
          rt-raft-cluster-node-ids rt-raft-cluster-leader-id
          rt-make-raft-node rt-make-raft-cluster rt-raft-become-leader
          rt-raft-request-vote rt-raft-start-election rt-raft-append-entries
          rt-raft-tick rt-raft-advance-commit rt-raft-apply
          rt-raft-propose rt-raft-snapshot rt-consensus-init))
