(in-package :cl-cc/optimize)

(defstruct (opt-profile-data (:conc-name opt-profile-))
  "Small profile container for edge, value, call-chain, and allocation data."
  (edges (make-hash-table :test #'equal))
  (values (make-hash-table :test #'equal))
  (call-chains (make-hash-table :test #'equal))
  (alloc-sites (make-hash-table :test #'equal))
  (value-limit 4 :type integer)
  (value-ranges (make-hash-table :test #'equal)))

(defun %opt-profile-incf (table key &optional (delta 1))
  (let ((next (+ delta (gethash key table 0))))
    (setf (gethash key table) next)
    next))

(defun %opt-profile-site-value-entries (profile site-id)
  (let ((entries nil))
    (maphash (lambda (key count)
               (when (equal site-id (first key))
                 (push (cons key count) entries)))
             (opt-profile-values profile))
    entries))

(defun %opt-profile-value-entry-preferred-p (left right)
  (let ((left-count (cdr left))
        (right-count (cdr right)))
    (or (> left-count right-count)
        (and (= left-count right-count)
             (string< (%opt-stable-print-string (second (car left)))
                      (%opt-stable-print-string (second (car right))))))))

(defun %opt-profile-prune-site-values (profile site-id)
  (let ((limit (opt-profile-value-limit profile)))
    (when (plusp limit)
      (let ((entries (%opt-profile-site-value-entries profile site-id)))
        (when (> (length entries) limit)
          (dolist (entry (nthcdr limit
                                 (sort entries #'%opt-profile-value-entry-preferred-p)))
            (remhash (car entry) (opt-profile-values profile))))))))

(defun %opt-profile-record-range (profile site-id value)
  (when (numberp value)
    (let ((current (gethash site-id (opt-profile-value-ranges profile))))
      (if current
          (progn
            (setf (car current) (min (car current) value))
            (setf (cdr current) (max (cdr current) value))
            current)
          (setf (gethash site-id (opt-profile-value-ranges profile))
                (cons value value))))))

(defun opt-profile-record-edge (profile from to &optional (delta 1))
  "Increment the execution count for CFG edge FROM → TO."
  (%opt-profile-incf (opt-profile-edges profile) (cons from to) delta))

(defun opt-profile-record-value (profile site-id value &optional (delta 1))
  "Increment a top-k style value counter for SITE-ID."
  (let ((count (%opt-profile-incf (opt-profile-values profile) (list site-id value) delta)))
    (%opt-profile-record-range profile site-id value)
    (%opt-profile-prune-site-values profile site-id)
    count))

(defun opt-profile-top-values (profile site-id &optional limit)
  "Return SITE-ID's retained top values as sorted (VALUE . COUNT) pairs."
  (let* ((entries (sort (%opt-profile-site-value-entries profile site-id)
                        #'%opt-profile-value-entry-preferred-p))
         (selected (if limit
                       (subseq entries 0 (min limit (length entries)))
                       entries)))
    (mapcar (lambda (entry)
              (cons (second (car entry)) (cdr entry)))
            selected)))

(defun opt-profile-value-range (profile site-id)
  "Return SITE-ID's numeric value range as (MIN . MAX), or NIL when absent."
  (let ((range (gethash site-id (opt-profile-value-ranges profile))))
    (and range (cons (car range) (cdr range)))))

(defun opt-profile-record-call-chain (profile chain &optional (delta 1))
  "Increment a context-sensitive call-chain sample."
  (%opt-profile-incf (opt-profile-call-chains profile) (copy-list chain) delta))

(defun opt-profile-record-allocation (profile site-id bytes &optional (count 1))
  "Record allocation COUNT and BYTES for SITE-ID."
  (let* ((table (opt-profile-alloc-sites profile))
         (current (or (gethash site-id table) (cons 0 0))))
    (incf (car current) count)
    (incf (cdr current) bytes)
    (setf (gethash site-id table) current)
    current))

(defun %opt-pgo-edge-count (edge-counts from to)
  (let ((key (cons from to)))
    (cond
      ((typep edge-counts 'opt-profile-data)
       (gethash key (opt-profile-edges edge-counts) 0))
      ((hash-table-p edge-counts)
       (gethash key edge-counts 0))
      (t (or (cdr (assoc key edge-counts :test #'equal)) 0)))))

(defun opt-pgo-best-successor (block successors edge-counts &optional visited)
  "Return BLOCK's hottest unvisited successor according to EDGE-COUNTS."
  (let ((candidates (remove-if (lambda (successor)
                                 (and visited (gethash successor visited)))
                               successors)))
    (car (sort (copy-list candidates)
               (lambda (left right)
                 (let ((left-count (%opt-pgo-edge-count edge-counts block left))
                       (right-count (%opt-pgo-edge-count edge-counts block right)))
                   (or (> left-count right-count)
                       (and (= left-count right-count)
                            (string< (%opt-stable-print-string left)
                                     (%opt-stable-print-string right))))))))))

(defun opt-pgo-build-hot-chain (entry successors-alist edge-counts)
  "Build a greedy Pettis-Hansen-style hot block chain from ENTRY."
  (let ((visited (make-hash-table :test #'equal))
        (chain nil)
        (current entry))
    (loop while current
          do (push current chain)
             (setf (gethash current visited) t)
             (let* ((successors (copy-list (cdr (assoc current successors-alist :test #'equal))))
                    (next (opt-pgo-best-successor current successors edge-counts visited)))
               (setf current next)))
    (nreverse chain)))

(defun opt-pgo-rotate-loop (loop-chain preferred-exit)
  "Rotate LOOP-CHAIN so PREFERRED-EXIT becomes the loop bottom."
  (let ((index (position preferred-exit loop-chain :test #'equal)))
    (if index
        (append (subseq loop-chain (1+ index))
                (subseq loop-chain 0 (1+ index)))
        (copy-list loop-chain))))

(defun opt-pgo-build-counter-plan (entry successors-alist)
  "Build an explicit BB/edge counter plan from CFG successor relations.

Returns plist:
  :bb-counters   ((BLOCK . BB-ID) ...)
  :edge-counters ((((FROM . TO) . EDGE-ID) ...)
  :total-bb      integer
  :total-edge    integer"
  (let* ((hot-chain (opt-pgo-build-hot-chain entry successors-alist nil))
         (all-blocks (remove-duplicates
                      (append (mapcar #'car successors-alist)
                              (mapcan #'copy-list (mapcar #'cdr successors-alist)))
                      :test #'equal))
         (ordered-blocks (append hot-chain
                                 (remove-if (lambda (b) (member b hot-chain :test #'equal))
                                            all-blocks)))
         (bb-counters nil)
         (edge-counters nil)
         (bb-id 0)
         (edge-id 0))
    (dolist (block ordered-blocks)
      (push (cons block bb-id) bb-counters)
      (incf bb-id))
    (dolist (block ordered-blocks)
      (dolist (succ (copy-list (cdr (assoc block successors-alist :test #'equal))))
        (push (cons (cons block succ) edge-id) edge-counters)
        (incf edge-id)))
    (list :bb-counters (nreverse bb-counters)
          :edge-counters (nreverse edge-counters)
          :total-bb bb-id
          :total-edge edge-id)))

(defun opt-pgo-make-profile-template (counter-plan)
  "Build a zero-initialized profile payload from COUNTER-PLAN." 
  (let ((bb-counts nil)
        (edge-counts nil))
    (dolist (cell (getf counter-plan :bb-counters))
      (push (cons (car cell) 0) bb-counts))
    (dolist (cell (getf counter-plan :edge-counters))
      (push (cons (car cell) 0) edge-counts))
    (list :magic :cl-cc-pgo-v1
          :bb-counts (nreverse bb-counts)
          :branch-counts (nreverse edge-counts)
          :total-bb (getf counter-plan :total-bb)
          :total-edge (getf counter-plan :total-edge))))
