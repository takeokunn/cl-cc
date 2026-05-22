;;;; pipeline-bolt.lisp — FR-660 BOLT-style post-link layout optimizer

(in-package :cl-cc/pipeline)

(defstruct (bolt-profile (:constructor make-bolt-profile))
  "Profile facts consumed by the BOLT-style native layout pass."
  (function-hotness nil)
  (raw-samples nil)
  (perf-map nil))

(defun %bolt-profile-function-hotness-table (profile)
  "Return an EQUAL table mapping function names to sample counts."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry (and profile (bolt-profile-function-hotness profile)) table)
      (setf (gethash (princ-to-string (car entry)) table) (cdr entry)))))

(defun read-bolt-profile (&key profile-path perf-map-path)
  "Read BOLT profile data using FR-553 perf-map/AutoFDO text ingestion.

PROFILE-PATH may be NIL; in that case the current process perf map is still read
so the layout pass can preserve symbol information while falling back to static
hot/cold heuristics."
  (if profile-path
      (let ((profile (read-autofdo-profile profile-path :perf-map-path perf-map-path)))
        (make-bolt-profile
         :function-hotness (autofdo-profile-function-hotness profile)
         :raw-samples (autofdo-profile-raw-samples profile)
         :perf-map (autofdo-profile-perf-map profile)))
      (make-bolt-profile :perf-map (read-perf-map (or perf-map-path (perf-map-path))))))

(defun %bolt-function-hotness (block hotness-table)
  "Return profile hotness for BLOCK, or zero when absent."
  (let ((name (pipeline-function-block-name block)))
    (if name
        (gethash (princ-to-string name) hotness-table 0)
        0)))

(defun %bolt-layout-function-body (instructions)
  "Reorder basic blocks inside one function body with hot paths contiguous.

The optimizer CFG layout already moves cold/error paths to the tail and patches
broken fall-through edges.  Any CFG construction failure leaves the body intact,
which keeps BOLT infrastructure safe for all current native backends."
  (handler-case
      (let ((cfg (cl-cc/optimize:cfg-build instructions)))
        (when (cl-cc/optimize:cfg-entry cfg)
          (cl-cc/optimize:cfg-compute-dominators cfg)
          (cl-cc/optimize:cfg-compute-loop-depths cfg))
        (cl-cc/optimize:cfg-flatten-hot-cold cfg))
    (error () instructions)))

(defun %bolt-rewrite-function-block (block)
  "Return BLOCK with hot/cold basic-block layout applied to its body."
  (make-pipeline-function-block
   :name (pipeline-function-block-name block)
   :label (pipeline-function-block-label block)
   :original-index (pipeline-function-block-original-index block)
   :instructions (%bolt-layout-function-body (pipeline-function-block-instructions block))))

(defun %bolt-order-function-blocks (blocks profile)
  "Order function BLOCKS so hot functions are contiguous and cold functions trail.

Profile hotness is primary.  The existing direct-call order is used as the stable
tie-breaker so non-profiled builds keep FR-186 behavior."
  (let* ((hotness (%bolt-profile-function-hotness-table profile))
         (base-order (%pipeline-order-function-blocks blocks))
         (rank (make-hash-table :test #'equal)))
    (loop for block in base-order
          for i from 0
          do (setf (gethash (or (pipeline-function-block-label block)
                                (princ-to-string (pipeline-function-block-name block)))
                            rank)
                   i))
    (stable-sort (mapcar #'%bolt-rewrite-function-block base-order)
                 (lambda (a b)
                   (let ((ha (%bolt-function-hotness a hotness))
                         (hb (%bolt-function-hotness b hotness)))
                     (if (= ha hb)
                         (< (gethash (or (pipeline-function-block-label a)
                                         (princ-to-string (pipeline-function-block-name a)))
                                     rank most-positive-fixnum)
                            (gethash (or (pipeline-function-block-label b)
                                         (princ-to-string (pipeline-function-block-name b)))
                                     rank most-positive-fixnum))
                         (> ha hb)))))))

(defun pipeline-bolt-optimize-program (program &key profile-path perf-map-path profile)
  "Apply FR-660 BOLT-style profile-guided layout optimization to PROGRAM.

The pass consumes perf/AutoFDO text samples mapped through FR-553 perf-map data,
orders hot functions first, applies hot/cold basic-block layout inside each
function, and appends cold function bodies at the end.  Non-VM inputs and
programs without extractable function bodies are returned unchanged."
  (unless (typep program 'cl-cc/vm:vm-program)
    (return-from pipeline-bolt-optimize-program program))
  (let ((effective-profile (or profile
                               (read-bolt-profile :profile-path profile-path
                                                  :perf-map-path perf-map-path))))
    (multiple-value-bind (skeleton blocks)
        (%pipeline-extract-function-blocks (cl-cc/vm:vm-program-instructions program))
      (if (null blocks)
          program
          (make-vm-program
           :instructions (append skeleton
                                 (mapcan #'copy-list
                                         (mapcar #'pipeline-function-block-instructions
                                                 (%bolt-order-function-blocks blocks effective-profile))))
           :result-register (cl-cc/vm:vm-program-result-register program)
           :leaf-p (cl-cc/vm:vm-program-leaf-p program))))))

(defun maybe-pipeline-bolt-optimize-program (program opts)
  "Run BOLT only when native pipeline OPTS contains :BOLT."
  (if (getf opts :bolt)
      (pipeline-bolt-optimize-program program
                                      :profile-path (getf opts :bolt-profile)
                                      :perf-map-path (and (getf opts :perf-map)
                                                          (perf-map-path)))
      program))
