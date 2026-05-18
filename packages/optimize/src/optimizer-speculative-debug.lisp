(in-package :cl-cc/optimize)

(defstruct (opt-debug-loc (:conc-name opt-debug-loc-))
  "Source-level location record for debug info planning."
  file
  (line 0 :type integer)
  (column 0 :type integer)
  (symbol nil :type t))

(defun opt-build-dwarf-line-row (address debug-loc)
  "Build a minimal DWARF-like line row plist from ADDRESS and DEBUG-LOC."
  (list :address address
        :file (opt-debug-loc-file debug-loc)
        :line (opt-debug-loc-line debug-loc)
        :column (opt-debug-loc-column debug-loc)))

(defun opt-build-wasm-source-map-entry (offset debug-loc)
  "Build a source-map entry plist for wasm OFFSET and DEBUG-LOC."
  (list :offset offset
        :source (opt-debug-loc-file debug-loc)
        :line (opt-debug-loc-line debug-loc)
        :column (opt-debug-loc-column debug-loc)))

(defun opt-build-wasm-source-map-v3 (entries &key file)
  "Build a backend-neutral Source Map v3 payload plist from ENTRIES.

This helper normalizes entry ordering and source lists but does not write files
or emit VLQ-encoded mappings."
  (let* ((sorted (sort (copy-list entries) #'< :key (lambda (entry) (getf entry :offset 0))))
         (sources-seen (make-hash-table :test #'equal))
         (sources nil)
         (mappings nil))
    (dolist (entry sorted)
      (let ((source (getf entry :source)))
        (unless (gethash source sources-seen)
          (setf (gethash source sources-seen) t)
          (push source sources)))
      (push (list :offset (getf entry :offset)
                  :source (getf entry :source)
                  :line (getf entry :line)
                  :column (getf entry :column))
            mappings))
    (list :version 3
          :file file
          :sources (nreverse sources)
          :mappings (nreverse mappings))))

(defun opt-format-diagnostic-reason (pass outcome reason)
  "Format optimization diagnostic reason in Rpass-like style."
  (format nil "~a: ~a (~a)" pass outcome reason))

(defun opt-build-diagnostic-caret-line (line-text column &key (caret #\^))
  "Return a two-line caret diagnostic snippet for LINE-TEXT at 1-based COLUMN."
  (let* ((text (or line-text ""))
         (len (length text))
         (col (max 1 (min (or column 1) (1+ len)))))
    (format nil "~a~%~v@T~c" text (max 0 (1- col)) caret)))

(defun %opt-candidate-score (needle candidate)
  (let* ((n (string-downcase (or needle "")))
         (c (string-downcase (or candidate "")))
         (nlen (length n))
         (clen (length c))
         (shared-prefix (loop for i from 0 below (min nlen clen)
                              while (char= (char n i) (char c i))
                              count t))
         (contains (if (search n c) 0 3)))
    (+ contains
       (abs (- nlen clen))
       (- nlen shared-prefix))))

(defun opt-diagnostic-did-you-mean (unknown candidates &key (limit 3))
  "Return up to LIMIT ranked suggestion strings for UNKNOWN from CANDIDATES."
  (let* ((pool (remove-if-not #'stringp candidates))
         (scored (mapcar (lambda (candidate)
                           (cons candidate
                                 (%opt-candidate-score unknown candidate)))
                         pool))
         (ordered (sort scored
                        (lambda (left right)
                          (or (< (cdr left) (cdr right))
                              (and (= (cdr left) (cdr right))
                                   (string< (car left) (car right))))))))
    (subseq (mapcar #'car ordered) 0 (min limit (length ordered)))))

(defun opt-format-type-trace (steps)
  "Format type-inference rationale STEPS as a human-readable trace string."
  (if (null steps)
      "Type trace: <none>"
      (with-output-to-string (out)
        (format out "Type trace:")
        (loop for step in steps
              for index from 1
              do (format out "~%~d. ~a" index step)))))
