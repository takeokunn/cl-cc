(in-package :cl-cc/vm)

;;; FR-820 Print-circle support.
;;;
;;; Keep the normal path cheap: callers enter this file only through
;;; VM-WRITE-OBJECT-TO-STRING's single CIRCLE conditional.  When CIRCLE is NIL
;;; we delegate to the existing host-backed fast path exactly as before.

(defvar *print-circle* nil
  "When true, detect shared/circular printed structure.")

(defstruct vm-print-circle-state
  (%counts (make-hash-table :test #'eq))
  (%label-objects (make-hash-table :test #'eq))
  (%printed-labels (make-hash-table :test #'eq))
  (%next-label 0 :type fixnum))

(defun %circle-trackable-p (object)
  "Only objects with identity and inspectable children need circle labels."
  (or (consp object)
      (and (vectorp object) (not (stringp object)))
      (hash-table-p object)))

(defun %collect-shared-objects-1 (object state traversed)
  "Phase 1 DFS: count every encounter, but traverse each object once."
  (when (%circle-trackable-p object)
    (incf (gethash object (vm-print-circle-state-%counts state) 0))
    (unless (gethash object traversed)
      (setf (gethash object traversed) t)
      (cond
        ((consp object)
         (%collect-shared-objects-1 (car object) state traversed)
         (%collect-shared-objects-1 (cdr object) state traversed))
        ((and (vectorp object) (not (stringp object)))
         (loop for element across object
               do (%collect-shared-objects-1 element state traversed)))
        ((hash-table-p object)
         (maphash (lambda (key value)
                    (%collect-shared-objects-1 key state traversed)
                    (%collect-shared-objects-1 value state traversed))
                  object)))))
  object)

(defun %collect-shared-objects (object)
  "Return a print-circle state with labels assigned to objects seen 2+ times."
  (let ((state (make-vm-print-circle-state)))
    (%collect-shared-objects-1 object state (make-hash-table :test #'eq))
    (maphash (lambda (object count)
               (when (> count 1)
                 (setf (gethash object (vm-print-circle-state-%label-objects state))
                       (prog1 (vm-print-circle-state-%next-label state)
                         (incf (vm-print-circle-state-%next-label state))))))
             (vm-print-circle-state-%counts state))
    state))

(defun %write-circle-atom (object stream escape)
  (let ((cl:*print-circle* nil)
        (cl:*print-escape* escape))
    (write object :stream stream)))

(defun %write-circle-object (object stream state escape)
  "Phase 2 printer: first labeled occurrence emits #n=, later ones emit #n#."
  (let ((label (and (%circle-trackable-p object)
                    (gethash object (vm-print-circle-state-%label-objects state)))))
    (when label
      (if (gethash object (vm-print-circle-state-%printed-labels state))
          (progn
            (format stream "#~D#" label)
            (return-from %write-circle-object object))
          (progn
            (setf (gethash object (vm-print-circle-state-%printed-labels state)) t)
            (format stream "#~D=" label))))
    (cond
      ((consp object)
       (write-char #\( stream)
       (loop for tail = object then (cdr tail)
             for firstp = t then nil
             while (consp tail)
             do (unless firstp (write-char #\Space stream))
                (%write-circle-object (car tail) stream state escape)
                (let ((next (cdr tail)))
                  (when (and (consp next)
                             (gethash next (vm-print-circle-state-%label-objects state))
                             (gethash next (vm-print-circle-state-%printed-labels state)))
                    (write-string " . " stream)
                    (%write-circle-object next stream state escape)
                    (return)))
             finally
                (when tail
                  (write-string " . " stream)
                  (%write-circle-object tail stream state escape)))
       (write-char #\) stream))
      ((and (vectorp object) (not (stringp object)))
       (write-string "#(" stream)
       (loop for index from 0 below (length object)
             do (when (plusp index) (write-char #\Space stream))
                (%write-circle-object (aref object index) stream state escape))
       (write-char #\) stream))
      ((hash-table-p object)
       ;; Hash tables are not readably printable in this runtime, but their
       ;; keys/values still participate in circle detection and references.
       (write-string "#<HASH-TABLE" stream)
       (maphash (lambda (key value)
                  (write-char #\Space stream)
                  (%write-circle-object key stream state escape)
                  (write-string " => " stream)
                  (%write-circle-object value stream state escape))
                object)
       (write-char #\> stream))
      (t
       (%write-circle-atom object stream escape))))
  object)

(defun %print-with-circle (object stream &key (escape t))
  (let ((state (%collect-shared-objects object)))
    (%write-circle-object object stream state escape))
  object)

(defun vm-write-object-to-string (object &key (escape t) (circle *print-circle*))
  "Return OBJECT's printed representation.
Uses the host fast path when CIRCLE is NIL; the full circle-detection path otherwise."
  (if (not circle)
      (let ((cl:*print-escape* escape)
            (cl:*print-case* *print-case*)
            (cl:*print-base* *print-base*)
            (cl:*print-radix* *print-radix*)
            (cl:*print-gensym* *print-gensym*)
            (cl:*print-array* *print-array*)
            (cl:*print-lines* *print-lines*)
            (cl:*print-right-margin* *print-right-margin*)
            (cl:*print-level* *print-level*)
            (cl:*print-length* *print-length*)
            (cl:*print-pretty* *print-pretty*)
            (cl:*print-readably* *print-readably*))
        (write-to-string object))
      (with-output-to-string (stream)
        (%print-with-circle object stream :escape escape))))
