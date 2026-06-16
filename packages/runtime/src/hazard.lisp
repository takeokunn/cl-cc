;;;; Hazard Pointers (FR-321)
(in-package :cl-cc/runtime)

(defvar *hazard-thread-arrays* (make-hash-table))
(defvar *hazard-slots-per-thread* 4)
(defvar *hazard-retired* (make-hash-table))
(defvar *hazard-retire-threshold* 32)
(defvar *hazard-free-function* #'identity)

(defun %rt-hp-thread-array (thread)
  (gethash thread *hazard-thread-arrays*))

(defun %rt-hp-slot-count (n)
  (max 1 n))

(defun %rt-hp-slot-index (array idx)
  (min idx (1- (length array))))

(defun %rt-hp-protected-p (object protected)
  (gethash object protected))

(defun %rt-hp-record-protected-array (array protected)
  (loop for object across array
        when object
          do (setf (gethash object protected) t))
  protected)

(defun %rt-hp-protected-table ()
  (let ((protected (make-hash-table :test #'eq)))
    (loop for array being the hash-values of *hazard-thread-arrays*
          do (%rt-hp-record-protected-array array protected))
    protected))

(defun %rt-hp-table-keys (table)
  (loop for key being the hash-keys of table
        collect key))

(defun %rt-hp-reclaim-retired-list (retired protected free-fn)
  (loop with remaining = nil
        with freed = 0
        for object in retired
        if (%rt-hp-protected-p object protected)
          do (push object remaining)
        else
          do (funcall free-fn object)
             (incf freed)
        finally (return (values (nreverse remaining) freed))))

(defun rt-hp-register-thread (&optional (n *hazard-slots-per-thread*)
                                        (thread sb-thread:*current-thread*))
  "Register THREAD with N hazard pointer slots."
  (setf (gethash thread *hazard-thread-arrays*)
        (make-array (%rt-hp-slot-count n) :initial-element nil)))

(defun rt-hp-unregister-thread (&optional (thread sb-thread:*current-thread*))
  "Remove THREAD from the hazard pointer registry."
  (remhash thread *hazard-thread-arrays*))

(defun rt-hp-protect (idx obj &optional (thread sb-thread:*current-thread*))
  "Store OBJ in THREAD's hazard slot IDX and return IDX."
  (let ((array (%rt-hp-thread-array thread)))
    (when array
      (setf (svref array (%rt-hp-slot-index array idx)) obj)))
  idx)

(defun rt-hp-clear (idx &optional (thread sb-thread:*current-thread*))
  "Clear hazard slot IDX for THREAD."
  (let ((array (%rt-hp-thread-array thread)))
    (when array
      (setf (svref array (%rt-hp-slot-index array idx)) nil))))

(defun rt-hp-clear-all (&optional (thread sb-thread:*current-thread*))
  "Clear every hazard slot for THREAD."
  (let ((array (%rt-hp-thread-array thread)))
    (when array
      (fill array nil))))

(defun rt-hp-scan (objects)
  "Return the objects that are not currently protected."
  (let ((protected (%rt-hp-protected-table)))
    (loop for object in objects
          unless (%rt-hp-protected-p object protected)
            collect object)))

(defun rt-hp-retire (retired free-fn threshold)
  "Reclaim unprotected objects once RETIRED reaches THRESHOLD."
  (if (< (length retired) threshold)
      retired
      (multiple-value-bind (remaining freed)
          (%rt-hp-reclaim-retired-list retired (%rt-hp-protected-table) free-fn)
        (declare (ignore freed))
        remaining)))

(defun rt-hp-retire-object (obj &key (thread sb-thread:*current-thread*)
                                       (free-fn *hazard-free-function*)
                                       (threshold *hazard-retire-threshold*))
  (let ((list (cons obj (gethash thread *hazard-retired*))))
    (setf (gethash thread *hazard-retired*)
          (if (>= (length list) threshold)
                (rt-hp-retire list free-fn threshold)
                list))))

(defun rt-hp-all-protected ()
  "Return a list of all objects currently in hazard slots."
  (%rt-hp-table-keys (%rt-hp-protected-table)))

(defun rt-hp-reclaim (&key (thread sb-thread:*current-thread*)
                           (free-fn *hazard-free-function*))
  "Reclaim THREAD's unprotected retired objects and return the reclaimed count."
  (multiple-value-bind (remaining freed)
      (%rt-hp-reclaim-retired-list (gethash thread *hazard-retired*)
                                   (%rt-hp-protected-table)
                                   free-fn)
    (setf (gethash thread *hazard-retired*) remaining)
    freed))

(defun rt-hp-set-threshold (n)
  (setf *hazard-retire-threshold* (max 1 n)))

(defun rt-hp-init (&optional (free-fn #'identity))
  (setf *hazard-free-function* free-fn)
  (clrhash *hazard-thread-arrays*)
  (clrhash *hazard-retired*)
  t)
