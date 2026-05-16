;;;; hash-execute.lisp - VM Hash Table Instruction Execution Methods
;;;;
;;;; Depends on hash.lisp being loaded first (instruction struct definitions,
;;;; helper functions, vm-hash-table-get-internal, resolve-hash-test).

(in-package :cl-cc/vm)

;;; Instruction Execution - Hash Table Operations

(defmethod execute-instruction ((inst vm-make-hash-table) state pc labels)
  (declare (ignore labels))
  (let* ((test-sym (when (vm-hash-test inst)
                      (vm-reg-get state (vm-hash-test inst))))
          (test-designator (resolve-hash-test test-sym))
          (table (make-hash-table :test test-designator))
         (hash-obj (make-instance 'vm-hash-table-object
                                  :table table
                                  :lock #+sb-thread (sb-thread:make-mutex :name "vm-hash-table-lock")
                                        #-sb-thread nil)))
    (vm-reg-set state (vm-dst inst) hash-obj)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-gethash) state pc labels)
  (declare (ignore labels))
  (let* ((key (vm-reg-get state (vm-hash-key inst)))
         (table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal table-obj))
         (default-val (when (vm-hash-default inst)
                         (vm-reg-get state (vm-hash-default inst)))))
    (if (typep table-obj 'vm-hash-table-object)
        (vm-hash-with-lock-fallback
         table-obj
         (lambda ()
           (multiple-value-bind (value found-p)
               (if default-val
                   (gethash key table default-val)
                   (gethash key table))
             (vm-reg-set state (vm-dst inst) value)
             (when (vm-found-dst inst)
               (vm-reg-set state (vm-found-dst inst) (if found-p 1 0)))
             (setf (vm-values-list state) (list value (if found-p 1 0))))))
        (multiple-value-bind (value found-p)
            (if default-val
                (gethash key table default-val)
                (gethash key table))
          (vm-reg-set state (vm-dst inst) value)
          (when (vm-found-dst inst)
            (vm-reg-set state (vm-found-dst inst) (if found-p 1 0)))
          (setf (vm-values-list state) (list value (if found-p 1 0)))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-sethash) state pc labels)
  (declare (ignore labels))
  (let* ((key (vm-reg-get state (vm-hash-key inst)))
         (value (vm-reg-get state (vm-hash-value inst)))
         (table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal (vm-hash-ensure-writable table-obj))))
    (if (typep table-obj 'vm-hash-table-object)
        (vm-hash-with-lock-fallback table-obj
                                    (lambda ()
                                      (setf (gethash key table) value)))
        (setf (gethash key table) value))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-remhash) state pc labels)
  (declare (ignore labels))
  (let* ((key (vm-reg-get state (vm-hash-key inst)))
         (table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal (vm-hash-ensure-writable table-obj))))
    (if (typep table-obj 'vm-hash-table-object)
        (vm-hash-with-lock-fallback table-obj
                                    (lambda ()
                                      (remhash key table)))
        (remhash key table))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-clrhash) state pc labels)
  (declare (ignore labels))
  (let* ((table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal (vm-hash-ensure-writable table-obj))))
    (if (typep table-obj 'vm-hash-table-object)
        (vm-hash-with-lock-fallback table-obj
                                    (lambda ()
                                      (clrhash table)))
        (clrhash table))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-hash-table-count) state pc labels)
  (declare (ignore labels))
  (let* ((table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal table-obj))
         (count (hash-table-count table)))
    (vm-reg-set state (vm-dst inst) count)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-hash-table-p) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-src inst)))
         (result (if (or (typep value 'vm-hash-table-object)
                         (hash-table-p value))
                     1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-hash-table-keys) state pc labels)
  (declare (ignore labels))
  (let* ((table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal table-obj))
         (keys (loop for key being the hash-keys of table collect key)))
    (vm-reg-set state (vm-dst inst) keys)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-hash-table-values) state pc labels)
  (declare (ignore labels))
  (let* ((table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal table-obj))
         (values (loop for val being the hash-values of table collect val)))
    (vm-reg-set state (vm-dst inst) values)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-hash-table-test) state pc labels)
  (declare (ignore labels))
  (let* ((table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal table-obj))
         (test-val (hash-table-test table))
         (test-sym (hash-test-symbol test-val)))
    (vm-reg-set state (vm-dst inst) test-sym)
    (values (1+ pc) nil nil)))

(define-vm-hash-property-executors)
