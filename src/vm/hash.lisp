(in-package :cl-cc)

;;; VM Hash Table Operations
;;;
;;; This file extends the VM with hash table operations for key-value storage.
;;;

;;; VM Hash Table Heap Object

(defclass vm-hash-table-object ()
  ((table :initarg :table :reader vm-hash-table-internal
          :documentation "The underlying Common Lisp hash table"))
  (:documentation "Represents a hash table in the VM heap."))

;;; Hash Table Instruction Classes

(define-vm-instruction vm-make-hash-table (vm-instruction)
  "Create a new hash table. Default test is EQL."
  (dst nil :reader vm-dst)
  (test nil :reader vm-hash-test))

(defmethod instruction->sexp ((inst vm-make-hash-table))
  (if (vm-hash-test inst)
      (list :make-hash-table (vm-dst inst) (vm-hash-test inst))
      (list :make-hash-table (vm-dst inst))))

(setf (gethash :make-hash-table *instruction-constructors*)
      (lambda (sexp)
        (make-vm-make-hash-table :dst (second sexp)
                                 :test (third sexp))))

(define-vm-instruction vm-gethash (vm-instruction)
  "Look up KEY in TABLE. Returns (values VALUE FOUND-P)."
  (dst nil :reader vm-dst)
  (found-dst nil :reader vm-found-dst)
  (key nil :reader vm-hash-key)
  (table nil :reader vm-hash-table-reg)
  (default nil :reader vm-hash-default))

(defmethod instruction->sexp ((inst vm-gethash))
  (let ((result (list :gethash (vm-dst inst)
                      (vm-hash-key inst)
                      (vm-hash-table-reg inst))))
    (when (vm-found-dst inst)
      (setf result (append result (list :found-dst (vm-found-dst inst)))))
    (when (vm-hash-default inst)
      (setf result (append result (list :default (vm-hash-default inst)))))
    result))

(setf (gethash :gethash *instruction-constructors*)
      (lambda (sexp)
        (make-vm-gethash :dst (second sexp)
                         :key (third sexp)
                         :table (fourth sexp)
                         :found-dst (getf (nthcdr 4 sexp) :found-dst)
                         :default (getf (nthcdr 4 sexp) :default))))

(define-vm-instruction vm-sethash (vm-instruction)
  "Store VALUE under KEY in TABLE. Equivalent to (setf (gethash key table) value)."
  (key nil :reader vm-hash-key)
  (value nil :reader vm-hash-value)
  (table nil :reader vm-hash-table-reg)
  (:sexp-tag :sethash)
  (:sexp-slots key value table))

(define-vm-instruction vm-remhash (vm-instruction)
  "Remove KEY from TABLE. Returns 1 if key existed, 0 otherwise."
  (key nil :reader vm-hash-key)
  (table nil :reader vm-hash-table-reg)
  (:sexp-tag :remhash)
  (:sexp-slots key table))

(define-vm-instruction vm-clrhash (vm-instruction)
  "Clear all entries from TABLE."
  (table nil :reader vm-hash-table-reg)
  (:sexp-tag :clrhash)
  (:sexp-slots table))

(define-vm-instruction vm-hash-table-count (vm-instruction)
  "Return the number of entries in TABLE."
  (dst nil :reader vm-dst)
  (table nil :reader vm-hash-table-reg)
  (:sexp-tag :hash-table-count)
  (:sexp-slots dst table))

(define-vm-instruction vm-hash-table-p (vm-instruction)
  "Test if SRC is a hash table. Returns 1 if true, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :hash-table-p)
  (:sexp-slots dst src))

(define-vm-instruction vm-hash-table-keys (vm-instruction)
  "Return a list of all keys in TABLE."
  (dst nil :reader vm-dst)
  (table nil :reader vm-hash-table-reg)
  (:sexp-tag :hash-table-keys)
  (:sexp-slots dst table))

(define-vm-instruction vm-hash-table-values (vm-instruction)
  "Return a list of all values in TABLE."
  (dst nil :reader vm-dst)
  (table nil :reader vm-hash-table-reg)
  (:sexp-tag :hash-table-values)
  (:sexp-slots dst table))

(define-vm-instruction vm-hash-table-test (vm-instruction)
  "Return the test function symbol (eq, eql, equal, equalp) of TABLE."
  (dst nil :reader vm-dst)
  (table nil :reader vm-hash-table-reg)
  (:sexp-tag :hash-table-test)
  (:sexp-slots dst table))

(define-vm-instruction vm-hash-table-size (vm-instruction)
  "Return the current size (bucket count) of TABLE."
  (dst nil :reader vm-dst)
  (table nil :reader vm-hash-table-reg)
  (:sexp-tag :hash-table-size)
  (:sexp-slots dst table))

(define-vm-instruction vm-hash-table-rehash-size (vm-instruction)
  "Return the rehash-size of TABLE."
  (dst nil :reader vm-dst)
  (table nil :reader vm-hash-table-reg)
  (:sexp-tag :hash-table-rehash-size)
  (:sexp-slots dst table))

(define-vm-instruction vm-hash-table-rehash-threshold (vm-instruction)
  "Return the rehash-threshold of TABLE."
  (dst nil :reader vm-dst)
  (table nil :reader vm-hash-table-reg)
  (:sexp-tag :hash-table-rehash-threshold)
  (:sexp-slots dst table))

;;; Helper Functions

(defun resolve-hash-test (test-symbol)
  "Convert a test symbol to the actual comparison function."
  (case test-symbol
    ((eq 'eq) #'eq)
    ((eql 'eql nil) #'eql)
    ((equal 'equal) #'equal)
    ((equalp 'equalp) #'equalp)
    (otherwise (error "Unknown hash table test: ~S" test-symbol))))

(defun vm-hash-table-get-internal (table-obj)
  "Get the internal hash table from a VM hash table object."
  (check-type table-obj vm-hash-table-object)
  (vm-hash-table-internal table-obj))

;;; Instruction Execution - Hash Table Operations

(defmethod execute-instruction ((inst vm-make-hash-table) state pc labels)
  (declare (ignore labels))
  (let* ((test-sym (when (vm-hash-test inst)
                     (vm-reg-get state (vm-hash-test inst))))
         (test-fn (resolve-hash-test test-sym))
         (table (make-hash-table :test test-fn))
         (hash-obj (make-instance 'vm-hash-table-object :table table)))
    (vm-reg-set state (vm-dst inst) hash-obj)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-gethash) state pc labels)
  (declare (ignore labels))
  (let* ((key (vm-reg-get state (vm-hash-key inst)))
         (table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal table-obj))
         (default-val (when (vm-hash-default inst)
                        (vm-reg-get state (vm-hash-default inst)))))
    (multiple-value-bind (value found-p)
        (if default-val
            (gethash key table default-val)
            (gethash key table))
      (vm-reg-set state (vm-dst inst) value)
      (when (vm-found-dst inst)
        (vm-reg-set state (vm-found-dst inst) (if found-p 1 0)))
      ;; Set values-list so multiple-value-bind can pick up (value found-p)
      (setf (vm-values-list state) (list value (if found-p 1 0)))
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-sethash) state pc labels)
  (declare (ignore labels))
  (let* ((key (vm-reg-get state (vm-hash-key inst)))
         (value (vm-reg-get state (vm-hash-value inst)))
         (table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal table-obj)))
    (setf (gethash key table) value)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-remhash) state pc labels)
  (declare (ignore labels))
  (let* ((key (vm-reg-get state (vm-hash-key inst)))
         (table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal table-obj)))
    (remhash key table)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-clrhash) state pc labels)
  (declare (ignore labels))
  (let* ((table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal table-obj)))
    (clrhash table)
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
         (result (if (typep value 'vm-hash-table-object) 1 0)))
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
         ;; hash-table-test returns a symbol in most implementations
         (test-sym (cond ((eq test-val 'eq) 'eq)
                         ((eq test-val 'eql) 'eql)
                         ((eq test-val 'equal) 'equal)
                         ((eq test-val 'equalp) 'equalp)
                         ;; Some implementations may return functions
                         ((and (functionp test-val) (eq test-val #'eq)) 'eq)
                         ((and (functionp test-val) (eq test-val #'eql)) 'eql)
                         ((and (functionp test-val) (eq test-val #'equal)) 'equal)
                         ((and (functionp test-val) (eq test-val #'equalp)) 'equalp)
                         (t 'eql))))
    (vm-reg-set state (vm-dst inst) test-sym)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-hash-table-size) state pc labels)
  (declare (ignore labels))
  (let* ((table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal table-obj)))
    (vm-reg-set state (vm-dst inst) (hash-table-size table))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-hash-table-rehash-size) state pc labels)
  (declare (ignore labels))
  (let* ((table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal table-obj)))
    (vm-reg-set state (vm-dst inst) (hash-table-rehash-size table))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-hash-table-rehash-threshold) state pc labels)
  (declare (ignore labels))
  (let* ((table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal table-obj)))
    (vm-reg-set state (vm-dst inst) (hash-table-rehash-threshold table))
    (values (1+ pc) nil nil)))
