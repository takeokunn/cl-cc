(in-package :cl-cc)

;;; ----------------------------------------------------------------------------
;;; VM Hash Table Operations
;;; ----------------------------------------------------------------------------
;;;
;;; This file extends the VM with hash table operations for key-value storage.
;;;

;;; ----------------------------------------------------------------------------
;;; VM Hash Table Heap Object
;;; ----------------------------------------------------------------------------

(defclass vm-hash-table-object ()
  ((table :initarg :table :reader vm-hash-table-internal
          :documentation "The underlying Common Lisp hash table"))
  (:documentation "Represents a hash table in the VM heap."))

;;; ----------------------------------------------------------------------------
;;; Hash Table Instruction Classes
;;; ----------------------------------------------------------------------------

(defclass vm-make-hash-table (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the new hash table")
   (test :initarg :test :reader vm-hash-test :initform nil
         :documentation "Register containing test function symbol (eq, eql, equal, equalp)"))
  (:documentation "Create a new hash table. Default test is EQL."))

(defclass vm-gethash (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the found value")
  (found-dst :initarg :found-dst :reader vm-found-dst :initform nil
             :documentation "Optional register to store found-p boolean (1 if found, 0 otherwise)")
  (key :initarg :key :reader vm-hash-key
       :documentation "Register containing the key to look up")
  (table :initarg :table :reader vm-hash-table-reg
         :documentation "Register containing the hash table")
  (default :initarg :default :reader vm-hash-default :initform nil
           :documentation "Optional register containing default value if key not found"))
  (:documentation "Look up KEY in TABLE. Returns (values VALUE FOUND-P)."))

(defclass vm-sethash (vm-instruction)
  ((key :initarg :key :reader vm-hash-key
        :documentation "Register containing the key")
   (value :initarg :value :reader vm-hash-value
          :documentation "Register containing the value to store")
   (table :initarg :table :reader vm-hash-table-reg
          :documentation "Register containing the hash table"))
  (:documentation "Store VALUE under KEY in TABLE. Equivalent to (setf (gethash key table) value)."))

(defclass vm-remhash (vm-instruction)
  ((key :initarg :key :reader vm-hash-key
        :documentation "Register containing the key to remove")
   (table :initarg :table :reader vm-hash-table-reg
          :documentation "Register containing the hash table"))
  (:documentation "Remove KEY from TABLE. Returns 1 if key existed, 0 otherwise."))

(defclass vm-clrhash (vm-instruction)
  ((table :initarg :table :reader vm-hash-table-reg
          :documentation "Register containing the hash table"))
  (:documentation "Clear all entries from TABLE."))

(defclass vm-hash-table-count (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the entry count")
   (table :initarg :table :reader vm-hash-table-reg
          :documentation "Register containing the hash table"))
  (:documentation "Return the number of entries in TABLE."))

(defclass vm-hash-table-p (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the result (1 if hash table, 0 otherwise)")
   (src :initarg :src :reader vm-src
        :documentation "Register containing the value to test"))
  (:documentation "Test if SRC is a hash table. Returns 1 if true, 0 otherwise."))

(defclass vm-maphash (vm-instruction)
  ((fn :initarg :fn :reader vm-hash-fn
       :documentation "Register containing the function to apply (closure)")
   (table :initarg :table :reader vm-hash-table-reg
          :documentation "Register containing the hash table"))
  (:documentation "Apply FN to each (KEY . VALUE) pair in TABLE. FN receives key and value as arguments."))

(defclass vm-hash-table-keys (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the list of keys")
   (table :initarg :table :reader vm-hash-table-reg
          :documentation "Register containing the hash table"))
  (:documentation "Return a list of all keys in TABLE."))

(defclass vm-hash-table-values (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the list of values")
   (table :initarg :table :reader vm-hash-table-reg
          :documentation "Register containing the hash table"))
  (:documentation "Return a list of all values in TABLE."))

(defclass vm-hash-table-test (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the test function symbol")
   (table :initarg :table :reader vm-hash-table-reg
          :documentation "Register containing the hash table"))
  (:documentation "Return the test function symbol (eq, eql, equal, equalp) of TABLE."))

;;; ----------------------------------------------------------------------------
;;; Instruction -> S-expression Conversion
;;; ----------------------------------------------------------------------------

(defmethod instruction->sexp ((inst vm-make-hash-table))
  (if (vm-hash-test inst)
      (list :make-hash-table (vm-dst inst) (vm-hash-test inst))
      (list :make-hash-table (vm-dst inst))))

(defmethod instruction->sexp ((inst vm-gethash))
  (let ((result (list :gethash (vm-dst inst)
                      (vm-hash-key inst)
                      (vm-hash-table-reg inst))))
    (when (vm-found-dst inst)
      (setf result (append result (list :found-dst (vm-found-dst inst)))))
    (when (vm-hash-default inst)
      (setf result (append result (list :default (vm-hash-default inst)))))
    result))

(defmethod instruction->sexp ((inst vm-sethash))
  (list :sethash (vm-hash-key inst) (vm-hash-value inst) (vm-hash-table-reg inst)))

(defmethod instruction->sexp ((inst vm-remhash))
  (list :remhash (vm-hash-key inst) (vm-hash-table-reg inst)))

(defmethod instruction->sexp ((inst vm-clrhash))
  (list :clrhash (vm-hash-table-reg inst)))

(defmethod instruction->sexp ((inst vm-hash-table-count))
  (list :hash-table-count (vm-dst inst) (vm-hash-table-reg inst)))

(defmethod instruction->sexp ((inst vm-hash-table-p))
  (list :hash-table-p (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-maphash))
  (list :maphash (vm-hash-fn inst) (vm-hash-table-reg inst)))

(defmethod instruction->sexp ((inst vm-hash-table-keys))
  (list :hash-table-keys (vm-dst inst) (vm-hash-table-reg inst)))

(defmethod instruction->sexp ((inst vm-hash-table-values))
  (list :hash-table-values (vm-dst inst) (vm-hash-table-reg inst)))

(defmethod instruction->sexp ((inst vm-hash-table-test))
  (list :hash-table-test (vm-dst inst) (vm-hash-table-reg inst)))

;;; ----------------------------------------------------------------------------
;;; S-expression -> Instruction Conversion
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Helper Functions
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - Hash Table Operations
;;; ----------------------------------------------------------------------------

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

(defmethod execute-instruction ((inst vm-maphash) state pc labels)
  "Execute maphash — placeholder. Use hash-table-keys + dolist + funcall instead."
  (declare (ignore labels))
  (let* ((table-obj (vm-reg-get state (vm-hash-table-reg inst)))
         (table (vm-hash-table-get-internal table-obj)))
    (declare (ignore table))
    ;; maphash with closures requires re-entrant VM execution.
    ;; Use (dolist (k (hash-table-keys ht)) (funcall fn k (gethash k ht))) instead.
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
