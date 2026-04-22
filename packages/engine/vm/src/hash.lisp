(in-package :cl-cc/vm)

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

(defparameter *hash-test-function-alist*
  '((eq     . eq)
    (eql    . eql)
    (equal  . equal)
    (equalp . equalp))
  "(designator-symbol . comparison-function) pairs for VM hash-table tests.")

(defun hash-test-symbol (test-val)
  "Convert TEST-VAL (symbol or function) back to the canonical hash-test symbol."
  (let ((entry (or (assoc test-val *hash-test-function-alist* :test #'eq)
                   (rassoc test-val *hash-test-function-alist*
                           :test (lambda (fn fn-name)
                                   (eq fn (symbol-function fn-name)))))))
    (if entry
        (car entry)
        'eql)))

(defmacro define-vm-hash-property-executors ()
  `(progn
     ,@(loop for (inst-type . accessor) in '((vm-hash-table-size             . hash-table-size)
                                             (vm-hash-table-rehash-size      . hash-table-rehash-size)
                                             (vm-hash-table-rehash-threshold . hash-table-rehash-threshold))
             collect
             `(defmethod execute-instruction ((inst ,inst-type) state pc labels)
                (declare (ignore labels))
                (let* ((table-obj (vm-reg-get state (vm-hash-table-reg inst)))
                       (table (vm-hash-table-get-internal table-obj)))
                  (vm-reg-set state (vm-dst inst) (,accessor table))
                  (values (1+ pc) nil nil))))))

(defun resolve-hash-test (test-symbol)
  "Convert a test symbol to the actual comparison function."
  (or (and (null test-symbol) #'eql)
      (let ((entry (assoc test-symbol *hash-test-function-alist* :test #'eq)))
        (and entry (symbol-function (cdr entry))))
      (error "Unknown hash table test: ~S" test-symbol)))

(defun vm-hash-table-get-internal (table-obj)
  "Get the internal hash table from a VM hash table object or native CL hash table.
Class registry entries and GF dispatch tables are native hash tables, so we
must handle both representations transparently."
  (etypecase table-obj
    (vm-hash-table-object (vm-hash-table-internal table-obj))
    (hash-table table-obj)))

;;; Instruction Execution - Hash Table Operations
;;; are in hash-execute.lisp (loaded after this file).
