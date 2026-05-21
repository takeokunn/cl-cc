(in-package :cl-cc/vm)

;;; VM List Operations

(defparameter *vm-hash-cons-table*
  #+sbcl (make-hash-table :test #'equal :weakness :value)
  #-sbcl (make-hash-table :test #'equal)
  "Runtime hash-cons table keyed by (car cdr).

On SBCL, values are weak so unreferenced interned cons cells can be reclaimed
by GC. Other implementations fall back to a regular hash table.")

(defun vm-clear-hash-cons-table ()
  "Clear the runtime hash-cons table and return it."
  (clrhash *vm-hash-cons-table*)
  *vm-hash-cons-table*)

(defun %vm-hash-cons-intern-pair (car-value cdr-value)
  "Intern one cons pair in the runtime hash-cons table."
  (let* ((key (list car-value cdr-value))
         (existing (gethash key *vm-hash-cons-table*)))
    (or existing
        (setf (gethash key *vm-hash-cons-table*)
              (cons car-value cdr-value)))))

(defun %vm-hash-cons-canonicalize (value seen)
  "Recursively canonicalize VALUE by hash-consing nested cons trees.

SEEN tracks already-visited cons cells to avoid infinite recursion on cyclic
inputs and to preserve graph sharing while canonicalizing."
  (if (consp value)
      (multiple-value-bind (cached present-p) (gethash value seen)
        (cond
          ;; Active recursion marker => cycle detected on this branch.
          ((and present-p (eq cached :visiting))
           (values value t))
          (present-p
           (values cached nil))
          (t
           (setf (gethash value seen) :visiting)
           (multiple-value-bind (canon-car car-cyclic-p)
               (%vm-hash-cons-canonicalize (car value) seen)
             (multiple-value-bind (canon-cdr cdr-cyclic-p)
                 (%vm-hash-cons-canonicalize (cdr value) seen)
               (if (or car-cyclic-p cdr-cyclic-p)
                   (progn
                     ;; Avoid interning cyclic structures in equal-hash tables.
                     (setf (gethash value seen) value)
                     (values value t))
                   (let ((canon (%vm-hash-cons-intern-pair canon-car canon-cdr)))
                     (setf (gethash value seen) canon)
                     (values canon nil))))))))
      (values value nil)))

(defun vm-hash-cons (car-value cdr-value)
  "Return a shared cons cell for CAR-VALUE/CDR-VALUE.

Nested cons values are recursively canonicalized so structurally equivalent
list/tree shapes can share storage under explicit hash-cons opt-in."
  (let ((seen (make-hash-table :test #'eq)))
    (multiple-value-bind (canon-car car-cyclic-p)
        (%vm-hash-cons-canonicalize car-value seen)
      (multiple-value-bind (canon-cdr cdr-cyclic-p)
          (%vm-hash-cons-canonicalize cdr-value seen)
        (if (or car-cyclic-p cdr-cyclic-p)
            (cons canon-car canon-cdr)
            (%vm-hash-cons-intern-pair canon-car canon-cdr))))))

;;; ─── Extensible Sequence Protocol (FR-274 partial) ───────────────────────

(defgeneric vm-sequence-elt (sequence index)
  (:documentation "Return element at INDEX in SEQUENCE."))

(defgeneric vm-sequence-length (sequence)
  (:documentation "Return the logical length of SEQUENCE."))

(defgeneric vm-make-sequence-like (sequence size &key initial-element)
  (:documentation "Create a new sequence like SEQUENCE with SIZE elements."))

(defgeneric vm-adjust-sequence (sequence size &key initial-element)
  (:documentation "Resize SEQUENCE to SIZE, preserving contents when possible."))

(defmethod vm-sequence-elt ((sequence list) index)
  (elt sequence index))

(defmethod vm-sequence-length ((sequence list))
  (length sequence))

(defmethod vm-make-sequence-like ((sequence list) size &key (initial-element nil))
  (declare (ignore sequence))
  (make-list size :initial-element initial-element))

(defmethod vm-adjust-sequence ((sequence list) size &key (initial-element nil))
  (let ((prefix (subseq sequence 0 (min size (length sequence)))))
    (if (< (length prefix) size)
        (nconc (copy-list prefix)
               (make-list (- size (length prefix)) :initial-element initial-element))
        prefix)))

(defmethod vm-sequence-elt ((sequence vector) index)
  (aref sequence index))

(defmethod vm-sequence-length ((sequence vector))
  (length sequence))

(defmethod vm-make-sequence-like ((sequence vector) size &key (initial-element nil))
  (declare (ignore sequence))
  (make-array size :initial-element initial-element))

(defmethod vm-adjust-sequence ((sequence vector) size &key (initial-element nil))
  (adjust-array sequence size :initial-element initial-element))

;;;
;;; This file extends the VM with list manipulation instructions including
;;; cons cell creation, list accessors, and common list operations.
;;;

;;; Cons Cell Instructions

(define-vm-instruction vm-cons (vm-instruction)
  "Create a cons cell from CAR-SRC and CDR-SRC, store in DST."
  (car-src nil :reader vm-car-reg)
  (cdr-src nil :reader vm-cdr-reg)
  (dst nil :reader vm-dst)
  (:sexp-tag :cons)
  (:sexp-slots dst car-src cdr-src))

(define-vm-instruction vm-hash-cons (vm-instruction)
  "Create or reuse a hash-consed cell from CAR-SRC and CDR-SRC, store in DST."
  (car-src nil :reader vm-car-reg)
  (cdr-src nil :reader vm-cdr-reg)
  (dst nil :reader vm-dst)
  (:sexp-tag :hash-cons)
  (:sexp-slots dst car-src cdr-src))

(define-vm-unary-instruction vm-car :car "Extract the car of the cons cell in SRC, store in DST.")
(define-vm-unary-instruction vm-cdr :cdr "Extract the cdr of the cons cell in SRC, store in DST.")

;;; FR-157: Managed cons allocation
;;;
;;; cl-cc-vm intentionally remains independently loadable, so all calls into the
;;; runtime heap are resolved lazily by name.  When the full cl-cc system is
;;; loaded, VM-CONS allocates a real runtime heap object and returns the runtime's
;;; NaN-boxed cons pointer.  Standalone :cl-cc-vm loads keep the previous host
;;; CONS fallback because the :cl-cc/runtime package is not present there.

(defun %vm-runtime-symbol (name &optional errorp)
  "Return the CL-CC/RUNTIME symbol named NAME, or NIL when runtime is absent."
  (let ((package (find-package "CL-CC/RUNTIME")))
    (multiple-value-bind (symbol status)
        (if package
            (find-symbol name package)
            (values nil nil))
      (cond
        ((and status symbol) symbol)
        (errorp (error "cl-cc/vm: runtime symbol ~A is not available" name))
        (t nil)))))

(defun %vm-runtime-function (name &optional errorp)
  "Return the runtime function named NAME, or NIL when unavailable."
  (let ((symbol (%vm-runtime-symbol name errorp)))
    (cond
      ((and symbol (fboundp symbol)) (symbol-function symbol))
      (errorp (error "cl-cc/vm: runtime function ~A is not available" name))
      (t nil))))

(defun %vm-runtime-value (name &optional errorp)
  "Return the runtime special/constant value named NAME."
  (let ((symbol (%vm-runtime-symbol name errorp)))
    (cond
      ((and symbol (boundp symbol)) (symbol-value symbol))
      (errorp (error "cl-cc/vm: runtime value ~A is not available" name))
      (t nil))))

(defun %vm-managed-cons-runtime-available-p ()
  "Return true when all runtime entry points needed for managed conses exist."
  (and (%vm-runtime-function "MAKE-RT-HEAP")
       (%vm-runtime-function "RT-GC-ALLOC")
       (%vm-runtime-function "MAKE-HEADER")
       (%vm-runtime-function "RT-HEAP-SET")
       (%vm-runtime-function "RT-HEAP-REF")
       (%vm-runtime-function "ENCODE-POINTER")
       (%vm-runtime-function "DECODE-POINTER")
       (%vm-runtime-function "VAL-CONS-P")
       (%vm-runtime-value "+RT-TAG-CONS+")
       (%vm-runtime-value "+TAG-CONS+")))

(defun %vm-managed-cons-heap-table (state)
  "Return STATE's VM metadata heap table, or NIL for states without one."
  (when (typep state 'vm-state)
    (vm-state-heap state)))

(defun %vm-managed-cons-heap (state)
  "Return STATE's lazily-created runtime heap for managed VM cons cells."
  (let ((table (%vm-managed-cons-heap-table state)))
    (when (and table (%vm-managed-cons-runtime-available-p))
      (or (gethash :managed-rt-heap table)
          (setf (gethash :managed-rt-heap table)
                (funcall (%vm-runtime-function "MAKE-RT-HEAP" t)))))))

(defun %vm-managed-cons-register-root (state reg value heap)
  "Keep register-held managed cons values visible to runtime GC roots."
  (let ((table (%vm-managed-cons-heap-table state))
        (add-root (%vm-runtime-function "RT-GC-ADD-ROOT")))
    (when (and table add-root)
      (let ((roots (or (gethash :managed-rt-roots table)
                       (setf (gethash :managed-rt-roots table)
                             (make-hash-table :test #'equal)))))
        (let ((root-cell (gethash reg roots)))
          (if root-cell
              (setf (cdr root-cell) value)
              (let ((new-root (cons reg value)))
                (setf (gethash reg roots) new-root)
                (funcall add-root heap new-root))))))))

(defun %vm-managed-cons-pointer-p (value)
  "Return true if VALUE is a runtime NaN-boxed cons pointer."
  (and (integerp value)
       (typep value '(unsigned-byte 64))
       (let ((predicate (%vm-runtime-function "VAL-CONS-P")))
         (and predicate (funcall predicate value)))))

(defvar *vm-managed-cons-sequence-state* nil
  "Dynamically bound VM state for managed cons sequence generic operations.")

(defvar *vm-managed-cons-allocation-enabled* nil
  "When T, vm-cons creates managed cons pointers via %vm-managed-cons-alloc.
When NIL (default), host CL cons cells are used.  Enable for native codegen
paths that require NaN-boxed cons pointers; the VM interpreter stores host
values directly.")

(defun %vm-managed-nil-p (value)
  "Return true if VALUE is host NIL or the runtime NaN-boxed NIL singleton."
  (or (null value)
      (and (integerp value)
           (typep value '(unsigned-byte 64))
           (let ((predicate (%vm-runtime-function "VAL-NIL-P")))
             (and predicate (funcall predicate value))))))

(defun %vm-managed-cons-alloc (state dst-reg car-value cdr-value)
  "Allocate a fresh managed runtime cons cell and return its NaN-boxed pointer.

The runtime heap layout stores a normal object header followed by the two cons
payload words: payload word 0 is CAR, payload word 1 is CDR.  The header is what
lets RT-OBJECT-POINTER-SLOTS trace slots 1 and 2 during minor/major GC."
  (let ((heap (%vm-managed-cons-heap state)))
    (if heap
        (let* ((rt-tag-cons (%vm-runtime-value "+RT-TAG-CONS+" t))
               (ptr-tag-cons (%vm-runtime-value "+TAG-CONS+" t))
               (alloc (%vm-runtime-function "RT-GC-ALLOC" t))
               (make-header (%vm-runtime-function "MAKE-HEADER" t))
               (heap-set (%vm-runtime-function "RT-HEAP-SET" t))
               (encode-pointer (%vm-runtime-function "ENCODE-POINTER" t))
               (object-size 3)
               (addr (funcall alloc heap rt-tag-cons object-size)))
          (funcall heap-set heap addr (funcall make-header object-size rt-tag-cons 0))
          (funcall heap-set heap (+ addr 1) car-value)
          (funcall heap-set heap (+ addr 2) cdr-value)
          (let ((boxed (funcall encode-pointer addr ptr-tag-cons)))
            (%vm-managed-cons-register-root state dst-reg boxed heap)
            boxed))
        (cons car-value cdr-value))))

(defun %vm-managed-cons-slot (state pointer offset)
  "Read managed cons POINTER payload slot OFFSET (1 = car, 2 = cdr)."
  (let ((heap (%vm-managed-cons-heap state)))
    (unless heap
      (error "cl-cc/vm: managed cons pointer has no associated runtime heap"))
    (let* ((decode-pointer (%vm-runtime-function "DECODE-POINTER" t))
           (heap-ref (%vm-runtime-function "RT-HEAP-REF" t))
           (addr (funcall decode-pointer pointer)))
      (funcall heap-ref heap (+ addr offset)))))

(defun %vm-managed-cons-car (state pointer)
  "Return the CAR payload of managed cons POINTER."
  (%vm-managed-cons-slot state pointer 1))

(defun %vm-managed-cons-cdr (state pointer)
  "Return the CDR payload of managed cons POINTER."
  (%vm-managed-cons-slot state pointer 2))

(defun %vm-managed-list-length (state list)
  "Return the length of a proper list that may contain managed cons pointers."
  (unless state
    (error "cl-cc/vm: managed cons sequence operation requires VM state"))
  (loop with count = 0
        for tail = list then (cond
                               ((%vm-managed-cons-pointer-p tail)
                                (%vm-managed-cons-cdr state tail))
                               ((consp tail)
                                (cdr tail))
                               (t tail))
        do (cond
             ((%vm-managed-nil-p tail)
              (return count))
             ((or (%vm-managed-cons-pointer-p tail) (consp tail))
              (incf count))
             (t
              (error 'type-error :datum tail :expected-type 'list)))))

(defun %vm-managed-list-elt (state list index)
  "Return the INDEXth element of a list that may contain managed cons pointers."
  (check-type index (integer 0 *))
  (unless state
    (error "cl-cc/vm: managed cons sequence operation requires VM state"))
  (loop for tail = list then (cond
                               ((%vm-managed-cons-pointer-p tail)
                                (%vm-managed-cons-cdr state tail))
                               ((consp tail)
                                (cdr tail))
                               (t tail))
        for i from 0
        do (cond
             ((%vm-managed-nil-p tail)
              (return nil))
             ((= i index)
              (return (if (%vm-managed-cons-pointer-p tail)
                          (%vm-managed-cons-car state tail)
                          (car tail))))
             ((not (or (%vm-managed-cons-pointer-p tail) (consp tail)))
              (error 'type-error :datum tail :expected-type 'list)))))

(defun %vm-managed-list-tail (state list index)
  "Return the INDEXth CDR of a list that may contain managed cons pointers."
  (check-type index (integer 0 *))
  (unless state
    (error "cl-cc/vm: managed cons sequence operation requires VM state"))
  (loop for tail = list then (cond
                               ((%vm-managed-cons-pointer-p tail)
                                (%vm-managed-cons-cdr state tail))
                               ((consp tail)
                                (cdr tail))
                               (t tail))
        for i from 0
        do (cond
             ((= i index)
              (return (if (%vm-managed-nil-p tail) nil tail)))
             ((%vm-managed-nil-p tail)
              (return nil))
             ((not (or (%vm-managed-cons-pointer-p tail) (consp tail)))
              (error 'type-error :datum tail :expected-type 'list)))))

(defun %vm-managed-member (state item list)
  "Return the first tail whose CAR is EQL to ITEM for a managed/host mixed list."
  (unless state
    (error "cl-cc/vm: managed cons sequence operation requires VM state"))
  (loop for tail = list then (cond
                               ((%vm-managed-cons-pointer-p tail)
                                (%vm-managed-cons-cdr state tail))
                               ((consp tail)
                                (cdr tail))
                               (t tail))
        do (cond
             ((%vm-managed-nil-p tail)
              (return nil))
             ((%vm-managed-cons-pointer-p tail)
              (when (eql item (%vm-managed-cons-car state tail))
                (return tail)))
             ((consp tail)
              (when (eql item (car tail))
                (return tail)))
             (t
              (error 'type-error :datum tail :expected-type 'list)))))

(defun %vm-managed-tree-materialize (state value)
  "Copy a managed/host mixed cons tree into ordinary host cons cells."
  (cond
    ((%vm-managed-nil-p value) nil)
    ((%vm-managed-cons-pointer-p value)
     (cons (%vm-managed-tree-materialize state (%vm-managed-cons-car state value))
           (%vm-managed-tree-materialize state (%vm-managed-cons-cdr state value))))
    ((consp value)
     (cons (%vm-managed-tree-materialize state (car value))
           (%vm-managed-tree-materialize state (cdr value))))
    (t value)))

(defmethod vm-sequence-length ((sequence integer))
  (if (%vm-managed-cons-pointer-p sequence)
      (%vm-managed-list-length *vm-managed-cons-sequence-state* sequence)
      (error 'type-error :datum sequence :expected-type 'sequence)))

(defmethod vm-sequence-elt ((sequence integer) index)
  (if (%vm-managed-cons-pointer-p sequence)
      (%vm-managed-list-elt *vm-managed-cons-sequence-state* sequence index)
      (error 'type-error :datum sequence :expected-type 'sequence)))

(defun %vm-managed-cons-set-slot (state pointer offset value)
  "Write VALUE into managed cons POINTER payload slot OFFSET."
  (let ((heap (%vm-managed-cons-heap state)))
    (unless heap
      (error "cl-cc/vm: managed cons pointer has no associated runtime heap"))
    (let* ((decode-pointer (%vm-runtime-function "DECODE-POINTER" t))
           (heap-set (%vm-runtime-function "RT-HEAP-SET" t))
           (addr (funcall decode-pointer pointer)))
      (funcall heap-set heap (+ addr offset) value)
      pointer)))

(defmethod execute-instruction :around ((inst vm-cons) state pc labels)
  (declare (ignore labels))
  (if *vm-managed-cons-allocation-enabled*
      (let ((car-val (vm-reg-get state (vm-car-reg inst)))
            (cdr-val (vm-reg-get state (vm-cdr-reg inst))))
        (vm-reg-set state (vm-dst inst)
                    (%vm-managed-cons-alloc state (vm-dst inst) car-val cdr-val))
        (values (1+ pc) nil nil))
      (let ((car-val (vm-reg-get state (vm-car-reg inst)))
            (cdr-val (vm-reg-get state (vm-cdr-reg inst))))
        (vm-reg-set state (vm-dst inst) (cons car-val cdr-val))
        (values (1+ pc) nil nil))))

(defmethod execute-instruction :around ((inst vm-car) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst) (%vm-managed-cons-slot state value 1))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-cdr) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst) (%vm-managed-cons-slot state value 2))
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-cons-p) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (%vm-managed-cons-pointer-p value)
        (progn
          (vm-reg-set state (vm-dst inst) 1)
          (values (1+ pc) nil nil))
        (call-next-method))))

;;; List Construction Instructions

(define-vm-instruction vm-make-list (vm-instruction)
  "Create a list of N nil elements."
  (dst nil :reader vm-dst)
  (size nil :reader vm-size)
  (:sexp-tag :make-list)
  (:sexp-slots dst size))

;;; List Accessor Instructions

(define-vm-unary-instruction vm-length  :length  "Get the length of the list in SRC, store in DST.")
(define-vm-unary-instruction vm-reverse :reverse "Reverse the list in SRC, store result in DST.")

(define-vm-instruction vm-append (vm-instruction)
  "Append lists in SRC1 and SRC2, store result in DST."
  (dst nil :reader vm-dst)
  (src1 nil :reader vm-lhs)
  (src2 nil :reader vm-rhs)
  (:sexp-tag :append)
  (:sexp-slots dst src1 src2))

(define-vm-instruction vm-member (vm-instruction)
  "Check if ITEM is in LIST using EQL. Store tail starting at item or nil in DST."
  (dst nil :reader vm-dst)
  (item nil :reader vm-item-reg)
  (list nil :reader vm-list-reg)
  (:sexp-tag :member)
  (:sexp-slots dst item list))

(define-vm-instruction vm-nth (vm-instruction)
  "Get the NTH element of LIST at INDEX, store in DST."
  (dst nil :reader vm-dst)
  (index nil :reader vm-index-reg)
  (list nil :reader vm-list-reg)
  (:sexp-tag :nth)
  (:sexp-slots dst index list))

(define-vm-instruction vm-nthcdr (vm-instruction)
  "Get the NTHCDR of LIST at INDEX, store in DST."
  (dst nil :reader vm-dst)
  (index nil :reader vm-index-reg)
  (list nil :reader vm-list-reg)
  (:sexp-tag :nthcdr)
  (:sexp-slots dst index list))

;;; Named Accessor Instructions (first–tenth, rest, last, butlast)
(define-vm-unary-instruction vm-first   :first   "Get the first element of the list in SRC, store in DST.")
(define-vm-unary-instruction vm-second  :second  "Get the second element of the list in SRC, store in DST.")
(define-vm-unary-instruction vm-third   :third   "Get the third element of the list in SRC, store in DST.")
(define-vm-unary-instruction vm-fourth  :fourth  "Get the fourth element of the list in SRC, store in DST.")
(define-vm-unary-instruction vm-fifth   :fifth   "Get the fifth element of the list in SRC, store in DST.")
(define-vm-unary-instruction vm-sixth   :sixth   "Get the sixth element of the list.")
(define-vm-unary-instruction vm-seventh :seventh "Get the seventh element of the list.")
(define-vm-unary-instruction vm-eighth  :eighth  "Get the eighth element of the list.")
(define-vm-unary-instruction vm-ninth   :ninth   "Get the ninth element of the list.")
(define-vm-unary-instruction vm-tenth   :tenth   "Get the tenth element of the list.")
(define-vm-unary-instruction vm-rest    :rest    "Get the cdr (rest) of the list in SRC, store in DST.")
(define-vm-unary-instruction vm-last    :last    "Get the last cons cell of the list in SRC, store in DST.")
(define-vm-unary-instruction vm-butlast :butlast "Get all but the last element of the list in SRC, store in DST.")

;;; Destructive Operations

(define-vm-unary-instruction vm-nreverse :nreverse "Destructively reverse the list in SRC, store in DST.")

;;; FR-596: nbutlast
(define-vm-unary-instruction vm-nbutlast :nbutlast "Destructively remove last N elements from list.")

;;; FR-640: nreconc
(define-vm-binary-instruction vm-nreconc :nreconc "Destructively reverse LHS appended to RHS.")

(define-vm-instruction vm-rplaca (vm-instruction)
  "Destructively replace the car of CONS with VAL. Store modified cons in result."
  (cons nil :reader vm-cons-reg)
  (val nil :reader vm-val-reg)
  (:sexp-tag :rplaca)
  (:sexp-slots cons val))

(define-vm-instruction vm-rplacd (vm-instruction)
  "Destructively replace the cdr of CONS with VAL. Store modified cons in result."
  (cons nil :reader vm-cons-reg)
  (val nil :reader vm-val-reg)
  (:sexp-tag :rplacd)
  (:sexp-slots cons val))

(defmethod execute-instruction :around ((inst vm-rplaca) state pc labels)
  (declare (ignore labels))
  (let ((cons-val (vm-reg-get state (vm-cons-reg inst))))
    (if (%vm-managed-cons-pointer-p cons-val)
        (progn
          (%vm-managed-cons-set-slot state cons-val 1
                                     (vm-reg-get state (vm-val-reg inst)))
          (vm-reg-set state (vm-cons-reg inst) cons-val)
          (values (1+ pc) nil nil))
        (call-next-method))))

(defmethod execute-instruction :around ((inst vm-rplacd) state pc labels)
  (declare (ignore labels))
  (let ((cons-val (vm-reg-get state (vm-cons-reg inst))))
    (if (%vm-managed-cons-pointer-p cons-val)
        (progn
          (%vm-managed-cons-set-slot state cons-val 2
                                     (vm-reg-get state (vm-val-reg inst)))
          (vm-reg-set state (vm-cons-reg inst) cons-val)
          (values (1+ pc) nil nil))
        (call-next-method))))

;;; Extended List Operations

(define-vm-unary-instruction vm-list-length :list-length "Get the length of a proper list in SRC, store in DST.")
(define-vm-unary-instruction vm-endp        :endp        "Check if list in SRC is empty (nil). Returns 1 if empty, 0 otherwise.")
(define-vm-unary-instruction vm-null        :null        "Check if value in SRC is nil. Returns 1 if nil, 0 otherwise.")

(define-vm-instruction vm-push (vm-instruction)
  "Create new cons with ITEM as car and LIST as cdr. Store result in DST."
  (dst nil :reader vm-dst)
  (item nil :reader vm-item-reg)
  (list nil :reader vm-list-reg)
  (:sexp-tag :push)
  (:sexp-slots dst item list))

(define-vm-instruction vm-pop (vm-instruction)
  "Get the car of LIST, store in DST. (Non-destructive pop semantics for value extraction.)"
  (dst nil :reader vm-dst)
  (list nil :reader vm-list-reg)
  (:sexp-tag :pop)
  (:sexp-slots dst list))
