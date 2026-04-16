(in-package :cl-cc/vm)

;;; VM List Operations

(defparameter *vm-hash-cons-table* (make-hash-table :test #'equal)
  "Runtime hash-cons table keyed by (car cdr).

This is a conservative partial FR-255 implementation. It intentionally uses a
normal hash table for now; weak-table GC integration remains future work.")

(defun vm-clear-hash-cons-table ()
  "Clear the runtime hash-cons table and return it."
  (clrhash *vm-hash-cons-table*)
  *vm-hash-cons-table*)

(defun %vm-hash-cons-shareable-p (car-value cdr-value)
  "Return T when it is safe to intern the cons cell globally.

We only intern flat atomic pairs. Reusing cons cells whose CAR or CDR already
contains cons structure creates observable aliasing for ordinary Lisp list
construction, which breaks destructive operators (for example NREVERSE/NUNION)
and can accidentally form cyclic syntax trees during self-hosted list building.
Fresh cons cells are therefore required for any nested/list-shaped pair."
  (and (not (consp car-value))
       (not (consp cdr-value))))

(defun vm-hash-cons (car-value cdr-value)
  "Return a shared cons cell for CAR-VALUE/CDR-VALUE."
  (if (%vm-hash-cons-shareable-p car-value cdr-value)
      (let* ((key (list car-value cdr-value))
             (existing (gethash key *vm-hash-cons-table*)))
        (or existing
            (setf (gethash key *vm-hash-cons-table*)
                  (cons car-value cdr-value))))
      (cons car-value cdr-value)))

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

(define-vm-instruction vm-car (vm-instruction)
  "Extract the car of the cons cell in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :car)
  (:sexp-slots dst src))

(define-vm-instruction vm-cdr (vm-instruction)
  "Extract the cdr of the cons cell in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :cdr)
  (:sexp-slots dst src))

;;; List Construction Instructions

(define-vm-instruction vm-make-list (vm-instruction)
  "Create a list of N nil elements."
  (dst nil :reader vm-dst)
  (size nil :reader vm-size)
  (:sexp-tag :make-list)
  (:sexp-slots dst size))

;;; List Accessor Instructions

(define-vm-instruction vm-length (vm-instruction)
  "Get the length of the list in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :length)
  (:sexp-slots dst src))

(define-vm-instruction vm-reverse (vm-instruction)
  "Reverse the list in SRC, store result in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :reverse)
  (:sexp-slots dst src))

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

;;; Named Accessor Instructions

(define-vm-instruction vm-first (vm-instruction)
  "Get the first element of the list in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :first)
  (:sexp-slots dst src))

(define-vm-instruction vm-second (vm-instruction)
  "Get the second element of the list in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :second)
  (:sexp-slots dst src))

(define-vm-instruction vm-third (vm-instruction)
  "Get the third element of the list in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :third)
  (:sexp-slots dst src))

(define-vm-instruction vm-fourth (vm-instruction)
  "Get the fourth element of the list in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :fourth)
  (:sexp-slots dst src))

(define-vm-instruction vm-fifth (vm-instruction)
  "Get the fifth element of the list in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :fifth)
  (:sexp-slots dst src))

;;; FR-563: sixth through tenth list accessors
(define-vm-unary-instruction vm-sixth   :sixth   "Get the sixth element of the list.")
(define-vm-unary-instruction vm-seventh :seventh "Get the seventh element of the list.")
(define-vm-unary-instruction vm-eighth  :eighth  "Get the eighth element of the list.")
(define-vm-unary-instruction vm-ninth   :ninth   "Get the ninth element of the list.")
(define-vm-unary-instruction vm-tenth   :tenth   "Get the tenth element of the list.")

(define-vm-instruction vm-rest (vm-instruction)
  "Get the cdr (rest) of the list in SRC, store in DST. Alias for vm-cdr."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :rest)
  (:sexp-slots dst src))

(define-vm-instruction vm-last (vm-instruction)
  "Get the last cons cell of the list in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :last)
  (:sexp-slots dst src))

(define-vm-instruction vm-butlast (vm-instruction)
  "Get all but the last element of the list in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :butlast)
  (:sexp-slots dst src))

;;; Destructive Operations

(define-vm-instruction vm-nreverse (vm-instruction)
  "Destructively reverse the list in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :nreverse)
  (:sexp-slots dst src))

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

;;; Extended List Operations

(define-vm-instruction vm-list-length (vm-instruction)
  "Get the length of a proper list in SRC, store in DST. Signals error for improper lists."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :list-length)
  (:sexp-slots dst src))

(define-vm-instruction vm-endp (vm-instruction)
  "Check if list in SRC is empty (nil). Returns 1 if empty, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :endp)
  (:sexp-slots dst src))

(define-vm-instruction vm-null (vm-instruction)
  "Check if value in SRC is nil. Returns 1 if nil, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :null)
  (:sexp-slots dst src))

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
