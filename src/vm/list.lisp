(in-package :cl-cc)

;;; VM List Operations

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

;;; Instruction Execution - Cons Cell Operations

(defmethod execute-instruction ((inst vm-cons) state pc labels)
  (declare (ignore labels))
  (let ((car-val (vm-reg-get state (vm-car-reg inst)))
        (cdr-val (vm-reg-get state (vm-cdr-reg inst))))
    (vm-reg-set state (vm-dst inst) (cons car-val cdr-val))
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-car :unary car)
(define-simple-instruction vm-cdr :unary cdr)

;;; Instruction Execution - List Construction

(defmethod execute-instruction ((inst vm-make-list) state pc labels)
  (declare (ignore labels))
  (let ((n (vm-reg-get state (vm-size inst))))
    (vm-reg-set state (vm-dst inst) (make-list n))
    (values (1+ pc) nil nil)))

;;; Instruction Execution - List Accessors

(define-simple-instruction vm-length :unary length)
(define-simple-instruction vm-reverse :unary reverse)
(define-simple-instruction vm-append :binary append)

(defmethod execute-instruction ((inst vm-member) state pc labels)
  (declare (ignore labels))
  (let ((item (vm-reg-get state (vm-item-reg inst)))
        (list (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst) (member item list))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-nth) state pc labels)
  (declare (ignore labels))
  (let ((index (vm-reg-get state (vm-index-reg inst)))
        (list (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst) (nth index list))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-nthcdr) state pc labels)
  (declare (ignore labels))
  (let ((index (vm-reg-get state (vm-index-reg inst)))
        (list (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst) (nthcdr index list))
    (values (1+ pc) nil nil)))

;;; Instruction Execution - Named Accessors

(define-simple-instruction vm-first :unary first)
(define-simple-instruction vm-second :unary second)
(define-simple-instruction vm-third :unary third)
(define-simple-instruction vm-fourth :unary fourth)
(define-simple-instruction vm-fifth :unary fifth)
(define-simple-instruction vm-rest :unary rest)
(define-simple-instruction vm-last :unary last)
(define-simple-instruction vm-butlast :unary butlast)

;;; Instruction Execution - Destructive Operations

(define-simple-instruction vm-nreverse :unary nreverse)

(defmethod execute-instruction ((inst vm-rplaca) state pc labels)
  (declare (ignore labels))
  (let ((cons-val (vm-reg-get state (vm-cons-reg inst)))
        (new-val (vm-reg-get state (vm-val-reg inst))))
    (rplaca cons-val new-val)
    (vm-reg-set state (vm-cons-reg inst) cons-val)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-rplacd) state pc labels)
  (declare (ignore labels))
  (let ((cons-val (vm-reg-get state (vm-cons-reg inst)))
        (new-val (vm-reg-get state (vm-val-reg inst))))
    (rplacd cons-val new-val)
    (vm-reg-set state (vm-cons-reg inst) cons-val)
    (values (1+ pc) nil nil)))

;;; Instruction Execution - Extended List Operations

(define-simple-instruction vm-list-length :unary list-length)
(define-simple-instruction vm-endp :pred1 endp)
(define-simple-instruction vm-null :pred1 null)

(defmethod execute-instruction ((inst vm-push) state pc labels)
  (declare (ignore labels))
  (let ((item (vm-reg-get state (vm-item-reg inst)))
        (list (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst) (cons item list))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-pop) state pc labels)
  (declare (ignore labels))
  (let ((list (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst) (car list))
    (values (1+ pc) nil nil)))

;;; Association List and Utility Instructions

(define-vm-instruction vm-assoc (vm-instruction)
  "Look up KEY in ALIST using EQL. Returns the pair or NIL."
  (dst nil :reader vm-dst)
  (key nil :reader vm-key)
  (alist nil :reader vm-alist)
  (:sexp-tag :assoc)
  (:sexp-slots dst key alist))

(define-vm-instruction vm-acons (vm-instruction)
  "Prepend (KEY . VALUE) to ALIST."
  (dst nil :reader vm-dst)
  (key nil :reader vm-key)
  (value nil :reader vm-value)
  (alist nil :reader vm-alist)
  (:sexp-tag :acons)
  (:sexp-slots dst key value alist))

(define-vm-instruction vm-equal (vm-instruction)
  "Deep structural equality. Returns 1/0."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :equal)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-nconc (vm-instruction)
  "Destructively concatenate two lists."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :nconc)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-copy-list (vm-instruction)
  "Shallow copy of a list."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :copy-list)
  (:sexp-slots dst src))

(define-vm-instruction vm-copy-tree (vm-instruction)
  "Deep copy of a tree (nested cons structure)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :copy-tree)
  (:sexp-slots dst src))

(define-vm-instruction vm-subst (vm-instruction)
  "Substitute NEW for OLD in TREE."
  (dst nil :reader vm-dst)
  (new-val nil :reader vm-new-val)
  (old-val nil :reader vm-old-val)
  (tree nil :reader vm-tree)
  (:sexp-tag :subst)
  (:sexp-slots dst new-val old-val tree))

(define-vm-instruction vm-listp (vm-instruction)
  "Test if value is a list (nil or cons). Returns 1/0."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :listp)
  (:sexp-slots dst src))

(define-vm-instruction vm-atom (vm-instruction)
  "Test if value is an atom (not a cons). Returns 1/0."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :atom)
  (:sexp-slots dst src))

(define-vm-instruction vm-string-coerce (vm-instruction)
  "Coerce value to string (CL string function)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :string)
  (:sexp-slots dst src))

(define-vm-instruction vm-coerce-to-string (vm-instruction)
  "Coerce sequence (char list, vector) to string via CL coerce."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :coerce-to-string)
  (:sexp-slots dst src))

(define-vm-instruction vm-coerce-to-list (vm-instruction)
  "Coerce sequence to list via CL coerce."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :coerce-to-list)
  (:sexp-slots dst src))

(define-vm-instruction vm-coerce-to-vector (vm-instruction)
  "Coerce sequence to vector via CL coerce."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :coerce-to-vector)
  (:sexp-slots dst src))

;; Execute instructions
(defmethod execute-instruction ((inst vm-assoc) state pc labels)
  (declare (ignore labels))
  (let* ((key (vm-reg-get state (vm-key inst)))
         (alist (vm-reg-get state (vm-alist inst)))
         (result (assoc key alist)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-acons) state pc labels)
  (declare (ignore labels))
  (let* ((key (vm-reg-get state (vm-key inst)))
         (value (vm-reg-get state (vm-value inst)))
         (alist (vm-reg-get state (vm-alist inst))))
    (vm-reg-set state (vm-dst inst) (acons key value alist))
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-equal :pred2 equal)
(define-simple-instruction vm-nconc :binary nconc)
(define-simple-instruction vm-copy-list :unary copy-list)
(define-simple-instruction vm-copy-tree :unary copy-tree)

(defmethod execute-instruction ((inst vm-subst) state pc labels)
  (declare (ignore labels))
  (let ((new-val (vm-reg-get state (vm-new-val inst)))
        (old-val (vm-reg-get state (vm-old-val inst)))
        (tree (vm-reg-get state (vm-tree inst))))
    (vm-reg-set state (vm-dst inst) (subst new-val old-val tree))
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-listp :pred1 listp)
(define-simple-instruction vm-atom :pred1 atom)
(define-simple-instruction vm-string-coerce :unary string)

(defmethod execute-instruction ((inst vm-coerce-to-string) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (coerce (vm-reg-get state (vm-src inst)) 'string))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-coerce-to-list) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (coerce (vm-reg-get state (vm-src inst)) 'list))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-coerce-to-vector) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (coerce (vm-reg-get state (vm-src inst)) 'vector))
  (values (1+ pc) nil nil))

;;; Array/Vector Operations

(define-vm-instruction vm-make-array (vm-instruction)
  "Create an array of given size. Supports :initial-element, :fill-pointer, :adjustable."
  (dst nil :reader vm-dst)
  (size-reg nil :reader vm-size-reg)
  (initial-element nil :reader vm-initial-element)
  (fill-pointer nil :reader vm-fill-pointer)
  (adjustable nil :reader vm-adjustable)
  (:sexp-tag :make-array)
  (:sexp-slots dst size-reg))

(define-vm-instruction vm-aref (vm-instruction)
  "Get element at INDEX from ARRAY, store in DST."
  (dst nil :reader vm-dst)
  (array-reg nil :reader vm-array-reg)
  (index-reg nil :reader vm-index-reg)
  (:sexp-tag :aref)
  (:sexp-slots dst array-reg index-reg))

(define-vm-instruction vm-aset (vm-instruction)
  "Set element at INDEX in ARRAY to VAL."
  (array-reg nil :reader vm-array-reg)
  (index-reg nil :reader vm-index-reg)
  (val-reg nil :reader vm-val-reg)
  (:sexp-tag :aset)
  (:sexp-slots array-reg index-reg val-reg))

(define-vm-instruction vm-vector-push-extend (vm-instruction)
  "Push VAL onto adjustable ARRAY, extending if needed. Store new index in DST."
  (dst nil :reader vm-dst)
  (val-reg nil :reader vm-val-reg)
  (array-reg nil :reader vm-array-reg)
  (:sexp-tag :vector-push-extend)
  (:sexp-slots dst val-reg array-reg))

(define-vm-instruction vm-array-length (vm-instruction)
  "Get the length of array/vector in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :array-length)
  (:sexp-slots dst src))

(define-vm-instruction vm-vectorp (vm-instruction)
  "Check if value is a vector."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :vectorp)
  (:sexp-slots dst src))

;;; Execute array instructions

(defmethod execute-instruction ((inst vm-make-array) state pc labels)
  (declare (ignore labels))
  (let* ((size (vm-reg-get state (vm-size-reg inst)))
         (init-elem (when (vm-initial-element inst)
                      (vm-reg-get state (vm-initial-element inst))))
         (fp (vm-fill-pointer inst))
         (adj (vm-adjustable inst))
         (arr (cond
                ((and fp adj)
                 (make-array size :initial-element (or init-elem 0)
                                  :fill-pointer (if (eq fp t) 0 fp)
                                  :adjustable t))
                (fp
                 (make-array size :initial-element (or init-elem 0)
                                  :fill-pointer (if (eq fp t) 0 fp)))
                (init-elem
                 (make-array size :initial-element init-elem))
                (t
                 (make-array size :initial-element 0)))))
    (vm-reg-set state (vm-dst inst) arr)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-aref) state pc labels)
  (declare (ignore labels))
  (let ((arr (vm-reg-get state (vm-array-reg inst)))
        (idx (vm-reg-get state (vm-index-reg inst))))
    (vm-reg-set state (vm-dst inst) (aref arr idx))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-aset) state pc labels)
  (declare (ignore labels))
  (let ((arr (vm-reg-get state (vm-array-reg inst)))
        (idx (vm-reg-get state (vm-index-reg inst)))
        (val (vm-reg-get state (vm-val-reg inst))))
    (setf (aref arr idx) val)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-vector-push-extend) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-val-reg inst)))
        (arr (vm-reg-get state (vm-array-reg inst))))
    (vm-reg-set state (vm-dst inst) (vector-push-extend val arr))
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-array-length :unary length)
(define-simple-instruction vm-vectorp :pred1 vectorp)

;;; FR-601: Array dimension queries

(define-vm-instruction vm-array-rank (vm-instruction)
  "Return number of dimensions of ARRAY."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-rank) (:sexp-slots dst src))
(define-simple-instruction vm-array-rank :unary array-rank)

(define-vm-instruction vm-array-total-size (vm-instruction)
  "Return total number of elements in ARRAY."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-total-size) (:sexp-slots dst src))
(define-simple-instruction vm-array-total-size :unary array-total-size)

(define-vm-instruction vm-array-dimensions (vm-instruction)
  "Return list of dimension sizes of ARRAY."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-dimensions) (:sexp-slots dst src))
(define-simple-instruction vm-array-dimensions :unary array-dimensions)

(define-vm-instruction vm-array-dimension (vm-instruction)
  "Return size of dimension AXIS of ARRAY."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :array-dimension) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-array-dimension) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (array-dimension (vm-reg-get state (vm-lhs inst))
                               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

;;; FR-602: Row-major access

(define-vm-instruction vm-row-major-aref (vm-instruction)
  "Access array element by row-major index."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :row-major-aref) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-row-major-aref) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (row-major-aref (vm-reg-get state (vm-lhs inst))
                              (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

;;; FR-602: array-row-major-index — compute flat index from multi-dimensional subscripts

(define-vm-instruction vm-array-row-major-index (vm-instruction)
  "Compute the row-major index for ARRAY given a list of SUBSCRIPTS."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (subs nil :reader vm-subs)
  (:sexp-tag :array-row-major-index) (:sexp-slots dst arr subs))
(defmethod execute-instruction ((inst vm-array-row-major-index) state pc labels)
  (declare (ignore labels))
  (let ((arr (vm-reg-get state (vm-arr inst)))
        (subs (vm-reg-get state (vm-subs inst))))
    (vm-reg-set state (vm-dst inst) (apply #'array-row-major-index arr subs))
    (values (1+ pc) nil nil)))

;;; FR-603: svref — simple-vector element access (1D vector, no fill-pointer)

(define-vm-instruction vm-svref (vm-instruction)
  "Access element of simple-vector by index."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :svref) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-svref) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (svref (vm-reg-get state (vm-lhs inst))
                     (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-svset (vm-instruction)
  "Set element of simple-vector by index. Returns new value."
  (dst nil :reader vm-dst) (array-reg nil :reader vm-array-reg)
  (index-reg nil :reader vm-index-reg) (val-reg nil :reader vm-val-reg)
  (:sexp-tag :svset) (:sexp-slots dst array-reg index-reg val-reg))
(defmethod execute-instruction ((inst vm-svset) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-val-reg inst))))
    (setf (svref (vm-reg-get state (vm-array-reg inst))
                 (vm-reg-get state (vm-index-reg inst))) val)
    (vm-reg-set state (vm-dst inst) val)
    (values (1+ pc) nil nil)))

;;; FR-604: fill-pointer and vector-push

(define-vm-instruction vm-fill-pointer-inst (vm-instruction)
  "Return fill pointer of vector."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :fill-pointer-inst) (:sexp-slots dst src))
(define-simple-instruction vm-fill-pointer-inst :unary fill-pointer)

(define-vm-instruction vm-array-has-fill-pointer-p (vm-instruction)
  "Return T if array has a fill pointer."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-has-fill-pointer-p) (:sexp-slots dst src))
(define-simple-instruction vm-array-has-fill-pointer-p :pred1 array-has-fill-pointer-p)

(define-vm-instruction vm-array-adjustable-p (vm-instruction)
  "Return T if array is adjustable."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-adjustable-p) (:sexp-slots dst src))
(define-simple-instruction vm-array-adjustable-p :pred1 adjustable-array-p)

(define-vm-instruction vm-vector-push (vm-instruction)
  "Push VAL onto ARRAY if below fill-pointer limit. Returns new index or NIL."
  (dst nil :reader vm-dst) (val-reg nil :reader vm-val-reg)
  (array-reg nil :reader vm-array-reg)
  (:sexp-tag :vector-push) (:sexp-slots dst val-reg array-reg))
(defmethod execute-instruction ((inst vm-vector-push) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-val-reg inst)))
        (arr (vm-reg-get state (vm-array-reg inst))))
    (vm-reg-set state (vm-dst inst) (vector-push val arr))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-vector-pop (vm-instruction)
  "Pop last element from ARRAY (decrement fill pointer). Returns element."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :vector-pop) (:sexp-slots dst src))
(define-simple-instruction vm-vector-pop :unary vector-pop)

(define-vm-instruction vm-set-fill-pointer (vm-instruction)
  "Set fill pointer of ARRAY to NEW-FP. Returns NEW-FP."
  (dst nil :reader vm-dst) (array-reg nil :reader vm-array-reg)
  (val-reg nil :reader vm-val-reg)
  (:sexp-tag :set-fill-pointer) (:sexp-slots dst array-reg val-reg))
(defmethod execute-instruction ((inst vm-set-fill-pointer) state pc labels)
  (declare (ignore labels))
  (let* ((arr (vm-reg-get state (vm-array-reg inst)))
         (fp (vm-reg-get state (vm-val-reg inst))))
    (setf (fill-pointer arr) fp)
    (vm-reg-set state (vm-dst inst) fp)
    (values (1+ pc) nil nil)))

;;; FR-606: Bit array operations

(define-vm-instruction vm-bit-access (vm-instruction)
  "Access element of bit array: (bit array index)."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (idx nil :reader vm-idx)
  (:sexp-tag :bit-access) (:sexp-slots dst arr idx))
(defmethod execute-instruction ((inst vm-bit-access) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (bit (vm-reg-get state (vm-arr inst))
                   (vm-reg-get state (vm-idx inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-bit-set (vm-instruction)
  "Set element of bit array: (setf (bit array index) val)."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (idx nil :reader vm-idx)
  (val nil :reader vm-val)
  (:sexp-tag :bit-set) (:sexp-slots dst arr idx val))
(defmethod execute-instruction ((inst vm-bit-set) state pc labels)
  (declare (ignore labels))
  (let ((arr (vm-reg-get state (vm-arr inst)))
        (idx (vm-reg-get state (vm-idx inst)))
        (v   (vm-reg-get state (vm-val inst))))
    (setf (bit arr idx) v)
    (vm-reg-set state (vm-dst inst) v)
    (values (1+ pc) nil nil)))

;; bit-and, bit-or, bit-xor, bit-not: element-wise boolean ops on bit-arrays
(define-vm-instruction vm-bit-and (vm-instruction)
  "Element-wise AND of two bit arrays."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :bit-and) (:sexp-slots dst lhs rhs))
(define-simple-instruction vm-bit-and :binary bit-and)

(define-vm-instruction vm-bit-or (vm-instruction)
  "Element-wise OR of two bit arrays."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :bit-or) (:sexp-slots dst lhs rhs))
(define-simple-instruction vm-bit-or :binary bit-or)

(define-vm-instruction vm-bit-xor (vm-instruction)
  "Element-wise XOR of two bit arrays."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :bit-xor) (:sexp-slots dst lhs rhs))
(define-simple-instruction vm-bit-xor :binary bit-xor)

(define-vm-instruction vm-bit-not (vm-instruction)
  "Element-wise NOT of a bit array."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :bit-not) (:sexp-slots dst src))
(define-simple-instruction vm-bit-not :unary bit-not)

(define-vm-instruction vm-sbit (vm-instruction)
  "Access element of simple bit-vector: (sbit svec index)."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (idx nil :reader vm-idx)
  (:sexp-tag :sbit) (:sexp-slots dst arr idx))
(defmethod execute-instruction ((inst vm-sbit) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (sbit (vm-reg-get state (vm-arr inst))
                    (vm-reg-get state (vm-idx inst))))
  (values (1+ pc) nil nil))

;;; FR-605: adjust-array and array-displacement

(define-vm-instruction vm-adjust-array (vm-instruction)
  "Adjust dimensions of an adjustable array."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (dims nil :reader vm-dims)
  (:sexp-tag :adjust-array) (:sexp-slots dst arr dims))
(defmethod execute-instruction ((inst vm-adjust-array) state pc labels)
  (declare (ignore labels))
  (let* ((arr (vm-reg-get state (vm-arr inst)))
         (dims (vm-reg-get state (vm-dims inst)))
         (new-dims (if (listp dims) dims (list dims))))
    (vm-reg-set state (vm-dst inst) (adjust-array arr new-dims))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-array-displacement (vm-instruction)
  "Return displaced-to array and offset (both nil for non-displaced arrays)."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-displacement) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-array-displacement) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (displaced-to offset)
      (array-displacement (vm-reg-get state (vm-src inst)))
    (vm-reg-set state (vm-dst inst) displaced-to)
    (setf (vm-values-list state) (list displaced-to offset))
    (values (1+ pc) nil nil)))
