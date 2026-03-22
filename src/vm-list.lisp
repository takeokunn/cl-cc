(in-package :cl-cc)

;;; ----------------------------------------------------------------------------
;;; VM List Operations
;;; ----------------------------------------------------------------------------
;;;
;;; This file extends the VM with list manipulation instructions including
;;; cons cell creation, list accessors, and common list operations.
;;;

;;; ----------------------------------------------------------------------------
;;; Cons Cell Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-cons (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (car-src :initarg :car-src :reader vm-car-reg)
   (cdr-src :initarg :cdr-src :reader vm-cdr-reg))
  (:documentation "Create a cons cell from CAR-SRC and CDR-SRC, store in DST."))

(defclass vm-car (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Extract the car of the cons cell in SRC, store in DST."))

(defclass vm-cdr (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Extract the cdr of the cons cell in SRC, store in DST."))

;;; ----------------------------------------------------------------------------
;;; List Construction Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-list (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (count :initarg :count :reader vm-list-count)
   (src-regs :initarg :src-regs :reader vm-src-regs))
  (:documentation "Create a list from COUNT values in SRC-REGS, store in DST.
The first element in SRC-REGS becomes the first element of the list."))

(defclass vm-make-list (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (size :initarg :size :reader vm-size))
  (:documentation "Create a list of N nil elements."))

;;; ----------------------------------------------------------------------------
;;; List Accessor Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-length (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Get the length of the list in SRC, store in DST."))

(defclass vm-reverse (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Reverse the list in SRC, store result in DST."))

(defclass vm-append (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src1 :initarg :src1 :reader vm-lhs)
   (src2 :initarg :src2 :reader vm-rhs))
  (:documentation "Append lists in SRC1 and SRC2, store result in DST."))

(defclass vm-member (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (item :initarg :item :reader vm-item-reg)
   (list :initarg :list :reader vm-list-reg))
  (:documentation "Check if ITEM is in LIST using EQL. Store tail starting at item or nil in DST."))

(defclass vm-nth (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (index :initarg :index :reader vm-index-reg)
   (list :initarg :list :reader vm-list-reg))
  (:documentation "Get the NTH element of LIST at INDEX, store in DST."))

(defclass vm-nthcdr (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (index :initarg :index :reader vm-index-reg)
   (list :initarg :list :reader vm-list-reg))
  (:documentation "Get the NTHCDR of LIST at INDEX, store in DST."))

;;; ----------------------------------------------------------------------------
;;; Named Accessor Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-first (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Get the first element of the list in SRC, store in DST."))

(defclass vm-second (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Get the second element of the list in SRC, store in DST."))

(defclass vm-third (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Get the third element of the list in SRC, store in DST."))

(defclass vm-fourth (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Get the fourth element of the list in SRC, store in DST."))

(defclass vm-fifth (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Get the fifth element of the list in SRC, store in DST."))

(defclass vm-rest (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Get the cdr (rest) of the list in SRC, store in DST. Alias for vm-cdr."))

(defclass vm-last (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Get the last cons cell of the list in SRC, store in DST."))

(defclass vm-butlast (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Get all but the last element of the list in SRC, store in DST."))

;;; ----------------------------------------------------------------------------
;;; Destructive Operations
;;; ----------------------------------------------------------------------------

(defclass vm-nreverse (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Destructively reverse the list in SRC, store in DST."))

(defclass vm-rplaca (vm-instruction)
  ((cons :initarg :cons :reader vm-cons-reg)
   (val :initarg :val :reader vm-val-reg))
  (:documentation "Destructively replace the car of CONS with VAL. Store modified cons in result."))

(defclass vm-rplacd (vm-instruction)
  ((cons :initarg :cons :reader vm-cons-reg)
   (val :initarg :val :reader vm-val-reg))
  (:documentation "Destructively replace the cdr of CONS with VAL. Store modified cons in result."))

;;; ----------------------------------------------------------------------------
;;; Extended List Operations
;;; ----------------------------------------------------------------------------

(defclass vm-list-length (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Get the length of a proper list in SRC, store in DST. Signals error for improper lists."))

(defclass vm-endp (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Check if list in SRC is empty (nil). Returns 1 if empty, 0 otherwise."))

(defclass vm-null (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Check if value in SRC is nil. Returns 1 if nil, 0 otherwise."))

(defclass vm-push (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (item :initarg :item :reader vm-item-reg)
   (list :initarg :list :reader vm-list-reg))
  (:documentation "Create new cons with ITEM as car and LIST as cdr. Store result in DST."))

(defclass vm-pop (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (list :initarg :list :reader vm-list-reg))
  (:documentation "Get the car of LIST, store in DST. (Non-destructive pop semantics for value extraction.)"))

;;; ----------------------------------------------------------------------------
;;; Instruction -> S-expression Conversion
;;; ----------------------------------------------------------------------------

(defmethod instruction->sexp ((inst vm-cons))
  (list :cons (vm-dst inst) (vm-car-reg inst) (vm-cdr-reg inst)))

(defmethod instruction->sexp ((inst vm-car))
  (list :car (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-cdr))
  (list :cdr (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-list))
  (list* :list (vm-dst inst) (vm-list-count inst) (vm-src-regs inst)))

(defmethod instruction->sexp ((inst vm-make-list))
  (list :make-list (vm-dst inst) (vm-size inst)))

(defmethod instruction->sexp ((inst vm-length))
  (list :length (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-reverse))
  (list :reverse (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-append))
  (list :append (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))

(defmethod instruction->sexp ((inst vm-member))
  (list :member (vm-dst inst) (vm-item-reg inst) (vm-list-reg inst)))

(defmethod instruction->sexp ((inst vm-nth))
  (list :nth (vm-dst inst) (vm-index-reg inst) (vm-list-reg inst)))

(defmethod instruction->sexp ((inst vm-nthcdr))
  (list :nthcdr (vm-dst inst) (vm-index-reg inst) (vm-list-reg inst)))

(defmethod instruction->sexp ((inst vm-first))
  (list :first (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-second))
  (list :second (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-third))
  (list :third (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-fourth))
  (list :fourth (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-fifth))
  (list :fifth (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-rest))
  (list :rest (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-last))
  (list :last (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-butlast))
  (list :butlast (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-nreverse))
  (list :nreverse (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-rplaca))
  (list :rplaca (vm-cons-reg inst) (vm-val-reg inst)))

(defmethod instruction->sexp ((inst vm-rplacd))
  (list :rplacd (vm-cons-reg inst) (vm-val-reg inst)))

(defmethod instruction->sexp ((inst vm-list-length))
  (list :list-length (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-endp))
  (list :endp (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-null))
  (list :null (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-push))
  (list :push (vm-dst inst) (vm-item-reg inst) (vm-list-reg inst)))

(defmethod instruction->sexp ((inst vm-pop))
  (list :pop (vm-dst inst) (vm-list-reg inst)))

;;; ----------------------------------------------------------------------------
;;; S-expression -> Instruction Conversion (Extended)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - Cons Cell Operations
;;; ----------------------------------------------------------------------------

(defmethod execute-instruction ((inst vm-cons) state pc labels)
  (declare (ignore labels))
  (let ((car-val (vm-reg-get state (vm-car-reg inst)))
        (cdr-val (vm-reg-get state (vm-cdr-reg inst))))
    (vm-reg-set state (vm-dst inst) (cons car-val cdr-val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-car) state pc labels)
  (declare (ignore labels))
  (let ((cons-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (car cons-val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-cdr) state pc labels)
  (declare (ignore labels))
  (let ((cons-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (cdr cons-val))
    (values (1+ pc) nil nil)))

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - List Construction
;;; ----------------------------------------------------------------------------

(defmethod execute-instruction ((inst vm-list) state pc labels)
  (declare (ignore labels))
  (let ((elements (mapcar (lambda (reg) (vm-reg-get state reg))
                          (vm-src-regs inst))))
    (vm-reg-set state (vm-dst inst) elements)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-make-list) state pc labels)
  (declare (ignore labels))
  (let ((n (vm-reg-get state (vm-size inst))))
    (vm-reg-set state (vm-dst inst) (make-list n))
    (values (1+ pc) nil nil)))

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - List Accessors
;;; ----------------------------------------------------------------------------

(defmethod execute-instruction ((inst vm-length) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (length list-val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-reverse) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (reverse list-val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-append) state pc labels)
  (declare (ignore labels))
  (let ((list1 (vm-reg-get state (vm-lhs inst)))
        (list2 (vm-reg-get state (vm-rhs inst))))
    (vm-reg-set state (vm-dst inst) (append list1 list2))
    (values (1+ pc) nil nil)))

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

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - Named Accessors
;;; ----------------------------------------------------------------------------

(defmethod execute-instruction ((inst vm-first) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (first list-val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-second) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (second list-val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-third) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (third list-val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-fourth) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (fourth list-val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-fifth) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (fifth list-val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-rest) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (rest list-val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-last) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (last list-val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-butlast) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (butlast list-val))
    (values (1+ pc) nil nil)))

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - Destructive Operations
;;; ----------------------------------------------------------------------------

(defmethod execute-instruction ((inst vm-nreverse) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (nreverse list-val))
    (values (1+ pc) nil nil)))

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

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - Extended List Operations
;;; ----------------------------------------------------------------------------

(defmethod execute-instruction ((inst vm-list-length) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (list-length list-val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-endp) state pc labels)
  (declare (ignore labels))
  (let ((list-val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (if (endp list-val) 1 0))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-null) state pc labels)
  (declare (ignore labels))
  (let ((result (if (null (vm-reg-get state (vm-src inst))) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

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

;;; ----------------------------------------------------------------------------
;;; Association List and Utility Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-assoc (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (key :initarg :key :reader vm-key)
   (alist :initarg :alist :reader vm-alist))
  (:documentation "Look up KEY in ALIST using EQL. Returns the pair or NIL."))

(defclass vm-acons (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (key :initarg :key :reader vm-key)
   (value :initarg :value :reader vm-value)
   (alist :initarg :alist :reader vm-alist))
  (:documentation "Prepend (KEY . VALUE) to ALIST."))

(defclass vm-equal (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Deep structural equality. Returns 1/0."))

(defclass vm-nconc (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Destructively concatenate two lists."))

(defclass vm-copy-list (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Shallow copy of a list."))

(defclass vm-copy-tree (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Deep copy of a tree (nested cons structure)."))

(defclass vm-subst (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (new-val :initarg :new-val :reader vm-new-val)
   (old-val :initarg :old-val :reader vm-old-val)
   (tree :initarg :tree :reader vm-tree))
  (:documentation "Substitute NEW for OLD in TREE."))

(defclass vm-listp (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Test if value is a list (nil or cons). Returns 1/0."))

(defclass vm-atom (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Test if value is an atom (not a cons). Returns 1/0."))

(defclass vm-string-coerce (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Coerce value to string (CL string function)."))

(defclass vm-coerce-to-string (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Coerce sequence (char list, vector) to string via CL coerce."))

(defclass vm-coerce-to-list (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Coerce sequence to list via CL coerce."))

(defclass vm-coerce-to-vector (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Coerce sequence to vector via CL coerce."))

;; Instruction -> S-expression
(defmethod instruction->sexp ((inst vm-assoc))
  (list :assoc (vm-dst inst) (vm-key inst) (vm-alist inst)))
(defmethod instruction->sexp ((inst vm-acons))
  (list :acons (vm-dst inst) (vm-key inst) (vm-value inst) (vm-alist inst)))
(defmethod instruction->sexp ((inst vm-equal))
  (list :equal (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))
(defmethod instruction->sexp ((inst vm-nconc))
  (list :nconc (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))
(defmethod instruction->sexp ((inst vm-copy-list))
  (list :copy-list (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-copy-tree))
  (list :copy-tree (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-subst))
  (list :subst (vm-dst inst) (vm-new-val inst) (vm-old-val inst) (vm-tree inst)))
(defmethod instruction->sexp ((inst vm-listp))
  (list :listp (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-atom))
  (list :atom (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-string-coerce))
  (list :string (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-coerce-to-string))
  (list :coerce-to-string (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-coerce-to-list))
  (list :coerce-to-list (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-coerce-to-vector))
  (list :coerce-to-vector (vm-dst inst) (vm-src inst)))

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

(defmethod execute-instruction ((inst vm-equal) state pc labels)
  (declare (ignore labels))
  (let ((result (if (equal (vm-reg-get state (vm-lhs inst))
                           (vm-reg-get state (vm-rhs inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-nconc) state pc labels)
  (declare (ignore labels))
  (let ((lhs (vm-reg-get state (vm-lhs inst)))
        (rhs (vm-reg-get state (vm-rhs inst))))
    (vm-reg-set state (vm-dst inst) (nconc lhs rhs))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-copy-list) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (copy-list (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-copy-tree) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (copy-tree (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-subst) state pc labels)
  (declare (ignore labels))
  (let ((new-val (vm-reg-get state (vm-new-val inst)))
        (old-val (vm-reg-get state (vm-old-val inst)))
        (tree (vm-reg-get state (vm-tree inst))))
    (vm-reg-set state (vm-dst inst) (subst new-val old-val tree))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-listp) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (listp (vm-reg-get state (vm-src inst))) 1 0))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-atom) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (atom (vm-reg-get state (vm-src inst))) 1 0))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-string-coerce) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (string (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

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

;;; ----------------------------------------------------------------------------
;;; Array/Vector Operations
;;; ----------------------------------------------------------------------------

(defclass vm-make-array (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (size-reg :initarg :size :reader vm-size-reg)
   (initial-element :initarg :initial-element :initform nil :reader vm-initial-element)
   (fill-pointer :initarg :fill-pointer :initform nil :reader vm-fill-pointer)
   (adjustable :initarg :adjustable :initform nil :reader vm-adjustable))
  (:documentation "Create an array of given size. Supports :initial-element, :fill-pointer, :adjustable."))

(defclass vm-aref (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (array-reg :initarg :array :reader vm-array-reg)
   (index-reg :initarg :index :reader vm-index-reg))
  (:documentation "Get element at INDEX from ARRAY, store in DST."))

(defclass vm-aset (vm-instruction)
  ((array-reg :initarg :array :reader vm-array-reg)
   (index-reg :initarg :index :reader vm-index-reg)
   (val-reg :initarg :val :reader vm-val-reg))
  (:documentation "Set element at INDEX in ARRAY to VAL."))

(defclass vm-vector-push-extend (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (val-reg :initarg :val :reader vm-val-reg)
   (array-reg :initarg :array :reader vm-array-reg))
  (:documentation "Push VAL onto adjustable ARRAY, extending if needed. Store new index in DST."))

(defclass vm-array-length (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Get the length of array/vector in SRC, store in DST."))

(defclass vm-vectorp (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Check if value is a vector."))

;;; Instruction -> S-expression for array ops

(defmethod instruction->sexp ((inst vm-make-array))
  (list :make-array (vm-dst inst) (vm-size-reg inst)))

(defmethod instruction->sexp ((inst vm-aref))
  (list :aref (vm-dst inst) (vm-array-reg inst) (vm-index-reg inst)))

(defmethod instruction->sexp ((inst vm-aset))
  (list :aset (vm-array-reg inst) (vm-index-reg inst) (vm-val-reg inst)))

(defmethod instruction->sexp ((inst vm-vector-push-extend))
  (list :vector-push-extend (vm-dst inst) (vm-val-reg inst) (vm-array-reg inst)))

(defmethod instruction->sexp ((inst vm-array-length))
  (list :array-length (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-vectorp))
  (list :vectorp (vm-dst inst) (vm-src inst)))

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

(defmethod execute-instruction ((inst vm-array-length) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (length (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-vectorp) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (if (vectorp (vm-reg-get state (vm-src inst))) 1 0))
  (values (1+ pc) nil nil))
