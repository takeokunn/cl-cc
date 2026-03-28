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
(define-simple-instruction vm-fifth   :unary fifth)
(define-simple-instruction vm-sixth   :unary sixth)
(define-simple-instruction vm-seventh :unary seventh)
(define-simple-instruction vm-eighth  :unary eighth)
(define-simple-instruction vm-ninth   :unary ninth)
(define-simple-instruction vm-tenth   :unary tenth)
(define-simple-instruction vm-rest :unary rest)
(define-simple-instruction vm-last :unary last)
(define-simple-instruction vm-butlast  :unary butlast)
(define-simple-instruction vm-nbutlast :unary nbutlast)

;;; Instruction Execution - Destructive Operations

(define-simple-instruction vm-nreverse :unary nreverse)
(define-simple-instruction vm-nreconc  :binary nreconc)

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

;;; FR-597: identity / constantly / complement
(define-vm-unary-instruction vm-identity   :identity   "Return argument unchanged.")
(define-vm-unary-instruction vm-constantly :constantly "Return a function that always returns VALUE.")
(define-vm-unary-instruction vm-complement :complement "Return a function that negates the result of PREDICATE.")
(define-simple-instruction vm-identity   :unary identity)
(define-simple-instruction vm-constantly :unary constantly)
(define-simple-instruction vm-complement :unary complement)

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

(define-simple-instruction vm-equal :binary equal)
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

;;; (Array/Vector operations moved to src/vm/array.lisp)

