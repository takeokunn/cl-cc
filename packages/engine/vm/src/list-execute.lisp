(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — List Instruction Execution Methods
;;;
;;; Contains all execute-instruction methods for list VM instructions:
;;; cons/car/cdr, make-list, length/member/nth/nthcdr, named accessors
;;; (first-tenth), rest/last/butlast, nreverse/nbutlast, push/pop,
;;; assoc/acons/subst, equal, nconc, copy-list/copy-tree, listp/atom.
;;;
;;; Instruction struct definitions (define-vm-instruction forms) and the
;;; hash-cons table + sequence protocol are in list.lisp (loads before).
;;;
;;; Load order: after list.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Instruction Execution - Cons Cell Operations

(defmethod execute-instruction ((inst vm-cons) state pc labels)
  (declare (ignore labels))
  (let ((car-val (vm-reg-get state (vm-car-reg inst)))
        (cdr-val (vm-reg-get state (vm-cdr-reg inst))))
    ;; CL CONS must produce a fresh cell. Reusing globally interned cons cells
    ;; breaks destructive operators (RPLACA/RPLACD/NREVERSE family) and can
    ;; accidentally create cyclic/shared syntax structures in user programs.
    ;; Keep VM-HASH-CONS available as an explicit utility, but ordinary VM-CONS
    ;; follows ANSI CL allocation semantics.
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

(defmethod execute-instruction ((inst vm-length) state pc labels)
  (declare (ignore labels))
  (let ((sequence (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (vm-sequence-length sequence))
    (values (1+ pc) nil nil)))

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
        (sequence (vm-reg-get state (vm-list-reg inst))))
    (vm-reg-set state (vm-dst inst) (vm-sequence-elt sequence index))
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

;;; (Array/Vector operations moved to packages/engine/vm/src/array.lisp)
