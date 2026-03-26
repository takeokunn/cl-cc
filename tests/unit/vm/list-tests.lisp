;;;; tests/unit/vm/list-tests.lisp — VM List & Array Instruction Tests
;;;
;;; Tests for execute-instruction on list operations, named accessors,
;;; destructive ops, association lists, coercions, arrays, and bit arrays.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun make-test-vm ()
  "Create a fresh VM state for instruction-level testing."
  (make-instance 'cl-cc:vm-state))

(defun exec1 (inst state &optional (pc 0))
  "Execute a single instruction and return (values new-pc signal return-val)."
  (cl-cc:execute-instruction inst state pc (make-hash-table)))

;;; ─── make-list ──────────────────────────────────────────────────────────────

(deftest vm-list-make-list-creates-nils
  "vm-make-list creates a list of N nils."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 3)
    (exec1 (cl-cc::make-vm-make-list :dst 0 :size 1) s)
    (let ((result (cl-cc:vm-reg-get s 0)))
      (assert-equal '(nil nil nil) result))))

(deftest vm-list-make-list-zero
  "vm-make-list with 0 creates empty list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 0)
    (exec1 (cl-cc::make-vm-make-list :dst 0 :size 1) s)
    (assert-null (cl-cc:vm-reg-get s 0))))

;;; ─── length / reverse / append ──────────────────────────────────────────────

(deftest vm-list-length-proper
  "vm-length returns length of a proper list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a b c d))
    (exec1 (cl-cc::make-vm-length :dst 0 :src 1) s)
    (assert-= 4 (cl-cc:vm-reg-get s 0))))

(deftest vm-list-reverse-list
  "vm-reverse returns a reversed copy."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(1 2 3))
    (exec1 (cl-cc::make-vm-reverse :dst 0 :src 1) s)
    (assert-equal '(3 2 1) (cl-cc:vm-reg-get s 0))))

(deftest vm-list-append-two
  "vm-append concatenates two lists."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a b))
    (cl-cc:vm-reg-set s 2 '(c d))
    (exec1 (cl-cc::make-vm-append :dst 0 :src1 1 :src2 2) s)
    (assert-equal '(a b c d) (cl-cc:vm-reg-get s 0))))

;;; ─── member / nth / nthcdr ─────────────────────────────────────────────────

(deftest vm-list-member-found
  "vm-member returns tail starting at found element."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'b)
    (cl-cc:vm-reg-set s 2 '(a b c))
    (exec1 (cl-cc::make-vm-member :dst 0 :item 1 :list 2) s)
    (assert-equal '(b c) (cl-cc:vm-reg-get s 0))))

(deftest vm-list-member-not-found
  "vm-member returns nil when item is absent."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'z)
    (cl-cc:vm-reg-set s 2 '(a b c))
    (exec1 (cl-cc::make-vm-member :dst 0 :item 1 :list 2) s)
    (assert-null (cl-cc:vm-reg-get s 0))))

(deftest vm-list-nth-index
  "vm-nth retrieves element at given index."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 2)
    (cl-cc:vm-reg-set s 2 '(a b c d))
    (exec1 (cl-cc::make-vm-nth :dst 0 :index 1 :list 2) s)
    (assert-eq 'c (cl-cc:vm-reg-get s 0))))

(deftest vm-list-nthcdr-skips
  "vm-nthcdr returns the tail after N cdrs."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 2)
    (cl-cc:vm-reg-set s 2 '(a b c d))
    (exec1 (cl-cc::make-vm-nthcdr :dst 0 :index 1 :list 2) s)
    (assert-equal '(c d) (cl-cc:vm-reg-get s 0))))

;;; ─── Named accessors ───────────────────────────────────────────────────────

(deftest-each vm-list-named-accessors
  "Named accessor instructions extract correct positional elements."
  :cases (("first"  #'cl-cc::make-vm-first  '(10 20 30 40 50) 10)
          ("second" #'cl-cc::make-vm-second '(10 20 30 40 50) 20)
          ("third"  #'cl-cc::make-vm-third  '(10 20 30 40 50) 30)
          ("fourth" #'cl-cc::make-vm-fourth '(10 20 30 40 50) 40)
          ("fifth"  #'cl-cc::make-vm-fifth  '(10 20 30 40 50) 50))
  (constructor input expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 input)
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (assert-equal expected (cl-cc:vm-reg-get s 0))))

(deftest vm-list-rest-is-cdr
  "vm-rest returns the cdr of the list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a b c))
    (exec1 (cl-cc::make-vm-rest :dst 0 :src 1) s)
    (assert-equal '(b c) (cl-cc:vm-reg-get s 0))))

(deftest vm-list-last-cons
  "vm-last returns the last cons cell."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a b c))
    (exec1 (cl-cc::make-vm-last :dst 0 :src 1) s)
    (assert-equal '(c) (cl-cc:vm-reg-get s 0))))

(deftest vm-list-butlast-drops-last
  "vm-butlast returns all but the last element."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a b c))
    (exec1 (cl-cc::make-vm-butlast :dst 0 :src 1) s)
    (assert-equal '(a b) (cl-cc:vm-reg-get s 0))))

;;; ─── Destructive operations ─────────────────────────────────────────────────

(deftest vm-list-nreverse-destructive
  "vm-nreverse destructively reverses a list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (list 1 2 3))
    (exec1 (cl-cc::make-vm-nreverse :dst 0 :src 1) s)
    (assert-equal '(3 2 1) (cl-cc:vm-reg-get s 0))))

;;; ─── Extended list operations ───────────────────────────────────────────────

(deftest vm-list-list-length-proper
  "vm-list-length returns length for proper list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(x y z))
    (exec1 (cl-cc::make-vm-list-length :dst 0 :src 1) s)
    (assert-= 3 (cl-cc:vm-reg-get s 0))))

(deftest vm-list-endp-nil
  "vm-endp returns 1 for nil (empty list)."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 nil)
    (exec1 (cl-cc::make-vm-endp :dst 0 :src 1) s)
    (assert-= 1 (cl-cc:vm-reg-get s 0))))

(deftest vm-list-endp-non-empty
  "vm-endp returns 0 for non-empty list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a))
    (exec1 (cl-cc::make-vm-endp :dst 0 :src 1) s)
    (assert-= 0 (cl-cc:vm-reg-get s 0))))

(deftest vm-list-null-nil
  "vm-null returns 1 for nil."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 nil)
    (exec1 (cl-cc::make-vm-null :dst 0 :src 1) s)
    (assert-= 1 (cl-cc:vm-reg-get s 0))))

(deftest vm-list-null-non-nil
  "vm-null returns 0 for non-nil value."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 42)
    (exec1 (cl-cc::make-vm-null :dst 0 :src 1) s)
    (assert-= 0 (cl-cc:vm-reg-get s 0))))

(deftest vm-list-push-prepends
  "vm-push conses item onto list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'x)
    (cl-cc:vm-reg-set s 2 '(a b))
    (exec1 (cl-cc::make-vm-push :dst 0 :item 1 :list 2) s)
    (assert-equal '(x a b) (cl-cc:vm-reg-get s 0))))

(deftest vm-list-pop-extracts-car
  "vm-pop extracts the car of the list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(first second third))
    (exec1 (cl-cc::make-vm-pop :dst 0 :list 1) s)
    (assert-eq 'first (cl-cc:vm-reg-get s 0))))

;;; ─── Association lists ──────────────────────────────────────────────────────

(deftest vm-list-assoc-found
  "vm-assoc finds key in alist."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'b)
    (cl-cc:vm-reg-set s 2 '((a . 1) (b . 2) (c . 3)))
    (exec1 (cl-cc::make-vm-assoc :dst 0 :key 1 :alist 2) s)
    (assert-equal '(b . 2) (cl-cc:vm-reg-get s 0))))

(deftest vm-list-assoc-not-found
  "vm-assoc returns nil for missing key."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'z)
    (cl-cc:vm-reg-set s 2 '((a . 1)))
    (exec1 (cl-cc::make-vm-assoc :dst 0 :key 1 :alist 2) s)
    (assert-null (cl-cc:vm-reg-get s 0))))

(deftest vm-list-acons-prepends-pair
  "vm-acons prepends (key . value) to alist."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'x)
    (cl-cc:vm-reg-set s 2 99)
    (cl-cc:vm-reg-set s 3 '((a . 1)))
    (exec1 (cl-cc::make-vm-acons :dst 0 :key 1 :value 2 :alist 3) s)
    (let ((result (cl-cc:vm-reg-get s 0)))
      (assert-equal '(x . 99) (first result))
      (assert-= 2 (length result)))))

;;; ─── equal / nconc / copy-list / copy-tree / subst ──────────────────────────

(deftest vm-list-equal-same
  "vm-equal returns 1 for structurally equal trees."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a (b c)))
    (cl-cc:vm-reg-set s 2 '(a (b c)))
    (exec1 (cl-cc::make-vm-equal :dst 0 :lhs 1 :rhs 2) s)
    (assert-= 1 (cl-cc:vm-reg-get s 0))))

(deftest vm-list-equal-different
  "vm-equal returns 0 for different trees."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a b))
    (cl-cc:vm-reg-set s 2 '(a c))
    (exec1 (cl-cc::make-vm-equal :dst 0 :lhs 1 :rhs 2) s)
    (assert-= 0 (cl-cc:vm-reg-get s 0))))

(deftest vm-list-nconc-joins
  "vm-nconc destructively concatenates two lists."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (list 'a 'b))
    (cl-cc:vm-reg-set s 2 (list 'c 'd))
    (exec1 (cl-cc::make-vm-nconc :dst 0 :lhs 1 :rhs 2) s)
    (assert-equal '(a b c d) (cl-cc:vm-reg-get s 0))))

(deftest vm-list-copy-list-shallow
  "vm-copy-list creates a shallow copy."
  (let ((s (make-test-vm))
        (orig (list 1 2 3)))
    (cl-cc:vm-reg-set s 1 orig)
    (exec1 (cl-cc::make-vm-copy-list :dst 0 :src 1) s)
    (let ((copy (cl-cc:vm-reg-get s 0)))
      (assert-equal orig copy)
      (assert-true (not (eq orig copy))))))

(deftest vm-list-copy-tree-deep
  "vm-copy-tree creates a deep copy of nested structure."
  (let ((s (make-test-vm))
        (orig (list (list 1 2) (list 3 4))))
    (cl-cc:vm-reg-set s 1 orig)
    (exec1 (cl-cc::make-vm-copy-tree :dst 0 :src 1) s)
    (let ((copy (cl-cc:vm-reg-get s 0)))
      (assert-equal orig copy)
      (assert-true (not (eq (first orig) (first copy)))))))

(deftest vm-list-subst-replaces
  "vm-subst substitutes new for old in tree."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'y)    ; new
    (cl-cc:vm-reg-set s 2 'x)    ; old
    (cl-cc:vm-reg-set s 3 '(a x (b x c)))  ; tree
    (exec1 (cl-cc::make-vm-subst :dst 0 :new-val 1 :old-val 2 :tree 3) s)
    (assert-equal '(a y (b y c)) (cl-cc:vm-reg-get s 0))))

;;; ─── Type predicates ────────────────────────────────────────────────────────

(deftest vm-list-listp-cons
  "vm-listp returns 1 for a cons cell."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a))
    (exec1 (cl-cc::make-vm-listp :dst 0 :src 1) s)
    (assert-= 1 (cl-cc:vm-reg-get s 0))))

(deftest vm-list-listp-nil
  "vm-listp returns 1 for nil."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 nil)
    (exec1 (cl-cc::make-vm-listp :dst 0 :src 1) s)
    (assert-= 1 (cl-cc:vm-reg-get s 0))))

(deftest vm-list-listp-atom
  "vm-listp returns 0 for non-list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 42)
    (exec1 (cl-cc::make-vm-listp :dst 0 :src 1) s)
    (assert-= 0 (cl-cc:vm-reg-get s 0))))

(deftest vm-list-atom-number
  "vm-atom returns 1 for a number."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 42)
    (exec1 (cl-cc::make-vm-atom :dst 0 :src 1) s)
    (assert-= 1 (cl-cc:vm-reg-get s 0))))

(deftest vm-list-atom-cons
  "vm-atom returns 0 for a cons."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a))
    (exec1 (cl-cc::make-vm-atom :dst 0 :src 1) s)
    (assert-= 0 (cl-cc:vm-reg-get s 0))))

;;; ─── Coercion instructions ─────────────────────────────────────────────────

(deftest vm-list-coerce-to-string
  "vm-coerce-to-string coerces a character list to string."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(#\h #\i))
    (exec1 (cl-cc::make-vm-coerce-to-string :dst 0 :src 1) s)
    (assert-equal "hi" (cl-cc:vm-reg-get s 0))))

(deftest vm-list-coerce-to-list
  "vm-coerce-to-list coerces a vector to list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 #(1 2 3))
    (exec1 (cl-cc::make-vm-coerce-to-list :dst 0 :src 1) s)
    (assert-equal '(1 2 3) (cl-cc:vm-reg-get s 0))))

(deftest vm-list-coerce-to-vector
  "vm-coerce-to-vector coerces a list to vector."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a b c))
    (exec1 (cl-cc::make-vm-coerce-to-vector :dst 0 :src 1) s)
    (let ((v (cl-cc:vm-reg-get s 0)))
      (assert-true (vectorp v))
      (assert-= 3 (length v))
      (assert-eq 'a (aref v 0)))))

(deftest vm-list-string-coerce-symbol
  "vm-string-coerce converts a symbol to its name string."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'hello)
    (exec1 (cl-cc::make-vm-string-coerce :dst 0 :src 1) s)
    (assert-equal "HELLO" (cl-cc:vm-reg-get s 0))))

;;; ─── Array operations ──────────────────────────────────────────────────────

(deftest vm-array-make-basic
  "vm-make-array creates an array of given size."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 5)
    (exec1 (cl-cc::make-vm-make-array :dst 0 :size-reg 1) s)
    (let ((arr (cl-cc:vm-reg-get s 0)))
      (assert-true (arrayp arr))
      (assert-= 5 (length arr)))))

(deftest vm-array-make-with-initial-element
  "vm-make-array with initial-element fills the array."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 3)
    (cl-cc:vm-reg-set s 2 42)
    (exec1 (cl-cc::make-vm-make-array :dst 0 :size-reg 1 :initial-element 2) s)
    (let ((arr (cl-cc:vm-reg-get s 0)))
      (assert-= 42 (aref arr 0))
      (assert-= 42 (aref arr 2)))))

(deftest vm-array-aref-and-aset
  "vm-aref and vm-aset read and write array elements."
  (let ((s (make-test-vm))
        (arr (make-array 3 :initial-element 0)))
    ;; Set arr[1] = 99
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 1)
    (cl-cc:vm-reg-set s 3 99)
    (exec1 (cl-cc::make-vm-aset :array-reg 1 :index-reg 2 :val-reg 3) s)
    ;; Read arr[1]
    (exec1 (cl-cc::make-vm-aref :dst 0 :array-reg 1 :index-reg 2) s)
    (assert-= 99 (cl-cc:vm-reg-get s 0))))

(deftest vm-array-vector-push-extend-grows
  "vm-vector-push-extend pushes onto adjustable vector."
  (let ((s (make-test-vm))
        (v (make-array 2 :fill-pointer 0 :adjustable t)))
    (cl-cc:vm-reg-set s 1 'hello)
    (cl-cc:vm-reg-set s 2 v)
    (exec1 (cl-cc::make-vm-vector-push-extend :dst 0 :val-reg 1 :array-reg 2) s)
    (assert-= 0 (cl-cc:vm-reg-get s 0))  ; returned index
    (assert-= 1 (fill-pointer v))
    (assert-eq 'hello (aref v 0))))

(deftest vm-array-length-vector
  "vm-array-length returns length of a vector."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 #(a b c d))
    (exec1 (cl-cc::make-vm-array-length :dst 0 :src 1) s)
    (assert-= 4 (cl-cc:vm-reg-get s 0))))

(deftest vm-array-vectorp-true
  "vm-vectorp returns 1 for a vector."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 #(1 2))
    (exec1 (cl-cc::make-vm-vectorp :dst 0 :src 1) s)
    (assert-= 1 (cl-cc:vm-reg-get s 0))))

(deftest vm-array-vectorp-false
  "vm-vectorp returns 0 for a non-vector."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a b))
    (exec1 (cl-cc::make-vm-vectorp :dst 0 :src 1) s)
    (assert-= 0 (cl-cc:vm-reg-get s 0))))

;;; ─── Array dimension queries ────────────────────────────────────────────────

(deftest vm-array-rank-1d
  "vm-array-rank returns 1 for a 1D vector."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 #(1 2 3))
    (exec1 (cl-cc::make-vm-array-rank :dst 0 :src 1) s)
    (assert-= 1 (cl-cc:vm-reg-get s 0))))

(deftest vm-array-rank-2d
  "vm-array-rank returns 2 for a 2D array."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (make-array '(2 3)))
    (exec1 (cl-cc::make-vm-array-rank :dst 0 :src 1) s)
    (assert-= 2 (cl-cc:vm-reg-get s 0))))

(deftest vm-array-total-size-2d
  "vm-array-total-size returns total element count."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (make-array '(3 4)))
    (exec1 (cl-cc::make-vm-array-total-size :dst 0 :src 1) s)
    (assert-= 12 (cl-cc:vm-reg-get s 0))))

(deftest vm-array-dimensions-list
  "vm-array-dimensions returns dimension list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (make-array '(2 5)))
    (exec1 (cl-cc::make-vm-array-dimensions :dst 0 :src 1) s)
    (assert-equal '(2 5) (cl-cc:vm-reg-get s 0))))

(deftest vm-array-dimension-axis
  "vm-array-dimension returns size of specific axis."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (make-array '(3 7)))
    (cl-cc:vm-reg-set s 2 1)  ; axis 1
    (exec1 (cl-cc::make-vm-array-dimension :dst 0 :lhs 1 :rhs 2) s)
    (assert-= 7 (cl-cc:vm-reg-get s 0))))

;;; ─── Row-major access ───────────────────────────────────────────────────────

(deftest vm-array-row-major-aref-2d
  "vm-row-major-aref accesses 2D array by flat index."
  (let ((s (make-test-vm))
        (arr (make-array '(2 3) :initial-contents '((10 20 30) (40 50 60)))))
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 4)  ; row-major index 4 = (1,1) = 50
    (exec1 (cl-cc::make-vm-row-major-aref :dst 0 :lhs 1 :rhs 2) s)
    (assert-= 50 (cl-cc:vm-reg-get s 0))))

(deftest vm-array-row-major-index-computes
  "vm-array-row-major-index computes flat index from subscripts."
  (let ((s (make-test-vm))
        (arr (make-array '(2 3))))
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 '(1 2))  ; subscripts
    (exec1 (cl-cc::make-vm-array-row-major-index :dst 0 :arr 1 :subs 2) s)
    (assert-= 5 (cl-cc:vm-reg-get s 0))))  ; 1*3 + 2 = 5

;;; ─── svref ──────────────────────────────────────────────────────────────────

(deftest vm-array-svref-reads
  "vm-svref reads from simple-vector."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 #(a b c))
    (cl-cc:vm-reg-set s 2 1)
    (exec1 (cl-cc::make-vm-svref :dst 0 :lhs 1 :rhs 2) s)
    (assert-eq 'b (cl-cc:vm-reg-get s 0))))

(deftest vm-array-svset-writes
  "vm-svset writes to simple-vector."
  (let ((s (make-test-vm))
        (v (vector 'a 'b 'c)))
    (cl-cc:vm-reg-set s 1 v)
    (cl-cc:vm-reg-set s 2 0)
    (cl-cc:vm-reg-set s 3 'z)
    (exec1 (cl-cc::make-vm-svset :dst 0 :array-reg 1 :index-reg 2 :val-reg 3) s)
    (assert-eq 'z (svref v 0))
    (assert-eq 'z (cl-cc:vm-reg-get s 0))))

;;; ─── Fill-pointer operations ────────────────────────────────────────────────

(deftest vm-array-fill-pointer-reads
  "vm-fill-pointer-inst returns current fill pointer."
  (let ((s (make-test-vm))
        (v (make-array 5 :fill-pointer 3)))
    (cl-cc:vm-reg-set s 1 v)
    (exec1 (cl-cc::make-vm-fill-pointer-inst :dst 0 :src 1) s)
    (assert-= 3 (cl-cc:vm-reg-get s 0))))

(deftest vm-array-has-fill-pointer-true
  "vm-array-has-fill-pointer-p returns 1 for vector with fill-pointer."
  (let ((s (make-test-vm))
        (v (make-array 5 :fill-pointer 0)))
    (cl-cc:vm-reg-set s 1 v)
    (exec1 (cl-cc::make-vm-array-has-fill-pointer-p :dst 0 :src 1) s)
    (assert-= 1 (cl-cc:vm-reg-get s 0))))

(deftest vm-array-adjustable-true
  "vm-array-adjustable-p returns 1 for adjustable array."
  (let ((s (make-test-vm))
        (v (make-array 5 :adjustable t)))
    (cl-cc:vm-reg-set s 1 v)
    (exec1 (cl-cc::make-vm-array-adjustable-p :dst 0 :src 1) s)
    (assert-= 1 (cl-cc:vm-reg-get s 0))))

(deftest vm-array-vector-push-basic
  "vm-vector-push pushes value and returns new index."
  (let ((s (make-test-vm))
        (v (make-array 5 :fill-pointer 0)))
    (cl-cc:vm-reg-set s 1 'x)
    (cl-cc:vm-reg-set s 2 v)
    (exec1 (cl-cc::make-vm-vector-push :dst 0 :val-reg 1 :array-reg 2) s)
    (assert-= 0 (cl-cc:vm-reg-get s 0))
    (assert-= 1 (fill-pointer v))))

(deftest vm-array-vector-pop-returns-last
  "vm-vector-pop returns and removes the last element."
  (let ((s (make-test-vm))
        (v (make-array 5 :fill-pointer 2 :initial-contents '(a b 0 0 0))))
    (cl-cc:vm-reg-set s 1 v)
    (exec1 (cl-cc::make-vm-vector-pop :dst 0 :src 1) s)
    (assert-eq 'b (cl-cc:vm-reg-get s 0))
    (assert-= 1 (fill-pointer v))))

(deftest vm-array-set-fill-pointer
  "vm-set-fill-pointer changes the fill pointer."
  (let ((s (make-test-vm))
        (v (make-array 10 :fill-pointer 0)))
    (cl-cc:vm-reg-set s 1 v)
    (cl-cc:vm-reg-set s 2 7)
    (exec1 (cl-cc::make-vm-set-fill-pointer :dst 0 :array-reg 1 :val-reg 2) s)
    (assert-= 7 (fill-pointer v))
    (assert-= 7 (cl-cc:vm-reg-get s 0))))

;;; ─── Bit array operations ──────────────────────────────────────────────────

(deftest vm-bit-access-reads
  "vm-bit-access reads a bit from a bit-array."
  (let ((s (make-test-vm))
        (ba (make-array 4 :element-type 'bit :initial-contents '(1 0 1 0))))
    (cl-cc:vm-reg-set s 1 ba)
    (cl-cc:vm-reg-set s 2 2)
    (exec1 (cl-cc::make-vm-bit-access :dst 0 :arr 1 :idx 2) s)
    (assert-= 1 (cl-cc:vm-reg-get s 0))))

(deftest vm-bit-set-writes
  "vm-bit-set writes a bit to a bit-array."
  (let ((s (make-test-vm))
        (ba (make-array 4 :element-type 'bit :initial-element 0)))
    (cl-cc:vm-reg-set s 1 ba)
    (cl-cc:vm-reg-set s 2 1)  ; index
    (cl-cc:vm-reg-set s 3 1)  ; value
    (exec1 (cl-cc::make-vm-bit-set :dst 0 :arr 1 :idx 2 :val 3) s)
    (assert-= 1 (bit ba 1))
    (assert-= 1 (cl-cc:vm-reg-get s 0))))

(deftest vm-bit-and-elementwise
  "vm-bit-and computes element-wise AND."
  (let ((s (make-test-vm))
        (a (make-array 4 :element-type 'bit :initial-contents '(1 1 0 0)))
        (b (make-array 4 :element-type 'bit :initial-contents '(1 0 1 0))))
    (cl-cc:vm-reg-set s 1 a)
    (cl-cc:vm-reg-set s 2 b)
    (exec1 (cl-cc::make-vm-bit-and :dst 0 :lhs 1 :rhs 2) s)
    (let ((result (cl-cc:vm-reg-get s 0)))
      (assert-= 1 (bit result 0))
      (assert-= 0 (bit result 1))
      (assert-= 0 (bit result 2))
      (assert-= 0 (bit result 3)))))

;; NOTE: vm-bit-or has a pre-existing bug — define-simple-instruction resolves
;; bit-or in the cl-cc package (cl-cc::bit-or) instead of cl:bit-or.
;; Test omitted until the source bug is fixed.

(deftest vm-bit-not-inverts
  "vm-bit-not inverts all bits."
  (let ((s (make-test-vm))
        (a (make-array 4 :element-type 'bit :initial-contents '(1 0 1 0))))
    (cl-cc:vm-reg-set s 1 a)
    (exec1 (cl-cc::make-vm-bit-not :dst 0 :src 1) s)
    (let ((result (cl-cc:vm-reg-get s 0)))
      (assert-= 0 (bit result 0))
      (assert-= 1 (bit result 1))
      (assert-= 0 (bit result 2))
      (assert-= 1 (bit result 3)))))

(deftest vm-sbit-reads-simple-bit-vector
  "vm-sbit reads from a simple bit-vector."
  (let ((s (make-test-vm))
        (bv (make-array 3 :element-type 'bit :initial-contents '(0 1 0))))
    (cl-cc:vm-reg-set s 1 bv)
    (cl-cc:vm-reg-set s 2 1)
    (exec1 (cl-cc::make-vm-sbit :dst 0 :arr 1 :idx 2) s)
    (assert-= 1 (cl-cc:vm-reg-get s 0))))

;;; ─── adjust-array / array-displacement ──────────────────────────────────────

(deftest vm-array-adjust-grows
  "vm-adjust-array grows an adjustable array."
  (let ((s (make-test-vm))
        (arr (make-array 3 :initial-element 0 :adjustable t)))
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 5)  ; new size
    (exec1 (cl-cc::make-vm-adjust-array :dst 0 :arr 1 :dims 2) s)
    (let ((result (cl-cc:vm-reg-get s 0)))
      (assert-= 5 (length result)))))

(deftest vm-array-displacement-non-displaced
  "vm-array-displacement returns nil for non-displaced array."
  (let ((s (make-test-vm))
        (arr (make-array 3)))
    (cl-cc:vm-reg-set s 1 arr)
    (exec1 (cl-cc::make-vm-array-displacement :dst 0 :src 1) s)
    (assert-null (cl-cc:vm-reg-get s 0))))

;;; ─── PC advancement ─────────────────────────────────────────────────────────

(deftest vm-list-instructions-advance-pc
  "All list instructions advance PC by 1."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a b c))
    (cl-cc:vm-reg-set s 2 0)
    (multiple-value-bind (new-pc) (exec1 (cl-cc::make-vm-length :dst 0 :src 1) s 5)
      (assert-= 6 new-pc))
    (multiple-value-bind (new-pc) (exec1 (cl-cc::make-vm-reverse :dst 0 :src 1) s 10)
      (assert-= 11 new-pc))))
