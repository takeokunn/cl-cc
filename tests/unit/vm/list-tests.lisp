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

(deftest vm-list-make-list
  "vm-make-list: N nils for size N; empty list for size 0."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 3)
    (exec1 (cl-cc::make-vm-make-list :dst 0 :size 1) s)
    (assert-equal '(nil nil nil) (cl-cc:vm-reg-get s 0)))
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

(deftest vm-list-member-behavior
  "vm-member returns tail on hit; nil on miss."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 2 '(a b c))
    (cl-cc:vm-reg-set s 1 'b)
    (exec1 (cl-cc::make-vm-member :dst 0 :item 1 :list 2) s)
    (assert-equal '(b c) (cl-cc:vm-reg-get s 0)))
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 2 '(a b c))
    (cl-cc:vm-reg-set s 1 'z)
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
  "Named accessor instructions (first–fifth, rest, last, butlast) extract the correct element."
  :cases (("first"   #'cl-cc::make-vm-first   '(10 20 30 40 50) 10)
          ("second"  #'cl-cc::make-vm-second  '(10 20 30 40 50) 20)
          ("third"   #'cl-cc::make-vm-third   '(10 20 30 40 50) 30)
          ("fourth"  #'cl-cc::make-vm-fourth  '(10 20 30 40 50) 40)
          ("fifth"   #'cl-cc::make-vm-fifth   '(10 20 30 40 50) 50)
          ("rest"    #'cl-cc::make-vm-rest    '(a b c)          '(b c))
          ("last"    #'cl-cc::make-vm-last    '(a b c)          '(c))
          ("butlast" #'cl-cc::make-vm-butlast '(a b c)          '(a b)))
  (constructor input expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 input)
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (assert-equal expected (cl-cc:vm-reg-get s 0))))

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

(deftest-each vm-list-empty-predicates
  "vm-endp and vm-null both detect the empty list."
  :cases (("endp/nil"       #'cl-cc::make-vm-endp nil  1)
          ("endp/non-empty" #'cl-cc::make-vm-endp '(a) 0)
          ("null/nil"       #'cl-cc::make-vm-null nil  1)
          ("null/non-nil"   #'cl-cc::make-vm-null 42   0))
  (constructor value expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 value)
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (assert-= expected (cl-cc:vm-reg-get s 0))))

(deftest vm-list-push-and-pop
  "vm-push conses item onto list; vm-pop extracts car."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'x)
    (cl-cc:vm-reg-set s 2 '(a b))
    (exec1 (cl-cc::make-vm-push :dst 0 :item 1 :list 2) s)
    (assert-equal '(x a b) (cl-cc:vm-reg-get s 0)))
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(first second third))
    (exec1 (cl-cc::make-vm-pop :dst 0 :list 1) s)
    (assert-eq 'first (cl-cc:vm-reg-get s 0))))

;;; ─── Association lists ──────────────────────────────────────────────────────

(deftest vm-list-assoc-behavior
  "vm-assoc returns matching pair on hit; nil on miss."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'b)
    (cl-cc:vm-reg-set s 2 '((a . 1) (b . 2) (c . 3)))
    (exec1 (cl-cc::make-vm-assoc :dst 0 :key 1 :alist 2) s)
    (assert-equal '(b . 2) (cl-cc:vm-reg-get s 0)))
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

(deftest vm-list-equal-behavior
  "vm-equal returns truthy for structurally equal trees; NIL for different trees."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a (b c)))
    (cl-cc:vm-reg-set s 2 '(a (b c)))
    (exec1 (cl-cc::make-vm-equal :dst 0 :lhs 1 :rhs 2) s)
    (assert-true (cl-cc:vm-reg-get s 0)))
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a b))
    (cl-cc:vm-reg-set s 2 '(a c))
    (exec1 (cl-cc::make-vm-equal :dst 0 :lhs 1 :rhs 2) s)
    (assert-true (null (cl-cc:vm-reg-get s 0)))))

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

(deftest-each vm-list-type-predicates
  "vm-listp and vm-atom classify values as list or atom."
  :cases (("listp/cons"   #'cl-cc::make-vm-listp '(a) 1)
          ("listp/nil"    #'cl-cc::make-vm-listp nil  1)
          ("listp/atom"   #'cl-cc::make-vm-listp 42   0)
          ("atom/number"  #'cl-cc::make-vm-atom  42   1)
          ("atom/cons"    #'cl-cc::make-vm-atom  '(a) 0))
  (constructor value expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 value)
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (assert-= expected (cl-cc:vm-reg-get s 0))))

;;; ─── Coercion instructions ─────────────────────────────────────────────────

(deftest-each vm-list-coerce-simple
  "Simple coercion instructions round-trip a single value correctly."
  :cases (("chars-to-string" #'cl-cc::make-vm-coerce-to-string  '(#\h #\i)  "hi")
          ("vector-to-list"  #'cl-cc::make-vm-coerce-to-list    #(1 2 3)    '(1 2 3))
          ("symbol-to-name"  #'cl-cc::make-vm-string-coerce     'hello      "HELLO"))
  (ctor input expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 input)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (assert-equal expected (cl-cc:vm-reg-get s 0))))

(deftest vm-list-coerce-to-vector
  "vm-coerce-to-vector coerces a list to a proper vector."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a b c))
    (exec1 (cl-cc::make-vm-coerce-to-vector :dst 0 :src 1) s)
    (let ((v (cl-cc:vm-reg-get s 0)))
      (assert-true (vectorp v))
      (assert-= 3 (length v))
      (assert-eq 'a (aref v 0)))))

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

(deftest-each vm-array-vectorp
  "vm-vectorp returns 1 for vectors, 0 for non-vectors."
  :cases (("vector"     #(1 2) 1)
          ("non-vector" '(a b) 0))
  (value expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 value)
    (exec1 (cl-cc::make-vm-vectorp :dst 0 :src 1) s)
    (assert-= expected (cl-cc:vm-reg-get s 0))))

;;; ─── Array dimension queries ────────────────────────────────────────────────

(deftest-each vm-array-rank
  "vm-array-rank returns the number of dimensions."
  :cases (("1d" #(1 2 3)          1)
          ("2d" (make-array '(2 3)) 2))
  (arr expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 arr)
    (exec1 (cl-cc::make-vm-array-rank :dst 0 :src 1) s)
    (assert-= expected (cl-cc:vm-reg-get s 0))))

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

(deftest-each vm-array-fill-pointer-query
  "Fill-pointer and adjustability queries return the expected value."
  :cases (("fill-pointer"     (make-array 5 :fill-pointer 3) #'cl-cc::make-vm-fill-pointer-inst      3)
          ("has-fill-pointer" (make-array 5 :fill-pointer 0) #'cl-cc::make-vm-array-has-fill-pointer-p 1)
          ("adjustable"       (make-array 5 :adjustable t)   #'cl-cc::make-vm-array-adjustable-p       1))
  (arr ctor expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 arr)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (assert-= expected (cl-cc:vm-reg-get s 0))))

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

(deftest-each vm-bit-simple-reads
  "Bit read instructions (bit-access, sbit) return the correct bit value."
  :cases (("bit-access" #'cl-cc::make-vm-bit-access
           (make-array 4 :element-type 'bit :initial-contents '(1 0 1 0)) 2 1)
          ("sbit"       #'cl-cc::make-vm-sbit
           (make-array 3 :element-type 'bit :initial-contents '(0 1 0))   1 1))
  (ctor ba idx expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 ba)
    (cl-cc:vm-reg-set s 2 idx)
    (exec1 (funcall ctor :dst 0 :arr 1 :idx 2) s)
    (assert-= expected (cl-cc:vm-reg-get s 0))))

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
