;;;; tests/unit/vm/list-tests.lisp — VM List Instruction Tests
;;;
;;; Tests for execute-instruction on list operations, named accessors,
;;; destructive ops, association lists, coercions, arrays, and bit arrays.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

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

(deftest vm-cons-uses-hash-cons-sharing
  "vm-cons now routes through vm-hash-cons and reuses identical cells."
  (let ((s (make-test-vm)))
    (cl-cc::vm-clear-hash-cons-table)
    (cl-cc:vm-reg-set s 1 'a)
    (cl-cc:vm-reg-set s 2 'b)
    (exec1 (cl-cc::make-vm-cons :dst 0 :car-src 1 :cdr-src 2) s)
    (exec1 (cl-cc::make-vm-cons :dst 3 :car-src 1 :cdr-src 2) s)
    (assert-true (eq (cl-cc:vm-reg-get s 0)
                     (cl-cc:vm-reg-get s 3)))))

(deftest vm-hash-cons-reuses-identical-cells
  "vm-hash-cons returns the same cons cell for identical car/cdr pairs."
  (cl-cc::vm-clear-hash-cons-table)
  (let ((c1 (cl-cc::vm-hash-cons 'a 'b))
        (c2 (cl-cc::vm-hash-cons 'a 'b)))
    (assert-true (eq c1 c2))
    (assert-equal '(a . b) c1)))

(deftest vm-hash-cons-clear-drops-interning
  "Clearing the hash-cons table forces subsequent allocations to be distinct."
  (cl-cc::vm-clear-hash-cons-table)
  (let ((c1 (cl-cc::vm-hash-cons 'x 'y)))
    (cl-cc::vm-clear-hash-cons-table)
    (let ((c2 (cl-cc::vm-hash-cons 'x 'y)))
      (assert-false (eq c1 c2))
      (assert-equal c1 c2))))

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
