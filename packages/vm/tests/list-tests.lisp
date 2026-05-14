;;;; tests/unit/vm/list-tests.lisp — VM List Instruction Tests
;;;
;;; Tests for execute-instruction on list operations, named accessors,
;;; destructive ops, association lists, coercions, arrays, and bit arrays.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── make-list ──────────────────────────────────────────────────────────────

(deftest vm-list-make-list
  "vm-make-list: N nils for size N; empty list for size 0."
  (with-test-vm (s (1 3))
    (exec1 (cl-cc:make-vm-make-list :dst 0 :size 1) s)
    (assert-equal '(nil nil nil) (cl-cc:vm-reg-get s 0)))
  (with-test-vm (s (1 0))
    (exec1 (cl-cc:make-vm-make-list :dst 0 :size 1) s)
    (assert-null (cl-cc:vm-reg-get s 0))))

;;; ─── length / reverse / append ──────────────────────────────────────────────

(deftest-each vm-list-unary-src-dst-ops
  "Unary list operations: src-in-reg-1 → dst-in-reg-0."
  :cases (("length"  #'cl-cc:make-vm-length  '(a b c d)  4)
          ("reverse" #'cl-cc:make-vm-reverse '(1 2 3)    '(3 2 1)))
  (constructor input expected)
  (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (assert-equal expected (cl-cc:vm-reg-get s 0))))

(deftest vm-list-append-two
  "vm-append concatenates two lists."
  (with-test-vm (s (1 '(a b)) (2 '(c d)))
    (exec1 (cl-cc:make-vm-append :dst 0 :src1 1 :src2 2) s)
    (assert-equal '(a b c d) (cl-cc:vm-reg-get s 0))))

;;; ─── member / nth / nthcdr ─────────────────────────────────────────────────

(deftest-each vm-list-member-hit-miss
  "vm-member returns tail on hit, nil on miss."
  :cases (("hit"  'b '(a b c) '(b c))
          ("miss" 'z '(a b c) nil))
  (item lst expected)
  (with-test-vm (s (1 item) (2 lst))
    (exec1 (cl-cc:make-vm-member :dst 0 :item 1 :list 2) s)
    (assert-equal expected (cl-cc:vm-reg-get s 0))))

(deftest-each vm-list-indexed-access-ops
  "vm-nth and vm-nthcdr fetch element and tail at index respectively."
  :cases (("nth"    #'cl-cc:make-vm-nth    'c
           (lambda (expected actual) (assert-eq expected actual)))
          ("nthcdr" #'cl-cc:make-vm-nthcdr '(c d)
           (lambda (expected actual) (assert-equal expected actual))))
  (constructor expected assert-fn)
  (with-test-vm (s (1 2) (2 '(a b c d)))
    (exec1 (funcall constructor :dst 0 :index 1 :list 2) s)
    (funcall assert-fn expected (cl-cc:vm-reg-get s 0))))

;;; ─── Named accessors ───────────────────────────────────────────────────────

(deftest-each vm-list-named-accessors
  "Named accessor instructions (first–fifth, rest, last, butlast) extract the correct element."
  :cases (("first"   #'cl-cc:make-vm-first   '(10 20 30 40 50) 10)
          ("second"  #'cl-cc:make-vm-second  '(10 20 30 40 50) 20)
          ("third"   #'cl-cc:make-vm-third   '(10 20 30 40 50) 30)
          ("fourth"  #'cl-cc:make-vm-fourth  '(10 20 30 40 50) 40)
          ("fifth"   #'cl-cc:make-vm-fifth   '(10 20 30 40 50) 50)
          ("rest"    #'cl-cc:make-vm-rest    '(a b c)          '(b c))
          ("last"    #'cl-cc:make-vm-last    '(a b c)          '(c))
          ("butlast" #'cl-cc:make-vm-butlast '(a b c)          '(a b)))
  (constructor input expected)
  (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (assert-equal expected (cl-cc:vm-reg-get s 0))))

;;; ─── Destructive + extended ops ─────────────────────────────────────────────

(deftest-each vm-list-extended-unary-ops
  "Destructive and extended unary list operations: src-in-reg-1 → dst-in-reg-0."
  :cases (("nreverse"    #'cl-cc:make-vm-nreverse    (list 1 2 3) '(3 2 1)
           (lambda (expected actual) (assert-equal expected actual)))
          ("list-length" #'cl-cc:make-vm-list-length '(x y z)     3
           (lambda (expected actual) (assert-= expected actual))))
  (constructor input expected assert-fn)
  (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (funcall assert-fn expected (cl-cc:vm-reg-get s 0))))

(deftest-each vm-list-empty-predicates
  "vm-endp and vm-null both detect the empty list."
  :cases (("endp/nil"       #'cl-cc:make-vm-endp nil  1)
          ("endp/non-empty" #'cl-cc:make-vm-endp '(a) 0)
          ("null/nil"       #'cl-cc:make-vm-null nil  1)
          ("null/non-nil"   #'cl-cc:make-vm-null 42   0))
  (constructor value expected)
  (with-test-vm (s (1 value))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (assert-= expected (cl-cc:vm-reg-get s 0))))

(deftest vm-list-push-and-pop
  "vm-push conses item onto list; vm-pop extracts car."
  (with-test-vm (s (1 'x) (2 '(a b)))
    (exec1 (cl-cc:make-vm-push :dst 0 :item 1 :list 2) s)
    (assert-equal '(x a b) (cl-cc:vm-reg-get s 0)))
  (with-test-vm (s (1 '(first second third)))
    (exec1 (cl-cc:make-vm-pop :dst 0 :list 1) s)
    (assert-eq 'first (cl-cc:vm-reg-get s 0))))

(deftest vm-cons-returns-fresh-cells
  "vm-cons follows CL CONS semantics and returns a fresh cell for each allocation."
  (let ((s (make-test-vm)))
    (cl-cc/vm::vm-clear-hash-cons-table)
    (cl-cc:vm-reg-set s 1 'a)
    (cl-cc:vm-reg-set s 2 'b)
    (exec1 (cl-cc:make-vm-cons :dst 0 :car-src 1 :cdr-src 2) s)
    (exec1 (cl-cc:make-vm-cons :dst 3 :car-src 1 :cdr-src 2) s)
    (assert-false (eq (cl-cc:vm-reg-get s 0)
                      (cl-cc:vm-reg-get s 3)))
    (assert-equal (cl-cc:vm-reg-get s 0)
                  (cl-cc:vm-reg-get s 3))))

(deftest-each vm-hash-cons-behavior
  "hash-cons: same cell for identical pairs; clearing forces fresh allocations."
  :cases (("reuses-identical"   nil)
          ("clear-breaks-reuse" t))
  (clear-between-p)
  (cl-cc/vm::vm-clear-hash-cons-table)
  (let ((c1 (cl-cc/vm::vm-hash-cons 'a 'b)))
    (when clear-between-p (cl-cc/vm::vm-clear-hash-cons-table))
    (let ((c2 (cl-cc/vm::vm-hash-cons 'a 'b)))
      (assert-equal c1 c2)
      (if clear-between-p
          (assert-false (eq c1 c2))
          (assert-true  (eq c1 c2))))))

(deftest vm-hash-cons-instruction-reuses-identical-flat-pairs
  "vm-hash-cons exposes explicit hash-consing without changing vm-cons freshness."
  (let ((s (make-test-vm)))
    (cl-cc/vm::vm-clear-hash-cons-table)
    (cl-cc:vm-reg-set s 1 'a)
    (cl-cc:vm-reg-set s 2 'b)
    (exec1 (cl-cc:make-vm-hash-cons :dst 0 :car-src 1 :cdr-src 2) s)
    (exec1 (cl-cc:make-vm-hash-cons :dst 3 :car-src 1 :cdr-src 2) s)
    (assert-true (eq (cl-cc:vm-reg-get s 0)
                     (cl-cc:vm-reg-get s 3)))
    (exec1 (cl-cc:make-vm-cons :dst 4 :car-src 1 :cdr-src 2) s)
    (assert-false (eq (cl-cc:vm-reg-get s 0)
                      (cl-cc:vm-reg-get s 4)))))

(deftest vm-hash-cons-reuses-structurally-equivalent-nested-trees
  "vm-hash-cons recursively canonicalizes nested cons trees."
  (cl-cc/vm::vm-clear-hash-cons-table)
  (let* ((left-1 (list 'a 'b))
         (right-1 (list 'c 'd))
         (left-2 (list 'a 'b))
         (right-2 (list 'c 'd))
         (t1 (cl-cc/vm::vm-hash-cons left-1 right-1))
         (t2 (cl-cc/vm::vm-hash-cons left-2 right-2)))
    (assert-true (eq t1 t2))
    (assert-true (eq (car t1) (car t2)))
    (assert-true (eq (cdr t1) (cdr t2)))))

(deftest vm-hash-cons-cyclic-input-does-not-overflow
  "vm-hash-cons handles cyclic inputs without recursion overflow."
  (cl-cc/vm::vm-clear-hash-cons-table)
  (let ((x (cons 'a nil)))
    (setf (cdr x) x)
    (let ((result (cl-cc/vm::vm-hash-cons x 'b)))
      (assert-true (consp result))
      (assert-eq 'b (cdr result)))))

(deftest vm-extensible-sequence-builtins
  "The partial sequence protocol works for list and vector builtins."
  (assert-equal 'b (cl-cc/vm::vm-sequence-elt '(a b c) 1))
  (assert-= 3 (cl-cc/vm::vm-sequence-length #(1 2 3)))
  (assert-equal '(x x) (cl-cc/vm::vm-make-sequence-like '(a) 2 :initial-element 'x))
  (assert-true (equalp #(1 2 0 0)
                       (cl-cc/vm::vm-adjust-sequence #(1 2) 4 :initial-element 0))))

(defclass test-sequence ()
  ((payload :initarg :payload :accessor test-sequence-payload)))

(defmethod cl-cc/vm::vm-sequence-elt ((sequence test-sequence) index)
  (aref (test-sequence-payload sequence) index))

(defmethod cl-cc/vm::vm-sequence-length ((sequence test-sequence))
  (length (test-sequence-payload sequence)))

(defmethod cl-cc/vm::vm-make-sequence-like ((sequence test-sequence) size &key (initial-element nil))
  (declare (ignore sequence))
  (make-instance 'test-sequence :payload (make-array size :initial-element initial-element)))

(deftest vm-extensible-sequence-user-extension
  "User-defined sequence types can extend the partial protocol via methods."
  (let* ((seq (make-instance 'test-sequence :payload #(10 20 30)))
         (like (cl-cc/vm::vm-make-sequence-like seq 2 :initial-element 7)))
    (assert-= 20 (cl-cc/vm::vm-sequence-elt seq 1))
    (assert-= 3 (cl-cc/vm::vm-sequence-length seq))
    (assert-= 2 (cl-cc/vm::vm-sequence-length like))
    (assert-= 7 (cl-cc/vm::vm-sequence-elt like 0))))

(deftest vm-instructions-use-extensible-sequence-protocol
  "vm-length and vm-nth dispatch through the extensible sequence protocol."
  (let ((s (make-test-vm))
        (seq (make-instance 'test-sequence :payload #(10 20 30))))
    (cl-cc:vm-reg-set s 1 seq)
    (cl-cc:vm-reg-set s 2 1)
    (exec1 (cl-cc:make-vm-length :dst 0 :src 1) s)
    (exec1 (cl-cc:make-vm-nth :dst 3 :index 2 :list 1) s)
    (assert-= 3 (cl-cc:vm-reg-get s 0))
    (assert-= 20 (cl-cc:vm-reg-get s 3))))
