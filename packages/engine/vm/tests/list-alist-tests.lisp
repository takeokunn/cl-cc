;;;; tests/unit/vm/list-alist-tests.lisp — VM Alist, Copy, Predicate, and PC Tests
;;;
;;; Tests for vm-assoc, vm-acons, vm-equal, vm-nconc, vm-copy-list/tree,
;;; vm-subst, vm-listp, vm-atom, and PC advancement for list instructions.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Association lists ──────────────────────────────────────────────────────

(deftest-each vm-list-assoc-hit-miss
  "vm-assoc returns matching pair on hit; nil on miss."
  :cases (("hit"  'b '((a . 1) (b . 2) (c . 3)) '(b . 2))
          ("miss" 'z '((a . 1))                  nil))
  (key alist expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 key)
    (cl-cc:vm-reg-set s 2 alist)
    (exec1 (cl-cc::make-vm-assoc :dst 0 :key 1 :alist 2) s)
    (assert-equal expected (cl-cc:vm-reg-get s 0))))

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

(deftest-each vm-list-equal-cases
  "vm-equal: truthy for structurally equal trees; nil for different trees."
  :cases (("equal"     '(a (b c)) '(a (b c)) t)
          ("not-equal" '(a b)     '(a c)     nil))
  (lhs rhs expected-bool)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (cl-cc::make-vm-equal :dst 0 :lhs 1 :rhs 2) s)
    (assert-equal expected-bool (and (cl-cc:vm-reg-get s 0) t))))

(deftest vm-list-nconc-joins
  "vm-nconc destructively concatenates two lists."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (list 'a 'b))
    (cl-cc:vm-reg-set s 2 (list 'c 'd))
    (exec1 (cl-cc::make-vm-nconc :dst 0 :lhs 1 :rhs 2) s)
    (assert-equal '(a b c d) (cl-cc:vm-reg-get s 0))))

(deftest-each vm-list-copy-ops
  "copy-list makes a shallow copy (outer distinct, nested refs shared); copy-tree makes deep copy."
  :cases (("copy-list" #'cl-cc::make-vm-copy-list (list 1 2 3)                nil)
          ("copy-tree" #'cl-cc::make-vm-copy-tree (list (list 1 2) (list 3 4)) t))
  (constructor orig deep-p)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 orig)
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (let ((copy (cl-cc:vm-reg-get s 0)))
      (assert-equal orig copy)
      (assert-true (not (eq orig copy)))
      (when deep-p
        (assert-true (not (eq (first orig) (first copy))))))))

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
