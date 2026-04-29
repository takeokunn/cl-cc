;;;; tests/unit/vm/vm-clos-tests.lisp — CLOS infrastructure + error dispatch unit tests

(in-package :cl-cc/test)

(defsuite vm-clos-suite
  :description "Unit tests for CLOS registry helpers and generic-dispatch utilities"
  :parent cl-cc-unit-suite)

(in-suite vm-clos-suite)

;;; Helpers — build a minimal class-registry hash table

(defun make-test-registry ()
  "Return a fresh empty registry (eq-keyed hash table)."
  (make-hash-table :test #'eq))

(defun registry-add-class (registry name &key (superclasses '()) (slots '()) (initargs '()))
  "Insert a minimal class entry into REGISTRY and return the class hash table."
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :__name__          ht) name
          (gethash :__superclasses__  ht) superclasses
          (gethash :__slots__         ht) slots
          (gethash :__initargs__      ht) initargs)
    (setf (gethash name registry) ht)
    ht))

;;; 1. collect-inherited-slots

(deftest collect-inherited-slots
  "collect-inherited-slots: no-super, single-super, multiple-supers, no-duplicates."
  (let ((reg (make-test-registry)))
    ;; no superclasses
    (assert-null (cl-cc/vm::collect-inherited-slots '() reg)))
  (let ((reg (make-test-registry)))
    ;; single superclass
    (registry-add-class reg 'animal :slots '(name age))
    (assert-equal '(name age)
                  (cl-cc/vm::collect-inherited-slots '(animal) reg)))
  (let ((reg (make-test-registry)))
    ;; multiple superclasses
    (registry-add-class reg 'flyable  :slots '(wingspan))
    (registry-add-class reg 'swimable :slots '(fin-count))
    (let ((result (cl-cc/vm::collect-inherited-slots '(flyable swimable) reg)))
      (assert-true (member 'wingspan  result))
      (assert-true (member 'fin-count result))))
  (let ((reg (make-test-registry)))
    ;; no duplicates
    (registry-add-class reg 'base  :slots '(id))
    (registry-add-class reg 'mixin :slots '(id extra))
    (let ((result (cl-cc/vm::collect-inherited-slots '(base mixin) reg)))
      (assert-= 2 (length result))
      (assert-true (member 'id    result))
      (assert-true (member 'extra result)))))

;;; 2. collect-inherited-initargs

(deftest collect-inherited-initargs
  "collect-inherited-initargs: no-super, single-super, multiple-supers."
  (let ((reg (make-test-registry)))
    ;; no superclasses
    (assert-null (cl-cc/vm::collect-inherited-initargs '() reg)))
  (let ((reg (make-test-registry)))
    ;; single superclass
    (registry-add-class reg 'person :initargs '((:name . name) (:age . age)))
    (let ((result (cl-cc/vm::collect-inherited-initargs '(person) reg)))
      (assert-true (assoc :name result))
      (assert-true (assoc :age  result))))
  (let ((reg (make-test-registry)))
    ;; multiple superclasses; first occurrence wins
    (registry-add-class reg 'named  :initargs '((:name . name)))
    (registry-add-class reg 'tagged :initargs '((:tag . tag)))
    (let ((result (cl-cc/vm::collect-inherited-initargs '(named tagged) reg)))
      (assert-true (assoc :name result))
      (assert-true (assoc :tag  result)))))

;;; 3. compute-class-precedence-list

(deftest compute-class-precedence-list
  "compute-class-precedence-list: no-super, single-super, linear-chain, diamond, C3-classic, branch."
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'root)
    (assert-equal '(root) (cl-cc/vm::compute-class-precedence-list 'root reg)))
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'animal)
    (registry-add-class reg 'dog :superclasses '(animal))
    (assert-equal '(dog animal) (cl-cc/vm::compute-class-precedence-list 'dog reg)))
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'c)
    (registry-add-class reg 'b :superclasses '(c))
    (registry-add-class reg 'a :superclasses '(b))
    (assert-equal '(a b c) (cl-cc/vm::compute-class-precedence-list 'a reg)))
  (let ((reg (make-test-registry)))
    ;; diamond: A -> B, A -> C, B -> D, C -> D
    (registry-add-class reg 'd)
    (registry-add-class reg 'b :superclasses '(d))
    (registry-add-class reg 'c :superclasses '(d))
    (registry-add-class reg 'a :superclasses '(b c))
    (let ((cpl (cl-cc/vm::compute-class-precedence-list 'a reg)))
      (assert-true (member 'a cpl))
      (assert-true (member 'b cpl))
      (assert-true (member 'c cpl))
      (assert-true (member 'd cpl))
      (assert-= (length cpl) (length (remove-duplicates cpl)))
      (assert-equal '(a b c d) cpl)))
  ;; C3 classic: D(B,C), B(A), C(A) — A appears once
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'a)
    (registry-add-class reg 'b :superclasses '(a))
    (registry-add-class reg 'c :superclasses '(a))
    (registry-add-class reg 'd :superclasses '(b c))
    (assert-equal '(d b c a) (cl-cc/vm::compute-class-precedence-list 'd reg)))
  ;; independent X(A,B) and Y(B,A) — no conflict when queried alone
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'a)
    (registry-add-class reg 'b)
    (registry-add-class reg 'x :superclasses '(a b))
    (registry-add-class reg 'y :superclasses '(b a))
    (assert-equal '(x a b) (cl-cc/vm::compute-class-precedence-list 'x reg))
    (assert-equal '(y b a) (cl-cc/vm::compute-class-precedence-list 'y reg)))
  ;; 3-level chain with branch: W(Y,X), Y(Z), X(Z)
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'z)
    (registry-add-class reg 'y :superclasses '(z))
    (registry-add-class reg 'x :superclasses '(z))
    (registry-add-class reg 'w :superclasses '(y x))
    (assert-equal '(w y x z) (cl-cc/vm::compute-class-precedence-list 'w reg))))

;;; 4. EQL specializer dispatch index

(deftest eql-specializer-dispatch-index
  "vm-register-method populates the EQL dispatch index for fast lookup."
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (gf (make-hash-table :test #'equal))
         (method 'read-method)
         (inst (cl-cc::make-vm-register-method
                :gf-reg :r0
                :specializer '(eql :read)
                :qualifier nil
                :method-reg :r1)))
    (setf (gethash :__methods__ gf) (make-hash-table :test #'equal)
          (gethash :__eql-index__ gf) (make-hash-table :test #'equal))
    (cl-cc:vm-reg-set state :r0 gf)
    (cl-cc:vm-reg-set state :r1 method)
    (cl-cc/vm::execute-instruction inst state 0 nil)
    (assert-equal (list method) (cl-cc/vm::%vm-gf-eql-methods gf :read))
    (assert-equal (list method) (gethash :read (gethash :__eql-index__ gf)))))

;;; 5. %cis-walk (extracted DFS helper for collect-inherited-slots)

(deftest cis-walk-cases
  "%cis-walk: empty list, single class, deduplication via seen."
  ;; empty class list leaves result-cell unchanged
  (let ((reg (make-test-registry))
        (seen (make-hash-table :test #'eq))
        (cell (list nil)))
    (cl-cc/vm::%cis-walk '() reg seen cell)
    (assert-null (car cell)))
  ;; single class accumulates slots into result-cell
  (let ((reg (make-test-registry))
        (seen (make-hash-table :test #'eq))
        (cell (list nil)))
    (registry-add-class reg 'thing :slots '(x y))
    (cl-cc/vm::%cis-walk '(thing) reg seen cell)
    (assert-true (member 'x (car cell)))
    (assert-true (member 'y (car cell))))
  ;; slot already in SEEN is not pushed again
  (let ((reg (make-test-registry))
        (seen (make-hash-table :test #'eq))
        (cell (list nil)))
    (setf (gethash 'id seen) t)
    (registry-add-class reg 'base :slots '(id extra))
    (cl-cc/vm::%cis-walk '(base) reg seen cell)
    (assert-false (member 'id    (car cell)))
    (assert-true  (member 'extra (car cell)))))

;;; 6. %cia-walk (extracted DFS helper for collect-inherited-initargs)

(deftest cia-walk-cases
  "%cia-walk: empty list, single class, no duplicate keys."
  ;; empty class list leaves result-cell unchanged
  (let ((reg (make-test-registry))
        (cell (list nil)))
    (cl-cc/vm::%cia-walk '() reg cell)
    (assert-null (car cell)))
  ;; single class accumulates initarg entries
  (let ((reg (make-test-registry))
        (cell (list nil)))
    (registry-add-class reg 'named :initargs '((:name . name)))
    (cl-cc/vm::%cia-walk '(named) reg cell)
    (assert-true (assoc :name (car cell))))
  ;; key already present in result-cell is not duplicated
  (let ((reg (make-test-registry))
        (cell (list (list '(:name . name-old)))))
    (registry-add-class reg 'alt :initargs '((:name . name-new) (:extra . extra)))
    (cl-cc/vm::%cia-walk '(alt) reg cell)
    (assert-= 1 (length (remove-if-not (lambda (e) (eq (car e) :name)) (car cell))))
    (assert-true (assoc :extra (car cell)))))

;;; 6b. %vm-allow-other-keys-p / %vm-validate-initargs

(deftest vm-allow-other-keys-p-cases
  "%vm-allow-other-keys-p: truthy value, nil value, absent key."
  ;; truthy register value → T
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 t)
    (assert-true (cl-cc/vm::%vm-allow-other-keys-p '((:allow-other-keys . :r0)) s)))
  ;; nil register value → NIL
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 nil)
    (assert-false (cl-cc/vm::%vm-allow-other-keys-p '((:allow-other-keys . :r0)) s)))
  ;; key absent from alist → NIL
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (assert-false (cl-cc/vm::%vm-allow-other-keys-p '((:x . :r0)) s))))

(deftest vm-validate-initargs-cases
  "%vm-validate-initargs: accepts known keys, signals for unknown, bypassed by allow-other-keys."
  ;; known key — no signal
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 42)
    (assert-true
     (progn
       (cl-cc/vm::%vm-validate-initargs '((:width . :r0)) '((:width . width)) s)
       t)))
  ;; unknown key — signals error
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 42)
    (assert-signals error
      (cl-cc/vm::%vm-validate-initargs '((:unknown-key . :r0)) '((:width . width)) s)))
  ;; :allow-other-keys truthy — bypasses check
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 42)
    (cl-cc:vm-reg-set s :r1 t)
    (assert-true
     (progn
       (cl-cc/vm::%vm-validate-initargs
        '((:unknown-key . :r0) (:allow-other-keys . :r1))
        '((:width . width))
        s)
       t))))

;;; 7. %cpl-linearize (extracted C3 linearization step)

(deftest cpl-linearize-cases
  "%cpl-linearize: unknown class, leaf class, linear A→B→C chain."
  (let ((reg (make-test-registry)))
    (assert-equal '(unknown) (cl-cc/vm::%cpl-linearize 'unknown reg)))
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'leaf)
    (assert-equal '(leaf) (cl-cc/vm::%cpl-linearize 'leaf reg)))
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'c)
    (registry-add-class reg 'b :superclasses '(c))
    (registry-add-class reg 'a :superclasses '(b))
    (assert-equal '(a b c) (cl-cc/vm::%cpl-linearize 'a reg))))
