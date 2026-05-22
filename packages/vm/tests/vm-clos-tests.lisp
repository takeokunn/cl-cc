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
         (inst (cl-cc:make-vm-register-method
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

(deftest cis-walk-empty-class-list-leaves-result-unchanged
  "%cis-walk on an empty class list leaves the result cell unchanged."
  (let ((reg (make-test-registry))
        (seen (make-hash-table :test #'eq))
        (cell (list nil)))
    (cl-cc/vm::%cis-walk '() reg seen cell)
    (assert-null (car cell))))

(deftest cis-walk-single-class-accumulates-slots
  "%cis-walk on a single class accumulates all its slots into the result cell."
  (let ((reg (make-test-registry))
        (seen (make-hash-table :test #'eq))
        (cell (list nil)))
    (registry-add-class reg 'thing :slots '(x y))
    (cl-cc/vm::%cis-walk '(thing) reg seen cell)
    (assert-true (member 'x (car cell)))
    (assert-true (member 'y (car cell)))))

(deftest cis-walk-deduplicates-slots-via-seen
  "%cis-walk skips slots already present in the seen hash table."
  (let ((reg (make-test-registry))
        (seen (make-hash-table :test #'eq))
        (cell (list nil)))
    (setf (gethash 'id seen) t)
    (registry-add-class reg 'base :slots '(id extra))
    (cl-cc/vm::%cis-walk '(base) reg seen cell)
    (assert-false (member 'id    (car cell)))
    (assert-true  (member 'extra (car cell)))))

;;; 6. %cia-walk (extracted DFS helper for collect-inherited-initargs)

(deftest cia-walk-empty-class-list-leaves-result-unchanged
  "%cia-walk on an empty class list leaves the result cell unchanged."
  (let ((reg (make-test-registry))
        (cell (list nil)))
    (cl-cc/vm::%cia-walk '() reg cell)
    (assert-null (car cell))))

(deftest cia-walk-single-class-accumulates-initargs
  "%cia-walk on a single class accumulates its initarg entries into the result cell."
  (let ((reg (make-test-registry))
        (cell (list nil)))
    (registry-add-class reg 'named :initargs '((:name . name)))
    (cl-cc/vm::%cia-walk '(named) reg cell)
    (assert-true (assoc :name (car cell)))))

(deftest cia-walk-does-not-duplicate-existing-keys
  "%cia-walk does not add a key already present in the result cell."
  (let ((reg (make-test-registry))
        (cell (list (list '(:name . name-old)))))
    (registry-add-class reg 'alt :initargs '((:name . name-new) (:extra . extra)))
    (cl-cc/vm::%cia-walk '(alt) reg cell)
    (assert-= 1 (length (remove-if-not (lambda (e) (eq (car e) :name)) (car cell))))
    (assert-true (assoc :extra (car cell)))))

;;; 6b. %vm-allow-other-keys-p / %vm-validate-initargs

(deftest vm-allow-other-keys-p-truthy-register-returns-true
  "%vm-allow-other-keys-p returns T when the :allow-other-keys register holds a truthy value."
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 t)
    (assert-true (cl-cc/vm::%vm-allow-other-keys-p '((:allow-other-keys . :r0)) s))))

(deftest vm-allow-other-keys-p-nil-register-returns-false
  "%vm-allow-other-keys-p returns NIL when the :allow-other-keys register holds nil."
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 nil)
    (assert-false (cl-cc/vm::%vm-allow-other-keys-p '((:allow-other-keys . :r0)) s))))

(deftest vm-allow-other-keys-p-absent-key-returns-false
  "%vm-allow-other-keys-p returns NIL when :allow-other-keys is absent from the alist."
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (assert-false (cl-cc/vm::%vm-allow-other-keys-p '((:x . :r0)) s))))

(deftest vm-validate-initargs-known-key-accepts-without-signal
  "%vm-validate-initargs accepts a known initarg key without signaling."
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 42)
    (assert-true
     (progn
       (cl-cc/vm::%vm-validate-initargs '((:width . :r0)) '((:width . width)) s)
       t))))

(deftest vm-validate-initargs-unknown-key-signals-error
  "%vm-validate-initargs signals an error for an unrecognized initarg key."
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 42)
    (assert-signals error
      (cl-cc/vm::%vm-validate-initargs '((:unknown-key . :r0)) '((:width . width)) s))))

(deftest vm-validate-initargs-allow-other-keys-bypasses-check
  "%vm-validate-initargs bypasses the unknown-key check when :allow-other-keys is truthy."
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

(deftest cpl-linearize-unknown-class-returns-singleton
  "%cpl-linearize on an unknown class returns a singleton list with that class name."
  (let ((reg (make-test-registry)))
    (assert-equal '(unknown) (cl-cc/vm::%cpl-linearize 'unknown reg))))

(deftest cpl-linearize-leaf-class-returns-singleton
  "%cpl-linearize on a registered leaf class returns a singleton list."
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'leaf)
    (assert-equal '(leaf) (cl-cc/vm::%cpl-linearize 'leaf reg))))

(deftest cpl-linearize-linear-inheritance-chain
  "%cpl-linearize on a linear A→B→C chain returns the MRO in order."
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'c)
    (registry-add-class reg 'b :superclasses '(c))
    (registry-add-class reg 'a :superclasses '(b))
    (assert-equal '(a b c) (cl-cc/vm::%cpl-linearize 'a reg))))

;;; FR-821: copy operations

(defclass vm-copy-host-object ()
  ((items :initarg :items :accessor vm-copy-host-object-items)
   (flag :initarg :flag :accessor vm-copy-host-object-flag)))

(deftest fr-821-copy-instance-host-standard-object
  "copy-instance copies host CLOS instances and preserves shallow slot values."
  (let* ((items (list 1 2 3))
         (object (make-instance 'vm-copy-host-object :items items :flag :original))
         (copy (cl-cc/vm::copy-instance object)))
    (setf (vm-copy-host-object-flag copy) :changed)
    (assert-false (eq object copy))
    (assert-eq items (vm-copy-host-object-items copy))
    (assert-eq :original (vm-copy-host-object-flag object))
    (assert-eq :changed (vm-copy-host-object-flag copy))))

(deftest fr-821-deep-copy-recursive-conses-and-vectors
  "deep-copy recursively copies nested mutable containers."
  (let* ((nested (vector (list :a :b)))
         (copy (cl-cc/vm::deep-copy nested)))
    (setf (car (aref nested 0)) :changed)
    (assert-false (eq nested copy))
    (assert-false (eq (aref nested 0) (aref copy 0)))
    (assert-eq :a (car (aref copy 0)))))

;;; FR-838: extensible sequence protocol

(defclass vm-test-deque (cl-cc/vm::sequence)
  ((storage :initarg :storage :accessor vm-test-deque-storage)))

(defmethod cl-cc/vm::length ((sequence vm-test-deque))
  (length (vm-test-deque-storage sequence)))

(defmethod cl-cc/vm::elt ((sequence vm-test-deque) index)
  (aref (vm-test-deque-storage sequence) index))

(defmethod (setf cl-cc/vm::elt) (value (sequence vm-test-deque) index)
  (setf (aref (vm-test-deque-storage sequence) index) value))

(defmethod cl-cc/vm::make-sequence-like ((sequence vm-test-deque) size &key initial-element)
  (declare (ignore sequence))
  (make-instance 'vm-test-deque
                 :storage (make-array size :initial-element initial-element)))

(deftest fr-838-extensible-sequence-subseq-and-setf-elt
  "User subclasses of cl-cc/vm::sequence participate in length/elt/subseq."
  (let* ((deque (make-instance 'vm-test-deque :storage (vector 10 20 30 40)))
         (slice (cl-cc/vm::subseq deque 1 3)))
    (setf (cl-cc/vm::elt deque 2) 99)
    (assert-true (cl-cc/vm::sequence-protocol-p deque))
    (assert-= 4 (cl-cc/vm::length deque))
    (assert-= 99 (cl-cc/vm::elt deque 2))
    (assert-= 2 (cl-cc/vm::length slice))
    (assert-= 20 (cl-cc/vm::elt slice 0))
    (assert-= 30 (cl-cc/vm::elt slice 1))))

;;; 8. FR-888 allocate-instance fast path

(deftest fr-888-finalize-class-builds-slot-vector-index
  "finalize-class-allocation-cache builds an O(1) slot-name→vector-index cache."
  (let ((class (registry-add-class (make-test-registry) 'point :slots '(x y))))
    (cl-cc/vm::finalize-class-allocation-cache class)
    (assert-= 1 (cl-cc/vm::class-slot-vector-index class 'x))
    (assert-= 2 (cl-cc/vm::class-slot-vector-index class 'y))
    (assert-null (cl-cc/vm::class-slot-vector-index class 'z))))

(deftest fr-888-allocate-instance-vector-uses-class-header-and-class-id
  "allocate-instance-vector creates vector storage with a class header and class-id tag."
  (let* ((class (registry-add-class (make-test-registry) 'point :slots '(x y)))
         (instance (progn
                     (cl-cc/vm::finalize-class-allocation-cache class)
                     (cl-cc/vm::allocate-instance-vector class))))
    (assert-true (vectorp instance))
    (assert-eq class (aref instance 0))
    (assert-= 3 (length instance))
    (assert-true (integerp (gethash :__class-id__ class)))
    (assert-= (gethash :__class-id__ class)
              (cl-cc/vm::%vm-vector-instance-class-id instance))))

(deftest fr-888-slot-value-by-index-reads-and-writes-in-constant-time
  "slot-value-by-index and its SETF writer operate directly on vector indexes."
  (let* ((class (registry-add-class (make-test-registry) 'point :slots '(x y)))
         (instance (progn
                     (cl-cc/vm::finalize-class-allocation-cache class)
                     (cl-cc/vm::allocate-instance-vector class)))
         (x-index (cl-cc/vm::class-slot-vector-index class 'x)))
    (setf (cl-cc/vm::slot-value-by-index instance x-index) 42)
    (assert-= 42 (cl-cc/vm::slot-value-by-index instance x-index))))

(deftest fr-888-dynamic-layout-falls-back-from-vector-allocation
  "Dynamic slot layout metadata prevents vector fast-path allocation."
  (let ((class (registry-add-class (make-test-registry) 'dynamic :slots '(x))))
    (setf (gethash :__dynamic-slot-layout__ class) t)
    (assert-signals error
      (cl-cc/vm::allocate-instance-vector class))))

;;; 9. FR-889 default-initargs / make-instance caching

(deftest fr-889-finalize-caches-evaluated-default-initargs
  "finalize-class-allocation-cache records the evaluated default-initargs list."
  (let ((class (registry-add-class (make-test-registry)
                                   'shape
                                   :slots '(color radius)
                                   :initargs '((:color . color) (:radius . radius)))))
    (setf (gethash :__default-initargs__ class) '((:color . :red) (:radius . 5)))
    (cl-cc/vm::finalize-class-allocation-cache class)
    (assert-equal '((:color . :red) (:radius . 5))
                  (gethash :__cached-default-initargs__ class))))

(deftest fr-889-merge-cached-defaults-preserves-explicit-initargs
  "merge-cached-default-initargs applies defaults while preserving explicit keys."
  (let ((merged (cl-cc/vm::merge-cached-default-initargs
                 '((:color . :red) (:radius . 5))
                 '((:radius . 9) (:name . circle)))))
    (assert-equal '((:radius . 9) (:name . circle) (:color . :red)) merged)))

(deftest fr-889-zero-arg-template-copy-produces-fresh-vector
  "The zero-arg shortcut copies a cached template instead of reusing it."
  (let* ((class (registry-add-class (make-test-registry) 'point :slots '(x)))
         (first nil)
         (second nil))
    (cl-cc/vm::finalize-class-allocation-cache class)
    (setf first (cl-cc/vm::%vm-copy-instance-template class)
          second (cl-cc/vm::%vm-copy-instance-template class))
    (assert-false (eq first second))
    (setf (cl-cc/vm::slot-value-by-index first 1) 10)
    (assert-null (cl-cc/vm::slot-value-by-index second 1))))

(deftest fr-889-make-instance-cache-specializes-by-initarg-signature
  "Constructor cache keys on the class and initarg signature, not values."
  (let ((class (registry-add-class (make-test-registry) 'point :slots '(x y))))
    (cl-cc/vm::finalize-class-allocation-cache class)
    (assert-eq :zero-arg-template
               (cl-cc/vm::%vm-cached-constructor-path class nil))
    (assert-eq :standard-vector
               (cl-cc/vm::%vm-cached-constructor-path class '((:x . :r0))))
    (assert-eq :standard-vector
               (cl-cc/vm::%vm-cached-constructor-path class '((:x . :r1))))
    (assert-= 2 (hash-table-count (gethash :__make-instance-cache__ class)))))
