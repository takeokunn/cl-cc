;;;; tests/unit/vm/hash-tests.lisp — VM Hash Table Operations Unit Tests
;;;;
;;;; Tests for hash table instruction execution (create, get, set, remove,
;;;; count, keys, values, test, clear, predicate) via the VM.

(in-package :cl-cc/test)

(defsuite hash-suite
  :description "VM hash table operations unit tests"
  :parent cl-cc-unit-suite)

(in-suite hash-suite)

;;; ─── resolve-hash-test ────────────────────────────────────────────────────

(deftest-each resolve-hash-test-cases
  "resolve-hash-test maps symbols to correct test functions."
  :cases (("eq"     'eq     #'eq)
          ("eql"    'eql    #'eql)
          ("equal"  'equal  #'equal)
          ("equalp" 'equalp #'equalp)
          ("nil-defaults-eql" nil #'eql))
  (test-sym expected-fn)
  (assert-equal expected-fn (cl-cc/vm::resolve-hash-test test-sym)))

(deftest resolve-hash-test-unknown-errors
  "resolve-hash-test signals error for unknown test."
  (assert-true
   (handler-case
       (progn (cl-cc/vm::resolve-hash-test 'bogus) nil)
     (error () t))))

;;; ─── Hash Table Create ────────────────────────────────────────────────────

(deftest make-hash-table-default-test
  "vm-make-hash-table with no test creates an EQL hash table."
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (let ((obj (cl-cc/vm::vm-reg-get state :R0)))
      (assert-true (typep obj 'cl-cc/vm::vm-hash-table-object)))))

;;; ─── Hash Table Set/Get Round-Trip ────────────────────────────────────────

(deftest sethash-gethash-roundtrip
  "Setting and getting a hash table entry round-trips."
  (let ((state (make-test-vm)))
    ;; Create table in :R0
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    ;; Put key=42 in :R1, value=99 in :R2
    (cl-cc/vm::vm-reg-set state :R1 42)
    (cl-cc/vm::vm-reg-set state :R2 99)
    ;; sethash :R1 :R2 :R0
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    ;; gethash :R3 :R1 :R0
    (vm-exec (cl-cc:make-vm-gethash :dst :R3 :key :R1 :table :R0) state)
    (assert-equal 99 (cl-cc/vm::vm-reg-get state :R3))))

(deftest gethash-missing-key
  "Getting a missing key returns nil."
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (cl-cc/vm::vm-reg-set state :R1 'nonexistent)
    (vm-exec (cl-cc:make-vm-gethash :dst :R3 :key :R1 :table :R0) state)
    (assert-equal nil (cl-cc/vm::vm-reg-get state :R3))))

;;; ─── Hash Table Remove ────────────────────────────────────────────────────

(deftest remhash-removes-entry
  "vm-remhash removes an existing entry."
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (cl-cc/vm::vm-reg-set state :R1 'key)
    (cl-cc/vm::vm-reg-set state :R2 'val)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    (vm-exec (cl-cc:make-vm-remhash :key :R1 :table :R0) state)
    (vm-exec (cl-cc:make-vm-gethash :dst :R3 :key :R1 :table :R0) state)
    (assert-equal nil (cl-cc/vm::vm-reg-get state :R3))))

;;; ─── Hash Table Count ─────────────────────────────────────────────────────

(deftest hash-table-count-behavior
  "Hash table count is 0 when empty and reflects number of entries after inserts."
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (vm-exec (cl-cc:make-vm-hash-table-count :dst :R1 :table :R0) state)
    (assert-equal 0 (cl-cc/vm::vm-reg-get state :R1)))
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (cl-cc/vm::vm-reg-set state :R1 'a)
    (cl-cc/vm::vm-reg-set state :R2 1)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    (cl-cc/vm::vm-reg-set state :R1 'b)
    (cl-cc/vm::vm-reg-set state :R2 2)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    (vm-exec (cl-cc:make-vm-hash-table-count :dst :R3 :table :R0) state)
    (assert-equal 2 (cl-cc/vm::vm-reg-get state :R3))))

;;; ─── Hash Table Clear ─────────────────────────────────────────────────────

(deftest clrhash-empties-table
  "vm-clrhash removes all entries."
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (cl-cc/vm::vm-reg-set state :R1 'key)
    (cl-cc/vm::vm-reg-set state :R2 'val)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    (vm-exec (cl-cc:make-vm-clrhash :table :R0) state)
    (vm-exec (cl-cc:make-vm-hash-table-count :dst :R3 :table :R0) state)
    (assert-equal 0 (cl-cc/vm::vm-reg-get state :R3))))

;;; ─── Hash Table Predicate ─────────────────────────────────────────────────

(deftest-each hash-table-p
  "vm-hash-table-p returns 1 for hash table objects, 0 for non-hash-tables."
  :cases (("true"  t)
          ("false" nil))
  (is-ht)
  (let ((state (make-test-vm)))
    (if is-ht
        (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
        (cl-cc/vm::vm-reg-set state :R0 42))
    (vm-exec (cl-cc:make-vm-hash-table-p :dst :R1 :src :R0) state)
    (assert-equal (if is-ht 1 0) (cl-cc/vm::vm-reg-get state :R1))))

;;; ─── Hash Table Keys / Values ─────────────────────────────────────────────

(deftest hash-table-keys-and-values
  "vm-hash-table-keys and vm-hash-table-values each return a 2-element list with correct members."
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (cl-cc/vm::vm-reg-set state :R1 'x) (cl-cc/vm::vm-reg-set state :R2 10)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    (cl-cc/vm::vm-reg-set state :R1 'y) (cl-cc/vm::vm-reg-set state :R2 20)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    ;; keys
    (vm-exec (cl-cc:make-vm-hash-table-keys :dst :R3 :table :R0) state)
    (let ((keys (cl-cc/vm::vm-reg-get state :R3)))
      (assert-equal 2 (length keys))
      (assert-true (member 'x keys))
      (assert-true (member 'y keys)))
    ;; values
    (vm-exec (cl-cc:make-vm-hash-table-values :dst :R4 :table :R0) state)
    (let ((vals (cl-cc/vm::vm-reg-get state :R4)))
      (assert-equal 2 (length vals))
      (assert-true (member 10 vals))
      (assert-true (member 20 vals)))))

;;; ─── Hash Table Test ──────────────────────────────────────────────────────

(deftest hash-table-test-returns-symbol
  "vm-hash-table-test returns test function symbol."
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (vm-exec (cl-cc:make-vm-hash-table-test :dst :R1 :table :R0) state)
    (assert-equal 'eql (cl-cc/vm::vm-reg-get state :R1))))
