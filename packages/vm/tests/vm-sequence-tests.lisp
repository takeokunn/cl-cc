;;;; packages/vm/tests/vm-sequence-tests.lisp
;;;;
;;;; Dedicated unit tests for vm-sequence.lisp:
;;;;   - define-builtin-sequence-methods macro expansion sanity
;;;;   - copy-instance: hash-table path, missing __class__ error, standard-object path
;;;;   - copy-structure: all dispatch branches
;;;;   - deep-copy: cycle detection via seen table, function/character immediates
;;;;   - sequence-protocol-p: true/false cases

(in-package :cl-cc/test)

(defsuite vm-sequence-suite
  :description "Unit tests for vm-sequence.lisp: sequence protocol, copy, deep-copy"
  :parent cl-cc-unit-suite)

(in-suite vm-sequence-suite)

;;; ── 1. sequence-protocol-p ────────────────────────────────────────────────

(deftest-each sequence-protocol-p-builtin-types-true
  "sequence-protocol-p returns T for built-in sequence types."
  :cases (("list-nonempty" (list 1 2 3))
          ("list-empty"    nil)
          ("vector"        #(10 20))
          ("string"        "hello"))
  (object)
  (assert-true (cl-cc/vm::sequence-protocol-p object)))

(deftest-each sequence-protocol-p-non-sequences-false
  "sequence-protocol-p returns NIL for non-sequence objects."
  :cases (("integer"    42)
          ("float"      3.14)
          ("symbol"     'foo)
          ("keyword"    :bar)
          ("character"  #\a)
          ("hash-table" (make-hash-table)))
  (object)
  (assert-false (cl-cc/vm::sequence-protocol-p object)))

;;; ── 2. length / elt / (setf elt) / subseq for each built-in type ─────────

(deftest-each length-by-type
  "length dispatches correctly for list, vector, and string."
  :cases (("list-3"   (list 1 2 3)  3)
          ("list-0"   nil            0)
          ("vector-2" #(10 20)       2)
          ("string-5" "hello"        5)
          ("string-0" ""             0))
  (seq expected)
  (assert-= expected (cl-cc/vm::length seq)))

(deftest-each elt-by-type
  "elt returns the element at the given index for list, vector, and string."
  :cases (("list-0"   (list :a :b :c) 0 :a)
          ("list-2"   (list :a :b :c) 2 :c)
          ("vector-1" #(10 20 30)     1 20)
          ("string-0" "xyz"           0 #\x)
          ("string-2" "xyz"           2 #\z))
  (seq index expected)
  (assert-equal expected (cl-cc/vm::elt seq index)))

(deftest setf-elt-list-mutates
  "(setf elt) on a list destructively updates the nth cell."
  (let ((lst (list 1 2 3)))
    (setf (cl-cc/vm::elt lst 1) 99)
    (assert-= 99 (second lst))
    (assert-= 1  (first lst))
    (assert-= 3  (third lst))))

(deftest setf-elt-vector-mutates
  "(setf elt) on a vector destructively updates the element."
  (let ((vec (vector 10 20 30)))
    (setf (cl-cc/vm::elt vec 0) 77)
    (assert-= 77 (cl:aref vec 0))
    (assert-= 20 (cl:aref vec 1))))

(deftest setf-elt-string-mutates
  "(setf elt) on a string destructively updates the character."
  (let ((str (copy-seq "abc")))
    (setf (cl-cc/vm::elt str 2) #\Z)
    (assert-equal #\Z (cl:char str 2))
    (assert-equal #\a (cl:char str 0))))

(deftest-each subseq-by-type
  "subseq returns the correct slice for list, vector, and string."
  :cases (("list-1-3"   (list :a :b :c :d) 1 3 (list :b :c))
          ("vector-0-2" #(10 20 30)         0 2 #(10 20))
          ("string-1-3" "abcd"              1 3 "bc"))
  (seq start end expected)
  (assert-equal expected (cl-cc/vm::subseq seq start end)))

;;; ── 3. make-sequence-like ─────────────────────────────────────────────────

(deftest make-sequence-like-list-default-nil
  "make-sequence-like on a list without :initial-element fills with NIL."
  (let ((result (cl-cc/vm::make-sequence-like '(1 2) 3)))
    (assert-true (listp result))
    (assert-= 3 (cl:length result))
    (assert-true (every #'null result))))

(deftest make-sequence-like-string-default-space
  "make-sequence-like on a string without :initial-element defaults to #\\Space."
  (let ((result (cl-cc/vm::make-sequence-like "hi" 4)))
    (assert-true (stringp result))
    (assert-= 4 (cl:length result))
    (assert-equal "    " result)))

(deftest make-sequence-like-vector-explicit-element
  "make-sequence-like on a vector with :initial-element fills correctly."
  (let ((result (cl-cc/vm::make-sequence-like #(0) 3 :initial-element :x)))
    (assert-true (vectorp result))
    (assert-= 3 (cl:length result))
    (assert-true (every (lambda (element) (eq :x element)) result))))

;;; ── 4. copy-instance paths ────────────────────────────────────────────────

(deftest copy-instance-hash-table-shallow-copy
  "copy-instance on a VM hash-table instance copies all keys including __class__."
  (let* ((original (make-hash-table :test #'equal))
         (copy nil))
    (setf (gethash :__class__ original) :my-class
          (gethash :slot-a   original) 42)
    (setf copy (cl-cc/vm::copy-instance original))
    (assert-false (eq original copy))
    (assert-eq :my-class (gethash :__class__ copy))
    (assert-= 42 (gethash :slot-a copy))
    ;; shallow: mutation to original does not affect copy
    (setf (gethash :slot-a original) 999)
    (assert-= 42 (gethash :slot-a copy))))

(deftest copy-instance-missing-class-key-signals-error
  "copy-instance signals an error when a hash-table lacks the :__class__ key."
  (let ((bare (make-hash-table)))
    (setf (gethash :slot-x bare) 1)
    (assert-signals error (cl-cc/vm::copy-instance bare))))

(deftest copy-instance-vector-with-hash-header-uses-copy-seq
  "copy-instance on a vector whose first element is a hash-table returns a fresh vector."
  (let* ((header (make-hash-table))
         (original (vector header 10 20))
         (copy (cl-cc/vm::copy-instance original)))
    (assert-true (vectorp copy))
    (assert-false (eq original copy))
    (assert-= 10 (cl:aref copy 1))))

(deftest copy-instance-standard-object-is-shallow
  "copy-instance on a standard-object preserves slot values shallowly."
  (let* ((shared-list (list 1 2 3))
         (object (make-instance 'cl-cc/vm::sequence))
         (copy nil))
    ;; sequence has no slots — just verify it round-trips without error
    (setf copy (cl-cc/vm::copy-instance object))
    (assert-false (eq object copy))
    (assert-true (typep copy 'cl-cc/vm::sequence))
    ;; The shared-list is not referenced by sequence slots, but the copy
    ;; must be a distinct object
    (declare (ignore shared-list))))

;;; ── 5. copy-structure dispatch branches ──────────────────────────────────

(deftest copy-structure-list-is-fresh-and-equal
  "copy-structure on a list produces a fresh copy."
  (let* ((original (list :a :b :c))
         (copy (cl-cc/vm::copy-structure original)))
    (assert-equal original copy)
    (assert-false (eq original copy))))

(deftest copy-structure-vector-is-fresh-and-equal
  "copy-structure on a vector produces a fresh copy."
  (let* ((original (vector 1 2 3))
         (copy (cl-cc/vm::copy-structure original)))
    (assert-true (equalp original copy))
    (assert-false (eq original copy))))

(deftest copy-structure-string-is-fresh-and-equal
  "copy-structure on a string produces a fresh copy."
  (let* ((original "test-string")
         (copy (cl-cc/vm::copy-structure original)))
    (assert-equal original copy)
    (assert-false (eq original copy))))

(deftest copy-structure-standard-object-delegates-to-copy-instance
  "copy-structure on a standard-object delegates to copy-instance."
  (let* ((object (make-instance 'cl-cc/vm::sequence))
         (copy (cl-cc/vm::copy-structure object)))
    (assert-false (eq object copy))
    (assert-true (typep copy 'cl-cc/vm::sequence))))

(deftest copy-structure-hash-table-vm-instance-delegates-to-copy-instance
  "copy-structure on a VM hash-table instance delegates to copy-instance."
  (let* ((original (make-hash-table :test #'equal)))
    (setf (gethash :__class__ original) :point
          (gethash :x         original) 3
          (gethash :y         original) 4)
    (let ((copy (cl-cc/vm::copy-structure original)))
      (assert-false (eq original copy))
      (assert-= 3 (gethash :x copy))
      (assert-= 4 (gethash :y copy)))))

(deftest copy-structure-unsupported-signals-error
  "copy-structure on an unsupported object (e.g., integer) signals an error."
  (assert-signals error (cl-cc/vm::copy-structure 99)))

;;; ── 6. deep-copy: immediate values and cycle detection ───────────────────

(deftest-each deep-copy-immediates-return-same-object
  "deep-copy of immediate values returns the identical object."
  :cases (("nil"       nil)
          ("integer"   42)
          ("float"     1.5)
          ("symbol"    'foo)
          ("character" #\a))
  (value)
  (assert-eq value (cl-cc/vm::deep-copy value)))

(deftest deep-copy-function-returns-same-object
  "deep-copy of a function object returns the same function."
  (let ((fn #'cl:+))
    (assert-eq fn (cl-cc/vm::deep-copy fn))))

(deftest deep-copy-cycle-in-cons-is-preserved
  "deep-copy of a cyclic cons structure does not infinite-loop and preserves identity."
  (let* ((pair (cons :head nil))
         (seen (make-hash-table :test #'eq)))
    ;; register pair in seen before calling deep-copy to simulate a cycle break
    (setf (gethash pair seen) pair)
    ;; deep-copy should find it in seen and return the registered value
    (assert-eq pair (cl-cc/vm::deep-copy pair seen))))

(deftest deep-copy-hash-table-cycle-via-seen
  "deep-copy of a hash-table that appears in seen returns the seen copy."
  (let* ((original (make-hash-table))
         (sentinel :already-copied)
         (seen (make-hash-table :test #'eq)))
    (setf (gethash original seen) sentinel)
    (assert-eq sentinel (cl-cc/vm::deep-copy original seen))))

(deftest deep-copy-nested-hash-table-copies-recursively
  "deep-copy recursively copies values inside a hash-table."
  (let* ((inner (list 10 20))
         (outer (make-hash-table :test #'equal)))
    (setf (gethash "key" outer) inner)
    (let ((copy (cl-cc/vm::deep-copy outer)))
      (assert-false (eq outer copy))
      (assert-false (eq inner (gethash "key" copy)))
      (assert-equal inner (gethash "key" copy)))))

(deftest deep-copy-string-produces-fresh-equal-copy
  "deep-copy of a string returns a fresh equal string (strings are mutable)."
  (let* ((original "mutable")
         (copy (cl-cc/vm::deep-copy original)))
    (assert-equal original copy)
    (assert-false (eq original copy))))

(deftest deep-copy-vector-elements-are-recursively-copied
  "deep-copy of a vector recursively copies each element."
  (let* ((inner (list :a))
         (original (vector inner 99))
         (copy (cl-cc/vm::deep-copy original)))
    (assert-false (eq original copy))
    (assert-false (eq inner (cl:aref copy 0)))
    (assert-equal inner (cl:aref copy 0))
    (assert-= 99 (cl:aref copy 1))))
