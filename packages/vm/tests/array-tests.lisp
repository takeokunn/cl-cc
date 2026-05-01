;;;; tests/unit/vm/array-tests.lisp — VM Array and Bit Array Instruction Tests
;;;;
;;;; Tests for array, vector, and bit-array instructions executed via the VM.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Array operations ──────────────────────────────────────────────────────

(deftest vm-array-make-creates-array-of-given-size
  "vm-make-array creates an array with the length stored in the size register."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 5)
    (exec1 (cl-cc::make-vm-make-array :dst 0 :size-reg 1) s)
    (let ((arr (cl-cc:vm-reg-get s 0)))
      (assert-true (arrayp arr))
      (assert-= 5 (length arr)))))

(deftest vm-array-make-with-initial-element-fills-all-slots
  "vm-make-array :initial-element fills every slot with the given value."
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
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 1)
    (cl-cc:vm-reg-set s 3 99)
    (exec1 (cl-cc::make-vm-aset :array-reg 1 :index-reg 2 :val-reg 3) s)
    (exec1 (cl-cc::make-vm-aref :dst 0 :array-reg 1 :index-reg 2) s)
    (assert-= 99 (cl-cc:vm-reg-get s 0))))

(deftest vm-array-vector-push-extend-grows
  "vm-vector-push-extend pushes onto adjustable vector."
  (let ((s (make-test-vm))
        (v (make-array 2 :fill-pointer 0 :adjustable t)))
    (cl-cc:vm-reg-set s 1 'hello)
    (cl-cc:vm-reg-set s 2 v)
    (exec1 (cl-cc::make-vm-vector-push-extend :dst 0 :val-reg 1 :array-reg 2) s)
    (assert-= 0 (cl-cc:vm-reg-get s 0))
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

(deftest vm-array-dim-queries
  "vm-array-total-size, vm-array-dimensions, vm-array-dimension return correct values."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (make-array '(3 4)))
    (exec1 (cl-cc::make-vm-array-total-size :dst 0 :src 1) s)
    (assert-= 12 (cl-cc:vm-reg-get s 0)))
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (make-array '(2 5)))
    (exec1 (cl-cc::make-vm-array-dimensions :dst 0 :src 1) s)
    (assert-equal '(2 5) (cl-cc:vm-reg-get s 0)))
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (make-array '(3 7)))
    (cl-cc:vm-reg-set s 2 1)
    (exec1 (cl-cc::make-vm-array-dimension :dst 0 :lhs 1 :rhs 2) s)
    (assert-= 7 (cl-cc:vm-reg-get s 0))))

;;; ─── Row-major access ───────────────────────────────────────────────────────

(deftest vm-array-row-major-operations
  "vm-row-major-aref accesses by flat index; vm-array-row-major-index computes flat index."
  (let ((s   (make-test-vm))
        (arr (make-array '(2 3) :initial-contents '((10 20 30) (40 50 60)))))
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 4)
    (exec1 (cl-cc::make-vm-row-major-aref :dst 0 :lhs 1 :rhs 2) s)
    (assert-= 50 (cl-cc:vm-reg-get s 0)))
  (let ((s   (make-test-vm))
        (arr (make-array '(2 3))))
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 '(1 2))
    (exec1 (cl-cc::make-vm-array-row-major-index :dst 0 :arr 1 :subs 2) s)
    (assert-= 5 (cl-cc:vm-reg-get s 0))))

;;; ─── svref ──────────────────────────────────────────────────────────────────

(deftest vm-array-svref-and-svset
  "vm-svref reads from a simple-vector; vm-svset writes and returns new value."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 #(a b c))
    (cl-cc:vm-reg-set s 2 1)
    (exec1 (cl-cc::make-vm-svref :dst 0 :lhs 1 :rhs 2) s)
    (assert-eq 'b (cl-cc:vm-reg-get s 0)))
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

(deftest vm-array-vector-push-and-pop
  "vm-vector-push pushes and returns new index; vm-vector-pop returns and removes last element."
  (let ((s (make-test-vm))
        (v (make-array 5 :fill-pointer 0)))
    (cl-cc:vm-reg-set s 1 'x)
    (cl-cc:vm-reg-set s 2 v)
    (exec1 (cl-cc::make-vm-vector-push :dst 0 :val-reg 1 :array-reg 2) s)
    (assert-= 0 (cl-cc:vm-reg-get s 0))
    (assert-= 1 (fill-pointer v)))
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
    (cl-cc:vm-reg-set s 2 1)
    (cl-cc:vm-reg-set s 3 1)
    (exec1 (cl-cc::make-vm-bit-set :dst 0 :arr 1 :idx 2 :val 3) s)
    (assert-= 1 (bit ba 1))
    (assert-= 1 (cl-cc:vm-reg-get s 0))))

(deftest vm-bit-and-elementwise
  "vm-bit-and computes element-wise AND: #*1100 AND #*1010 = #*1000."
  (let ((s (make-test-vm))
        (a (make-array 4 :element-type 'bit :initial-contents '(1 1 0 0)))
        (b (make-array 4 :element-type 'bit :initial-contents '(1 0 1 0))))
    (cl-cc:vm-reg-set s 1 a)
    (cl-cc:vm-reg-set s 2 b)
    (exec1 (cl-cc::make-vm-bit-and :dst 0 :lhs 1 :rhs 2) s)
    (let ((result (cl-cc:vm-reg-get s 0)))
      (loop for (idx expected) in '((0 1) (1 0) (2 0) (3 0))
            do (assert-= expected (bit result idx))))))

(deftest vm-bit-or-elementwise
  "vm-bit-or ORs two bit arrays element-wise: #*1100 OR #*1010 -> #*1110."
  (let ((s (make-test-vm))
        (a (make-array 4 :element-type 'bit :initial-contents '(1 1 0 0)))
        (b (make-array 4 :element-type 'bit :initial-contents '(1 0 1 0))))
    (cl-cc:vm-reg-set s 1 a)
    (cl-cc:vm-reg-set s 2 b)
    (exec1 (cl-cc::make-vm-bit-or :dst 0 :lhs 1 :rhs 2) s)
    (let ((result (cl-cc:vm-reg-get s 0)))
      (loop for (idx expected) in '((0 1) (1 1) (2 1) (3 0))
            do (assert-= expected (bit result idx))))))

(deftest vm-bit-not-inverts
  "vm-bit-not inverts all bits: #*1010 -> #*0101."
  (let ((s (make-test-vm))
        (a (make-array 4 :element-type 'bit :initial-contents '(1 0 1 0))))
    (cl-cc:vm-reg-set s 1 a)
    (exec1 (cl-cc::make-vm-bit-not :dst 0 :src 1) s)
    (let ((result (cl-cc:vm-reg-get s 0)))
      (loop for (idx expected) in '((0 0) (1 1) (2 0) (3 1))
            do (assert-= expected (bit result idx))))))

;;; ─── adjust-array / array-displacement ──────────────────────────────────────

(deftest vm-adjust-array-grows-array-to-new-size
  "vm-adjust-array changes the length of an adjustable array to the value in the dims register."
  (let ((s (make-test-vm))
        (arr (make-array 3 :initial-element 0 :adjustable t)))
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 5)
    (exec1 (cl-cc::make-vm-adjust-array :dst 0 :arr 1 :dims 2) s)
    (assert-= 5 (length (cl-cc:vm-reg-get s 0)))))

(deftest vm-array-displacement-returns-nil-for-non-displaced
  "vm-array-displacement returns NIL for a simple (non-displaced) array."
  (let ((s (make-test-vm))
        (arr (make-array 3)))
    (cl-cc:vm-reg-set s 1 arr)
    (exec1 (cl-cc::make-vm-array-displacement :dst 0 :src 1) s)
    (assert-null (cl-cc:vm-reg-get s 0))))
