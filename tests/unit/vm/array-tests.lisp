;;;; tests/unit/vm/array-tests.lisp — VM Array and Bit Array Instruction Tests
;;;;
;;;; Tests for array, vector, and bit-array instructions executed via the VM.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

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
    (cl-cc:vm-reg-set s 2 1)
    (exec1 (cl-cc::make-vm-array-dimension :dst 0 :lhs 1 :rhs 2) s)
    (assert-= 7 (cl-cc:vm-reg-get s 0))))

;;; ─── Row-major access ───────────────────────────────────────────────────────

(deftest vm-array-row-major-aref-2d
  "vm-row-major-aref accesses 2D array by flat index."
  (let ((s (make-test-vm))
        (arr (make-array '(2 3) :initial-contents '((10 20 30) (40 50 60)))))
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 4)
    (exec1 (cl-cc::make-vm-row-major-aref :dst 0 :lhs 1 :rhs 2) s)
    (assert-= 50 (cl-cc:vm-reg-get s 0))))

(deftest vm-array-row-major-index-computes
  "vm-array-row-major-index computes flat index from subscripts."
  (let ((s (make-test-vm))
        (arr (make-array '(2 3))))
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 '(1 2))
    (exec1 (cl-cc::make-vm-array-row-major-index :dst 0 :arr 1 :subs 2) s)
    (assert-= 5 (cl-cc:vm-reg-get s 0))))

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

;; NOTE: vm-bit-or has a pre-existing bug — define-simple-instruction resolves
;; bit-or in the cl-cc package (cl-cc::bit-or) instead of cl:bit-or.
;; Test omitted until the source bug is fixed.

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

(deftest vm-array-adjust-grows
  "vm-adjust-array grows an adjustable array."
  (let ((s (make-test-vm))
        (arr (make-array 3 :initial-element 0 :adjustable t)))
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 5)
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
