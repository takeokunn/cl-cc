;;;; packages/javascript/tests/js-runtime-typed-array-methods-tests.lisp
;;;;
;;;; Unit tests for runtime-typed-arrays-methods.lisp:
;;;; ES2023 non-mutating methods (toReversed, toSorted, with, at, findLast…),
;;;; mutating methods (reverse, sort, copyWithin), iterator methods
;;;; (values, keys, entries), and ES2025 Uint8Array hex/base64 encoding.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr)

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun %make-int32-ta (&rest values)
  "Build an Int32Array TypedArray filled with VALUES."
  (let ((ta (cl-cc/javascript::%js-make-typed-array "Int32Array" (length values))))
    (loop for v in values for i from 0
          do (cl-cc/javascript::%js-ta-set ta i v))
    ta))

(defun %ta-to-list (ta)
  "Convert a TypedArray's buffer to a CL list of numbers."
  (loop for i below (cl-cc/javascript::js-ta-length ta)
        collect (cl-cc/javascript::%js-ta-get ta i)))

;;; ─── at (negative indexing) ──────────────────────────────────────────────────

(deftest-each js-rt-ta-at
  "TypedArray.at() supports positive and negative indices."
  :cases (("first"   0  10.0d0)
          ("last"   -1  30.0d0)
          ("middle"  1  20.0d0))
  (idx expected)
  (let ((ta (%make-int32-ta 10 20 30)))
    (assert-= expected (cl-cc/javascript::%js-ta-at ta idx))))

;;; ─── find / findIndex ────────────────────────────────────────────────────────

(deftest js-rt-ta-find
  "TypedArray.find() returns first element satisfying predicate or +js-undefined+."
  (let ((ta (%make-int32-ta 3 7 4 9)))
    (assert-= 7.0d0 (cl-cc/javascript::%js-ta-find ta (lambda (v &rest _) (declare (ignore _)) (> v 5))))
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-ta-find ta (lambda (v &rest _) (declare (ignore _)) (> v 100))))))

(deftest js-rt-ta-find-index
  "TypedArray.findIndex() returns index of first match or -1."
  (let ((ta (%make-int32-ta 3 7 4)))
    (assert-= 1.0d0  (cl-cc/javascript::%js-ta-find-index ta (lambda (v &rest _) (declare (ignore _)) (> v 5))))
    (assert-= -1.0d0 (cl-cc/javascript::%js-ta-find-index ta (lambda (v &rest _) (declare (ignore _)) (> v 100))))))

(deftest js-rt-ta-find-last
  "TypedArray.findLast() returns last element satisfying predicate."
  (let ((ta (%make-int32-ta 3 7 4 9)))
    (assert-= 9.0d0 (cl-cc/javascript::%js-ta-find-last ta (lambda (v &rest _) (declare (ignore _)) (> v 5))))))

(deftest js-rt-ta-find-last-index
  "TypedArray.findLastIndex() returns index of last match."
  (let ((ta (%make-int32-ta 3 7 4 9)))
    (assert-= 3.0d0 (cl-cc/javascript::%js-ta-find-last-index ta (lambda (v &rest _) (declare (ignore _)) (> v 5))))))

;;; ─── every / some ────────────────────────────────────────────────────────────

(deftest-each js-rt-ta-every-some
  "TypedArray.every() and .some() test all/any elements against a predicate."
  :cases (("every-all-positive"  #'cl-cc/javascript::%js-ta-every  '(1 2 3)  (lambda (v &rest _) (declare (ignore _)) (> v 0))  t)
          ("every-some-negative" #'cl-cc/javascript::%js-ta-every  '(1 -2 3) (lambda (v &rest _) (declare (ignore _)) (> v 0))  nil)
          ("some-found"         #'cl-cc/javascript::%js-ta-some   '(1 -2 3) (lambda (v &rest _) (declare (ignore _)) (< v 0))  t)
          ("some-not-found"     #'cl-cc/javascript::%js-ta-some   '(1 2 3)  (lambda (v &rest _) (declare (ignore _)) (< v 0))  nil))
  (fn values pred expected)
  (let ((ta (apply #'%make-int32-ta values)))
    (assert-equal expected (funcall fn ta pred))))

;;; ─── reverse / toReversed ────────────────────────────────────────────────────

(deftest js-rt-ta-reverse-mutating
  "TypedArray.reverse() reverses elements in-place and returns same TA."
  (let ((ta (%make-int32-ta 1 2 3)))
    (let ((ret (cl-cc/javascript::%js-ta-reverse ta)))
      (assert-eq ta ret)
      (assert-equal '(3 2 1) (%ta-to-list ta)))))

(deftest js-rt-ta-to-reversed-non-mutating
  "TypedArray.toReversed() returns a reversed copy without mutating original."
  (let* ((ta  (%make-int32-ta 1 2 3))
         (rev (cl-cc/javascript::%js-ta-to-reversed ta)))
    (assert-false (eq ta rev))
    (assert-equal '(1 2 3) (%ta-to-list ta))
    (assert-equal '(3 2 1) (%ta-to-list rev))))

;;; ─── sort / toSorted ─────────────────────────────────────────────────────────

(deftest js-rt-ta-sort-mutating
  "TypedArray.sort() sorts in-place and returns same TA."
  (let ((ta (%make-int32-ta 3 1 2)))
    (let ((ret (cl-cc/javascript::%js-ta-sort ta)))
      (assert-eq ta ret)
      (assert-equal '(1 2 3) (%ta-to-list ta)))))

(deftest js-rt-ta-to-sorted-non-mutating
  "TypedArray.toSorted() returns a sorted copy without mutating original."
  (let* ((ta     (%make-int32-ta 3 1 2))
         (sorted (cl-cc/javascript::%js-ta-to-sorted ta)))
    (assert-false (eq ta sorted))
    (assert-equal '(3 1 2) (%ta-to-list ta))
    (assert-equal '(1 2 3) (%ta-to-list sorted))))

;;; ─── with ────────────────────────────────────────────────────────────────────

(deftest js-rt-ta-with-non-mutating
  "TypedArray.with(index, value) returns a copy with one element replaced."
  (let* ((ta  (%make-int32-ta 1 2 3))
         (w   (cl-cc/javascript::%js-ta-with ta 1 99)))
    (assert-false (eq ta w))
    (assert-equal '(1 2 3)  (%ta-to-list ta))
    (assert-equal '(1 99 3) (%ta-to-list w))))

;;; ─── copyWithin ──────────────────────────────────────────────────────────────

(deftest js-rt-ta-copy-within
  "TypedArray.copyWithin(target, start, end) copies a sub-segment in-place."
  (let ((ta (%make-int32-ta 1 2 3 4 5)))
    (cl-cc/javascript::%js-ta-copy-within ta 0 3 5)
    (assert-equal '(4 5 3 4 5) (%ta-to-list ta))))

;;; ─── lastIndexOf ─────────────────────────────────────────────────────────────

(deftest-each js-rt-ta-last-index-of
  "TypedArray.lastIndexOf returns last matching index or -1."
  :cases (("found"  '(1 2 3 2) 2  3.0d0)
          ("absent" '(1 2 3)   9 -1.0d0))
  (values target expected)
  (let ((ta (apply #'%make-int32-ta values)))
    (assert-= expected (cl-cc/javascript::%js-ta-last-index-of ta target))))

;;; ─── values / keys / entries iterators ──────────────────────────────────────

(deftest js-rt-ta-values-iterator
  "TypedArray.values() returns an iterator that yields each element as double."
  (let* ((ta    (%make-int32-ta 10 20))
         (iter  (cl-cc/javascript::%js-ta-values ta))
         (next  (gethash "next" iter))
         (r1    (funcall next))
         (r2    (funcall next))
         (done  (funcall next)))
    (assert-= 10.0d0 (gethash "value" r1))
    (assert-= 20.0d0 (gethash "value" r2))
    (assert-true     (gethash "done"  done))))

(deftest js-rt-ta-keys-iterator
  "TypedArray.keys() returns an iterator yielding indices as doubles."
  (let* ((ta   (%make-int32-ta 10 20))
         (iter (cl-cc/javascript::%js-ta-keys ta))
         (next (gethash "next" iter))
         (r1   (funcall next))
         (r2   (funcall next))
         (done (funcall next)))
    (assert-= 0.0d0 (gethash "value" r1))
    (assert-= 1.0d0 (gethash "value" r2))
    (assert-true    (gethash "done"  done))))

(deftest js-rt-ta-entries-iterator
  "TypedArray.entries() returns [index, value] pair arrays."
  (let* ((ta   (%make-int32-ta 5))
         (iter (cl-cc/javascript::%js-ta-entries ta))
         (next (gethash "next" iter))
         (r1   (funcall next))
         (done (funcall next)))
    (let ((pair (gethash "value" r1)))
      (assert-= 0.0d0 (aref pair 0))
      (assert-= 5.0d0 (aref pair 1)))
    (assert-true (gethash "done" done))))

;;; ─── clone-with-buffer ───────────────────────────────────────────────────────

(deftest js-rt-ta-clone-with-buffer
  "%js-ta-clone-with-buffer reuses type/element-size and uses the new buffer."
  (let* ((orig    (%make-int32-ta 1 2 3))
         (new-buf (make-array 2 :initial-contents (list 7 8) :element-type t))
         (clone   (cl-cc/javascript::%js-ta-clone-with-buffer orig new-buf)))
    (assert-false (eq orig clone))
    (assert-string= "Int32Array" (cl-cc/javascript::js-ta-type-name clone))
    (assert-= 2 (cl-cc/javascript::js-ta-length clone))))

;;; ─── ES2025 Uint8Array hex / base64 ─────────────────────────────────────────

(defun %make-u8-ta (&rest bytes)
  (let ((ta (cl-cc/javascript::%js-make-typed-array "Uint8Array" (length bytes))))
    (loop for b in bytes for i from 0
          do (cl-cc/javascript::%js-ta-set ta i b))
    ta))

(deftest js-rt-uint8-to-hex
  "Uint8Array.toHex() produces lowercase hex pairs."
  (let ((ta (%make-u8-ta 0 15 255)))
    (assert-string= "000fff" (cl-cc/javascript::%js-uint8-to-hex ta))))

(deftest js-rt-uint8-from-hex
  "Uint8Array.fromHex() parses hex string into Uint8Array."
  (let* ((ta  (cl-cc/javascript::%js-uint8-from-hex "000fff"))
         (buf (cl-cc/javascript::js-ta-buffer ta)))
    (assert-= 3 (cl-cc/javascript::js-ta-length ta))
    (assert-= 0   (aref buf 0))
    (assert-= 15  (aref buf 1))
    (assert-= 255 (aref buf 2))))

(deftest js-rt-uint8-base64-roundtrip
  "toBase64/fromBase64 round-trip Uint8Array data."
  (let* ((orig   (%make-u8-ta 1 2 3))
         (b64    (cl-cc/javascript::%js-uint8-to-base64 orig))
         (result (cl-cc/javascript::%js-uint8-from-base64 b64)))
    (assert-= 3 (cl-cc/javascript::js-ta-length result))
    (assert-= 1 (cl-cc/javascript::%js-ta-get result 0))
    (assert-= 2 (cl-cc/javascript::%js-ta-get result 1))
    (assert-= 3 (cl-cc/javascript::%js-ta-get result 2))))
