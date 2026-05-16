(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest runtime-ubsan-heap-ref-rejects-non-integer-index
  "UBSan mode rejects non-integer heap indices for rt-heap-ref."
  (let ((heap (cl-cc/runtime:make-rt-heap)))
    (let ((cl-cc/runtime:*rt-ubsan-enabled* t))
      (assert-signals error
        (cl-cc/runtime:rt-heap-ref heap :bad-index)))))

(deftest runtime-ubsan-heap-set-rejects-negative-index
  "UBSan mode rejects negative heap indices for rt-heap-set."
  (let ((heap (cl-cc/runtime:make-rt-heap)))
    (let ((cl-cc/runtime:*rt-ubsan-enabled* t))
      (assert-signals error
        (cl-cc/runtime:rt-heap-set heap -1 42)))))

(deftest runtime-ubsan-heap-access-works-when-disabled
  "Heap access keeps working for valid indices when UBSan mode is disabled."
  (let ((heap (cl-cc/runtime:make-rt-heap)))
    (let ((cl-cc/runtime:*rt-ubsan-enabled* nil))
      (cl-cc/runtime:rt-heap-set heap 0 123)
      (assert-= 123 (cl-cc/runtime:rt-heap-ref heap 0)))))
