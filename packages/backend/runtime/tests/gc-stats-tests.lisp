;;;; tests/gc-stats-tests.lisp — GC Statistics Tests
;;;;
;;;; Continuation of gc-tests.lisp.
;;;; Tests for rt-gc-stats plist structure and correctness.

(in-package :cl-cc/test)

(in-suite gc-suite)

;;; ------------------------------------------------------------
;;; Test 8: gc-stats
;;; ------------------------------------------------------------

(deftest gc-stats-returns-plist
  "rt-gc-stats returns a plist with all required keys."
  (let* ((heap (%make-small-heap))
         (stats (cl-cc/runtime:rt-gc-stats heap)))
    (assert-true (getf stats :minor-gc-count))
    (assert-true (listp stats))
    (assert-true (member :minor-gc-count  stats))
    (assert-true (member :major-gc-count  stats))
    (assert-true (member :words-collected stats))
    (assert-true (member :words-promoted  stats))
    (assert-true (member :young-used      stats))
    (assert-true (member :young-total     stats))
    (assert-true (member :old-used        stats))
    (assert-true (member :old-total       stats))
    (assert-true (member :free-list-count stats))))

(deftest gc-stats-minor-gc-count-increments
  "After one minor GC, :minor-gc-count is 1."
  (let* ((heap (%make-small-heap)))
    (let ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%write-header heap addr 3 cl-cc/runtime:+rt-tag-cons+)
      (let ((root (cons nil addr)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-minor-collect heap)
        (assert-= 1 (getf (cl-cc/runtime:rt-gc-stats heap) :minor-gc-count))
        (cl-cc/runtime:rt-gc-remove-root heap root)))))

(deftest gc-stats-totals-match-heap-structure
  ":young-total matches semi-size; :old-total matches old-size slot."
  (let* ((heap  (%make-small-heap))
         (stats (cl-cc/runtime:rt-gc-stats heap)))
    (assert-= (cl-cc/runtime:rt-heap-young-semi-size heap) (getf stats :young-total))
    (assert-= (cl-cc/runtime:rt-heap-old-size heap)        (getf stats :old-total))))

(deftest gc-stats-young-used-after-alloc
  ":young-used reflects allocated words before GC."
  (let* ((heap (%make-small-heap)))
    (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)
    (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)
    (assert-= 6 (getf (cl-cc/runtime:rt-gc-stats heap) :young-used))))
