;;;; tests/gc-sweep-major-tests.lisp - GC Sweep and Major Collection Tests
;;;
;;; Tests for %gc-sweep-old-space and rt-gc-major-collect:
;;; - Sweep reclaims dead (unmarked) old-space objects to the free-list
;;; - Sweep clears mark bits on live objects
;;; - Major GC increments counter and restores gc-state
;;; - Major GC reclaims unreachable old-space objects
;;; - Major GC preserves rooted old-space objects

(in-package :cl-cc/test)

(in-suite gc-suite)

;;; ------------------------------------------------------------
;;; Test 9: %gc-sweep-old-space
;;; ------------------------------------------------------------

(deftest gc-sweep-old-space-reclaims-dead-objects
  "Sweep reclaims unmarked (dead) objects to the free-list and clears mark
   bits on live (marked) objects."
  (let* ((heap     (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64))
         (old-base (cl-cc/runtime:rt-heap-old-base heap))
         ;; Place two 3-word objects in old space
         (live-addr old-base)
         (dead-addr (+ old-base 3)))
    ;; Write headers: live object is marked; dead object is not
    (cl-cc/runtime:rt-heap-set-header
     heap live-addr
     (cl-cc/runtime:header-set-mark
      (cl-cc/runtime:make-header 3 cl-cc/runtime:+rt-tag-cons+ 0)))
    (cl-cc/runtime:rt-heap-set-header
     heap dead-addr
     (cl-cc/runtime:make-header 3 cl-cc/runtime:+rt-tag-cons+ 0))
    ;; Advance old-free past both objects
    (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-base 6))
    ;; Sweep
    (cl-cc/runtime::%gc-sweep-old-space heap)
    ;; Dead object (3 words) should be reclaimed
    (assert-true (>= (cl-cc/runtime:rt-heap-words-collected heap) 3))
    ;; Free-list should contain the dead object
    (assert-true (not (null (cl-cc/runtime:rt-heap-free-list heap))))
    ;; Live object's mark bit should be cleared after sweep
    (let ((live-hdr (cl-cc/runtime:rt-heap-object-header heap live-addr)))
      (assert-false (cl-cc/runtime:header-marked-p live-hdr)))))

(deftest gc-sweep-old-space-all-live-no-reclaim
  "Sweep of all-marked objects reclaims nothing; free-list stays empty."
  (let* ((heap     (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    ;; Two marked objects
    (cl-cc/runtime:rt-heap-set-header
     heap old-base
     (cl-cc/runtime:header-set-mark
      (cl-cc/runtime:make-header 3 cl-cc/runtime:+rt-tag-cons+ 0)))
    (cl-cc/runtime:rt-heap-set-header
     heap (+ old-base 3)
     (cl-cc/runtime:header-set-mark
      (cl-cc/runtime:make-header 3 cl-cc/runtime:+rt-tag-cons+ 0)))
    (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-base 6))
    (cl-cc/runtime::%gc-sweep-old-space heap)
    (assert-= 0 (cl-cc/runtime:rt-heap-words-collected heap))
    (assert-true (null (cl-cc/runtime:rt-heap-free-list heap)))))

;;; ------------------------------------------------------------
;;; Test 10: rt-gc-major-collect
;;; ------------------------------------------------------------

(deftest gc-major-collect-increments-counter
  "rt-gc-major-collect increments major-gc-count and restores gc-state to :normal."
  (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64)))
    (assert-= 0 (cl-cc/runtime:rt-heap-major-gc-count heap))
    (cl-cc/runtime:rt-gc-major-collect heap)
    (assert-= 1 (cl-cc/runtime:rt-heap-major-gc-count heap))
    (assert-eq :normal (cl-cc/runtime:rt-heap-gc-state heap))))

(deftest gc-major-collect-gc-state-restored-on-empty-heap
  "Major GC on a fresh heap: gc-state is :normal after the call."
  (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 128 :old-size 128)))
    (cl-cc/runtime:rt-gc-major-collect heap)
    (assert-eq :normal (cl-cc/runtime:rt-heap-gc-state heap))))

(deftest gc-major-collect-reclaims-unreachable-old-object
  "An old-space object with no root is swept; words-collected increases."
  (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 128 :old-size 128))
         ;; Promote an object by bumping it into old space manually
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    ;; Place an unmarked object in old space with no root
    (cl-cc/runtime:rt-heap-set-header
     heap old-base
     (cl-cc/runtime:make-header 3 cl-cc/runtime:+rt-tag-cons+ 0))
    (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-base 3))
    ;; Run major GC — object is unreachable, should be collected
    (cl-cc/runtime:rt-gc-major-collect heap)
    (assert-true (>= (cl-cc/runtime:rt-heap-words-collected heap) 3))))

(deftest gc-major-collect-preserves-rooted-old-object
  "An old-space object reachable from a root survives major GC."
  (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 128 :old-size 128))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    ;; Place live object in old space
    (cl-cc/runtime:rt-heap-set-header
     heap old-base
     (cl-cc/runtime:make-header 3 cl-cc/runtime:+rt-tag-cons+ 0))
    (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-base 3))
    ;; Register root pointing directly into old space
    (let ((root (cons nil old-base)))
      (cl-cc/runtime:rt-gc-add-root heap root)
      (cl-cc/runtime:rt-gc-major-collect heap)
      ;; Live object must still have a readable header
      (let ((hdr (cl-cc/runtime:rt-heap-object-header heap old-base)))
        (assert-true (integerp hdr))
        (assert-= 3 (cl-cc/runtime:header-size hdr)))
      (cl-cc/runtime:rt-gc-remove-root heap root))))

(deftest gc-major-collect-stats-major-count
  "rt-gc-stats :major-gc-count reflects the number of major GCs run."
  (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 128 :old-size 128)))
    (cl-cc/runtime:rt-gc-major-collect heap)
    (cl-cc/runtime:rt-gc-major-collect heap)
    (assert-= 2 (getf (cl-cc/runtime:rt-gc-stats heap) :major-gc-count))))
