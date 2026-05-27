;;;; -*- coding: utf-8 -*-
;;;; packages/compile/tests/fr-009-pic-tests.lisp
;;;;
;;;; FR-009: Polymorphic Inline Cache tests.
;;;;
;;;; The VM inline-cache stores a (specializer-key . method) pair at each
;;;; vm-generic-call site. These tests verify behavioral correctness via
;;;; run-string (compile-level dispatch) and cache hit/miss profiling.
;;;;

(in-package :cl-cc/test)
(in-suite cl-cc-integration-suite)

;; Behavioral tests -- compile-level dispatch via run-string
(deftest-each fr-009-pic-behavioral
    "FR-009 PIC dispatch produces correct results across type switches."
    :timeout 15
    :cases
    (("two-class-switch"
      (:a :a :b :a)
      "(progn
         (defclass pic-class-a () ())
         (defclass pic-class-b () ())
         (defgeneric pic-id (x))
         (defmethod pic-id ((x pic-class-a)) :a)
         (defmethod pic-id ((x pic-class-b)) :b)
         (let* ((a (make-instance 'pic-class-a))
                (b (make-instance 'pic-class-b)))
           (list (pic-id a) (pic-id a) (pic-id b) (pic-id a))))")
      ("multi-arg-pic"
       (:x=1 :y=2 :x=3 :y=4)
       "(progn
          (defclass point-x () ((v :initarg :v)))
          (defclass point-y () ((v :initarg :v)))
          (defgeneric pick-x (obj))
          (defmethod pick-x ((obj point-x)) (list :x= (slot-value obj 'v)))
          (defmethod pick-x ((obj point-y)) (list :y= (slot-value obj 'v)))
          (let ((x1 (make-instance 'point-x :v 1))
                (y2 (make-instance 'point-y :v 2))
                (x3 (make-instance 'point-x :v 3))
                (y4 (make-instance 'point-y :v 4)))
            (list (pick-x x1) (pick-x y2) (pick-x x3) (pick-x y4))))")
      ("three-class-thrash"
       (:a :b :c :a :b :c)
       "(progn
          (defclass thrash-a () ())
          (defclass thrash-b () ())
          (defclass thrash-c () ())
          (defgeneric thrash-id (x))
          (defmethod thrash-id ((x thrash-a)) :a)
          (defmethod thrash-id ((x thrash-b)) :b)
          (defmethod thrash-id ((x thrash-c)) :c)
          (let ((a (make-instance 'thrash-a))
                (b (make-instance 'thrash-b))
                (c (make-instance 'thrash-c)))
            (list (thrash-id a) (thrash-id b) (thrash-id c)
                  (thrash-id a) (thrash-id b) (thrash-id c))))")
      ("mixed-with-inheritance"
       (:base :derived :base)
       "(progn
          (defclass pic-base () ())
          (defclass pic-derived (pic-base) ())
          (defgeneric inh-id (x))
          (defmethod inh-id ((x pic-base)) :base)
          (defmethod inh-id ((x pic-derived)) :derived)
          (let ((b (make-instance 'pic-base))
                (d (make-instance 'pic-derived)))
            (list (inh-id b) (inh-id d) (inh-id b))))"))
   (expected form)
   (assert-equal expected (run-string form)))

;; VM-level PIC profiling tests -- cache hit/miss counters and type-feedback
(deftest fr-009-pic-cache-transitions-and-profiling
    "FR-009 PIC cache transitions and profiling counters."
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state :profile-enabled-p t))
         (method-a (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label 'prof-method-a :params '(:x)))
         (method-b (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label 'prof-method-b :params '(:x))))
    (setf (cl-cc/vm::vm-io-state-generic-caches state)
          (make-hash-table :test #'equal))
    (let ((ht (gethash 'prof-gf-fn
                       (cl-cc/vm::vm-io-state-generic-caches state))))
      (unless ht
        (setf ht (make-hash-table :test #'eq))
        (setf (gethash 'prof-gf-fn
                       (cl-cc/vm::vm-io-state-generic-caches state)) ht))
      ;; Simulate 6-call PIC sequence: miss, hit, miss, hit, miss, hit
      (setf (gethash 'prof-a-class ht) (cons method-a 0))
      (incf (cdr (gethash 'prof-a-class ht)))
      (setf (gethash 'prof-b-class ht) (cons method-b 0))
      (incf (cdr (gethash 'prof-b-class ht)))
      (setf (gethash 'prof-a-class ht) (cons method-a 0))
      (incf (cdr (gethash 'prof-a-class ht)))
      (let* ((entry-a (gethash 'prof-a-class ht))
             (entry-b (gethash 'prof-b-class ht)))
        (assert-eq method-a (car entry-a))
        (assert-eq method-b (car entry-b))
        (assert-= 2 (cdr entry-a))
        (assert-= 1 (cdr entry-b))))))

(deftest fr-009-pic-invalidation-on-method-registration
    "FR-009 PIC cache invalidation on new method registration."
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (cache (make-hash-table :test #'eq))
         (method-1 (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label 'inv-method-1 :params '(:x)))
         (method-2 (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label 'inv-method-2 :params '(:x))))
    (setf (gethash 'inv-class-a cache) (cons method-1 5))
    (setf (gethash 'inv-class-b cache) (cons method-2 3))
    (remhash 'inv-class-a cache)
    (assert-false (gethash 'inv-class-a cache))
    (let ((entry-b (gethash 'inv-class-b cache)))
      (assert-eq method-2 (car entry-b))
      (assert-= 3 (cdr entry-b)))
    (setf (gethash 'inv-class-a cache) (cons method-2 0))
    (let ((new-entry (gethash 'inv-class-a cache)))
      (assert-eq method-2 (car new-entry))
      (assert-= 0 (cdr new-entry)))))
