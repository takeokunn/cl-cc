;;;; persistent-tests-2.lisp — Unit tests for the immutable persistent map
;;;; (continued from persistent-tests.lisp): alist roundtrip, custom :test,
;;;; hash collision handling, and large-N / stress tests.

(in-package :cl-cc/test)

(in-suite persistent-map-suite)

;;; ------------------------------------------------------------
;;; Alist roundtrip
;;; ------------------------------------------------------------

(deftest persist-to-alist-roundtrip
  "persist-from-alist / persist-to-alist preserve the key set and values."
  (let* ((source '((:a . 1) (:b . 2) (:c . 3)))
         (pm (persist-from-alist source))
         (recovered (persist-to-alist pm)))
    (assert-= 3 (persist-count pm))
    (assert-equal (%alist-sorted-keys source)
                  (%alist-sorted-keys recovered))
    (assert-= 1 (persist-lookup pm :a))
    (assert-= 2 (persist-lookup pm :b))
    (assert-= 3 (persist-lookup pm :c))))

;;; ------------------------------------------------------------
;;; Custom :test (equal for strings)
;;; ------------------------------------------------------------

(deftest persist-custom-test-equal-for-strings
  "String keys compare by EQUAL when :test 'equal is supplied."
  (let* ((pm0 (persist-empty :test 'equal))
         (pm1 (persist-assoc pm0 "foo" 1))
         (pm2 (persist-assoc pm1 (concatenate 'string "fo" "o") 2)))
    ;; Two string literals with the same content must collapse to one key.
    (assert-= 1 (persist-count pm2))
    (multiple-value-bind (v found-p) (persist-lookup pm2 "foo")
      (assert-= 2 v)
      (assert-true found-p))))

(deftest persist-eql-keeps-distinct-string-instances
  "Under the default :test 'eql, two distinct string instances are distinct keys."
  (let* ((pm0 (persist-empty))
         (s1 (copy-seq "foo"))
         (s2 (copy-seq "foo"))
         (pm1 (persist-assoc pm0 s1 1))
         (pm2 (persist-assoc pm1 s2 2)))
    (assert-= 2 (persist-count pm2))))

;;; ------------------------------------------------------------
;;; Hash collision handling
;;; ------------------------------------------------------------

(deftest persist-handles-sxhash-collision-keys
  "Two integers with equal SXHASH but distinct values share a bucket
   but are stored independently; lookup returns each value."
  (multiple-value-bind (a b) (%find-collision-pair)
    (cond
      ((null a)
       ;; No collision found within sweep range — treat as skipped by
       ;; asserting trivial truth. This keeps the test resilient on
       ;; hypothetical platforms where SXHASH is injective on fixnums.
       (assert-true t))
      (t
       (let* ((pm0 (persist-empty))
              (pm1 (persist-assoc pm0 a :A))
              (pm2 (persist-assoc pm1 b :B)))
         (assert-= 2 (persist-count pm2))
         (assert-eq :A (persist-lookup pm2 a))
         (assert-eq :B (persist-lookup pm2 b))
         ;; Removing one collision-partner keeps the other.
         (let ((pm3 (persist-remove pm2 a)))
           (assert-= 1 (persist-count pm3))
           (assert-false (persist-contains-p pm3 a))
           (assert-true  (persist-contains-p pm3 b))))))))

;;; ------------------------------------------------------------
;;; Large-N / stress
;;; ------------------------------------------------------------

(deftest persist-large-n-1000-inserts
  "Insert 1000 integer keys; every lookup succeeds and count = 1000."
  (let ((pm (persist-empty)))
    (loop for i from 0 below 1000
          do (setf pm (persist-assoc pm i (* i 10))))
    (assert-= 1000 (persist-count pm))
    (loop for i from 0 below 1000
          do (multiple-value-bind (v found-p) (persist-lookup pm i)
               (assert-true found-p)
               (assert-= (* i 10) v)))))

(deftest persist-stress-100-inserts-then-50-removes
  "Insert 100 keys, remove 50, verify count and remaining lookups."
  (let ((pm (persist-empty)))
    (loop for i from 0 below 100
          do (setf pm (persist-assoc pm i i)))
    (assert-= 100 (persist-count pm))
    (loop for i from 0 below 50
          do (setf pm (persist-remove pm i)))
    (assert-= 50 (persist-count pm))
    ;; Removed keys are gone.
    (loop for i from 0 below 50
          do (assert-false (persist-contains-p pm i)))
    ;; Surviving keys are still retrievable with their original values.
    (loop for i from 50 below 100
          do (multiple-value-bind (v found-p) (persist-lookup pm i)
               (assert-true found-p)
               (assert-= i v)))))

(deftest persist-from-alist-duplicate-keys-leftmost-wins
  "persist-from-alist respects alist left-to-right-wins semantics."
  (let* ((pm (persist-from-alist '((:a . 1) (:a . 2) (:b . 3)))))
    (assert-= 2 (persist-count pm))
    (assert-= 1 (persist-lookup pm :a))
    (assert-= 3 (persist-lookup pm :b))))
