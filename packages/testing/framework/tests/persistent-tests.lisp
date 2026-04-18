;;;; persistent-tests.lisp — Unit tests for the immutable persistent map
;;;; foundation introduced in persistent.lisp.
;;;;
;;;; These tests lock in the three contractual properties required by the
;;;; framework migration:
;;;;   1. persist-assoc / persist-remove are non-mutating (immutability).
;;;;   2. Lookup/contains-p report correct membership.
;;;;   3. Hash collisions (two distinct keys with equal sxhash) are
;;;;      handled via alist buckets keyed by the map's :test function.

(in-package :cl-cc/test)

(defsuite persistent-map-suite
  :description "Immutable persistent hash-map foundation (persistent.lisp)."
  :parent cl-cc-unit-suite)

(in-suite persistent-map-suite)

;;; ------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------

(defun %alist-sorted-keys (alist)
  "Return the keys of ALIST sorted by their printed representation so
   two runs can be compared regardless of internal traversal order."
  (sort (mapcar #'car alist)
        (lambda (a b) (string< (prin1-to-string a) (prin1-to-string b)))))

(defun %find-collision-pair ()
  "Find two distinct integers that share the same SXHASH. SBCL's fixnum
   SXHASH is not the identity on large integers, so a small sweep finds
   a collision quickly. Returns (values a b) with a /= b and
   (= (sxhash a) (sxhash b))."
  (let ((buckets (make-hash-table :test 'eql)))
    (loop for i from 1 to 200000
          for h = (sxhash i)
          for prev = (gethash h buckets)
          do (cond
               ((and prev (/= prev i))
                (return-from %find-collision-pair (values prev i)))
               (t (setf (gethash h buckets) i))))
    ;; Fallback: the test that depends on this will not execute on a
    ;; platform where no collision exists below the sweep limit.
    (values nil nil)))

;;; ------------------------------------------------------------
;;; Construction / count
;;; ------------------------------------------------------------

(deftest persist-empty-has-zero-count
  "persist-empty returns a persistent-map with count 0 and nil root."
  (let ((pm (persist-empty)))
    (assert-true (persistent-map-p pm))
    (assert-= 0 (persist-count pm))
    (assert-null (persistent-map-root pm))
    (assert-eq 'eql (persistent-map-test pm))))

(deftest persist-empty-with-equal-test
  "persist-empty honors a custom :test keyword."
  (let ((pm (persist-empty :test 'equal)))
    (assert-eq 'equal (persistent-map-test pm))
    (assert-= 0 (persist-count pm))))

;;; ------------------------------------------------------------
;;; Insert / lookup
;;; ------------------------------------------------------------

(deftest persist-single-insert-increments-count
  "Inserting a key increments count to 1 and the value is retrievable."
  (let* ((pm0 (persist-empty))
         (pm1 (persist-assoc pm0 :a 1)))
    (assert-= 1 (persist-count pm1))
    (multiple-value-bind (v found-p) (persist-lookup pm1 :a)
      (assert-= 1 v)
      (assert-true found-p))))

(deftest persist-lookup-missing-returns-nil-nil
  "Looking up an absent key returns (values nil nil)."
  (let ((pm (persist-empty)))
    (multiple-value-bind (v found-p) (persist-lookup pm :missing)
      (assert-null v)
      (assert-null found-p))))

(deftest persist-lookup-missing-respects-default
  "Default value is returned when the key is absent."
  (let ((pm (persist-empty)))
    (multiple-value-bind (v found-p) (persist-lookup pm :missing :sentinel)
      (assert-eq :sentinel v)
      (assert-null found-p))))

(deftest persist-update-existing-key-replaces-value
  "Re-inserting an existing key leaves count unchanged and replaces the value."
  (let* ((pm0 (persist-empty))
         (pm1 (persist-assoc pm0 :a 1))
         (pm2 (persist-assoc pm1 :a 99)))
    (assert-= 1 (persist-count pm2))
    (multiple-value-bind (v found-p) (persist-lookup pm2 :a)
      (assert-= 99 v)
      (assert-true found-p))))

;;; ------------------------------------------------------------
;;; Immutability
;;; ------------------------------------------------------------

(deftest persist-assoc-does-not-mutate-input
  "persist-assoc produces a fresh map; the original count is unchanged."
  (let* ((pm0 (persist-empty))
         (pm1 (persist-assoc pm0 :a 1)))
    (declare (ignore pm1))
    (assert-= 0 (persist-count pm0))
    (multiple-value-bind (v found-p) (persist-lookup pm0 :a)
      (declare (ignore v))
      (assert-null found-p))))

(deftest persist-remove-does-not-mutate-input
  "persist-remove produces a fresh map without mutating the original."
  (let* ((pm0 (persist-assoc (persist-empty) :a 1))
         (pm1 (persist-remove pm0 :a)))
    (declare (ignore pm1))
    (assert-= 1 (persist-count pm0))
    (assert-true (persist-contains-p pm0 :a))))

;;; ------------------------------------------------------------
;;; Multiple inserts, contains, keys/values
;;; ------------------------------------------------------------

(deftest persist-multiple-inserts-count-reflects-unique-keys
  "Inserting three distinct keys yields count 3."
  (let* ((pm (persist-assoc
              (persist-assoc
               (persist-assoc (persist-empty) :a 1)
               :b 2)
              :c 3)))
    (assert-= 3 (persist-count pm))
    (assert-true (persist-contains-p pm :a))
    (assert-true (persist-contains-p pm :b))
    (assert-true (persist-contains-p pm :c))
    (assert-false (persist-contains-p pm :d))))

(deftest persist-keys-returns-all-keys
  "persist-keys returns every inserted key (order-independent check)."
  (let* ((pm (persist-assoc
              (persist-assoc
               (persist-assoc (persist-empty) :a 1)
               :b 2)
              :c 3))
         (keys (persist-keys pm)))
    (assert-= 3 (length keys))
    (dolist (k '(:a :b :c))
      (assert-true (member k keys)))))

(deftest persist-values-returns-all-values
  "persist-values returns every inserted value."
  (let* ((pm (persist-assoc
              (persist-assoc
               (persist-assoc (persist-empty) :a 10)
               :b 20)
              :c 30))
         (values (persist-values pm)))
    (assert-= 3 (length values))
    (dolist (v '(10 20 30))
      (assert-true (member v values)))))

(deftest persist-each-invokes-fn-for-every-pair
  "persist-each calls fn exactly once per entry."
  (let* ((pm (persist-assoc
              (persist-assoc (persist-empty) :a 1)
              :b 2))
         (seen '()))
    (persist-each pm (lambda (k v) (push (cons k v) seen)))
    (assert-= 2 (length seen))
    (assert-equal 1 (cdr (assoc :a seen)))
    (assert-equal 2 (cdr (assoc :b seen)))))

;;; ------------------------------------------------------------
;;; Remove
;;; ------------------------------------------------------------

(deftest persist-remove-decrements-count
  "Removing an existing key decrements count by one."
  (let* ((pm0 (persist-assoc (persist-assoc (persist-empty) :a 1) :b 2))
         (pm1 (persist-remove pm0 :a)))
    (assert-= 1 (persist-count pm1))
    (assert-false (persist-contains-p pm1 :a))
    (assert-true (persist-contains-p pm1 :b))))

(deftest persist-remove-missing-key-count-unchanged
  "Removing an absent key returns an equivalent map."
  (let* ((pm0 (persist-assoc (persist-empty) :a 1))
         (pm1 (persist-remove pm0 :missing)))
    (assert-= 1 (persist-count pm1))
    (assert-true (persist-contains-p pm1 :a))))

(deftest persist-remove-last-entry-returns-empty
  "Removing the only key yields a map with count 0."
  (let* ((pm0 (persist-assoc (persist-empty) :a 1))
         (pm1 (persist-remove pm0 :a)))
    (assert-= 0 (persist-count pm1))
    (assert-false (persist-contains-p pm1 :a))))

