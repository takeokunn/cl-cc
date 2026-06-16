(in-package :cl-cc/test)

(defsuite persistent-suite
  :description "FR-748 HAMT, FR-749 persistent vector, and FR-750 lazy sequence tests"
  :parent cl-cc-unit-suite)

(in-suite persistent-suite)

(defun assert-pmap-entry (map key expected &key (test #'equal))
  (multiple-value-bind (value found-p) (cl-cc/vm:pmap-get map key)
    (assert-true found-p)
    (assert-true (funcall test expected value))))

(deftest persistent-map-assoc-get-and-dissoc
  "Persistent maps return new maps for pmap-assoc/dissoc while preserving old versions."
  (let* ((m0 (cl-cc/vm:persistent-map))
         (m1 (cl-cc/vm:pmap-assoc m0 'a 1))
         (m2 (cl-cc/vm:pmap-assoc m1 'b 2))
         (m3 (cl-cc/vm:pmap-assoc m2 'a 10))
         (m4 (cl-cc/vm:dissoc m3 'b)))
    (multiple-value-bind (value found-p) (cl-cc/vm:pmap-get m0 'a :missing)
      (assert-equal :missing value)
      (assert-false found-p))
    (multiple-value-bind (value found-p) (cl-cc/vm:pmap-get m1 'a)
      (assert-equal 1 value)
      (assert-true found-p))
    (assert-equal 2 (cl-cc/vm:persistent-map-count m2))
    (assert-equal 2 (cl-cc/vm:persistent-map-count m3))
    (assert-equal 1 (cl-cc/vm:pmap-get m1 'a))
    (assert-equal 10 (cl-cc/vm:pmap-get m3 'a))
    (multiple-value-bind (value found-p) (cl-cc/vm:pmap-get m4 'b :missing)
      (assert-equal :missing value)
      (assert-false found-p))
    (assert-equal 1 (cl-cc/vm:persistent-map-count m4))
    (assert-pmap-entry m2 'b 2)))

(deftest persistent-map-supports-equal-keys-and-bitmap-branching
  "HAMT maps support non-EQL tests and enough entries to exercise bitmap nodes."
  (let ((map (cl-cc/vm:persistent-map :test 'equal)))
    (dotimes (i 80)
      (setf map (cl-cc/vm:pmap-assoc map (format nil "k-~d" i) (* i 2))))
    (assert-equal 80 (cl-cc/vm:persistent-map-count map))
    (assert-equal 42 (cl-cc/vm:pmap-get map (copy-seq "k-21")))
    (let ((smaller (cl-cc/vm:dissoc map (copy-seq "k-21"))))
      (assert-equal 80 (cl-cc/vm:persistent-map-count map))
      (assert-equal 79 (cl-cc/vm:persistent-map-count smaller))
      (assert-pmap-entry map "k-21" 42)
      (multiple-value-bind (value found-p) (cl-cc/vm:pmap-get smaller "k-21" :missing)
        (assert-equal :missing value)
        (assert-false found-p)))))

(deftest persistent-map-api-rejects-host-collection-fallbacks
  "Persistent map operations do not fall back to host alist or plist APIs."
  (assert-signals error (cl-cc/vm:persistent-map :a 1 :b))
  (assert-signals error (cl-cc/vm:pmap-assoc 'a '((a . 1)) 0))
  (assert-signals error (cl-cc/vm:pmap-get '(a 1) 'a))
  (assert-signals error (cl-cc/vm:dissoc '(a 1) 'a)))

(deftest persistent-vector-get-assoc-conj
  "Persistent vector operations preserve previous versions."
  (let* ((v0 (cl-cc/vm:pvec 1 2 3))
         (v1 (cl-cc/vm:pvec-assoc v0 1 20))
         (v2 (cl-cc/vm:pvec-conj v1 40)))
    (assert-equal 3 (cl-cc/vm:pvec-count v0))
    (assert-equal 2 (cl-cc/vm:pvec-get v0 1))
    (assert-equal 20 (cl-cc/vm:pvec-get v1 1))
    (assert-equal 3 (cl-cc/vm:pvec-count v1))
    (assert-equal 4 (cl-cc/vm:pvec-count v2))
    (assert-equal 40 (cl-cc/vm:pvec-get v2 3))
    (assert-equal :missing (cl-cc/vm:pvec-get v2 99 :missing))))

(deftest transient-map-assoc-dissoc-and-persistent-freeze
  "Transient maps mutate in place and freeze back to persistent maps."
  (let* ((m0 (cl-cc/vm:persistent-map :test 'equal "a" 1 "b" 2))
         (tm (cl-cc/vm:transient m0)))
    (assert-eq tm (cl-cc/vm:assoc! tm (copy-seq "a") 10))
    (assert-eq tm (cl-cc/vm:dissoc! tm "b"))
    (let ((m1 (cl-cc/vm:persistent! tm)))
      (assert-equal 2 (cl-cc/vm:persistent-map-count m0))
      (assert-equal 1 (cl-cc/vm:persistent-map-count m1))
      (assert-equal 10 (cl-cc/vm:pmap-get m1 "a"))
      (multiple-value-bind (value found-p) (cl-cc/vm:pmap-get m1 "b" :missing)
        (assert-equal :missing value)
        (assert-false found-p)))))

(deftest transient-vector-conj-and-persistent-freeze
  "Transient vectors append in place and freeze back to persistent vectors."
  (let* ((v0 (cl-cc/vm:pvec 1 2))
         (tv (cl-cc/vm:transient! v0)))
    (assert-eq tv (cl-cc/vm:conj! tv 3))
    (assert-eq tv (cl-cc/vm:conj! tv 4))
    (let ((v1 (cl-cc/vm:persistent! tv)))
      (assert-equal 2 (cl-cc/vm:pvec-count v0))
      (assert-equal 4 (cl-cc/vm:pvec-count v1))
      (assert-equal 4 (cl-cc/vm:pvec-get v1 3)))))

(deftest lazy-seq-force-take-map-filter-iterate
  "Lazy sequences are memoized and support infinite iterate pipelines."
  (let ((forced 0))
    (let ((seq (cl-cc/vm:lazy-seq
                 (incf forced)
                 (cons 1 (cl-cc/vm:lazy-seq
                           (incf forced)
                           (cons 2 nil))))))
    (assert-equal 0 forced)
    (assert-equal '(1) (cl-cc/vm:lazy-take-seq 1 seq))
    (assert-equal 1 forced)
    (assert-equal '(1 2) (cl-cc/vm:lazy-take-seq 2 seq))
    (assert-equal 2 forced)
    (assert-equal '(1 2) (cl-cc/vm:lazy-take-seq 2 seq))
      (assert-equal 2 forced)))
  (let* ((naturals (cl-cc/vm:iterate #'1+ 0))
         (evens (cl-cc/vm:lazy-filter #'evenp naturals))
         (doubled (cl-cc/vm:lazy-map (lambda (x) (* x 2)) evens)))
    (assert-equal '(0 4 8 12 16) (cl-cc/vm:lazy-take-seq 5 doubled))))

(deftest lazy-seq-preserves-nil-elements-and-descending-ranges
  "Lazy sequences keep NIL values as values and support descending ranges."
  (let ((seq (cl-cc/vm:lazy-seq
               (cons nil
                     (cl-cc/vm:lazy-seq
                       (cons :tail nil))))))
    (assert-equal '(nil :tail) (cl-cc/vm:lazy-take-seq 2 seq)))
  (assert-equal '(3 2 1)
                (cl-cc/vm:lazy-take-seq
                 5
                 (cl-cc/vm:lazy-range :start 3 :end 0 :step -1)))
  (assert-signals error (cl-cc/vm:lazy-range :step 0)))
