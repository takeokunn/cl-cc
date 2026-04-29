(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

(deftest fr-555-copy-structure-top-level-slots
  "copy-structure returns an independent top-level struct copy."
  (assert-= 10
            (run-string
             "(progn
                (defstruct point x y)
                (let* ((p1 (make-point :x 10 :y 20))
                       (p2 (copy-structure p1)))
                  (setf (point-x p1) 99)
                  (point-x p2)))")))

(deftest fr-555-copy-structure-is-shallow
  "copy-structure shallow-copies slot contents rather than deep-copying them."
  (assert-eq :changed
             (run-string
              "(progn
                 (defstruct registry entries)
                 (let* ((inner (make-hash-table))
                        (r1 (make-registry :entries inner))
                        (r2 (copy-structure r1)))
                   (setf (gethash 'k inner) :changed)
                   (gethash 'k (registry-entries r2))))")))

(deftest fr-555-copy-structure-type-list
  "copy-structure also works for :type list defstructs."
  :timeout 180
  (assert-= 10
            (run-string
             "(progn
                (defstruct (pair (:type list)) left right)
                (let* ((p1 (make-pair :left 10 :right 20))
                       (p2 (copy-structure p1)))
                  (setf (first (cdr p1)) 99)
                  (first (cdr p2))))")))

(deftest fr-555-copy-structure-type-vector
  "copy-structure also works for :type vector defstructs."
  (assert-= 10
            (run-string
             "(progn
                (defstruct (pairv (:type vector)) left right)
                (let* ((p1 (make-pairv :left 10 :right 20))
                       (p2 (copy-structure p1)))
                  (setf (aref p1 1) 99)
                  (aref p2 1)))")))
