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

;;; FR-446: defstruct :copier option integration tests

(deftest fr-446-copier-default-clos
  "Default copier COPY-POINT generates an independent shallow copy."
  (assert-= 10
            (run-string
             "(progn
                (defstruct point x y)
                (let* ((p1 (make-point :x 10 :y 20))
                       (p2 (copy-point p1)))
                  (setf (point-x p1) 99)
                  (point-x p2)))")))

(deftest fr-446-copier-nil-suppressed
  ":copier nil means COPY-POINT is not defined."
  (assert-eq 'error
             (run-string
              "(progn
                 (defstruct (point (:copier nil)) x y)
                 (handler-case
                     (progn (copy-point (make-point :x 1 :y 2)) :no-error)
                   (undefined-function () 'error)))")))

(deftest fr-446-copier-custom-name
  ":copier custom-name defines the copier under the custom name."
  (assert-= 42
            (run-string
             "(progn
                (defstruct (widget (:copier clone-widget)) value)
                (let* ((w1 (make-widget :value 42))
                       (w2 (clone-widget w1)))
                  (setf (widget-value w1) 0)
                  (widget-value w2)))")))

(deftest fr-446-copier-type-list
  "Default copier works for :type list defstructs."
  (assert-= 10
            (run-string
             "(progn
                (defstruct (pair (:type list)) left right)
                (let* ((p1 (make-pair :left 10 :right 20))
                       (p2 (copy-pair p1)))
                  (setf (first (cdr p1)) 99)
                  (first (cdr p2))))")))

(deftest fr-446-copier-type-vector
  "Default copier works for :type vector defstructs."
  (assert-= 10
            (run-string
             "(progn
                (defstruct (vec3 (:type vector)) x y z)
                (let* ((v1 (make-vec3 :x 10 :y 20 :z 30))
                       (v2 (copy-vec3 v1)))
                  (setf (aref v1 1) 99)
                  (aref v2 1)))")))
