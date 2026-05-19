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

;;; FR-546: defstruct :type/:conc-name/slot option integration tests

(defsuite fr-546-defstruct-type-slot-options-suite
  :description "FR-546 defstruct :type/:conc-name/slot option integration tests"
  :parent cl-cc-integration-suite)

(in-suite fr-546-defstruct-type-slot-options-suite)

(deftest fr-546-conc-name-nil-accessor
  ":conc-name nil exposes bare slot-name accessors."
  (assert-= 7
            (run-string
             "(progn
                (defstruct (point546 (:conc-name nil)) x y)
                (let ((p (make-point546 :x 7 :y 8)))
                  (x p)))")))

(deftest fr-546-type-list-accessor-setf
  "(:type list) defstruct writable accessors support SETF."
  (assert-= 30
            (run-string
             "(progn
                (defstruct (pair546 (:type list)) left right)
                (let ((p (make-pair546 :left 10 :right 20)))
                  (setf (pair546-left p) 30)
                  (pair546-left p)))")))

(deftest fr-546-type-vector-accessor-setf
  "(:type vector) defstruct writable accessors support SETF."
  (assert-= 40
            (run-string
             "(progn
                (defstruct (vec546 (:type vector)) left right)
                (let ((p (make-vec546 :left 10 :right 20)))
                  (setf (vec546-left p) 40)
                  (vec546-left p)))")))

(deftest fr-546-type-read-only-accessor-setf-rejected
  "(:type list) read-only accessors reject SETF."
  (assert-signals
   error
   (run-string
    "(progn
       (defstruct (ro546 (:type list)) (left 10 :read-only t) right)
       (let ((p (make-ro546 :left 10 :right 20)))
         (setf (ro546-left p) 30)))")))

(deftest fr-546-run-string-isolates-typed-setf-handler
  "A typed defstruct SETF handler from one run-string must not affect a later CLOS-backed defstruct."
  (assert-= 11
            (run-string
             "(progn
                (defstruct (pairiso (:type vector)) left right)
                (let ((p (make-pairiso :left 1 :right 2)))
                  (setf (pairiso-left p) 11)
                  (pairiso-left p)))"))
  (assert-= 22
            (run-string
             "(progn
                (defstruct pairiso left right)
                (let ((p (make-pairiso :left 1 :right 2)))
                  (setf (pairiso-left p) 22)
                  (pairiso-left p)))")))

(deftest fr-546-run-string-isolates-read-only-accessor-map
  "A read-only accessor from one run-string must not make a later CLOS-backed accessor read-only."
  (assert-signals
   error
   (run-string
    "(progn
       (defstruct (roiso (:type list)) (left 1 :read-only t) right)
       (let ((p (make-roiso :left 1 :right 2)))
         (setf (roiso-left p) 9)))"))
  (assert-= 33
            (run-string
             "(progn
                (defstruct roiso left right)
                (let ((p (make-roiso :left 1 :right 2)))
                  (setf (roiso-left p) 33)
                  (roiso-left p)))")))
