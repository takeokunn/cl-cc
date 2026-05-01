;;;; tests/unit/expand/expander-defclass-tests.lisp — Defclass expander tests

(in-package :cl-cc/test)

(defsuite expander-defclass-suite :description "Defclass expander unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-defclass-suite)
(deftest expand-defclass-slot-spec-bare-symbol
  "expand-defclass-slot-spec passes through a bare symbol unchanged."
  (assert-eq 'foo (cl-cc/expand::expand-defclass-slot-spec 'foo)))

(deftest expand-defclass-slot-spec-no-initform-preserved
  "expand-defclass-slot-spec with no :initform leaves all keys untouched."
  (let ((result (cl-cc/expand::expand-defclass-slot-spec
                 '(x :initarg :x :accessor x-accessor))))
    (assert-eq 'x (first result))
    (assert-eq :initarg (second result))
    (assert-eq :x (third result))
    (assert-eq :accessor (fourth result))
    (assert-eq 'x-accessor (fifth result))))

(deftest expand-defclass-slot-spec-expands-initform
  "expand-defclass-slot-spec macro-expands the :initform value; key is preserved."
  (let ((result (cl-cc/expand::expand-defclass-slot-spec
                 '(x :initarg :x :initform (+ 1 2)))))
    (assert-true (member :initform result))))

(deftest expand-defclass-slot-spec-non-initform-keys-untouched
  "expand-defclass-slot-spec does not expand :type or :accessor values."
  (let* ((spec '(x :type integer :accessor get-x :initform 0))
         (result (cl-cc/expand::expand-defclass-slot-spec spec)))
    (assert-eq 'integer (getf (rest result) :type))))
