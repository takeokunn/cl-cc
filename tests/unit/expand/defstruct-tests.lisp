;;;; tests/unit/expand/defstruct-tests.lisp — expand-defstruct unit tests
;;;;
;;;; Tests for the defstruct → (progn defclass defun defun) expansion.
;;;; Covers: basic structs, :conc-name, :constructor, BOA constructors,
;;;; slot defaults, predicate generation, and accessor-slot-map side effect.

(in-package :cl-cc/test)

(defsuite defstruct-suite :description "expand-defstruct unit tests")

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun ds-expand (form)
  "Shorthand: call expand-defstruct and return the expansion."
  (cl-cc::expand-defstruct form))

(defun ds-progn-forms (expansion)
  "Return the body forms of a (progn ...) expansion (cdr)."
  (rest expansion))

;;; ─── Basic struct ─────────────────────────────────────────────────────────

(deftest ds-basic-expansion
  "Basic defstruct: is progn, generates defclass with correct name, slot count, initarg, and initform."
  (let* ((exp         (ds-expand '(defstruct point x y)))
         (defclass-form (second exp))
         (slot-specs  (fourth defclass-form))
         (first-slot  (first slot-specs)))
    (assert-equal 'progn   (first exp))
    (assert-equal 'defclass (first defclass-form))
    (assert-equal 'point    (second defclass-form))
    (assert-equal 2         (length slot-specs))
    (assert-equal :x        (getf (rest first-slot) :initarg))
    (assert-equal nil       (getf (rest first-slot) :initform))))

(deftest ds-basic-constructor
  "Default constructor is MAKE-<name> and its body calls make-instance."
  (let* ((exp   (ds-expand '(defstruct point x y)))
         (forms (ds-progn-forms exp))
         (ctor  (second forms))
         (body  (fourth ctor)))
    (assert-equal 'defun              (first ctor))
    (assert-equal (intern "MAKE-POINT") (second ctor))
    (assert-equal 'make-instance      (first body))))

(deftest ds-basic-predicate
  "Basic defstruct generates a predicate POINT-P."
  (let* ((exp (ds-expand '(defstruct point x y)))
         (forms (ds-progn-forms exp))
         (pred (third forms)))
    (assert-equal 'defun (first pred))
    (assert-equal (intern "POINT-P") (second pred))))

(deftest ds-basic-returns-name
  "Expansion ends with quoted struct name."
  (let* ((exp (ds-expand '(defstruct point x y)))
         (forms (ds-progn-forms exp))
         (last-form (car (last forms))))
    (assert-equal 'quote (first last-form))
    (assert-equal 'point (second last-form))))

;;; ─── Slot defaults ────────────────────────────────────────────────────────

(deftest ds-slot-with-default
  "Slot specified as (name default) gets correct initform."
  (let* ((exp (ds-expand '(defstruct config (timeout 30) (verbose nil))))
         (defclass-form (second exp))
         (slot-specs (fourth defclass-form))
         (first-slot (first slot-specs)))
    (assert-equal 30 (getf (rest first-slot) :initform))))

;;; ─── :conc-name option ────────────────────────────────────────────────────

(deftest ds-conc-name-behavior
  "conc-name: custom prefix and default NAME- prefix are applied correctly."
  (let* ((exp-custom  (ds-expand '(defstruct (point (:conc-name pt-)) x y)))
         (slot-custom (first (fourth (second exp-custom))))
         (exp-default (ds-expand '(defstruct point x)))
         (slot-default (first (fourth (second exp-default)))))
    (assert-equal (intern "PT-X")    (getf (rest slot-custom)  :accessor))
    (assert-equal (intern "POINT-X") (getf (rest slot-default) :accessor))))

;;; ─── :constructor option ──────────────────────────────────────────────────

(deftest ds-custom-constructor-name
  ":constructor option changes constructor name."
  (let* ((exp (ds-expand '(defstruct (point (:constructor new-point)) x y)))
         (forms (ds-progn-forms exp))
         (ctor (second forms)))
    (assert-equal "NEW-POINT" (symbol-name (second ctor)))))

(deftest ds-boa-constructor
  "BOA constructor uses positional args, not &key."
  (let* ((exp (ds-expand '(defstruct (point (:constructor make-pt (x y))) x y)))
         (forms (ds-progn-forms exp))
         (ctor (second forms))
         (params (third ctor)))
    (assert-equal 2 (length params))
    (assert-equal "X" (symbol-name (first params)))
    (assert-equal "Y" (symbol-name (second params)))))

;;; ─── Docstring filtering ──────────────────────────────────────────────────

(deftest ds-docstring-ignored
  "Docstring in slot list is filtered out."
  (let* ((exp (ds-expand '(defstruct point "A 2D point." x y)))
         (defclass-form (second exp))
         (slot-specs (fourth defclass-form)))
    (assert-equal 2 (length slot-specs))))

;;; ─── *accessor-slot-map* side effect ──────────────────────────────────────

(deftest ds-accessor-slot-map-populated
  "expand-defstruct populates *accessor-slot-map* for each accessor."
  (let ((cl-cc::*accessor-slot-map* (make-hash-table :test #'eq)))
    (ds-expand '(defstruct widget width height))
    (let ((entry (gethash (intern "WIDGET-WIDTH") cl-cc::*accessor-slot-map*)))
      (assert-true (not (null entry)))
      (assert-equal 'widget (car entry))
      (assert-equal 'width (cdr entry)))))

;;; ─── Empty struct ─────────────────────────────────────────────────────────

(deftest ds-empty-struct
  "Struct with no slots generates empty defclass slots and &key constructor."
  (let* ((exp (ds-expand '(defstruct empty)))
         (defclass-form (second exp))
         (slot-specs (fourth defclass-form)))
    (assert-equal 0 (length slot-specs))))
