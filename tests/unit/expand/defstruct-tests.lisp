;;;; tests/unit/expand/defstruct-tests.lisp — expand-defstruct unit tests
;;;;
;;;; Tests for the defstruct → (progn defclass defun defun) expansion.
;;;; Covers: basic structs, :conc-name, :constructor, BOA constructors,
;;;; slot defaults, predicate generation, and accessor-slot-map side effect.

(in-package :cl-cc/test)

(defsuite defstruct-suite :description "expand-defstruct unit tests"
  :parent cl-cc-suite)


(in-suite defstruct-suite)
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

(deftest-each ds-conc-name-behavior
  "conc-name: custom prefix and default NAME- prefix are applied correctly."
  :cases (("custom"  '(defstruct (point (:conc-name pt-)) x y)  "PT-X")
          ("default" '(defstruct point x)                        "POINT-X"))
  (form expected-accessor-name)
  (let* ((exp  (ds-expand form))
         (slot (first (fourth (second exp)))))
    (assert-equal (intern expected-accessor-name) (getf (rest slot) :accessor))))

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

(deftest-each ds-deriving-registers-typeclass-instances
  "defstruct :deriving emits the registration hook and registers instances on eval."
  :cases (("eq"   'eq)
          ("show" 'show)
          ("ord"  'ord))
  (tc-name)
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal))
        (name (gensym "DERIVING-POINT-")))
    (eval (ds-expand `(defstruct (,name (:deriving eq show ord)) x y)))
    (assert-true (has-typeclass-instance-p tc-name name))))

;;; ─── Empty struct ─────────────────────────────────────────────────────────

(deftest ds-empty-struct
  "Struct with no slots generates empty defclass slots and &key constructor."
  (let* ((exp (ds-expand '(defstruct empty)))
         (defclass-form (second exp))
         (slot-specs (fourth defclass-form)))
    (assert-equal 0 (length slot-specs))))

;;; ─── %defstruct-extract-boa-parts ────────────────────────────────────────

(deftest-each ds-extract-boa-parts-normal-only
  "BOA lambda list with no &aux: all params go to normal, aux is empty."
  :cases (("empty"  nil           '(nil . nil))
          ("single" '(x)          '((x) . nil))
          ("two"    '(x y)        '((x y) . nil)))
  (boa-args expected)
  (let ((result (cl-cc::%defstruct-extract-boa-parts boa-args)))
    (assert-equal (car expected) (car result))
    (assert-equal (cdr expected) (cdr result))))

(deftest ds-extract-boa-parts-with-aux
  "BOA lambda list with &aux: splits correctly into normal + aux bindings."
  (let ((result (cl-cc::%defstruct-extract-boa-parts '(x y &aux (z 0) (w 1)))))
    (assert-equal '(x y) (car result))
    (assert-equal '((z 0) (w 1)) (cdr result))))

(deftest ds-extract-boa-parts-bare-aux
  "Bare &aux symbol (no binding) is promoted to (sym nil) pair."
  (let ((result (cl-cc::%defstruct-extract-boa-parts '(x &aux z))))
    (assert-equal '(x) (car result))
    (assert-equal '((z nil)) (cdr result))))

;;; ─── %defstruct-boa-param-names ──────────────────────────────────────────

(deftest ds-boa-param-names-simple
  "BOA param names extracts plain symbols."
  (assert-equal '(x y z) (cl-cc::%defstruct-boa-param-names '(x y z))))

(deftest ds-boa-param-names-skips-lambda-keywords
  "BOA param names skips &key, &optional, &rest lambda list keywords."
  (let ((result (cl-cc::%defstruct-boa-param-names '(&optional x &rest y))))
    ;; x and y are extracted, not the keywords
    (assert-equal '(x y) result)))

(deftest ds-boa-param-names-empty
  "BOA param names on empty list returns nil."
  (assert-null (cl-cc::%defstruct-boa-param-names nil)))

;;; ─── :type list / :type vector (FR-546) ─────────────────────────────────

(defun %ds-tree-member (item tree)
  "Return T if ITEM appears anywhere in the nested list TREE."
  (cond ((null tree) nil)
        ((eq tree item) t)
        ((atom tree) nil)
        (t (or (%ds-tree-member item (car tree))
               (%ds-tree-member item (cdr tree))))))

(deftest ds-type-list-constructor-uses-list
  "(:type list) defstruct: constructor body uses LIST."
  (let* ((exp   (ds-expand '(defstruct (point (:type list)) x y)))
         (forms (ds-progn-forms exp))
         (ctor  (first forms))  ; no defclass for typed form
         (body  (fourth ctor)))
    ;; The constructor should produce a list, not make-instance
    (assert-equal 'defun (first ctor))
    (assert-true (%ds-tree-member 'list body))))

(deftest ds-type-list-predicate-checks-listp
  "(:type list) defstruct: predicate tests with LISTP."
  (let* ((exp   (ds-expand '(defstruct (rect (:type list) (:predicate rect-list-p))
                              width height)))
         (forms (ds-progn-forms exp))
         (pred  (find-if (lambda (f) (and (listp f)
                                          (eq (first f) 'defun)
                                          (eq (second f) 'rect-list-p)))
                         forms)))
    (assert-true (not (null pred)))
    (assert-true (%ds-tree-member 'listp pred))))

(deftest ds-type-vector-constructor-uses-vector
  "(:type vector) defstruct: constructor body uses VECTOR."
  (let* ((exp  (ds-expand '(defstruct (seg (:type vector)) a b)))
         (forms (ds-progn-forms exp))
         (ctor  (first forms))
         (body  (fourth ctor)))
    (assert-equal 'defun (first ctor))
    (assert-true (%ds-tree-member 'vector body))))
