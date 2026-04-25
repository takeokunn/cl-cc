;;;; tests/unit/expand/defstruct-tests.lisp — expand-defstruct unit tests
;;;;
;;;; Tests for the defstruct → (progn defclass defun defun) expansion.
;;;; Covers: basic structs, :conc-name, :constructor, BOA constructors,
;;;; slot defaults, predicate generation, and accessor-slot-map side effect.

(in-package :cl-cc/test)

(defsuite defstruct-suite :description "expand-defstruct unit tests"
  :parent cl-cc-unit-suite)


(in-suite defstruct-suite)
;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun ds-expand (form)
  "Shorthand: call expand-defstruct and return the expansion."
  (cl-cc/expand::expand-defstruct form))

(defun ds-progn-forms (expansion)
  "Return the body forms of a (progn ...) expansion (cdr)."
  (rest expansion))

(defun ds-assert-deriving-registers (typeclass-name)
  "Assert that a :deriving defstruct registers TYPECLASS-NAME when evaluated."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal))
        (name (gensym "DERIVING-POINT-")))
    (eval (ds-expand `(defstruct (,name (:deriving eq show ord)) x y)))
    (assert-true (has-typeclass-instance-p typeclass-name name))))

;;; ─── Basic struct ─────────────────────────────────────────────────────────

(deftest ds-basic-expansion-cases
  "Basic defstruct: progn with defclass; default constructor calls make-instance; predicate generated; ends with quoted name."
  (let* ((exp         (ds-expand '(defstruct point x y)))
         (defclass-form (second exp))
         (slot-specs  (fourth defclass-form))
         (first-slot  (first slot-specs)))
    (assert-equal 'progn   (first exp))
    (assert-equal 'defclass (first defclass-form))
    (assert-equal 'point    (second defclass-form))
    (assert-equal 2         (length slot-specs))
    (assert-equal :x        (getf (rest first-slot) :initarg))
    (assert-equal nil       (getf (rest first-slot) :initform)))
  (let* ((exp   (ds-expand '(defstruct point x y)))
         (forms (ds-progn-forms exp))
         (ctor  (second forms))
         (body  (fourth ctor)))
    (assert-equal 'defun              (first ctor))
    (assert-equal (intern "MAKE-POINT") (second ctor))
    (assert-equal 'make-instance      (first body)))
  (let* ((exp (ds-expand '(defstruct point x y)))
         (forms (ds-progn-forms exp))
         (pred (third forms)))
    (assert-equal 'defun (first pred))
    (assert-equal (intern "POINT-P") (second pred)))
  (let* ((exp (ds-expand '(defstruct point x y)))
         (forms (ds-progn-forms exp))
         (last-form (car (last forms))))
    (assert-equal 'quote (first last-form))
    (assert-equal 'point (second last-form))))

;;; ─── Slot defaults and filtering ─────────────────────────────────────────

(deftest ds-slot-options-cases
  "Slot (name default) gets correct initform; docstring in slot list is filtered out."
  (let* ((exp (ds-expand '(defstruct config (timeout 30) (verbose nil))))
         (defclass-form (second exp))
         (slot-specs (fourth defclass-form))
         (first-slot (first slot-specs)))
    (assert-equal 30 (getf (rest first-slot) :initform)))
  (let* ((exp (ds-expand '(defstruct point "A 2D point." x y)))
         (defclass-form (second exp))
         (slot-specs (fourth defclass-form)))
    (assert-equal 2 (length slot-specs))))

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

(deftest ds-constructor-options-cases
  ":constructor renames the constructor; BOA constructor uses positional args not &key."
  (let* ((exp (ds-expand '(defstruct (point (:constructor new-point)) x y)))
         (forms (ds-progn-forms exp))
         (ctor (second forms)))
    (assert-equal "NEW-POINT" (symbol-name (second ctor))))
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

(deftest ds-accessor-and-empty-cases
  "*accessor-slot-map* populated for each accessor; empty struct has 0 slots."
  (let ((cl-cc/expand::*accessor-slot-map* (make-hash-table :test #'eq)))
    (ds-expand '(defstruct widget width height))
    (let ((entry (gethash (intern "WIDGET-WIDTH") cl-cc/expand::*accessor-slot-map*)))
      (assert-true (not (null entry)))
      (assert-equal 'widget (car entry))
      (assert-equal 'width (cdr entry))))
  (let* ((exp (ds-expand '(defstruct empty)))
         (defclass-form (second exp))
         (slot-specs (fourth defclass-form)))
    (assert-equal 0 (length slot-specs))))

(deftest-each ds-deriving-registers-typeclass-instances
  "defstruct :deriving emits the registration hook and registers instances on eval."
  :cases (("eq"   'eq)
           ("show" 'show)
           ("ord"  'ord))
  (tc-name)
  (ds-assert-deriving-registers tc-name))

;;; ─── %defstruct-extract-boa-parts ────────────────────────────────────────

(deftest-each ds-extract-boa-parts-normal-only
  "BOA lambda list with no &aux: all params go to normal, aux is empty."
  :cases (("empty"  nil           '(nil . nil))
          ("single" '(x)          '((x) . nil))
          ("two"    '(x y)        '((x y) . nil)))
  (boa-args expected)
  (let ((result (cl-cc/expand::%defstruct-extract-boa-parts boa-args)))
    (assert-equal (car expected) (car result))
    (assert-equal (cdr expected) (cdr result))))

(deftest ds-extract-boa-parts-edge-cases
  "&aux bindings split into normal+aux; bare &aux symbol promoted to (sym nil) pair."
  (let ((result (cl-cc/expand::%defstruct-extract-boa-parts '(x y &aux (z 0) (w 1)))))
    (assert-equal '(x y) (car result))
    (assert-equal '((z 0) (w 1)) (cdr result)))
  (let ((result (cl-cc/expand::%defstruct-extract-boa-parts '(x &aux z))))
    (assert-equal '(x) (car result))
    (assert-equal '((z nil)) (cdr result))))

;;; ─── %defstruct-boa-param-names ──────────────────────────────────────────

(deftest-each ds-boa-param-names-cases
  "BOA param names: extracts plain symbols; skips lambda keywords; returns nil for empty."
  :cases (("simple"         '(x y z)               '(x y z))
          ("skips-keywords" '(&optional x &rest y)  '(x y))
          ("empty"          nil                      nil))
  (params expected)
  (assert-equal expected (cl-cc/expand::%defstruct-boa-param-names params)))

;;; ─── :type list / :type vector (FR-546) ─────────────────────────────────

(defun %ds-tree-member (item tree)
  "Return T if ITEM appears anywhere in the nested list TREE."
  (cond ((null tree) nil)
        ((eq tree item) t)
        ((atom tree) nil)
        (t (or (%ds-tree-member item (car tree))
               (%ds-tree-member item (cdr tree))))))

(deftest-each ds-type-typed-constructor-cases
  "(:type list/vector) defstruct: constructor body uses the appropriate collection constructor."
  :cases (("list"   '(defstruct (point (:type list)) x y)   'list)
          ("vector" '(defstruct (seg (:type vector)) a b)   'vector))
  (form expected-ctor)
  (let* ((ctor (first (ds-progn-forms (ds-expand form)))))
    (assert-equal 'defun (first ctor))
    (assert-true (%ds-tree-member expected-ctor (fourth ctor)))))

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

;;; ─── %defstruct-resolve-slot-values ──────────────────────────────────────

(deftest-each ds-resolve-slot-values-cases
  "%defstruct-resolve-slot-values: uses slot name when bound, default when unbound."
  :cases (("all-bound"    '((x 0) (y 0)) '(x y)    '(x y))
          ("none-bound"   '((x 0) (y 1)) '()        '(0 1))
          ("partial"      '((x 0) (y 1)) '(x)       '(x 1)))
  (all-slots bound-names expected)
  (assert-equal expected
                (cl-cc/expand::%defstruct-resolve-slot-values all-slots bound-names)))

;;; ─── %defstruct-build-constructor ────────────────────────────────────────

(deftest ds-build-constructor-keyword-form
  "%defstruct-build-constructor with no BOA args builds (defun ctor (&key ...) body)."
  (let* ((slots '((x 0) (y 1)))
         (result (cl-cc/expand::%defstruct-build-constructor
                  'make-pt nil slots
                  (lambda (svs) (cons 'list svs)))))
    (assert-equal 'defun (first result))
    (assert-equal 'make-pt (second result))
    (assert-equal '(&key (x 0) (y 1)) (third result))
    (assert-equal '(list x y) (fourth result))))

(deftest ds-build-constructor-boa-form
  "%defstruct-build-constructor with BOA args builds (defun ctor params (let* aux body))."
  (let* ((slots '((x 0) (y 0)))
         (result (cl-cc/expand::%defstruct-build-constructor
                  'make-pt '(a b) slots
                  (lambda (svs) (cons 'list svs)))))
    (assert-equal 'defun (first result))
    (assert-equal 'make-pt (second result))
    (assert-equal '(a b) (third result))
    (assert-equal 'let* (car (fourth result)))))
