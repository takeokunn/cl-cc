;;;; tests/unit/expand/expander-defstruct-typed-tests.lisp
;;;; Coverage for expander-defstruct-typed.lisp helpers.

(in-package :cl-cc/test)

(defsuite expander-defstruct-typed-suite
  :description ":type list/vector defstruct expansion helper tests"
  :parent cl-cc-unit-suite)

(in-suite expander-defstruct-typed-suite)

;;; ── %defstruct-typed-container-form ─────────────────────────────────────

(deftest-each typed-container-form-dispatcher
  "%defstruct-typed-container-form uses LIST for :type list and VECTOR for :type vector."
  :cases (("list"   'list   'my-struct '(a b) 'list)
          ("vector" 'vector 'my-struct '(a b) 'vector))
  (struct-type name slot-values expected-head)
  (let ((form (cl-cc/expand::%defstruct-typed-container-form struct-type name slot-values)))
    (assert-eq expected-head (first form))
    (assert-equal (list 'quote name) (second form))
    (assert-equal slot-values (cddr form))))

;;; ── %defstruct-typed-constructor ────────────────────────────────────────

(deftest-each typed-constructor-is-defun
  "%defstruct-typed-constructor generates a DEFUN form."
  :cases (("list"   'list)
          ("vector" 'vector))
  (struct-type)
  (cl-cc/expand:with-fresh-defstruct-registries
    (let ((form (cl-cc/expand::%defstruct-typed-constructor
                 'make-pt 'pt struct-type nil '((x 0) (y 0)))))
      (assert-eq 'defun (first form))
      (assert-eq 'make-pt (second form)))))

(deftest typed-constructor-list-body-uses-list
  "%defstruct-typed-constructor :type list body includes LIST call."
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((form (cl-cc/expand::%defstruct-typed-constructor
                  'make-pt 'pt 'list nil '((x 0) (y 0))))
           (body (fourth form)))
      (assert-true (member 'list body)))))

(deftest typed-constructor-vector-body-uses-vector
  "%defstruct-typed-constructor :type vector body includes VECTOR call."
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((form (cl-cc/expand::%defstruct-typed-constructor
                  'make-seg 'seg 'vector nil '((a 0) (b 0))))
           (body (fourth form)))
      (assert-true (member 'vector body)))))

;;; ── %defstruct-typed-accessors ──────────────────────────────────────────

(deftest typed-accessors-list-uses-nth
  "%defstruct-typed-accessors for :type list uses NTH for slot access."
  (let ((forms (cl-cc/expand::%defstruct-typed-accessors 'list 'pt- '((x 0) (y 0)))))
    (assert-= 2 (length forms))
    (assert-eq 'defun (first (first forms)))
    (assert-true (member 'nth (fourth (first forms))))))

(deftest typed-accessors-vector-uses-aref
  "%defstruct-typed-accessors for :type vector uses AREF for slot access."
  (let ((forms (cl-cc/expand::%defstruct-typed-accessors 'vector 'pt- '((x 0) (y 0)))))
    (assert-= 2 (length forms))
    (assert-true (member 'aref (fourth (first forms))))))

(deftest typed-accessors-count-matches-slots
  "%defstruct-typed-accessors generates one accessor per slot."
  (let ((forms (cl-cc/expand::%defstruct-typed-accessors 'list 'r- '((a 0) (b 0) (c 0)))))
    (assert-= 3 (length forms))))

;;; ── %defstruct-typed-predicate ──────────────────────────────────────────

(deftest typed-predicate-list-uses-listp
  "%defstruct-typed-predicate for :type list body uses LISTP."
  (let* ((form (cl-cc/expand::%defstruct-typed-predicate 'pt-p 'pt 'list 2))
         (body (fourth form)))
    (assert-eq 'defun (first form))
    (assert-eq 'pt-p (second form))
    (assert-eq 'listp (first (second body)))))

(deftest typed-predicate-vector-uses-vectorp
  "%defstruct-typed-predicate for :type vector body uses VECTORP."
  (let* ((form (cl-cc/expand::%defstruct-typed-predicate 'seg-p 'seg 'vector 2))
         (body (fourth form)))
    (assert-eq 'defun (first form))
    (assert-eq 'vectorp (first (second body)))))

(deftest typed-predicate-nil-name-returns-nil
  "%defstruct-typed-predicate returns NIL when pred-name is NIL."
  (assert-eq nil (cl-cc/expand::%defstruct-typed-predicate nil 'pt 'list 2)))

;;; ── %defstruct-typed-expansion-forms ────────────────────────────────────

(deftest typed-expansion-forms-includes-quote-name
  "%defstruct-typed-expansion-forms always ends with (quote name)."
  (let ((forms (cl-cc/expand::%defstruct-typed-expansion-forms
                'pt nil '() nil nil)))
    (assert-equal '(quote pt) (car (last forms)))))

(deftest typed-expansion-forms-ctor-is-first-when-present
  "%defstruct-typed-expansion-forms places ctor-form first when non-nil."
  (let* ((ctor  '(defun make-pt (&key x y) (list 'pt x y)))
         (forms (cl-cc/expand::%defstruct-typed-expansion-forms 'pt ctor '() nil nil)))
    (assert-equal ctor (first forms))))

(deftest typed-expansion-forms-pred-precedes-quote
  "%defstruct-typed-expansion-forms places predicate before the trailing quote."
  (let* ((pred  '(defun pt-p (obj) (listp obj)))
         (forms (cl-cc/expand::%defstruct-typed-expansion-forms 'pt nil '() pred nil)))
    (assert-equal pred (first (last forms 2)))))

;;; ── %defstruct-typed-expansion (integration) ────────────────────────────

(deftest-each typed-expansion-integration
  "%defstruct-typed-expansion builds a PROGN for :type list/vector structs."
  :cases (("list"   '(defstruct (point (:type list)) x y))
          ("vector" '(defstruct (point (:type vector)) x y)))
  (form)
  (cl-cc/expand:with-fresh-defstruct-registries
    (let ((exp (cl-cc/expand::expand-defstruct form)))
      (assert-eq 'progn (first exp))
      (assert-true (> (length (rest exp)) 1)))))
