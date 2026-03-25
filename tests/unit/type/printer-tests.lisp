;;;; tests/unit/type/printer-tests.lisp — Type Printer Tests
;;;;
;;;; Tests for src/type/printer.lisp:
;;;; type-to-string for all type-node subtypes, unparse-type,
;;;; list-interleave.

(in-package :cl-cc/test)

(defsuite printer-suite :description "Type pretty-printer tests")

;;; ─── type-to-string: basic types ───────────────────────────────────────────

(deftest printer-primitive
  "Primitives print as CL type name."
  (assert-string= "FIXNUM" (type-to-string type-int))
  (assert-string= "STRING" (type-to-string type-string)))

(deftest printer-type-var-named
  "Named type-var prints as ?name."
  (let ((v (fresh-type-var "alpha")))
    (assert-true (search "alpha" (type-to-string v)))))

(deftest printer-type-var-linked
  "Linked type-var follows the link."
  (let ((v (fresh-type-var "a")))
    (setf (cl-cc/type::type-var-link v) type-int)
    (assert-string= "FIXNUM" (type-to-string v))))

(deftest printer-rigid
  "Rigid var prints with sk prefix."
  (let ((r (fresh-rigid-var "test")))
    (assert-true (search "sk" (type-to-string r)))))

;;; ─── type-to-string: composite types ───────────────────────────────────────

(deftest printer-arrow-single-param
  "Single-param arrow: int -> string."
  (let ((s (type-to-string (make-type-arrow (list type-int) type-string))))
    (assert-true (search "FIXNUM" s))
    (assert-true (search "->" s))
    (assert-true (search "STRING" s))))

(deftest printer-arrow-multi-param
  "Multi-param arrow: (int string) -> bool."
  (let ((s (type-to-string (make-type-arrow (list type-int type-string) type-bool))))
    (assert-true (search "->" s))))

(deftest printer-arrow-with-effects
  "Arrow with effects includes effect annotation."
  (let ((arr (make-type-arrow (list type-int) type-string :effects +io-effect-row+)))
    (let ((s (type-to-string arr)))
      (assert-true (search "IO" s)))))

(deftest printer-product
  "Product type prints as (A, B)."
  (let ((s (type-to-string (make-type-product :elems (list type-int type-string)))))
    (assert-true (search "," s))))

(deftest printer-record-closed
  "Closed record: {x: int, y: string}."
  (let* ((r (make-type-record :fields (list (cons 'x type-int) (cons 'y type-string))
                              :row-var nil))
         (s (type-to-string r)))
    (assert-true (search "{" s))
    (assert-true (search "x" (string-downcase s)))))

(deftest printer-record-open
  "Open record includes row variable."
  (let* ((rv (fresh-type-var "rho"))
         (r (make-type-record :fields (list (cons 'x type-int)) :row-var rv))
         (s (type-to-string r)))
    (assert-true (search "|" s))))

(deftest printer-variant
  "Variant type prints with angle brackets."
  (let* ((v (make-type-variant :cases (list (cons 'some type-int) (cons 'none type-null))
                               :row-var nil))
         (s (type-to-string v)))
    (assert-true (search "<" s))))

(deftest printer-union
  "Union type includes | separator."
  (let ((s (type-to-string (make-type-union (list type-int type-string)))))
    (assert-true (search "|" s))))

(deftest printer-intersection
  "Intersection type includes & separator."
  (let ((s (type-to-string (make-type-intersection (list type-int type-string)))))
    (assert-true (search "&" s))))

(deftest printer-forall
  "Forall type prints with quantifier."
  (let* ((v (fresh-type-var "a"))
         (s (type-to-string (make-type-forall :var v :body type-int))))
    (assert-true (> (length s) 0))))

(deftest printer-exists
  "Exists type prints with quantifier."
  (let* ((v (fresh-type-var "a"))
         (s (type-to-string (make-type-exists :var v :body type-int))))
    (assert-true (> (length s) 0))))

(deftest printer-type-app
  "Type application prints as (F A)."
  (let ((s (type-to-string (make-type-app :fun type-int :arg type-string))))
    (assert-true (search "(" s))))

(deftest printer-type-lambda
  "Type-level lambda."
  (let* ((v (fresh-type-var "a"))
         (s (type-to-string (cl-cc/type::make-type-lambda :var v :knd nil :body type-int))))
    (assert-true (> (length s) 0))))

(deftest printer-type-mu
  "Recursive type."
  (let* ((v (fresh-type-var "a"))
         (s (type-to-string (make-type-mu :var v :body v))))
    (assert-true (> (length s) 0))))

(deftest printer-refinement
  "Refinement type includes <pred>."
  (let* ((r (cl-cc/type::make-type-refinement :base type-int :predicate nil))
         (s (type-to-string r)))
    (assert-true (search "<pred>" s))))

(deftest printer-linear
  "Linear type includes grade."
  (let* ((l (make-type-linear :base type-int :grade :one))
         (s (type-to-string l)))
    (assert-true (search "1" s))))

(deftest printer-capability
  "Capability type includes cap name."
  (let* ((c (cl-cc/type::make-type-capability :base type-int :cap 'read))
         (s (type-to-string c)))
    (assert-true (search "READ" s))))

(deftest printer-handler
  "Handler type prints in bracket notation."
  (let* ((eff (make-type-effect-op :name 'io :args nil))
         (h (cl-cc/type::make-type-handler :effect eff :input type-int :output type-string))
         (s (type-to-string h)))
    (assert-true (search "[" s))
    (assert-true (search "=>" s))))

(deftest printer-gadt-con
  "GADT constructor type includes ::."
  (let* ((gc (cl-cc/type::make-type-gadt-con
              :name 'just :arg-types (list type-int) :index-type type-any))
         (s (type-to-string gc)))
    (assert-true (search "::" s))))

;;; ─── type-to-string: effect rows ──────────────────────────────────────────

(deftest printer-effect-row-empty
  "Empty effect row prints as {}."
  (assert-string= "{}" (type-to-string +pure-effect-row+)))

(deftest printer-effect-row-closed
  "Closed effect row with effects."
  (let ((s (type-to-string +io-effect-row+)))
    (assert-true (search "IO" s))))

(deftest printer-effect-row-open
  "Open effect row with row variable."
  (let* ((rv (fresh-type-var "e"))
         (er (make-type-effect-row :effects nil :row-var rv))
         (s (type-to-string er)))
    (assert-true (search "|" s))))

(deftest printer-effect-op-with-args
  "Effect op with args: (State Int)."
  (let* ((op (make-type-effect-op :name 'state :args (list type-int)))
         (s (type-to-string op)))
    (assert-true (search "STATE" s))
    (assert-true (search "FIXNUM" s))))

;;; ─── type-to-string: constraint / qualified ───────────────────────────────

(deftest printer-type-constraint
  "Type constraint prints as (Class Type)."
  (let* ((tc (cl-cc/type::make-type-constraint :class-name 'eq :type-arg type-int))
         (s (type-to-string tc)))
    (assert-true (search "EQ" s))))

(deftest printer-type-qualified-with-constraints
  "Qualified type prints constraints => body."
  (let* ((tc (cl-cc/type::make-type-constraint :class-name 'eq :type-arg type-int))
         (q (make-type-qualified :constraints (list tc) :body type-int))
         (s (type-to-string q)))
    (assert-true (search "=>" s))))

(deftest printer-type-qualified-empty-constraints
  "Qualified type with no constraints prints just the body."
  (let* ((q (make-type-qualified :constraints nil :body type-int))
         (s (type-to-string q)))
    (assert-string= "FIXNUM" s)))

(deftest printer-type-scheme
  "Type scheme prints with quantifiers."
  (let* ((v (fresh-type-var "a"))
         (scheme (make-type-scheme (list v) v))
         (s (type-to-string scheme)))
    (assert-true (> (length s) 0))))

(deftest printer-type-error
  "Type error prints with <error: message>."
  (let ((s (type-to-string (make-type-error :message "test error"))))
    (assert-true (search "error" s))))

;;; ─── type-to-string: backward-compat ───────────────────────────────────────

(deftest printer-compat-type-class-constraint
  "Backward-compat type-class-constraint prints."
  (let* ((c (cl-cc/type::make-type-class-constraint :class-name 'num :type-arg type-int))
         (s (type-to-string c)))
    (assert-true (search "NUM" s))))

(deftest printer-compat-type-skolem
  "Backward-compat type-skolem prints."
  (let* ((sk (cl-cc/type::make-type-skolem "a"))
         (s (type-to-string sk)))
    (assert-true (search "sk" s))))

(deftest printer-compat-type-effect
  "Backward-compat type-effect prints name."
  (let* ((e (cl-cc/type::make-type-effect :name 'io))
         (s (type-to-string e)))
    (assert-string= "IO" s)))

;;; ─── unparse-type roundtrip ────────────────────────────────────────────────

(deftest unparse-primitive
  "Unparse primitive returns symbol name."
  (assert-eq 'fixnum (cl-cc/type::unparse-type type-int)))

(deftest unparse-arrow
  "Unparse arrow returns (-> params... return)."
  (let ((result (cl-cc/type::unparse-type (make-type-arrow (list type-int) type-string))))
    (assert-eq 'cl-cc/type::-> (first result))))

(deftest unparse-union
  "Unparse union returns (or ...)."
  (let ((result (cl-cc/type::unparse-type (make-type-union (list type-int type-string)))))
    (assert-eq 'or (first result))))

(deftest unparse-product
  "Unparse product returns (values ...)."
  (let ((result (cl-cc/type::unparse-type (make-type-product :elems (list type-int)))))
    (assert-eq 'values (first result))))

;;; ─── list-interleave ───────────────────────────────────────────────────────

(deftest list-interleave-basic
  "Interleave separator between elements."
  (let ((result (cl-cc/type::list-interleave '(a b c) 'x)))
    (assert-equal '(a x b x c) result)))

(deftest list-interleave-single
  "Single element list has no separator."
  (assert-equal '(a) (cl-cc/type::list-interleave '(a) 'x)))

(deftest list-interleave-empty
  "Empty list returns nil."
  (assert-null (cl-cc/type::list-interleave nil 'x)))
