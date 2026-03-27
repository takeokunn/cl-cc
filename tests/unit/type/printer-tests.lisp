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

(deftest-each printer-binary-separator-types
  "Union includes | separator; intersection includes & separator."
  :cases (("union"        (make-type-union        (list type-int type-string)) "|")
          ("intersection" (make-type-intersection (list type-int type-string)) "&"))
  (ty expected-sep)
  (assert-true (search expected-sep (type-to-string ty))))

(deftest-each printer-quantified-types
  "Forall and exists types each produce non-empty strings."
  :cases (("forall" (let ((v (fresh-type-var "a"))) (make-type-forall :var v :body type-int)))
          ("exists" (let ((v (fresh-type-var "a"))) (make-type-exists :var v :body type-int))))
  (ty)
  (assert-true (> (length (type-to-string ty)) 0)))

(deftest printer-type-app
  "Type application prints as (F A)."
  (let ((s (type-to-string (make-type-app :fun type-int :arg type-string))))
    (assert-true (search "(" s))))

(deftest printer-binder-types
  "Type-lambda and mu (recursive) types each produce non-empty strings."
  (let ((v (fresh-type-var "a")))
    (assert-true (> (length (type-to-string (cl-cc/type::make-type-lambda :var v :knd nil :body type-int))) 0))
    (assert-true (> (length (type-to-string (make-type-mu :var v :body v))) 0))))

(deftest-each printer-wrapper-type-annotations
  "Refinement shows <pred>; linear shows grade; capability shows cap name."
  :cases (("refinement"  (cl-cc/type::make-type-refinement :base type-int :predicate nil) "<pred>")
          ("linear"      (make-type-linear :base type-int :grade :one)                    "1")
          ("capability"  (cl-cc/type::make-type-capability :base type-int :cap 'read)     "READ"))
  (ty expected-fragment)
  (assert-true (search expected-fragment (type-to-string ty))))

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

(deftest printer-effect-rows-and-ops
  "Effect rows: pure={}, io row has IO, open row has |; effect op with args shows name and arg."
  (assert-string= "{}" (type-to-string +pure-effect-row+))
  (assert-true (search "IO" (type-to-string +io-effect-row+)))
  (let* ((rv (fresh-type-var "e"))
         (er (make-type-effect-row :effects nil :row-var rv)))
    (assert-true (search "|" (type-to-string er))))
  (let* ((op (make-type-effect-op :name 'state :args (list type-int)))
         (s  (type-to-string op)))
    (assert-true (search "STATE"  s))
    (assert-true (search "FIXNUM" s))))

;;; ─── type-to-string: constraint / qualified ───────────────────────────────

(deftest printer-constraint-and-qualified
  "Constraint prints class name; qualified with constraints includes =>; qualified with no constraints prints just body."
  (let ((tc (cl-cc/type::make-type-constraint :class-name 'eq :type-arg type-int)))
    (assert-true (search "EQ" (type-to-string tc)))
    (let ((s (type-to-string (make-type-qualified :constraints (list tc) :body type-int))))
      (assert-true (search "=>" s)))
    (let ((s (type-to-string (make-type-qualified :constraints nil :body type-int))))
      (assert-string= "FIXNUM" s))))

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

(deftest printer-backward-compat-types
  "Backward-compat printer: type-class-constraint shows class name; type-skolem shows 'sk'; type-effect shows name."
  (assert-true  (search "NUM" (type-to-string (cl-cc/type::make-type-class-constraint :class-name 'num :type-arg type-int))))
  (assert-true  (search "sk"  (type-to-string (cl-cc/type::make-type-skolem "a"))))
  (assert-string= "IO"        (type-to-string (cl-cc/type::make-type-effect :name 'io))))

;;; ─── unparse-type roundtrip ────────────────────────────────────────────────

(deftest unparse-type-forms
  "unparse-type: primitive→symbol; arrow→(->...); union→(or...); product→(values...)."
  (assert-eq 'fixnum (cl-cc/type::unparse-type type-int))
  (assert-eq 'cl-cc/type::-> (first (cl-cc/type::unparse-type (make-type-arrow (list type-int) type-string))))
  (assert-eq 'or     (first (cl-cc/type::unparse-type (make-type-union (list type-int type-string)))))
  (assert-eq 'values (first (cl-cc/type::unparse-type (make-type-product :elems (list type-int))))))

;;; ─── list-interleave ───────────────────────────────────────────────────────

(deftest list-interleave-behavior
  "list-interleave: inserts separator between elements; single element unchanged; empty returns nil."
  (assert-equal '(a x b x c) (cl-cc/type::list-interleave '(a b c) 'x))
  (assert-equal '(a)         (cl-cc/type::list-interleave '(a) 'x))
  (assert-null               (cl-cc/type::list-interleave nil 'x)))
