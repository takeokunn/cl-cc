;;;; tests/unit/type/printer-tests.lisp — Type Printer Tests
;;;;
;;;; Tests for src/type/printer.lisp:
;;;; type-to-string for all type-node subtypes, unparse-type,
;;;; list-interleave, looks-like-type-specifier-p.
;;;; Coverage goal: every defmethod clause + every data table entry.

(in-package :cl-cc/test)

(defsuite printer-suite :description "Type pretty-printer tests"
  :parent cl-cc-unit-suite)


(in-suite printer-suite)
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

;;; ─── type-to-string: edge cases not covered above ────────────────────────

(deftest printer-null-type
  "Null object (not type-null) prints as NIL."
  (assert-string= "NIL" (type-to-string nil)))

(deftest printer-type-var-unnamed
  "Unnamed, unlinked type-var prints as ?tN."
  (let* ((v (fresh-type-var nil)))
    (let ((s (type-to-string v)))
      (assert-true (search "?t" s)))))

(deftest printer-type-rigid-unnamed
  "Unnamed rigid var prints as skN with no brackets."
  (let* ((r (fresh-rigid-var nil))
         (s (type-to-string r)))
    (assert-true  (search "sk" s))
    (assert-false (search "["  s))))

(deftest printer-type-unknown
  "type-unknown (a deftype alias over type-error) prints as ?."
  ;; type-unknown-p checks (type-error-p x) && message == "unknown"
  (assert-string= "?" (type-to-string (make-type-unknown)))
  ;; A real type-error with a different message still shows <error: …>
  (assert-true (search "error" (type-to-string (make-type-error :message "boom")))))

(deftest printer-arrow-mult-zero
  "Arrow with :zero multiplicity uses -0-> arrow."
  (let ((arr (make-type-arrow (list type-int) type-string :mult :zero)))
    (assert-true (search "-0->" (type-to-string arr)))))

(deftest printer-arrow-mult-one
  "Arrow with :one multiplicity uses -1-> arrow."
  (let ((arr (make-type-arrow (list type-int) type-string :mult :one)))
    (assert-true (search "-1->" (type-to-string arr)))))

(deftest printer-fallback-unknown-type
  "Fallback defmethod for unrecognised objects includes #<type ...>."
  ;; Use a plain struct that is not a type-node subclass
  (let ((s (type-to-string (make-hash-table))))
    (assert-true (search "#<type" s))))

(deftest printer-looks-like-type-specifier-primitives
  "looks-like-type-specifier-p recognises CL primitive type symbols."
  (assert-true  (cl-cc/type::looks-like-type-specifier-p 'fixnum))
  (assert-true  (cl-cc/type::looks-like-type-specifier-p 'string))
  (assert-false (cl-cc/type::looks-like-type-specifier-p 'frobnitz)))

(deftest printer-looks-like-type-specifier-shorthand
  "looks-like-type-specifier-p recognises shorthand type names INT and BOOL."
  (assert-true (cl-cc/type::looks-like-type-specifier-p 'cl-cc/type::int))
  (assert-true (cl-cc/type::looks-like-type-specifier-p 'cl-cc/type::bool)))

(deftest printer-looks-like-type-specifier-composite-heads
  "looks-like-type-specifier-p recognises composite type head symbols."
  (assert-true  (cl-cc/type::looks-like-type-specifier-p '(or fixnum string)))
  (assert-true  (cl-cc/type::looks-like-type-specifier-p '(and fixnum string)))
  (assert-false (cl-cc/type::looks-like-type-specifier-p '(frobnitz fixnum))))

(deftest printer-looks-like-type-specifier-bang-prefix
  "looks-like-type-specifier-p recognises ! prefix as capability / linear head."
  (assert-true (cl-cc/type::looks-like-type-specifier-p '(!linear fixnum))))

(deftest-each printer-arrow-mult-table
  "Each *arrow-mult-strings* entry maps to the expected arrow string."
  :cases (("zero"  :zero  "-0->")
          ("one"   :one   "-1->")
          ("omega" :omega "->"))
  (mult expected)
  (let ((arr (make-type-arrow (list type-int) type-string :mult mult)))
    (assert-true (search expected (type-to-string arr)))))

(deftest printer-type-error-sentinel
  "type-to-string formats unknown sentinels as ? and other errors as <error: message>."
  (let ((e1 (make-type-error :message "unbound x"))
        (e2 (make-type-error :message "unknown")))
    (assert-string= "<error: unbound x>" (type-to-string e1))
    (assert-string= "?"                  (type-to-string e2))
    (assert-string= "?"                  (type-to-string +type-unknown+))))

(deftest printer-compound-types
  "type-to-string formats product, record, and linear types correctly."
  (let ((pair (make-type-product :elems (list type-int type-string))))
    (assert-string= "(FIXNUM, STRING)" (type-to-string pair)))
  (let ((closed (make-type-record :fields (list (cons 'x type-int)
                                                (cons 'y type-bool))
                                  :row-var nil)))
    (let ((s (type-to-string closed)))
      (assert-true (search "X" (string-upcase s)))
      (assert-true (search "Y" (string-upcase s)))))
  (let ((open (make-type-record :fields (list (cons 'x type-int))
                                :row-var (fresh-type-var 'rho))))
    (assert-true (search "|" (type-to-string open))))
  (let ((lin (make-type-linear :base type-int :grade :one)))
    (let ((s (type-to-string lin)))
      (assert-true (search "1" s))
      (assert-true (search "FIXNUM" s)))))

(deftest-each printer-unicode-type-operators
  "type-to-string uses Unicode symbols ∀ and μ for quantifier and recursive types."
  :cases (("forall" "∀" (let* ((a  (fresh-type-var 'a))
                                 (fn (make-type-arrow (list a) a)))
                            (make-type-forall :var a :body fn)))
          ("mu"     "μ" (let* ((a (fresh-type-var 'a)))
                            (make-type-mu :var a :body (make-type-union (list type-null a))))))
  (glyph ty)
  (assert-true (search glyph (type-to-string ty))))

(deftest printer-effect-row-open
  "type-to-string formats open effect rows with | separator."
  (let* ((rv  (fresh-type-var 'epsilon))
         (row (make-type-effect-row
               :effects (list (make-type-effect-op :name 'io))
               :row-var rv)))
    (let ((s (type-to-string row)))
      (assert-true (search "IO" (string-upcase s)))
      (assert-true (search "|" s)))))

;;; ─── list-interleave ───────────────────────────────────────────────────────

(deftest list-interleave-behavior
  "list-interleave: inserts separator between elements; single element unchanged; empty returns nil."
  (assert-equal '(a x b x c) (cl-cc/type::list-interleave '(a b c) 'x))
  (assert-equal '(a)         (cl-cc/type::list-interleave '(a) 'x))
  (assert-null               (cl-cc/type::list-interleave nil 'x)))
