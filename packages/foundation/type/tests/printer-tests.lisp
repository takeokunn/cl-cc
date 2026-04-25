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

(deftest-each printer-primitive-types
  "type-to-string for primitive type constants renders expected name."
  :cases (("int"    type-int    "FIXNUM")
          ("string" type-string "STRING"))
  (type-node expected)
  (assert-string= expected (type-to-string type-node)))

(deftest-each printer-var-contains-substring
  "type-to-string for named and rigid vars contains expected substring."
  :cases (("named" (fresh-type-var  "alpha") "alpha")
          ("rigid" (fresh-rigid-var "test")  "sk"))
  (type-node expected-sub)
  (assert-true (search expected-sub (type-to-string type-node))))

(deftest printer-var-and-scheme-cases
  "Linked type-var shows FIXNUM; type-scheme with quantifiers produces non-empty string."
  (let ((v (fresh-type-var "a")))
    (setf (cl-cc/type::type-var-link v) type-int)
    (assert-string= "FIXNUM" (type-to-string v)))
  (let* ((v (fresh-type-var "a"))
         (scheme (make-type-scheme (list v) v))
         (s (type-to-string scheme)))
    (assert-true (> (length s) 0))))

;;; ─── type-to-string: composite types ───────────────────────────────────────

(deftest-each printer-arrow-cases
  "Arrow types print with '->'; single-param shows both types; effectful arrow includes 'IO'."
  :cases (("single-param"   (list type-int) type-string nil                 "FIXNUM" "STRING")
          ("multi-param"    (list type-int type-string) type-bool nil       nil      nil)
          ("with-effects"   (list type-int) type-string +io-effect-row+     "IO"     nil))
  (params ret effects expected-sub expected-sub2)
  (let ((s (type-to-string (make-type-arrow params ret :effects effects))))
    (assert-true (search "->" s))
    (when expected-sub  (assert-true (search expected-sub  s)))
    (when expected-sub2 (assert-true (search expected-sub2 s)))
    (when effects       (assert-true (search "IO" s)))  ))

(deftest-each printer-container-type-delimiters
  "Product uses comma, variant uses <, and type-app uses ( as its primary delimiter."
  :cases (("product"  (make-type-product :elems (list type-int type-string)) ",")
          ("variant"  (make-type-variant :cases (list (cons 'some type-int) (cons 'none type-null))
                                         :row-var nil) "<")
          ("type-app" (make-type-app :fun type-int :arg type-string) "("))
  (ty expected-sub)
  (assert-true (search expected-sub (type-to-string ty))))

(deftest-each printer-record-cases
  "Closed record contains '{' and field name; open record contains '|' row separator."
  :cases (("closed" nil "{" "x")
          ("open"   t   "|" nil))
  (open-p expected-bracket expected-field)
  (let* ((rv (when open-p (fresh-type-var "rho")))
         (fields (if open-p
                     (list (cons 'x type-int))
                     (list (cons 'x type-int) (cons 'y type-string))))
         (r (make-type-record :fields fields :row-var rv))
         (s (type-to-string r)))
    (assert-true (search expected-bracket s))
    (when expected-field (assert-true (search expected-field (string-downcase s))))))

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

(deftest printer-handler-gadt-cases
  "Handler type uses bracket notation; GADT constructor includes '::'."
  (let* ((eff (make-type-effect-op :name 'io :args nil))
         (h (cl-cc/type::make-type-handler :effect eff :input type-int :output type-string))
         (s (type-to-string h)))
    (assert-true (search "[" s))
    (assert-true (search "=>" s)))
  (let* ((gc (cl-cc/type::make-type-gadt-con
              :name 'just :arg-types (list type-int) :index-type type-any))
         (s (type-to-string gc)))
    (assert-true (search "::" s))))

;;; ─── type-to-string: effect rows ──────────────────────────────────────────

(deftest printer-effect-rows-and-ops
  "Effect rows: pure={}, io row has IO, open row has |; op with args shows name; open IO row has both."
  (assert-string= "{}" (type-to-string +pure-effect-row+))
  (assert-true (search "IO" (type-to-string +io-effect-row+)))
  (let* ((rv (fresh-type-var "e"))
         (er (make-type-effect-row :effects nil :row-var rv)))
    (assert-true (search "|" (type-to-string er))))
  (let* ((op (make-type-effect-op :name 'state :args (list type-int)))
         (s  (type-to-string op)))
    (assert-true (search "STATE"  s))
    (assert-true (search "FIXNUM" s)))
  (let* ((rv  (fresh-type-var 'epsilon))
         (row (make-type-effect-row
               :effects (list (make-type-effect-op :name 'io))
               :row-var rv))
         (s (type-to-string row)))
    (assert-true (search "IO" (string-upcase s)))
    (assert-true (search "|" s))))

;;; ─── type-to-string: constraint / qualified ───────────────────────────────

(deftest printer-constraint-and-qualified
  "Constraint prints class name; qualified with constraints includes =>; qualified with no constraints prints just body."
  (let ((tc (cl-cc/type::make-type-constraint :class-name 'eq :type-arg type-int)))
    (assert-true (search "EQ" (type-to-string tc)))
    (let ((s (type-to-string (make-type-qualified :constraints (list tc) :body type-int))))
      (assert-true (search "=>" s)))
    (let ((s (type-to-string (make-type-qualified :constraints nil :body type-int))))
      (assert-string= "FIXNUM" s))))

;;; ─── unparse-type roundtrip ────────────────────────────────────────────────

(deftest unparse-type-forms
  "unparse-type: primitive→symbol; arrow→(->...); union→(or...); product→(values...)."
  (assert-eq 'fixnum (cl-cc/type::unparse-type type-int))
  (assert-eq 'cl-cc/type::-> (first (cl-cc/type::unparse-type (make-type-arrow (list type-int) type-string))))
  (assert-eq 'or     (first (cl-cc/type::unparse-type (make-type-union (list type-int type-string)))))
  (assert-eq 'values (first (cl-cc/type::unparse-type (make-type-product :elems (list type-int))))))

;;; ─── type-to-string: edge cases not covered above ────────────────────────

(deftest-each printer-atomic-sentinel-strings
  "nil prints as NIL; the unknown sentinel prints exactly as '?'."
  :cases (("nil-val"  "NIL" nil)
          ("unknown"  "?"   cl-cc/type::+type-unknown+))
  (expected node)
  (assert-string= expected (type-to-string node)))

(deftest-each printer-unnamed-var-format
  "Unnamed type-var has '?t' prefix; unnamed rigid var has 'sk' prefix with no bracket."
  :cases (("type-var"    (fresh-type-var nil)   "?t" nil)
          ("rigid-var"   (fresh-rigid-var nil)   "sk" "["))
  (node expected-sub forbidden-sub)
  (let ((s (type-to-string node)))
    (assert-true (search expected-sub s))
    (when forbidden-sub (assert-false (search forbidden-sub s)))))

(deftest printer-fallback-hash-table
  "Hash-table falls back to #<type... printer."
  (assert-true (search "#<type" (type-to-string (make-hash-table)))))

(deftest-each printer-looks-like-type-specifier-p
  "looks-like-type-specifier-p recognises CL primitives, shorthands, composites, ! prefix; rejects unknowns."
  :cases (("fixnum"         t   'fixnum)
          ("string"         t   'string)
          ("int-shorthand"  t   'cl-cc/type::int)
          ("bool-shorthand" t   'cl-cc/type::bool)
          ("or-composite"   t   '(or fixnum string))
          ("and-composite"  t   '(and fixnum string))
          ("bang-prefix"    t   '(!linear fixnum))
          ("frobnitz-sym"   nil 'frobnitz)
          ("frobnitz-list"  nil '(frobnitz fixnum)))
  (expected form)
  (if expected
      (assert-true  (cl-cc/type::looks-like-type-specifier-p form))
      (assert-false (cl-cc/type::looks-like-type-specifier-p form))))

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
    (assert-string= "?"                  (type-to-string cl-cc/type::+type-unknown+))))

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

;;; ─── list-interleave ───────────────────────────────────────────────────────

(deftest list-interleave-behavior
  "list-interleave: inserts separator between elements; single element unchanged; empty returns nil."
  (assert-equal '(a x b x c) (cl-cc/type::list-interleave '(a b c) 'x))
  (assert-equal '(a)         (cl-cc/type::list-interleave '(a) 'x))
  (assert-null               (cl-cc/type::list-interleave nil 'x)))
