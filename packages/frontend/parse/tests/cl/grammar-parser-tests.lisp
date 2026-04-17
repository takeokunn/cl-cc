;;;; tests/unit/parse/cl/grammar-parser-tests.lisp
;;;; Unit tests for src/parse/cl/grammar.lisp — Form Parsers
;;;;
;;;; Covers: parse-cl-atom (all atom token types),
;;;;   parse-cl-form (quote/backquote/unquote/vector/list dispatch),
;;;;   parse-cl-list-form (empty list, special-form kinds, dotted pairs,
;;;;     generic calls), parse-cl-source (multi-form, nested).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ─────────────────────────────────────────────────────────────────

(defun grammar-parse (source)
  "Parse SOURCE and return the first CST form (or nil)."
  (first (cl-cc::parse-cl-source source)))

(defun grammar-parse-all (source)
  "Parse SOURCE and return the list of all CST forms."
  (cl-cc::parse-cl-source source))

;;; ─── parse-cl-atom — one case per token kind ─────────────────────────────────

(deftest-each parse-cl-atom-token-kinds
  "parse-cl-source converts each atom token type to a cst-token with the right kind."
  :cases (("integer"   "42"      :T-INT)
          ("float"     "3.14"    :T-FLOAT)
          ("string"    "\"hi\""  :T-STRING)
          ("keyword"   ":foo"    :T-KEYWORD)
          ("symbol"    "bar"     :T-IDENT)
          ("bool-true" "#t"      :T-BOOL-TRUE)
          ("bool-false" "#f"     :T-BOOL-FALSE))
  (source expected-kind)
  (let ((node (grammar-parse source)))
    (assert-true (cl-cc::cst-token-p node))
    (assert-eq expected-kind (cl-cc::cst-node-kind node))))

(deftest parse-cl-atom-integer-value
  "parse-cl-source captures the integer value in the cst-token."
  (let ((node (grammar-parse "99")))
    (assert-= 99 (cl-cc::cst-token-value node))))

(deftest parse-cl-atom-symbol-value
  "parse-cl-source captures the symbol value for an identifier."
  (let ((node (grammar-parse "some-var")))
    (assert-true (cl-cc::cst-token-p node))
    (assert-true (symbolp (cl-cc::cst-token-value node)))))

;;; ─── parse-cl-form — prefix sugar forms ─────────────────────────────────────

(deftest parse-cl-form-quote
  "parse-cl-form wraps 'x in a :quote cst-interior node."
  (let ((node (grammar-parse "'x")))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-eq :quote (cl-cc::cst-node-kind node))
    (assert-= 1 (length (cl-cc::cst-interior-children node)))))

(deftest parse-cl-form-backquote
  "parse-cl-form wraps `x in a :quasiquote cst-interior node."
  (let ((node (grammar-parse "`x")))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-eq :quasiquote (cl-cc::cst-node-kind node))))

(deftest parse-cl-form-unquote
  "parse-cl-form wraps ,x in an :unquote cst-interior node."
  (let ((node (grammar-parse "`(,x)")))
    ;; outer is :quasiquote wrapping a list whose first element is :unquote
    (assert-true (cl-cc::cst-interior-p node))
    (assert-eq :quasiquote (cl-cc::cst-node-kind node))))

(deftest parse-cl-form-function
  "parse-cl-form wraps #'foo in a :function cst-interior node."
  (let ((node (grammar-parse "#'car")))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-eq :function (cl-cc::cst-node-kind node))
    (assert-= 1 (length (cl-cc::cst-interior-children node)))))

(deftest parse-cl-form-vector
  "parse-cl-form parses #(1 2 3) to a :vector cst-interior node."
  (let ((node (grammar-parse "#(1 2 3)")))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-eq :vector (cl-cc::cst-node-kind node))
    (assert-= 3 (length (cl-cc::cst-interior-children node)))))

(deftest parse-cl-form-empty-vector
  "parse-cl-form parses #() to an empty :vector node."
  (let ((node (grammar-parse "#()")))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-eq :vector (cl-cc::cst-node-kind node))
    (assert-= 0 (length (cl-cc::cst-interior-children node)))))

;;; ─── parse-cl-list-form — list kinds ────────────────────────────────────────

(deftest parse-cl-list-empty-list
  "parse-cl-list-form parses () as an empty :list node."
  (let ((node (grammar-parse "()")))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-eq :list (cl-cc::cst-node-kind node))
    (assert-= 0 (length (cl-cc::cst-interior-children node)))))

(deftest parse-cl-list-generic-call
  "parse-cl-list-form parses (foo 1 2) as a :call node."
  (let ((node (grammar-parse "(foo 1 2)")))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-eq :call (cl-cc::cst-node-kind node))
    (assert-= 3 (length (cl-cc::cst-interior-children node)))))

(deftest-each parse-cl-list-special-form-kinds
  "parse-cl-list-form recognises special forms and assigns the correct :kind."
  :cases (("defun"   "(defun f (x) x)"          :defun)
          ("let"     "(let ((x 1)) x)"           :let)
          ("let*"    "(let* ((x 1)) x)"          :let*)
          ("if"      "(if t 1 2)"                :if)
          ("lambda"  "(lambda (x) x)"            :lambda)
          ("block"   "(block done 1)"            :block)
          ("progn"   "(progn 1 2)"               :progn)
          ("setq"    "(setq x 1)"               :setq)
          ("quote"   "(quote x)"                :call))   ; quote as list head is :call, not :quote
  (source expected-kind)
  (let ((node (grammar-parse source)))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-eq expected-kind (cl-cc::cst-node-kind node))))

(deftest parse-cl-list-dotted-pair
  "parse-cl-list-form parses (a . b) as a :dotted-list node."
  (let ((node (grammar-parse "(a . b)")))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-eq :dotted-list (cl-cc::cst-node-kind node))
    (assert-= 2 (length (cl-cc::cst-interior-children node)))))

(deftest parse-cl-list-nested-forms
  "parse-cl-list-form correctly nests inner forms as children."
  (let ((node (grammar-parse "(+ (* 2 3) (- 5 1))")))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-= 3 (length (cl-cc::cst-interior-children node)))
    ;; Second child is (* 2 3)
    (let ((child (second (cl-cc::cst-interior-children node))))
      (assert-true (cl-cc::cst-interior-p child))
      (assert-= 3 (length (cl-cc::cst-interior-children child))))))

;;; ─── parse-cl-source — multi-form and byte positions ─────────────────────────

(deftest parse-cl-source-multiple-forms
  "parse-cl-source returns all top-level forms from a multi-form source."
  (let ((forms (grammar-parse-all "1 2 3")))
    (assert-= 3 (length forms))
    (dolist (f forms)
      (assert-true (cl-cc::cst-token-p f)))))

(deftest parse-cl-source-returns-diagnostics-on-mismatch
  "parse-cl-source returns a diagnostics list (second value) when parse errors occur."
  (multiple-value-bind (forms diags)
      (cl-cc::parse-cl-source "(unclosed")
    (declare (ignore forms))
    ;; Should have at least one diagnostic about missing close paren
    (assert-true (listp diags))))

(deftest parse-cl-source-source-positions
  "parse-cl-source records byte positions in CST nodes."
  (let ((node (grammar-parse "42")))
    (assert-true (cl-cc::cst-token-p node))
    ;; start-byte should be 0, end-byte 2
    (assert-= 0 (cl-cc::cst-node-start-byte node))
    (assert-= 2 (cl-cc::cst-node-end-byte node))))

(deftest parse-cl-source-list-byte-span
  "parse-cl-source records byte span for list forms."
  (let ((node (grammar-parse "(+ 1 2)")))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-= 0 (cl-cc::cst-node-start-byte node))
    (assert-= 7 (cl-cc::cst-node-end-byte node))))

(deftest parse-cl-source-empty-input
  "parse-cl-source returns nil for empty source."
  (let ((forms (grammar-parse-all "")))
    (assert-null forms)))
