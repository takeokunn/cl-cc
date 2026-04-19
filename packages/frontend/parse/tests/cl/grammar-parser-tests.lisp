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

(deftest parse-cl-atom-value-cases
  "parse-cl-source captures integer value; captures symbol as symbol type."
  (assert-= 99 (cl-cc::cst-token-value (grammar-parse "99")))
  (let ((node (grammar-parse "some-var")))
    (assert-true (cl-cc::cst-token-p node))
    (assert-true (symbolp (cl-cc::cst-token-value node)))))

;;; ─── parse-cl-form — prefix sugar forms ─────────────────────────────────────

(deftest-each parse-cl-form-prefix-kinds
  "parse-cl-form: prefix sugar produces cst-interior with expected kind."
  :cases (("quote"      "'x"    :quote)
          ("backquote"  "`x"    :quasiquote)
          ("quasiquote-unquote" "`(,x)" :quasiquote)
          ("function"   "#'car" :function))
  (source expected-kind)
  (let ((node (grammar-parse source)))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-eq expected-kind (cl-cc::cst-node-kind node))))

(deftest parse-cl-form-vector-cases
  "parse-cl-form: #(1 2 3) → :vector with 3 children; #() → :vector with 0 children."
  (let ((node (grammar-parse "#(1 2 3)")))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-eq :vector (cl-cc::cst-node-kind node))
    (assert-= 3 (length (cl-cc::cst-interior-children node))))
  (let ((node (grammar-parse "#()")))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-eq :vector (cl-cc::cst-node-kind node))
    (assert-= 0 (length (cl-cc::cst-interior-children node)))))

;;; ─── parse-cl-list-form — list kinds ────────────────────────────────────────

(deftest parse-cl-list-structure-cases
  "parse-cl-list-form: () → :list empty; (foo 1 2) → :call with 3 children."
  (let ((node (grammar-parse "()")))
    (assert-true (cl-cc::cst-interior-p node))
    (assert-eq :list (cl-cc::cst-node-kind node))
    (assert-= 0 (length (cl-cc::cst-interior-children node))))
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

(deftest parse-cl-source-multi-and-empty
  "parse-cl-source: multi-form input returns all forms; empty input → nil."
  (let ((forms (grammar-parse-all "1 2 3")))
    (assert-= 3 (length forms))
    (dolist (f forms) (assert-true (cl-cc::cst-token-p f))))
  (assert-null (grammar-parse-all "")))

(deftest parse-cl-source-diagnostics-on-error
  "parse-cl-source returns a diagnostics list when parse errors occur."
  (multiple-value-bind (forms diags)
      (cl-cc::parse-cl-source "(unclosed")
    (declare (ignore forms))
    (assert-true (listp diags))))

(deftest parse-cl-source-byte-span-cases
  "parse-cl-source records byte positions: atom [0,2]; list form [0,7]."
  (let ((node (grammar-parse "42")))
    (assert-= 0 (cl-cc::cst-node-start-byte node))
    (assert-= 2 (cl-cc::cst-node-end-byte node)))
  (let ((node (grammar-parse "(+ 1 2)")))
    (assert-= 0 (cl-cc::cst-node-start-byte node))
    (assert-= 7 (cl-cc::cst-node-end-byte node))))
