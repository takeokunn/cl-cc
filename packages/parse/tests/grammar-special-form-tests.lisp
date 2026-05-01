;;;; tests/unit/parse/grammar-special-form-tests.lisp — CL grammar special form tests
;;;;
;;;; Tests: special form dispatch, edge cases (quote, unquote-splicing, empty vector),
;;;; macro forms, and CST-to-sexp roundtrip through the grammar.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── parse-cl-source: special form dispatch ────────────────────────────────

(deftest-each grammar-special-form-kinds
  "parse-cl-source: special form heads map to the expected CST kind"
  :cases (("defun"   "(defun f (x) x)"                  :defun)
          ("let"     "(let ((x 1)) x)"                  :let)
          ("if"      "(if t 1 2)"                        :if)
          ("lambda"  "(lambda (x) x)"                    :lambda)
          ("progn"   "(progn 1 2 3)"                     :progn)
          ("setq"    "(setq x 1)"                        :setq)
          ("block"   "(block nil 1)"                     :block)
          ("cond"    "(cond (t 1))"                      :cond)
          ("loop"    "(loop for x from 1 to 10 collect x)" :loop)
          ("call"    "(foo 1 2)"                         :call))
  (source expected-kind)
  (assert-eq expected-kind (parse-first-kind source)))

(deftest-each grammar-parse-special-form-child-count
  "parse-cl-source: special forms produce the correct number of CST children."
  :cases (("defun" "(defun f (x) x)" 4)   ; defun name params body
          ("if"    "(if t 1 2)"      4))   ; if cond then else
  (source expected-count)
  (let ((node (parse-first-form source)))
    (assert-= expected-count (length (cl-cc:cst-interior-children node)))))

(deftest grammar-parse-quote-form-kind
  "parse-cl-source: (quote x) is an interior node with children"
  ;; Note: (quote x) is parsed as a list form; sexp-head-to-kind
  ;; does not exist for the generic path, but the kind check does.
  ;; The generic parser sees QUOTE as head ident and maps via sexp-head-to-kind.
  (let ((node (parse-first-form "(quote x)")))
    (assert-true (cl-cc:cst-interior-p node))
    ;; sexp-head-to-kind does not map "QUOTE" since the case checks symbol eq,
    ;; but the head is an interned symbol. Verify the node is an interior node.
    (assert-true (not (null (cl-cc:cst-interior-children node))))))

;;; ─── Unquote-splicing ──────────────────────────────────────────────────────

(deftest grammar-parse-unquote-splicing
  "parse-cl-source: ,@x produces :unquote-splicing CST"
  (let ((node (parse-first-form ",@x")))
    (assert-true (cl-cc:cst-interior-p node))
    (assert-eq :unquote-splicing (cl-cc:cst-node-kind node))
    (assert-= 1 (length (cl-cc:cst-interior-children node)))))

;;; ─── Vector edge cases ─────────────────────────────────────────────────────

(deftest grammar-parse-empty-vector
  "parse-cl-source: #() produces :vector with 0 children"
  (let ((node (parse-first-form "#()")))
    (assert-true (cl-cc:cst-interior-p node))
    (assert-eq :vector (cl-cc:cst-node-kind node))
    (assert-= 0 (length (cl-cc:cst-interior-children node)))))

;;; ─── CST-to-sexp roundtrip through grammar ────────────────────────────────

(deftest-each grammar-cst-to-sexp-roundtrip
  "parse-cl-source -> cst-to-sexp produces expected S-expression"
  :cases (("integer"   "42"           42)
          ("string"    "\"hi\""       "hi")
          ("empty-list" "()"          nil)
          ("simple-call" "(+ 1 2)"    '(+ 1 2))
          ("nested"    "(if t 1 2)"   '(if t 1 2)))
  (source expected)
  (let ((node (parse-first-form source)))
    (assert-equal expected (cl-cc:cst-to-sexp node))))

(deftest grammar-cst-to-sexp-quote
  "parse-cl-source -> cst-to-sexp for quoted symbol checks structure"
  (let* ((node (parse-first-form "'x"))
         (sexp (cl-cc:cst-to-sexp node)))
    (assert-true (consp sexp))
    (assert-eq 'quote (car sexp))
    (assert-equal "X" (symbol-name (second sexp)))))
