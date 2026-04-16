;;;; tests/unit/parse/grammar-tests.lisp — CL grammar parser unit tests
;;;;
;;;; Tests: token-stream operations, parse-cl-atom, parse-cl-form via
;;;; parse-cl-source, multi-form parsing, error recovery, and Pratt bridge.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── Helper ──────────────────────────────────────────────────────────────────

(defun make-grammar-token (type value &key (start 0) (end 1))
  "Create a lexer-token for grammar token-stream tests."
  (cl-cc/parse::make-lexer-token :type type :value value
                           :start-byte start :end-byte end
                           :line 1 :column 0))

(defun make-grammar-ts (&rest token-specs)
  "Create a token-stream from TOKEN-SPECS: each is (type value) or (type value :start s :end e)."
  (let ((tokens (mapcar (lambda (spec)
                          (apply #'make-grammar-token spec))
                        token-specs)))
    (cl-cc/parse::make-token-stream :tokens tokens :source "" :source-file nil)))

(defun parse-first-form (source)
  "Parse SOURCE and return the first CST node."
  (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source source)
    (declare (ignore diags))
    (first forms)))

(defun parse-first-kind (source)
  "Parse SOURCE and return the kind of the first CST node."
  (let ((node (parse-first-form source)))
    (when node (cl-cc:cst-node-kind node))))

(defun parse-first-value (source)
  "Parse SOURCE and return the value of the first CST token node."
  (let ((node (parse-first-form source)))
    (when (and node (cl-cc:cst-token-p node))
      (cl-cc:cst-token-value node))))

;;; ─── Token Stream: ts-peek ─────────────────────────────────────────────────

(deftest-each grammar-ts-peek-cases
  "ts-peek behavior on non-empty and empty streams"
  :cases (("non-empty" :non-empty)
          ("empty"     :empty))
  (scenario)
  (ecase scenario
    (:non-empty
     (let* ((ts (make-grammar-ts '(:T-INT 42) '(:T-INT 99)))
            (tok (cl-cc/parse::ts-peek ts)))
       (assert-eq :T-INT (cl-cc::lexer-token-type tok))
       (assert-= 42 (cl-cc::lexer-token-value tok))
       ;; Peeking again returns the same token
       (assert-= 42 (cl-cc::lexer-token-value (cl-cc/parse::ts-peek ts)))))
    (:empty
     (let ((ts (cl-cc/parse::make-token-stream :tokens nil :source "")))
       (assert-null (cl-cc/parse::ts-peek ts))))))

;;; ─── Token Stream: ts-advance ──────────────────────────────────────────────

(deftest grammar-ts-advance-consumes-token
  "ts-advance consumes and returns current token, advances to next"
  (let ((ts (make-grammar-ts '(:T-INT 1) '(:T-INT 2) '(:T-INT 3))))
    (let ((tok1 (cl-cc/parse::ts-advance ts)))
      (assert-= 1 (cl-cc::lexer-token-value tok1)))
    (let ((tok2 (cl-cc/parse::ts-advance ts)))
      (assert-= 2 (cl-cc::lexer-token-value tok2)))
    (let ((tok3 (cl-cc/parse::ts-advance ts)))
      (assert-= 3 (cl-cc::lexer-token-value tok3)))
    ;; Stream now exhausted
    (assert-null (cl-cc/parse::ts-advance ts))))

;;; ─── Token Stream: ts-peek-type ────────────────────────────────────────────

(deftest-each grammar-ts-peek-type-cases
  "ts-peek-type behavior on non-empty and empty streams"
  :cases (("non-empty" :non-empty)
          ("empty"     :empty))
  (scenario)
  (ecase scenario
    (:non-empty
     (let ((ts (make-grammar-ts '(:T-STRING "hello"))))
       (assert-eq :T-STRING (cl-cc/parse::ts-peek-type ts))))
    (:empty
     (let ((ts (cl-cc/parse::make-token-stream :tokens nil :source "")))
       (assert-null (cl-cc/parse::ts-peek-type ts))))))

;;; ─── Token Stream: ts-expect ───────────────────────────────────────────────

(deftest grammar-ts-expect-behavior
  "ts-expect: matching consumes token; mismatch and end-of-input add diagnostics"
  ;; Matching type consumes and returns the token
  (let* ((ts (make-grammar-ts '(:T-INT 42)))
         (tok (cl-cc/parse::ts-expect ts :T-INT)))
    (assert-true (not (null tok)))
    (assert-= 42 (cl-cc::lexer-token-value tok))
    ;; Stream is now empty
    (assert-null (cl-cc/parse::ts-peek ts)))
  ;; Mismatch adds a diagnostic and returns nil without consuming
  (let* ((ts (make-grammar-ts '(:T-INT 42)))
         (result (cl-cc/parse::ts-expect ts :T-STRING)))
    (assert-null result)
    (assert-true (not (null (cl-cc/parse::token-stream-diagnostics ts))))
    ;; Token was NOT consumed
    (assert-= 42 (cl-cc::lexer-token-value (cl-cc/parse::ts-peek ts))))
  ;; At end of input adds a diagnostic
  (let* ((ts (cl-cc/parse::make-token-stream :tokens nil :source ""))
         (result (cl-cc/parse::ts-expect ts :T-RPAREN)))
    (assert-null result)
    (assert-true (not (null (cl-cc/parse::token-stream-diagnostics ts))))))

;;; ─── Token Stream: ts-at-end-p ─────────────────────────────────────────────

(deftest-each grammar-ts-at-end-p-cases
  "ts-at-end-p: empty list and :T-EOF return true; non-empty returns false"
  :cases (("empty"     :empty     t)
          ("non-empty" :non-empty nil)
          ("eof-token" :eof       t))
  (scenario expected)
  (let ((ts (ecase scenario
               (:empty     (cl-cc/parse::make-token-stream :tokens nil :source ""))
               (:non-empty (make-grammar-ts '(:T-INT 42)))
               (:eof       (make-grammar-ts '(:T-EOF nil))))))
    (if expected
        (assert-true  (cl-cc/parse::ts-at-end-p ts))
        (assert-false (cl-cc/parse::ts-at-end-p ts)))))

;;; ─── Token Stream: ts-token-value ──────────────────────────────────────────

(deftest-each grammar-ts-token-value-cases
  "ts-token-value: returns current token value or nil on empty stream"
  :cases (("non-empty" :non-empty)
          ("empty"     :empty))
  (scenario)
  (ecase scenario
    (:non-empty
     (let ((ts (make-grammar-ts '(:T-INT 42))))
       (assert-= 42 (cl-cc/parse::ts-token-value ts))))
    (:empty
     (let ((ts (cl-cc/parse::make-token-stream :tokens nil :source "")))
       (assert-null (cl-cc/parse::ts-token-value ts))))))

;;; ─── parse-cl-atom ─────────────────────────────────────────────────────────

(deftest-each grammar-parse-cl-atom-cases
  "parse-cl-atom: integer/string/keyword produce cst-token; LPAREN and empty stream return nil"
  :cases (("integer"   :integer   42        :int     :T-INT)
          ("string"    :string    "hello"   :string  :T-STRING)
          ("keyword"   :keyword   :foo      :keyword :T-KEYWORD)
          ("non-atom"  :non-atom  nil       nil      :T-LPAREN)
          ("empty"     :empty     nil       nil      nil))
  (scenario value kind tok-type)
  (declare (ignore kind))
  (let* ((ts (if tok-type
                 (make-grammar-ts (list tok-type value))
                 (cl-cc/parse::make-token-stream :tokens nil :source "")))
         (node (cl-cc/parse::parse-cl-atom ts)))
    (ecase scenario
      (:integer
       (assert-true (cl-cc:cst-token-p node))
       (assert-= value (cl-cc:cst-token-value node)))
      (:string
       (assert-true (cl-cc:cst-token-p node))
       (assert-string= value (cl-cc:cst-token-value node)))
      (:keyword
       (assert-true (cl-cc:cst-token-p node))
       (assert-eq value (cl-cc:cst-token-value node)))
      ((:non-atom :empty)
       (assert-null node)))))

;;; ─── parse-cl-source: atoms ────────────────────────────────────────────────

(deftest grammar-parse-integer
  "parse-cl-source: \"42\" produces integer CST token"
  (let ((node (parse-first-form "42")))
    (assert-true (cl-cc:cst-token-p node))
    (assert-= 42 (cl-cc:cst-token-value node))))

(deftest grammar-parse-string
  "parse-cl-source: string literal produces string CST token"
  (let ((node (parse-first-form "\"hello\"")))
    (assert-true (cl-cc:cst-token-p node))
    (assert-string= "hello" (cl-cc:cst-token-value node))))

;;; ─── parse-cl-source: quote sugar ──────────────────────────────────────────

(deftest-each grammar-quote-sugar-forms
  "parse-cl-source: quote sugar and vector reader macros produce correct interior CST nodes"
  :cases (("quote"      "'x"       :quote       1)
          ("backquote"  "`x"       :quasiquote  1)
          ("unquote"    ",x"       :unquote     1)
          ("function"   "#'foo"    :function    1)
          ("vector"     "#(1 2 3)" :vector      3))
  (source expected-kind expected-children)
  (let ((node (parse-first-form source)))
    (assert-true (cl-cc:cst-interior-p node))
    (assert-eq expected-kind (cl-cc:cst-node-kind node))
    (assert-= expected-children (length (cl-cc:cst-interior-children node)))))

;;; ─── parse-cl-source: list forms ───────────────────────────────────────────

(deftest-each grammar-list-forms
  "parse-cl-source: empty list, simple list, and dotted list produce correct CST interior nodes"
  :cases (("empty-list"   "()"      :list        0)
          ("simple-list"  "(a b c)" nil          3)
          ("dotted-list"  "(a . b)" :dotted-list 2))
  (source expected-kind expected-children)
  (let ((node (parse-first-form source)))
    (assert-true (cl-cc:cst-interior-p node))
    (when expected-kind
      (assert-eq expected-kind (cl-cc:cst-node-kind node)))
    (assert-= expected-children (length (cl-cc:cst-interior-children node)))))

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

(deftest grammar-parse-defun-children
  "parse-cl-source: (defun f (x) x) has 4 children"
  (let ((node (parse-first-form "(defun f (x) x)")))
    ;; children: defun, f, (x), x
    (assert-= 4 (length (cl-cc:cst-interior-children node)))))

(deftest grammar-parse-if-children
  "parse-cl-source: (if t 1 2) has 4 children (if, cond, then, else)"
  (let ((node (parse-first-form "(if t 1 2)")))
    (assert-= 4 (length (cl-cc:cst-interior-children node)))))

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

;;; ─── parse-cl-source: multi-form ───────────────────────────────────────────

(deftest-each grammar-multi-form-cases
  "parse-cl-source: three atoms, defun+call, and empty input return correct form counts"
  :cases (("three-atoms"    "1 2 3"                3   nil)
          ("defun-and-call" "(defun f () 1) (f)"   2   :defun)
          ("empty-input"    ""                     0   nil))
  (source expected-count expected-first-kind)
  (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source source)
    (declare (ignore diags))
    (assert-= expected-count (length forms))
    (when expected-first-kind
      (assert-eq expected-first-kind (cl-cc:cst-node-kind (first forms))))))

;;; ─── Error Recovery ────────────────────────────────────────────────────────

(deftest grammar-error-recovery-behavior
  "parse-cl-source: missing close paren adds diagnostic; diagnostics are a list; valid input has none"
  ;; Missing close paren adds a diagnostic
  (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source "(a b c")
    (declare (ignore forms))
    (assert-true (> (length diags) 0)))
  ;; Diagnostics are returned as a list (second value)
  (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source "(a b")
    (declare (ignore forms))
    (assert-true (listp diags))
    (assert-true (> (length diags) 0)))
  ;; Valid input returns empty diagnostics
  (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source "(+ 1 2)")
    (declare (ignore forms))
    (assert-= 0 (length diags))))

;;; ─── Pratt Bridge ──────────────────────────────────────────────────────────

(deftest grammar-pratt-nud-table-has-entries
  "cl-nud-table has entries for atom token types"
  (assert-true (not (null (gethash :T-INT cl-cc/parse::*cl-nud-table*))))
  (assert-true (not (null (gethash :T-IDENT cl-cc/parse::*cl-nud-table*))))
  (assert-true (not (null (gethash :T-STRING cl-cc/parse::*cl-nud-table*)))))

(deftest grammar-pratt-led-table-empty
  "cl-led-table is empty (CL has no infix operators)"
  (assert-= 0 (hash-table-count cl-cc/parse::*cl-led-table*)))

(deftest grammar-pratt-context-creation
  "make-cl-pratt-context returns a valid pratt-context"
  (let* ((tokens (cl-cc:lex-all "42"))
         (ctx (cl-cc/parse::make-cl-pratt-context tokens "42" nil)))
    (assert-true (cl-cc/parse::pratt-context-p ctx))))

;;; ─── CST byte positions ────────────────────────────────────────────────────

(deftest-each grammar-byte-positions-cases
  "parse-cl-source: CST nodes carry correct start/end byte offsets"
  :cases (("atom" "  42"    2 4)
          ("list" "(+ 1 2)" 0 7))
  (source expected-start expected-end)
  (let ((node (parse-first-form source)))
    (assert-= expected-start (cl-cc:cst-node-start-byte node))
    (assert-= expected-end   (cl-cc:cst-node-end-byte   node))))

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
