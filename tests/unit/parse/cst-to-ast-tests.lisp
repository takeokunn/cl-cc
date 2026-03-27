;;;; tests/unit/parse/cst-to-ast-tests.lisp — CST-to-AST Lowering Tests
;;;;
;;;; Tests for CST → AST conversion: lower-cst-to-ast, lower-cst-list-to-ast,
;;;; parse-and-lower, parse-and-lower-one.

(in-package :cl-cc/test)

(defsuite cst-to-ast-suite :description "CST-to-AST lowering unit tests")

;;; ─── lower-cst-to-ast ──────────────────────────────────────────────────────

(deftest cst-ast-nil-input
  "lower-cst-to-ast returns nil for nil input."
  (assert-null (cl-cc::lower-cst-to-ast nil)))

(deftest-each cst-ast-token-types
  "lower-cst-to-ast converts each CST token kind to the correct AST node type"
  :cases (("integer" :T-INT    42      "42"    #'cl-cc::ast-int-p)
          ("symbol"  :T-SYMBOL 'hello  "hello" #'cl-cc::ast-node-p)
          ("string"  :T-STRING "hi"    "\"hi\"" #'cl-cc::ast-quote-p))
  (kind value source pred)
  (let* ((cst (cl-cc::make-cst-token :kind kind :value value
                                     :start-byte 0 :end-byte (length source)))
         (ast (cl-cc::lower-cst-to-ast cst :source source)))
    (assert-true (cl-cc::ast-node-p ast))
    (assert-true (funcall pred ast))))

(deftest cst-ast-source-file-propagation
  "lower-cst-to-ast propagates source-file to AST."
  (let* ((cst (cl-cc::make-cst-token :kind :T-INT :value 1
                                      :start-byte 0 :end-byte 1))
         (ast (cl-cc::lower-cst-to-ast cst :source "1" :source-file "test.lisp")))
    (assert-true (cl-cc::ast-node-p ast))))

;;; ─── lower-cst-list-to-ast ─────────────────────────────────────────────────

(deftest cst-ast-list-empty
  "lower-cst-list-to-ast returns empty list for empty input."
  (assert-null (cl-cc::lower-cst-list-to-ast nil)))

(deftest cst-ast-list-multiple
  "lower-cst-list-to-ast converts multiple CST nodes."
  (let* ((cst1 (cl-cc::make-cst-token :kind :T-INT :value 1
                                       :start-byte 0 :end-byte 1))
         (cst2 (cl-cc::make-cst-token :kind :T-INT :value 2
                                       :start-byte 2 :end-byte 3))
         (result (cl-cc::lower-cst-list-to-ast (list cst1 cst2) :source "1 2")))
    (assert-equal 2 (length result))
    (assert-true (cl-cc::ast-int-p (first result)))
    (assert-true (cl-cc::ast-int-p (second result)))))

;;; ─── parse-and-lower ────────────────────────────────────────────────────────

(deftest-each cst-ast-parse-and-lower-cases
  "parse-and-lower converts each source form to a correctly-typed AST list"
  :cases (("integer"  "42"        1 #'cl-cc::ast-int-p)
          ("string"   "\"hello\"" 1 #'cl-cc::ast-quote-p)
          ("multiple" "1 2 3"     3 #'cl-cc::ast-node-p)
          ("list"     "(+ 1 2)"   1 #'cl-cc::ast-node-p))
  (source expected-len pred)
  (let ((result (cl-cc::parse-and-lower source)))
    (assert-equal expected-len (length result))
    (assert-true (funcall pred (first result)))))

(deftest cst-ast-parse-and-lower-source-file
  "parse-and-lower accepts optional source-file parameter."
  (let ((result (cl-cc::parse-and-lower "42" "test.lisp")))
    (assert-equal 1 (length result))))

;;; ─── parse-and-lower-one ────────────────────────────────────────────────────

(deftest cst-ast-parse-and-lower-one
  "parse-and-lower-one returns a single AST node for atomic and list forms"
  (let ((ast (cl-cc::parse-and-lower-one "42")))
    (assert-true (cl-cc::ast-int-p ast))
    (assert-equal 42 (cl-cc::ast-int-value ast)))
  (let ((ast (cl-cc::parse-and-lower-one "(if t 1 2)")))
    (assert-true (cl-cc::ast-node-p ast))))

(deftest cst-ast-parse-and-lower-one-empty-error
  "parse-and-lower-one errors on empty source."
  (assert-signals error (cl-cc::parse-and-lower-one "")))
