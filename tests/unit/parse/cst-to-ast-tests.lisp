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

(deftest cst-ast-integer-token
  "lower-cst-to-ast converts integer CST token to AST."
  (let* ((cst (cl-cc::make-cst-token :kind :T-INT :value 42
                                      :start-byte 0 :end-byte 2))
         (ast (cl-cc::lower-cst-to-ast cst :source "42")))
    (assert-true (cl-cc::ast-node-p ast))
    (assert-true (cl-cc::ast-int-p ast))
    (assert-equal 42 (cl-cc::ast-int-value ast))))

(deftest cst-ast-symbol-token
  "lower-cst-to-ast converts symbol CST token to AST."
  (let* ((cst (cl-cc::make-cst-token :kind :T-SYMBOL :value 'hello
                                      :start-byte 0 :end-byte 5))
         (ast (cl-cc::lower-cst-to-ast cst :source "hello")))
    (assert-true (cl-cc::ast-node-p ast))))

(deftest cst-ast-string-token
  "lower-cst-to-ast converts string CST token to AST."
  (let* ((cst (cl-cc::make-cst-token :kind :T-STRING :value "hi"
                                      :start-byte 0 :end-byte 4))
         (ast (cl-cc::lower-cst-to-ast cst :source "\"hi\"")))
    (assert-true (cl-cc::ast-quote-p ast))
    (assert-equal "hi" (cl-cc::ast-quote-value ast))))

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

(deftest cst-ast-parse-and-lower-integer
  "parse-and-lower converts integer source to AST list."
  (let ((result (cl-cc::parse-and-lower "42")))
    (assert-equal 1 (length result))
    (assert-true (cl-cc::ast-int-p (first result)))
    (assert-equal 42 (cl-cc::ast-int-value (first result)))))

(deftest cst-ast-parse-and-lower-string
  "parse-and-lower converts string literal to AST."
  (let ((result (cl-cc::parse-and-lower "\"hello\"")))
    (assert-equal 1 (length result))
    (assert-true (cl-cc::ast-quote-p (first result)))
    (assert-equal "hello" (cl-cc::ast-quote-value (first result)))))

(deftest cst-ast-parse-and-lower-multiple
  "parse-and-lower converts multiple forms."
  (let ((result (cl-cc::parse-and-lower "1 2 3")))
    (assert-equal 3 (length result))))

(deftest cst-ast-parse-and-lower-list
  "parse-and-lower converts list form to AST."
  (let ((result (cl-cc::parse-and-lower "(+ 1 2)")))
    (assert-equal 1 (length result))
    (assert-true (cl-cc::ast-node-p (first result)))))

(deftest cst-ast-parse-and-lower-source-file
  "parse-and-lower accepts optional source-file parameter."
  (let ((result (cl-cc::parse-and-lower "42" "test.lisp")))
    (assert-equal 1 (length result))))

;;; ─── parse-and-lower-one ────────────────────────────────────────────────────

(deftest cst-ast-parse-and-lower-one-single
  "parse-and-lower-one returns a single AST node."
  (let ((ast (cl-cc::parse-and-lower-one "42")))
    (assert-true (cl-cc::ast-int-p ast))
    (assert-equal 42 (cl-cc::ast-int-value ast))))

(deftest cst-ast-parse-and-lower-one-list
  "parse-and-lower-one handles list forms."
  (let ((ast (cl-cc::parse-and-lower-one "(if t 1 2)")))
    (assert-true (cl-cc::ast-node-p ast))))

(deftest cst-ast-parse-and-lower-one-empty-error
  "parse-and-lower-one errors on empty source."
  (assert-signals error (cl-cc::parse-and-lower-one "")))
