;;;; tests/unit/parse/php/parser-tests.lisp — PHP Parser tests
;;;;
;;;; Coverage for the dispatch-table statement parsers in parser-stmt.lisp.
;;;; Each test exercises one *php-stmt-parsers* handler via parse-php-source.
(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── Helper ───────────────────────────────────────────────────────────────

(defun %php-first (src)
  "Parse SRC and return the first top-level AST node."
  (first (parse-php-source src)))

;;; ─── :echo handler → ast-print ───────────────────────────────────────────

(deftest php-parser-echo-produces-ast-print
  "echo expr; lowers to ast-print."
  (assert-true (typep (%php-first "<?php echo 42;") 'cl-cc:ast-print)))

(deftest php-parser-echo-wraps-expression
  "ast-print wraps the parsed expression."
  (let ((ast (%php-first "<?php echo 42;")))
    (assert-true (ast-print-p ast))
    (assert-true (typep (cl-cc::ast-print-expr ast) 'cl-cc:ast-int))))

;;; ─── :return handler → ast-return-from ───────────────────────────────────

(deftest php-parser-return-with-value
  "return expr; lowers to ast-return-from with a value."
  (let ((ast (%php-first "<?php return 1;")))
    (assert-true (typep ast 'cl-cc:ast-return-from))
    (assert-true (typep (cl-cc::ast-return-from-value ast) 'cl-cc:ast-int))))

(deftest php-parser-return-void
  "return; with no value lowers to ast-return-from wrapping nil."
  (let ((ast (%php-first "<?php return;")))
    (assert-true (typep ast 'cl-cc:ast-return-from))
    (assert-null (cl-cc::ast-return-from-name ast))))

;;; ─── :if handler → ast-if ────────────────────────────────────────────────

(deftest php-parser-if-produces-ast-if
  "if ($cond) { } lowers to ast-if."
  (assert-true (typep (%php-first "<?php if ($x) { echo 1; }") 'cl-cc:ast-if)))

(deftest php-parser-if-else
  "if/else lowers to ast-if with a non-nil else branch."
  (let ((ast (%php-first "<?php if ($x) { echo 1; } else { echo 2; }")))
    (assert-true (ast-if-p ast))
    (assert-true (typep (cl-cc::ast-if-else ast) 'cl-cc:ast-progn))))

(deftest php-parser-if-no-else-is-nil-quote
  "if without else gets a nil quote as the else branch."
  (let ((ast (%php-first "<?php if ($x) { echo 1; }")))
    (assert-true (ast-if-p ast))
    (assert-true (typep (cl-cc::ast-if-else ast) 'cl-cc:ast-quote))))

;;; ─── :while handler → ast-block ──────────────────────────────────────────

(deftest php-parser-while-lowering
  "while forms lower to a block-based loop AST."
  (assert-true (typep (%php-first "<?php while ($x) { echo 1; }") 'cl-cc:ast-block)))

;;; ─── :for handler → ast-progn wrapping while ─────────────────────────────

(deftest php-parser-for-produces-ast-progn
  "for loop lowers to ast-progn(init, while-loop)."
  (let ((ast (%php-first "<?php for ($i = 0; $i < 10; $i++) { echo $i; }")))
    (assert-true (typep ast 'cl-cc:ast-progn))))

(deftest php-parser-for-progn-has-two-forms
  "for loop ast-progn contains init and while-body."
  (let ((ast (%php-first "<?php for ($i = 0; $i < 10; $i++) { echo $i; }")))
    (assert-= 2 (length (cl-cc::ast-progn-forms ast)))))

;;; ─── :foreach handler → ast-let ──────────────────────────────────────────

(deftest php-parser-foreach-lowering
  "foreach forms lower to a let-based loop AST."
  (assert-true (ast-let-p (%php-first "<?php foreach ($items as $item) { echo $item; }"))))

(deftest php-parser-foreach-key-value
  "foreach with key => value form uses the value variable."
  (let ((ast (%php-first "<?php foreach ($arr as $k => $v) { echo $v; }")))
    (assert-true (ast-let-p ast))))

;;; ─── :function handler → ast-defun ───────────────────────────────────────

(deftest php-parser-function-produces-ast-defun
  "function declaration lowers to ast-defun."
  (assert-true (typep (%php-first "<?php function greet($name) { return $name; }")
                       'cl-cc:ast-defun)))

(deftest php-parser-function-name-and-params
  "ast-defun captures function name (upcased) and parameter list."
  (let ((ast (%php-first "<?php function add($a, $b) { return $a; }")))
    (assert-equal "ADD" (symbol-name (cl-cc::ast-defun-name ast)))
    (assert-= 2 (length (cl-cc::ast-defun-params ast)))))

(deftest php-parser-function-empty-params
  "Zero-parameter function lowers cleanly."
  (let ((ast (%php-first "<?php function noop() { return 0; }")))
    (assert-true (typep ast 'cl-cc:ast-defun))
    (assert-null (cl-cc::ast-defun-params ast))))

;;; ─── :class handler → ast-defclass ───────────────────────────────────────

(deftest php-parser-class-produces-ast-defclass
  "class declaration lowers to ast-defclass."
  (assert-true (typep (%php-first "<?php class Dog { }") 'cl-cc:ast-defclass)))

(deftest php-parser-class-name
  "Class name is interned as a symbol with the upcased PHP identifier name."
  (let ((ast (%php-first "<?php class Cat { }")))
    (assert-equal "CAT" (symbol-name (cl-cc::ast-defclass-name ast)))))

(deftest php-parser-class-with-extends
  "class Foo extends Bar captures superclass by upcased name."
  (let ((ast (%php-first "<?php class Puppy extends Dog { }")))
    (assert-true (some (lambda (s) (string= "DOG" (symbol-name s)))
                       (cl-cc::ast-defclass-superclasses ast)))))

(deftest php-parser-class-with-property
  "class with a property slot produces ast-slot-def."
  (let* ((ast   (%php-first "<?php class Point { public $x; public $y; }"))
         (slots (cl-cc::ast-defclass-slots ast)))
    (assert-= 2 (length slots))
    (assert-true (every #'cl-cc::ast-slot-def-p slots))))

;;; ─── Expression statement (dispatch fallthrough) ─────────────────────────

(deftest php-parser-expression-statement-assign
  "Plain assignment is parsed as an expression statement."
  (let ((ast (%php-first "<?php $x = 42;")))
    ;; Assignment lowers as a setq or let form — just verify it's not a keyword node
    (assert-true (or (typep ast 'cl-cc:ast-setq)
                     (typep ast 'cl-cc:ast-let)
                     (typep ast 'cl-cc:ast-call)))))

;;; ─── Multiple top-level statements ───────────────────────────────────────

(deftest php-parser-multi-statement-source
  "parse-php-source returns all top-level statements in order."
  (let ((asts (parse-php-source "<?php echo 1; echo 2; echo 3;")))
    (assert-= 3 (length asts))
    (assert-true (every #'ast-print-p asts))))
