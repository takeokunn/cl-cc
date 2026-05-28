;;;; tests/unit/parse/php/parser-tests.lisp — PHP Parser tests
;;;;
;;;; Coverage for the dispatch-table statement parsers in parser-stmt.lisp.
;;;; Each test exercises one *php-stmt-parsers* handler via parse-php-source.
(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helper ───────────────────────────────────────────────────────────────

(defun %php-first (src)
  "Parse SRC and return the first top-level AST node."
  (first (cl-cc/php:parse-php-source src)))

(defun %php-first-binding-value (src)
  "Parse SRC as an assignment and return the value expression from the first binding."
  (let ((ast (%php-first src)))
    (assert-true (cl-cc:ast-let-p ast))
    (cdr (first (cl-cc:ast-let-bindings ast)))))

(defun %php-call-name (ast)
  "Return the symbol name for an AST-CALL function when it is a variable."
  (when (and (cl-cc:ast-call-p ast)
             (cl-cc:ast-var-p (cl-cc:ast-call-func ast)))
    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func ast)))))

;;; ─── :echo handler → ast-print ───────────────────────────────────────────

(deftest php-parser-echo-produces-ast-print
  "echo expr; lowers to ast-print wrapping the expression."
  (let ((ast (%php-first "<?php echo 42;")))
    (assert-true (ast-print-p ast))
    (assert-true (typep (cl-cc:ast-print-expr ast) 'cl-cc:ast-int))))

;;; ─── :return handler → ast-return-from ───────────────────────────────────

(deftest php-parser-return-with-value-lowering
  "return expr; lowers to ast-return-from with an ast-int value."
  (let ((ast (%php-first "<?php return 1;")))
    (assert-true (typep ast 'cl-cc:ast-return-from))
    (assert-true (typep (cl-cc:ast-return-from-value ast) 'cl-cc:ast-int))))

(deftest php-parser-bare-return-has-nil-name
  "return; (without value) lowers to ast-return-from with nil name slot."
  (let ((ast (%php-first "<?php return;")))
    (assert-true (typep ast 'cl-cc:ast-return-from))
    (assert-null (cl-cc:ast-return-from-name ast))))

;;; ─── :if handler → ast-if ────────────────────────────────────────────────

(deftest php-parser-if-produces-ast-if
  "if ($cond) { ... } lowers to ast-if."
  (assert-true (typep (%php-first "<?php if ($x) { echo 1; }") 'cl-cc:ast-if)))

(deftest php-parser-if-else-branch-is-ast-progn
  "if-else lowers to ast-if where the else slot is ast-progn."
  (let ((ast (%php-first "<?php if ($x) { echo 1; } else { echo 2; }")))
    (assert-true (ast-if-p ast))
    (assert-true (typep (cl-cc:ast-if-else ast) 'cl-cc:ast-progn))))

(deftest php-parser-if-no-else-branch-is-nil-quote
  "if without else lowers to ast-if where the else slot is ast-quote (nil)."
  (let ((ast (%php-first "<?php if ($x) { echo 1; }")))
    (assert-true (ast-if-p ast))
    (assert-true (typep (cl-cc:ast-if-else ast) 'cl-cc:ast-quote))))

;;; ─── :while handler → ast-block ──────────────────────────────────────────

(deftest php-parser-while-lowering
  "while forms lower to a block-based loop AST."
  (assert-true (typep (%php-first "<?php while ($x) { echo 1; }") 'cl-cc:ast-block)))

;;; ─── :for handler → ast-progn wrapping while ─────────────────────────────

(deftest php-parser-for-produces-ast-progn
  "for loop lowers to ast-progn(init, while-loop) with exactly 2 forms."
  (let ((ast (%php-first "<?php for ($i = 0; $i < 10; $i++) { echo $i; }")))
    (assert-true (typep ast 'cl-cc:ast-progn))
    (assert-= 2 (length (cl-cc:ast-progn-forms ast)))))

;;; ─── :foreach handler → ast-let ──────────────────────────────────────────

(deftest php-parser-foreach-simple-lowers-to-let
  "foreach ($arr as $item) lowers to ast-let."
  (assert-true (ast-let-p (%php-first "<?php foreach ($items as $item) { echo $item; }"))))

(deftest php-parser-foreach-key-value-lowers-to-let
  "foreach ($arr as $k => $v) lowers to ast-let."
  (assert-true (ast-let-p (%php-first "<?php foreach ($arr as $k => $v) { echo $v; }"))))

;;; ─── :function handler → ast-defun ───────────────────────────────────────

(deftest php-parser-function-produces-ast-defun
  "function declaration lowers to ast-defun."
  (assert-true (typep (%php-first "<?php function greet($name) { return $name; }") 'cl-cc:ast-defun)))

(deftest php-parser-function-name-and-params-captured
  "function add($a, $b) captures upcased name ADD and 2 params."
  (let ((ast (%php-first "<?php function add($a, $b) { return $a; }")))
    (assert-equal "ADD" (symbol-name (cl-cc:ast-defun-name ast)))
    (assert-= 2 (length (cl-cc:ast-defun-params ast)))))

(deftest php-parser-function-no-params-is-nil
  "function noop() with no params produces nil params slot."
  (let ((ast (%php-first "<?php function noop() { return 0; }")))
    (assert-true (typep ast 'cl-cc:ast-defun))
    (assert-null (cl-cc:ast-defun-params ast))))

;;; ─── :class handler → ast-defclass ───────────────────────────────────────

(deftest php-parser-class-lowering
  "class declaration lowers to ast-defclass with upcased name."
  (assert-true (typep (%php-first "<?php class Dog { }") 'cl-cc:ast-defclass))
  (let ((ast (%php-first "<?php class Cat { }")))
    (assert-equal "CAT" (symbol-name (cl-cc:ast-defclass-name ast)))))

(deftest php-parser-class-with-extends
  "class Foo extends Bar captures superclass by upcased name."
  (let ((ast (%php-first "<?php class Puppy extends Dog { }")))
    (assert-true (some (lambda (s) (string= "DOG" (symbol-name s)))
                       (cl-cc:ast-defclass-superclasses ast)))))

(deftest php-parser-class-with-property
  "class with a property slot produces ast-slot-def."
  (let* ((ast   (%php-first "<?php class Point { public $x; public $y; }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-= 2 (length slots))
    (assert-true (every #'cl-cc:ast-slot-def-p slots))))

;;; ─── Expression statement (dispatch fallthrough) ─────────────────────────

(deftest php-parser-expression-statement-assign
  "Plain assignment is parsed as an expression statement."
  (let ((ast (%php-first "<?php $x = 42;")))
    (assert-true (or (typep ast 'cl-cc:ast-setq)
                     (typep ast 'cl-cc:ast-let)
                     (typep ast 'cl-cc:ast-call)))))

;;; ─── Multiple top-level statements ───────────────────────────────────────

(deftest php-parser-multi-statement-source
  "parse-php-source returns all top-level statements in order."
  (let ((asts (cl-cc/php:parse-php-source "<?php echo 1; echo 2; echo 3;")))
    (assert-= 3 (length asts))
    (assert-true (every #'ast-print-p asts))))

;;; ─── Characterization tests for unsupported PHP support gaps ───────────────

(deftest php-parser-match-expression
  "Characterization: match should lower to a conditional AST, not a placeholder call."
  (let ((value (%php-first-binding-value
                "<?php $result = match($x) { 1 => 'one', 2 => 'two', default => 'other' };")))
    (assert-true (cl-cc:ast-if-p value))
    (assert-false (string= "MATCH" (or (%php-call-name value) "")))))

(deftest php-parser-null-coalesce-expression
  "Characterization: ?? should preserve the PHP null-coalescing operator in the AST."
  (let ((value (%php-first-binding-value "<?php $result = $a ?? $b;")))
    (assert-true (cl-cc:ast-binop-p value))
    (assert-equal '?? (cl-cc:ast-binop-op value))))

(deftest php-parser-ternary-expression
  "Characterization: ternary ?: should parse as ast-if with cond/then/else."
  (let ((value (%php-first-binding-value "<?php $result = $cond ? $yes : $no;")))
    (assert-true (cl-cc:ast-if-p value))))

(deftest php-parser-arrow-function-expression
  "Characterization: fn($x) => $x + 1 should parse as a real lambda with parameters and body."
  (let ((value (%php-first-binding-value "<?php $inc = fn($x) => $x + 1;")))
    (assert-true (cl-cc:ast-lambda-p value))
    (assert-equal '(X) (cl-cc:ast-lambda-params value))))

(deftest php-parser-yield-expression-unsupported-error
  "Characterization: yield remains explicitly unsupported until generator lowering is implemented."
  (assert-signals error
    (cl-cc/php:parse-php-source "<?php function g() { yield 1; }")))

(deftest php-parser-yield-from-expression-unsupported-error
  "Characterization: yield from remains explicitly unsupported until delegation generators exist."
  (assert-signals error
    (cl-cc/php:parse-php-source "<?php function g() { yield from $items; }")))

(deftest php-parser-switch-case-default-statement
  "switch/case/default lowers to let + block + tagbody dispatch/fallthrough control flow."
  (let ((ast (%php-first "<?php switch ($x) { case 1: echo 'one'; break; default: echo 'other'; }")))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((block (first (cl-cc:ast-let-body ast))))
      (assert-true (cl-cc:ast-block-p block))
      (let ((tagbody (first (cl-cc:ast-block-body block))))
        (assert-true (cl-cc:ast-tagbody-p tagbody))
        (assert-true (some (lambda (section)
                             (some #'cl-cc:ast-if-p (cdr section)))
                           (cl-cc:ast-tagbody-tags tagbody)))
        (assert-true (some (lambda (section)
                             (some #'cl-cc:ast-go-p (cdr section)))
                           (cl-cc:ast-tagbody-tags tagbody)))))))

(deftest php-parser-break-continue-level-statements
  "break N and continue N lower to ast-go within nested loop tagbodies."
  (let ((ast (%php-first "<?php while ($a) { while ($b) { continue 2; break 2; } }")))
    (assert-true (cl-cc:ast-block-p ast))
    (let ((outer-tagbody (first (cl-cc:ast-block-body ast))))
      (assert-true (cl-cc:ast-tagbody-p outer-tagbody))
      (assert-true (some (lambda (section)
                           (some (lambda (form)
                                   (and (cl-cc:ast-block-p form)
                                        (cl-cc:ast-tagbody-p (first (cl-cc:ast-block-body form)))))
                                 (cdr section)))
                         (cl-cc:ast-tagbody-tags outer-tagbody))))))

(deftest php-parser-try-catch-finally-statement
  "Characterization: try/catch/finally should preserve catch type/variable and cleanup metadata."
  (let ((ast (%php-first "<?php try { throw new Ex(); } catch (Ex $e) { echo $e; } finally { echo 'done'; }")))
    (assert-true (cl-cc:ast-unwind-protect-p ast))
    (assert-true (cl-cc:ast-handler-case-p (cl-cc:ast-unwind-protected ast)))
    (assert-equal 'EX (first (first (cl-cc:ast-handler-case-clauses
                                     (cl-cc:ast-unwind-protected ast)))))))

(deftest php-parser-throw-statement
  "Characterization: throw should parse as ast-throw without being treated as a value-only expression placeholder."
  (let ((ast (%php-first "<?php throw new Ex();")))
    (assert-true (cl-cc:ast-throw-p ast))
    (assert-equal 'php-exception (cl-cc:ast-quote-value (cl-cc:ast-throw-tag ast)))))

(deftest php-parser-short-array-literal
  "Characterization: [1,2,3] should preserve an ordered PHP array literal node."
  (let ((value (%php-first-binding-value "<?php $xs = [1, 2, 3];")))
    (assert-true (cl-cc:ast-call-p value))
    (assert-string= "%PHP-ARRAY" (%php-call-name value))
    (assert-= 3 (length (cl-cc:ast-call-args value)))
    (assert-true (every #'cl-cc:ast-list-p (cl-cc:ast-call-args value)))))

(deftest php-parser-associative-array-literal
  "Characterization: [\"a\"=>1,\"b\"=>2] should preserve key/value pairs."
  (let ((value (%php-first-binding-value "<?php $map = [\"a\" => 1, \"b\" => 2];")))
    (assert-true (cl-cc:ast-call-p value))
    (assert-string= "%PHP-ARRAY" (%php-call-name value))
    (assert-= 2 (length (cl-cc:ast-call-args value)))
    (assert-true
     (every (lambda (entry)
              (and (cl-cc:ast-list-p entry)
                   (cl-cc:ast-quote-value (first (cl-cc:ast-list-elements entry)))))
            (cl-cc:ast-call-args value)))))

(deftest php-parser-legacy-array-literal
  "Characterization: array(1,2,3) should preserve PHP array literal semantics."
  (let ((value (%php-first-binding-value "<?php $xs = array(1, 2, 3);")))
    (assert-true (cl-cc:ast-call-p value))
    (assert-string= "%PHP-ARRAY" (%php-call-name value))
    (assert-= 3 (length (cl-cc:ast-call-args value)))))

(deftest php-parser-array-element-access
  "$a[0] lowers to the ordered PHP array reference helper."
  (let ((value (%php-first-binding-value "<?php $x = $a[0];")))
    (assert-true (cl-cc:ast-call-p value))
    (assert-string= "%PHP-ARRAY-REF" (%php-call-name value))
    (assert-= 2 (length (cl-cc:ast-call-args value)))))

(deftest php-parser-array-element-assignment
  "$a[0] = $v lowers to the ordered PHP array mutation helper."
  (let ((ast (%php-first "<?php $a[0] = $v;")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%PHP-ARRAY-SET" (%php-call-name ast))
    (assert-= 3 (length (cl-cc:ast-call-args ast)))))

(deftest php-parser-namespace-use-metadata-preservation
  "namespace/use declarations annotate subsequent top-level AST nodes."
  (let ((asts (cl-cc/php:parse-php-source "<?php namespace App\\Lib; use Vendor\\Thing as Thing; function f() { return 1; }")))
    (assert-= 1 (length asts))
    (assert-string= "App\\Lib" (cl-cc:ast-namespace (first asts)))
    (assert-equal '((:type :class :name "Vendor\\Thing" :alias "Thing"))
                  (cl-cc:ast-imports (first asts)))))

(deftest php-parser-braced-namespace-and-group-use-metadata
  "Braced namespaces and grouped function/const imports annotate enclosed forms."
  (let ((asts (cl-cc/php:parse-php-source
               "<?php namespace App\\Lib { use function Vendor\\Fns\\{foo, bar as baz}; function f() { return 1; } class C {} }")))
    (assert-= 2 (length asts))
    (assert-true (every (lambda (ast) (string= "App\\Lib" (cl-cc:ast-namespace ast))) asts))
    (assert-equal '((:type :function :name "Vendor\\Fns\\foo" :alias nil)
                    (:type :function :name "Vendor\\Fns\\bar" :alias "baz"))
                  (cl-cc:ast-imports (first asts)))
    (assert-equal (cl-cc:ast-imports (first asts))
                  (cl-cc:ast-imports (second asts)))))

(deftest php-parser-function-type-annotation-preservation
  "Characterization: function parameter and return type annotations should be preserved as declarations."
  (let ((ast (%php-first "<?php function add(int $a, ?string $b = null, int|string $c): bool { return true; }")))
    (assert-true (cl-cc:ast-defun-p ast))
    (let ((decls (cl-cc:ast-defun-declarations ast)))
      (assert-equal "bool" (getf decls :php-return-type))
      (assert-equal '(("A" . "int") ("B" . "?string") ("C" . "int|string"))
                    (mapcar (lambda (entry)
                              (cons (symbol-name (car entry)) (cdr entry)))
                            (getf decls :php-param-types))))))

(deftest php-parser-function-special-return-types
  "PHP return type annotations preserve void, never, mixed, static, nullable, union, and intersection spelling."
  (dolist (case '(("void" . "void")
                  ("never" . "never")
                  ("mixed" . "mixed")
                  ("static" . "static")
                  ("?int" . "?int")
                  ("int|string|null" . "int|string|null")
                  ("Countable&Iterator" . "countable&iterator")))
    (let* ((source (format nil "<?php function f(): ~A { return 1; }" (car case)))
           (ast (%php-first source)))
      (assert-equal (cdr case)
                    (getf (cl-cc:ast-defun-declarations ast) :php-return-type)))))

(deftest php-parser-class-typed-properties
  "Characterization: class typed properties should preserve their declared PHP types on slot definitions."
  (let* ((ast (%php-first "<?php class User { public int $id; private ?string $name; readonly public int|float $score; }"))
          (slots (cl-cc:ast-defclass-slots ast)))
    (assert-= 3 (length slots))
    (assert-equal '("int" "?string" "int|float") (mapcar #'cl-cc:ast-slot-type slots))
    (assert-true (member :readonly (getf (cl-cc:ast-imports (third slots)) :php-modifiers)))))

(deftest php-parser-class-typed-constants
  "PHP class constants preserve optional type annotations as class-scoped metadata slots."
  (let* ((ast (%php-first "<?php class C { const int FOO = 1; const BAR = 'x'; }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-= 2 (length slots))
    (assert-equal '("FOO" "BAR") (mapcar (lambda (slot)
                                             (symbol-name (cl-cc:ast-slot-name slot)))
                                           slots))
    (assert-equal '("int" nil) (mapcar #'cl-cc:ast-slot-type slots))
    (assert-true (every (lambda (slot)
                          (and (eq :class (cl-cc:ast-slot-allocation slot))
                               (getf (cl-cc:ast-imports slot) :php-class-constant)))
                        slots))))
