;;;; tests/unit/parse/php/parser-tests.lisp — PHP Parser tests
;;;;
;;;; Coverage for the dispatch-table statement parsers in parser-stmt-decls.lisp.
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

(defun %php-assert-full-source-unsupported (src)
  "Assert that checking every parsed form in SRC rejects unsupported PHP."
  (assert-signals error
    (cl-cc/php:php-check-supported-forms
     (cl-cc/php:parse-php-source src))))

;;; ─── :echo handler → output helper call (no trailing newline) ────────────

(deftest php-parser-echo-lowers-to-output-write-call
  "echo expr; lowers to a %php-output-write call (no trailing newline, output
buffer aware) wrapping a %php-concat call (echo applies PHP string conversion to
each value); the concat's first arg is the echoed expr."
  (let ((ast (%php-first "<?php echo 42;")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%PHP-OUTPUT-WRITE" (%php-call-name ast))
    (let ((expr (first (cl-cc:ast-call-args ast))))
      (assert-true (cl-cc:ast-call-p expr))
      (assert-string= "%PHP-CONCAT" (%php-call-name expr))
      (assert-true (typep (first (cl-cc:ast-call-args expr)) 'cl-cc:ast-int)))))

(deftest php-parser-reference-assignment-lowers-to-ref-box
  "$b = &$a boxes $a, binds $b to the same box, and dereferences later reads."
  (let* ((ast (%php-first "<?php $a=1; $b=&$a; echo $b;"))
         (body (cl-cc:ast-let-body ast))
         (box-set (first body))
         (alias-let (second body))
         (echo (first (cl-cc:ast-let-body alias-let)))
         (concat (first (cl-cc:ast-call-args echo)))
         (deref (first (cl-cc:ast-call-args concat))))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-true (cl-cc:ast-setq-p box-set))
    (assert-string= "%PHP-MAKE-REF" (%php-call-name (cl-cc:ast-setq-value box-set)))
    (assert-true (cl-cc:ast-let-p alias-let))
    (assert-true (cl-cc:ast-var-p (cdr (first (cl-cc:ast-let-bindings alias-let)))))
    (assert-string= "%PHP-DEREF" (%php-call-name deref))))

(deftest php-parser-settype-lowers-first-arg-by-reference
  "settype($x, ...) boxes the first argument and writes it back after the call."
  (let ((found-box nil)
        (found-writeback nil))
    (labels ((walk (node)
               (when (cl-cc:ast-call-p node)
                 (when (string= "%PHP-MAKE-REF" (%php-call-name node))
                   (setf found-box t)))
               (when (cl-cc:ast-setq-p node)
                 (when (string= (symbol-name (cl-cc:ast-setq-var node)) "x")
                   (setf found-writeback t)))
               (when (typep node 'cl-cc:ast-node)
                 (dolist (child (cl-cc:ast-children node))
                   (when child
                     (walk child))))))
      (dolist (ast (cl-cc/php:parse-php-source "<?php $x=5; settype($x,'string');"))
        (walk ast)))
    (assert-true found-box)
    (assert-true found-writeback)))

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

;;; ─── Simple statement → AST-type checks ─────────────────────────────────

(deftest-each php-parser-stmt-ast-type
  "Each statement construct lowers to the expected AST node type."
  :cases (("if"          "<?php if ($x) { echo 1; }"                          #'cl-cc:ast-if-p)
          ("while"       "<?php while ($x) { echo 1; }"                       #'cl-cc:ast-block-p)
          ("foreach"     "<?php foreach ($items as $item) { echo $item; }"    #'cl-cc:ast-let-p)
          ("foreach-kv"  "<?php foreach ($arr as $k => $v) { echo $v; }"      #'cl-cc:ast-let-p)
          ("function"    "<?php function greet($name) { return $name; }"       #'cl-cc:ast-defun-p))
  (src pred)
  (assert-true (funcall pred (%php-first src))))

;;; ─── :if handler → ast-if ────────────────────────────────────────────────

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

;;; ─── :for handler → ast-progn wrapping while ─────────────────────────────

(deftest php-parser-for-produces-ast-progn
  "for ($i=0;...) lowers to an ast-progn whose single form is the init's let,
nesting the while-loop so the loop variable scopes over cond/body/increment.
(Previously the init and while-loop were two sibling forms, leaving $i unscoped
and the loop producing no output.)"
  (let ((ast (%php-first "<?php for ($i = 0; $i < 10; $i++) { echo $i; }")))
    (assert-true (typep ast 'cl-cc:ast-progn))
    ;; $i = 0 introduces a new variable, so php-finish-let-bindings nests the
    ;; while-loop inside that let — one progn form (the let), not two siblings.
    (assert-= 1 (length (cl-cc:ast-progn-forms ast)))
    (assert-true (typep (first (cl-cc:ast-progn-forms ast)) 'cl-cc:ast-let))))

;;; ─── :function handler → ast-defun ───────────────────────────────────────

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

(deftest php-parser-class-with-implements
  "class Foo implements A, B preserves interface names in class ancestry metadata."
  (let* ((ast (%php-first "<?php class Box implements IfaceA, IfaceB { }"))
         (names (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (assert-equal '("IFACEA" "IFACEB") names)))

(deftest php-parser-class-with-property
  "class with a property slot produces ast-slot-def."
  (let* ((ast   (%php-first "<?php class Point { public $x; public $y; }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-= 2 (length slots))
    (assert-true (every #'cl-cc:ast-slot-def-p slots))))

(deftest php-parser-bare-defclass-is-rejected
  "A raw ast-defclass without a PHP kind marker is rejected by support checks."
  (assert-signals error
    (cl-cc/php:php-check-supported-forms
     (list (cl-cc:make-ast-defclass)))))

;;; ─── Expression statement (dispatch fallthrough) ─────────────────────────

(deftest php-parser-expression-statement-assign
  "Plain assignment is parsed as an expression statement."
  (let ((ast (%php-first "<?php $x = 42;")))
    (assert-true (or (typep ast 'cl-cc:ast-setq)
                      (typep ast 'cl-cc:ast-let)
                      (typep ast 'cl-cc:ast-call)))))

(deftest php-parser-variable-names-preserve-case
  "PHP variables are case-sensitive: $foo, $FOO, and $Foo are distinct AST symbols.
After php-finish-let-bindings the 3 assignments nest into one top-level let chain."
  (let* ((asts (cl-cc/php:parse-php-source "<?php $foo = 1; $FOO = 2; $Foo = 3;"))
         (names nil))
    ;; Walk the nested let chain collecting each variable name
    (labels ((collect (nodes)
               (dolist (node nodes)
                 (when (cl-cc:ast-let-p node)
                   (push (symbol-name (car (first (cl-cc:ast-let-bindings node)))) names)
                   (collect (cl-cc:ast-let-body node))))))
      (collect asts))
    (setf names (nreverse names))
    (assert-equal '("foo" "FOO" "Foo") names)
    (assert-= 3 (length (remove-duplicates names :test #'string=)))))

;;; ─── Multiple top-level statements ───────────────────────────────────────

(deftest php-parser-multi-statement-source
  "parse-php-source returns all top-level statements in order."
  (let ((asts (cl-cc/php:parse-php-source "<?php echo 1; echo 2; echo 3;")))
    (assert-= 3 (length asts))
    (assert-true (every (lambda (a)
                          (and (cl-cc:ast-call-p a)
                               (string= "%PHP-OUTPUT-WRITE" (%php-call-name a))))
                        asts))))

;;; ─── Characterization tests for unsupported PHP support gaps ───────────────

(deftest php-parser-null-distinct-from-false
  "null and false produce different AST quote values."
  (let ((null-ast (%php-first "<?php $x = null;"))
        (false-ast (%php-first "<?php $x = false;")))
    (let ((null-val (cl-cc:ast-quote-value (cdr (first (cl-cc:ast-let-bindings null-ast)))))
          (false-val (cl-cc:ast-quote-value (cdr (first (cl-cc:ast-let-bindings false-ast))))))
      (assert-false (eql null-val false-val)))))

(deftest php-parser-truthiness-rules
  "PHP conditionals should lower conditions through PHP truthiness rules."
  (let ((ast (%php-first "<?php if ($x) { echo 1; } else { echo 2; }")))
    (assert-true (cl-cc:ast-if-p ast))
    (let ((cond (cl-cc:ast-if-cond ast)))
      (assert-true (cl-cc:ast-call-p cond))
      (assert-string= "%PHP-TRUTHY" (%php-call-name cond)))))

(deftest php-parser-variable-case-sensitive
  "$foo and $FOO produce different variable symbols."
  (let ((ast (%php-first "<?php $foo = $FOO;")))
    (assert-true (cl-cc:ast-let-p ast))
    (let* ((bindings (cl-cc:ast-let-bindings ast))
           (lhs (car (first bindings)))
           (rhs (cdr (first bindings))))
      (assert-true (cl-cc:ast-var-p rhs))
      (assert-false (string= (symbol-name lhs) (symbol-name (cl-cc:ast-var-name rhs)))))))

(deftest php-parser-count-builtin-lowering
  "count($arr) should lower to %php-count helper, not raw function call."
  (let ((ast (%php-first "<?php $n = count($arr);")))
    (let ((call (cdr (first (cl-cc:ast-let-bindings ast)))))
      (assert-true (cl-cc:ast-call-p call))
      (assert-string= "%PHP-COUNT" (%php-call-name call)))))

(deftest php-parser-absolute-count-builtin-lowering
  "\\count($arr) should lower to the global %php-count helper."
  (let ((ast (%php-first "<?php namespace App\\Lib; $n = \\count($arr);")))
    (let ((call (cdr (first (cl-cc:ast-let-bindings ast)))))
      (assert-true (cl-cc:ast-call-p call))
      (assert-string= "%PHP-COUNT" (%php-call-name call)))))

(deftest php-parser-namespaced-count-call-does-not-force-global-builtin
  "Unqualified count() inside a namespace must remain fallback-safe, not force %php-count."
  (let* ((asts (cl-cc/php:parse-php-source
                "<?php namespace App\\Lib; function count($xs) { return 99; } $n = count($arr);"))
         (call (cdr (first (cl-cc:ast-let-bindings (second asts))))))
    (assert-true (cl-cc:ast-call-p call))
    (assert-string= "COUNT" (%php-call-name call))))

(deftest php-parser-isset-syntax-lowering
  "isset($x) should be lowered without treating $x as a variable reference."
  (let ((ast (%php-first "<?php $result = isset($x);")))
    (let ((call (cdr (first (cl-cc:ast-let-bindings ast)))))
      (assert-true (cl-cc:ast-call-p call))
      (assert-true (search "ISSET" (%php-call-name call))))))

(deftest php-parser-empty-variable-syntax-lowering
  "empty($x) should avoid evaluating an undefined variable operand."
  (let ((value (%php-first-binding-value "<?php $result = empty($x);")))
    (assert-true (cl-cc:ast-quote-p value))
    (assert-eq t (cl-cc:ast-quote-value value)))
  (let* ((let-x (%php-first "<?php $x = 0; $result = empty($x);"))
         (let-result (first (cl-cc:ast-let-body let-x)))
         (value (cdr (first (cl-cc:ast-let-bindings let-result)))))
    (assert-true (cl-cc:ast-call-p value))
    (assert-true (search "EMPTY" (%php-call-name value)))))

(deftest php-parser-match-strict-comparison
  "match should use strict equality, not EQUAL."
  (let ((ast (%php-first "<?php $x = match($v) { 1 => 'one', 2 => 'two' };")))
    (let ((val (cdr (first (cl-cc:ast-let-bindings ast)))))
      (assert-true (cl-cc:ast-let-p val))
      (let ((if-chain (first (cl-cc:ast-let-body val))))
        (assert-true (cl-cc:ast-if-p if-chain))
        (let ((cond (cl-cc:ast-if-cond if-chain)))
          (assert-true (cl-cc:ast-call-p cond))
          (assert-false (string= "EQUAL" (%php-call-name cond))))))))

(deftest php-parser-foreach-ordered-iteration
  "foreach should bind key and value and iterate array order."
  (let ((ast (%php-first "<?php foreach ($arr as $k => $v) { echo $v; }")))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((bindings (cl-cc:ast-let-bindings ast)))
      (assert-= 2 (length bindings)))))

(deftest php-parser-throw-catch-consistency
  "throw inside try should produce catchable exception structure."
  (let ((ast (%php-first "<?php try { throw new Ex(); } catch (Ex $e) { echo 'caught'; }")))
    (assert-true (cl-cc:ast-unwind-protect-p ast))
    (assert-true (cl-cc:ast-let-p (cl-cc:ast-unwind-protected ast)))))

(deftest php-parser-match-expression
  "match lowers to a subject let with nested conditional dispatch."
  (let ((value (%php-first-binding-value
                "<?php $result = match($x) { 1 => 'one', 2 => 'two', default => 'other' };")))
    (assert-true (cl-cc:ast-let-p value))
    (assert-true (cl-cc:ast-if-p (first (cl-cc:ast-let-body value))))
    (assert-false (string= "MATCH" (or (%php-call-name value) "")))))

(deftest php-parser-null-coalesce-expression
  "?? lowers to a temp let so the left-hand side is evaluated only once."
  (let ((value (%php-first-binding-value "<?php $result = $a ?? $b;")))
    (assert-true (cl-cc:ast-let-p value))
    (assert-true (cl-cc:ast-if-p (first (cl-cc:ast-let-body value))))))

(deftest php-parser-ternary-expression
  "Characterization: ternary ?: should parse as ast-if with cond/then/else."
  (let ((value (%php-first-binding-value "<?php $result = $cond ? $yes : $no;")))
    (assert-true (cl-cc:ast-if-p value))))

;;; ─── Operator helper lowering ────────────────────────────────────────────

(deftest-each php-parser-operator-helper-lowering
  "Binary/unary operators lower to named PHP helper functions via %php-first-binding-value."
  :cases (("modulo"         "<?php $r = 7 % 4;"         "%PHP-MODULO")
          ("bitwise-not"    "<?php $r = ~1;"             "%PHP-BITWISE-NOT")
          ("unary-plus"     "<?php $r = +'7';"           "%PHP-UNARY-PLUS")
          ("unary-minus"    "<?php $r = -'7';"           "%PHP-UNARY-MINUS")
          ("spaceship"      "<?php $r = $a <=> $b;"      "%PHP-SPACESHIP")
          ("str-interp"     "<?php $s = \"Hello $name\";" "%PHP-CONCAT")
          ("braced-interp"  "<?php $s = \"Hello {$name}\";" "%PHP-CONCAT")
          ("array-ref"      "<?php $x = $a[0];"          "%PHP-ARRAY-REF")
          ("bitwise-and"    "<?php $x = $a & $b;"        "%PHP-BITWISE-AND"))
  (src expected-fn)
  (let ((val (%php-first-binding-value src)))
    (assert-true (cl-cc:ast-call-p val))
    (assert-string= expected-fn (%php-call-name val))))

(deftest php-parser-exponentiation-is-right-associative
  "** parses above unary and associates to the right."
  (let ((value (%php-first-binding-value "<?php $result = 2 ** 3 ** 2;")))
    (assert-true (cl-cc:ast-call-p value))
    (assert-string= "EXPT" (%php-call-name value))
    (assert-true (cl-cc:ast-call-p (second (cl-cc:ast-call-args value))))
    (assert-string= "EXPT" (%php-call-name (second (cl-cc:ast-call-args value))))))

(deftest php-parser-shift-operators-lower-to-helpers
  "<< and >> lower to PHP shift helpers."
  (let ((left (%php-first-binding-value "<?php $result = 1 << 3;"))
        (right (%php-first-binding-value "<?php $result = 8 >> 1;")))
    (assert-string= "%PHP-SHIFT-LEFT" (%php-call-name left))
    (assert-string= "%PHP-SHIFT-RIGHT" (%php-call-name right))))

(deftest php-parser-shift-precedence-is-below-addition
  "Addition binds tighter than shifts in PHP 8.x.  (+ now lowers to a %php-add
helper call — operand-coercing — so the shift's left operand is that call.)"
  (let ((value (%php-first-binding-value "<?php $result = 1 + 2 << 3;")))
    (assert-string= "%PHP-SHIFT-LEFT" (%php-call-name value))
    (assert-string= "%PHP-ADD" (%php-call-name (first (cl-cc:ast-call-args value))))))

(deftest php-parser-concat-precedence-is-below-addition
  "String concatenation binds looser than + and - in PHP 8.x."
  (let ((value (%php-first-binding-value "<?php $result = 1 + 2 . 3;")))
    (assert-string= "%PHP-CONCAT" (%php-call-name value))
    (assert-string= "%PHP-ADD" (%php-call-name (first (cl-cc:ast-call-args value))))))

(deftest php-parser-bitwise-operators-lower-to-helpers
  "&, ^, and | lower to PHP bitwise helpers."
  (let ((and-value (%php-first-binding-value "<?php $result = 6 & 3;"))
        (xor-value (%php-first-binding-value "<?php $result = 6 ^ 3;"))
        (or-value (%php-first-binding-value "<?php $result = 4 | 1;")))
    (assert-string= "%PHP-BITWISE-AND" (%php-call-name and-value))
    (assert-string= "%PHP-BITWISE-XOR" (%php-call-name xor-value))
    (assert-string= "%PHP-BITWISE-OR" (%php-call-name or-value))))

(deftest php-parser-bitwise-precedence-follows-php-order
  "Comparison binds above &, which binds above ^, which binds above |."
  (let ((value (%php-first-binding-value "<?php $result = 1 == 1 & 6 ^ 3 | 8;")))
    (assert-string= "%PHP-BITWISE-OR" (%php-call-name value))
    (let ((xor-node (first (cl-cc:ast-call-args value))))
      (assert-string= "%PHP-BITWISE-XOR" (%php-call-name xor-node))
      (let ((and-node (first (cl-cc:ast-call-args xor-node))))
        (assert-string= "%PHP-BITWISE-AND" (%php-call-name and-node))
        ;; == lowers to a %php-eq-loose call (PHP loose-equality type juggling),
        ;; and binds tighter than &, so it is the AND node's first operand.
        (let ((eq-node (first (cl-cc:ast-call-args and-node))))
          (assert-true (cl-cc:ast-call-p eq-node))
          (assert-string= "%PHP-EQ-LOOSE" (%php-call-name eq-node)))))))

(deftest php-parser-arrow-function-expression
  "Characterization: fn($x) => $x + 1 should parse to a capture-wrapped ast-lambda."
  (let ((value (%php-first-binding-value "<?php $inc = fn($x) => $x + 1;")))
    ;; fn arrow functions wrap the lambda in a capture let-binding
    (assert-true (cl-cc:ast-let-p value))
    (let ((lambda (first (cl-cc:ast-let-body value))))
      (assert-true (cl-cc:ast-lambda-p lambda))
      (assert-equal '("x") (mapcar #'symbol-name (cl-cc:ast-lambda-params lambda))))))

(defun %php-generator-body-block (ast)
  "For a yield-containing function AST, return the inner (block nil ...) that
%php-callable-body wraps. The generator lowering is:
  (let ((gen (%php-generator-enter)))
    (%php-generator-exit gen <block>)
    gen)
so the block is the second arg of the %php-generator-exit call."
  (let* ((let-form  (first (cl-cc:ast-defun-body ast)))
         (exit-call (first (cl-cc:ast-let-body let-form))))
    (second (cl-cc:ast-call-args exit-call))))

(deftest php-parser-yield-expression-lowering
  "A function with yield becomes a generator: its body is threaded through
%php-generator-enter / -exit, and yield lowers to the %php-yield helper."
  (let* ((ast (%php-first "<?php function g() { yield 1; }"))
         (let-form (first (cl-cc:ast-defun-body ast)))
         (enter-call (cdr (first (cl-cc:ast-let-bindings let-form))))
         (block (%php-generator-body-block ast))
         (yield-call (first (cl-cc:ast-block-body block))))
    (cl-cc/php:php-check-supported-forms (list ast))
    (assert-true (cl-cc:ast-let-p let-form))
    (assert-string= "%PHP-GENERATOR-ENTER" (%php-call-name enter-call))
    (assert-true (cl-cc:ast-block-p block))
    (assert-true (cl-cc:ast-call-p yield-call))
    (assert-string= "%PHP-YIELD" (%php-call-name yield-call))))

(deftest php-parser-yield-from-expression-lowering
  "yield from lowers to the %php-yield-from helper inside the generator body."
  (let* ((ast (%php-first "<?php function g() { yield from $items; }"))
         (block (%php-generator-body-block ast))
         (yield-call (first (cl-cc:ast-block-body block))))
    (cl-cc/php:php-check-supported-forms (list ast))
    (assert-true (cl-cc:ast-block-p block))
    (assert-true (cl-cc:ast-call-p yield-call))
    (assert-string= "%PHP-YIELD-FROM" (%php-call-name yield-call))))

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
  "Characterization: try/catch/finally should produce unwind-protect wrapping catch dispatch."
  (let ((ast (%php-first "<?php try { throw new Ex(); } catch (Ex $e) { echo $e; } finally { echo 'done'; }")))
    (assert-true (cl-cc:ast-unwind-protect-p ast))
    (let ((inner (cl-cc:ast-unwind-protected ast)))
      (assert-true (cl-cc:ast-let-p inner))
      (let ((dispatch (first (cl-cc:ast-let-body inner))))
        (assert-true (cl-cc:ast-if-p dispatch))))))

(deftest php-parser-catch-union-types
  "PHP catch clauses preserve union type alternatives for runtime dispatch."
  (let* ((ast (%php-first "<?php try { throw new ExA(); } catch (ExA | ExB $e) { echo $e; }"))
         (inner (cl-cc:ast-unwind-protected ast))
         (top-dispatch (first (cl-cc:ast-let-body inner)))
         (catch-dispatch (cl-cc:ast-if-then top-dispatch))
         (match-cond (cl-cc:ast-if-cond catch-dispatch))
         (class-arg (second (cl-cc:ast-call-args match-cond))))
    (assert-true (cl-cc:ast-call-p match-cond))
    (assert-equal '("EXA" "EXB")
                  (mapcar #'symbol-name (cl-cc:ast-quote-value class-arg)))))

(deftest php-parser-throw-statement
  "Characterization: throw should parse as ast-throw with PHP exception payload metadata."
  (let ((ast (%php-first "<?php throw new Ex();")))
    (assert-true (cl-cc:ast-throw-p ast))
    (let ((tag-val (cl-cc:ast-quote-value (cl-cc:ast-throw-tag ast))))
      (assert-true (symbolp tag-val))
      (assert-true (search "EXCEPTION" (symbol-name tag-val))))
    (assert-true (cl-cc:ast-call-p (cl-cc:ast-throw-value ast)))
    (assert-string= "%PHP-MAKE-EXCEPTION" (%php-call-name (cl-cc:ast-throw-value ast)))))

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

(deftest php-parser-array-function-style-literal
  "Characterization: array(1,2,3) should preserve PHP array literal semantics."
  (let ((value (%php-first-binding-value "<?php $xs = array(1, 2, 3);")))
    (assert-true (cl-cc:ast-call-p value))
    (assert-string= "%PHP-ARRAY" (%php-call-name value))
    (assert-= 3 (length (cl-cc:ast-call-args value)))))

(deftest php-parser-array-element-access
  "$a[0] lowers to %PHP-ARRAY-REF with exactly 2 arguments (array + index)."
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

(deftest php-parser-compound-assignment-variable
  "$x += 5 — after php-finish-let-bindings, $x=0 wraps $x+=5 in its body."
  (let* ((asts (cl-cc/php:parse-php-source "<?php $x = 0; $x += 5;"))
         ;; $x=0 let wraps $x+=5 in its body; $x+=5 is first in that body
         (compound (first (cl-cc:ast-let-body (first asts)))))
    (assert-true (cl-cc:ast-let-p compound))
    (let ((setq (first (cl-cc:ast-let-body compound))))
      (assert-true (cl-cc:ast-setq-p setq))
      (assert-string= "x" (symbol-name (cl-cc:ast-setq-var setq)))
      (let ((value (cl-cc:ast-setq-value setq)))
        (assert-true (cl-cc:ast-call-p value))
        (assert-string= "%PHP-ADD" (%php-call-name value))))))

(deftest php-parser-compound-assignment-array-element
  "$arr[0] += 1 lowers to an array-set around an array-ref read."
  (let ((ast (%php-first "<?php $arr[0] += 1;")))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((set-call (first (cl-cc:ast-let-body ast))))
      (assert-true (cl-cc:ast-call-p set-call))
      (assert-string= "%PHP-ARRAY-SET" (%php-call-name set-call))
      (let ((value (third (cl-cc:ast-call-args set-call))))
        (assert-true (cl-cc:ast-call-p value))
        (assert-string= "%PHP-ADD" (%php-call-name value))
        (assert-string= "%PHP-ARRAY-REF"
                        (%php-call-name (first (cl-cc:ast-call-args value))))))))

(deftest php-parser-null-coalescing-assignment-variable
  "$x ??= 42 on a KNOWN variable lowers to a null-checking conditional
assignment.  ($x is pre-declared; on an undefined var ??= introduces it to the
RHS directly, a different and simpler shape.)"
  (let* ((outer (%php-first "<?php $x = 1; $x ??= 42;"))
         (ast (first (cl-cc:ast-let-body outer))))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((if-node (first (cl-cc:ast-let-body ast))))
      (assert-true (cl-cc:ast-if-p if-node))
      (assert-true (cl-cc:ast-setq-p (cl-cc:ast-if-then if-node)))
      (assert-string= "x" (symbol-name (cl-cc:ast-setq-var (cl-cc:ast-if-then if-node)))))))

(deftest php-parser-compound-assignment-property
  "$obj->count *= 3 lowers to a slot write using the previous slot value."
  (let ((ast (%php-first "<?php $obj->count *= 3;")))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((slot-set (first (cl-cc:ast-let-body ast))))
      (assert-true (cl-cc:ast-set-slot-value-p slot-set))
      (assert-string= "COUNT" (symbol-name (cl-cc:ast-set-slot-value-slot slot-set)))
      (let ((value (cl-cc:ast-set-slot-value-value slot-set)))
        (assert-true (cl-cc:ast-call-p value))
        (assert-string= "%PHP-MUL" (%php-call-name value))))))

(deftest php-parser-all-compound-assignment-operators-parse
  "Every PHP compound assignment operator on a KNOWN variable parses as a
read-modify-write form (a let whose body reads the old value and writes back).
$x is pre-declared so this exercises the read-modify-write path, not the
undefined-var introduce path."
  (dolist (op '("+=" "-=" "*=" "/=" ".=" "%=" "**=" "&=" "|=" "^=" "<<=" ">>=" "??="))
    ;; `$x = 1' lowers to (let ((x 1)) BODY); the compound form is BODY[0].
    (let* ((outer (%php-first (format nil "<?php $x = 1; $x ~A 2;" op)))
           (ast (first (cl-cc:ast-let-body outer))))
      (assert-true (cl-cc:ast-let-p ast))
      (assert-true (first (cl-cc:ast-let-body ast))))))

(deftest php-parser-unset-array-element-lowering
  "unset($a[0]) lowers to the ordered PHP array deletion helper."
  (let ((ast (%php-first "<?php unset($a[0]);")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%PHP-ARRAY-UNSET" (%php-call-name ast))
    (assert-= 2 (length (cl-cc:ast-call-args ast)))))

(deftest php-parser-unset-object-property-lowering
  "unset($o->x) lowers to a property write of PHP null."
  (let ((ast (%php-first "<?php unset($o->x);")))
    (assert-true (cl-cc:ast-set-slot-value-p ast))
    (assert-string= "X" (symbol-name (cl-cc:ast-set-slot-value-slot ast)))
    (assert-true (cl-cc:ast-quote-p (cl-cc:ast-set-slot-value-value ast)))))

(deftest php-parser-declare-block-keeps-body
  "declare(ticks=...) { ... } is a directive wrapper; the parser must keep the body."
  (let ((ast (%php-first "<?php declare(ticks=1) { echo 'a'; echo 'b'; }")))
    (assert-true (cl-cc:ast-progn-p ast))
    (assert-= 2 (length (cl-cc:ast-progn-forms ast)))
    (assert-true (every #'cl-cc:ast-call-p (cl-cc:ast-progn-forms ast)))))

(deftest php-parser-declare-alternative-keeps-body
  "declare(ticks=...): ... enddeclare; is a directive wrapper; the body remains ordered."
  (let ((ast (%php-first "<?php declare(ticks=1): echo 'a'; echo 'b'; enddeclare;")))
    (assert-true (cl-cc:ast-progn-p ast))
    (assert-= 2 (length (cl-cc:ast-progn-forms ast)))
    (assert-true (every #'cl-cc:ast-call-p (cl-cc:ast-progn-forms ast)))))

(deftest php-parser-close-tag-is-accepted
  "A closing ?> tag terminates PHP mode without becoming an expression token."
  (let ((asts (cl-cc/php:parse-php-source "<?php echo 1; ?>")))
    (assert-= 1 (length asts))
    (assert-true (cl-cc:ast-call-p (first asts)))
    (assert-string= "%PHP-OUTPUT-WRITE" (%php-call-name (first asts)))))

(deftest php-parser-inline-html-between-tags
  "Inline HTML after ?> lowers to verbatim, output-buffer-aware output before the
next PHP block."
  (let ((asts (cl-cc/php:parse-php-source "<?php echo 1; ?>hello<?php echo 2;")))
    (assert-= 3 (length asts))
    (assert-true (every (lambda (a)
                          (and (cl-cc:ast-call-p a)
                               (string= "%PHP-OUTPUT-WRITE" (%php-call-name a))))
                        asts))))

(deftest php-parser-namespace-use-metadata-preservation
  "namespace/use declarations annotate subsequent top-level AST nodes."
  (let ((asts (cl-cc/php:parse-php-source "<?php namespace App\\Lib; use Vendor\\Thing as Thing; function f() { return 1; }")))
    (assert-= 1 (length asts))
    (assert-string= "App\\Lib" (cl-cc:ast-namespace (first asts)))
    (assert-equal '((:type :class :name "Vendor\\Thing" :alias "Thing"))
                  (cl-cc:ast-imports (first asts)))))

(deftest php-parser-qualified-name-rejects-single-token-namespace-separators
  "Qualified names require explicit T-BACKSLASH separator tokens."
  (assert-signals error
    (cl-cc/php::php-parse-qualified-name
     (list (cl-cc/php::make-php-token :T-IDENT "Vendor\\Thing")))))

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

(defun %php-new-make-instance (value)
  "Extract the ast-make-instance from a `new C(...)' lowering. The lowering wraps
it in (let ((inst (make-instance C))) (if (has __construct) ...) inst), so the
make-instance is the first binding's value; falls back to VALUE itself."
  (if (cl-cc:ast-make-instance-p value)
      value
      (cdr (first (cl-cc:ast-let-bindings value)))))

(deftest php-parser-clone-lowers-to-runtime-helper
  "clone $obj lowers to a shallow-copy helper followed by an optional __clone call."
  (let* ((value (%php-first-binding-value "<?php $b = clone $a;"))
         (copy-call (cdr (first (cl-cc:ast-let-bindings value))))
         (body (cl-cc:ast-let-body value)))
    (assert-true (cl-cc:ast-let-p value))
    (assert-true (cl-cc:ast-call-p copy-call))
    (assert-eq 'cl-cc/php::%php-clone
               (cl-cc:ast-var-name (cl-cc:ast-call-func copy-call)))
    (assert-true (cl-cc:ast-if-p (first body)))
    (assert-true (cl-cc:ast-var-p (second body)))))

(deftest php-parser-use-alias-resolves-new-class-name
  "A class import alias resolves `new Alias()` to the imported fully-qualified class name."
  (let* ((mi (%php-new-make-instance
              (%php-first-binding-value
               "<?php namespace App\\Lib; use Vendor\\Thing as Thing; $x = new Thing();")))
         (class-ref (cl-cc:ast-make-instance-class mi)))
    (assert-true (cl-cc:ast-make-instance-p mi))
    (assert-true (cl-cc:ast-var-p class-ref))
    (assert-string= "VENDOR\\THING" (symbol-name (cl-cc:ast-var-name class-ref)))))

(deftest php-parser-default-use-alias-resolves-new-class-name
  "A class import without `as` uses the final namespace segment as its alias."
  (let* ((mi (%php-new-make-instance
              (%php-first-binding-value
               "<?php namespace App\\Lib; use Vendor\\Thing; $x = new Thing();")))
         (class-ref (cl-cc:ast-make-instance-class mi)))
    (assert-string= "VENDOR\\THING" (symbol-name (cl-cc:ast-var-name class-ref)))))

(deftest php-parser-fully-qualified-new-class-name-stays-global
  "A leading namespace separator on `new` names resolves from the PHP global namespace."
  (let* ((mi (%php-new-make-instance
              (%php-first-binding-value
               "<?php namespace App\\Lib; $x = new \\Vendor\\Thing();")))
         (class-ref (cl-cc:ast-make-instance-class mi)))
    (assert-string= "VENDOR\\THING" (symbol-name (cl-cc:ast-var-name class-ref)))))

(deftest php-parser-namespace-resolves-relative-class-declaration-and-ancestry
  "Namespaced class declarations and relative ancestry names are resolved consistently."
  (let* ((ast (%php-first "<?php namespace App\\Lib; class Box extends Base implements Iface {}"))
         (supers (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (assert-string= "APP\\LIB\\BOX" (symbol-name (cl-cc:ast-defclass-name ast)))
    (assert-equal '("APP\\LIB\\BASE" "APP\\LIB\\IFACE") supers)))

(deftest php-parser-class-ancestry-resolves-imports-and-absolute-names
  "Class ancestry resolves imported aliases and absolute names without namespace prefixing."
  (let* ((ast (%php-first "<?php namespace App\\Lib; use Vendor\\Base; class Box extends Base implements \\Contracts\\Iface {}"))
         (supers (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (assert-equal '("VENDOR\\BASE" "CONTRACTS\\IFACE") supers)))

(deftest php-parser-function-import-alias-resolves-call-name
  "Function imports resolve unqualified and aliased function call names.
After php-finish-let-bindings, $x let wraps $y let in its body."
  (let* ((asts (cl-cc/php:parse-php-source
                "<?php namespace App\\Lib; use function Vendor\\Fns\\{foo, bar as baz}; $x = foo(); $y = baz();"))
         (let-x       (first asts))
         (let-y       (first (cl-cc:ast-let-body let-x)))
         (first-call  (cdr (first (cl-cc:ast-let-bindings let-x))))
         (second-call (cdr (first (cl-cc:ast-let-bindings let-y)))))
    (assert-string= "VENDOR\\FNS\\FOO"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func first-call))))
    (assert-string= "VENDOR\\FNS\\BAR"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func second-call))))))

(deftest php-parser-function-import-overrides-builtin-name
  "A function import named like a PHP builtin must not lower to the builtin helper."
  (let* ((call (%php-first-binding-value
                "<?php namespace App\\Lib; use function Vendor\\Fns\\count; $x = count($items);")))
    (assert-string= "VENDOR\\FNS\\COUNT"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))))))

(deftest php-parser-function-import-alias-overrides-builtin-name
  "A function import alias named like a PHP builtin must keep the imported target."
  (let* ((call (%php-first-binding-value
                "<?php namespace App\\Lib; use function Vendor\\Fns\\strlen as count; $x = count($items);")))
    (assert-string= "VENDOR\\FNS\\STRLEN"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))))))

(deftest php-parser-unqualified-function-call-keeps-global-fallback-name
  "Unimported unqualified function calls in a namespace keep their fallback-safe bare name."
  (let* ((call (%php-first-binding-value "<?php namespace App\\Lib; $x = helper();")))
    (assert-string= "HELPER"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))))))

(deftest php-parser-qualified-function-call-resolves-relative-to-namespace
  "Qualified relative function calls are namespace-relative in PHP."
  (let* ((call (%php-first-binding-value "<?php namespace App\\Lib; $x = Tools\\helper();")))
    (assert-string= "APP\\LIB\\TOOLS\\HELPER"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))))))

(deftest php-parser-fully-qualified-function-call-stays-global
  "A leading namespace separator on function calls resolves from the PHP global namespace."
  (let* ((call (%php-first-binding-value "<?php namespace App\\Lib; $x = \\Vendor\\Fns\\foo();")))
    (assert-string= "VENDOR\\FNS\\FOO"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))))))

(deftest php-parser-unqualified-constant-keeps-global-fallback-name
  "Unimported unqualified constants in a namespace keep their fallback-safe bare name."
  (let ((value (%php-first-binding-value "<?php namespace App\\Lib; $x = SOME_CONST;")))
    (assert-true (cl-cc:ast-var-p value))
    (assert-string= "SOME_CONST" (symbol-name (cl-cc:ast-var-name value)))))

(deftest php-parser-qualified-constant-resolves-relative-to-namespace
  "Qualified relative constants are namespace-relative in PHP."
  (let ((value (%php-first-binding-value "<?php namespace App\\Lib; $x = Config\\VALUE;")))
    (assert-true (cl-cc:ast-var-p value))
    (assert-string= "APP\\LIB\\CONFIG\\VALUE" (symbol-name (cl-cc:ast-var-name value)))))

(deftest php-parser-qualified-catch-types-resolve-imports-and-absolute-names
  "Catch union types resolve imported aliases and fully-qualified names."
  (let* ((ast (%php-first "<?php namespace App\\Lib; use Vendor\\Ex; try { throw new Ex(); } catch (Ex | \\Other\\Alt $e) { echo $e; }"))
         (inner (cl-cc:ast-unwind-protected ast))
         (top-dispatch (first (cl-cc:ast-let-body inner)))
         (catch-dispatch (cl-cc:ast-if-then top-dispatch))
         (match-cond (cl-cc:ast-if-cond catch-dispatch))
         (class-arg (second (cl-cc:ast-call-args match-cond))))
    (assert-equal '("VENDOR\\EX" "OTHER\\ALT")
                  (mapcar #'symbol-name (cl-cc:ast-quote-value class-arg)))))

(deftest php-parser-function-type-annotation-preservation
  "Characterization: function parameter and return type annotations should be preserved as declarations."
  (let ((ast (%php-first "<?php function add(int $a, ?string $b = null, int|string $c): bool { return true; }")))
    (assert-true (cl-cc:ast-defun-p ast))
    (let ((decls (cl-cc:ast-defun-declarations ast)))
      (assert-equal "bool" (getf decls :php-return-type))
      (assert-equal '(("a" . "int") ("b" . "?string") ("c" . "int|string"))
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
                  ("Countable&Iterator" . "countable&iterator")
                  ("(Countable&Iterator)|Traversable" . "(countable&iterator)|traversable")
                  ("Countable|(Iterator&Traversable)" . "countable|(iterator&traversable)")))
    (let* ((source (format nil "<?php function f(): ~A { return 1; }" (car case)))
           (ast (%php-first source)))
      (assert-equal (cdr case)
                    (getf (cl-cc:ast-defun-declarations ast) :php-return-type)))))

(deftest php-parser-function-return-by-reference-metadata
  "PHP function &name(...) parses and records the return-by-reference marker."
  (let* ((ast (%php-first "<?php function &current_item(): mixed { return $item; }"))
         (decls (cl-cc:ast-defun-declarations ast)))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-equal "mixed" (getf decls :php-return-type))
    (assert-true (getf decls :php-returns-by-ref))))

(deftest php-parser-anonymous-function-return-by-reference-metadata
  "PHP function &() closure syntax parses and records the return-by-reference marker."
  (let* ((value (%php-first-binding-value "<?php $f = function &() { return $item; };"))
         (lambda (if (cl-cc:ast-let-p value)
                     (first (cl-cc:ast-let-body value))
                     value))
         (decls (cl-cc:ast-lambda-declarations lambda)))
    (assert-true (cl-cc:ast-lambda-p lambda))
    (assert-true (member '(:php-returns-by-ref t) decls :test #'equal))))

(deftest php-parser-dnf-type-annotation-preservation
  "PHP 8.2 DNF type annotations are preserved on callables and class members."
  (let* ((fn (%php-first "<?php function f((A&B)|C $x): D|(E&F) { return $x; }"))
         (class (%php-first "<?php class Box { public (A&B)|C $value; const D|(E&F) KIND = 1; }"))
         (decls (cl-cc:ast-defun-declarations fn))
         (slots (cl-cc:ast-defclass-slots class)))
    (assert-equal '(("x" . "(a&b)|c"))
                  (mapcar (lambda (entry)
                            (cons (symbol-name (car entry)) (cdr entry)))
                          (getf decls :php-param-types)))
    (assert-equal "d|(e&f)" (getf decls :php-return-type))
    (assert-equal '("(a&b)|c" "d|(e&f)") (mapcar #'cl-cc:ast-slot-type slots))))

(deftest php-parser-reference-parameter-and-foreach-are-supported
  "By-reference function parameters and foreach values parse as supported forms."
  (dolist (src '("<?php function f(&$x) { return $x; }"
                 "<?php foreach ($items as &$item) { echo $item; }"))
    (let ((asts (cl-cc/php:parse-php-source src)))
      (assert-true (cl-cc/php:php-check-supported-forms asts)))))

(deftest php-parser-trait-is-supported-by-check
  "Traits parse as trait class-like declarations and pass support checks."
  (let ((ast (%php-first "<?php trait T { public $x; }")))
    (assert-eq :trait (cl-cc:ast-defclass-php-kind ast))
    (assert-true (cl-cc/php:php-check-supported-forms (list ast)))))

(deftest php-parser-interface-is-supported-by-check
  "Interfaces parse as interface class-like declarations and pass support checks."
  (let ((ast (%php-first "<?php interface I {}")))
    (assert-eq :interface (cl-cc:ast-defclass-php-kind ast))
    (assert-true (cl-cc/php:php-check-supported-forms (list ast)))))

(deftest php-parser-unit-enum-cases
  "Unit enums parse as PHP enum defclasses with singleton case class slots."
  (let* ((form (%php-first "<?php enum Suit { case Hearts; case Diamonds; }"))
         ;; An enum now lowers to (progn defclass (link-cases)); unwrap the defclass.
         (ast (if (cl-cc:ast-progn-p form) (first (cl-cc:ast-progn-forms form)) form))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-eq :enum (cl-cc:ast-defclass-php-kind ast))
    (assert-null (cl-cc:ast-defclass-php-enum-type ast))
    (assert-equal '("HEARTS" "DIAMONDS")
                  (mapcar (lambda (slot) (symbol-name (cl-cc:ast-slot-name slot))) slots))
    (assert-true (every (lambda (slot)
                          (and (eq :class (cl-cc:ast-slot-allocation slot))
                               (getf (cl-cc:ast-imports slot) :php-enum-case)))
                        slots))
    (assert-equal '("HEARTS" "DIAMONDS")
                  (mapcar (lambda (case) (symbol-name (getf case :name)))
                          (cl-cc:ast-defclass-php-enum-cases ast)))
    (cl-cc/php:php-check-supported-forms (list ast))))

(deftest php-parser-backed-enum-cases
  "Backed enums preserve int/string backing metadata and per-case values."
  (let* ((form (%php-first "<?php enum Status: int { case Draft = 0; case Published = 1; }"))
         (ast (if (cl-cc:ast-progn-p form) (first (cl-cc:ast-progn-forms form)) form))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-eq :enum (cl-cc:ast-defclass-php-kind ast))
    (assert-eq :int (cl-cc:ast-defclass-php-enum-type ast))
    (assert-= 2 (length slots))
    (assert-true (every (lambda (slot)
                          (cl-cc:ast-call-p (cl-cc:ast-slot-initform slot)))
                        slots))))

(deftest php-parser-enum-implements-methods-traits-and-constants
  "Enum bodies accept implements, methods, trait uses, and constants."
  (let* ((form (%php-first "<?php enum Status implements JsonSerializable { use HasLabels; const FOO = 'x'; public function label() { return 'ok'; } case Draft = 0; }"))
         (ast (if (cl-cc:ast-progn-p form) (first (cl-cc:ast-progn-forms form)) form))
         (slot-names (mapcar (lambda (slot) (symbol-name (cl-cc:ast-slot-name slot)))
                             (cl-cc:ast-defclass-slots ast))))
    (assert-true (member "JSONSERIALIZABLE"
                         (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))
                         :test #'string=))
    (assert-true (member "FOO" slot-names :test #'string=))
    (assert-true (member "LABEL" slot-names :test #'string=))
    (assert-true (member "DRAFT" slot-names :test #'string=))))

(deftest php-parser-enum-static-builtins
  "Enum static built-ins lower to runtime helper calls.
Enum defclass is first; $x/$y/$z assignments nest (php-finish-let-bindings)."
  (let* ((asts (cl-cc/php:parse-php-source "<?php enum Status: int { case Draft = 0; case Published = 1; } $x = Status::from(1); $y = Status::tryFrom(99); $z = Status::cases();"))
         ;; enum defclass is first; let-x is second (wraps y and z in its body chain)
         (let-x  (second asts))
         (let-y  (first (cl-cc:ast-let-body let-x)))
         (let-z  (first (cl-cc:ast-let-body let-y)))
         (from-call     (cdr (first (cl-cc:ast-let-bindings let-x))))
         (try-from-call (cdr (first (cl-cc:ast-let-bindings let-y))))
         (cases-call    (cdr (first (cl-cc:ast-let-bindings let-z)))))
    (assert-string= "%PHP-ENUM-FROM" (%php-call-name from-call))
    (assert-string= "%PHP-ENUM-TRY-FROM" (%php-call-name try-from-call))
    (assert-string= "%PHP-ENUM-CASES" (%php-call-name cases-call))))

(deftest-each php-parser-reference-syntax-is-supported
  "By-reference parameter, foreach value, and closure capture syntax parse and pass support checks."
  :cases (("closure-use-ref"    "<?php $fn = function() use (&$x) { return $x; };")
          ("function-ref-param" "<?php function f(&$x) { return $x; }")
          ("foreach-ref-value"  "<?php foreach ($items as &$item) { echo $item; }"))
  (src)
  (let ((asts (cl-cc/php:parse-php-source src)))
    (assert-true (cl-cc/php:php-check-supported-forms asts))))

(deftest php-parser-class-typed-properties
  "Characterization: class typed properties should preserve their declared PHP types on slot definitions."
  (let* ((ast (%php-first "<?php class User { public int $id; private ?string $name; readonly public int|float $score; }"))
          (slots (cl-cc:ast-defclass-slots ast)))
    (assert-= 3 (length slots))
    (assert-equal '("int" "?string" "int|float") (mapcar #'cl-cc:ast-slot-type slots))
    (assert-true (member :readonly (getf (cl-cc:ast-imports (third slots)) :php-modifiers)))))

(deftest php-parser-readonly-class-marks-instance-properties
  "PHP 8.2 readonly class declarations mark instance properties, not static members or methods."
  (let* ((ast (%php-first "<?php readonly class User { public int $id; public static int $count; public function name() { return 1; } }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (labels ((slot (name)
               (find name slots
                     :key (lambda (slot)
                            (symbol-name (cl-cc:ast-slot-name slot)))
                     :test #'string=)))
      (let ((id (slot "ID"))
            (count (slot "COUNT"))
            (name (slot "NAME")))
        (assert-true (getf (cl-cc:ast-imports id) :readonly-p))
        (assert-true (member :readonly (getf (cl-cc:ast-imports id) :php-modifiers)))
        (assert-false (getf (cl-cc:ast-imports count) :readonly-p))
        (assert-false (getf (cl-cc:ast-imports name) :readonly-p))))))

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

(deftest php-parser-multiple-class-constants-in-one-declaration
  "PHP class const declarations may contain several constants sharing one optional type."
  (let* ((ast (%php-first "<?php class C { public const int FOO = 1, BAR = 2; }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-= 2 (length slots))
    (assert-equal '("FOO" "BAR")
                  (mapcar (lambda (slot)
                            (symbol-name (cl-cc:ast-slot-name slot)))
                          slots))
    (assert-equal '("int" "int") (mapcar #'cl-cc:ast-slot-type slots))
    (assert-true (every (lambda (slot)
                          (and (eq :class (cl-cc:ast-slot-allocation slot))
                               (member :public (getf (cl-cc:ast-imports slot) :php-modifiers))
                               (getf (cl-cc:ast-imports slot) :php-class-constant)))
                        slots))))

(defun %php-node-attributes (node)
  "Return PHP attribute metadata attached to NODE."
  (getf (cl-cc:ast-imports node) :php-attributes))

(deftest php-parser-attribute-class-metadata
  "#[Attr] class Foo {} attaches attribute metadata to the class AST node."
  (let* ((ast (%php-first "<?php #[Attr] class Foo {}"))
         (attrs (%php-node-attributes ast)))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-= 1 (length attrs))
    (assert-string= "Attr" (cl-cc/php:php-attribute-name (first attrs)))
    (assert-eq :class (cl-cc/php:php-attribute-target-type (first attrs)))))

(deftest php-parser-attribute-function-string-arg
  "#[Attr('value')] function foo() {} parses and preserves attribute arguments."
  (let* ((ast (%php-first "<?php #[Attr('value')] function foo() { return 1; }"))
         (attr (first (%php-node-attributes ast)))
         (arg (first (cl-cc/php:php-attribute-args attr))))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-string= "Attr" (cl-cc/php:php-attribute-name attr))
    (assert-true (cl-cc:ast-quote-p arg))
    (assert-string= "value" (cl-cc:ast-quote-value arg))))

(deftest php-parser-multiple-attributes-class
  "#[Attr1, Attr2] class Bar {} attaches both attributes in order."
  (let* ((ast (%php-first "<?php #[Attr1, Attr2] class Bar {}"))
         (attrs (%php-node-attributes ast)))
    (assert-equal '("Attr1" "Attr2")
                  (mapcar #'cl-cc/php:php-attribute-name attrs))))

(deftest php-parser-attribute-named-arguments
  "#[Attr(42, name: 'val')] parses positional and named attribute arguments."
  (let* ((ast (%php-first "<?php #[Attr(42, name: 'val')] function bar() { return 1; }"))
         (attr (first (%php-node-attributes ast)))
         (args (cl-cc/php:php-attribute-args attr)))
    (assert-= 2 (length args))
    (assert-true (cl-cc:ast-int-p (first args)))
    (assert-equal "name" (getf (second args) :name))
    (assert-true (cl-cc:ast-quote-p (getf (second args) :value)))
    (assert-string= "val" (cl-cc:ast-quote-value (getf (second args) :value)))))

(deftest php-parser-hash-comment-still-skips
  "A standalone # still starts a PHP line comment, including before declarations."
  (let ((ast (%php-first "<?php # this is a comment
function commented() { return 1; }")))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-string= "COMMENTED" (symbol-name (cl-cc:ast-defun-name ast)))))

(deftest php-parser-constructor-promotion
  "PHP 8.0 constructor property promotion: visibility/readonly modifiers may
precede a parameter's type in __construct."
  (let ((ast (%php-first
              "<?php class P { public function __construct(public int $x, private string $y) {} }")))
    (assert-true (cl-cc:ast-defclass-p ast))))

(deftest php-parser-constructor-promotion-readonly
  "Promoted constructor params accept readonly + nullable + default."
  (let ((ast (%php-first
              "<?php class P { public function __construct(int $a, public readonly ?string $b = null) {} }")))
    (assert-true (cl-cc:ast-defclass-p ast))))

(deftest-each php-parser-call-syntax-variants
  "Modern PHP call syntax variants parse without error."
  ;; foo(...$args) lowers to an apply over a runtime-spread argument list, so it
  ;; is an ast-apply (not an ast-call). The named-arg variants stay ast-call.
  :cases (("spread-arg"    "<?php foo(...$args);"                #'cl-cc:ast-apply-p)
          ("named-args"    "<?php foo(name: 'x', age: 5);"      #'cl-cc:ast-call-p)
          ("named-mixed"   "<?php foo('pos', name: 'x');"       #'cl-cc:ast-call-p))
  (src pred)
  (assert-true (funcall pred (%php-first src))))

(deftest php-parser-named-args-after-dynamic-spread
  "Named arguments after dynamic spread lower to apply instead of an unsupported
parser error."
  (let* ((asts (cl-cc/php:parse-php-source
                "<?php function f($a,$b,$c) { return $c; } f(...$args, c: 3);"))
         (call (second asts)))
    (assert-true (cl-cc:ast-apply-p call))))

(deftest php-parser-named-argument-metadata-is-source-local
  "Named-argument parameter metadata from one parse must not affect later sources."
  (cl-cc/php:parse-php-source
   "<?php function foo($value) { return $value; } echo foo(value: 'x');")
  (let ((ast (%php-first "<?php foo(name: 'x');")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%PHP-NAMED-ARG"
                    (%php-call-name (first (cl-cc:ast-call-args ast))))))

(deftest php-parser-first-class-callable
  "strlen(...) parses as a first-class callable reference."
  (let ((ast (%php-first "<?php $f = strlen(...);")))
    ;; assignment lowers to ast-let/ast-setq with a call value; just ensure it parsed.
    (assert-true ast)))

(deftest-each php-parser-array-spread-syntax
  "Array spread syntax parses without error."
  :cases (("spread-only"  "<?php $a = [...$b, ...$c];")
          ("spread-mixed" "<?php $a = [1, ...$b, 2];"))
  (src)
  (assert-true (%php-first src)))

(deftest php-parser-anonymous-class
  "new class { ... } parses to a progn defining and instantiating an anon class."
  (let ((ast (%php-first "<?php $o = new class { public $x = 1; };")))
    (assert-true ast)))

(deftest php-parser-anonymous-class-extends-ctor
  "new class(5) extends Base { __construct(public int $n) {} } parses."
  (let ((ast (%php-first
              "<?php $o = new class(5) extends Base { public function __construct(public int $n) {} };")))
    (assert-true ast)))

(deftest-each php-parser-list-destructuring
  "[$a, $b, ...] = $arr lowers to ast-let with the correct binding count."
  :cases (("two-targets"   "<?php [$a, $b] = $arr;"        2)
          ("three-targets" "<?php [$x, $y, $z] = $data;"   3))
  (src expected-bindings)
  (let ((ast (%php-first src)))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-= expected-bindings (length (cl-cc:ast-let-bindings ast)))))
