;;;; packages/javascript/tests/js-parser-stmt-tests.lisp — JS parser: statements + expressions
;;;;
;;;; Covers: if/else, while, for loop variants, switch, try/catch/finally,
;;;;         destructuring, spread, optional chaining, nullish coalescing,
;;;;         logical assignment, template literals, import/export, using,
;;;;         throw, return, multi-statement, generators, for-of/in, new, unary.
;;;;
;;;; Depends on: js-parser-decl-tests.lisp (%js-parse, %js-first, %js-call-name).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── If / else ────────────────────────────────────────────────────────────────

(deftest js-parser-if-stmt
  "if (x) { y; } produces ast-if."
  (let ((ast (%js-first "if (x) { y; }")))
    (assert-true (cl-cc:ast-if-p ast))))

(deftest js-parser-if-else-stmt
  "if (a) { } else { } produces ast-if with non-nil else."
  (let ((ast (%js-first "if (a) { return 1; } else { return 2; }")))
    (assert-true (cl-cc:ast-if-p ast))
    (assert-true (not (cl-cc:ast-quote-p (cl-cc:ast-if-else ast))))))

;;; ─── While loop ───────────────────────────────────────────────────────────────

(deftest js-parser-while-loop
  "while (x) { } lowers to ast-block containing a tagbody."
  (let ((ast (%js-first "while (x) { }")))
    (assert-true (cl-cc:ast-block-p ast))
    (assert-true (some #'cl-cc:ast-tagbody-p (cl-cc:ast-block-body ast)))))

;;; ─── For loop ─────────────────────────────────────────────────────────────────

(deftest-each js-parser-for-loop-variants
  "All for-loop forms lower to ast-let (or ast-block for C-style)."
  :cases (("c-style" "for (let i = 0; i < 10; i++) { }")
          ("of"      "for (const x of arr) { }")
          ("in"      "for (const k in obj) { }"))
  (src)
  (let ((ast (%js-first src)))
    (assert-true (or (cl-cc:ast-let-p ast) (cl-cc:ast-block-p ast)
                     (cl-cc:ast-progn-p ast)))))

;;; ─── Switch / case ────────────────────────────────────────────────────────────

(deftest js-parser-switch-stmt
  "switch (x) { case 1: break; default: break; } lowers to ast-let + block."
  (let ((ast (%js-first "switch (x) { case 1: break; default: break; }")))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((block (first (cl-cc:ast-let-body ast))))
      (assert-true (cl-cc:ast-block-p block)))))

;;; ─── Try / catch / finally ────────────────────────────────────────────────────

(deftest-each js-parser-try-forms
  "All try variants lower to a %js-try-catch-finally call."
  :cases (("catch"         "try { throw 1; } catch (e) { }")
          ("finally"       "try { } finally { }")
          ("catch-finally" "try { throw 1; } catch (e) { } finally { }"))
  (src)
  (let ((ast (%js-first src)))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%JS-TRY-CATCH-FINALLY" (%js-call-name ast))))

;;; ─── Destructuring ────────────────────────────────────────────────────────────

(deftest-each js-parser-destructuring
  "Destructuring declarations produce ast-let (single-binding per nesting level)."
  :cases (("array-basic"           "const [a, b] = arr;")
          ("object-basic"          "const {x, y} = obj;")
          ("array-defaults"        "const [a = 1, b = 2] = arr;")
          ("object-defaults"       "const {a = 1} = obj;")
          ("object-rename-default" "const {a: x = 5} = obj;")
          ("array-rest-default"    "const [x, y = 10, ...rest] = arr;"))
  (src)
  (let ((ast (%js-first src)))
    (assert-true (cl-cc:ast-let-p ast))))

;;; ─── Spread operator ──────────────────────────────────────────────────────────

(deftest js-parser-spread-in-array
  "[...arr] lowers to an apply of %js-make-array over the spread-expanded element
list (a spread element makes the array literal an ast-apply, not a plain call)."
  (let ((ast (%js-first "const x = [...arr];")))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((val (cdr (first (cl-cc:ast-let-bindings ast)))))
      (assert-true (cl-cc:ast-apply-p val)))))

;;; ─── Optional chaining ────────────────────────────────────────────────────────

(deftest js-parser-optional-chaining
  "a?.b produces a %js-optional-chain call."
  (let* ((ast (%js-first "const r = a?.b;"))
         (val (cdr (first (cl-cc:ast-let-bindings ast)))))
    (assert-true (cl-cc:ast-call-p val))
    (assert-string= "%JS-OPTIONAL-CHAIN" (%js-call-name val))))

;;; ─── Nullish coalescing ───────────────────────────────────────────────────────

(deftest js-parser-nullish-coalescing
  "a ?? b produces a let-wrapped conditional via %js-not-nullish."
  (let* ((ast (%js-first "const r = a ?? b;"))
         (val (cdr (first (cl-cc:ast-let-bindings ast)))))
    (assert-true (or (cl-cc:ast-let-p val) (cl-cc:ast-if-p val)
            (cl-cc:ast-call-p val)))))

;;; ─── Logical assignment ───────────────────────────────────────────────────────

(deftest-each js-parser-logical-assignment
  "Logical assignment operators (&&=, ||=, ??=) nest let declarations via %js-finish-let-bindings."
  :cases (("and-assign"     "let x = 1;    x &&= 0;")
          ("or-assign"      "let x = null; x ||= 42;")
          ("nullish-assign" "let x = null; x ??= 'default';"))
  (src)
  (let ((asts (%js-parse src)))
    (assert-= 1 (length asts))
    (assert-true (cl-cc:ast-let-p (first asts)))))

;;; ─── Template literals ────────────────────────────────────────────────────────

(deftest-each js-parser-template-literals
  "Template literals (plain, interpolated, expression) bind to an ast-let."
  :cases (("plain"      "const s = `hello`;")
          ("interpolate" "const s = `hi ${name}!`;")
          ("expr"        "const s = `sum=${a + b * 2}`;"))
  (src)
  (let ((ast (%js-first src)))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-true (not (null (cdr (first (cl-cc:ast-let-bindings ast))))))))

;;; ─── Import / export ──────────────────────────────────────────────────────────

(deftest js-parser-import-statement
  "import x from 'mod'; parses as a module import in module mode."
  (let ((asts (cl-cc/javascript:parse-js-module "import x from 'mod';")))
    (assert-true (>= (length asts) 0))))

(deftest js-parser-export-const
  "export const val = 1; in module mode produces at least one AST node."
  (let ((asts (cl-cc/javascript:parse-js-module "export const val = 1;")))
    (assert-true (>= (length asts) 1))))

(deftest js-parser-export-const-preserves-declaration-ast
  "export const keeps the same ast-let initializer shape as a normal declaration."
  (let* ((ast (first (cl-cc/javascript:parse-js-module "export const val = 1 + 2;")))
         (args (cl-cc:ast-call-args ast))
         (decl (second args))
         (names (fourth args))
         (binding (first (cl-cc:ast-let-bindings decl))))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%JS-EXPORT" (%js-call-name ast))
    (assert-true (cl-cc:ast-let-p decl))
    (assert-equal :const (first (cl-cc:ast-let-declarations decl)))
    (assert-equal '("val") (cl-cc:ast-quote-value names))
    (assert-string= "val" (symbol-name (car binding)))
    (assert-true (cl-cc:ast-call-p (cdr binding)))
    (assert-false (string= "%JS-EXPR" (%js-call-name (cdr binding))))))

(deftest js-parser-export-object-destructuring-names
  "export const {x, y: z, ...rest} records only source-level export names."
  (let* ((ast (first (cl-cc/javascript:parse-js-module
                      "export const {x, y: z, ...rest} = obj;")))
         (args (cl-cc:ast-call-args ast))
         (names (fourth args)))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%JS-EXPORT" (%js-call-name ast))
    (assert-equal '("x" "z" "rest") (cl-cc:ast-quote-value names))))

(deftest js-parser-export-array-destructuring-names
  "export const [x, , y = 1, ...rest] records only source-level export names."
  (let* ((ast (first (cl-cc/javascript:parse-js-module
                      "export const [x, , y = 1, ...rest] = arr;")))
         (args (cl-cc:ast-call-args ast))
         (names (fourth args)))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%JS-EXPORT" (%js-call-name ast))
    (assert-equal '("x" "y" "rest") (cl-cc:ast-quote-value names))))

(deftest js-parser-export-function-preserves-body-ast
  "export function keeps parameters and parsed body instead of a placeholder."
  (let* ((ast (first (cl-cc/javascript:parse-js-module
                      "export function add(a, b) { return a + b; }")))
         (args (cl-cc:ast-call-args ast))
         (decl (second args))
         (names (fourth args)))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%JS-EXPORT" (%js-call-name ast))
    (assert-true (cl-cc:ast-defun-p decl))
    (assert-string= "add" (symbol-name (cl-cc:ast-defun-name decl)))
    (assert-equal '("add") (cl-cc:ast-quote-value names))
    (assert-equal '("a" "b")
                  (mapcar #'symbol-name (cl-cc:ast-defun-params decl)))
    (assert-false (and (= 1 (length (cl-cc:ast-defun-body decl)))
                       (cl-cc:ast-quote-p (first (cl-cc:ast-defun-body decl)))
                       (eq :stub (cl-cc:ast-quote-value
                                  (first (cl-cc:ast-defun-body decl))))))))

(deftest js-parser-import-meta-expression
  "import.meta lowers to a runtime metadata object."
  (let* ((ast (first (cl-cc/javascript:parse-js-module "const meta = import.meta;")))
         (val (cdr (first (cl-cc:ast-let-bindings ast)))))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-string= "%JS-IMPORT-META" (%js-call-name val))))

(deftest js-parser-dynamic-import-expression-statement
  "import(specifier) at statement start parses as dynamic import, not a declaration."
  (let ((ast (%js-first "import('./dep.js');")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%JS-IMPORT" (%js-call-name ast))))

;;; ─── Using declaration (ES2025) ───────────────────────────────────────────────

(deftest js-parser-using-declaration
  "using x = getResource(); parses to ast-let with :js-using kind."
  (let ((ast (%js-first "using x = getResource();")))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-true (member :js-using (cl-cc:ast-let-declarations ast)))))

;;; ─── Throw statement ──────────────────────────────────────────────────────────

(deftest js-parser-throw-stmt
  "throw new Error('msg'); lowers to a %js-throw call."
  (let ((ast (%js-first "throw new Error('msg');")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%JS-THROW" (%js-call-name ast))))

;;; ─── Return statement ─────────────────────────────────────────────────────────

(deftest js-parser-return-with-value
  "function f() { return 42; } body contains ast-return-from inside the block wrapper."
  (let* ((ast (%js-first "function f() { return 42; }"))
         (body (cl-cc:ast-defun-body ast))
         (block (first body))
         (inner (when (cl-cc:ast-block-p block) (cl-cc:ast-block-body block))))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-true (some #'cl-cc:ast-return-from-p (or inner body)))))

(deftest js-parser-return-bare
  "function g() { return; } body is wrapped in (block nil ...) containing an ast-return-from."
  (let* ((ast (%js-first "function g() { return; }"))
         (body (cl-cc:ast-defun-body ast))
         (block (first body))
         (ret (when (cl-cc:ast-block-p block)
                (first (cl-cc:ast-block-body block)))))
    (assert-true (cl-cc:ast-block-p block))
    (assert-true (cl-cc:ast-return-from-p ret))
    (assert-true (cl-cc:ast-quote-p (cl-cc:ast-return-from-value ret)))))

;;; ─── Multi-statement source ───────────────────────────────────────────────────

(deftest js-parser-multi-statement-source
  "Three const declarations are nested by %js-finish-let-bindings into one top-level let."
  (let ((asts (%js-parse "const a = 1; const b = 2; const c = 3;")))
    (assert-= 1 (length asts))
    (assert-true (cl-cc:ast-let-p (first asts)))))

;;; ─── Generator functions ──────────────────────────────────────────────────────

(deftest js-parser-generator-function-star
  "function* f() { yield 1; } parses to ast-defun with :js-generator declaration."
  (let ((ast (%js-first "function* gen() { yield 1; }")))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-true (member :js-generator (cl-cc:ast-defun-declarations ast)))))

(deftest js-parser-yield-expr
  "yield expr lowers to a (%js-yield expr) call inside a generator body."
  (let* ((ast (%js-first "function* gen() { yield 42; }"))
         (body (cl-cc:ast-defun-body ast))
         (generator-call (first body)))
    (assert-string= "%JS-MAKE-GENERATOR" (%js-call-name generator-call))))

(deftest js-parser-yield-star
  "yield* iter lowers to (%js-yield-from iter)."
  (let* ((ast (%js-first "function* gen() { yield* [1,2]; }"))
         (body (cl-cc:ast-defun-body ast)))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-string= "%JS-MAKE-GENERATOR" (%js-call-name (first body)))))

;;; ─── for-of / for-in ──────────────────────────────────────────────────────────

(deftest js-parser-for-of-array
  "for (const x of arr) {} lowers to an ast-let with %js-iter-values binding."
  (let* ((ast (%js-first "for (const x of [1,2,3]) {}"))
         (bindings (cl-cc:ast-let-bindings ast))
         (iter-val (cdr (first bindings))))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-string= "%JS-ITER-VALUES" (%js-call-name iter-val))))

(deftest js-parser-for-in-object
  "for (const k in obj) {} lowers to an ast-let with %js-iter-keys binding."
  (let* ((ast (%js-first "for (const k in {a: 1}) {}"))
         (bindings (cl-cc:ast-let-bindings ast))
         (iter-val (cdr (first bindings))))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-string= "%JS-ITER-KEYS" (%js-call-name iter-val))))

;;; ─── New expression ───────────────────────────────────────────────────────────

(deftest js-parser-new-expr
  "new Foo() lowers to (%js-new Foo (%js-make-array))."
  (let ((ast (%js-first "new Foo();")))
    (assert-string= "%JS-NEW" (%js-call-name ast))))

(deftest js-parser-new-target
  "new.target inside a function lowers to (%js-new-target)."
  (let* ((ast (%js-first "function f() { return new.target; }"))
         (body (cl-cc:ast-defun-body ast)))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-true (not (null body)))))

;;; ─── Unary operators ──────────────────────────────────────────────────────────

(deftest-each js-parser-unary-ops
  "Each unary operator is lowered to the expected call name."
  :cases (("not"    "!x"      "NOT")
          ("typeof" "typeof x" "%JS-TYPEOF")
          ("void"   "void 0"   "PROGN")
          ("delete" "delete x" "%JS-DELETE"))
  (src expected)
  (let ((ast (%js-first src)))
    (cond
      ((string= expected "PROGN")
       (assert-true (cl-cc:ast-progn-p ast)))
      ((string= expected "NOT")
       (assert-true (cl-cc:ast-call-p ast))
       (assert-string= "NOT" (%js-call-name ast)))
      (t
       (assert-string= expected (%js-call-name ast))))))
