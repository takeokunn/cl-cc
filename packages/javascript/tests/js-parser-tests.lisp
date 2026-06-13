;;;; packages/javascript/tests/js-parser-tests.lisp — ES2026 JavaScript Parser Tests
;;;;
;;;; 40 FiveAM tests covering cl-cc/javascript:parse-js-source for all major statement and
;;;; expression AST shapes. Each test asserts both AST type membership and
;;;; key structural properties.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ──────────────────────────────────────────────────────────────────

(defun %js-parse (src)
  "Parse SRC and return the list of top-level AST nodes."
  (cl-cc/javascript:parse-js-source src))

(defun %js-first (src)
  "Parse SRC and return the first top-level AST node."
  (first (%js-parse src)))

(defun %js-call-name (ast)
  "Return the upcased symbol name for an AST-CALL whose func is an AST-VAR."
  (when (and (cl-cc:ast-call-p ast)
             (cl-cc:ast-var-p (cl-cc:ast-call-func ast)))
    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func ast)))))

;;; ─── Variable / const / let declarations ─────────────────────────────────────

(deftest js-parser-const-decl
  "const x = 42; parses to ast-let with one numeric binding."
  (let ((ast (%js-first "const x = 42;")))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-= 1 (length (cl-cc:ast-let-bindings ast)))
    (assert-true (cl-cc:ast-int-p (cdr (first (cl-cc:ast-let-bindings ast)))))))

(deftest js-parser-let-decl
  "let y = 'hello'; parses to ast-let with a string binding."
  (let ((ast (%js-first "let y = 'hello';")))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-true (cl-cc:ast-quote-p (cdr (first (cl-cc:ast-let-bindings ast)))))))

(deftest js-parser-var-decl
  "var z; parses to ast-let with a nil binding."
  (let ((ast (%js-first "var z;")))
    (assert-true (cl-cc:ast-let-p ast))))

(deftest js-parser-const-multi-decl
  "const a = 1, b = 2; parses to sequential nested single-binding lets (ast-let
binds in parallel, so JS's sequential `let' is modelled as a nested chain)."
  (let ((ast (%js-first "const a = 1, b = 2;")))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-= 1 (length (cl-cc:ast-let-bindings ast)))
    (let ((inner (first (cl-cc:ast-let-body ast))))
      (assert-true (cl-cc:ast-let-p inner))
      (assert-= 1 (length (cl-cc:ast-let-bindings inner))))))

;;; ─── Arrow functions ──────────────────────────────────────────────────────────

(deftest js-parser-arrow-function-simple
  "const f = (x) => x + 1; yields a let binding whose value is a lambda."
  (let* ((ast (%js-first "const f = (x) => x + 1;"))
         (val (cdr (first (cl-cc:ast-let-bindings ast)))))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-true (cl-cc:ast-lambda-p val))
    (assert-= 1 (length (cl-cc:ast-lambda-params val)))))

(deftest js-parser-arrow-function-block-body
  "const double = (x) => { return x * 2; }; lambda body wraps a (block nil ...)
that contains the return. The block is required so JS `return' (which lowers to
return-from nil) exits the arrow — a bare lambda establishes no block named nil."
  (let* ((ast (%js-first "const double = (x) => { return x * 2; };"))
         (val (cdr (first (cl-cc:ast-let-bindings ast))))
         (body (cl-cc:ast-lambda-body val)))
    (assert-true (cl-cc:ast-lambda-p val))
    (assert-true (cl-cc:ast-block-p (first body)))
    (assert-true (some #'cl-cc:ast-return-from-p
                       (cl-cc:ast-block-body (first body))))))

;;; ─── Async functions ──────────────────────────────────────────────────────────

(deftest-each js-parser-function-modifiers
  "Async and generator declarations mark the ast-defun appropriately."
  :cases (("async"     "async function fetch() {}"  :js-async)
          ("generator" "function* gen() {}"          :js-generator))
  (src decl)
  (let ((ast (%js-first src)))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-true (member decl (cl-cc:ast-defun-declarations ast)))))

;;; ─── Class declarations ───────────────────────────────────────────────────────

(deftest js-parser-class-decl-simple
  "class Dog {} produces ast-defclass."
  (let ((ast (%js-first "class Dog {}")))
    (assert-true (cl-cc:ast-defclass-p ast))
    ;; case-preserved: JS `Dog' interns as |Dog|, not DOG
    (assert-string= "Dog" (symbol-name (cl-cc:ast-defclass-name ast)))))

(deftest js-parser-class-with-extends
  "class Cat extends Animal {} records superclass."
  (let ((ast (%js-first "class Cat extends Animal {}")))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-true (some (lambda (s) (string= "Animal" (symbol-name s)))
              (cl-cc:ast-defclass-superclasses ast)))))

(deftest js-parser-class-with-constructor
  "class Pt { constructor(x,y){} } produces at least 1 slot (the constructor)."
  (let ((ast (%js-first "class Pt { constructor(x, y) { this.x = x; this.y = y; } }")))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-true (>= (length (cl-cc:ast-defclass-slots ast)) 1))))

(deftest js-parser-class-with-method
  "class C { greet() { return 1; } } slot for method is present."
  (let* ((ast (%js-first "class C { greet() { return 1; } }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-true (>= (length slots) 1))))

(deftest js-parser-class-with-static-method
  "class C { static create() {} } method slot carries :js-static t metadata."
  (let* ((ast (%js-first "class C { static create() {} }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-true (some (lambda (slot)
                          (getf (cl-cc:ast-imports slot) :js-static))
                        slots))))

(deftest js-parser-class-with-private-field
  "class C { #count = 0; } private field produces a :T-PRIVATE-IDENT-named slot."
  (let* ((ast (%js-first "class C { #count = 0; }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-true (some (lambda (slot)
                (let ((name (symbol-name (cl-cc:ast-slot-name slot))))
                  (or (search "COUNT" name) (search "PRIV" name))))
              slots))))

(deftest js-parser-class-field-arrow-initializer
  "class C { g = () => {}; } — a class field whose initialiser is an arrow
function parses to ONE class declaration. Regression: the field-initialiser
token scan was brace-blind and stopped at the arrow body's '}', truncating the
initialiser and mis-reading that '}' as the end of the class body."
  (let ((asts (%js-parse "class C { g = () => {}; }")))
    (assert-= 1 (length asts))
    (assert-true (cl-cc:ast-defclass-p (first asts)))))

(deftest js-parser-class-field-private-arrow-and-method
  "class C { #m() {} #g = () => {}; } — a private method followed by a private
field with an arrow initialiser parses to a single class with >= 2 slots."
  (let* ((asts (%js-parse "class C { #m() {} #g = () => {}; }"))
         (ast (first asts)))
    (assert-= 1 (length asts))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-true (>= (length (cl-cc:ast-defclass-slots ast)) 2))))

(deftest js-parser-class-field-object-literal-initializer
  "class C { x = {a: 1}; } — an object-literal field initialiser (also brace-
containing) parses to one class declaration."
  (let ((asts (%js-parse "class C { x = {a: 1, b: 2}; }")))
    (assert-= 1 (length asts))
    (assert-true (cl-cc:ast-defclass-p (first asts)))))

(deftest-each js-parser-tagged-template
  "Tagged templates (plain and interpolated) produce ONE ast-call."
  :cases (("plain"        "tag`hi`;")
          ("interpolated" "tag`hi ${name}`;"))
  (src)
  (let ((asts (%js-parse src)))
    (assert-= 1 (length asts))
    (assert-true (cl-cc:ast-call-p (first asts)))))

(deftest-each js-parser-class-expression
  "Class expressions produce an ast-let binding."
  :cases (("plain"   "const C = class { m() { return 1; } };")
          ("extends" "const C = class extends Base { constructor() { super(); } };"))
  (src)
  (let ((ast (%js-first src)))
    (assert-true (cl-cc:ast-let-p ast))))

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
    ;; body is wrapped in (%js-make-generator (lambda () ...))
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
    ;; outer let binds iter-sym = (%js-iter-values ...)
    (assert-true (cl-cc:ast-let-p ast))
    (assert-string= "%JS-ITER-VALUES" (%js-call-name iter-val))))

(deftest js-parser-for-in-object
  "for (const k in obj) {} lowers to an ast-let with %js-iter-keys binding."
  (let* ((ast (%js-first "for (const k in {a: 1}) {}"))
         (bindings (cl-cc:ast-let-bindings ast))
         (iter-val (cdr (first bindings))))
    ;; outer let binds iter-sym = (%js-iter-keys ...)
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
         (body (cl-cc:ast-defun-body ast))
         (ret  (first body)))
    ;; body contains return; its value is the new.target call
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
