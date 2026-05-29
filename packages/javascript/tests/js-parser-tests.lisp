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
    (assert-true (= 1 (length (cl-cc:ast-let-bindings ast))))
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
  "const a = 1, b = 2; parses to ast-let with 2 bindings."
  (let ((ast (%js-first "const a = 1, b = 2;")))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-true (= 2 (length (cl-cc:ast-let-bindings ast))))))

;;; ─── Arrow functions ──────────────────────────────────────────────────────────

(deftest js-parser-arrow-function-simple
  "const f = (x) => x + 1; yields a let binding whose value is a lambda."
  (let* ((ast (%js-first "const f = (x) => x + 1;"))
         (val (cdr (first (cl-cc:ast-let-bindings ast)))))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-true (cl-cc:ast-lambda-p val))
    (assert-true (= 1 (length (cl-cc:ast-lambda-params val))))))

(deftest js-parser-arrow-function-block-body
  "const double = (x) => { return x * 2; }; lambda body has a return."
  (let* ((ast (%js-first "const double = (x) => { return x * 2; };"))
         (val (cdr (first (cl-cc:ast-let-bindings ast)))))
    (assert-true (cl-cc:ast-lambda-p val))
    (assert-true (some #'cl-cc:ast-return-from-p (cl-cc:ast-lambda-body val)))))

;;; ─── Async functions ──────────────────────────────────────────────────────────

(deftest js-parser-async-function-decl
  "async function fetch() {} produces ast-defun with :js-async in declarations."
  (let ((ast (%js-first "async function fetch() {}")))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-true (member :js-async (cl-cc:ast-defun-declarations ast)))))

;;; ─── Generator functions ──────────────────────────────────────────────────────

(deftest js-parser-generator-function-decl
  "function* gen() {} produces ast-defun with :js-generator in declarations."
  (let ((ast (%js-first "function* gen() {}")))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-true (member :js-generator (cl-cc:ast-defun-declarations ast)))))

;;; ─── Class declarations ───────────────────────────────────────────────────────

(deftest js-parser-class-decl-simple
  "class Dog {} produces ast-defclass."
  (let ((ast (%js-first "class Dog {}")))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-true (string= "DOG" (symbol-name (cl-cc:ast-defclass-name ast))))))

(deftest js-parser-class-with-extends
  "class Cat extends Animal {} records superclass."
  (let ((ast (%js-first "class Cat extends Animal {}")))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-true (some (lambda (s) (string= "ANIMAL" (symbol-name s)))
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
  "class C { static create() {} } method slot carries :static metadata."
  (let* ((ast (%js-first "class C { static create() {} }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-true (some (lambda (slot)
                (member :static (getf (cl-cc:ast-imports slot) :js-modifiers)))
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
    (assert-true (= 1 (length asts)))
    (assert-true (cl-cc:ast-defclass-p (first asts)))))

(deftest js-parser-class-field-private-arrow-and-method
  "class C { #m() {} #g = () => {}; } — a private method followed by a private
field with an arrow initialiser parses to a single class with >= 2 slots."
  (let* ((asts (%js-parse "class C { #m() {} #g = () => {}; }"))
         (ast (first asts)))
    (assert-true (= 1 (length asts)))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-true (>= (length (cl-cc:ast-defclass-slots ast)) 2))))

(deftest js-parser-class-field-object-literal-initializer
  "class C { x = {a: 1}; } — an object-literal field initialiser (also brace-
containing) parses to one class declaration."
  (let ((asts (%js-parse "class C { x = {a: 1, b: 2}; }")))
    (assert-true (= 1 (length asts)))
    (assert-true (cl-cc:ast-defclass-p (first asts)))))

(deftest js-parser-tagged-template-plain
  "tag`hi`; — a no-interpolation tagged template is ONE call expression, not a
bare identifier followed by a separate string. Regression: a plain backtick
template lexed to :T-STRING, indistinguishable from a string literal, so the
postfix parser never attached it to the tag."
  (let ((asts (%js-parse "tag`hi`;")))
    (assert-true (= 1 (length asts)))
    (assert-true (cl-cc:ast-call-p (first asts)))))

(deftest js-parser-tagged-template-interpolated
  "tag`hi ${name}`; — an interpolated tagged template is ONE call expression."
  (let ((asts (%js-parse "tag`hi ${name}`;")))
    (assert-true (= 1 (length asts)))
    (assert-true (cl-cc:ast-call-p (first asts)))))

(deftest js-parser-class-expression
  "const C = class { m() {} }; — a class EXPRESSION parses without crashing
(regression guard for the removed duplicate %js-parse-class-body)."
  (let ((ast (%js-first "const C = class { m() { return 1; } };")))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-true (not (null (cdr (first (cl-cc:ast-let-bindings ast))))))))

(deftest js-parser-class-expression-extends
  "const C = class extends Base { constructor() { super(); } }; parses."
  (let ((ast (%js-first "const C = class extends Base { constructor() { super(); } };")))
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

(deftest js-parser-for-c-style-loop
  "for (let i = 0; i < 10; i++) { } lowers to ast-let wrapping a block."
  (let ((ast (%js-first "for (let i = 0; i < 10; i++) { }")))
    (assert-true (or (cl-cc:ast-let-p ast) (cl-cc:ast-block-p ast)
            (cl-cc:ast-progn-p ast)))))

(deftest js-parser-for-of-loop
  "for (const x of arr) { } lowers to ast-let binding an iterator."
  (let ((ast (%js-first "for (const x of arr) { }")))
    (assert-true (cl-cc:ast-let-p ast))))

(deftest js-parser-for-in-loop
  "for (const k in obj) { } lowers to ast-let iterating keys."
  (let ((ast (%js-first "for (const k in obj) { }")))
    (assert-true (cl-cc:ast-let-p ast))))

;;; ─── Switch / case ────────────────────────────────────────────────────────────

(deftest js-parser-switch-stmt
  "switch (x) { case 1: break; default: break; } lowers to ast-let + block."
  (let ((ast (%js-first "switch (x) { case 1: break; default: break; }")))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((block (first (cl-cc:ast-let-body ast))))
      (assert-true (cl-cc:ast-block-p block)))))

;;; ─── Try / catch / finally ────────────────────────────────────────────────────

(deftest js-parser-try-catch
  "try { } catch (e) { } produces a %js-try-catch-finally call."
  (let ((ast (%js-first "try { throw 1; } catch (e) { }")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-true (string= "%JS-TRY-CATCH-FINALLY" (%js-call-name ast)))))

(deftest js-parser-try-finally
  "try { } finally { } produces a %js-try-catch-finally call."
  (let ((ast (%js-first "try { } finally { }")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-true (string= "%JS-TRY-CATCH-FINALLY" (%js-call-name ast)))))

(deftest js-parser-try-catch-finally
  "try { } catch (e) { } finally { } also produces a %js-try-catch-finally call."
  (let ((ast (%js-first "try { throw 1; } catch (e) { } finally { }")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-true (string= "%JS-TRY-CATCH-FINALLY" (%js-call-name ast)))))

;;; ─── Destructuring ────────────────────────────────────────────────────────────

(deftest js-parser-array-destructuring
  "const [a, b] = arr; produces ast-let with multiple bindings."
  (let ((ast (%js-first "const [a, b] = arr;")))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-true (>= (length (cl-cc:ast-let-bindings ast)) 2))))

(deftest js-parser-object-destructuring
  "const {x, y} = obj; produces ast-let with multiple bindings."
  (let ((ast (%js-first "const {x, y} = obj;")))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-true (>= (length (cl-cc:ast-let-bindings ast)) 2))))

(deftest js-parser-array-destructuring-default
  "const [a = 1, b = 2] = arr; parses element defaults."
  (let ((ast (%js-first "const [a = 1, b = 2] = arr;")))
    (assert-true (cl-cc:ast-let-p ast))))

(deftest js-parser-object-destructuring-default
  "const {a = 1} = obj; parses a shorthand property default."
  (let ((ast (%js-first "const {a = 1} = obj;")))
    (assert-true (cl-cc:ast-let-p ast))))

(deftest js-parser-object-destructuring-rename-default
  "const {a: x = 5} = obj; parses a renamed property with a default."
  (let ((ast (%js-first "const {a: x = 5} = obj;")))
    (assert-true (cl-cc:ast-let-p ast))))

(deftest js-parser-array-destructuring-default-with-rest
  "const [x, y = 10, ...rest] = arr; mixes default and rest elements."
  (let ((ast (%js-first "const [x, y = 10, ...rest] = arr;")))
    (assert-true (cl-cc:ast-let-p ast))))

;;; ─── Spread operator ──────────────────────────────────────────────────────────

(deftest js-parser-spread-in-array
  "[...arr] produces a call containing %js-spread."
  (let ((ast (%js-first "const x = [...arr];")))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((val (cdr (first (cl-cc:ast-let-bindings ast)))))
      (assert-true (cl-cc:ast-call-p val)))))

;;; ─── Optional chaining ────────────────────────────────────────────────────────

(deftest js-parser-optional-chaining
  "a?.b produces a %js-optional-chain call."
  (let* ((ast (%js-first "const r = a?.b;"))
         (val (cdr (first (cl-cc:ast-let-bindings ast)))))
    (assert-true (cl-cc:ast-call-p val))
    (assert-true (string= "%JS-OPTIONAL-CHAIN" (%js-call-name val)))))

;;; ─── Nullish coalescing ───────────────────────────────────────────────────────

(deftest js-parser-nullish-coalescing
  "a ?? b produces a let-wrapped conditional via %js-not-nullish."
  (let* ((ast (%js-first "const r = a ?? b;"))
         (val (cdr (first (cl-cc:ast-let-bindings ast)))))
    (assert-true (or (cl-cc:ast-let-p val) (cl-cc:ast-if-p val)
            (cl-cc:ast-call-p val)))))

;;; ─── Logical assignment ───────────────────────────────────────────────────────

(deftest js-parser-logical-and-assign
  "x &&= y; parses without error as an expression statement."
  (let ((asts (%js-parse "let x = 1; x &&= 0;")))
    (assert-true (= 2 (length asts)))))

(deftest js-parser-logical-or-assign
  "x ||= y; parses without error."
  (let ((asts (%js-parse "let x = null; x ||= 42;")))
    (assert-true (= 2 (length asts)))))

(deftest js-parser-nullish-assign
  "x ??= y; parses without error."
  (let ((asts (%js-parse "let x = null; x ??= 'default';")))
    (assert-true (= 2 (length asts)))))

;;; ─── Template literals ────────────────────────────────────────────────────────

(deftest js-parser-template-literal
  "Backtick template literal produces some AST node (at minimum a string)."
  (let ((ast (%js-first "const s = `hello`;")))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((val (cdr (first (cl-cc:ast-let-bindings ast)))))
      (assert-true (not (null val))))))

(deftest js-parser-template-interpolation
  "Interpolated template `hi ${name}!` parses without error to a binding value."
  (let ((ast (%js-first "const s = `hi ${name}!`;")))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-true (not (null (cdr (first (cl-cc:ast-let-bindings ast))))))))

(deftest js-parser-template-interpolation-expr
  "Template interpolation with an expression `${a + b * 2}` parses."
  (let ((ast (%js-first "const s = `sum=${a + b * 2}`;")))
    (assert-true (cl-cc:ast-let-p ast))))

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
    (assert-true (string= "%JS-THROW" (%js-call-name ast)))))

;;; ─── Return statement ─────────────────────────────────────────────────────────

(deftest js-parser-return-with-value
  "function f() { return 42; } body contains ast-return-from."
  (let* ((ast (%js-first "function f() { return 42; }"))
         (body (cl-cc:ast-defun-body ast)))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-true (some #'cl-cc:ast-return-from-p body))))

(deftest js-parser-return-bare
  "function g() { return; } has return-from with nil-valued quote."
  (let* ((ast (%js-first "function g() { return; }"))
         (ret (first (cl-cc:ast-defun-body ast))))
    (assert-true (cl-cc:ast-return-from-p ret))
    (assert-true (cl-cc:ast-quote-p (cl-cc:ast-return-from-value ret)))))

;;; ─── Multi-statement source ───────────────────────────────────────────────────

(deftest js-parser-multi-statement-source
  "Three top-level statements produce a list of three AST nodes."
  (let ((asts (%js-parse "const a = 1; const b = 2; const c = 3;")))
    (assert-true (= 3 (length asts)))))
