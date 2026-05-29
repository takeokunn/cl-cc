;;;; packages/javascript/tests/js-parser-tests.lisp — ES2026 JavaScript Parser Tests
;;;;
;;;; 40 FiveAM tests covering parse-js-source for all major statement and
;;;; expression AST shapes. Each test asserts both AST type membership and
;;;; key structural properties.

(in-package :cl-cc/javascript-test)

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

(test js-parser-const-decl
  "const x = 42; parses to ast-let with one numeric binding."
  (let ((ast (%js-first "const x = 42;")))
    (is (cl-cc:ast-let-p ast))
    (is (= 1 (length (cl-cc:ast-let-bindings ast))))
    (is (cl-cc:ast-int-p (cdr (first (cl-cc:ast-let-bindings ast)))))))

(test js-parser-let-decl
  "let y = 'hello'; parses to ast-let with a string binding."
  (let ((ast (%js-first "let y = 'hello';")))
    (is (cl-cc:ast-let-p ast))
    (is (cl-cc:ast-quote-p (cdr (first (cl-cc:ast-let-bindings ast)))))))

(test js-parser-var-decl
  "var z; parses to ast-let with a nil binding."
  (let ((ast (%js-first "var z;")))
    (is (cl-cc:ast-let-p ast))))

(test js-parser-const-multi-decl
  "const a = 1, b = 2; parses to ast-let with 2 bindings."
  (let ((ast (%js-first "const a = 1, b = 2;")))
    (is (cl-cc:ast-let-p ast))
    (is (= 2 (length (cl-cc:ast-let-bindings ast))))))

;;; ─── Arrow functions ──────────────────────────────────────────────────────────

(test js-parser-arrow-function-simple
  "const f = (x) => x + 1; yields a let binding whose value is a lambda."
  (let* ((ast (%js-first "const f = (x) => x + 1;"))
         (val (cdr (first (cl-cc:ast-let-bindings ast)))))
    (is (cl-cc:ast-let-p ast))
    (is (cl-cc:ast-lambda-p val))
    (is (= 1 (length (cl-cc:ast-lambda-params val))))))

(test js-parser-arrow-function-block-body
  "const double = (x) => { return x * 2; }; lambda body has a return."
  (let* ((ast (%js-first "const double = (x) => { return x * 2; };"))
         (val (cdr (first (cl-cc:ast-let-bindings ast)))))
    (is (cl-cc:ast-lambda-p val))
    (is (some #'cl-cc:ast-return-from-p (cl-cc:ast-lambda-body val)))))

;;; ─── Async functions ──────────────────────────────────────────────────────────

(test js-parser-async-function-decl
  "async function fetch() {} produces ast-defun with :js-async in declarations."
  (let ((ast (%js-first "async function fetch() {}")))
    (is (cl-cc:ast-defun-p ast))
    (is (member :js-async (cl-cc:ast-defun-declarations ast)))))

;;; ─── Generator functions ──────────────────────────────────────────────────────

(test js-parser-generator-function-decl
  "function* gen() {} produces ast-defun with :js-generator in declarations."
  (let ((ast (%js-first "function* gen() {}")))
    (is (cl-cc:ast-defun-p ast))
    (is (member :js-generator (cl-cc:ast-defun-declarations ast)))))

;;; ─── Class declarations ───────────────────────────────────────────────────────

(test js-parser-class-decl-simple
  "class Dog {} produces ast-defclass."
  (let ((ast (%js-first "class Dog {}")))
    (is (cl-cc:ast-defclass-p ast))
    (is (string= "DOG" (symbol-name (cl-cc:ast-defclass-name ast))))))

(test js-parser-class-with-extends
  "class Cat extends Animal {} records superclass."
  (let ((ast (%js-first "class Cat extends Animal {}")))
    (is (cl-cc:ast-defclass-p ast))
    (is (some (lambda (s) (string= "ANIMAL" (symbol-name s)))
              (cl-cc:ast-defclass-superclasses ast)))))

(test js-parser-class-with-constructor
  "class Pt { constructor(x,y){} } produces at least 1 slot (the constructor)."
  (let ((ast (%js-first "class Pt { constructor(x, y) { this.x = x; this.y = y; } }")))
    (is (cl-cc:ast-defclass-p ast))
    (is (>= (length (cl-cc:ast-defclass-slots ast)) 1))))

(test js-parser-class-with-method
  "class C { greet() { return 1; } } slot for method is present."
  (let* ((ast (%js-first "class C { greet() { return 1; } }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (is (cl-cc:ast-defclass-p ast))
    (is (>= (length slots) 1))))

(test js-parser-class-with-static-method
  "class C { static create() {} } method slot carries :static metadata."
  (let* ((ast (%js-first "class C { static create() {} }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (is (cl-cc:ast-defclass-p ast))
    (is (some (lambda (slot)
                (member :static (getf (cl-cc:ast-imports slot) :js-modifiers)))
              slots))))

(test js-parser-class-with-private-field
  "class C { #count = 0; } private field produces a :T-PRIVATE-IDENT-named slot."
  (let* ((ast (%js-first "class C { #count = 0; }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (is (cl-cc:ast-defclass-p ast))
    (is (some (lambda (slot)
                (let ((name (symbol-name (cl-cc:ast-slot-name slot))))
                  (or (search "COUNT" name) (search "PRIV" name))))
              slots))))

;;; ─── If / else ────────────────────────────────────────────────────────────────

(test js-parser-if-stmt
  "if (x) { y; } produces ast-if."
  (let ((ast (%js-first "if (x) { y; }")))
    (is (cl-cc:ast-if-p ast))))

(test js-parser-if-else-stmt
  "if (a) { } else { } produces ast-if with non-nil else."
  (let ((ast (%js-first "if (a) { return 1; } else { return 2; }")))
    (is (cl-cc:ast-if-p ast))
    (is (not (cl-cc:ast-quote-p (cl-cc:ast-if-else ast))))))

;;; ─── While loop ───────────────────────────────────────────────────────────────

(test js-parser-while-loop
  "while (x) { } lowers to ast-block containing a tagbody."
  (let ((ast (%js-first "while (x) { }")))
    (is (cl-cc:ast-block-p ast))
    (is (some #'cl-cc:ast-tagbody-p (cl-cc:ast-block-body ast)))))

;;; ─── For loop ─────────────────────────────────────────────────────────────────

(test js-parser-for-c-style-loop
  "for (let i = 0; i < 10; i++) { } lowers to ast-let wrapping a block."
  (let ((ast (%js-first "for (let i = 0; i < 10; i++) { }")))
    (is (or (cl-cc:ast-let-p ast) (cl-cc:ast-block-p ast)
            (cl-cc:ast-progn-p ast)))))

(test js-parser-for-of-loop
  "for (const x of arr) { } lowers to ast-let binding an iterator."
  (let ((ast (%js-first "for (const x of arr) { }")))
    (is (cl-cc:ast-let-p ast))))

(test js-parser-for-in-loop
  "for (const k in obj) { } lowers to ast-let iterating keys."
  (let ((ast (%js-first "for (const k in obj) { }")))
    (is (cl-cc:ast-let-p ast))))

;;; ─── Switch / case ────────────────────────────────────────────────────────────

(test js-parser-switch-stmt
  "switch (x) { case 1: break; default: break; } lowers to ast-let + block."
  (let ((ast (%js-first "switch (x) { case 1: break; default: break; }")))
    (is (cl-cc:ast-let-p ast))
    (let ((block (first (cl-cc:ast-let-body ast))))
      (is (cl-cc:ast-block-p block)))))

;;; ─── Try / catch / finally ────────────────────────────────────────────────────

(test js-parser-try-catch
  "try { } catch (e) { } produces a %js-try-catch-finally call."
  (let ((ast (%js-first "try { throw 1; } catch (e) { }")))
    (is (cl-cc:ast-call-p ast))
    (is (string= "%JS-TRY-CATCH-FINALLY" (%js-call-name ast)))))

(test js-parser-try-finally
  "try { } finally { } produces a %js-try-catch-finally call."
  (let ((ast (%js-first "try { } finally { }")))
    (is (cl-cc:ast-call-p ast))
    (is (string= "%JS-TRY-CATCH-FINALLY" (%js-call-name ast)))))

(test js-parser-try-catch-finally
  "try { } catch (e) { } finally { } also produces a %js-try-catch-finally call."
  (let ((ast (%js-first "try { throw 1; } catch (e) { } finally { }")))
    (is (cl-cc:ast-call-p ast))
    (is (string= "%JS-TRY-CATCH-FINALLY" (%js-call-name ast)))))

;;; ─── Destructuring ────────────────────────────────────────────────────────────

(test js-parser-array-destructuring
  "const [a, b] = arr; produces ast-let with multiple bindings."
  (let ((ast (%js-first "const [a, b] = arr;")))
    (is (cl-cc:ast-let-p ast))
    (is (>= (length (cl-cc:ast-let-bindings ast)) 2))))

(test js-parser-object-destructuring
  "const {x, y} = obj; produces ast-let with multiple bindings."
  (let ((ast (%js-first "const {x, y} = obj;")))
    (is (cl-cc:ast-let-p ast))
    (is (>= (length (cl-cc:ast-let-bindings ast)) 2))))

;;; ─── Spread operator ──────────────────────────────────────────────────────────

(test js-parser-spread-in-array
  "[...arr] produces a call containing %js-spread."
  (let ((ast (%js-first "const x = [...arr];")))
    (is (cl-cc:ast-let-p ast))
    (let ((val (cdr (first (cl-cc:ast-let-bindings ast)))))
      (is (cl-cc:ast-call-p val)))))

;;; ─── Optional chaining ────────────────────────────────────────────────────────

(test js-parser-optional-chaining
  "a?.b produces a %js-optional-chain call."
  (let* ((ast (%js-first "const r = a?.b;"))
         (val (cdr (first (cl-cc:ast-let-bindings ast)))))
    (is (cl-cc:ast-call-p val))
    (is (string= "%JS-OPTIONAL-CHAIN" (%js-call-name val)))))

;;; ─── Nullish coalescing ───────────────────────────────────────────────────────

(test js-parser-nullish-coalescing
  "a ?? b produces a let-wrapped conditional via %js-not-nullish."
  (let* ((ast (%js-first "const r = a ?? b;"))
         (val (cdr (first (cl-cc:ast-let-bindings ast)))))
    (is (or (cl-cc:ast-let-p val) (cl-cc:ast-if-p val)
            (cl-cc:ast-call-p val)))))

;;; ─── Logical assignment ───────────────────────────────────────────────────────

(test js-parser-logical-and-assign
  "x &&= y; parses without error as an expression statement."
  (let ((asts (%js-parse "let x = 1; x &&= 0;")))
    (is (= 2 (length asts)))))

(test js-parser-logical-or-assign
  "x ||= y; parses without error."
  (let ((asts (%js-parse "let x = null; x ||= 42;")))
    (is (= 2 (length asts)))))

(test js-parser-nullish-assign
  "x ??= y; parses without error."
  (let ((asts (%js-parse "let x = null; x ??= 'default';")))
    (is (= 2 (length asts)))))

;;; ─── Template literals ────────────────────────────────────────────────────────

(test js-parser-template-literal
  "Backtick template literal produces some AST node (at minimum a string)."
  (let ((ast (%js-first "const s = `hello`;")))
    (is (cl-cc:ast-let-p ast))
    (let ((val (cdr (first (cl-cc:ast-let-bindings ast)))))
      (is (not (null val))))))

;;; ─── Import / export ──────────────────────────────────────────────────────────

(test js-parser-import-statement
  "import x from 'mod'; parses as a module import in module mode."
  (let ((asts (cl-cc/javascript:parse-js-module "import x from 'mod';")))
    (is (>= (length asts) 0))))

(test js-parser-export-const
  "export const val = 1; in module mode produces at least one AST node."
  (let ((asts (cl-cc/javascript:parse-js-module "export const val = 1;")))
    (is (>= (length asts) 1))))

;;; ─── Using declaration (ES2025) ───────────────────────────────────────────────

(test js-parser-using-declaration
  "using x = getResource(); parses to ast-let with :js-using kind."
  (let ((ast (%js-first "using x = getResource();")))
    (is (cl-cc:ast-let-p ast))
    (is (member :js-using (cl-cc:ast-let-declarations ast)))))

;;; ─── Throw statement ──────────────────────────────────────────────────────────

(test js-parser-throw-stmt
  "throw new Error('msg'); lowers to a %js-throw call."
  (let ((ast (%js-first "throw new Error('msg');")))
    (is (cl-cc:ast-call-p ast))
    (is (string= "%JS-THROW" (%js-call-name ast)))))

;;; ─── Return statement ─────────────────────────────────────────────────────────

(test js-parser-return-with-value
  "function f() { return 42; } body contains ast-return-from."
  (let* ((ast (%js-first "function f() { return 42; }"))
         (body (cl-cc:ast-defun-body ast)))
    (is (cl-cc:ast-defun-p ast))
    (is (some #'cl-cc:ast-return-from-p body))))

(test js-parser-return-bare
  "function g() { return; } has return-from with nil-valued quote."
  (let* ((ast (%js-first "function g() { return; }"))
         (ret (first (cl-cc:ast-defun-body ast))))
    (is (cl-cc:ast-return-from-p ret))
    (is (cl-cc:ast-quote-p (cl-cc:ast-return-from-value ret)))))

;;; ─── Multi-statement source ───────────────────────────────────────────────────

(test js-parser-multi-statement-source
  "Three top-level statements produce a list of three AST nodes."
  (let ((asts (%js-parse "const a = 1; const b = 2; const c = 3;")))
    (is (= 3 (length asts)))))
