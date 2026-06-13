;;;; packages/javascript/tests/js-parser-decl-tests.lisp — JS parser: declarations
;;;;
;;;; Shared parse helpers (available to js-parser-stmt-tests via serial load):
;;;;   %js-parse SOURCE  → list of top-level AST nodes
;;;;   %js-first SOURCE  → first top-level AST node
;;;;   %js-call-name AST → upcased name string for an ast-call
;;;;
;;;; Covers: variable/const/let declarations, arrow functions, async/generator
;;;;         modifiers, class declarations with fields/methods/static/private.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Shared helpers (available to all js-parser-* files via serial load) ─────

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

;;; ─── Async / generator function modifiers ────────────────────────────────────

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
