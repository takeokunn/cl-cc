;;;; tests/unit/parse/cl-parser-ast-tests.lisp — ast-to-sexp roundtrip and CLOS lowering tests
;;;;
;;;; Tests: ast-to-sexp roundtrip, sexp-head-to-kind, grammar specialized parsers,
;;;; defmacro/defclass/defgeneric/defmethod/make-instance lowering.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── ast-to-sexp roundtrip ──────────────────────────────────────────────────

(defun ast-roundtrip (sexp)
  "Lower sexp to AST then convert back to sexp."
  (cl-cc/ast:ast-to-sexp (lower sexp)))

(deftest-each ast-roundtrip-atoms
  "ast-to-sexp roundtrip: atomic forms preserve integer and symbol identity."
  :cases (("integer" 42 42)
          ("symbol"  'x 'x))
  (input expected)
  (assert-equal expected (ast-roundtrip input)))

(deftest ast-roundtrip-wildcard-and-if-structure
  "ast-to-sexp roundtrip: _ round-trips by name; if preserves condition, then, and else."
  (assert-string= "_" (symbol-name (ast-roundtrip '_)))
  (let ((result (ast-roundtrip '(if x 1 2))))
    (assert-eq 'x (second result))
    (assert-= 1 (third result))
    (assert-= 2 (fourth result))))

(deftest-each ast-roundtrip-control-flow
  "ast-to-sexp roundtrip: control-flow heads are preserved."
  :cases (("if"    '(if x 1 2)        'if)
          ("progn" '(progn 1 2 3)     'progn)
          ("block" '(block outer 1 2) 'block)
          ("let"   '(let ((x 10)) x)  'let))
  (input expected-head)
  (assert-eq expected-head (first (ast-roundtrip input))))

(deftest-each ast-roundtrip-exact-forms
  "ast-roundtrip preserves the complete sexp for self-roundtripping forms."
  :cases (("setq"  '(setq x 42))
          ("quote" '(quote hello)))
  (form)
  (assert-equal form (ast-roundtrip form)))

(deftest-each ast-roundtrip-definition-heads
  "ast-roundtrip preserves head, name, and param-list for definition forms."
  :cases (("lambda" '(lambda (x y) (+ x y)) 'lambda  nil       '(x y))
          ("defun"  '(defun add (a b) (+ a b)) 'defun 'add     '(a b))
          ("defvar" '(defvar *count* 0)          'defvar '*count* nil))
  (form expected-head expected-name expected-params)
  (let ((result (ast-roundtrip form)))
    (assert-eq expected-head (first result))
    (when expected-name
      (assert-eq expected-name (second result)))
    (when expected-params
      (assert-equal expected-params (if expected-name (third result) (second result))))))

;;; ─── sexp-head-to-kind ───────────────────────────────────────────────────────

(deftest-each grammar-sexp-head-to-kind
  "sexp-head-to-kind maps each special form head to its kind keyword."
  :cases (("defun"   'defun                   :defun)
          ("let"     'let                     :let)
          ("if"      'if                      :if)
          ("lambda"  'lambda                  :lambda)
          ("setq"    'setq                    :setq)
          ("progn"   'progn                   :progn)
          ("defclass" 'defclass               :defclass)
          ("unknown" 'completely-unknown-symbol :call))
  (sym expected)
  (assert-eq expected (cl-cc/parse::sexp-head-to-kind sym)))

;;; ─── Grammar specialized parsers ────────────────────────────────────────────

(deftest-each grammar-parse-cl-form-atoms
  "parse-cl-form: scalar tokens produce cst-token with correct value."
  :cases (("integer" "42"        42)
          ("string"  "\"hello\"" "hello"))
  (source expected-value)
  (let* ((tokens (cl-cc:lex-all source))
         (ts (cl-cc/parse::make-token-stream :tokens tokens :source source))
         (form (cl-cc/parse::parse-cl-form ts)))
    (assert-true (cl-cc:cst-token-p form))
    (assert-equal expected-value (cl-cc:cst-token-value form))))

(deftest-each grammar-parse-cl-form-lists
  "parse-cl-form: list -> cst-interior; empty list -> nil children."
  :cases (("non-empty" "(1 2 3)" 3)
          ("empty"     "()"      0))
  (source expected-child-count)
  (let* ((tokens (cl-cc:lex-all source))
         (ts (cl-cc/parse::make-token-stream :tokens tokens :source source))
         (form (cl-cc/parse::parse-cl-form ts)))
    (assert-true (cl-cc:cst-interior-p form))
    (assert-= expected-child-count (length (cl-cc:cst-children form)))))

(deftest-each grammar-parse-cl-form-reader-macros
  "parse-cl-form: reader-macro sugar -> cst-interior node with correct kind."
  :cases (("quote"    "'foo"  :quote)
          ("backquote" "`foo" :quasiquote)
          ("function" "#'foo" :function))
  (source expected-kind)
  (let* ((tokens (cl-cc:lex-all source))
         (ts (cl-cc/parse::make-token-stream :tokens tokens :source source))
         (form (cl-cc/parse::parse-cl-form ts)))
    (assert-true (cl-cc:cst-interior-p form))
    (assert-eq expected-kind (cl-cc:cst-node-kind form))))

;;; ─── defmacro lowering ───────────────────────────────────────────────────────

(deftest lower-defmacro-form
  "lower-sexp-to-ast: defmacro form -> ast-defmacro"
  (let ((node (lower '(defmacro my-mac (x) `(list ,x)))))
    (assert-true (cl-cc/ast::ast-defmacro-p node))
    (assert-eq 'my-mac (cl-cc/ast::ast-defmacro-name node))
    (assert-equal '(x) (cl-cc/ast::ast-defmacro-lambda-list node))))

;;; ─── defclass lowering ───────────────────────────────────────────────────────

(deftest-each lower-defclass-form
  "lower-sexp-to-ast: defclass -> ast-defclass with correct name, superclasses, and slot count."
  :cases (("no-superclass"   '(defclass point () (x y))              'point '()      2)
          ("with-superclass" '(defclass colored-point (point) (color)) 'colored-point '(point) 1))
  (form expected-name expected-supers expected-slots)
  (let ((node (lower form)))
    (assert-true (cl-cc/ast::ast-defclass-p node))
    (assert-eq expected-name (cl-cc/ast::ast-defclass-name node))
    (assert-equal expected-supers (cl-cc/ast::ast-defclass-superclasses node))
    (assert-= expected-slots (length (cl-cc/ast::ast-defclass-slots node)))))

;;; ─── defgeneric / defmethod lowering ────────────────────────────────────────

(deftest-each lower-generic-dispatch-forms
  "lower-sexp-to-ast: defgeneric and defmethod produce correct AST nodes."
  :cases (("defgeneric" '(defgeneric area (shape))
           #'cl-cc/ast::ast-defgeneric-p #'cl-cc/ast::ast-defgeneric-name #'cl-cc/ast::ast-defgeneric-params
           'area '(shape))
          ("defmethod"  '(defmethod area ((s circle)) (* pi (expt (slot-value s 'radius) 2)))
           #'cl-cc/ast::ast-defmethod-p #'cl-cc/ast::ast-defmethod-name #'cl-cc/ast::ast-defmethod-params
           'area '(s)))
  (form pred-p get-name get-params expected-name expected-params)
  (let ((node (lower form)))
    (assert-true (funcall pred-p node))
    (assert-eq expected-name (funcall get-name node))
    (assert-equal expected-params (funcall get-params node))))

;;; ─── make-instance lowering ──────────────────────────────────────────────────

(deftest-each lower-clos-access-forms
  "lower-sexp-to-ast: make-instance and slot-value produce correct node types."
  :cases (("make-instance" '(make-instance 'point :x 1 :y 2) #'cl-cc/ast::ast-make-instance-p)
          ("slot-value"    '(slot-value obj 'name)            #'cl-cc/ast::ast-slot-value-p))
  (form pred)
  (let ((node (lower form)))
    (assert-true (funcall pred node))))
