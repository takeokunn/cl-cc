;;;; tests/unit/parse/cl-parser-new-tests.lisp — Additional parser and AST tests
;;;;
;;;; Tests: parse-source atom types, structural forms, vectors, edge cases,
;;;; parse-all-forms additional cases, AST constructors, struct properties,
;;;; lower-sexp extended cases, additional roundtrip tests,
;;;; parse-slot-spec, lambda-list edge cases, parse-then-lower pipeline.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── NEW: parse-source atom parsing ────────────────────────────────────────

(deftest-each parser-atom-types
  "parse-source: various atom types parsed correctly"
  :cases (("float"          "3.14"        3.14)
          ("zero"           "0"           0)
          ("large-int"      "999999"      999999)
          ("keyword"        ":foo"        :foo)
          ("keyword-upper"  ":BAR"        :bar)
          ("char-a"         "#\\a"        #\a)
          ("char-space"     "#\\Space"    #\Space)
          ("char-newline"   "#\\Newline"  #\Newline))
  (source expected)
  (let ((result (parse-one source)))
    (assert-true (eql expected result))))

(deftest parser-parse-source-structure-cases
  "parse-source: dotted pair → cons with A/B; 6-deep nested → innermost is 1."
  (let ((result (parse-one "(a . b)")))
    (assert-true (consp result))
    (assert-equal "A" (symbol-name (car result)))
    (assert-equal "B" (symbol-name (cdr result))))
  (let ((result (parse-one "((((((1))))))")))
    (assert-true (consp result))
    (assert-= 1 (first (first (first (first (first (first result)))))))))

(deftest-each parser-parse-source-vector
  "parse-source: vector literals parse to vectors with the expected length."
  :cases (("empty" "#()"      0)
          ("three" "#(1 2 3)" 3))
  (source expected-len)
  (let ((result (parse-one source)))
    (assert-true (vectorp result))
    (assert-= expected-len (length result))))

(deftest parser-parse-source-empty-signals-error
  "parse-source signals an error on an empty source string."
  (assert-signals error (parse-one "")))

(deftest parser-parse-source-quasiquote-produces-cons
  "parse-source: quasiquoted list and quasiquoted atom both produce cons cells."
  (let ((result (parse-one "`(a b)")))
    (assert-true (consp result))
    (assert-true (symbolp (first result))))
  (let ((result (parse-one "`,x")))
    (assert-true (consp result))))

(deftest parser-parse-source-long-symbol-name-preserved
  "parse-source preserves a 200-character symbol name exactly."
  (let* ((long-name (make-string 200 :initial-element #\A))
         (result (parse-one long-name)))
    (assert-true (symbolp result))
    (assert-= 200 (length (symbol-name result)))))

;;; ─── NEW: parse-all-forms additional cases ─────────────────────────────────

(deftest parser-parse-all-forms-cases
  "parse-all-forms: int/string/list parsed correctly; line comments stripped."
  (let ((forms (parse-many "42 \"hello\" (+ 1 2)")))
    (assert-= 3 (length forms))
    (assert-= 42 (first forms))
    (assert-string= "hello" (second forms))
    (assert-true (consp (third forms))))
  (let ((forms (parse-many (format nil "; comment~%1 2"))))
    (assert-= 2 (length forms))
    (assert-= 1 (first forms))
    (assert-= 2 (second forms))))

(deftest-each parse-all-forms-length-cases
  "parse-all-forms returns the correct form count."
  :cases (("whitespace-only" (format nil " ~% ") 0)
          ("ten-forms"       "1 2 3 4 5 6 7 8 9 10"  10))
  (source expected-len)
  (assert-= expected-len (length (parse-many source))))

;;; ─── NEW: AST node constructors ────────────────────────────────────────────

(deftest-each ast-constructor-basic
  "AST node constructors create correct struct types"
  :cases (("ast-int"       (cl-cc/ast::make-ast-int :value 5)              #'cl-cc/ast::ast-int-p)
          ("ast-var"       (cl-cc/ast::make-ast-var :name 'x)              #'cl-cc/ast::ast-var-p)
          ("ast-quote"     (cl-cc/ast::make-ast-quote :value 'hello)       #'cl-cc/ast::ast-quote-p)
          ("ast-progn"     (cl-cc/ast::make-ast-progn :forms nil)          #'cl-cc/ast::ast-progn-p)
          ("ast-print"     (cl-cc/ast::make-ast-print :expr nil)           #'cl-cc/ast::ast-print-p)
          ("ast-block"     (cl-cc/ast::make-ast-block :name 'b :body nil)  #'cl-cc/ast::ast-block-p)
          ("ast-go"        (cl-cc/ast::make-ast-go :tag 'done)             #'cl-cc/ast::ast-go-p)
          ("ast-setq"      (cl-cc/ast::make-ast-setq :var 'x :value nil)   #'cl-cc/ast::ast-setq-p)
          ("ast-function"  (cl-cc/ast::make-ast-function :name 'foo)       #'cl-cc/ast::ast-function-p)
          ("ast-the"       (cl-cc/ast::make-ast-the :type 'fixnum :value nil) #'cl-cc/ast::ast-the-p)
          ("ast-values"    (cl-cc/ast::make-ast-values :forms nil)         #'cl-cc/ast::ast-values-p))
  (node pred)
  (assert-true (funcall pred node)))

(deftest ast-source-location-fields-stored
  "AST nodes store source-file, source-line, and source-column from constructor."
  (let ((node (cl-cc/ast::make-ast-int :value 42
                                       :source-file "test.lisp"
                                       :source-line 10
                                       :source-column 5)))
    (assert-string= "test.lisp" (cl-cc::ast-source-file node))
    (assert-= 10 (cl-cc::ast-source-line node))
    (assert-= 5  (cl-cc::ast-source-column node))))

(deftest ast-lambda-callable-slots
  "ast-lambda stores params, optional-params, rest-param, and key-params correctly."
  (let ((node (cl-cc/ast::make-ast-lambda :params '(x)
                                          :optional-params '((y nil))
                                          :rest-param 'args
                                          :key-params '((z nil))
                                          :body nil)))
    (assert-equal '(x) (cl-cc::ast-lambda-params node))
    (assert-equal '((y nil)) (cl-cc::ast-lambda-optional-params node))
    (assert-eq 'args (cl-cc::ast-lambda-rest-param node))
    (assert-equal '((z nil)) (cl-cc::ast-lambda-key-params node))))

(deftest ast-slot-def-full-options
  "ast-slot-def stores name, initarg, reader, writer, accessor, and type."
  (let ((slot (cl-cc/ast::make-ast-slot-def :name 'x
                                            :initarg :x
                                            :reader 'get-x
                                            :writer 'set-x
                                            :accessor 'x-accessor
                                            :type 'integer)))
    (assert-eq 'x         (cl-cc::ast-slot-name     slot))
    (assert-eq :x         (cl-cc::ast-slot-initarg   slot))
    (assert-eq 'get-x     (cl-cc::ast-slot-reader    slot))
    (assert-eq 'set-x     (cl-cc::ast-slot-writer    slot))
    (assert-eq 'x-accessor (cl-cc::ast-slot-accessor slot))
    (assert-eq 'integer   (cl-cc::ast-slot-type      slot))))

(deftest-each ast-location-string-formats
  "ast-location-string formats file:line:col when present; falls back to <unknown location>."
  :cases (("with-location"    (cl-cc/ast::make-ast-int :value 1
                                :source-file "foo.lisp" :source-line 3 :source-column 7)
           "foo.lisp:3:7")
          ("unknown-location" (cl-cc/ast::make-ast-int :value 1)
           "<unknown location>"))
  (node expected)
  (assert-string= expected (cl-cc/ast::ast-location-string node)))

;;; ─── NEW: lower-sexp-to-ast additional forms ──────────────────────────────

(deftest lower-binary-operators-produce-ast-binop
  "lower-sexp-to-ast maps all standard binary operators to ast-binop with the correct op."
  (dolist (op '(+ - * = < > <= >=))
    (let ((node (lower (list op 1 2))))
      (assert-true (cl-cc/ast::ast-binop-p node))
      (assert-eq op (cl-cc/ast::ast-binop-op node)))))

(deftest lower-simple-form-cases
  "lower-sexp-to-ast: (print 42) → ast-print with ast-int; defparameter → ast-defvar."
  (let ((node (lower '(print 42))))
    (assert-true (cl-cc/ast::ast-print-p node))
    (assert-true (cl-cc/ast::ast-int-p (cl-cc/ast::ast-print-expr node))))
  (let ((node (lower '(defparameter *x* 99))))
    (assert-true (cl-cc/ast::ast-defvar-p node))
    (assert-eq '*x* (cl-cc/ast::ast-defvar-name node))))

(deftest-each lower-extended-lambda-list-params
  "lower-sexp-to-ast: &optional, &rest, &key all populate the correct extended-params slot."
  :cases (("optional" '(lambda (x &optional (y 0)) (+ x y))
           #'cl-cc/ast::ast-lambda-p #'cl-cc::ast-lambda-optional-params 1)
          ("rest"     '(lambda (x &rest args) args)
           #'cl-cc/ast::ast-lambda-p #'cl-cc::ast-lambda-rest-param :rest)
          ("key"      '(defun f (x &key (size 10)) x)
           #'cl-cc/ast::ast-defun-p #'cl-cc::ast-defun-key-params 1))
  (form pred-p get-slot expected)
  (let ((node (lower form)))
    (assert-true (funcall pred-p node))
    (let ((slot-val (funcall get-slot node)))
      (if (eq expected :rest)
          (assert-eq 'args slot-val)
          (assert-= expected (length slot-val))))))

(deftest lower-special-form-cases
  "lower-sexp-to-ast: return-from without value → error; multiple-value-call → ast-mvc with 2 args."
  (assert-signals error (lower '(return-from blk)))
  (let ((node (lower '(multiple-value-call #'list (values 1 2) (values 3 4)))))
    (assert-true (cl-cc/ast::ast-multiple-value-call-p node))
    (assert-= 2 (length (cl-cc::ast-mv-call-args node)))))

(deftest lower-setf-cases
  "lower-sexp-to-ast: (setf (slot-value obj 'x) 10) → ast-set-slot-value; (setf x 10) → ast-setq."
  (let ((node (lower '(setf (slot-value obj 'x) 10))))
    (assert-true (cl-cc/ast::ast-set-slot-value-p node))
    (assert-eq 'x (cl-cc/ast::ast-set-slot-value-slot node)))
  (let ((node (lower '(setf x 10))))
    (assert-true (cl-cc/ast::ast-setq-p node))
    (assert-eq 'x (cl-cc/ast::ast-setq-var node))))

;;; ─── NEW: lower-sexp-to-ast error cases ───────────────────────────────────

(deftest-each lower-sexp-body-errors
  "lower-sexp-to-ast signals error for forms missing required body or cleanup."
  :cases (("progn-no-body"     '(progn))
          ("let-no-body"       '(let ()))
          ("lambda-no-body"    '(lambda ()))
          ("defun-no-body"     '(defun f ()))
          ("block-no-body"     '(block b))
          ("binop-wrong-arity" '(+ 1))
          ("go-no-tag"         '(go))
          ("catch-no-body"     '(catch 'tag))
          ("throw-wrong-arity" '(throw 'tag))
          ("unwind-no-cleanup" '(unwind-protect (x)))
          ("handler-no-clause" '(handler-case (x)))
          ("the-wrong-arity"   '(the fixnum))
          ("defclass-no-slots" '(defclass c ()))
          ("defgeneric-no-ll"  '(defgeneric g))
          ("defmethod-no-body" '(defmethod m ()))
          ("defmacro-no-body"  '(defmacro m ())))
  (form)
  (assert-signals error (lower form)))

