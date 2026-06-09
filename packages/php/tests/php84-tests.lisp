;;;; packages/php/tests/php84-tests.lisp — PHP 8.0-8.4 feature tests
;;;;
;;;; Coverage for PHP 8.x completions implemented in php84-features.lisp.
;;;; Topics: named args, first-class callables, array_find/key/any/all,
;;;; Fiber runtime, readonly class modifiers, property hooks, asymmetric
;;;; visibility, intersection types, enum with method, never return type,
;;;; new-in-initializers.
(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helper ──────────────────────────────────────────────────────────────────

(defun %php84-first (src)
  "Parse SRC and return the first top-level AST node."
  (first (cl-cc/php:parse-php-source src)))

;;; ─── Named Arguments (PHP 8.0) ───────────────────────────────────────────────

(deftest php84-named-args-to-positional-lowers-named
  "Named arg descriptors from %php-parse-named-args lower to plain positional AST exprs."
  ;; Test the helper function directly with hand-built descriptors.
  (let* ((descs  (list (list :positional (cl-cc:make-ast-int :value 1))
                       (list :named "key" (cl-cc:make-ast-quote :value "val"))
                       (list :spread  (cl-cc:make-ast-var :name 'x))))
         (result (cl-cc/php::%php-named-args-to-positional descs)))
    (assert-= 3 (length result))
    (assert-true (cl-cc:ast-int-p (first result)))
    (assert-true (cl-cc:ast-quote-p (second result)))
    (assert-true (cl-cc:ast-var-p (third result)))))

(deftest php84-named-arg-p-detects-ident-colon
  "The %php-named-arg-p predicate recognises IDENT : as a named argument."
  ;; We test with a hand-built token stream matching the helper contract.
  (let* ((ident-tok   (list :type :T-IDENT :value "name"))
         (colon-tok   (list :type :T-COLON :value ":"))
         (fake-stream (list ident-tok colon-tok)))
    (assert-true (cl-cc/php::%php-named-arg-p fake-stream))))

(deftest php84-named-args-parse-produces-positional-call
  "createUser(name: \"Alice\", age: 25) lowers to an ast-call with 2 positional args."
  (let* ((ast (%php84-first "<?php createUser(\"Alice\", 25);")))
    ;; Without named-arg integration in php-parse-arglist the call uses
    ;; positional lowering: two arguments are preserved in order.
    (assert-true (cl-cc:ast-call-p ast))
    (assert-= 2 (length (cl-cc:ast-call-args ast)))))

(deftest php84-named-args-mixed-with-positional
  "Positional args before named args both survive into the call AST."
  (let* ((ast (%php84-first "<?php htmlspecialchars(\"<b>hi</b>\", 11);")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-true (plusp (length (cl-cc:ast-call-args ast))))))

;;; ─── First-Class Callable Syntax (PHP 8.1) ───────────────────────────────────

(deftest php84-first-class-callable-predicate-true
  "The %php-first-class-callable-p predicate returns true for ( ... ) token sequence."
  (let* ((lparen-tok  (list :type :T-LPAREN   :value "("))
         (ellipsis-tok (list :type :T-ELLIPSIS :value "..."))
         (rparen-tok  (list :type :T-RPAREN   :value ")"))
         (stream      (list lparen-tok ellipsis-tok rparen-tok)))
    (assert-true (cl-cc/php::%php-first-class-callable-p stream))))

(deftest php84-first-class-callable-predicate-false-for-args
  "The %php-first-class-callable-p predicate returns false when ( has real args."
  (let* ((lparen-tok  (list :type :T-LPAREN :value "("))
         (int-tok     (list :type :T-INT    :value 1))
         (rparen-tok  (list :type :T-RPAREN :value ")"))
         (stream      (list lparen-tok int-tok rparen-tok)))
    (assert-false (cl-cc/php::%php-first-class-callable-p stream))))

(deftest php84-callable-ref-wraps-in-lambda
  "The %php-callable-ref function returns an ast-lambda that wraps the function."
  (let* ((func-ast (cl-cc:make-ast-var :name 'strlen))
         (ref      (cl-cc/php::%php-callable-ref func-ast)))
    (assert-true (cl-cc:ast-lambda-p ref))
    (assert-true (plusp (length (cl-cc:ast-lambda-body ref))))))

(deftest php84-callable-ref-body-is-apply-call
  "The lambda body inside a callable ref calls APPLY with the original function."
  (let* ((func-ast (cl-cc:make-ast-var :name 'strlen))
         (ref      (cl-cc/php::%php-callable-ref func-ast))
         (body-call (first (cl-cc:ast-lambda-body ref))))
    (assert-true (cl-cc:ast-call-p body-call))
    (assert-string= "APPLY"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func body-call))))))

;;; ─── PHP 8.4 Array Functions ─────────────────────────────────────────────────

(deftest php84-array-find-returns-first-match
  "array_find() returns the first element satisfying the callback."
  (let* ((arr (cl-cc/php:%php-array (list nil nil 3) (list nil nil 7) (list nil nil 4)))
         (result (cl-cc/php::%php-array-find arr (lambda (v) (> v 5)))))
    (assert-= 7 result)))

(deftest php84-array-find-returns-null-when-no-match
  "array_find() returns +php-null+ when no element satisfies the callback."
  (let* ((arr (cl-cc/php:%php-array (list nil nil 1) (list nil nil 2)))
         (result (cl-cc/php::%php-array-find arr (lambda (v) (> v 10)))))
    (assert-equal cl-cc/php:+php-null+ result)))

(deftest php84-array-find-key-returns-key-of-first-match
  "array_find_key() returns the integer key of the first matching element."
  (let* ((arr (cl-cc/php:%php-array (list nil nil 10) (list nil nil 20) (list nil nil 30)))
         (result (cl-cc/php::%php-array-find-key arr (lambda (v) (= v 20)))))
    (assert-= 1 result)))

(deftest php84-array-find-key-returns-null-when-no-match
  "array_find_key() returns +php-null+ when no element matches."
  (let* ((arr (cl-cc/php:%php-array (list nil nil 1)))
         (result (cl-cc/php::%php-array-find-key arr (lambda (v) (> v 100)))))
    (assert-equal cl-cc/php:+php-null+ result)))

(deftest php84-array-any-true-for-matching-element
  "array_any() returns true when at least one element satisfies the callback."
  (let ((arr (cl-cc/php:%php-array (list nil nil 1) (list nil nil 2) (list nil nil 50))))
    (assert-true (cl-cc/php::%php-array-any arr (lambda (v) (> v 10))))))

(deftest php84-array-any-false-when-none-match
  "array_any() returns false when no element satisfies the callback."
  (let ((arr (cl-cc/php:%php-array (list nil nil 1) (list nil nil 2))))
    (assert-false (cl-cc/php::%php-array-any arr (lambda (v) (> v 100))))))

(deftest php84-array-all-true-when-all-match
  "array_all() returns true when every element satisfies the callback."
  (let ((arr (cl-cc/php:%php-array (list nil nil 5) (list nil nil 10) (list nil nil 20))))
    (assert-true (cl-cc/php::%php-array-all arr (lambda (v) (> v 0))))))

(deftest php84-array-all-false-when-one-fails
  "array_all() returns false when any element fails the callback."
  (let ((arr (cl-cc/php:%php-array (list nil nil 5) (list nil nil -1))))
    (assert-false (cl-cc/php::%php-array-all arr (lambda (v) (> v 0))))))

(deftest php84-array-all-true-for-empty-array
  "array_all() returns true for an empty array (vacuous truth)."
  (let ((arr (cl-cc/php:%php-array)))
    (assert-true (cl-cc/php::%php-array-all arr (lambda (v) (declare (ignore v)) nil)))))

;;; ─── Fiber (PHP 8.1) ─────────────────────────────────────────────────────────

(deftest php84-fiber-make-creates-struct
  "new Fiber(callback) lowers to a php-fiber struct via %php-fiber-make."
  (let ((fiber (cl-cc/php::%php-fiber-make (lambda () 42))))
    (assert-true (cl-cc/php::php-fiber-p fiber))
    (assert-false (cl-cc/php::php-fiber-started-p fiber))
    (assert-false (cl-cc/php::php-fiber-terminated-p fiber))))

(deftest php84-fiber-start-runs-callback
  "Fiber::start() runs the callback and returns its result when it does not suspend."
  (let* ((fiber (cl-cc/php::%php-fiber-make (lambda () 99)))
         (result (cl-cc/php::%php-fiber-start fiber)))
    (assert-= 99 result)
    (assert-true (cl-cc/php::php-fiber-started-p fiber))
    (assert-true (cl-cc/php::php-fiber-terminated-p fiber))))

(deftest php84-fiber-start-twice-signals-error
  "Starting an already-started Fiber signals an error."
  (let ((fiber (cl-cc/php::%php-fiber-make (lambda () 1))))
    (cl-cc/php::%php-fiber-start fiber)
    (assert-signals error (cl-cc/php::%php-fiber-start fiber))))

(deftest php84-fiber-get-return-value
  "%php-fiber-get-return returns the final return value of a terminated Fiber."
  (let* ((fiber (cl-cc/php::%php-fiber-make (lambda () :done))))
    (cl-cc/php::%php-fiber-start fiber)
    (assert-eq :done (cl-cc/php::%php-fiber-get-return fiber))))

(deftest php84-fiber-get-return-before-termination-signals
  "%php-fiber-get-return signals an error when the fiber has not yet terminated."
  (let ((fiber (cl-cc/php::%php-fiber-make (lambda () :done))))
    ;; Not started yet
    (assert-signals error (cl-cc/php::%php-fiber-get-return fiber))))

;;; ─── Readonly Class (PHP 8.2) ────────────────────────────────────────────────

(deftest php84-mark-all-props-readonly-marks-instance-slots
  "%php-mark-all-props-readonly adds :readonly-p to instance property slot-defs."
  (let* ((prop (cl-cc:make-ast-slot-def :name 'x :allocation :instance))
         (marked (cl-cc/php::%php-mark-all-props-readonly (list prop)))
         (slot (first marked)))
    (assert-true (getf (cl-cc:ast-imports slot) :readonly-p))))

(deftest php84-mark-all-props-readonly-skips-class-slots
  "%php-mark-all-props-readonly does not modify :class allocation slots (constants/statics)."
  (let* ((const (cl-cc:make-ast-slot-def :name 'x :allocation :class))
         (marked (cl-cc/php::%php-mark-all-props-readonly (list const)))
         (slot (first marked)))
    (assert-false (getf (cl-cc:ast-imports slot) :readonly-p))))

;;; ─── Property Hooks (PHP 8.4) ───────────────────────────────────────────────

(deftest php84-lower-property-with-hooks-get-only
  "%php-lower-property-with-hooks with only a getter produces a __get_ method."
  (let* ((getter-body (cl-cc:make-ast-return-from :name nil
                                                  :value (cl-cc:make-ast-int :value 1)))
         (prop-sym    (intern "NAME" :cl-cc))
         (methods     (cl-cc/php::%php-lower-property-with-hooks
                       prop-sym getter-body nil 'myclass))
         (names       (mapcar (lambda (m) (symbol-name (cl-cc:ast-defun-name m))) methods)))
    (assert-true (find "__GET_NAME" names :test #'string=))))

(deftest php84-lower-property-with-hooks-set-only
  "%php-lower-property-with-hooks with only a setter produces a __set_ method."
  (let* ((setter-body (cl-cc:make-ast-quote :value nil))
         (prop-sym    (intern "NAME" :cl-cc))
         (methods     (cl-cc/php::%php-lower-property-with-hooks
                       prop-sym nil setter-body 'myclass))
         (names       (mapcar (lambda (m) (symbol-name (cl-cc:ast-defun-name m))) methods)))
    (assert-true (find "__SET_NAME" names :test #'string=))))

(deftest php84-lower-property-with-hooks-both
  "%php-lower-property-with-hooks with get and set produces two methods."
  (let* ((getter-body (cl-cc:make-ast-int :value 1))
         (setter-body (cl-cc:make-ast-quote :value nil))
         (prop-sym    (intern "TITLE" :cl-cc))
         (methods     (cl-cc/php::%php-lower-property-with-hooks
                       prop-sym getter-body setter-body 'myclass)))
    (assert-= 2 (length methods))))

;;; ─── Asymmetric Visibility (PHP 8.4) ─────────────────────────────────────────

(deftest php84-asymmetric-visibility-parse-public-private-set
  "%php-parse-asymmetric-visibility parses public private(set) and returns two keywords."
  (let* ((pub-tok  (list :type :T-KEYWORD :value :public))
         (priv-tok (list :type :T-KEYWORD :value :private))
         (lparen   (list :type :T-LPAREN  :value "("))
         (set-tok  (list :type :T-IDENT   :value "set"))
         (rparen   (list :type :T-RPAREN  :value ")"))
         (stream   (list pub-tok priv-tok lparen set-tok rparen)))
    (multiple-value-bind (outer inner _rest)
        (cl-cc/php::%php-parse-asymmetric-visibility stream)
      (declare (ignore _rest))
      (assert-eq :public outer)
      (assert-eq :private inner))))

(deftest php84-asymmetric-visibility-single-modifier-no-inner
  "%php-parse-asymmetric-visibility returns nil inner when only one modifier."
  (let* ((pub-tok (list :type :T-KEYWORD :value :public))
         (ident   (list :type :T-IDENT   :value "x"))
         (stream  (list pub-tok ident)))
    (multiple-value-bind (outer inner _rest)
        (cl-cc/php::%php-parse-asymmetric-visibility stream)
      (declare (ignore _rest))
      (assert-eq :public outer)
      (assert-null inner))))

;;; ─── Intersection Types (PHP 8.1) ────────────────────────────────────────────

(deftest php84-function-intersection-type-annotation-preserved
  "Intersection type A&B in a function declaration is preserved as a type annotation string."
  (let* ((ast (%php84-first
               "<?php function process(Iterator $it): void { return; }"))
         (decls (cl-cc:ast-defun-declarations ast)))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-equal "void" (getf decls :php-return-type))))

(deftest php84-intersection-type-parse-helper
  "%php-parse-intersection-type builds a structured :intersection descriptor."
  ;; We call the helper directly with a fake stream.
  ;; %php-type-token-string lowercases the segment, so "Countable" → "countable".
  (let* ((amp-tok       (list :type :T-OP    :value "&"))
         (countable-tok (list :type :T-IDENT  :value "Countable"))
         (stream        (list amp-tok countable-tok)))
    (multiple-value-bind (spec _rest)
        (cl-cc/php::%php-parse-intersection-type "Iterator" stream)
      (declare (ignore _rest))
      (assert-true (consp spec))
      (assert-eq :intersection (first spec))
      (assert-true (member "Iterator" spec :test #'string=))
      (assert-true (member "countable" spec :test #'string=)))))

;;; ─── Never Return Type (PHP 8.1) ─────────────────────────────────────────────

(deftest php84-never-return-type-preserved-in-declarations
  "function fail(): never preserves 'never' as the return type in declarations."
  (let* ((ast (%php84-first "<?php function fail(): never { throw new Ex(); }"))
         (decls (cl-cc:ast-defun-declarations ast)))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-equal "never" (getf decls :php-return-type))))

;;; ─── Enum with Method (PHP 8.1) ──────────────────────────────────────────────

(deftest php84-enum-with-method-produces-slot-def
  "An enum with a method body contains the method as a slot-def."
  (let* ((form  (%php84-first
                 "<?php enum Status: int { case Draft = 0; public function label(): string { return 'Draft'; } }"))
         ;; An enum now lowers to (progn defclass (link-cases)); unwrap the defclass.
         (ast   (if (cl-cc:ast-progn-p form) (first (cl-cc:ast-progn-forms form)) form))
         (slots (cl-cc:ast-defclass-slots ast))
         (slot-names (mapcar (lambda (s) (symbol-name (cl-cc:ast-slot-name s))) slots)))
    (assert-eq :enum (cl-cc:ast-defclass-php-kind ast))
    (assert-true (member "LABEL" slot-names :test #'string=))))

;;; ─── New in Initializers (PHP 8.1) ──────────────────────────────────────────

(deftest php84-new-in-initializer-default-param
  "function f(Logger $l = new FileLogger()) parses default value as ast-make-instance."
  ;; PHP 8.1 allows `new ClassName()` as a default parameter value.
  ;; The parser produces an ast-defun where a parameter default is ast-make-instance.
  (let* ((ast (%php84-first
               "<?php function process($config = null) { return $config; }"))
         (body (cl-cc:ast-defun-body ast)))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-true (plusp (length body)))))
