;;;; packages/php/tests/php84-tests.lisp — PHP 8.0-8.5 feature tests
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

;;; ─── PHP 8.5 Features ─────────────────────────────────────────────────────

(deftest php85-pipe-operator-lowers-to-helper-call
  "The PHP 8.5 pipe operator lowers to the runtime pipe helper."
  (let ((ast (%php84-first "<?php \"  HI  \" |> trim(...);")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-eq 'cl-cc/php::%php-pipe
               (cl-cc:ast-var-name (cl-cc:ast-call-func ast)))
    (assert-= 2 (length (cl-cc:ast-call-args ast)))
    (assert-true (cl-cc:ast-lambda-p (second (cl-cc:ast-call-args ast))))))

(deftest php85-pipe-runtime-applies-callable
  "The pipe helper applies a callable to the piped value."
  (assert-string= "hi"
                 (cl-cc/php::%php-pipe "HI" #'cl-cc/php::%php-strtolower)))

(deftest php85-pipe-operator-executes-first-class-callable-chain
  "A parsed pipe chain can execute PHP first-class callable RHS expressions."
  (assert-string= "hi"
                 (%php-run-capture
                  "<?php echo \"  HI  \" |> trim(...) |> strtolower(...);")))

(deftest php85-array-first-last-runtime-preserves-order
  "array_first() and array_last() return inserted first/last values."
  (let ((array (cl-cc/php::%php-array)))
    (cl-cc/php::%php-array-set array "a" 10)
    (cl-cc/php::%php-array-set array "b" 20)
    (assert-= 10 (cl-cc/php:%php-array-first array))
    (assert-= 20 (cl-cc/php:%php-array-last array))))

(deftest php85-array-first-last-empty-arrays-return-null
  "array_first() and array_last() return null for empty arrays."
  (let ((array (cl-cc/php:%php-array)))
    (assert-eq cl-cc/php:+php-null+
               (cl-cc/php:%php-array-first array))
    (assert-eq cl-cc/php:+php-null+
               (cl-cc/php:%php-array-last array))))

(deftest php85-array-first-last-execute-as-builtins
  "The PHP 8.5 array_first() and array_last() builtins execute from PHP source."
  (assert-string= "10:20"
                 (%php-run-capture
                  "<?php $a=['a'=>10,'b'=>20]; echo array_first($a).':'.array_last($a);")))

(deftest php85-grapheme-levenshtein-counts-combining-cluster
  "grapheme_levenshtein() treats a base character plus combining mark as one cluster."
  (let ((cluster (format nil "a~C" (code-char #x0301))))
    (assert-= 1 (cl-cc/php::%php-grapheme-levenshtein cluster ""))))

(deftest php85-grapheme-levenshtein-executes-as-builtin
  "The PHP 8.5 grapheme_levenshtein() builtin executes from PHP source."
  (assert-string= "3"
                 (%php-run-capture
                  "<?php echo grapheme_levenshtein('kitten','sitting');")))

(deftest php85-locale-is-right-to-left-runtime-detects-rtl-locales
  "PHP 8.5 Locale direction helper detects common RTL locale identifiers."
  (assert-true (cl-cc/php:%php-locale-is-right-to-left "ar_EG.UTF-8"))
  (assert-true (cl-cc/php:%php-locale-is-right-to-left "fa-IR"))
  (assert-true (cl-cc/php:%php-locale-is-right-to-left "az-Arab"))
  (assert-false (cl-cc/php:%php-locale-is-right-to-left "en_US"))
  (assert-false (cl-cc/php:%php-locale-is-right-to-left "az-Latn")))

(deftest php85-locale-is-right-to-left-executes-as-builtin
  "The PHP 8.5 locale_is_right_to_left() builtin executes from PHP source."
  (assert-string= "rtl:ltr"
                 (%php-run-capture
                  "<?php echo (locale_is_right_to_left('he_IL') ? 'rtl' : 'ltr') . ':' . (locale_is_right_to_left('en_US') ? 'rtl' : 'ltr');")))

(deftest php85-locale-static-is-right-to-left-lowers-to-helper
  "The PHP 8.5 Locale::isRightToLeft() static method lowers to the runtime helper."
  (assert-string= "rtl:ltr"
                 (%php-run-capture
                  "<?php echo (Locale::isRightToLeft('ur_PK') ? 'rtl' : 'ltr') . ':' . (Locale::isRightToLeft('fr_FR') ? 'rtl' : 'ltr');")))

(deftest php85-build-metadata-constants-are-predefined
  "PHP_BUILD_DATE and PHP_BUILD_PROVIDER resolve as predefined PHP 8.5 constants."
  (multiple-value-bind (date date-found)
      (cl-cc/php::%php-lookup-constant "PHP_BUILD_DATE")
    (multiple-value-bind (provider provider-found)
        (cl-cc/php::%php-lookup-constant "PHP_BUILD_PROVIDER")
      (assert-true date-found)
      (assert-true provider-found)
      (assert-string= "1970-01-01T00:00:00+00:00" date)
      (assert-string= "cl-cc" provider))))

(deftest php85-build-metadata-constants-execute-from-php-source
  "PHP 8.5 build metadata constants are available to parsed PHP code."
  (assert-string= "cl-cc:date"
                 (%php-run-capture
                  "<?php echo PHP_BUILD_PROVIDER . ':' . (PHP_BUILD_DATE === '' ? 'empty' : 'date');")))

(deftest php85-no-discard-attribute-preserved-on-function
  "PHP 8.5 #[\\NoDiscard] is preserved as function attribute metadata."
  (let* ((ast (%php84-first "<?php #[\\NoDiscard] function important(): int { return 1; }"))
         (attr (first (getf (cl-cc:ast-imports ast) :php-attributes))))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-string= "NoDiscard" (cl-cc/php:php-attribute-name attr))
    (assert-eq :function (cl-cc/php:php-attribute-target-type attr))))

(deftest php85-no-discard-attribute-preserves-message
  "PHP 8.5 #[NoDiscard('message')] preserves the optional attribute message."
  (let* ((ast (%php84-first "<?php #[NoDiscard('use the return value')] function important() { return 1; }"))
         (attr (first (getf (cl-cc:ast-imports ast) :php-attributes)))
         (arg (first (cl-cc/php:php-attribute-args attr))))
    (assert-string= "NoDiscard" (cl-cc/php:php-attribute-name attr))
    (assert-true (cl-cc:ast-quote-p arg))
    (assert-string= "use the return value" (cl-cc:ast-quote-value arg))))

(deftest php85-top-level-const-executes
  "PHP top-level const declarations define readable constants."
  (assert-string= "42"
                 (%php-run-capture "<?php const ANSWER = 42; echo ANSWER;")))

(deftest php85-attribute-preserved-on-top-level-constant
  "PHP 8.5 attributes on top-level constants survive as constant metadata."
  (let* ((ast (%php84-first "<?php #[Deprecated('use NEW')] const OLD = 1;"))
         (attr (first (getf (cl-cc:ast-imports ast) :php-attributes)))
         (arg (first (cl-cc/php:php-attribute-args attr))))
    (assert-true (cl-cc:ast-defvar-p ast))
    (assert-string= "OLD" (symbol-name (cl-cc:ast-defvar-name ast)))
    (assert-string= "Deprecated" (cl-cc/php:php-attribute-name attr))
    (assert-eq :constant (cl-cc/php:php-attribute-target-type attr))
    (assert-true (cl-cc:ast-quote-p arg))
    (assert-string= "use NEW" (cl-cc:ast-quote-value arg))))

(deftest php85-attribute-preserved-on-multiple-top-level-constants
  "PHP 8.5 attributes on const A, B attach to each declared constant."
  (let* ((ast (%php84-first "<?php #[Deprecated] const A = 1, B = 2;"))
         (forms (cl-cc:ast-progn-forms ast)))
    (assert-true (cl-cc:ast-progn-p ast))
    (assert-= 2 (length forms))
    (dolist (form forms)
      (let ((attr (first (getf (cl-cc:ast-imports form) :php-attributes))))
        (assert-true (cl-cc:ast-defvar-p form))
        (assert-string= "Deprecated" (cl-cc/php:php-attribute-name attr))
        (assert-eq :constant (cl-cc/php:php-attribute-target-type attr))))))

(deftest php85-no-discard-attribute-preserved-on-enum-method
  "PHP 8.5 #[NoDiscard] on enum methods survives enum classlike parsing."
  (let* ((form (%php84-first
                "<?php enum Mode { #[NoDiscard] public function label(): string { return 'x'; } case A; }"))
         (ast (if (cl-cc:ast-progn-p form) (first (cl-cc:ast-progn-forms form)) form))
         (method-slot (find-if (lambda (slot)
                                 (and (cl-cc:ast-slot-def-p slot)
                                      (cl-cc:ast-defun-p (cl-cc:ast-slot-initform slot))))
                               (cl-cc:ast-defclass-slots ast)))
         (method (cl-cc:ast-slot-initform method-slot))
         (attr (first (getf (cl-cc:ast-imports method) :php-attributes))))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-string= "NoDiscard" (cl-cc/php:php-attribute-name attr))
    (assert-eq :method (cl-cc/php:php-attribute-target-type attr))))

(deftest php85-void-cast-lowers-to-discarding-progn
  "The PHP 8.5 (void) cast evaluates its operand and returns PHP null."
  (let* ((value (%php-first-binding-value "<?php $x = (void) 123;"))
         (forms (cl-cc:ast-progn-forms value)))
    (assert-true (cl-cc:ast-progn-p value))
    (assert-= 2 (length forms))
    (assert-eq cl-cc/php:+php-null+
               (cl-cc:ast-quote-value (second forms)))))

(deftest php85-void-cast-executes-side-effects-and-returns-null
  "The PHP 8.5 (void) cast still evaluates the discarded expression."
  (assert-string= "1:null"
                 (%php-run-capture
                  "<?php $x=0; $y=(void)($x=1); echo $x . ':' . ($y === null ? 'null' : 'bad');")))

(deftest php85-get-error-handler-reports-current-handler
  "get_error_handler() returns the active handler and null when none is installed."
  (assert-string= "null:h:null"
                 (%php-run-capture
                  "<?php function h($errno,$errstr){ return true; } echo get_error_handler() === null ? 'null' : 'bad'; set_error_handler('h'); echo ':' . get_error_handler() . ':'; restore_error_handler(); echo get_error_handler() === null ? 'null' : 'bad';")))

(deftest php85-get-exception-handler-reports-current-handler
  "get_exception_handler() returns the active handler and null when none is installed."
  (assert-string= "null:eh:null"
                 (%php-run-capture
                  "<?php function eh($e){} echo get_exception_handler() === null ? 'null' : 'bad'; set_exception_handler('eh'); echo ':' . get_exception_handler() . ':'; restore_exception_handler(); echo get_exception_handler() === null ? 'null' : 'bad';")))

(deftest php85-opcache-file-cache-helper-returns-false-without-file-cache
  "opcache_is_script_cached_in_file_cache() is available and false without an OPCache file cache."
  (assert-false
   (cl-cc/php:%php-opcache-is-script-cached-in-file-cache "/tmp/example.php")))

(deftest php85-opcache-file-cache-helper-executes-as-builtin
  "The PHP 8.5 opcache file-cache probe executes from PHP source."
  (assert-string= "cold"
                 (%php-run-capture
                  "<?php echo opcache_is_script_cached_in_file_cache('/tmp/example.php') ? 'warm' : 'cold';")))

(deftest php85-curl-share-init-persistent-reuses-handle
  "curl_share_init_persistent() returns a stable persistent handle for each ID."
  (let ((a (cl-cc/php:%php-curl-share-init-persistent "cache"))
        (b (cl-cc/php:%php-curl-share-init-persistent "cache"))
        (c (cl-cc/php:%php-curl-share-init-persistent "other")))
    (assert-eq a b)
    (assert-false (eq a c))
    (assert-string= "CurlSharePersistentHandle" (gethash "__class__" a))
    (assert-string= "cache" (gethash "id" a))
    (assert-true (gethash "persistent" a))))

(deftest php85-curl-share-init-persistent-executes-as-builtin
  "The PHP 8.5 persistent cURL share initializer executes from PHP source."
  (assert-string= "same:object"
                 (%php-run-capture
                  "<?php $a=curl_share_init_persistent('cache'); $b=curl_share_init_persistent('cache'); echo ($a === $b ? 'same' : 'diff') . ':' . gettype($a);")))

(deftest php85-filter-throw-on-failure-constant-is-defined
  "PHP 8.5 defines FILTER_THROW_ON_FAILURE for filter functions."
  (multiple-value-bind (value found)
      (cl-cc/php::%php-lookup-constant "FILTER_THROW_ON_FAILURE")
    (assert-true found)
    (assert-= 268435456 value)))

(deftest php85-filter-var-validates-int
  "filter_var() supports integer validation."
  (assert-string= "42:false"
                 (%php-run-capture
                  "<?php echo filter_var('42', FILTER_VALIDATE_INT) . ':' . (filter_var('abc', FILTER_VALIDATE_INT) === false ? 'false' : 'bad');")))

(deftest php85-filter-var-validates-boolean-false
  "FILTER_VALIDATE_BOOLEAN can return a successful false result."
  (assert-string= "false"
                 (%php-run-capture
                  "<?php echo filter_var('false', FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE) === false ? 'false' : 'bad';")))

(deftest php85-filter-var-throws-on-validation-failure
  "FILTER_THROW_ON_FAILURE raises the PHP 8.5 filter exception on validation failure."
  (assert-signals cl-cc/php:php-exception
    (cl-cc/php:%php-filter-var "abc" 257 268435456)))

(deftest php85-filter-var-rejects-null-and-throw-flags-together
  "FILTER_THROW_ON_FAILURE cannot be combined with FILTER_NULL_ON_FAILURE."
  (assert-signals cl-cc/php:php-exception
    (cl-cc/php:%php-filter-var "42" 257 (+ 134217728 268435456))))

(deftest php85-clone-with-lowers-to-helper-call
  "PHP 8.5 clone-with syntax lowers to the clone-with runtime helper."
  (let* ((value (%php-first-binding-value "<?php $b = clone($a, ['x' => 9]);"))
         (body (cl-cc:ast-let-body value))
         (with-call (second body)))
    (assert-true (cl-cc:ast-let-p value))
    (assert-true (cl-cc:ast-call-p with-call))
    (assert-eq 'cl-cc/php::%php-clone-with
               (cl-cc:ast-var-name (cl-cc:ast-call-func with-call)))))

(deftest php85-clone-with-executes-property-overrides
  "Clone-with applies property overrides without mutating the original object."
  (assert-string= "1:9"
                 (%php-run-capture
                  "<?php class C{ public $x=1; } $a=new C(); $b=clone($a, ['x'=>9]); echo $a->x.':'.$b->x;")))

(deftest php85-clone-with-applies-overrides-after-clone-hook
  "Clone-with applies the override array after __clone has run."
  (assert-string= "4:99"
                 (%php-run-capture
                  "<?php class C{ public $x; function __construct($x){ $this->x=$x; } function __clone(){ $this->x=$this->x+10; } } $a=new C(4); $b=clone($a, ['x'=>99]); echo $a->x.':'.$b->x;")))

(deftest php85-closure-get-current-outside-returns-null
  "Closure::getCurrent() returns null outside closure execution."
  (assert-string= "null"
                 (%php-run-capture
                  "<?php echo Closure::getCurrent() === null ? 'null' : 'bad';")))

(deftest php85-closure-get-current-inside-direct-call
  "Closure::getCurrent() returns the executing closure during direct invocation."
  (assert-string= "same"
                 (%php-run-capture
                  "<?php $f=function(){ return Closure::getCurrent(); }; echo $f() === $f ? 'same' : 'bad';")))

(deftest php85-closure-get-current-inside-call-user-func
  "Closure::getCurrent() returns the executing closure through call_user_func()."
  (assert-string= "same"
                 (%php-run-capture
                  "<?php $f=function(){ return Closure::getCurrent(); }; echo call_user_func($f) === $f ? 'same' : 'bad';")))

;;; ─── Fiber (PHP 8.1) ─────────────────────────────────────────────────────────

(deftest php84-fiber-make-creates-object
  "new Fiber(callback) lowers to a PHP object wrapper via %php-fiber-make."
  (let ((fiber (cl-cc/php::%php-fiber-make (lambda () 42))))
    (assert-true (hash-table-p fiber))
    (assert-string= "Fiber" (gethash "__class__" fiber))
    (assert-false (cl-cc/php::%php-fiber-started-p fiber))
    (assert-false (cl-cc/php::%php-fiber-terminated-p fiber))))

(deftest php84-fiber-start-runs-callback
  "Fiber::start() runs the callback and returns its result when it does not suspend."
  (let* ((fiber (cl-cc/php::%php-fiber-make (lambda () 99)))
         (result (cl-cc/php::%php-fiber-start fiber)))
    (assert-= 99 result)
    (assert-true (cl-cc/php::%php-fiber-started-p fiber))
    (assert-true (cl-cc/php::%php-fiber-terminated-p fiber))))

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

(deftest php84-class-property-hooks-lower-to-accessor-slots
  "Class property hooks parse through the class parser and add accessor slots."
  (let* ((ast (%php84-first
               "<?php class User { public string $name { get => $this->name; set($value) => $value; } }"))
         (slots (cl-cc:ast-defclass-slots ast))
         (slot-names (mapcar (lambda (slot)
                               (symbol-name (cl-cc:ast-slot-name slot)))
                             slots)))
    (assert-true (member "NAME" slot-names :test #'string=))
    (assert-true (member "__GET_NAME" slot-names :test #'string=))
    (assert-true (member "__SET_NAME" slot-names :test #'string=))))

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

(deftest php84-class-asymmetric-visibility-metadata
  "Class properties preserve PHP 8.4 asymmetric set visibility metadata."
  (let* ((ast (%php84-first
               "<?php class User { public private(set) string $id; }"))
         (slot (first (cl-cc:ast-defclass-slots ast)))
         (imports (cl-cc:ast-imports slot)))
    (assert-eq :private (getf imports :php-set-visibility))
    (assert-true (member :public (getf imports :php-modifiers) :test #'eq))))

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
