;;;; packages/javascript/tests/js-runtime-symbol-tests.lisp
;;;;
;;;; Unit tests for runtime-symbol.lisp: Symbol primitive, global registry
;;;; (Symbol.for / keyFor), well-known symbols, and Symbol-as-property-key.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr, %jr-list)

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Symbol predicate ────────────────────────────────────────────────────────

(deftest-each js-rt-symbol-p
  "%js-symbol-p returns t only for js-symbol structs."
  :cases (("symbol"   (cl-cc/javascript::%js-make-symbol "x") t)
          ("string"   "sym"                                    nil)
          ("number"   42                                       nil)
          ("nil"      nil                                      nil))
  (val expected)
  (assert-equal expected (cl-cc/javascript::%js-symbol-p val)))

;;; ─── Symbol creation ─────────────────────────────────────────────────────────

(deftest js-rt-symbol-unique-identity
  "Two Symbol() calls with the same description are never eq."
  (let ((a (cl-cc/javascript::%js-make-symbol "tag"))
        (b (cl-cc/javascript::%js-make-symbol "tag")))
    (assert-false (eq a b))))

(deftest js-rt-symbol-no-description
  "Symbol() with no arg stores +js-undefined+ as description."
  (let ((sym (cl-cc/javascript::%js-make-symbol)))
    (assert-true  (cl-cc/javascript::%js-symbol-p sym))
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-symbol-description sym))))

;;; ─── toString / description ──────────────────────────────────────────────────

(deftest-each js-rt-symbol-to-string
  "Symbol.prototype.toString renders 'Symbol(desc)' or 'Symbol()'."
  :cases (("with-desc"    "tag"                                "Symbol(tag)")
          ("empty-desc"   ""                                   "Symbol()")
          ("no-desc"      cl-cc/javascript::+js-undefined+     "Symbol()"))
  (desc expected)
  (let* ((sym (cl-cc/javascript::%js-make-symbol desc))
         (str (cl-cc/javascript::%js-symbol-to-string sym)))
    (assert-string= expected str)))

(deftest js-rt-symbol-description-accessor
  "Symbol.prototype.description returns the description string or +js-undefined+."
  (let ((with-desc (cl-cc/javascript::%js-make-symbol "label"))
        (no-desc   (cl-cc/javascript::%js-make-symbol)))
    (assert-string= "label" (cl-cc/javascript::%js-symbol-description with-desc))
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-symbol-description no-desc))))

;;; ─── Global registry — Symbol.for / Symbol.keyFor ────────────────────────────

(deftest js-rt-symbol-for-idempotent
  "Symbol.for with the same key returns the identical symbol object."
  (let ((s1 (cl-cc/javascript::%js-symbol-for "my-key"))
        (s2 (cl-cc/javascript::%js-symbol-for "my-key")))
    (assert-eq s1 s2)))

(deftest js-rt-symbol-for-different-keys
  "Symbol.for with different keys returns distinct symbol objects."
  (let ((a (cl-cc/javascript::%js-symbol-for "alpha"))
        (b (cl-cc/javascript::%js-symbol-for "beta")))
    (assert-false (eq a b))))

(deftest js-rt-symbol-key-for-registered
  "Symbol.keyFor returns the registry key for a registered symbol."
  (let ((sym (cl-cc/javascript::%js-symbol-for "registry-key")))
    (assert-string= "registry-key"
                    (cl-cc/javascript::%js-symbol-key-for sym))))

(deftest js-rt-symbol-key-for-unregistered
  "Symbol.keyFor returns +js-undefined+ for non-registry symbols."
  (let ((local (cl-cc/javascript::%js-make-symbol "local")))
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-symbol-key-for local))))

;;; ─── Symbol as property key ──────────────────────────────────────────────────

(deftest js-rt-symbol-as-key-format
  "%js-symbol-as-key produces a '__sym_GENSYM__' string."
  (let* ((sym (cl-cc/javascript::%js-make-symbol "k"))
         (key (cl-cc/javascript::%js-symbol-as-key sym)))
    (assert-true (stringp key))
    (assert-true (string= key "__sym_" :end1 6))
    (assert-true (string= key "__" :start1 (- (length key) 2)))))

;;; ─── Well-known symbols ──────────────────────────────────────────────────────

(deftest js-rt-well-known-symbols-are-registered
  "Well-known symbols (iterator, toPrimitive, etc.) are js-symbol instances."
  (assert-true (cl-cc/javascript::%js-symbol-p cl-cc/javascript::%js-symbol-iterator))
  (assert-true (cl-cc/javascript::%js-symbol-p cl-cc/javascript::%js-symbol-to-primitive))
  (assert-true (cl-cc/javascript::%js-symbol-p cl-cc/javascript::%js-symbol-to-string-tag)))

;;; ─── Symbol global object ────────────────────────────────────────────────────

(deftest js-rt-symbol-global-has-well-known-props
  "*js-symbol-global* exposes well-known symbols and static methods."
  (let ((g cl-cc/javascript::*js-symbol-global*))
    (assert-true (cl-cc/javascript::%js-ht-p g))
    (assert-true (cl-cc/javascript::%js-symbol-p (gethash "iterator" g)))
    (assert-true (functionp (gethash "for" g)))
    (assert-true (functionp (gethash "__call__" g)))))

;;; ─── Iterator control: iter-values / iter-keys / advance-iterator ────────────

(deftest js-rt-iter-values-from-array
  "%js-iter-values collects a JS array's elements into a CL list."
  (let* ((arr    (%jr-arr 10 20 30))
         (values (cl-cc/javascript::%js-iter-values arr)))
    (assert-equal '(10 20 30) values)))

(deftest js-rt-iter-values-from-string
  "%js-iter-values collects individual characters from a string."
  (assert-equal '("a" "b" "c") (cl-cc/javascript::%js-iter-values "abc")))

(deftest js-rt-advance-iterator-drains-object
  "%js-advance-iterator calls body-fn for each {value, done} step."
  (let* ((items '(7 8 9))
         (idx   (list 0))
         (iter  (cl-cc/javascript::%js-make-object
                 "next" (lambda ()
                           (if (< (car idx) (length items))
                               (prog1 (cl-cc/javascript::%js-make-object
                                       "value" (nth (car idx) items) "done" nil)
                                 (incf (car idx)))
                               (cl-cc/javascript::%js-make-object
                                "value" cl-cc/javascript::+js-undefined+ "done" t)))))
         (acc   nil))
    (cl-cc/javascript::%js-advance-iterator iter (lambda (v) (push v acc)))
    (assert-equal '(9 8 7) acc)))
