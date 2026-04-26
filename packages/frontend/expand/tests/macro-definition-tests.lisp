;;;; tests/unit/expand/macro-definition-tests.lisp — Macro definition tests

(in-package :cl-cc/test)

(defsuite macro-definition-suite
  :description "Macro definition expansion tests"
  :parent cl-cc-unit-suite)


(in-suite macro-definition-suite)
(deftest-each defun-macro-structure
  "DEFUN expands to (setf (fdefinition ...) (lambda ...)) regardless of docstring"
  :cases (("basic"          '(defun foo (x y) body1 body2))
          ("with-docstring" '(defun foo (x y) "Docstring" body)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'setf)
    (assert-equal (cadr result) '(fdefinition 'foo))
    (assert-eq (caaddr result) 'lambda)))

(deftest define-compiler-macro-returns-name
  "DEFINE-COMPILER-MACRO returns the macro name (no compile-time expansion)."
  (let ((result (our-macroexpand-1 '(define-compiler-macro foo (x) (+ x 1)))))
    (assert-equal '(quote foo) result)))

(deftest define-compiler-macro-expands-call
  "DEFINE-COMPILER-MACRO registers a compiler macro used by compiler-macroexpand-all."
  (our-macroexpand-1 '(define-compiler-macro foo (x) (+ x 1)))
  (let ((result (cl-cc/expand::compiler-macroexpand-all '(foo 2))))
    (assert-equal result 3)))

(deftest invoke-registered-expander-supports-descriptor-backed-compiler-macros
  "Descriptor-backed compiler macro expanders still execute through invoke-registered-expander."
  (let ((expander (cl-cc/expand::make-compiler-macro-expander '(x) '((+ x 1)))))
    (assert-equal 3
                  (cl-cc/expand::invoke-registered-expander expander '(foo 2) nil))))

;;; ─── %contains-uninterned-symbol-p ──────────────────────────────────────

(deftest-each contains-uninterned-symbol-p-cases
  "%contains-uninterned-symbol-p: T for gensyms/uninterned; nil for normal symbols and atoms."
  :cases (("gensym"          t   (list (gensym "G")))
          ("normal-symbol"   nil '(foo bar))
          ("integer"         nil 42)
          ("string"          nil "hello")
          ("nested-gensym"   t   nil))
  (expected form)
  (let ((test-form (if (eq form nil)
                       (list 'quote (list (gensym "NESTED")))
                       form)))
    (if expected
        (assert-true  (cl-cc/expand::%contains-uninterned-symbol-p test-form))
        (assert-false (cl-cc/expand::%contains-uninterned-symbol-p test-form)))))

;;; ─── %cacheable-macroexpansion-p ─────────────────────────────────────────

(deftest-each cacheable-macroexpansion-p-cases
  "%cacheable-macroexpansion-p: NIL for forms with gensyms; T for fully interned forms."
  :cases (("interned-form"   t   '(+ 1 2))
          ("keyword-form"    t   '(:x :y))
          ("gensym-form"     nil nil))
  (expected form)
  (let ((test-form (if (null form)
                       (list (gensym "G") 1 2)
                       form)))
    (if expected
        (assert-true  (cl-cc/expand::%cacheable-macroexpansion-p test-form))
        (assert-false (cl-cc/expand::%cacheable-macroexpansion-p test-form)))))

;;; ─── %expander-descriptor-p ──────────────────────────────────────────────

(deftest-each expander-descriptor-p-cases
  "%expander-descriptor-p: T for valid descriptor plists; NIL for functions and non-lists."
  :cases (("macro-kind"      t   (list :kind :macro-expander :lambda-list '() :body '()))
          ("compiler-kind"   t   (list :kind :compiler-macro-expander :lambda-list '() :body '()))
          ("function"        nil #'identity)
          ("bare-list"       nil '(foo bar)))
  (expected object)
  (if expected
      (assert-true  (cl-cc/expand::%expander-descriptor-p object))
      (assert-false (cl-cc/expand::%expander-descriptor-p object))))
