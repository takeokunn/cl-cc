;;;; tests/unit/expand/macro-definition-tests.lisp — Macro definition tests

(in-package :cl-cc/test)

(defsuite macro-definition-suite
  :description "Macro definition expansion tests"
  :parent cl-cc-serial-suite)


(defbefore :each (macro-definition-suite)
  (clrhash cl-cc/expand:*compiler-macro-table*)
  (setf cl-cc/expand:*macro-eval-fn* #'eval))


(in-suite macro-definition-suite)
(deftest-each defun-macro-structure
  "DEFUN expands to fdefinition assignment and preserves its implicit named block."
  :cases (("basic"          '(defun foo (x y) body1 body2) '(block foo body1 body2))
           ("with-docstring" '(defun foo (x y) "Docstring" body) '(block foo body)))
  (form expected-block)
  (let* ((result (our-macroexpand-1 form))
         (lambda-form (third result)))
    (assert-eq (car result) 'setf)
    (assert-equal (cadr result) '(fdefinition 'foo))
    (assert-eq (car lambda-form) 'lambda)
    (assert-true (member expected-block (cddr lambda-form) :test #'equal))))

(deftest defun-macro-block-follows-docstring-and-declarations
  "DEFUN macro keeps docstring/declarations in lambda body and wraps executable forms in BLOCK."
  (let* ((result (our-macroexpand-1 '(defun foo (x) "Doc" (declare (ignore x)) (return-from foo 1))))
         (lambda-form (third result)))
    (assert-equal "Doc" (third lambda-form))
    (assert-equal '(declare (ignore x)) (fourth lambda-form))
    (assert-equal '(block foo (return-from foo 1)) (fifth lambda-form))))

(deftest define-compiler-macro-returns-name
  "DEFINE-COMPILER-MACRO returns the macro name (no compile-time expansion)."
  (let ((result (our-macroexpand-1 '(define-compiler-macro foo (x) (+ x 1)))))
    (assert-equal '(quote foo) result)))

(deftest define-compiler-macro-expands-call
  "DEFINE-COMPILER-MACRO registers a compiler macro used by compiler-macroexpand-all."
  (let ((name (gensym "CM-FOO-")))
    (our-macroexpand-1 `(define-compiler-macro ,name (x) (+ x 1)))
    (let ((result (cl-cc/expand:compiler-macroexpand-all `(,name 2))))
      (assert-equal 3 result))))

(deftest invoke-registered-expander-supports-descriptor-backed-compiler-macros
  "Descriptor-backed compiler macro expanders still execute through invoke-registered-expander."
  (let ((cl-cc/expand:*macro-eval-fn* #'eval)
        (expander (cl-cc/expand::make-compiler-macro-expander '(x) '((+ x 1)))))
    (assert-equal 3
                  (cl-cc/expand::invoke-registered-expander expander '(foo 2) nil))))

(deftest compiler-macro-function-accesses-registered-expander
  "compiler-macro-function reflects the compiler macro registry."
  (let ((name (gensym "CMF-"))
        (expander (lambda (form env)
                    (declare (ignore form env))
                    42)))
    (setf (cl-cc/expand::compiler-macro-function name) expander)
    (assert-eq expander (cl-cc/expand::compiler-macro-function name))
    (setf (cl-cc/expand::compiler-macro-function name) nil)
    (assert-null (cl-cc/expand::compiler-macro-function name))))

(deftest define-compiler-macro-expands-funcall-function-designator
  "Compiler macros apply to (funcall #'name ...) and see the original &whole form."
  (let ((name (gensym "CM-FUNCALL-")))
    (our-macroexpand-1
     `(define-compiler-macro ,name (&whole form x)
        (if (eq (car form) 'funcall)
            (+ x 10)
            form)))
    (assert-equal 12
                  (cl-cc/expand:compiler-macroexpand-all
                   `(funcall #',name 2)))))

(deftest define-compiler-macro-can-decline-with-whole-form
  "Returning &whole declines expansion and leaves the call form unchanged."
  (let ((name (gensym "CM-DECLINE-")))
    (our-macroexpand-1
     `(define-compiler-macro ,name (&whole form x)
        (if (integerp x)
            (+ x 1)
            form)))
    (assert-equal 4 (cl-cc/expand:compiler-macroexpand-all `(,name 3)))
    (assert-equal `(,name a)
                  (cl-cc/expand:compiler-macroexpand-all `(,name a)))))

(deftest define-compiler-macro-binds-environment
  "Compiler macro &environment is bound to the expansion environment argument."
  (let ((name (gensym "CM-ENV-")))
    (our-macroexpand-1
     `(define-compiler-macro ,name (&environment env x)
        (if (and (null env) (eql x 1)) :null-env :non-null-env)))
    (assert-eq :null-env
               (cl-cc/expand:compiler-macroexpand-all `(,name 1)))))

(deftest define-compiler-macro-reregisters-after-repeat-expansion
  "DEFINE-COMPILER-MACRO side effects are not hidden by macroexpansion caches."
  (let ((form '(define-compiler-macro cached-cm (x) (+ x 2))))
    (let ((cl-cc/expand:*compiler-macro-table* (make-hash-table :test #'eq)))
      (assert-equal '(quote cached-cm) (our-macroexpand-1 form))
      (assert-true (cl-cc/expand::compiler-macro-function 'cached-cm)))
    (let ((cl-cc/expand:*compiler-macro-table* (make-hash-table :test #'eq)))
      (assert-equal '(quote cached-cm) (our-macroexpand-1 form))
      (assert-true (cl-cc/expand::compiler-macro-function 'cached-cm))
      (assert-equal 5
                    (cl-cc/expand:compiler-macroexpand-all '(cached-cm 3))))))

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

;;; ─── %compiler-macro-lambda-list-parts ──────────────────────────────────

(deftest compiler-macro-lambda-list-plain
  "%compiler-macro-lambda-list-parts passes through a plain lambda list unchanged."
  (multiple-value-bind (ll whole env)
      (cl-cc/expand::%compiler-macro-lambda-list-parts '(x y z))
    (assert-equal '(x y z) ll)
    (assert-null whole)
    (assert-null env)))

(deftest compiler-macro-lambda-list-whole
  "%compiler-macro-lambda-list-parts extracts &whole binding and removes it from the list."
  (multiple-value-bind (ll whole env)
      (cl-cc/expand::%compiler-macro-lambda-list-parts '(&whole w x y))
    (assert-equal '(x y) ll)
    (assert-eq 'w whole)
    (assert-null env)))

(deftest compiler-macro-lambda-list-environment
  "%compiler-macro-lambda-list-parts extracts &environment binding."
  (multiple-value-bind (ll whole env)
      (cl-cc/expand::%compiler-macro-lambda-list-parts '(x &environment e))
    (assert-equal '(x) ll)
    (assert-null whole)
    (assert-eq 'e env)))

(deftest compiler-macro-lambda-list-whole-and-environment
  "%compiler-macro-lambda-list-parts handles both &whole and &environment together."
  (multiple-value-bind (ll whole env)
      (cl-cc/expand::%compiler-macro-lambda-list-parts '(&whole w x &environment e y))
    (assert-equal '(x y) ll)
    (assert-eq 'w whole)
    (assert-eq 'e env)))
