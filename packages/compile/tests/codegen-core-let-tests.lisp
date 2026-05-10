;;;; tests/unit/compile/codegen-core-let-tests.lisp
;;;; Unit tests for codegen-core-let.lisp — let-binding analysis helpers.
;;;;
;;;; Covers:
;;;;   %ast-let-binding-ignored-p, %ast-cons-call-p, %ast-make-array-call-p,
;;;;   %ast-make-array-int-call-p, %binding-mentioned-in-body-p,
;;;;   %ast-lambda-bound-names, %ast-as-body-forms, %ast-wrap-bindings,
;;;;   %ast-let-sink-if-candidate.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── %ast-let-binding-ignored-p ───────────────────────────────────────────

(deftest-each ast-let-binding-ignored-p
  "%ast-let-binding-ignored-p: true only for ignore declarations on the same name."
  :cases (("ignore"     t   'x '((ignore x)))
          ("ignorable"  nil 'x '((ignorable x)))
          ("wrong-name" nil 'x '((ignore y)))
          ("empty"      nil 'x nil))
  (expected name decls)
  (assert-bool expected (cl-cc/compile::%ast-let-binding-ignored-p name decls)))

;;; ─── %ast-cons-call-p ─────────────────────────────────────────────────────

(deftest-each ast-cons-call-p
  "%ast-cons-call-p: true for 2-arg cons calls; false for wrong arity, non-cons, or non-call."
  :cases (("symbol-func"  (cl-cc/ast:make-ast-call :func 'cons
                            :args (list (cl-cc/ast:make-ast-int :value 1) (cl-cc/ast:make-ast-int :value 2)))
                          t)
          ("var-func"     (cl-cc/ast:make-ast-call :func (cl-cc/ast:make-ast-var :name 'cons)
                            :args (list (cl-cc/ast:make-ast-int :value 1) (cl-cc/ast:make-ast-int :value 2)))
                          t)
          ("wrong-arity"  (cl-cc/ast:make-ast-call :func 'cons
                            :args (list (cl-cc/ast:make-ast-int :value 1)))
                          nil)
          ("non-cons"     (cl-cc/ast:make-ast-call :func 'list
                            :args (list (cl-cc/ast:make-ast-int :value 1) (cl-cc/ast:make-ast-int :value 2)))
                          nil)
          ("non-call"     (cl-cc/ast:make-ast-int :value 5)
                          nil))
  (node expected)
  (assert-bool expected (cl-cc/compile::%ast-cons-call-p node)))

;;; ─── %ast-make-array-call-p / %ast-make-array-int-call-p ─────────────────

(deftest-each ast-make-array-call-p
  "%ast-make-array-call-p: true for 1-arg make-array calls; false for wrong arity."
  :cases (("symbol-func"  (cl-cc/ast:make-ast-call :func 'make-array
                            :args (list (cl-cc/ast:make-ast-int :value 10)))
                          t)
          ("wrong-arity"  (cl-cc/ast:make-ast-call :func 'make-array
                            :args (list (cl-cc/ast:make-ast-int :value 10)
                                        (cl-cc/ast:make-ast-int :value 5)))
                          nil))
  (node expected)
  (assert-bool expected (cl-cc/compile::%ast-make-array-call-p node)))

(deftest-each ast-make-array-int-call-p
  "%ast-make-array-int-call-p: true when size is ast-int; false when size is a variable."
  :cases (("literal-size"  (cl-cc/ast:make-ast-call :func 'make-array
                             :args (list (cl-cc/ast:make-ast-int :value 5)))
                           t)
          ("var-size"      (cl-cc/ast:make-ast-call :func 'make-array
                             :args (list (cl-cc/ast:make-ast-var :name 'n)))
                           nil))
  (node expected)
  (assert-bool expected (cl-cc/compile::%ast-make-array-int-call-p node)))

;;; ─── %binding-mentioned-in-body-p ─────────────────────────────────────────

(deftest-each binding-mentioned-in-body-p
  "%binding-mentioned-in-body-p: true when var referenced; false for empty body or different var."
  :cases (("var-in-body"    (list (cl-cc/ast:make-ast-var :name 'x))  'x  t)
          ("empty-body"     nil                                     'x  nil)
          ("different-var"  (list (cl-cc/ast:make-ast-var :name 'y))  'x  nil))
  (body name expected)
  (assert-bool expected (cl-cc/compile::%binding-mentioned-in-body-p body name)))

;;; ─── %ast-lambda-bound-names ──────────────────────────────────────────────

(deftest ast-lambda-bound-names-required-params
  "%ast-lambda-bound-names includes required params."
  (let ((node (cl-cc/ast:make-ast-lambda
               :params '(a b c)
               :body nil)))
    (assert-equal '(a b c) (cl-cc/compile::%ast-lambda-bound-names node))))

(deftest ast-lambda-bound-names-includes-rest-param
  "%ast-lambda-bound-names includes the &rest parameter."
  (let ((node (cl-cc/ast:make-ast-lambda
               :params '(x)
               :rest-param 'rest
               :body nil)))
    (let ((names (cl-cc/compile::%ast-lambda-bound-names node)))
      (assert-true (member 'x names))
      (assert-true (member 'rest names)))))

(deftest ast-lambda-bound-names-includes-optional-params
  "%ast-lambda-bound-names includes &optional parameter names."
  (let ((node (cl-cc/ast:make-ast-lambda
               :params nil
               :optional-params (list (list 'a (cl-cc/ast:make-ast-int :value 0)))
               :body nil)))
    (assert-true (member 'a (cl-cc/compile::%ast-lambda-bound-names node)))))

;;; ─── %ast-as-body-forms ───────────────────────────────────────────────────

(deftest ast-as-body-forms-unwraps-progn
  "%ast-as-body-forms returns the forms list from an ast-progn node."
  (let* ((f1 (cl-cc/ast:make-ast-int :value 1))
         (f2 (cl-cc/ast:make-ast-int :value 2))
         (node (cl-cc/ast:make-ast-progn :forms (list f1 f2))))
    (assert-equal (list f1 f2) (cl-cc/compile::%ast-as-body-forms node))))

(deftest ast-as-body-forms-wraps-non-progn-in-list
  "%ast-as-body-forms wraps a non-progn node in a singleton list."
  (let ((node (cl-cc/ast:make-ast-int :value 42)))
    (assert-equal (list node) (cl-cc/compile::%ast-as-body-forms node))))

;;; ─── %ast-wrap-bindings ───────────────────────────────────────────────────

(deftest ast-wrap-bindings-no-bindings-single-body-returns-form
  "%ast-wrap-bindings with no bindings and one body form returns the form directly."
  (let ((f (cl-cc/ast:make-ast-int :value 7)))
    (assert-eq f (cl-cc/compile::%ast-wrap-bindings nil (list f)))))

(deftest ast-wrap-bindings-no-bindings-multi-body-returns-progn
  "%ast-wrap-bindings with no bindings and multiple body forms returns ast-progn."
  (let ((result (cl-cc/compile::%ast-wrap-bindings
                 nil
                 (list (cl-cc/ast:make-ast-int :value 1)
                       (cl-cc/ast:make-ast-int :value 2)))))
    (assert-true (typep result 'cl-cc::ast-progn))))

(deftest ast-wrap-bindings-with-bindings-returns-ast-let
  "%ast-wrap-bindings with bindings returns an ast-let node."
  (let ((result (cl-cc/compile::%ast-wrap-bindings
                 (list (cons 'x (cl-cc/ast:make-ast-int :value 1)))
                 (list (cl-cc/ast:make-ast-var :name 'x)))))
    (assert-true (typep result 'cl-cc::ast-let))))

;;; ─── %ast-let-sink-if-candidate ───────────────────────────────────────────

(deftest sink-if-candidate-detected-for-single-binding-single-if-body
  "%ast-let-sink-if-candidate returns non-nil for a let binding a make-array
expression used only in one branch of an IF body. The sink-if optimizer only
targets allocating expressions (make-array, make-instance, cons); plain
integer bindings aren't candidates."
  ;; (let ((arr (make-array 3))) (if cond (aref arr 0) 0))
  ;; arr is used only in then-branch → sink into then
  (let* ((node (cl-cc/ast:make-ast-let
                :bindings (list (cons 'arr (cl-cc/ast:make-ast-call
                                            :func 'make-array
                                            :args (list (cl-cc/ast:make-ast-int :value 3)))))
                :body (list (cl-cc/ast:make-ast-if
                             :cond (cl-cc/ast:make-ast-int :value 1)
                             :then (cl-cc/ast:make-ast-call
                                    :func 'aref
                                    :args (list (cl-cc/ast:make-ast-var :name 'arr)
                                                (cl-cc/ast:make-ast-int :value 0)))
                             :else (cl-cc/ast:make-ast-int :value 0)))))
         (result (cl-cc/compile::%ast-let-sink-if-candidate node)))
    (assert-true result)))

(deftest sink-if-candidate-nil-for-non-if-body
  "%ast-let-sink-if-candidate returns nil when body is not an IF."
  (let ((node (cl-cc/ast:make-ast-let
               :bindings (list (cons 'x (cl-cc/ast:make-ast-int :value 1)))
               :body (list (cl-cc/ast:make-ast-var :name 'x)))))
    (assert-null (cl-cc/compile::%ast-let-sink-if-candidate node))))

;;; ─── %let-binding-special-p (from codegen-core-let-emit.lisp) ───────────────

(deftest-each let-binding-special-p
  "%let-binding-special-p: true only for registered *earmuff* globals; false otherwise."
  :cases (("earmuff-global"      '*x*  '*x*  t)    ; registered *earmuff* → true
          ("plain-symbol"        'x    'x    nil)   ; no earmuffs → false
          ("earmuff-unregistered" '*x* nil   nil)   ; earmuffs but not registered → false
          ("single-star"         '*    '*    nil))  ; * alone fails earmuff check → false
  (sym register-sym expected)
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (when register-sym
      (setf (gethash register-sym (cl-cc/compile:ctx-global-variables ctx)) t))
    (assert-bool expected (cl-cc/compile::%let-binding-special-p sym ctx))))
