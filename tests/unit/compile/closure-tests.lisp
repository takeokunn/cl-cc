;;;; tests/unit/compile/closure-tests.lisp — Unit tests for find-free-variables
;;;;
;;;; Tests the free variable analysis on hand-constructed AST nodes.
;;;; Each test builds an AST directly (no parsing) and checks the result
;;;; of find-free-variables against the expected set of free variables.

(in-package :cl-cc/test)

(defsuite closure-suite :description "Free variable analysis unit tests"
  :parent cl-cc-suite)


(in-suite closure-suite)
;;; ─── Literals ─────────────────────────────────────────────────────────────

(deftest-each free-vars-atomic-forms
  "Atomic forms (integer literal, quote) have no free variables."
  :cases (("int"   (cl-cc::make-ast-int   :value 42))
          ("quote" (cl-cc::make-ast-quote :value '(a b c))))
  (node)
  (assert-equal nil (cl-cc::find-free-variables node)))

;;; ─── Simple references ────────────────────────────────────────────────────

(deftest ast-free-vars-single-var
  "A single variable reference is free in find-free-variables."
  (assert-equal '(x)
    (cl-cc::find-free-variables (cl-cc::make-ast-var :name 'x))))

(deftest free-vars-binop-two-vars
  "A binop with two different vars has both free."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-binop
                  :op '+
                  :lhs (cl-cc::make-ast-var :name 'x)
                  :rhs (cl-cc::make-ast-var :name 'y)))))
    (assert-true (and (member 'x result) (member 'y result)))
    (assert-equal 2 (length result))))

(deftest free-vars-binop-same-var
  "A binop referencing the same var twice returns it once (union)."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-binop
                  :op '+
                  :lhs (cl-cc::make-ast-var :name 'x)
                  :rhs (cl-cc::make-ast-var :name 'x)))))
    (assert-equal '(x) result)))

;;; ─── Let binding ──────────────────────────────────────────────────────────

(deftest free-vars-let-scenarios
  "Let: bound vars shadow body; binding exprs are free; unbound body vars remain free."
  ;; x bound in let → not free
  (assert-equal nil (cl-cc::find-free-variables
                     (cl-cc::make-ast-let
                      :bindings (list (cons 'x (cl-cc::make-ast-int :value 1)))
                      :body     (list (cl-cc::make-ast-var :name 'x)))))
  ;; binding expr references y → y is free; x is bound
  (assert-equal '(y) (cl-cc::find-free-variables
                      (cl-cc::make-ast-let
                       :bindings (list (cons 'x (cl-cc::make-ast-var :name 'y)))
                       :body     (list (cl-cc::make-ast-var :name 'x)))))
  ;; x bound, z is free in body
  (assert-equal '(z) (cl-cc::find-free-variables
                      (cl-cc::make-ast-let
                       :bindings (list (cons 'x (cl-cc::make-ast-int :value 1)))
                       :body     (list (cl-cc::make-ast-binop
                                        :op '+
                                        :lhs (cl-cc::make-ast-var :name 'x)
                                        :rhs (cl-cc::make-ast-var :name 'z)))))))

;;; ─── Lambda params ────────────────────────────────────────────────────────

(deftest free-vars-lambda-scope
  "Lambda params are not free in body; outer variables referenced in body are free."
  ;; params shadow body references — no free vars
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-lambda
                  :params '(x y)
                  :body (list (cl-cc::make-ast-binop
                               :op '+
                               :lhs (cl-cc::make-ast-var :name 'x)
                               :rhs (cl-cc::make-ast-var :name 'y)))))))
    (assert-equal nil result))
  ;; x is a param (bound), z is free
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-lambda
                  :params '(x)
                  :body (list (cl-cc::make-ast-binop
                               :op '+
                               :lhs (cl-cc::make-ast-var :name 'x)
                               :rhs (cl-cc::make-ast-var :name 'z)))))))
    (assert-equal '(z) result)))

(deftest free-vars-lambda-extended-params-shadow
  "Lambda &rest and &optional params shadow their names in the body."
  ;; &rest param
  (assert-equal nil (cl-cc::find-free-variables
                     (cl-cc::make-ast-lambda
                      :params '(x)
                      :rest-param 'rest
                      :body (list (cl-cc::make-ast-var :name 'rest)))))
  ;; &optional param
  (assert-equal nil (cl-cc::find-free-variables
                     (cl-cc::make-ast-lambda
                      :params nil
                      :optional-params (list (list 'a (cl-cc::make-ast-int :value 0)))
                      :body (list (cl-cc::make-ast-var :name 'a))))))

(deftest free-vars-lambda-optional-default-free
  "Free variables in optional param defaults are captured."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-lambda
                  :params nil
                  :optional-params (list (list 'a (cl-cc::make-ast-var :name 'z)))
                  :body (list (cl-cc::make-ast-var :name 'a))))))
    ;; a is bound as optional, z is free from the default expression
    (assert-equal '(z) result)))

(deftest free-vars-lambda-key-param
  "Lambda keyword params are not free in body."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-lambda
                  :params nil
                  :key-params (list (list 'k nil))
                  :body (list (cl-cc::make-ast-var :name 'k))))))
    (assert-equal nil result)))

;;; ─── Nested scope ─────────────────────────────────────────────────────────

(deftest free-vars-nested-let
  "Inner let does not shadow outer free variables."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-let
                  :bindings (list (cons 'x (cl-cc::make-ast-int :value 1)))
                  :body (list
                         (cl-cc::make-ast-let
                          :bindings (list (cons 'y (cl-cc::make-ast-var :name 'x)))
                          :body (list (cl-cc::make-ast-binop
                                       :op '+
                                       :lhs (cl-cc::make-ast-var :name 'y)
                                       :rhs (cl-cc::make-ast-var :name 'w)))))))))
    ;; x is bound by outer let, y is bound by inner let, w is free
    (assert-equal '(w) result)))

;;; ─── Defun ────────────────────────────────────────────────────────────────

(deftest free-vars-defun-shadows-params
  "Defun params are not free in body."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-defun
                  :name 'my-fn
                  :params '(a b)
                  :body (list (cl-cc::make-ast-binop
                               :op '+
                               :lhs (cl-cc::make-ast-var :name 'a)
                               :rhs (cl-cc::make-ast-var :name 'c)))))))
    ;; a,b are params, c is free
    (assert-equal '(c) result)))

;;; ─── Setq ─────────────────────────────────────────────────────────────────

(deftest free-vars-setq
  "Setq marks both the variable and the value expression's free vars."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-setq
                  :var 'x
                  :value (cl-cc::make-ast-var :name 'y)))))
    (assert-true (and (member 'x result) (member 'y result)))
    (assert-equal 2 (length result))))

;;; ─── Call ─────────────────────────────────────────────────────────────────

(deftest free-vars-call-function-forms
  "Call: symbol func contributes no free vars (only args); AST func node contributes free vars from both func and args."
  ;; symbol func — only arg x is free
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-call
                  :func 'foo
                  :args (list (cl-cc::make-ast-var :name 'x))))))
    (assert-equal '(x) result))
  ;; AST func node — both f and x are free
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-call
                  :func (cl-cc::make-ast-var :name 'f)
                  :args (list (cl-cc::make-ast-var :name 'x))))))
    (assert-true (and (member 'f result) (member 'x result)))
    (assert-equal 2 (length result))))

;;; ─── Flet / Labels ───────────────────────────────────────────────────────

(deftest free-vars-flet-shadows-func-name
  "Flet function names are not free in body."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-flet
                  :bindings (list (list 'my-fn '(a) (cl-cc::make-ast-var :name 'a)))
                  :body (list (cl-cc::make-ast-call
                               :func 'my-fn
                               :args (list (cl-cc::make-ast-int :value 1))))))))
    ;; my-fn is shadowed by flet, a is shadowed inside the binding body
    (assert-equal nil result)))

(deftest free-vars-labels-captures-outer
  "Labels bodies with outer references mark them free."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-labels
                  :bindings (list (list 'rec '(n) (cl-cc::make-ast-var :name 'limit)))
                  :body (list (cl-cc::make-ast-call
                               :func 'rec
                               :args (list (cl-cc::make-ast-int :value 0))))))))
    ;; rec is bound by labels, n is param inside binding, limit is free
    (assert-equal '(limit) result)))

;;; ─── If / Progn ──────────────────────────────────────────────────────────

(deftest free-vars-control-forms
  "If collects free vars from all three branches; progn collects free vars from all forms."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-if
                  :cond (cl-cc::make-ast-var :name 'p)
                  :then (cl-cc::make-ast-var :name 'x)
                  :else (cl-cc::make-ast-var :name 'y)))))
    (assert-true (and (member 'p result) (member 'x result) (member 'y result)))
    (assert-equal 3 (length result)))
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-progn
                  :forms (list (cl-cc::make-ast-var :name 'a)
                               (cl-cc::make-ast-var :name 'b))))))
    (assert-true (and (member 'a result) (member 'b result)))
    (assert-equal 2 (length result))))

;;; ─── Conservative escape analysis helper ─────────────────────────────────

(deftest binding-escapes-when-returned
  "A binding escapes when it is returned directly from the body."
  (assert-true
   (cl-cc::binding-escapes-in-body-p
    (list (cl-cc::make-ast-var :name 'p))
    'p)))

(deftest binding-does-not-escape-through-safe-consumer
  "A binding does not escape when only consumed by a whitelisted safe call."
  (assert-null
   (cl-cc::binding-escapes-in-body-p
    (list (cl-cc::make-ast-call
           :func 'car
           :args (list (cl-cc::make-ast-var :name 'p))))
    'p
     :safe-consumers '("CAR"))))

(deftest binding-escapes-when-captured-by-inner-lambda
  "A binding escapes when captured by a nested lambda."
  (assert-true
   (cl-cc::binding-escapes-in-body-p
     (list (cl-cc::make-ast-lambda
            :params '()
            :body (list (cl-cc::make-ast-var :name 'p))))
     'p)))

(deftest-each binding-escape-kinds-reports
  "binding-escape-kinds-in-body classifies escapes as :return, :external-call, or :capture."
  :cases (("direct-return"
           :return
           (list (cl-cc::make-ast-var :name 'p))
           'p)
          ("external-call"
           :external-call
           (list (cl-cc::make-ast-call :func 'list
                                       :args (list (cl-cc::make-ast-var :name 'p))))
           'p)
          ("inner-capture"
           :capture
           (list (cl-cc::make-ast-lambda :params '()
                                         :body (list (cl-cc::make-ast-var :name 'p))))
           'p))
  (expected-kind forms binding)
  (assert-true (member expected-kind (cl-cc::binding-escape-kinds-in-body forms binding))))

(deftest-each closure-key-normalization
  "closure-capture-key and closure-sharing-key normalize captured-variable order."
  :cases (("capture-key"
           '(x y)
           (cl-cc::closure-capture-key '((y . :r2) (x . :r1) (x . :r9))))
          ("sharing-key"
           '("L0" (x y))
           (cl-cc::closure-sharing-key "L0" '((y . :r2) (x . :r1)))))
  (expected actual)
  (assert-equal expected actual))

(deftest group-shared-sibling-captures-groups-identical-capture-sets
  "Sibling closures with the same capture set are grouped together."
  (let ((groups (cl-cc::group-shared-sibling-captures
                 '(((x . :r1) (y . :r2))
                   ((y . :r8) (x . :r7))
                   ((z . :r3))))))
    (assert-equal 1 (hash-table-count groups))
    (assert-equal 2 (length (gethash '(x y) groups)))
    (assert-false (gethash '(z) groups))))

(deftest binding-direct-call-count-in-body-counts-only-direct-calls
  "Direct calls to the binding are counted, other references are ignored." 
  (assert-equal 1
                (cl-cc::binding-direct-call-count-in-body
                 (list (cl-cc::make-ast-call :func 'f :args nil)
                       (cl-cc::make-ast-var :name 'f))
                 'f)))

(deftest binding-one-shot-p-detects-single-use-non-escaping-binding
  "A binding used by exactly one direct call and not escaping is one-shot." 
  (assert-true
   (cl-cc::binding-one-shot-p
    (list (cl-cc::make-ast-call :func 'f :args (list (cl-cc::make-ast-int :value 1))))
    'f)))

(deftest binding-one-shot-p-rejects-captured-or-multi-use-binding
  "Capture or multiple direct uses reject the one-shot predicate." 
  (assert-false
   (cl-cc::binding-one-shot-p
    (list (cl-cc::make-ast-call :func 'f :args nil)
          (cl-cc::make-ast-call :func 'f :args nil))
    'f))
  (assert-false
   (cl-cc::binding-one-shot-p
    (list (cl-cc::make-ast-lambda :params '() :body (list (cl-cc::make-ast-var :name 'f))))
    'f)))

(deftest group-shareable-closures-groups-by-label-and-captures
  "Only closures with both identical label and capture sets are grouped." 
  (let ((groups (cl-cc::group-shareable-closures
                 '((:entry-label "L0" :captured-vars ((x . :r1) (y . :r2)))
                   (:entry-label "L0" :captured-vars ((y . :r8) (x . :r7)))
                   (:entry-label "L1" :captured-vars ((x . :r1) (y . :r2)))))))
    (assert-equal 1 (hash-table-count groups))
    (assert-equal 2 (length (gethash '("L0" (x y)) groups)))))
