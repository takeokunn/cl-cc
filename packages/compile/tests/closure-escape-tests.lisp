;;;; tests/unit/compile/closure-escape-tests.lisp — Escape analysis and closure-sharing tests
;;;;
;;;; Tests the escape analysis helpers and closure sharing/normalization utilities
;;;; from the compile package. Depends on closure-tests.lisp being loaded first
;;;; (via ASDF :serial t) for shared helpers.

(in-package :cl-cc/test)

(in-suite closure-suite)

;;; ─── Conservative escape analysis helper ─────────────────────────────────

(deftest binding-escapes-direct-return-returns-true
  "binding-escapes-in-body-p returns true when the binding appears as a direct return value."
  (assert-true
   (cl-cc/compile::binding-escapes-in-body-p
    (list (cl-cc/ast::make-ast-var :name 'p))
    'p)))

(deftest binding-escapes-safe-consumer-returns-nil
  "binding-escapes-in-body-p returns nil when the only use is via a declared safe consumer."
  (assert-null
   (cl-cc/compile::binding-escapes-in-body-p
    (list (cl-cc/ast::make-ast-call
           :func 'car
           :args (list (cl-cc/ast::make-ast-var :name 'p))))
    'p
    :safe-consumers '("CAR"))))

(deftest binding-escapes-inner-lambda-capture-returns-true
  "binding-escapes-in-body-p returns true when the binding is captured by an inner lambda."
  (assert-true
   (cl-cc/compile::binding-escapes-in-body-p
    (list (cl-cc/ast::make-ast-lambda
           :params '()
           :body (list (cl-cc/ast::make-ast-var :name 'p))))
    'p)))

(deftest-each binding-escape-kinds-reports
  "binding-escape-kinds-in-body classifies escapes as :return, :external-call, or :capture."
  :cases (("direct-return"
           :return
           (list (cl-cc/ast::make-ast-var :name 'p))
           'p)
          ("external-call"
           :external-call
           (list (cl-cc/ast::make-ast-call :func 'list
                                       :args (list (cl-cc/ast::make-ast-var :name 'p))))
           'p)
          ("inner-capture"
           :capture
           (list (cl-cc/ast::make-ast-lambda :params '()
                                         :body (list (cl-cc/ast::make-ast-var :name 'p))))
           'p))
  (expected-kind forms binding)
  (assert-true (member expected-kind (cl-cc/compile::binding-escape-kinds-in-body forms binding))))

(deftest-each closure-key-normalization
  "closure-capture-key and closure-sharing-key normalize captured-variable order."
  :cases (("capture-key"
           '(x y)
           (cl-cc/compile::closure-capture-key '((y . :r2) (x . :r1) (x . :r9))))
          ("sharing-key"
           '("L0" (x y))
           (cl-cc/compile::closure-sharing-key "L0" '((y . :r2) (x . :r1)))))
  (expected actual)
  (assert-equal expected actual))

(deftest binding-direct-call-count-ignores-non-call-refs
  "binding-direct-call-count-in-body counts calls but not bare variable references."
  (assert-equal 1
                (cl-cc/compile::binding-direct-call-count-in-body
                 (list (cl-cc/ast::make-ast-call :func 'f :args nil)
                       (cl-cc/ast::make-ast-var :name 'f))
                 'f)))

(deftest binding-one-shot-single-call-returns-true
  "binding-one-shot-p returns true when the binding is called exactly once and does not escape."
  (assert-true
   (cl-cc/compile::binding-one-shot-p
    (list (cl-cc/ast::make-ast-call :func 'f :args (list (cl-cc/ast::make-ast-int :value 1))))
    'f)))

(deftest binding-one-shot-multi-call-returns-false
  "binding-one-shot-p returns false when the binding is called more than once."
  (assert-false
   (cl-cc/compile::binding-one-shot-p
    (list (cl-cc/ast::make-ast-call :func 'f :args nil)
          (cl-cc/ast::make-ast-call :func 'f :args nil))
    'f)))

(deftest binding-one-shot-captured-in-lambda-returns-false
  "binding-one-shot-p returns false when the binding is captured inside an inner lambda."
  (assert-false
   (cl-cc/compile::binding-one-shot-p
    (list (cl-cc/ast::make-ast-lambda :params '() :body (list (cl-cc/ast::make-ast-var :name 'f))))
    'f)))

(deftest group-shared-sibling-captures-groups-by-capture-set
  "group-shared-sibling-captures groups closures sharing the same normalized capture set."
  (let ((groups (cl-cc/compile::group-shared-sibling-captures
                 '(((x . :r1) (y . :r2))
                   ((y . :r8) (x . :r7))
                   ((z . :r3))))))
    (assert-equal 1 (hash-table-count groups))
    (assert-equal 2 (length (gethash '(x y) groups)))
    (assert-false (gethash '(z) groups))))

(deftest group-shareable-closures-groups-by-label-and-captures
  "group-shareable-closures groups closures with identical entry-label and capture set."
  (let ((groups (cl-cc/compile::group-shareable-closures
                 '((:entry-label "L0" :captured-vars ((x . :r1) (y . :r2)))
                   (:entry-label "L0" :captured-vars ((y . :r8) (x . :r7)))
                   (:entry-label "L1" :captured-vars ((x . :r1) (y . :r2)))))))
    (assert-equal 1 (hash-table-count groups))
    (assert-equal 2 (length (gethash '("L0" (x y)) groups)))))

(deftest binding-escape-defun-body-yields-capture
  "A binding referenced inside an ast-defun body is classified as :capture."
  (assert-true
   (member :capture
           (cl-cc/compile::binding-escape-kinds-in-body
            (list (cl-cc/ast::make-ast-defun
                   :name 'inner
                   :params '()
                   :body (list (cl-cc/ast::make-ast-var :name 'p))))
            'p))))

(deftest binding-escape-apply-arg-yields-external-call
  "A binding passed as an argument to ast-apply is classified as :external-call."
  (assert-true
   (member :external-call
           (cl-cc/compile::binding-escape-kinds-in-body
            (list (cl-cc/ast::make-ast-apply
                   :func (cl-cc/ast::make-ast-var :name 'f)
                   :args (list (cl-cc/ast::make-ast-var :name 'p))))
            'p))))

(deftest binding-escape-flet-body-yields-capture
  "A binding referenced inside an ast-flet body is classified as :capture."
  (assert-true
   (member :capture
           (cl-cc/compile::binding-escape-kinds-in-body
            (list (cl-cc/ast::make-ast-flet
                   :bindings nil
                   :body (list (cl-cc/ast::make-ast-var :name 'p))))
            'p))))

(deftest binding-no-escape-empty-body-returns-nil
  "binding-escape-kinds-in-body returns nil for a nil or empty body."
  (assert-null (cl-cc/compile::binding-escape-kinds-in-body nil 'p))
  (assert-null (cl-cc/compile::binding-escape-kinds-in-body '() 'p)))

(deftest binding-no-escape-absent-binding-returns-nil
  "binding-escape-kinds-in-body returns nil when the binding does not appear in the body."
  (assert-null
   (cl-cc/compile::binding-escape-kinds-in-body
    (list (cl-cc/ast::make-ast-var :name 'q))
    'p)))
