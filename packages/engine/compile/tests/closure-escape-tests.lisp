;;;; tests/unit/compile/closure-escape-tests.lisp — Escape analysis and closure-sharing tests
;;;;
;;;; Tests the escape analysis helpers and closure sharing/normalization utilities
;;;; from the compile package. Depends on closure-tests.lisp being loaded first
;;;; (via ASDF :serial t) for shared helpers.

(in-package :cl-cc/test)

(in-suite closure-suite)

;;; ─── Conservative escape analysis helper ─────────────────────────────────

(deftest binding-escape-analysis-cases
  "binding-escapes-in-body-p: true for direct return; nil for safe consumer; true for inner lambda capture."
  (assert-true
   (cl-cc/compile::binding-escapes-in-body-p
    (list (cl-cc/ast::make-ast-var :name 'p))
    'p))
  (assert-null
   (cl-cc/compile::binding-escapes-in-body-p
    (list (cl-cc/ast::make-ast-call
           :func 'car
           :args (list (cl-cc/ast::make-ast-var :name 'p))))
    'p
     :safe-consumers '("CAR")))
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

(deftest binding-one-shot-cases
  "Direct call count ignores non-call refs; one-shot true for single non-escaping call; false for multi-use or capture."
  (assert-equal 1
                (cl-cc/compile::binding-direct-call-count-in-body
                 (list (cl-cc/ast::make-ast-call :func 'f :args nil)
                       (cl-cc/ast::make-ast-var :name 'f))
                 'f))
  (assert-true
   (cl-cc/compile::binding-one-shot-p
    (list (cl-cc/ast::make-ast-call :func 'f :args (list (cl-cc/ast::make-ast-int :value 1))))
    'f))
  (assert-false
   (cl-cc/compile::binding-one-shot-p
    (list (cl-cc/ast::make-ast-call :func 'f :args nil)
          (cl-cc/ast::make-ast-call :func 'f :args nil))
    'f))
  (assert-false
   (cl-cc/compile::binding-one-shot-p
    (list (cl-cc/ast::make-ast-lambda :params '() :body (list (cl-cc/ast::make-ast-var :name 'f))))
    'f)))

(deftest group-closures-cases
  "group-shared-sibling-captures groups by capture set; group-shareable-closures groups by label+captures."
  (let ((groups (cl-cc/compile::group-shared-sibling-captures
                 '(((x . :r1) (y . :r2))
                   ((y . :r8) (x . :r7))
                   ((z . :r3))))))
    (assert-equal 1 (hash-table-count groups))
    (assert-equal 2 (length (gethash '(x y) groups)))
    (assert-false (gethash '(z) groups)))
  (let ((groups (cl-cc/compile::group-shareable-closures
                 '((:entry-label "L0" :captured-vars ((x . :r1) (y . :r2)))
                   (:entry-label "L0" :captured-vars ((y . :r8) (x . :r7)))
                   (:entry-label "L1" :captured-vars ((x . :r1) (y . :r2)))))))
    (assert-equal 1 (hash-table-count groups))
    (assert-equal 2 (length (gethash '("L0" (x y)) groups)))))

(deftest binding-escape-capture-cases
  "Captures in ast-defun, ast-apply (external-call), and ast-flet are classified correctly."
  (assert-true
   (member :capture
           (cl-cc/compile::binding-escape-kinds-in-body
            (list (cl-cc/ast::make-ast-defun
                   :name 'inner
                   :params '()
                   :body (list (cl-cc/ast::make-ast-var :name 'p))))
            'p)))
  (assert-true
   (member :external-call
           (cl-cc/compile::binding-escape-kinds-in-body
            (list (cl-cc/ast::make-ast-apply
                   :func (cl-cc/ast::make-ast-var :name 'f)
                   :args (list (cl-cc/ast::make-ast-var :name 'p))))
            'p)))
  (assert-true
   (member :capture
           (cl-cc/compile::binding-escape-kinds-in-body
            (list (cl-cc/ast::make-ast-flet
                   :bindings nil
                   :body (list (cl-cc/ast::make-ast-var :name 'p))))
            'p))))

(deftest binding-no-escape-cases
  "Empty body and absent binding both yield no escape kinds."
  (assert-null (cl-cc/compile::binding-escape-kinds-in-body nil 'p))
  (assert-null (cl-cc/compile::binding-escape-kinds-in-body '() 'p))
  (assert-null
   (cl-cc/compile::binding-escape-kinds-in-body
    (list (cl-cc/ast::make-ast-var :name 'q))
    'p)))
