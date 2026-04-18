;;;; tests/unit/compile/closure-escape-tests.lisp — Escape analysis and closure-sharing tests
;;;;
;;;; Tests the escape analysis helpers and closure sharing/normalization utilities
;;;; from the compile package. Depends on closure-tests.lisp being loaded first
;;;; (via ASDF :serial t) for shared helpers.

(in-package :cl-cc/test)

(in-suite closure-suite)

;;; ─── Conservative escape analysis helper ─────────────────────────────────

(deftest binding-escapes-when-returned
  "A binding escapes when it is returned directly from the body."
  (assert-true
   (cl-cc/compile::binding-escapes-in-body-p
    (list (cl-cc/ast::make-ast-var :name 'p))
    'p)))

(deftest binding-does-not-escape-through-safe-consumer
  "A binding does not escape when only consumed by a whitelisted safe call."
  (assert-null
   (cl-cc/compile::binding-escapes-in-body-p
    (list (cl-cc/ast::make-ast-call
           :func 'car
           :args (list (cl-cc/ast::make-ast-var :name 'p))))
    'p
     :safe-consumers '("CAR"))))

(deftest binding-escapes-when-captured-by-inner-lambda
  "A binding escapes when captured by a nested lambda."
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

(deftest group-shared-sibling-captures-groups-identical-capture-sets
  "Sibling closures with the same capture set are grouped together."
  (let ((groups (cl-cc/compile::group-shared-sibling-captures
                 '(((x . :r1) (y . :r2))
                   ((y . :r8) (x . :r7))
                   ((z . :r3))))))
    (assert-equal 1 (hash-table-count groups))
    (assert-equal 2 (length (gethash '(x y) groups)))
    (assert-false (gethash '(z) groups))))

(deftest binding-direct-call-count-in-body-counts-only-direct-calls
  "Direct calls to the binding are counted, other references are ignored."
  (assert-equal 1
                (cl-cc/compile::binding-direct-call-count-in-body
                 (list (cl-cc/ast::make-ast-call :func 'f :args nil)
                       (cl-cc/ast::make-ast-var :name 'f))
                 'f)))

(deftest binding-one-shot-p-detects-single-use-non-escaping-binding
  "A binding used by exactly one direct call and not escaping is one-shot."
  (assert-true
   (cl-cc/compile::binding-one-shot-p
    (list (cl-cc/ast::make-ast-call :func 'f :args (list (cl-cc/ast::make-ast-int :value 1))))
    'f)))

(deftest binding-one-shot-p-rejects-captured-or-multi-use-binding
  "Capture or multiple direct uses reject the one-shot predicate."
  (assert-false
   (cl-cc/compile::binding-one-shot-p
    (list (cl-cc/ast::make-ast-call :func 'f :args nil)
          (cl-cc/ast::make-ast-call :func 'f :args nil))
    'f))
  (assert-false
   (cl-cc/compile::binding-one-shot-p
    (list (cl-cc/ast::make-ast-lambda :params '() :body (list (cl-cc/ast::make-ast-var :name 'f))))
    'f)))

(deftest group-shareable-closures-groups-by-label-and-captures
  "Only closures with both identical label and capture sets are grouped."
  (let ((groups (cl-cc/compile::group-shareable-closures
                 '((:entry-label "L0" :captured-vars ((x . :r1) (y . :r2)))
                   (:entry-label "L0" :captured-vars ((y . :r8) (x . :r7)))
                   (:entry-label "L1" :captured-vars ((x . :r1) (y . :r2)))))))
    (assert-equal 1 (hash-table-count groups))
    (assert-equal 2 (length (gethash '("L0" (x y)) groups)))))

(deftest binding-escapes-captured-by-defun
  "A binding captured inside an ast-defun body is classified as :capture."
  (assert-true
   (member :capture
           (cl-cc/compile::binding-escape-kinds-in-body
            (list (cl-cc/ast::make-ast-defun
                   :name 'inner
                   :params '()
                   :body (list (cl-cc/ast::make-ast-var :name 'p))))
            'p))))

(deftest binding-escapes-via-apply-node
  "A binding in an ast-apply argument is classified as :external-call."
  (assert-true
   (member :external-call
           (cl-cc/compile::binding-escape-kinds-in-body
            (list (cl-cc/ast::make-ast-apply
                   :func (cl-cc/ast::make-ast-var :name 'f)
                   :args (list (cl-cc/ast::make-ast-var :name 'p))))
            'p))))

(deftest binding-escape-kinds-empty-forms
  "Empty body forms return nil (no escapes)."
  (assert-null (cl-cc/compile::binding-escape-kinds-in-body nil 'p))
  (assert-null (cl-cc/compile::binding-escape-kinds-in-body '() 'p)))

(deftest binding-escapes-captured-by-local-fns-body
  "A binding referenced in the body of ast-local-fns is classified as :capture."
  (assert-true
   (member :capture
           (cl-cc/compile::binding-escape-kinds-in-body
            (list (cl-cc/ast::make-ast-flet
                   :bindings nil
                   :body (list (cl-cc/ast::make-ast-var :name 'p))))
            'p))))

(deftest binding-no-escape-when-absent
  "A binding that does not appear in body forms yields no escape kinds."
  (assert-null
   (cl-cc/compile::binding-escape-kinds-in-body
    (list (cl-cc/ast::make-ast-var :name 'q))
    'p)))
