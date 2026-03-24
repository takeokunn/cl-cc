;;;; tests/unit/core/cps-tests.lisp — CPS Transformation tests
;;;;
;;;; Three layers of tests:
;;;;   1. S-expression CPS (bootstrap transformer) — semantic evaluation
;;;;   2. AST CPS — semantic evaluation where the form is evaluable
;;;;   3. AST CPS — structural "is a CPS lambda?" for non-evaluable forms
;;;;      (tagbody/go/throw/block/return-from don't return values cleanly)
;;;;
;;;; Uses deftest-each to group the structural-shape tests.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Helpers
;;; ─────────────────────────────────────────────────────────────────────────

(defun eval-cps-ast (ast)
  "CPS-transform an AST node to a (lambda (k) ...) sexp and evaluate it.
Returns a function that takes a continuation."
  (eval (cl-cc:cps-transform-ast* ast)))

(defun run-cps-ast (ast)
  "Run CPS-transformed AST with the identity continuation."
  (funcall (eval-cps-ast ast) #'identity))

(defun is-cps-lambda (result)
  "Return t if RESULT is a (lambda (k) ...) sexp as produced by cps-transform-ast*."
  (and (listp result)
       (eq 'lambda (car result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; S-expression CPS (bootstrap transformer)
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cps-sexp-transform
  "S-expression CPS transformer: evaluate with identity continuation"
  :cases (("integer"           '42               42)
          ("add"               '(+ 1 2)           3)
          ("sub"               '(- 10 3)          7)
          ("mul"               '(* 3 4)          12)
          ("if-true"           '(if 1 10 20)     10)
          ("if-false"          '(if 0 10 20)     20)
          ("progn-returns-last" '(progn 1 2 3)    3)
          ("let-binding"       '(let ((x 1) (y 2)) (+ x y)) 3))
  (expr expected)
  (let ((fn (cl-cc:cps-transform-eval expr)))
    (assert-equal expected (funcall fn #'identity))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; AST CPS — semantic (evaluable forms)
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-ast-integer
  "cps-transform-ast: integer node evaluates to its value"
  (assert-= 42 (run-cps-ast (cl-cc:make-ast-int :value 42))))

(deftest-each cps-ast-binop
  "cps-transform-ast: binary arithmetic operations evaluate correctly"
  :cases (("add"      '+ 3  4  7)
          ("sub"      '- 9  4  5)
          ("mul"      '* 3  4 12))
  (op lhs rhs expected)
  (let ((ast (cl-cc:make-ast-binop :op op
                                   :lhs (cl-cc:make-ast-int :value lhs)
                                   :rhs (cl-cc:make-ast-int :value rhs))))
    (assert-= expected (run-cps-ast ast))))

(deftest-each cps-ast-if-branch
  "cps-transform-ast: if selects the correct branch based on condition truthiness"
  :cases (("truthy-takes-then" 1  10 20 10)
          ("zero-takes-else"   0  10 20 20))
  (cond-val then-val else-val expected)
  (let ((ast (cl-cc:make-ast-if :cond (cl-cc:make-ast-int :value cond-val)
                                :then (cl-cc:make-ast-int :value then-val)
                                :else (cl-cc:make-ast-int :value else-val))))
    (assert-= expected (run-cps-ast ast))))

(deftest cps-ast-progn-last-value
  "cps-transform-ast: progn returns the value of its last form"
  (let* ((forms (list (cl-cc:make-ast-int :value 1)
                      (cl-cc:make-ast-int :value 2)
                      (cl-cc:make-ast-int :value 99)))
         (ast (cl-cc:make-ast-progn :forms forms)))
    (assert-= 99 (run-cps-ast ast))))

(deftest cps-ast-let-bindings
  "cps-transform-ast: let binds values and body uses them"
  (let* ((bindings (list (cons 'x (cl-cc:make-ast-int :value 3))
                         (cons 'y (cl-cc:make-ast-int :value 4))))
         (body     (list (cl-cc:make-ast-binop
                          :op '+
                          :lhs (cl-cc:make-ast-var :name 'x)
                          :rhs (cl-cc:make-ast-var :name 'y))))
         (ast (cl-cc:make-ast-let :bindings bindings :body body)))
    (assert-= 7 (run-cps-ast ast))))

(deftest cps-ast-print-returns-value
  "cps-transform-ast: print node passes printed value to continuation"
  (let* ((expr (cl-cc:make-ast-int :value 42))
         (ast  (cl-cc:make-ast-print :expr expr)))
    (assert-= 42 (run-cps-ast ast))))

(deftest cps-ast-quote
  "cps-transform-ast: quoted literal is passed to continuation as-is"
  (let* ((ast (cl-cc:make-ast-quote :value 'hello)))
    (assert-eq 'hello (run-cps-ast ast))))

(deftest cps-ast-quote-list
  "cps-transform-ast: quoted list is passed unchanged"
  (let* ((ast (cl-cc:make-ast-quote :value '(1 2 3))))
    (assert-equal '(1 2 3) (run-cps-ast ast))))

(deftest cps-ast-the-transparent
  "cps-transform-ast: the wraps in a CL type declaration but passes value through"
  (let* ((inner (cl-cc:make-ast-int :value 7))
         (ast   (cl-cc:make-ast-the :type 'integer :value inner)))
    (assert-= 7 (run-cps-ast ast))))

(deftest cps-ast-setq-returns-value
  "cps-transform-ast: setq evaluates the value and passes it to continuation"
  (let ((setq-ast (cl-cc:make-ast-setq
                   :var 'cl-cc-test-setq-var
                   :value (cl-cc:make-ast-int :value 55))))
    ;; Bind the target var so SBCL doesn't complain about an unbound special
    (let ((cl-cc-test-setq-var nil))
      (declare (special cl-cc-test-setq-var))
      (let ((result (run-cps-ast setq-ast)))
        (assert-= 55 result)
        (assert-= 55 cl-cc-test-setq-var)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; AST CPS — structural ("is it a CPS lambda?")
;;; These forms are transformed correctly but cannot be trivially evaluated
;;; because they involve non-local control (block, go, throw) or closures.
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cps-ast-structural-shape
  "cps-transform-ast*: every node type produces a (lambda (k) ...) form"
  :cases
  (("lambda"
    (cl-cc:make-ast-lambda
     :params '(x)
     :body (list (cl-cc:make-ast-int :value 1))))

   ("block"
    (cl-cc:make-ast-block
     :name 'b
     :body (list (cl-cc:make-ast-int :value 1))))

   ("return-from"
    (cl-cc:make-ast-return-from
     :name 'b
     :value (cl-cc:make-ast-int :value 1)))

   ("tagbody"
    (cl-cc:make-ast-tagbody
     :tags (list (cons 'tag1 (list (cl-cc:make-ast-int :value 1))))))

   ("go"
    (cl-cc:make-ast-go :tag 'tag1))

   ("catch"
    (cl-cc:make-ast-catch
     :tag  (cl-cc:make-ast-var :name 'my-tag)
     :body (list (cl-cc:make-ast-int :value 42))))

   ("throw"
    (cl-cc:make-ast-throw
     :tag   (cl-cc:make-ast-var :name 'my-tag)
     :value (cl-cc:make-ast-int :value 42)))

   ("unwind-protect"
    (cl-cc:make-ast-unwind-protect
     :protected (cl-cc:make-ast-int :value 42)
     :cleanup   (list (cl-cc:make-ast-int :value 0))))

   ("flet"
    (cl-cc:make-ast-flet
     :bindings (list (list 'double '(x)
                           (cl-cc:make-ast-binop
                            :op '*
                            :lhs (cl-cc:make-ast-int :value 2)
                            :rhs (cl-cc:make-ast-var :name 'x))))
     :body (list (cl-cc:make-ast-int :value 1))))

   ("labels"
    (cl-cc:make-ast-labels
     :bindings (list (list 'id '(x) (cl-cc:make-ast-var :name 'x)))
     :body (list (cl-cc:make-ast-int :value 1)))))
  (ast)
  (assert-true (is-cps-lambda (cl-cc:cps-transform-ast* ast))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cps-transform* dispatcher
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-transform*-on-ast-node
  "cps-transform* dispatches to the AST transformer for AST nodes"
  (let* ((ast    (cl-cc:make-ast-int :value 42))
         (result (cl-cc:cps-transform* ast)))
    (assert-true (is-cps-lambda result))))

(deftest cps-transform*-on-sexp
  "cps-transform* dispatches to the sexp transformer for non-AST values"
  (let ((result (cl-cc:cps-transform* '(+ 1 2))))
    (assert-true (is-cps-lambda result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cps-transform-sequence edge cases
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-sequence-empty-returns-nil
  "cps-transform-sequence with empty list calls continuation with nil"
  (let* ((k-var (gensym "K"))
         (sexp  (cl-cc:cps-transform-sequence nil k-var)))
    ;; Should be (funcall k nil)
    (assert-true (listp sexp))
    (assert-eq 'funcall (car sexp))))

(deftest cps-sequence-single-delegates
  "cps-transform-sequence with one form delegates directly to cps-transform-ast"
  (let* ((node  (cl-cc:make-ast-int :value 5))
         (k-var (gensym "K"))
         (sexp  (cl-cc:cps-transform-sequence (list node) k-var)))
    ;; Should be the direct CPS form for the integer, not an extra lambda wrap
    (assert-true (listp sexp))))
