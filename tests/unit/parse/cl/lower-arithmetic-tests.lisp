;;;; tests/unit/parse/cl/lower-arithmetic-tests.lisp
;;;; Additional coverage for src/parse/cl/lower.lisp
;;;;
;;;; Tests n-ary arithmetic lowering, comparison lowering, sequence,
;;;; let, lambda, function reference, block, return-from, tagbody, go,
;;;; multi-var setq, and setf (symbol and compound place forms).
;;;; The cl-lower-suite defsuite is in lower-tests.lisp (loaded first).

(in-package :cl-cc/test)
(in-suite cl-lower-suite)

;;; ─── Nullary and unary arithmetic (*arith-op-descriptors* table) ────────

(deftest lower-nullary-plus-returns-zero
  "(+) lowers to ast-int with value 0 (identity element)."
  (let ((node (lower '(+))))
    (assert-true (cl-cc::ast-int-p node))
    (assert-= 0 (cl-cc::ast-int-value node))))

(deftest lower-nullary-mul-returns-one
  "(*) lowers to ast-int with value 1 (identity element)."
  (let ((node (lower '(*))))
    (assert-true (cl-cc::ast-int-p node))
    (assert-= 1 (cl-cc::ast-int-value node))))

(deftest lower-unary-plus-returns-arg
  "(+ x) lowers to just x (no wrapping binop)."
  (let ((node (lower '(+ 42))))
    (assert-true (cl-cc::ast-int-p node))
    (assert-= 42 (cl-cc::ast-int-value node))))

(deftest lower-unary-divide-becomes-reciprocal
  "(/ x) lowers to (/ 1 x) — unary reciprocal form."
  (let ((node (lower '(/ 2))))
    (assert-true (cl-cc::ast-binop-p node))
    (assert-eq '/ (cl-cc::ast-binop-op node))
    (assert-= 1 (cl-cc::ast-int-value (cl-cc::ast-binop-lhs node)))))

;;; ─── N-ary arithmetic left-fold ──────────────────────────────────────────

(deftest lower-nary-plus-folds-left
  "(+ 1 2 3) lowers to nested left-assoc binops: (+ (+ 1 2) 3)."
  (let ((node (lower '(+ 1 2 3))))
    (assert-true (cl-cc::ast-binop-p node))
    (assert-eq '+ (cl-cc::ast-binop-op node))
    ;; rhs is 3
    (assert-= 3 (cl-cc::ast-int-value (cl-cc::ast-binop-rhs node)))
    ;; lhs is (+ 1 2)
    (assert-true (cl-cc::ast-binop-p (cl-cc::ast-binop-lhs node)))))

(deftest lower-binary-comparison-produces-binop
  "(= 3 4) lowers to ast-binop with op =."
  (let ((node (lower '(= 3 4))))
    (assert-true (cl-cc::ast-binop-p node))
    (assert-eq '= (cl-cc::ast-binop-op node))))

(deftest-each lower-comparison-ops-produce-binop
  "Each comparison op lowers to ast-binop with the correct op."
  :cases (("lt" '(< 1 2) '<)
          ("gt" '(> 2 1) '>)
          ("le" '(<= 1 1) '<=)
          ("ge" '(>= 2 1) '>=))
  (form expected-op)
  (let ((node (lower form)))
    (assert-true (cl-cc::ast-binop-p node))
    (assert-eq expected-op (cl-cc::ast-binop-op node))))

;;; ─── Sequence (progn) ────────────────────────────────────────────────────

(deftest lower-empty-progn-returns-nil-quote
  "(progn) lowers to (quote nil)."
  (let ((node (lower '(progn))))
    (assert-true (cl-cc::ast-quote-p node))
    (assert-null (cl-cc::ast-quote-value node))))

(deftest lower-progn-single-form-wraps-in-progn
  "(progn x) lowers to ast-progn containing the single form."
  (let ((node (lower '(progn 42))))
    (assert-true (cl-cc::ast-progn-p node))
    (assert-= 1 (length (cl-cc::ast-progn-forms node)))))

(deftest lower-progn-multiple-forms-collects-all
  "(progn 1 2 3) lowers to ast-progn with 3 forms."
  (let ((node (lower '(progn 1 2 3))))
    (assert-true (cl-cc::ast-progn-p node))
    (assert-= 3 (length (cl-cc::ast-progn-forms node)))))

;;; ─── Let binding ──────────────────────────────────────────────────────────

(deftest lower-let-produces-ast-let
  "(let ((x 1)) x) lowers to ast-let with one binding."
  (let ((node (lower '(let ((x 1)) x))))
    (assert-true (cl-cc::ast-let-p node))
    (assert-= 1 (length (cl-cc::ast-let-bindings node)))
    (assert-eq 'x (car (first (cl-cc::ast-let-bindings node))))))

(deftest lower-let-symbol-binding-defaults-to-nil
  "(let (x) x) treats bare symbol binding as (x nil)."
  (let ((node (lower '(let (x) x))))
    (assert-true (cl-cc::ast-let-p node))
    (let ((binding (first (cl-cc::ast-let-bindings node))))
      (assert-eq 'x (car binding))
      (assert-true (cl-cc::ast-quote-p (cdr binding))))))

(deftest lower-let-single-element-binding-defaults-to-nil
  "(let ((x)) x) treats (x) as binding x to nil."
  (let ((node (lower '(let ((x)) x))))
    (assert-true (cl-cc::ast-let-p node))
    (let ((binding (first (cl-cc::ast-let-bindings node))))
      (assert-eq 'x (car binding))
      (assert-true (cl-cc::ast-quote-p (cdr binding))))))

;;; ─── Lambda ───────────────────────────────────────────────────────────────

(deftest lower-lambda-produces-ast-lambda
  "(lambda (a b) (+ a b)) lowers to ast-lambda."
  (let ((node (lower '(lambda (a b) (+ a b)))))
    (assert-true (cl-cc::ast-lambda-p node))
    (assert-= 2 (length (cl-cc::ast-lambda-params node)))
    (assert-eq 'a (first (cl-cc::ast-lambda-params node)))))

(deftest lower-lambda-empty-params
  "(lambda () 42) lowers to ast-lambda with empty params."
  (let ((node (lower '(lambda () 42))))
    (assert-true (cl-cc::ast-lambda-p node))
    (assert-null (cl-cc::ast-lambda-params node))))

;;; ─── Function reference ───────────────────────────────────────────────────

(deftest lower-function-symbol-produces-ast-function
  "(function cons) lowers to ast-function naming cons."
  (let ((node (lower '(function cons))))
    (assert-true (cl-cc::ast-function-p node))
    (assert-eq 'cons (cl-cc::ast-function-name node))))

(deftest lower-function-lambda-lowers-inline
  "(function (lambda (x) x)) lowers to ast-lambda directly."
  (let ((node (lower '(function (lambda (x) x)))))
    (assert-true (cl-cc::ast-lambda-p node))))

;;; ─── Block / return-from ─────────────────────────────────────────────────

(deftest lower-block-produces-ast-block
  "(block outer (+ 1 2)) lowers to ast-block named OUTER."
  (let ((node (lower '(block outer (+ 1 2)))))
    (assert-true (cl-cc::ast-block-p node))
    (assert-eq 'outer (cl-cc::ast-block-name node))))

(deftest lower-block-empty-body-gets-nil-quote
  "(block foo) — empty body — lowers to ast-block with (quote nil) body."
  (let ((node (lower '(block foo))))
    (assert-true (cl-cc::ast-block-p node))
    (assert-true (cl-cc::ast-quote-p (first (cl-cc::ast-block-body node))))))

(deftest lower-return-from-with-value
  "(return-from outer 42) lowers to ast-return-from with integer value."
  (let ((node (lower '(return-from outer 42))))
    (assert-true (cl-cc::ast-return-from-p node))
    (assert-eq 'outer (cl-cc::ast-return-from-name node))
    (assert-= 42 (cl-cc::ast-int-value (cl-cc::ast-return-from-value node)))))

(deftest lower-return-from-without-value-uses-nil
  "(return-from outer) lowers to ast-return-from with (quote nil) value."
  (let ((node (lower '(return-from outer))))
    (assert-true (cl-cc::ast-return-from-p node))
    (assert-true (cl-cc::ast-quote-p (cl-cc::ast-return-from-value node)))))

;;; ─── Tagbody / go ────────────────────────────────────────────────────────

(deftest lower-go-produces-ast-go
  "(go top) lowers to ast-go with tag TOP."
  (let ((node (lower '(go top))))
    (assert-true (cl-cc::ast-go-p node))
    (assert-eq 'top (cl-cc::ast-go-tag node))))

(deftest lower-tagbody-produces-ast-tagbody
  "(tagbody top (go top)) lowers to ast-tagbody with one tag entry."
  (let ((node (lower '(tagbody top (go top)))))
    (assert-true (cl-cc::ast-tagbody-p node))
    (assert-= 1 (length (cl-cc::ast-tagbody-tags node)))
    (assert-eq 'top (car (first (cl-cc::ast-tagbody-tags node))))))

;;; ─── setf (compound places) ──────────────────────────────────────────────

(deftest lower-setf-symbol-place-produces-setq
  "(setf x 5) with symbol place lowers to ast-setq."
  (let ((node (lower '(setf x 5))))
    (assert-true (cl-cc::ast-setq-p node))
    (assert-eq 'x (cl-cc::ast-setq-var node))))

(deftest lower-setf-gethash-place-produces-set-gethash
  "(setf (gethash :k ht) v) lowers to ast-set-gethash."
  (let ((node (lower '(setf (gethash :k ht) v))))
    (assert-true (cl-cc::ast-set-gethash-p node))))

(deftest lower-setf-slot-value-place-produces-set-slot-value
  "(setf (slot-value obj 'x) 5) lowers to ast-set-slot-value."
  (let ((node (lower '(setf (slot-value obj 'x) 5))))
    (assert-true (cl-cc::ast-set-slot-value-p node))
    (assert-eq 'x (cl-cc::ast-set-slot-value-slot node))))
