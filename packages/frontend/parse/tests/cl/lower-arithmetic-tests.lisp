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

(deftest-each lower-nullary-arithmetic
  "Nullary +/* lower to ast-int identity elements: + → 0, * → 1."
  :cases (("plus" '(+) 0)
          ("mul"  '(*) 1))
  (form expected)
  (let ((node (lower form)))
    (assert-true (cl-cc::ast-int-p node))
    (assert-= expected (cl-cc::ast-int-value node))))

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

(deftest-each lower-comparison-ops-produce-binop
  "Each comparison op lowers to ast-binop with the correct op."
  :cases (("eq" '(= 3 4)  '=)
          ("lt" '(< 1 2)  '<)
          ("gt" '(> 2 1)  '>)
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

(deftest-each lower-progn-form-count
  "(progn forms) lowers to ast-progn with the correct number of forms."
  :cases (("single"   '(progn 42)    1)
          ("multiple" '(progn 1 2 3) 3))
  (form expected-count)
  (let ((node (lower form)))
    (assert-true (cl-cc::ast-progn-p node))
    (assert-= expected-count (length (cl-cc::ast-progn-forms node)))))

;;; ─── Let binding ──────────────────────────────────────────────────────────

(deftest lower-let-produces-ast-let
  "(let ((x 1)) x) lowers to ast-let with one binding."
  (let ((node (lower '(let ((x 1)) x))))
    (assert-true (cl-cc::ast-let-p node))
    (assert-= 1 (length (cl-cc::ast-let-bindings node)))
    (assert-eq 'x (car (first (cl-cc::ast-let-bindings node))))))

(deftest-each lower-let-default-nil-binding-forms
  "Bare symbol and single-element binding both default to (quote nil) value."
  :cases (("bare-symbol"    '(let (x) x))
          ("single-element" '(let ((x)) x)))
  (form)
  (let* ((node (lower form))
         (binding (first (cl-cc::ast-let-bindings node))))
    (assert-true (cl-cc::ast-let-p node))
    (assert-eq 'x (car binding))
    (assert-true (cl-cc::ast-quote-p (cdr binding)))))

;;; ─── Lambda ───────────────────────────────────────────────────────────────

(deftest-each lower-lambda-cases
  "lambda with params and without both produce ast-lambda."
  :cases (("with-params"  '(lambda (a b) (+ a b)) 2 'a)
          ("empty-params" '(lambda () 42)           0 nil))
  (form expected-count expected-first)
  (let ((node (lower form)))
    (assert-true (cl-cc::ast-lambda-p node))
    (assert-= expected-count (length (cl-cc::ast-lambda-params node)))
    (when expected-first
      (assert-eq expected-first (first (cl-cc::ast-lambda-params node))))))

;;; ─── Function reference ───────────────────────────────────────────────────

(deftest-each lower-function-reference-cases
  "#'sym → ast-function; #'(lambda ...) → ast-lambda directly."
  :cases (("symbol" '(function cons)         #'cl-cc::ast-function-p)
          ("lambda" '(function (lambda (x) x)) #'cl-cc::ast-lambda-p))
  (form pred)
  (assert-true (funcall pred (lower form))))

;;; ─── Block / return-from ─────────────────────────────────────────────────

(deftest-each lower-block-cases
  "block with body has the given name; block with empty body gets (quote nil) body."
  :cases (("with-body"  '(block outer (+ 1 2)) 'outer nil)
          ("empty-body" '(block foo)            'foo   t))
  (form expected-name empty-body-p)
  (let ((node (lower form)))
    (assert-true (cl-cc::ast-block-p node))
    (assert-eq expected-name (cl-cc::ast-block-name node))
    (when empty-body-p
      (assert-true (cl-cc::ast-quote-p (first (cl-cc::ast-block-body node)))))))

(deftest-each lower-return-from-cases
  "return-from: with value → ast-int; without value → (quote nil)."
  :cases (("with-value"    '(return-from outer 42) t)
          ("without-value" '(return-from outer)    nil))
  (form has-value-p)
  (let ((node (lower form)))
    (assert-true (cl-cc::ast-return-from-p node))
    (assert-eq 'outer (cl-cc::ast-return-from-name node))
    (if has-value-p
        (assert-= 42 (cl-cc::ast-int-value (cl-cc::ast-return-from-value node)))
        (assert-true (cl-cc::ast-quote-p (cl-cc::ast-return-from-value node))))))

;;; ─── Tagbody / go ────────────────────────────────────────────────────────

(deftest lower-go-and-tagbody
  "(go top) → ast-go with tag; (tagbody top ...) → ast-tagbody with one tag entry."
  (let ((go-node (lower '(go top))))
    (assert-true (cl-cc::ast-go-p go-node))
    (assert-eq 'top (cl-cc::ast-go-tag go-node)))
  (let ((tb-node (lower '(tagbody top (go top)))))
    (assert-true (cl-cc::ast-tagbody-p tb-node))
    (assert-= 1 (length (cl-cc::ast-tagbody-tags tb-node)))
    (assert-eq 'top (car (first (cl-cc::ast-tagbody-tags tb-node))))))

;;; ─── setf (compound places) ──────────────────────────────────────────────

(deftest lower-setf-symbol-place-produces-setq
  "(setf x 5) with symbol place lowers to ast-setq."
  (let ((node (lower '(setf x 5))))
    (assert-true (cl-cc::ast-setq-p node))
    (assert-eq 'x (cl-cc::ast-setq-var node))))

(deftest-each lower-setf-compound-place-cases
  "setf compound places: gethash → ast-set-gethash; slot-value → ast-set-slot-value."
  :cases (("gethash"    '(setf (gethash :k ht) v)      #'cl-cc::ast-set-gethash-p)
          ("slot-value" '(setf (slot-value obj 'x) 5)  #'cl-cc::ast-set-slot-value-p))
  (form pred)
  (assert-true (funcall pred (lower form))))
