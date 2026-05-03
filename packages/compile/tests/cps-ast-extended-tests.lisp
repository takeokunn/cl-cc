;;;; tests/unit/compile/cps-ast-extended-tests.lisp
;;;; Unit tests for src/compile/cps-ast-extended.lisp
;;;;
;;;; Covers: cps-transform-ast for ast-quote, ast-setq, ast-the, ast-values,
;;;;   ast-apply, ast-call (general calls), ast-defun, ast-defmacro,
;;;;   ast-defclass, ast-defgeneric, ast-defmethod; entry points cps-transform-ast*,
;;;;   cps-transform*, cps-transform-eval.
;;;;
;;;; Tests inspect the *structure* of the produced S-expression rather than
;;;; evaluating it (which would require a full runtime environment).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; Helper: transform a node with a fixed continuation symbol.
(defun cps-with-k (node)
  "Return the CPS form for NODE with continuation symbol K."
  (cl-cc/cps::cps-transform-ast node 'k))

;;; ─── ast-quote ────────────────────────────────────────────────────────────

(deftest cps-quote-symbol-produces-funcall-k-form
  "CPS transform of ast-quote with a symbol produces (funcall k (quote val))."
  (let ((result (cps-with-k (cl-cc/ast:make-ast-quote :value 'hello))))
    (assert-eq 'funcall (car result))
    (assert-eq 'k (second result))
    (assert-equal '(quote hello) (third result))))

(deftest cps-quote-list-preserves-list-value
  "CPS transform of ast-quote with a list preserves the quoted list in third position."
  (let ((result (cps-with-k (cl-cc/ast:make-ast-quote :value '(a b c)))))
    (assert-equal '(quote (a b c)) (third result))))

;;; ─── ast-the ─────────────────────────────────────────────────────────────

(deftest cps-the-wraps-value-with-the-declaration
  "CPS transform of ast-the wraps the inner value in (the type ...)."
  (let* ((node (cl-cc/ast:make-ast-the
                :type 'integer
                :value (cl-cc/ast:make-ast-int :value 5)))
         (result (cps-with-k node)))
    ;; Result is a (cps-transform-ast (ast-int 5) (lambda (v) (funcall k (the integer v))))
    ;; The continuation arg is a lambda containing (the integer ...) and (funcall k ...)
    (assert-true (consp result))
    ;; Walk into the lambda body to find (the integer ...)
    (labels ((contains-the-p (form)
               (if (consp form)
                   (or (eq (car form) 'the)
                       (some #'contains-the-p form))
                   nil)))
      (assert-true (contains-the-p result)))))

;;; ─── ast-setq ────────────────────────────────────────────────────────────

(deftest cps-setq-contains-setq-and-funcall-k
  "CPS transform of ast-setq produces a form with (setq var ...) and (funcall k ...)."
  (let* ((node (cl-cc/ast:make-ast-setq
                :var 'x
                :value (cl-cc/ast:make-ast-int :value 0)))
         (result (cps-with-k node)))
    (labels ((contains-sym-p (form sym)
               (if (consp form)
                   (or (eq (car form) sym)
                       (some (lambda (sub) (contains-sym-p sub sym)) (cdr form)))
                   nil)))
      (assert-true (contains-sym-p result 'setq))
      (assert-true (contains-sym-p result 'funcall)))))

;;; ─── ast-values ──────────────────────────────────────────────────────────

(deftest cps-values-empty-forms-produces-funcall-k-nil
  "CPS transform of ast-values with no forms produces (funcall k nil)."
  (let* ((node (cl-cc/ast:make-ast-values :forms nil))
         (result (cps-with-k node)))
    (assert-eq 'funcall (car result))
    (assert-eq 'k (second result))
    (assert-null (third result))))

(deftest cps-values-single-form-produces-multiple-value-call
  "CPS transform of ast-values with one form produces a form containing multiple-value-call."
  (let* ((node (cl-cc/ast:make-ast-values
                :forms (list (cl-cc/ast:make-ast-int :value 42))))
         (result (cps-with-k node)))
    (labels ((contains-p (form sym)
               (if (consp form)
                   (or (eq (car form) sym)
                       (some (lambda (sub) (contains-p sub sym)) (cdr form)))
                   nil)))
      (assert-true (contains-p result 'multiple-value-call)))))

;;; ─── ast-apply ───────────────────────────────────────────────────────────

(deftest cps-apply-contains-apply-call
  "CPS transform of ast-apply produces a form containing (apply func ...)."
  (let* ((node (cl-cc/ast:make-ast-apply
                :func (cl-cc/ast:make-ast-var :name 'f)
                :args (list (cl-cc/ast:make-ast-var :name 'args))))
         (result (cps-with-k node)))
    (labels ((contains-apply-p (form)
               (if (consp form)
                   (or (eq (car form) 'apply)
                       (some #'contains-apply-p (cdr form)))
                   nil)))
      (assert-true (contains-apply-p result)))))

;;; ─── ast-call ─────────────────────────────────────────────────────────────

(deftest cps-call-no-args-produces-funcall-k-form
  "CPS transform of a zero-arg call produces (funcall k (func))."
  (let* ((node (cl-cc/ast:make-ast-call :func 'compute :args nil))
         (result (cps-with-k node)))
    (assert-eq 'funcall (car result))
    (assert-eq 'k (second result))
    (assert-equal '(compute) (third result))))

(deftest cps-call-with-args-threads-args-into-call
  "CPS transform of a 2-arg call threads both arg symbols into the final (add G1 G2) form."
  (let* ((node (cl-cc/ast:make-ast-call
                :func 'add
                :args (list (cl-cc/ast:make-ast-int :value 1)
                            (cl-cc/ast:make-ast-int :value 2))))
         (result (cps-with-k node)))
    ;; Walk the CPS tree to find the innermost (funcall k <call-form>)
    (labels ((find-funcall-k (form)
               (cond
                 ((atom form) nil)
                 ((and (eq (car form) 'funcall) (eq (second form) 'k))
                  form)
                 (t (loop for sub in form
                          for found = (find-funcall-k sub)
                          when found return found)))))
      (let* ((funcall-k (find-funcall-k result))
             (call-form (third funcall-k)))
        ;; The call form must be (add G1 G2) — head is 'add, exactly 2 args
        (assert-true (consp call-form))
        (assert-eq 'add (car call-form))
        (assert-= 2 (length (cdr call-form)))
        ;; Both arg symbols must be distinct gensyms (not NIL, not ADD)
        (assert-true (symbolp (second call-form)))
        (assert-true (symbolp (third call-form)))
        (assert-false (eq (second call-form) (third call-form)))))))

;;; ─── ast-defgeneric ───────────────────────────────────────────────────────

(deftest cps-defun-produces-progn-defun-funcall-k
  "CPS transform of ast-defun produces (progn (defun ...) (funcall k 'name))."
  (let* ((node (cl-cc/ast:make-ast-defun
                :name 'square
                :params '(x)
                :body (list (cl-cc/ast:make-ast-binop
                             :op '*
                             :lhs (cl-cc/ast:make-ast-var :name 'x)
                             :rhs (cl-cc/ast:make-ast-var :name 'x)))))
         (result (cps-with-k node)))
    (assert-eq 'progn (car result))
    (assert-eq 'defun (caadr result))
    (let ((last-form (car (last result))))
      (assert-eq 'funcall (car last-form))
      (assert-eq 'k (second last-form)))))

(deftest cps-defmacro-produces-progn-defmacro-funcall-k
  "CPS transform of ast-defmacro produces (progn (defmacro ...) (funcall k 'name))."
  (let* ((node (cl-cc/ast:make-ast-defmacro
                :name 'when1
                :lambda-list '(test &body body)
                :body '((list 'if test (cons 'progn body) nil))))
         (result (cps-with-k node)))
    (assert-eq 'progn (car result))
    (assert-eq 'defmacro (caadr result))
    (let ((last-form (car (last result))))
      (assert-eq 'funcall (car last-form))
      (assert-eq 'k (second last-form)))))

(deftest cps-defgeneric-produces-progn-defgeneric-funcall-k
  "CPS transform of ast-defgeneric produces (progn (defgeneric ...) (funcall k 'name))."
  (let* ((node (cl-cc/ast:make-ast-defgeneric :name 'area :params '(shape)))
         (result (cps-with-k node)))
    (assert-eq 'progn (car result))
    (assert-eq 'defgeneric (caadr result))
    (let ((last-form (car (last result))))
      (assert-eq 'funcall (car last-form))
      (assert-eq 'k (second last-form)))))

;;; ─── cps-transform-ast* ───────────────────────────────────────────────────

(deftest cps-transform-ast-star-returns-lambda-form
  "cps-transform-ast* wraps the transformed node in (lambda (k) ...)."
  (let* ((node (cl-cc/ast:make-ast-int :value 7))
         (result (cl-cc/cps::cps-transform-ast* node)))
    (assert-eq 'lambda (car result))
    (assert-= 1 (length (second result)))   ; single param
    (assert-true (symbolp (car (second result))))))  ; param is gensym

;;; ─── cps-transform* ───────────────────────────────────────────────────────

(deftest cps-transform-star-sexp-returns-cons
  "cps-transform* on a plain s-expression returns a cons."
  (assert-true (consp (cl-cc/cps::cps-transform* '42))))

(deftest cps-transform-star-ast-node-returns-lambda-with-one-param
  "cps-transform* on an AST node returns a (lambda (k) ...) form with one parameter."
  (let* ((node (cl-cc/ast:make-ast-quote :value 'x))
         (result (cl-cc/cps::cps-transform* node)))
    (assert-eq 'lambda (car result))
    (assert-= 1 (length (second result)))))

;;; ─── %cps-lower-lambda-param ─────────────────────────────────────────────────

(deftest-each cps-lower-lambda-param-cases
  "%cps-lower-lambda-param: no-default → symbol; default-only → (name default); both → (name default svar)."
  :cases (("no-default"       (list 'x nil nil)
     'x)
    ("default-only"     (list 'y (cl-cc/ast:make-ast-int :value 42) nil)
     '(y 42))
    ("default-and-svar" (list 'z (cl-cc/ast:make-ast-int :value 0) 'z-p)
     '(z 0 z-p)))
  (slot expected)
  (assert-equal expected (cl-cc/cps::%cps-lower-lambda-param slot)))

;;; ─── %cps-extended-lambda-list ───────────────────────────────────────────────
;;; Verifies that keyword markers (&optional, &rest, &key) are preserved — this
;;; was a latent bug where (when optional (list '&optional) (mapcar ...)) discarded
;;; the marker because `when` returns only its last form.

(deftest-each cps-extended-lambda-list-cases
  "Lambda-list reconstruction preserves &optional, &rest, and &key markers."
  :cases (("required-only"
    '(a b) nil nil nil
    '(a b))
         ("with-optional"
    '(x) (list (list 'y nil nil)) nil nil
    '(x &optional y))
         ("with-optional-default"
    '(x) (list (list 'y (cl-cc/ast:make-ast-int :value 0) nil)) nil nil
    '(x &optional (y 0)))
         ("with-rest"
    '(x) nil 'rest nil
    '(x &rest rest))
         ("with-key"
    '() nil nil (list (list 'k nil nil))
    '(&key k))
         ("combined"
    '(a) (list (list 'b nil nil)) 'r (list (list 'c nil nil))
    '(a &optional b &rest r &key c)))
  (required optional rest key expected)
  (assert-equal expected
                (cl-cc/cps::%cps-extended-lambda-list required optional rest key)))

;;; ─── cps-transform* shared entrypoint ──────────────────────────────────────

(deftest cps-transform*-handles-ast-node-and-sexp
  "cps-transform* returns a truthy result for both AST nodes and plain s-expressions."
  (assert-true (cl-cc/cps::cps-transform* (cl-cc/ast:make-ast-int :value 1)))
  (assert-true (cl-cc/cps::cps-transform* '42)))

;;; ─── cps-transform-eval ───────────────────────────────────────────────────

(defun %cps-unwrap (v)
  "Apply identity to a CPS-transformed lambda to retrieve the underlying value.
cps-transform-eval returns the raw CPS lambda; other callers depend on that
shape, so we unwrap locally in these tests rather than changing the function."
  (if (functionp v) (funcall v #'identity) v))

(deftest cps-transform-eval-integer-literal-returns-value
  "cps-transform-eval on an integer literal evaluates to that integer."
  (assert-= 42 (%cps-unwrap (cl-cc/cps::cps-transform-eval '42))))

(deftest cps-transform-eval-arithmetic-expression-returns-result
  "cps-transform-eval on (+ 3 4) evaluates to 7."
  (assert-= 7 (%cps-unwrap (cl-cc/cps::cps-transform-eval '(+ 3 4)))))
