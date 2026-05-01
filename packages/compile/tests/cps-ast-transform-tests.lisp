;;;; tests/unit/compile/cps-ast-transform-tests.lisp — CPS Transform Dispatch + Control Tests
;;;;
;;;; Tests for cps-transform* dispatch, sequence/simplify helpers,
;;;; control flow (block/catch/return-from/throw/tagbody), local function
;;;; bindings, and unwind-protect CPS transformation.
;;;; Suite: cps-ast-suite (defined elsewhere, used by in-suite).

(in-package :cl-cc/test)

(in-suite cps-ast-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; cps-transform* dispatcher
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cps-transform*-dispatch
  "cps-transform* dispatches to AST transformer for AST nodes and sexp transformer otherwise."
  :cases (("ast-node" (cl-cc:make-ast-int :value 42))
          ("sexp"     '(+ 1 2)))
  (input)
  (assert-true (is-cps-lambda (cl-cc:cps-transform* input))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cps-transform-sequence edge cases
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-sequence-behavior
  "cps-transform-sequence: empty→(funcall k nil); single→direct CPS form."
  (let ((k-var (gensym "K")))
    ;; empty: calls continuation with nil
    (let ((sexp (cl-cc:cps-transform-sequence nil k-var)))
      (assert-true (listp sexp))
      (assert-eq 'funcall (car sexp)))
    ;; single form: delegates to cps-transform-ast (still a list)
    (let ((sexp (cl-cc:cps-transform-sequence (list (cl-cc:make-ast-int :value 5)) k-var)))
      (assert-true (listp sexp)))))

(deftest cps-simplify-fixed-point-stops-on-stable-form
  "%cps-simplify-fixed-point keeps applying a step until the form stabilizes."
  (let ((calls 0))
    (assert-equal 'done
                  (cl-cc/cps::%cps-simplify-fixed-point
                   'start
                   (lambda (form)
                     (incf calls)
                     (if (eq form 'start) 'done 'done))))
    (assert-= 2 calls)))

(deftest cps-dispatch-table-covers-bootstrap-special-forms
  "The bootstrap CPS dispatch table keeps handlers for every supported special form."
  (dolist (operator '(+ - * if progn let print))
    (assert-true (functionp (gethash operator cl-cc/cps::*cps-sexp-dispatch-table*)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for block / return-from
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cps-control-outer-car
  "CPS-transformed block/catch nodes produce expected outer form keyword."
  :cases (("block" (cl-cc:make-ast-block :name 'nil :body (list (cl-cc:make-ast-int :value 1))) 'block)
          ("catch" (cl-cc:make-ast-catch :tag  (cl-cc:make-ast-quote :value :done)
                                         :body (list (cl-cc:make-ast-int :value 0)))            'funcall))
  (node expected-car)
  (let* ((k      (gensym "K"))
         (result (cl-cc/cps::cps-transform-ast node k)))
    (assert-eq expected-car (car result))))

(deftest-each cps-control-contains-token
  "CPS-transformed return-from/throw contain expected operator tokens in printed output."
  :cases (("return-from" (cl-cc:make-ast-return-from :name 'nil
                                                     :value (cl-cc:make-ast-int :value 42))
                         "RETURN-FROM")
          ("throw"       (cl-cc:make-ast-throw :tag   (cl-cc:make-ast-quote :value :done)
                                               :value (cl-cc:make-ast-int :value 99))
                         "THROW"))
  (node expected-token)
  (let* ((k      (gensym "K"))
         (result (format nil "~S" (cl-cc/cps::cps-transform-ast node k))))
    (assert-true (search expected-token result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for tagbody-section
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-tagbody-section-behavior
  "cps-transform-tagbody-section: empty→continue-form; single form→funcall wrapper."
  (let ((continue-form '(go next-tag)))
    ;; empty: returns the continuation form directly
    (let ((result (cl-cc/cps::cps-transform-tagbody-section nil continue-form)))
      (assert-equal continue-form result))
    ;; single form: CPS wrapper is a funcall
    (let ((result (cl-cc/cps::cps-transform-tagbody-section
                   (list (cl-cc:make-ast-int :value 1)) continue-form)))
      (assert-eq 'funcall (car result)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for local function bindings (flet/labels helpers)
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-fn-binding-structure
  "cps-transform-fn-binding: produces valid FLET/LABELS binding (name (params k) body)."
  (let* ((k-var 'my-k)
         (binding (list 'my-fn '(a b) (cl-cc:make-ast-int :value 42)))
         (result (cl-cc/cps::cps-transform-fn-binding binding k-var)))
    (assert-eq 'my-fn (first result))
    (let ((lambda-list (second result)))
      (assert-eq 'a    (first  lambda-list))
      (assert-eq 'b    (second lambda-list))
      (assert-eq 'my-k (third  lambda-list)))))

(deftest-each cps-local-fns-outer-is-form-kw
  "cps-transform-local-fns produces (flet ...) or (labels ...) as the outer form."
  :cases (("flet"   'flet)
          ("labels" 'labels))
  (form-kw)
  (let* ((k    (gensym "K"))
         (body (list (cl-cc:make-ast-int :value 42))))
    (assert-eq form-kw (first (cl-cc/cps::cps-transform-local-fns form-kw nil body k)))))

(deftest cps-local-fns-bindings-transformed
  "cps-transform-local-fns applies cps-transform-fn-binding to each binding."
  (let* ((k (gensym "K"))
         (body (list (cl-cc:make-ast-int :value 1)))
         ;; binding = (f (x) <ast-node>)
         (binding (list 'f '(x) (cl-cc:make-ast-int :value 99)))
         (result (cl-cc/cps::cps-transform-local-fns
                  'flet (list binding) body k)))
    ;; second element is the binding list: ((f (x K) ...))
    (assert-eq 'f (first (first (second result))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for unwind-protect
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-unwind-protect-structure
  "ast-unwind-protect CPS: outer is (unwind-protect ...) and nil cleanup emits nil."
  (let ((k (gensym "K")))
    (let* ((node (cl-cc:make-ast-unwind-protect
                  :protected (cl-cc:make-ast-int :value 1)
                  :cleanup   (list (cl-cc:make-ast-int :value 2))))
           (result (cl-cc/cps::cps-transform-ast node k)))
      (assert-eq 'unwind-protect (first result)))
    (let* ((node (cl-cc:make-ast-unwind-protect
                  :protected (cl-cc:make-ast-int :value 1)
                  :cleanup   nil))
           (result (cl-cc/cps::cps-transform-ast node k)))
      (assert-eq nil (third result)))))
