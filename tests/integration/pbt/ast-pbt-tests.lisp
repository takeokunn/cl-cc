;;;; tests/pbt/ast-pbt-tests.lisp - Property-Based Tests for AST Roundtrip
;;;
;;; This module provides property-based tests verifying that AST nodes
;;; can be converted to S-expressions and back without data loss.

(in-package :cl-cc/pbt)

(in-suite cl-cc-pbt-suite)

;;; Basic AST Roundtrip Tests

(deftest ast-int-roundtrip
  "Test integer AST roundtrip."
  (for-all ((value (gen-fn (gen-integer :min -10000 :max 10000))))
    (let* ((ast (make-ast-int :value value))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-int))
(assert-true (= (ast-int-value ast) (ast-int-value ast2))))))

(deftest ast-var-roundtrip
  "Test variable AST roundtrip."
  (for-all ((name (gen-fn (gen-symbol :package nil :prefix "VAR"))))
    (let* ((ast (make-ast-var :name name))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-var))
(assert-true (eq (ast-var-name ast) (ast-var-name ast2))))))

;;; Binary Operation Roundtrip

(deftest ast-binop-roundtrip
  "Test binary operation AST roundtrip."
  (for-all ((op (gen-fn (gen-one-of '(+ - *))))
                   (lhs-val (gen-fn (gen-integer :min -100 :max 100)))
                   (rhs-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((lhs (make-ast-int :value lhs-val))
           (rhs (make-ast-int :value rhs-val))
           (ast (make-ast-binop :op op :lhs lhs :rhs rhs))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-binop))
(assert-true (eq (ast-binop-op ast) (ast-binop-op ast2)))
(assert-true (typep (ast-binop-lhs ast2) 'ast-int))
(assert-true (typep (ast-binop-rhs ast2) 'ast-int))
(assert-true (= lhs-val (ast-int-value (ast-binop-lhs ast2))))
(assert-true (= rhs-val (ast-int-value (ast-binop-rhs ast2)))))))

;;; Conditional Roundtrip

(deftest ast-if-roundtrip
  "Test if expression AST roundtrip."
  (for-all ((cond-val (gen-fn (gen-integer :min 0 :max 1)))
                   (then-val (gen-fn (gen-integer :min -100 :max 100)))
                   (else-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((cond-ast (make-ast-int :value cond-val))
           (then-ast (make-ast-int :value then-val))
           (else-ast (make-ast-int :value else-val))
           (ast (make-ast-if :cond cond-ast :then then-ast :else else-ast))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-if))
(assert-true (typep (ast-if-cond ast2) 'ast-int))
(assert-true (typep (ast-if-then ast2) 'ast-int))
(assert-true (typep (ast-if-else ast2) 'ast-int))
(assert-true (= cond-val (ast-int-value (ast-if-cond ast2))))
(assert-true (= then-val (ast-int-value (ast-if-then ast2))))
(assert-true (= else-val (ast-int-value (ast-if-else ast2)))))))

;;; Sequence Roundtrip

(deftest ast-progn-roundtrip
  "Test progn AST roundtrip."
  (for-all ((vals (gen-fn (gen-list-of (gen-integer :min -100 :max 100)
                                              :min-length 1 :max-length 5))))
    (let* ((forms (mapcar (lambda (v) (make-ast-int :value v)) vals))
           (ast (make-ast-progn :forms forms))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-progn))
(assert-true (= (length vals) (length (ast-progn-forms ast2))))
(assert-true (every #'typep (ast-progn-forms ast2) (make-list (length vals) :initial-element 'ast-int)))
(assert-true (equal vals (mapcar #'ast-int-value (ast-progn-forms ast2)))))))

;;; Print Roundtrip

(deftest ast-print-roundtrip
  "Test print AST roundtrip."
  (for-all ((value (gen-fn (gen-integer :min -1000 :max 1000))))
    (let* ((expr (make-ast-int :value value))
           (ast (make-ast-print :expr expr))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-print))
(assert-true (typep (ast-print-expr ast2) 'ast-int))
(assert-true (= value (ast-int-value (ast-print-expr ast2)))))))

;;; Let Binding Roundtrip

(deftest ast-let-roundtrip
  "Test let binding AST roundtrip."
  (for-all ((var-name (gen-fn (gen-symbol :package nil :prefix "VAR")))
                   (init-val (gen-fn (gen-integer :min -100 :max 100)))
                   (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((init-ast (make-ast-int :value init-val))
           (var-ref (make-ast-var :name var-name))
           (body-ast (make-ast-int :value body-val))
           (ast (make-ast-let
                               :bindings (list (cons var-name init-ast))
                               :body (list var-ref body-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-let))
(assert-true (= 1 (length (ast-let-bindings ast2))))
(assert-true (eq var-name (car (first (ast-let-bindings ast2)))))
(assert-true (= init-val (ast-int-value (cdr (first (ast-let-bindings ast2))))))
(assert-true (= 2 (length (ast-let-body ast2)))))))

(deftest ast-let-empty-bindings-roundtrip
  "Test let with empty bindings roundtrip."
  (for-all ((body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((body-ast (make-ast-int :value body-val))
           (ast (make-ast-let :bindings nil :body (list body-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-let))
(assert-true (null (ast-let-bindings ast2)))
(assert-true (= 1 (length (ast-let-body ast2))))
(assert-true (= body-val (ast-int-value (first (ast-let-body ast2))))))))

;;; Lambda Roundtrip

(deftest ast-lambda-roundtrip
  "Test lambda AST roundtrip."
  (for-all ((params (gen-fn (gen-list-of (gen-symbol :package nil :prefix "ARG")
                                                :min-length 0 :max-length 4)))
                   (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((body-ast (make-ast-int :value body-val))
           (ast (make-ast-lambda :params params :body (list body-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-lambda))
(assert-true (equal params (ast-lambda-params ast2)))
(assert-true (= 1 (length (ast-lambda-body ast2))))
(assert-true (= body-val (ast-int-value (first (ast-lambda-body ast2))))))))

;;; Function Reference Roundtrip

(deftest ast-function-roundtrip
  "Test function reference AST roundtrip."
  (for-all ((name (gen-fn (gen-symbol :package nil :prefix "FN"))))
    (let* ((ast (make-ast-function :name name))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-function))
(assert-true (eq name (ast-function-name ast2))))))

(deftest ast-function-setf-roundtrip
  "Test function reference with setf name roundtrip."
  (let* ((name '(setf accessor))
         (ast (make-ast-function :name name))
         (sexp (ast-to-sexp ast))
         (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-function))
(assert-true (equal name (ast-function-name ast2)))))

;;; Flet Roundtrip

(deftest ast-flet-roundtrip
  "Test flet AST roundtrip."
  (for-all ((fn-name (gen-fn (gen-symbol :package nil :prefix "FN")))
                   (param (gen-fn (gen-symbol :package nil :prefix "ARG")))
                   (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((body-ast (make-ast-int :value body-val))
           (ast (make-ast-flet
                               :bindings (list (list* fn-name (list param) (list body-ast)))
                               :body (list (make-ast-var :name fn-name))))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-flet))
(assert-true (= 1 (length (ast-flet-bindings ast2))))
(assert-true (eq fn-name (first (first (ast-flet-bindings ast2)))))
(assert-true (equal (list param) (second (first (ast-flet-bindings ast2)))))
(assert-true (= body-val (ast-int-value (third (first (ast-flet-bindings ast2)))))))))

;;; Labels Roundtrip

(deftest ast-labels-roundtrip
  "Test labels AST roundtrip."
  (for-all ((fn1-name (gen-fn (gen-symbol :package nil :prefix "FN1")))
                   (fn2-name (gen-fn (gen-symbol :package nil :prefix "FN2")))
                   (param (gen-fn (gen-symbol :package nil :prefix "ARG")))
                   (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((body-ast (make-ast-int :value body-val))
           (ast (make-ast-labels
                               :bindings (list (list* fn1-name (list param) (list body-ast))
                                               (list* fn2-name (list param) (list body-ast)))
                               :body (list (make-ast-var :name fn1-name))))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-labels))
(assert-true (= 2 (length (ast-labels-bindings ast2))))
(assert-true (eq fn1-name (first (first (ast-labels-bindings ast2)))))
(assert-true (eq fn2-name (first (second (ast-labels-bindings ast2))))))))

;;; Block Roundtrip

(deftest ast-block-roundtrip
  "Test block AST roundtrip."
  (for-all ((name (gen-fn (gen-symbol :package nil :prefix "BLOCK")))
                   (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((body-ast (make-ast-int :value body-val))
           (ast (make-ast-block :name name :body (list body-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-block))
(assert-true (eq name (ast-block-name ast2)))
(assert-true (= 1 (length (ast-block-body ast2))))
(assert-true (= body-val (ast-int-value (first (ast-block-body ast2))))))))

;;; Return-From Roundtrip

(deftest ast-return-from-roundtrip
  "Test return-from AST roundtrip."
  (for-all ((name (gen-fn (gen-symbol :package nil :prefix "BLOCK")))
                   (value (gen-fn (gen-integer :min -1000 :max 1000))))
    (let* ((value-ast (make-ast-int :value value))
           (ast (make-ast-return-from :name name :value value-ast))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-return-from))
(assert-true (eq name (ast-return-from-name ast2)))
(assert-true (typep (ast-return-from-value ast2) 'ast-int))
(assert-true (= value (ast-int-value (ast-return-from-value ast2)))))))

;;; Tagbody Roundtrip

(deftest ast-tagbody-roundtrip
  "Test tagbody AST roundtrip - verifies tagbody structure survives roundtrip."
  (for-all ((tag-val (gen-fn (gen-integer :min 0 :max 100))))
    (let* ((body-ast (make-ast-var :name 'x))
           (ast (make-ast-tagbody :tags (list (cons tag-val (list body-ast)))))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-tagbody))
      ;; After roundtrip, tag should be present
(assert-true (not (null (ast-tagbody-tags ast2)))))))

(deftest ast-tagbody-integer-tag-roundtrip
  "Test tagbody with integer tag roundtrip."
  (for-all ((body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((body-ast (make-ast-var :name 'x))
           (ast (make-ast-tagbody :tags (list (cons 0 (list body-ast)))))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-tagbody))
      ;; Tag 0 should be present
(assert-true (not (null (ast-tagbody-tags ast2)))))))

;;; Go Roundtrip

(deftest ast-go-roundtrip
  "Test go AST roundtrip."
  (for-all ((tag (gen-fn (gen-symbol :package nil :prefix "TAG"))))
    (let* ((ast (make-ast-go :tag tag))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-go))
(assert-true (eq tag (ast-go-tag ast2))))))

(deftest ast-go-integer-tag-roundtrip
  "Test go with integer tag roundtrip."
  (let* ((ast (make-ast-go :tag 42))
         (sexp (ast-to-sexp ast))
         (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-go))
(assert-true (= 42 (ast-go-tag ast2)))))

;;; Setq Roundtrip

(deftest ast-setq-roundtrip
  "Test setq AST roundtrip."
  (for-all ((var (gen-fn (gen-symbol :package nil :prefix "VAR")))
                   (value (gen-fn (gen-integer :min -1000 :max 1000))))
    (let* ((value-ast (make-ast-int :value value))
           (ast (make-ast-setq :var var :value value-ast))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-setq))
(assert-true (eq var (ast-setq-var ast2)))
(assert-true (typep (ast-setq-value ast2) 'ast-int))
(assert-true (= value (ast-int-value (ast-setq-value ast2)))))))

;;; Multiple-Value-Call Roundtrip

(deftest ast-multiple-value-call-roundtrip
  "Test multiple-value-call AST roundtrip."
  (for-all ((fn-name (gen-fn (gen-symbol :package nil :prefix "FN")))
                   (arg-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((func-ast (make-ast-var :name fn-name))
           (arg-ast (make-ast-int :value arg-val))
           (ast (make-ast-multiple-value-call
                               :func func-ast
                               :args (list arg-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-multiple-value-call))
(assert-true (typep (ast-mv-call-func ast2) 'ast-var))
(assert-true (eq fn-name (ast-var-name (ast-mv-call-func ast2))))
(assert-true (= 1 (length (ast-mv-call-args ast2)))))))

;;; Multiple-Value-Prog1 Roundtrip

(deftest ast-multiple-value-prog1-roundtrip
  "Test multiple-value-prog1 AST roundtrip."
  (for-all ((first-val (gen-fn (gen-integer :min -100 :max 100)))
                   (form-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((first-ast (make-ast-int :value first-val))
           (form-ast (make-ast-int :value form-val))
           (ast (make-ast-multiple-value-prog1
                               :first first-ast
                               :forms (list form-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-multiple-value-prog1))
(assert-true (typep (ast-mv-prog1-first ast2) 'ast-int))
(assert-true (= first-val (ast-int-value (ast-mv-prog1-first ast2))))
(assert-true (= 1 (length (ast-mv-prog1-forms ast2)))))))

;;; Catch Roundtrip

(deftest ast-catch-roundtrip
  "Test catch AST roundtrip."
  (for-all ((tag (gen-fn (gen-symbol :package :keyword :prefix "TAG")))
                   (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((tag-ast (make-ast-var :name tag))
           (body-ast (make-ast-int :value body-val))
           (ast (make-ast-catch :tag tag-ast :body (list body-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-catch))
(assert-true (typep (ast-catch-tag ast2) 'ast-var))
(assert-true (= 1 (length (ast-catch-body ast2)))))))

;;; Throw Roundtrip

(deftest ast-throw-roundtrip
  "Test throw AST roundtrip."
  (for-all ((tag (gen-fn (gen-symbol :package :keyword :prefix "TAG")))
                   (value (gen-fn (gen-integer :min -1000 :max 1000))))
    (let* ((tag-ast (make-ast-var :name tag))
           (value-ast (make-ast-int :value value))
           (ast (make-ast-throw :tag tag-ast :value value-ast))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-throw))
(assert-true (typep (ast-throw-tag ast2) 'ast-var))
(assert-true (typep (ast-throw-value ast2) 'ast-int))
(assert-true (= value (ast-int-value (ast-throw-value ast2)))))))

;;; Unwind-Protect Roundtrip

(deftest ast-unwind-protect-roundtrip
  "Test unwind-protect AST roundtrip."
  (for-all ((protected-val (gen-fn (gen-integer :min -100 :max 100)))
                   (cleanup-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((protected-ast (make-ast-int :value protected-val))
           (cleanup-ast (make-ast-int :value cleanup-val))
           (ast (make-ast-unwind-protect
                               :protected protected-ast
                               :cleanup (list cleanup-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-unwind-protect))
(assert-true (typep (ast-unwind-protected ast2) 'ast-int))
(assert-true (= protected-val (ast-int-value (ast-unwind-protected ast2))))
(assert-true (= 1 (length (ast-unwind-cleanup ast2)))))))

;;; Function Call Roundtrip

(deftest ast-call-roundtrip
  "Test function call AST roundtrip."
  (for-all ((fn-name (gen-fn (gen-symbol :prefix "FN")))
                   (args (gen-fn (gen-list-of (gen-integer :min -100 :max 100)
                                              :min-length 1 :max-length 5))))
    (let* ((arg-asts (mapcar (lambda (v) (make-ast-int :value v)) args))
           (ast (make-ast-call :func fn-name :args arg-asts))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-call))
(assert-true (= (length args) (length (ast-call-args ast2))))
(assert-true (equal args (mapcar #'ast-int-value (ast-call-args ast2)))))))

;;; Quote Roundtrip

(deftest ast-quote-roundtrip
  "Test quote AST roundtrip."
  (for-all ((value (gen-fn (gen-one-of '(nil t 42 "string" (a b c))))))
    (let* ((ast (make-ast-quote :value value))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-quote))
(assert-true (equal value (ast-quote-value ast2))))))

(deftest ast-quote-symbol-roundtrip
  "Test quote with symbol roundtrip."
  (for-all ((sym (gen-fn (gen-symbol :package nil :prefix "SYM"))))
    (let* ((ast (make-ast-quote :value sym))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-quote))
(assert-true (eq sym (ast-quote-value ast2))))))

(deftest ast-quote-list-roundtrip
  "Test quote with list roundtrip."
  (let* ((value '(a (b c) d))
         (ast (make-ast-quote :value value))
         (sexp (ast-to-sexp ast))
         (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-quote))
(assert-true (equal value (ast-quote-value ast2)))))

;;; The Type Declaration Roundtrip

(deftest ast-the-roundtrip
  "Test the type declaration AST roundtrip."
  (for-all ((value (gen-fn (gen-integer :min -1000 :max 1000)))
                   (type (gen-fn (gen-one-of '(integer fixnum number)))))
    (let* ((value-ast (make-ast-int :value value))
           (ast (make-ast-the :type type :value value-ast))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-the))
(assert-true (eq type (ast-the-type ast2)))
(assert-true (typep (ast-the-value ast2) 'ast-int))
(assert-true (= value (ast-int-value (ast-the-value ast2)))))))

;;; Nested AST Roundtrip Tests

(deftest nested-if-roundtrip
  "Test nested if expressions roundtrip."
  (for-all ((v1 (gen-fn (gen-integer :min 0 :max 1)))
                   (v2 (gen-fn (gen-integer :min 0 :max 1)))
                   (v3 (gen-fn (gen-integer :min -10 :max 10)))
                   (v4 (gen-fn (gen-integer :min -10 :max 10)))
                   (v5 (gen-fn (gen-integer :min -10 :max 10))))
    (let* ((inner-if (make-ast-if
                                     :cond (make-ast-int :value v2)
                                     :then (make-ast-int :value v3)
                                     :else (make-ast-int :value v4)))
           (ast (make-ast-if
                               :cond (make-ast-int :value v1)
                               :then inner-if
                               :else (make-ast-int :value v5)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-if))
(assert-true (typep (ast-if-then ast2) 'ast-if))
(assert-true (= v1 (ast-int-value (ast-if-cond ast2))))
(assert-true (= v2 (ast-int-value (ast-if-cond (ast-if-then ast2)))))
(assert-true (= v3 (ast-int-value (ast-if-then (ast-if-then ast2)))))
(assert-true (= v4 (ast-int-value (ast-if-else (ast-if-then ast2)))))
(assert-true (= v5 (ast-int-value (ast-if-else ast2)))))))

(deftest nested-binop-roundtrip
  "Test nested binary operations roundtrip."
  (for-all ((v1 (gen-fn (gen-integer :min -10 :max 10)))
                   (v2 (gen-fn (gen-integer :min -10 :max 10)))
                   (v3 (gen-fn (gen-integer :min -10 :max 10))))
    (let* ((inner (make-ast-binop
                                  :op '+
                                  :lhs (make-ast-int :value v1)
                                  :rhs (make-ast-int :value v2)))
           (ast (make-ast-binop
                               :op '*
                               :lhs inner
                               :rhs (make-ast-int :value v3)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-binop))
(assert-true (eq '* (ast-binop-op ast2)))
(assert-true (typep (ast-binop-lhs ast2) 'ast-binop))
(assert-true (eq '+ (ast-binop-op (ast-binop-lhs ast2))))
(assert-true (= v1 (ast-int-value (ast-binop-lhs (ast-binop-lhs ast2)))))
(assert-true (= v2 (ast-int-value (ast-binop-rhs (ast-binop-lhs ast2)))))
(assert-true (= v3 (ast-int-value (ast-binop-rhs ast2)))))))

(deftest nested-let-roundtrip
  "Test nested let expressions roundtrip."
  (for-all ((var1 (gen-fn (gen-symbol :package nil :prefix "X")))
                   (var2 (gen-fn (gen-symbol :package nil :prefix "Y")))
                   (v1 (gen-fn (gen-integer :min -10 :max 10)))
                   (v2 (gen-fn (gen-integer :min -10 :max 10))))
    (let* ((inner-let (make-ast-let
                                      :bindings (list (cons var2 (make-ast-int :value v2)))
                                      :body (list (make-ast-var :name var2))))
           (ast (make-ast-let
                               :bindings (list (cons var1 (make-ast-int :value v1)))
                               :body (list inner-let)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
(assert-true (typep ast2 'ast-let))
(assert-true (eq var1 (car (first (ast-let-bindings ast2)))))
(assert-true (= v1 (ast-int-value (cdr (first (ast-let-bindings ast2))))))
(assert-true (= 1 (length (ast-let-body ast2))))
(assert-true (typep (first (ast-let-body ast2)) 'ast-let))
(assert-true (eq var2 (car (first (ast-let-bindings (first (ast-let-body ast2))))))))))
