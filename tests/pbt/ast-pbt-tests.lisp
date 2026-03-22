;;;; tests/pbt/ast-pbt-tests.lisp - Property-Based Tests for AST Roundtrip
;;;
;;; This module provides property-based tests verifying that AST nodes
;;; can be converted to S-expressions and back without data loss.

(in-package :cl-cc/pbt)

(in-suite cl-cc-pbt-suite)

;;; ----------------------------------------------------------------------------
;;; Basic AST Roundtrip Tests
;;; ----------------------------------------------------------------------------

(test ast-int-roundtrip
  "Test integer AST roundtrip."
  (fiveam:for-all ((value (gen-fn (gen-integer :min -10000 :max 10000))))
    (let* ((ast (make-instance 'ast-int :value value))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-int))
      (is (= (ast-int-value ast) (ast-int-value ast2))))))

(test ast-var-roundtrip
  "Test variable AST roundtrip."
  (fiveam:for-all ((name (gen-fn (gen-symbol :package nil :prefix "VAR"))))
    (let* ((ast (make-instance 'ast-var :name name))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-var))
      (is (eq (ast-var-name ast) (ast-var-name ast2))))))

;;; ----------------------------------------------------------------------------
;;; Binary Operation Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-binop-roundtrip
  "Test binary operation AST roundtrip."
  (fiveam:for-all ((op (gen-fn (gen-one-of '(+ - *))))
                   (lhs-val (gen-fn (gen-integer :min -100 :max 100)))
                   (rhs-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((lhs (make-instance 'ast-int :value lhs-val))
           (rhs (make-instance 'ast-int :value rhs-val))
           (ast (make-instance 'ast-binop :op op :lhs lhs :rhs rhs))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-binop))
      (is (eq (ast-binop-op ast) (ast-binop-op ast2)))
      (is (typep (ast-binop-lhs ast2) 'ast-int))
      (is (typep (ast-binop-rhs ast2) 'ast-int))
      (is (= lhs-val (ast-int-value (ast-binop-lhs ast2))))
      (is (= rhs-val (ast-int-value (ast-binop-rhs ast2)))))))

;;; ----------------------------------------------------------------------------
;;; Conditional Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-if-roundtrip
  "Test if expression AST roundtrip."
  (fiveam:for-all ((cond-val (gen-fn (gen-integer :min 0 :max 1)))
                   (then-val (gen-fn (gen-integer :min -100 :max 100)))
                   (else-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((cond-ast (make-instance 'ast-int :value cond-val))
           (then-ast (make-instance 'ast-int :value then-val))
           (else-ast (make-instance 'ast-int :value else-val))
           (ast (make-instance 'ast-if :cond cond-ast :then then-ast :else else-ast))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-if))
      (is (typep (ast-if-cond ast2) 'ast-int))
      (is (typep (ast-if-then ast2) 'ast-int))
      (is (typep (ast-if-else ast2) 'ast-int))
      (is (= cond-val (ast-int-value (ast-if-cond ast2))))
      (is (= then-val (ast-int-value (ast-if-then ast2))))
      (is (= else-val (ast-int-value (ast-if-else ast2)))))))

;;; ----------------------------------------------------------------------------
;;; Sequence Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-progn-roundtrip
  "Test progn AST roundtrip."
  (fiveam:for-all ((vals (gen-fn (gen-list-of (gen-integer :min -100 :max 100)
                                              :min-length 1 :max-length 5))))
    (let* ((forms (mapcar (lambda (v) (make-instance 'ast-int :value v)) vals))
           (ast (make-instance 'ast-progn :forms forms))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-progn))
      (is (= (length vals) (length (ast-progn-forms ast2))))
      (is (every #'typep (ast-progn-forms ast2) (make-list (length vals) :initial-element 'ast-int)))
      (is (equal vals (mapcar #'ast-int-value (ast-progn-forms ast2)))))))

;;; ----------------------------------------------------------------------------
;;; Print Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-print-roundtrip
  "Test print AST roundtrip."
  (fiveam:for-all ((value (gen-fn (gen-integer :min -1000 :max 1000))))
    (let* ((expr (make-instance 'ast-int :value value))
           (ast (make-instance 'ast-print :expr expr))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-print))
      (is (typep (ast-print-expr ast2) 'ast-int))
      (is (= value (ast-int-value (ast-print-expr ast2)))))))

;;; ----------------------------------------------------------------------------
;;; Let Binding Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-let-roundtrip
  "Test let binding AST roundtrip."
  (fiveam:for-all ((var-name (gen-fn (gen-symbol :package nil :prefix "VAR")))
                   (init-val (gen-fn (gen-integer :min -100 :max 100)))
                   (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((init-ast (make-instance 'ast-int :value init-val))
           (var-ref (make-instance 'ast-var :name var-name))
           (body-ast (make-instance 'ast-int :value body-val))
           (ast (make-instance 'ast-let
                               :bindings (list (cons var-name init-ast))
                               :body (list var-ref body-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-let))
      (is (= 1 (length (ast-let-bindings ast2))))
      (is (eq var-name (car (first (ast-let-bindings ast2)))))
      (is (= init-val (ast-int-value (cdr (first (ast-let-bindings ast2))))))
      (is (= 2 (length (ast-let-body ast2)))))))

(test ast-let-empty-bindings-roundtrip
  "Test let with empty bindings roundtrip."
  (fiveam:for-all ((body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((body-ast (make-instance 'ast-int :value body-val))
           (ast (make-instance 'ast-let :bindings nil :body (list body-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-let))
      (is (null (ast-let-bindings ast2)))
      (is (= 1 (length (ast-let-body ast2))))
      (is (= body-val (ast-int-value (first (ast-let-body ast2))))))))

;;; ----------------------------------------------------------------------------
;;; Lambda Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-lambda-roundtrip
  "Test lambda AST roundtrip."
  (fiveam:for-all ((params (gen-fn (gen-list-of (gen-symbol :package nil :prefix "ARG")
                                                :min-length 0 :max-length 4)))
                   (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((body-ast (make-instance 'ast-int :value body-val))
           (ast (make-instance 'ast-lambda :params params :body (list body-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-lambda))
      (is (equal params (ast-lambda-params ast2)))
      (is (= 1 (length (ast-lambda-body ast2))))
      (is (= body-val (ast-int-value (first (ast-lambda-body ast2))))))))

;;; ----------------------------------------------------------------------------
;;; Function Reference Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-function-roundtrip
  "Test function reference AST roundtrip."
  (fiveam:for-all ((name (gen-fn (gen-symbol :package nil :prefix "FN"))))
    (let* ((ast (make-instance 'ast-function :name name))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-function))
      (is (eq name (ast-function-name ast2))))))

(test ast-function-setf-roundtrip
  "Test function reference with setf name roundtrip."
  (let* ((name '(setf accessor))
         (ast (make-instance 'ast-function :name name))
         (sexp (ast-to-sexp ast))
         (ast2 (lower-sexp-to-ast sexp)))
    (is (typep ast2 'ast-function))
    (is (equal name (ast-function-name ast2)))))

;;; ----------------------------------------------------------------------------
;;; Flet Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-flet-roundtrip
  "Test flet AST roundtrip."
  (fiveam:for-all ((fn-name (gen-fn (gen-symbol :package nil :prefix "FN")))
                   (param (gen-fn (gen-symbol :package nil :prefix "ARG")))
                   (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((body-ast (make-instance 'ast-int :value body-val))
           (ast (make-instance 'ast-flet
                               :bindings (list (list* fn-name (list param) (list body-ast)))
                               :body (list (make-instance 'ast-var :name fn-name))))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-flet))
      (is (= 1 (length (ast-flet-bindings ast2))))
      (is (eq fn-name (first (first (ast-flet-bindings ast2)))))
      (is (equal (list param) (second (first (ast-flet-bindings ast2)))))
      (is (= body-val (ast-int-value (third (first (ast-flet-bindings ast2)))))))))

;;; ----------------------------------------------------------------------------
;;; Labels Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-labels-roundtrip
  "Test labels AST roundtrip."
  (fiveam:for-all ((fn1-name (gen-fn (gen-symbol :package nil :prefix "FN1")))
                   (fn2-name (gen-fn (gen-symbol :package nil :prefix "FN2")))
                   (param (gen-fn (gen-symbol :package nil :prefix "ARG")))
                   (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((body-ast (make-instance 'ast-int :value body-val))
           (ast (make-instance 'ast-labels
                               :bindings (list (list* fn1-name (list param) (list body-ast))
                                               (list* fn2-name (list param) (list body-ast)))
                               :body (list (make-instance 'ast-var :name fn1-name))))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-labels))
      (is (= 2 (length (ast-labels-bindings ast2))))
      (is (eq fn1-name (first (first (ast-labels-bindings ast2)))))
      (is (eq fn2-name (first (second (ast-labels-bindings ast2))))))))

;;; ----------------------------------------------------------------------------
;;; Block Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-block-roundtrip
  "Test block AST roundtrip."
  (fiveam:for-all ((name (gen-fn (gen-symbol :package nil :prefix "BLOCK")))
                   (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((body-ast (make-instance 'ast-int :value body-val))
           (ast (make-instance 'ast-block :name name :body (list body-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-block))
      (is (eq name (ast-block-name ast2)))
      (is (= 1 (length (ast-block-body ast2))))
      (is (= body-val (ast-int-value (first (ast-block-body ast2))))))))

;;; ----------------------------------------------------------------------------
;;; Return-From Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-return-from-roundtrip
  "Test return-from AST roundtrip."
  (fiveam:for-all ((name (gen-fn (gen-symbol :package nil :prefix "BLOCK")))
                   (value (gen-fn (gen-integer :min -1000 :max 1000))))
    (let* ((value-ast (make-instance 'ast-int :value value))
           (ast (make-instance 'ast-return-from :name name :value value-ast))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-return-from))
      (is (eq name (ast-return-from-name ast2)))
      (is (typep (ast-return-from-value ast2) 'ast-int))
      (is (= value (ast-int-value (ast-return-from-value ast2)))))))

;;; ----------------------------------------------------------------------------
;;; Tagbody Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-tagbody-roundtrip
  "Test tagbody AST roundtrip - verifies tagbody structure survives roundtrip."
  (fiveam:for-all ((tag-val (gen-fn (gen-integer :min 0 :max 100))))
    (let* ((body-ast (make-instance 'ast-var :name 'x))
           (ast (make-instance 'ast-tagbody :tags (list (cons tag-val (list body-ast)))))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-tagbody))
      ;; After roundtrip, tag should be present
      (is (not (null (ast-tagbody-tags ast2)))))))

(test ast-tagbody-integer-tag-roundtrip
  "Test tagbody with integer tag roundtrip."
  (fiveam:for-all ((body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((body-ast (make-instance 'ast-var :name 'x))
           (ast (make-instance 'ast-tagbody :tags (list (cons 0 (list body-ast)))))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-tagbody))
      ;; Tag 0 should be present
      (is (not (null (ast-tagbody-tags ast2)))))))

;;; ----------------------------------------------------------------------------
;;; Go Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-go-roundtrip
  "Test go AST roundtrip."
  (fiveam:for-all ((tag (gen-fn (gen-symbol :package nil :prefix "TAG"))))
    (let* ((ast (make-instance 'ast-go :tag tag))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-go))
      (is (eq tag (ast-go-tag ast2))))))

(test ast-go-integer-tag-roundtrip
  "Test go with integer tag roundtrip."
  (let* ((ast (make-instance 'ast-go :tag 42))
         (sexp (ast-to-sexp ast))
         (ast2 (lower-sexp-to-ast sexp)))
    (is (typep ast2 'ast-go))
    (is (= 42 (ast-go-tag ast2)))))

;;; ----------------------------------------------------------------------------
;;; Setq Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-setq-roundtrip
  "Test setq AST roundtrip."
  (fiveam:for-all ((var (gen-fn (gen-symbol :package nil :prefix "VAR")))
                   (value (gen-fn (gen-integer :min -1000 :max 1000))))
    (let* ((value-ast (make-instance 'ast-int :value value))
           (ast (make-instance 'ast-setq :var var :value value-ast))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-setq))
      (is (eq var (ast-setq-var ast2)))
      (is (typep (ast-setq-value ast2) 'ast-int))
      (is (= value (ast-int-value (ast-setq-value ast2)))))))

;;; ----------------------------------------------------------------------------
;;; Multiple-Value-Call Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-multiple-value-call-roundtrip
  "Test multiple-value-call AST roundtrip."
  (fiveam:for-all ((fn-name (gen-fn (gen-symbol :package nil :prefix "FN")))
                   (arg-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((func-ast (make-instance 'ast-var :name fn-name))
           (arg-ast (make-instance 'ast-int :value arg-val))
           (ast (make-instance 'ast-multiple-value-call
                               :func func-ast
                               :args (list arg-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-multiple-value-call))
      (is (typep (ast-mv-call-func ast2) 'ast-var))
      (is (eq fn-name (ast-var-name (ast-mv-call-func ast2))))
      (is (= 1 (length (ast-mv-call-args ast2)))))))

;;; ----------------------------------------------------------------------------
;;; Multiple-Value-Prog1 Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-multiple-value-prog1-roundtrip
  "Test multiple-value-prog1 AST roundtrip."
  (fiveam:for-all ((first-val (gen-fn (gen-integer :min -100 :max 100)))
                   (form-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((first-ast (make-instance 'ast-int :value first-val))
           (form-ast (make-instance 'ast-int :value form-val))
           (ast (make-instance 'ast-multiple-value-prog1
                               :first-form first-ast
                               :forms (list form-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-multiple-value-prog1))
      (is (typep (ast-mv-prog1-first ast2) 'ast-int))
      (is (= first-val (ast-int-value (ast-mv-prog1-first ast2))))
      (is (= 1 (length (ast-mv-prog1-forms ast2)))))))

;;; ----------------------------------------------------------------------------
;;; Catch Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-catch-roundtrip
  "Test catch AST roundtrip."
  (fiveam:for-all ((tag (gen-fn (gen-symbol :package :keyword :prefix "TAG")))
                   (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((tag-ast (make-instance 'ast-var :name tag))
           (body-ast (make-instance 'ast-int :value body-val))
           (ast (make-instance 'ast-catch :tag tag-ast :body (list body-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-catch))
      (is (typep (ast-catch-tag ast2) 'ast-var))
      (is (= 1 (length (ast-catch-body ast2)))))))

;;; ----------------------------------------------------------------------------
;;; Throw Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-throw-roundtrip
  "Test throw AST roundtrip."
  (fiveam:for-all ((tag (gen-fn (gen-symbol :package :keyword :prefix "TAG")))
                   (value (gen-fn (gen-integer :min -1000 :max 1000))))
    (let* ((tag-ast (make-instance 'ast-var :name tag))
           (value-ast (make-instance 'ast-int :value value))
           (ast (make-instance 'ast-throw :tag tag-ast :value value-ast))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-throw))
      (is (typep (ast-throw-tag ast2) 'ast-var))
      (is (typep (ast-throw-value ast2) 'ast-int))
      (is (= value (ast-int-value (ast-throw-value ast2)))))))

;;; ----------------------------------------------------------------------------
;;; Unwind-Protect Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-unwind-protect-roundtrip
  "Test unwind-protect AST roundtrip."
  (fiveam:for-all ((protected-val (gen-fn (gen-integer :min -100 :max 100)))
                   (cleanup-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((protected-ast (make-instance 'ast-int :value protected-val))
           (cleanup-ast (make-instance 'ast-int :value cleanup-val))
           (ast (make-instance 'ast-unwind-protect
                               :protected protected-ast
                               :cleanup (list cleanup-ast)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-unwind-protect))
      (is (typep (ast-unwind-protected ast2) 'ast-int))
      (is (= protected-val (ast-int-value (ast-unwind-protected ast2))))
      (is (= 1 (length (ast-unwind-cleanup ast2)))))))

;;; ----------------------------------------------------------------------------
;;; Function Call Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-call-roundtrip
  "Test function call AST roundtrip."
  (fiveam:for-all ((fn-name (gen-fn (gen-symbol :prefix "FN")))
                   (args (gen-fn (gen-list-of (gen-integer :min -100 :max 100)
                                              :min-length 1 :max-length 5))))
    (let* ((arg-asts (mapcar (lambda (v) (make-instance 'ast-int :value v)) args))
           (ast (make-instance 'ast-call :func fn-name :args arg-asts))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-call))
      (is (= (length args) (length (ast-call-args ast2))))
      (is (equal args (mapcar #'ast-int-value (ast-call-args ast2)))))))

;;; ----------------------------------------------------------------------------
;;; Quote Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-quote-roundtrip
  "Test quote AST roundtrip."
  (fiveam:for-all ((value (gen-fn (gen-one-of '(nil t 42 "string" (a b c))))))
    (let* ((ast (make-instance 'ast-quote :value value))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-quote))
      (is (equal value (ast-quote-value ast2))))))

(test ast-quote-symbol-roundtrip
  "Test quote with symbol roundtrip."
  (fiveam:for-all ((sym (gen-fn (gen-symbol :package nil :prefix "SYM"))))
    (let* ((ast (make-instance 'ast-quote :value sym))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-quote))
      (is (eq sym (ast-quote-value ast2))))))

(test ast-quote-list-roundtrip
  "Test quote with list roundtrip."
  (let* ((value '(a (b c) d))
         (ast (make-instance 'ast-quote :value value))
         (sexp (ast-to-sexp ast))
         (ast2 (lower-sexp-to-ast sexp)))
    (is (typep ast2 'ast-quote))
    (is (equal value (ast-quote-value ast2)))))

;;; ----------------------------------------------------------------------------
;;; The Type Declaration Roundtrip
;;; ----------------------------------------------------------------------------

(test ast-the-roundtrip
  "Test the type declaration AST roundtrip."
  (fiveam:for-all ((value (gen-fn (gen-integer :min -1000 :max 1000)))
                   (type (gen-fn (gen-one-of '(integer fixnum number)))))
    (let* ((value-ast (make-instance 'ast-int :value value))
           (ast (make-instance 'ast-the :type type :value value-ast))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-the))
      (is (eq type (ast-the-type ast2)))
      (is (typep (ast-the-value ast2) 'ast-int))
      (is (= value (ast-int-value (ast-the-value ast2)))))))

;;; ----------------------------------------------------------------------------
;;; Nested AST Roundtrip Tests
;;; ----------------------------------------------------------------------------

(test nested-if-roundtrip
  "Test nested if expressions roundtrip."
  (fiveam:for-all ((v1 (gen-fn (gen-integer :min 0 :max 1)))
                   (v2 (gen-fn (gen-integer :min 0 :max 1)))
                   (v3 (gen-fn (gen-integer :min -10 :max 10)))
                   (v4 (gen-fn (gen-integer :min -10 :max 10)))
                   (v5 (gen-fn (gen-integer :min -10 :max 10))))
    (let* ((inner-if (make-instance 'ast-if
                                     :cond (make-instance 'ast-int :value v2)
                                     :then (make-instance 'ast-int :value v3)
                                     :else (make-instance 'ast-int :value v4)))
           (ast (make-instance 'ast-if
                               :cond (make-instance 'ast-int :value v1)
                               :then inner-if
                               :else (make-instance 'ast-int :value v5)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-if))
      (is (typep (ast-if-then ast2) 'ast-if))
      (is (= v1 (ast-int-value (ast-if-cond ast2))))
      (is (= v2 (ast-int-value (ast-if-cond (ast-if-then ast2)))))
      (is (= v3 (ast-int-value (ast-if-then (ast-if-then ast2)))))
      (is (= v4 (ast-int-value (ast-if-else (ast-if-then ast2)))))
      (is (= v5 (ast-int-value (ast-if-else ast2)))))))

(test nested-binop-roundtrip
  "Test nested binary operations roundtrip."
  (fiveam:for-all ((v1 (gen-fn (gen-integer :min -10 :max 10)))
                   (v2 (gen-fn (gen-integer :min -10 :max 10)))
                   (v3 (gen-fn (gen-integer :min -10 :max 10))))
    (let* ((inner (make-instance 'ast-binop
                                  :op '+
                                  :lhs (make-instance 'ast-int :value v1)
                                  :rhs (make-instance 'ast-int :value v2)))
           (ast (make-instance 'ast-binop
                               :op '*
                               :lhs inner
                               :rhs (make-instance 'ast-int :value v3)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-binop))
      (is (eq '* (ast-binop-op ast2)))
      (is (typep (ast-binop-lhs ast2) 'ast-binop))
      (is (eq '+ (ast-binop-op (ast-binop-lhs ast2))))
      (is (= v1 (ast-int-value (ast-binop-lhs (ast-binop-lhs ast2)))))
      (is (= v2 (ast-int-value (ast-binop-rhs (ast-binop-lhs ast2)))))
      (is (= v3 (ast-int-value (ast-binop-rhs ast2)))))))

(test nested-let-roundtrip
  "Test nested let expressions roundtrip."
  (fiveam:for-all ((var1 (gen-fn (gen-symbol :package nil :prefix "X")))
                   (var2 (gen-fn (gen-symbol :package nil :prefix "Y")))
                   (v1 (gen-fn (gen-integer :min -10 :max 10)))
                   (v2 (gen-fn (gen-integer :min -10 :max 10))))
    (let* ((inner-let (make-instance 'ast-let
                                      :bindings (list (cons var2 (make-instance 'ast-int :value v2)))
                                      :body (list (make-instance 'ast-var :name var2))))
           (ast (make-instance 'ast-let
                               :bindings (list (cons var1 (make-instance 'ast-int :value v1)))
                               :body (list inner-let)))
           (sexp (ast-to-sexp ast))
           (ast2 (lower-sexp-to-ast sexp)))
      (is (typep ast2 'ast-let))
      (is (eq var1 (car (first (ast-let-bindings ast2)))))
      (is (= v1 (ast-int-value (cdr (first (ast-let-bindings ast2))))))
      (is (= 1 (length (ast-let-body ast2))))
      (is (typep (first (ast-let-body ast2)) 'ast-let))
      (is (eq var2 (car (first (ast-let-bindings (first (ast-let-body ast2))))))))))
