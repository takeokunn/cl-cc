;;;; bootstrap/minimal-subset.lisp
;;;
;;; Minimal Bootstrap Subset Definition
;;;
;;; ブートストラップに必要な最小限のCommon Lispサブセットを定義
;;;

(in-package :cl-cc)

(defvar *minimal-forms* nil
  "ブートストラップ用に必要な基本フォームのリスト")

(defvar *minimal-functions* nil
  "ブートストラップ用に必要な基本関数のリスト")

(defun define-minimal-form (name &rest args)
  "最小サブセットのフォームを定義"
  (pushnew name *minimal-forms* :test #'eq))

(defun define-minimal-function (name &rest args)
  "最小サブセットの関数を定義"
  (pushnew name *minimal-functions* :test #'eq))

;; ----------------------------------------------------------------------------
;; Core Forms (基本フォーム）
;; ----------------------------------------------------------------------------

;; 制御フロー
(define-minimal-form 'lambda)
(define-minimal-form 'if)
(define-minimal-form 'let)
(define-minimal-form 'progn)
(define-minimal-form 'setq)

;; 特殊な制御構造
(define-minimal-form 'block)
(define-minimal-form 'return-from)
(define-minimal-form 'tagbody)
(define-minimal-form 'go)

;; 例外処理
(define-minimal-form 'catch)
(define-minimal-form 'throw)
(define-minimal-form 'unwind-protect)

;; 関数定義
(define-minimal-form 'defun)
(define-minimal-form 'flet)
(define-minimal-form 'labels)

;; 型述語
(define-minimal-form 'the)

;; 引用
(define-minimal-form 'quote)

;; ----------------------------------------------------------------------------
;; Core Functions (基本関数）
;; ----------------------------------------------------------------------------

;; 算術演算
(define-minimal-function '+)
(define-minimal-function '-)
(define-minimal-function '*)
(define-minimal-function '/)
(define-minimal-function 'mod)
(define-minimal-function 'rem)
(define-minimal-function '1+)
(define-minimal-function '1-)

;; 唯較演算
(define-minimal-function '=)
(define-minimal-function '<)
(define-minimal-function '>)
(define-minimal-function '<=)
(define-minimal-function '>=')
(define-minimal-function '/=')

;; 等価性テスト
(define-minimal-function 'eq)
(define-minimal-function 'eql)
(define-minimal-function 'equal)

;; リスト操作
(define-minimal-function 'car)
(define-minimal-function 'cdr)
(define-minimal-function 'cons)
(define-minimal-function 'list)
(define-minimal-function 'append)
(define-minimal-function 'reverse)
(define-minimal-function 'nreverse)

;; シンボル操作
(define-minimal-function 'symbolp)
(define-minimal-function 'symbol-name)

;; 型述語
(define-minimal-function 'typep)
(define-minimal-function 'coerce)

;; 関数適用
(define-minimal-function 'apply)
(define-minimal-function 'funcall)

;; 値検査
(define-minimal-function 'null)
(define-minimal-function 'atom)
(define-minimal-function 'consp)
(define-minimal-function 'listp)
(define-minimal-function 'numberp)
(define-minimal-function 'integerp)

;; 入出力
(define-minimal-function 'print)

;; ----------------------------------------------------------------------------
;; Bootstrap Subset Verification Functions
;; ----------------------------------------------------------------------------

(defun minimal-form-p (form)
  "フォームが最小サブセットに含まれているかを確認"
  (member (car form) *minimal-forms*))

(defun minimal-function-p (function-name)
  "関数が最小サブセットに含まれているかを確認"
  (member function-name *minimal-functions*))

(defun verify-minimal-coverage (ast)
  "ASTが最小サブセットのみを使用していることを確認"
  (let ((found-forms nil)
         (found-functions nil))
    ;; ASTを走査して使用されているフォームと関数を収集
    (collect-used-forms-and-functions ast found-forms found-functions)
    ;; 最小サブセット外の使用をチェック
    (let* ((invalid-forms (set-difference found-forms *minimal-forms*))
            (invalid-functions (set-difference found-functions *minimal-functions*)))
      (if (or invalid-forms invalid-functions)
          (error "AST uses features outside minimal subset:~%  Forms: ~A~%  Functions: ~A"
                 invalid-forms invalid-functions)
          t))))

(defun collect-used-forms-and-functions (ast forms functions)
  "ASTから使用されているフォームと関数を収集（再帰的）"
  (typecase ast
    (ast-int forms functions)
    (ast-var
      (when (typep (ast-var-name ast) 'symbol)
        (pushnew (ast-var-name ast) functions :test #'eq)))
    (ast-binop
      (collect-used-forms-and-functions (ast-binop-lhs ast) forms functions)
      (collect-used-forms-and-functions (ast-binop-rhs ast) forms functions))
    (ast-if
      (collect-used-forms-and-functions (ast-if-cond ast) forms functions)
      (collect-used-forms-and-functions (ast-if-then ast) forms functions)
      (collect-used-forms-and-functions (ast-if-else ast) forms functions))
    (ast-progn
      (dolist (form (ast-progn-forms ast))
        (collect-used-forms-and-functions form forms functions)))
    (ast-let
      (dolist (binding (ast-let-bindings ast))
        (collect-used-forms-and-functions (cdr binding) forms functions))
      (dolist (form (ast-let-body ast))
        (collect-used-forms-and-functions form forms functions)))
    (ast-lambda
      (dolist (form (ast-lambda-body ast))
        (collect-used-forms-and-functions form forms functions)))
    (ast-call
      (let ((func (ast-call-func ast)))
        (if (symbolp func)
            (pushnew func functions :test #'eq)
            (collect-used-forms-and-functions func forms functions)))
      (dolist (arg (ast-call-args ast))
        (collect-used-forms-and-functions arg forms functions)))
    (ast-function
      (pushnew (ast-function-name ast) functions :test #'eq))
    (t
      forms)))

(defun get-minimal-subset-info ()
  "最小サブセットの情報を返す"
  (list
    :forms *minimal-forms*
    :functions *minimal-functions*
    :total-count (+ (length *minimal-forms*)
                   (length *minimal-functions*))
    :description "Bootstrap requires ~D forms and ~D functions"
    (format nil "~D" (length *minimal-forms*) (length *minimal-functions*))))
