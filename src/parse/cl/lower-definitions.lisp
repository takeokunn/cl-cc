(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Parser — Lowering: Definition Forms
;;;
;;; Contains: lowerers for flet, labels, defun, defvar/defparameter, defmacro,
;;; defclass, defgeneric, defmethod, make-instance, slot-value, values,
;;; multiple-value-bind, multiple-value-call, multiple-value-prog1, apply,
;;; catch, throw, unwind-protect, handler-case, quote, the, funcall.
;;;
;;; Expression lowerers (arithmetic, binops, conditionals, let/lambda/setq,
;;; progn, block, return-from, if, tagbody/go, locally, load-time-value,
;;; symbol-macrolet) are in lower.lisp (loads before this file).
;;;
;;; Load order: after lower.lisp, before parser-roundtrip.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Flet / Labels ────────────────────────────────────────────────────────────

(define-list-lowerer (flet) (node sf sl sc)
  (unless (>= (length node) 3) (error "flet requires bindings and body"))
  (unless (listp (second node)) (error "flet bindings must be a list"))
  (make-ast-flet :bindings (%lower-local-fn-bindings 'flet (second node))
                 :body (mapcar #'lower-sexp-to-ast (cddr node))
                 :source-file sf :source-line sl :source-column sc))

(define-list-lowerer (labels) (node sf sl sc)
  (unless (>= (length node) 3) (error "labels requires bindings and body"))
  (unless (listp (second node)) (error "labels bindings must be a list"))
  (make-ast-labels :bindings (%lower-local-fn-bindings 'labels (second node))
                   :body (mapcar #'lower-sexp-to-ast (cddr node))
                   :source-file sf :source-line sl :source-column sc))

;;; ── Defun ────────────────────────────────────────────────────────────────────

(define-list-lowerer (defun) (node sf sl sc)
  (unless (>= (length node) 3)
    (error "defun requires name, parameters and body"))
  (let ((name (second node)) (raw-params (third node)))
    (unless (symbolp name)   (error "defun name must be a symbol"))
    (unless (listp raw-params) (error "defun parameters must be a list"))
    (multiple-value-bind (type-bindings body-forms)
        (%extract-leading-type-declarations (cdddr node))
      (multiple-value-bind (declarations body-forms)
          (%extract-leading-declarations body-forms)
        (let ((block-body (list (lower-sexp-to-ast (list* 'block name body-forms)))))
          (if (lambda-list-has-extended-p raw-params)
              (multiple-value-bind (required optional rest-param key-params)
                  (%lower-extended-params raw-params)
                (make-ast-defun :name name
                                :params (%apply-type-bindings-to-params required type-bindings)
                                :optional-params optional :rest-param rest-param
                                :key-params key-params   :body block-body
                                :declarations declarations
                                :source-file sf :source-line sl :source-column sc))
              (progn
                (unless (every #'symbolp raw-params)
                  (error "defun parameters must be symbols"))
                (make-ast-defun :name name
                                :params (%apply-type-bindings-to-params raw-params type-bindings)
                                :declarations declarations
                                :body block-body
                                :source-file sf :source-line sl :source-column sc))))))))

;;; ── Defvar / Defparameter ────────────────────────────────────────────────────

(define-list-lowerer (defvar defparameter) (node sf sl sc)
  (unless (>= (length node) 2)
    (error "~A requires at least a name" (car node)))
  (let ((name (second node)))
    (unless (symbolp name) (error "~A name must be a symbol" (car node)))
    (make-ast-defvar :name name
                     :value (when (>= (length node) 3)
                              (lower-sexp-to-ast (third node)))
                     :kind (car node)   ; FR-600: distinguish defvar from defparameter
                     :source-file sf :source-line sl :source-column sc)))

;;; ── Defmacro ─────────────────────────────────────────────────────────────────

(define-list-lowerer (defmacro) (node sf sl sc)
  (unless (>= (length node) 4)
    (error "defmacro requires name, lambda-list, and body"))
  (let ((name (second node)))
    (unless (symbolp name) (error "defmacro name must be a symbol"))
    (make-ast-defmacro :name name
                       :lambda-list (third node)
                       :body (cdddr node)
                       :source-file sf :source-line sl :source-column sc)))

;;; CLOS lowerers (defclass, defgeneric, defmethod, make-instance, slot-value)
;;; are in lower-clos.lisp (loaded next).

;;; ── Values / Multiple-value forms ───────────────────────────────────────────

(define-list-lowerer (values) (node sf sl sc)
  (make-ast-values :forms (mapcar #'lower-sexp-to-ast (cdr node))
                   :source-file sf :source-line sl :source-column sc))

(define-list-lowerer (multiple-value-bind) (node sf sl sc)
  (unless (>= (length node) 4)
    (error "multiple-value-bind requires vars, values-form, and body"))
  (let ((vars (second node)) (values-form (third node)))
    (unless (and (listp vars) (every #'symbolp vars))
      (error "multiple-value-bind vars must be a list of symbols"))
    (make-ast-multiple-value-bind
     :vars (second node)
     :values-form (lower-sexp-to-ast values-form)
     :body (mapcar #'lower-sexp-to-ast (cdddr node))
     :source-file sf :source-line sl :source-column sc)))

(define-list-lowerer (multiple-value-call) (node sf sl sc)
  (unless (>= (length node) 3)
    (error "multiple-value-call requires function and arguments"))
  (make-ast-multiple-value-call
   :func (lower-sexp-to-ast (second node))
   :args (mapcar #'lower-sexp-to-ast (cddr node))
   :source-file sf :source-line sl :source-column sc))

(define-list-lowerer (multiple-value-prog1) (node sf sl sc)
  (unless (>= (length node) 2)
    (error "multiple-value-prog1 requires at least one form"))
  (make-ast-multiple-value-prog1
   :first (lower-sexp-to-ast (second node))
   :forms (mapcar #'lower-sexp-to-ast (cddr node))
   :source-file sf :source-line sl :source-column sc))

;;; ── Apply ────────────────────────────────────────────────────────────────────

(define-list-lowerer (apply) (node sf sl sc)
  (unless (>= (length node) 3)
    (error "apply requires at least a function and one argument"))
  (make-ast-apply :func (lower-sexp-to-ast (second node))
                  :args (mapcar #'lower-sexp-to-ast (cddr node))
                  :source-file sf :source-line sl :source-column sc))

;;; ── Dynamic control (catch/throw/unwind-protect/handler-case) ───────────────

(define-list-lowerer (catch) (node sf sl sc)
  (unless (>= (length node) 3) (error "catch requires tag and body"))
  (make-ast-catch :tag  (lower-sexp-to-ast (second node))
                  :body (mapcar #'lower-sexp-to-ast (cddr node))
                  :source-file sf :source-line sl :source-column sc))

(define-list-lowerer (throw) (node sf sl sc)
  (unless (= (length node) 3) (error "throw requires tag and value"))
  (make-ast-throw :tag   (lower-sexp-to-ast (second node))
                  :value (lower-sexp-to-ast (third node))
                  :source-file sf :source-line sl :source-column sc))

(define-list-lowerer (unwind-protect) (node sf sl sc)
  (unless (>= (length node) 3)
    (error "unwind-protect requires protected form and cleanup"))
  (make-ast-unwind-protect
   :protected (lower-sexp-to-ast (second node))
   :cleanup   (mapcar #'lower-sexp-to-ast (cddr node))
   :source-file sf :source-line sl :source-column sc))

(define-list-lowerer (handler-case) (node sf sl sc)
  (unless (>= (length node) 3)
    (error "handler-case requires a form and at least one handler clause"))
  (let ((clauses (mapcar (lambda (clause)
                           (unless (and (consp clause)
                                        (>= (length clause) 2)
                                        (symbolp (first clause))
                                        (listp (second clause)))
                             (error "Invalid handler-case clause: ~S" clause))
                           (list* (first clause)
                                  (when (second clause) (first (second clause)))
                                  (mapcar #'lower-sexp-to-ast (cddr clause))))
                         (cddr node))))
    (make-ast-handler-case
     :form    (lower-sexp-to-ast (second node))
     :clauses clauses
     :source-file sf :source-line sl :source-column sc)))

;;; ── Quote / The / Funcall ────────────────────────────────────────────────────

(define-list-lowerer (quote) (node sf sl sc)
  (unless (= (length node) 2) (error "quote takes exactly one argument"))
  (make-ast-quote :value (second node)
                  :source-file sf :source-line sl :source-column sc))

(define-list-lowerer (the) (node sf sl sc)
  (unless (= (length node) 3) (error "the requires type and value"))
  (make-ast-the :type  (second node)
                :value (lower-sexp-to-ast (third node))
                :source-file sf :source-line sl :source-column sc))

(define-list-lowerer (funcall) (node sf sl sc)
  (unless (>= (length node) 2)
    (error "funcall requires at least a function argument"))
  (make-ast-call :func (lower-sexp-to-ast (second node))
                 :args (mapcar #'lower-sexp-to-ast (cddr node))
                 :source-file sf :source-line sl :source-column sc))
