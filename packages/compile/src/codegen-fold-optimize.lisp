;;;; packages/compile/src/codegen-fold-optimize.lisp — AST constant-fold pass (optimize-ast)
;;;
;;; Contains:
;;;   - %loc         macro: inherit source location from an AST node
;;;   - optimize-ast top-level fold pass called from compile-toplevel-forms
;;;
;;; Depends on codegen-fold.lisp (%fold-ast-binop, %evaluate-ast, %compile-time-eval-depth-limit,
;;; *compile-time-value-env*, *compile-time-function-env*, %compile-time-value->ast).
;;; Load order: after codegen-fold, before codegen.

(in-package :cl-cc/compile)

;;; ── Source-location helpers ──────────────────────────────────────────────────

;;; This file reuses %clone-source from codegen-fold.lisp rather than defining a
;;; local macro wrapper.

;;; ── AST constant fold pass ───────────────────────────────────────────────────

(defparameter *optimize-ast-rebuilder-table*
  '((ast-progn       . %optimize-ast-progn-node)
     (ast-let         . %optimize-ast-let-node)
     (ast-if          . %optimize-ast-if-node)
     (ast-lambda      . %optimize-ast-lambda-node)
     (ast-defun       . %optimize-ast-defun-node)
     (ast-defclass    . %optimize-ast-defclass-node)
     (ast-defvar      . %optimize-ast-defvar-node)
     (ast-block       . %optimize-ast-block-node)
     (ast-return-from . %optimize-ast-return-from-node)
     (ast-setq        . %optimize-ast-setq-node)
     (ast-the         . %optimize-ast-the-node))
  "Type symbol → rebuilder function symbol for optimize-ast's simple recursive cases.
Special folding nodes like ast-binop and ast-call stay in optimize-ast directly.")

(defun %optimize-ast-list (nodes &optional lexical-bound)
  "Optimize every AST node in NODES, preserving list order."
  (if (consp nodes)
      (cons (optimize-ast (car nodes) lexical-bound)
            (%optimize-ast-list (cdr nodes) lexical-bound))
      nil))

(defun %optimize-ast-binding-alist (bindings &optional lexical-bound)
  "Optimize the value side of an alist of lexical BINDINGS."
  (if (consp bindings)
      (let ((binding (car bindings)))
        (cons (cons (car binding) (optimize-ast (cdr binding) lexical-bound))
              (%optimize-ast-binding-alist (cdr bindings) lexical-bound)))
      nil))

(defun %optimize-ast-binding-names (bindings)
  (if (consp bindings)
      (cons (caar bindings)
            (%optimize-ast-binding-names (cdr bindings)))
      nil))

(defun %optimize-ast-extend-lexical-bound (names lexical-bound)
  (if (consp names)
      (let ((name (car names)))
        (%optimize-ast-extend-lexical-bound
         (cdr names)
         (if (%member-eq-p name lexical-bound)
             lexical-bound
             (cons name lexical-bound))))
      lexical-bound))

(defun %optimize-ast-optional-param-name (spec)
  (if (consp spec) (car spec) spec))

(defun %optimize-ast-key-param-name (spec)
  (let ((name (if (consp spec) (car spec) spec)))
    (if (consp name) (second name) name)))

(defun %optimize-ast-callable-bound-names (params optional-params rest-param key-params)
  (append params
          (mapcar #'%optimize-ast-optional-param-name optional-params)
          (if rest-param (list rest-param) nil)
          (mapcar #'%optimize-ast-key-param-name key-params)))

(defun %optimize-ast-slot-def-node (node &optional lexical-bound)
  (%clone-source node #'make-ast-slot-def
    :name       (ast-slot-name node)
    :initarg    (ast-slot-initarg node)
    :initform   (let ((initform (ast-slot-initform node)))
                  (if initform
                      (optimize-ast initform lexical-bound)
                      nil))
    :reader     (ast-slot-reader node)
    :writer     (ast-slot-writer node)
    :accessor   (ast-slot-accessor node)
    :type       (ast-slot-type node)
    :allocation (ast-slot-allocation node)))

(defun %optimize-ast-slot-def-list (slots &optional lexical-bound)
  (if (consp slots)
      (cons (%optimize-ast-slot-def-node (car slots) lexical-bound)
            (%optimize-ast-slot-def-list (cdr slots) lexical-bound))
      nil))

(defun %optimize-ast-default-initargs (default-initargs &optional lexical-bound)
  (if (consp default-initargs)
      (let ((entry (car default-initargs)))
        (cons (cons (car entry) (optimize-ast (cdr entry) lexical-bound))
              (%optimize-ast-default-initargs (cdr default-initargs) lexical-bound)))
      nil))

(defun %optimize-ast-var-node (node lexical-bound)
  (let ((name (ast-var-name node)))
    (if (%member-eq-p name lexical-bound)
        node
        (multiple-value-bind (constant-value constant-present-p)
            (gethash name *constant-table*)
          (if constant-present-p
              (%compile-time-value->ast constant-value node)
              node)))))

(defun %optimize-ast-progn-node (node &optional lexical-bound)
  (%clone-source node #'make-ast-progn
    :forms (%optimize-ast-list (ast-progn-forms node) lexical-bound)))

(defun %optimize-ast-let-node (node &optional lexical-bound)
  (let ((bound-names (%optimize-ast-binding-names (ast-let-bindings node))))
    (%clone-source node #'make-ast-let
      :bindings     (%optimize-ast-binding-alist (ast-let-bindings node) lexical-bound)
      :declarations (ast-let-declarations node)
      :body         (%optimize-ast-list
                     (ast-let-body node)
                     (%optimize-ast-extend-lexical-bound bound-names lexical-bound)))))

(defun %optimize-ast-if-node (node &optional lexical-bound)
  (%clone-source node #'make-ast-if
    :cond (optimize-ast (ast-if-cond node) lexical-bound)
    :then (optimize-ast (ast-if-then node) lexical-bound)
    :else (optimize-ast (ast-if-else node) lexical-bound)))

(defun %optimize-ast-lambda-node (node &optional lexical-bound)
  (let ((bound-names (%optimize-ast-callable-bound-names
                      (ast-lambda-params node)
                      (ast-lambda-optional-params node)
                      (ast-lambda-rest-param node)
                      (ast-lambda-key-params node))))
    (%clone-source node #'make-ast-lambda
      :params          (ast-lambda-params node)
      :optional-params (ast-lambda-optional-params node)
      :rest-param      (ast-lambda-rest-param node)
      :key-params      (ast-lambda-key-params node)
      :declarations    (ast-lambda-declarations node)
      :body            (%optimize-ast-list
                        (ast-lambda-body node)
                        (%optimize-ast-extend-lexical-bound bound-names lexical-bound))
      :env             (ast-lambda-env node))))

(defun %optimize-ast-defun-node (node &optional lexical-bound)
  (let ((bound-names (%optimize-ast-callable-bound-names
                      (ast-defun-params node)
                      (ast-defun-optional-params node)
                      (ast-defun-rest-param node)
                      (ast-defun-key-params node))))
    (%clone-source node #'make-ast-defun
      :name            (ast-defun-name node)
      :params          (ast-defun-params node)
      :optional-params (ast-defun-optional-params node)
      :rest-param      (ast-defun-rest-param node)
      :key-params      (ast-defun-key-params node)
      :declarations    (ast-defun-declarations node)
      :documentation   (ast-defun-documentation node)
      :body            (%optimize-ast-list
                        (ast-defun-body node)
                        (%optimize-ast-extend-lexical-bound bound-names lexical-bound)))))

(defun %optimize-ast-defclass-node (node &optional lexical-bound)
  (%clone-source node #'make-ast-defclass
    :name             (ast-defclass-name node)
    :superclasses     (ast-defclass-superclasses node)
    :slots            (%optimize-ast-slot-def-list (ast-defclass-slots node)
                                                   lexical-bound)
    :default-initargs (%optimize-ast-default-initargs
                       (ast-defclass-default-initargs node)
                       lexical-bound)
    :metaclass        (let ((metaclass (ast-defclass-metaclass node)))
                        (if metaclass
                            (optimize-ast metaclass lexical-bound)
                            nil))))

(defun %optimize-ast-defvar-node (node &optional lexical-bound)
  (%clone-source node #'make-ast-defvar
    :name  (ast-defvar-name node)
    :value (optimize-ast (ast-defvar-value node) lexical-bound)
    :kind  (ast-defvar-kind node)))

(defun %optimize-ast-block-node (node &optional lexical-bound)
  (%clone-source node #'make-ast-block
    :name (ast-block-name node)
    :body (%optimize-ast-list (ast-block-body node) lexical-bound)))

(defun %optimize-ast-return-from-node (node &optional lexical-bound)
  (%clone-source node #'make-ast-return-from
    :name  (ast-return-from-name node)
    :value (optimize-ast (ast-return-from-value node) lexical-bound)))

(defun %optimize-ast-setq-node (node &optional lexical-bound)
  (%clone-source node #'make-ast-setq
    :var   (ast-setq-var node)
    :value (optimize-ast (ast-setq-value node) lexical-bound)))

(defun %optimize-ast-the-node (node &optional lexical-bound)
  (%clone-source node #'make-ast-the
    :type  (ast-the-type node)
    :value (optimize-ast (ast-the-value node) lexical-bound)))

(defun %optimize-ast-rebuild-node (node &optional lexical-bound)
  "Rebuild NODE when it is a simple recursive case.
Uses TYPECASE so the implementation can dispatch on CLASS-OF instead of running
up to 11 sequential TYPEP calls per AST node visit."
  (typecase node
    (ast-progn       (%optimize-ast-progn-node node lexical-bound))
    (ast-let         (%optimize-ast-let-node node lexical-bound))
    (ast-if          (%optimize-ast-if-node node lexical-bound))
    (ast-lambda      (%optimize-ast-lambda-node node lexical-bound))
    (ast-defun       (%optimize-ast-defun-node node lexical-bound))
    (ast-defclass    (%optimize-ast-defclass-node node lexical-bound))
    (ast-defvar      (%optimize-ast-defvar-node node lexical-bound))
    (ast-block       (%optimize-ast-block-node node lexical-bound))
    (ast-return-from (%optimize-ast-return-from-node node lexical-bound))
    (ast-setq        (%optimize-ast-setq-node node lexical-bound))
    (ast-the         (%optimize-ast-the-node node lexical-bound))
    (t               nil)))

(defun optimize-ast (node &optional lexical-bound)
  "Fold small pure constant expressions before VM lowering."
  (typecase node
    (ast-binop
     (%fold-ast-binop node
                      (optimize-ast (ast-binop-lhs node) lexical-bound)
                      (optimize-ast (ast-binop-rhs node) lexical-bound)))
    (ast-call
     (let* ((func (optimize-ast (ast-call-func node) lexical-bound))
            (args (%optimize-ast-list (ast-call-args node) lexical-bound))
            (call-node (%clone-source node #'make-ast-call :func func :args args)))
       (multiple-value-bind (value ok)
           (let ((*compile-time-value-env* *compile-time-value-env*)
                 (*compile-time-function-env* *compile-time-function-env*))
             (%evaluate-ast call-node *compile-time-eval-depth-limit*))
         (if ok (%compile-time-value->ast value node) call-node))))
    (ast-var
     (%optimize-ast-var-node node lexical-bound))
    (t
     (let ((rebuilt (%optimize-ast-rebuild-node node lexical-bound)))
       (if rebuilt rebuilt node)))))
