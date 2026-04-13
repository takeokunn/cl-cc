(in-package :cl-cc)


;;; ── Arithmetic and comparison binary operators ───────────────────────────────

(defun %lower-constant-number (value sf sl sc)
  (make-ast-int :value value :source-file sf :source-line sl :source-column sc))

;;; Data table: each entry is (op identity-value unary-prefix-p).
;;; unary-prefix-p=NIL  → (op arg)        lowers to just arg  (pure associative)
;;; unary-prefix-p=T    → (op arg)        lowers to (op identity arg)  (negate/reciprocal)
(defparameter *arith-op-descriptors*
  '((+  0 nil)
    (*  1 nil)
    (-  0 t)
    (/  1 t)))

(defun %lower-associative-arithmetic (op operands sf sl sc)
  (labels ((lower-one (sexp)
             (lower-sexp-to-ast sexp :source-file sf :source-line sl :source-column sc))
           (binop-ast (lhs-ast rhs-ast)
             (make-ast-binop :op op :lhs lhs-ast :rhs rhs-ast
                             :source-file sf :source-line sl :source-column sc))
           (fold-from-left (items)
             (reduce (lambda (acc rhs-sexp) (binop-ast acc (lower-one rhs-sexp)))
                     (cdr items)
                     :initial-value (lower-one (car items)))))
    (let* ((desc (assoc op *arith-op-descriptors*))
           (identity-node (%lower-constant-number (cadr desc) sf sl sc))
           (unary-prefix-p (caddr desc)))
      (unless desc (error "~S takes exactly 2 args" op))
      (cond
        ((null operands)                   identity-node)
        ((and (null (cdr operands))
              unary-prefix-p)              (binop-ast identity-node (lower-one (car operands))))
        ((null (cdr operands))             (lower-one (car operands)))
        (t                                 (fold-from-left operands))))))

(define-list-lowerer (+ - * /) (node sf sl sc)
  (%lower-associative-arithmetic (car node) (cdr node) sf sl sc))

(define-list-lowerer (= < > <= >=) (node sf sl sc)
  (unless (= (length node) 3)
    (error "~S takes exactly 2 args" (car node)))
  (make-ast-binop :op (car node)
                  :lhs (lower-sexp-to-ast (second node))
                  :rhs (lower-sexp-to-ast (third node))
                  :source-file sf :source-line sl :source-column sc))

;;; ── Conditional ──────────────────────────────────────────────────────────────

(define-list-lowerer (if) (node sf sl sc)
  (unless (member (length node) '(3 4))
    (error "if takes cond then [else]"))
  (make-ast-if :cond (lower-sexp-to-ast (second node))
               :then (lower-sexp-to-ast (third node))
               :else (if (fourth node)
                         (lower-sexp-to-ast (fourth node))
                         (make-ast-quote :value nil))
               :source-file sf :source-line sl :source-column sc))

;;; ── Sequence ─────────────────────────────────────────────────────────────────

(define-list-lowerer (progn) (node sf sl sc)
  (if (< (length node) 2)
      (make-ast-quote :value nil)
      (make-ast-progn :forms (mapcar #'lower-sexp-to-ast (cdr node))
                      :source-file sf :source-line sl :source-column sc)))

;;; ── Print ────────────────────────────────────────────────────────────────────

(define-list-lowerer (print) (node sf sl sc)
  (unless (member (length node) '(2 3))
    (error "print takes one or two args"))
  (make-ast-print :expr (lower-sexp-to-ast (second node))
                  :source-file sf :source-line sl :source-column sc))

;;; ── Let binding ──────────────────────────────────────────────────────────────

(define-list-lowerer (let) (node sf sl sc)
  (unless (>= (length node) 3)
    (error "let requires bindings and body"))
  (let ((bindings (second node)))
    (unless (listp bindings)
      (error "let bindings must be a list"))
    (multiple-value-bind (declarations body-forms)
        (%extract-leading-declarations (cddr node))
      (make-ast-let
       :bindings (mapcar (lambda (binding)
                           (cond
                             ((symbolp binding)
                              (cons binding (make-ast-quote :value nil)))
                             ((and (consp binding) (= (length binding) 2)
                                   (symbolp (first binding)))
                              (cons (first binding) (lower-sexp-to-ast (second binding))))
                             ((and (consp binding) (= (length binding) 1)
                                   (symbolp (first binding)))
                              (cons (first binding) (make-ast-quote :value nil)))
                             (t (error "Invalid let binding: ~S" binding))))
                         bindings)
       :declarations declarations
       :body (mapcar #'lower-sexp-to-ast body-forms)
       :source-file sf :source-line sl :source-column sc))))

;;; ── Lambda expression ────────────────────────────────────────────────────────

(define-list-lowerer (lambda) (node sf sl sc)
  (unless (>= (length node) 3)
    (error "lambda requires parameters and body"))
  (let ((raw-params (second node)))
    (unless (listp raw-params)
      (error "lambda parameters must be a list"))
    (multiple-value-bind (type-bindings body-forms)
        (%extract-leading-type-declarations (cddr node))
      (multiple-value-bind (declarations body-forms)
          (%extract-leading-declarations body-forms)
        (if (lambda-list-has-extended-p raw-params)
            (multiple-value-bind (required optional rest-param key-params)
                (%lower-extended-params raw-params)
              (make-ast-lambda :params (%apply-type-bindings-to-params required type-bindings)
                               :optional-params optional
                               :rest-param rest-param
                               :key-params key-params
                               :declarations declarations
                               :body (mapcar #'lower-sexp-to-ast body-forms)
                               :source-file sf :source-line sl :source-column sc))
            (progn
              (unless (every #'symbolp raw-params)
                (error "lambda parameters must be symbols"))
              (make-ast-lambda :params (%apply-type-bindings-to-params raw-params type-bindings)
                               :declarations declarations
                               :body (mapcar #'lower-sexp-to-ast body-forms)
                               :source-file sf :source-line sl :source-column sc)))))))

;;; ── Function reference ───────────────────────────────────────────────────────

(define-list-lowerer (function) (node sf sl sc)
  (unless (= (length node) 2)
    (error "function takes exactly one argument"))
  (let ((name (second node)))
    (cond
      ((and (consp name) (eq (car name) 'lambda))
       (lower-sexp-to-ast name))
      ((or (symbolp name) (and (consp name) (eq (car name) 'setf)))
       (make-ast-function :name name
                          :source-file sf :source-line sl :source-column sc))
      (t (error "function argument must be a symbol, (setf name), or (lambda ...)")))))

;;; ── Block ────────────────────────────────────────────────────────────────────

(define-list-lowerer (block) (node sf sl sc)
  (unless (>= (length node) 2)
    (error "block requires a name"))
  (let ((name (second node)))
    (unless (symbolp name)
      (error "block name must be a symbol"))
    (make-ast-block :name name
                    :body (if (cddr node)
                              (mapcar #'lower-sexp-to-ast (cddr node))
                              (list (make-ast-quote :value nil)))
                    :source-file sf :source-line sl :source-column sc)))

;;; ── Return-from ──────────────────────────────────────────────────────────────

(define-list-lowerer (return-from) (node sf sl sc)
  (unless (member (length node) '(2 3))
    (error "return-from requires name and optional value"))
  (let ((name (second node)))
    (unless (symbolp name)
      (error "return-from name must be a symbol"))
    (make-ast-return-from
     :name name
     :value (if (= (length node) 3)
                (lower-sexp-to-ast (third node))
                (make-ast-quote :value nil))
     :source-file sf :source-line sl :source-column sc)))

;;; ── Tagbody ──────────────────────────────────────────────────────────────────

(define-list-lowerer (tagbody) (node sf sl sc)
  (when (< (length node) 2)
    (error "tagbody requires at least one tag or form"))
  (let ((tags nil) (current-tag nil) (current-forms nil))
    (dolist (item (cdr node))
      (if (or (symbolp item) (integerp item))
          (progn
            (when current-tag
              (push (cons current-tag (nreverse current-forms)) tags))
            (setf current-tag item current-forms nil))
          (progn
            (unless current-tag
              (setf current-tag (gensym "TAGBODY-START")))
            (push (lower-sexp-to-ast item) current-forms))))
    (when current-tag
      (push (cons current-tag (nreverse current-forms)) tags))
    (make-ast-tagbody :tags (nreverse tags)
                      :source-file sf :source-line sl :source-column sc)))

;;; ── Go ───────────────────────────────────────────────────────────────────────

(define-list-lowerer (go) (node sf sl sc)
  (unless (= (length node) 2)
    (error "go requires exactly one tag"))
  (let ((tag (second node)))
    (unless (or (symbolp tag) (integerp tag))
      (error "go tag must be a symbol or integer"))
    (make-ast-go :tag tag
                 :source-file sf :source-line sl :source-column sc)))

;;; ── Setq ─────────────────────────────────────────────────────────────────────
;;; Supports multi-var: (setq a 1 b 2) → (progn (setq a 1) (setq b 2))

(define-list-lowerer (setq) (node sf sl sc)
  (let ((args (cdr node)))
    (unless (and args (evenp (length args)))
      (error "setq requires pairs of variable and value"))
    (if (= (length args) 2)
        (let ((var (first args)))
          (unless (symbolp var) (error "setq variable must be a symbol"))
          (make-ast-setq :var var :value (lower-sexp-to-ast (second args))
                         :source-file sf :source-line sl :source-column sc))
        (make-ast-progn
         :forms (loop for (var val) on args by #'cddr
                      collect (progn
                                (unless (symbolp var)
                                  (error "setq variable must be a symbol"))
                                (make-ast-setq :var var
                                               :value (lower-sexp-to-ast val))))
         :source-file sf :source-line sl :source-column sc))))

;;; ── Setf ─────────────────────────────────────────────────────────────────────

(defun %lower-setf-place (place value-form sf sl sc)
  "Lower a (setf (PLACE ...) VALUE-FORM) compound place.
Simple places dispatch via *setf-place-simple-rewrites*; complex places
(slot-value, gethash, getf) are handled specially."
  (let* ((head (car place))
         (place-args (cdr place))
         (rule (assoc head *setf-place-simple-rewrites*)))
    (if rule
        (let* ((fn-sym  (second rule))
               (n-args  (third rule))
               (used-args (subseq place-args 0 n-args)))
          (lower-sexp-to-ast `(,fn-sym ,@used-args ,value-form)))
        (case head
          (slot-value
           (let ((obj-form (second place)) (slot-form (third place)))
             (if (and (consp slot-form) (eq (car slot-form) 'quote))
                 (make-ast-set-slot-value
                  :object (lower-sexp-to-ast obj-form)
                  :slot   (second slot-form)
                  :value  (lower-sexp-to-ast value-form)
                  :source-file sf :source-line sl :source-column sc)
                 (lower-sexp-to-ast (list 'rt-slot-set obj-form slot-form value-form)))))
          (gethash
           (make-ast-set-gethash
            :key   (lower-sexp-to-ast (second place))
            :table (lower-sexp-to-ast (third place))
            :value (lower-sexp-to-ast value-form)
            :source-file sf :source-line sl :source-column sc))
          (getf
           (let ((plist-var (second place)) (indicator (third place)))
             (if (symbolp plist-var)
                 (let ((val-ast   (lower-sexp-to-ast value-form))
                       (plist-ast (lower-sexp-to-ast plist-var))
                       (ind-ast   (lower-sexp-to-ast indicator)))
                   (make-ast-progn
                    :forms (list (make-ast-setq
                                  :var plist-var
                                  :value (make-ast-call
                                          :func (lower-sexp-to-ast 'rt-plist-put)
                                          :args (list plist-ast ind-ast val-ast)))
                                 val-ast)))
                 (lower-sexp-to-ast `(rt-plist-put ,(second place) ,(third place) ,value-form)))))
          (otherwise
           (error "setf only supports symbol, slot-value, gethash, getf, aref, svref, car, cdr, bit, char, elt, nth places"))))))

(define-list-lowerer (setf) (node sf sl sc)
  (unless (= (length node) 3)
    (error "setf requires a place and a value"))
  (let ((place (second node)) (value-form (third node)))
    (if (symbolp place)
        (make-ast-setq :var place :value (lower-sexp-to-ast value-form)
                       :source-file sf :source-line sl :source-column sc)
        (%lower-setf-place place value-form sf sl sc))))

