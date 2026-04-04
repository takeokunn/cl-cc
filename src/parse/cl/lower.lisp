(in-package :cl-cc)


;;; ── Arithmetic and comparison binary operators ───────────────────────────────

(defun %lower-constant-number (value sf sl sc)
  (make-ast-int :value value :source-file sf :source-line sl :source-column sc))

(defun %lower-associative-arithmetic (op operands sf sl sc)
  (labels ((lower-one (sexp)
             (lower-sexp-to-ast sexp :source-file sf :source-line sl :source-column sc))
           (fold-left (items identity)
             (cond
               ((null items) identity)
               ((null (cdr items)) (lower-one (car items)))
               (t (reduce (lambda (lhs rhs)
                            (make-ast-binop :op op
                                            :lhs lhs
                                            :rhs (lower-one rhs)
                                            :source-file sf
                                            :source-line sl
                                            :source-column sc))
                          (cdr items)
                          :initial-value (lower-one (car items)))))))
    (case op
      (+ (fold-left operands (%lower-constant-number 0 sf sl sc)))
      (* (fold-left operands (%lower-constant-number 1 sf sl sc)))
      (- (cond
           ((null operands) (%lower-constant-number 0 sf sl sc))
           ((null (cdr operands))
            (make-ast-binop :op '-
                            :lhs (%lower-constant-number 0 sf sl sc)
                            :rhs (lower-one (car operands))
                            :source-file sf :source-line sl :source-column sc))
           (t (reduce (lambda (lhs rhs)
                        (make-ast-binop :op '-
                                        :lhs lhs
                                        :rhs (lower-one rhs)
                                        :source-file sf :source-line sl :source-column sc))
                      (cdr operands)
                      :initial-value (lower-one (car operands))))))
      (/ (cond
           ((null operands) (%lower-constant-number 1 sf sl sc))
           ((null (cdr operands))
            (make-ast-binop :op '/
                            :lhs (%lower-constant-number 1 sf sl sc)
                            :rhs (lower-one (car operands))
                            :source-file sf :source-line sl :source-column sc))
           (t (reduce (lambda (lhs rhs)
                        (make-ast-binop :op '/
                                        :lhs lhs
                                        :rhs (lower-one rhs)
                                        :source-file sf :source-line sl :source-column sc))
                      (cdr operands)
                      :initial-value (lower-one (car operands))))))
      (otherwise
       (error "~S takes exactly 2 args" op)))))

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

;;; ── Defclass ─────────────────────────────────────────────────────────────────

(define-list-lowerer (defclass) (node sf sl sc)
  (unless (>= (length node) 4)
    (error "defclass requires name, superclasses, and slot definitions"))
  (let* ((name (second node)) (superclasses (third node)) (slot-specs (fourth node))
         ;; Parse class options (5th+ elements): (:default-initargs key val ...)
         (class-options (nthcdr 4 node))
         (default-initargs-opt (find :default-initargs class-options
                                     :key (lambda (o) (when (listp o) (first o)))))
         (default-initargs (when default-initargs-opt
                             (loop for (k v) on (rest default-initargs-opt) by #'cddr
                                   collect (cons k (lower-sexp-to-ast v))))))
    (unless (symbolp name)      (error "defclass name must be a symbol"))
    (unless (listp superclasses) (error "defclass superclasses must be a list"))
    (unless (listp slot-specs)   (error "defclass slots must be a list"))
    (make-ast-defclass :name name
                       :superclasses superclasses
                       :slots (mapcar #'parse-slot-spec slot-specs)
                       :default-initargs default-initargs
                       :source-file sf :source-line sl :source-column sc)))

;;; ── Defgeneric ───────────────────────────────────────────────────────────────

(define-list-lowerer (defgeneric) (node sf sl sc)
  (unless (>= (length node) 3)
    (error "defgeneric requires name and lambda-list"))
  (let* ((name (second node))
         (lambda-list (third node))
         (options (cdddr node))
         (inline-methods nil))
    (unless (symbolp name) (error "defgeneric name must be a symbol"))
    ;; Parse options: extract (:method ...) forms, :method-combination, accept and ignore others
    (let ((combination nil))
      (dolist (opt options)
        (when (consp opt)
          (case (car opt)
            (:method
             (push (cons 'defmethod (cons name (cdr opt))) inline-methods))
            (:method-combination
             (setf combination (second opt))))))
    (setf inline-methods (nreverse inline-methods))
    (let ((gf-node (make-ast-defgeneric :name name :params lambda-list
                                        :combination combination
                                        :source-file sf :source-line sl
                                        :source-column sc)))
      (if inline-methods
          ;; Wrap in progn: defgeneric + defmethod forms
          (make-ast-progn
           :forms (cons gf-node
                        (mapcar (lambda (m)
                                  (lower-sexp-to-ast m :source-file sf
                                                       :source-line sl
                                                       :source-column sc))
                                inline-methods))
           :source-file sf :source-line sl :source-column sc)
          gf-node)))))

;;; ── Defmethod ────────────────────────────────────────────────────────────────

(define-list-lowerer (defmethod) (node sf sl sc)
  (unless (>= (length node) 4)
    (error "defmethod requires name, parameters, and body"))
  (let* ((name (second node))
         (rest-after-name (cddr node))
         ;; Skip optional method qualifier (:before, :after, :around)
         (qualifier (when (and (symbolp (car rest-after-name))
                               (not (listp (car rest-after-name))))
                      (pop rest-after-name)))
         (raw-params (car rest-after-name))
         (body-forms (cdr rest-after-name))
         (specializers nil) (param-names nil))
    (unless (symbolp name)    (error "defmethod name must be a symbol"))
    (unless (listp raw-params) (error "defmethod parameters must be a list"))
    (dolist (p raw-params)
      (if (listp p)
          (progn (push (cons (first p) (second p)) specializers)
                 (push (first p) param-names))
          (progn (push nil specializers)
                 (push p param-names))))
    (make-ast-defmethod
     :name name
     :qualifier qualifier
     :specializers (nreverse specializers)
     :params       (nreverse param-names)
     :body         (list (lower-sexp-to-ast (list* 'block name body-forms)))
     :source-file sf :source-line sl :source-column sc)))

;;; ── Make-instance ────────────────────────────────────────────────────────────

(define-list-lowerer (make-instance) (node sf sl sc)
  (unless (>= (length node) 2)
    (error "make-instance requires at least a class name"))
  (let ((initargs nil) (rest (cddr node)))
    (loop while rest
          do (let ((key (pop rest)))
               (unless (keywordp key)
                 (error "make-instance initarg must be a keyword, got ~S" key))
               (unless rest
                 (error "make-instance initarg ~S missing value" key))
               (push (cons key (lower-sexp-to-ast (pop rest))) initargs)))
    (make-ast-make-instance :class (lower-sexp-to-ast (second node))
                            :initargs (nreverse initargs)
                            :source-file sf :source-line sl :source-column sc)))

;;; ── Slot-value ───────────────────────────────────────────────────────────────

(define-list-lowerer (slot-value) (node sf sl sc)
  (unless (= (length node) 3)
    (error "slot-value requires object and slot-name"))
  (make-ast-slot-value
   :object (lower-sexp-to-ast (second node))
   :slot   (let ((sn (third node)))
             (if (and (listp sn) (eq (car sn) 'quote)) (second sn) sn))
   :source-file sf :source-line sl :source-column sc))

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
