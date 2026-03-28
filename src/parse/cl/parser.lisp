;;;; frontend/cl/parser.lisp - Common Lisp S-Expression Parser
;;;;
;;;; This module provides:
;;;; - S-expression parsing from source strings (parse-source, parse-all-forms)
;;;; - S-expression to AST transformation (lower-sexp-to-ast)
;;;; - AST to S-expression roundtrip (ast-to-sexp)

(in-package :cl-cc)

;;; S-Expression Parser

(defun parse-source (source)
  "Parse SOURCE into one s-expression using the hand-written CL lexer."
  (let ((forms (parse-all-forms source)))
    (when (null forms)
      (error "Empty source"))
    (first forms)))

(defun parse-all-forms (source)
  "Parse SOURCE into a list of all top-level s-expressions.
Uses the hand-written CL lexer and recursive-descent parser (no host reader)."
  (multiple-value-bind (cst-list _diagnostics)
      (parse-cl-source source)
    (declare (ignore _diagnostics))
    (mapcar #'cst-to-sexp cst-list)))

;;; S-Expression to AST Transformation

(defgeneric lower-sexp-to-ast (node &key source-file source-line source-column)
  (:documentation "Convert an S-expression NODE to an AST node with optional source location."))

(defmethod lower-sexp-to-ast ((node integer) &key source-file source-line source-column)
  (make-ast-int :value node
                 :source-file source-file
                 :source-line source-line
                 :source-column source-column))

(defmethod lower-sexp-to-ast ((node float) &key source-file source-line source-column)
  (make-ast-quote :value node
                  :source-file source-file
                  :source-line source-line
                  :source-column source-column))

(defmethod lower-sexp-to-ast ((node string) &key source-file source-line source-column)
  (make-ast-quote :value node
                 :source-file source-file
                 :source-line source-line
                 :source-column source-column))

(defmethod lower-sexp-to-ast ((node character) &key source-file source-line source-column)
  (make-ast-quote :value node
                 :source-file source-file
                 :source-line source-line
                 :source-column source-column))

(defmethod lower-sexp-to-ast ((node array) &key source-file source-line source-column)
  ;; Vector/array literals like #() or #(:X :Y) are self-evaluating constants.
  (make-ast-quote :value node
                  :source-file source-file
                  :source-line source-line
                  :source-column source-column))

(defmethod lower-sexp-to-ast ((node symbol) &key source-file source-line source-column)
  ;; nil and t are self-evaluating constants, not variable references.
  (if (member node '(nil t))
      (make-ast-quote :value node
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)
      (make-ast-var :name node
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column)))

(defun parse-compiler-lambda-list (params)
  "Parse a lambda list into required, optional, rest, and key parameters.
Returns (values required optional rest key) where:
  required = list of symbols
  optional = list of (name default-sexp) pairs
  rest = symbol or nil
  key = list of (name default-sexp) pairs"
  (let ((required nil) (optional nil) (rest-param nil) (key-params nil)
        (state :required))
    (dolist (p params)
      (cond
        ((eq p '&optional) (setf state :optional))
        ((eq p '&rest)     (setf state :rest))
        ((eq p '&body)     (setf state :rest))
        ((eq p '&key)      (setf state :key))
        ((eq p '&allow-other-keys) nil)  ; skip
        (t (case state
             (:required (unless (symbolp p)
                          (error "Required parameter must be a symbol: ~S" p))
                        (push p required))
             (:optional (if (listp p)
                            (push (list (first p) (second p) (third p)) optional)
                            (push (list p nil nil) optional)))
             (:rest     (unless (symbolp p)
                          (error "&rest parameter must be a symbol: ~S" p))
                        (setf rest-param p)
                        (setf state :post-rest))
             (:post-rest (cond ((eq p '&key) (setf state :key))
                               (t (error "Unexpected parameter after &rest: ~S" p))))
             (:key      (if (listp p)
                            (let ((name (if (listp (first p)) (second (first p)) (first p)))
                                  (default (second p))
                                  (supplied-p (third p)))
                              (push (list name default supplied-p) key-params))
                            (push (list p nil nil) key-params)))))))
    (values (nreverse required) (nreverse optional) rest-param (nreverse key-params))))

(defun lambda-list-has-extended-p (params)
  "Return T if PARAMS contains &optional, &rest, &body, or &key."
  (and (listp params)
       (some (lambda (p) (member p '(&optional &rest &body &key &allow-other-keys)))
             params)))

(defun parse-slot-spec (spec)
  "Parse a CLOS slot specification into an ast-slot-def.
Handles both simple (name) and full ((name :initarg :name :reader name-reader)) forms."
  (if (symbolp spec)
      (make-ast-slot-def :name spec)
      (let ((name (first spec))
            (initarg nil) (initform nil) (reader nil) (writer nil) (accessor nil)
            (slot-type nil) (allocation :instance))
        (let ((opts (rest spec)))
          (loop while opts
                do (let ((key (pop opts)))
                     (case key
                       (:initarg (setf initarg (pop opts)))
                       (:initform (setf initform (when opts
                                                   (lower-sexp-to-ast (pop opts)))))
                       (:reader (setf reader (pop opts)))
                       (:writer (setf writer (pop opts)))
                       (:accessor (setf accessor (pop opts)))
                       (:documentation (pop opts))
                       (:type (setf slot-type (pop opts)))
                       (:allocation (setf allocation (pop opts)))
                       (otherwise (pop opts))))))
        (make-ast-slot-def
                       :name name
                       :initarg initarg
                       :initform initform
                       :reader reader
                       :writer writer
                       :accessor accessor
                       :type slot-type
                       :allocation allocation))))

;;; ── List-form lowering dispatch table ───────────────────────────────────────
;;;
;;; Each clause of the old 626-line CASE statement is now a named function
;;; registered via DEFINE-LIST-LOWERER.  The dispatch engine is a hash table
;;; lookup — same architecture as *expander-head-table* and
;;; *phase2-builtin-handlers*.

(defvar *list-lowering-table* (make-hash-table :test 'eq)
  "Maps form head symbols to handler functions (node sf sl sc) → ast-node.
Populated by DEFINE-LIST-LOWERER registrations below.")

(defmacro define-list-lowerer (heads (node sf sl sc) &body body)
  "Register a lowering handler for each symbol in HEADS.
Each handler is a lambda (node source-file source-line source-column)."
  `(let ((fn (lambda (,node ,sf ,sl ,sc) ,@body)))
     (dolist (head ',heads)
       (setf (gethash head *list-lowering-table*) fn))))

;;; ── Setf simple-place rewrite table ─────────────────────────────────────────
;;;
;;; Maps place-head → (runtime-fn num-place-args).
;;; Used by %lower-setf-place to dispatch the 14 trivial setf places
;;; without a 14-branch case statement.

(defparameter *setf-place-simple-rewrites*
  '((get              %set-symbol-prop   2)
    (symbol-plist     %set-symbol-plist  1)
    (aref             aset               2)
    (svref            %svset             2)
    (row-major-aref   aset               2)
    (fill-pointer     %set-fill-pointer  1)
    (car              rplaca             1)
    (cdr              rplacd             1)
    (bit              rt-bit-set         2)
    (sbit             rt-bit-set         2)
    (char             rt-string-set      2)
    (schar            rt-string-set      2)
    (elt              aset               2)
    (nth              %set-nth           2))
  "Alist mapping setf place heads to (runtime-fn num-place-args).
Simple places: the rewrite is (runtime-fn place-arg-1 ... place-arg-N value-form).
Complex places (slot-value, gethash, getf) are handled separately in %lower-setf-place.")

;;; Shared helpers ─────────────────────────────────────────────────────────────

(defun %lower-extended-params (raw-params)
  "Parse an extended lambda list, lowering default-value sexps to AST.
Returns (values required lowered-optional rest-param lowered-key)."
  (multiple-value-bind (required optional rest-param key-params)
      (parse-compiler-lambda-list raw-params)
    (values required
            (mapcar (lambda (opt)
                      (list (first opt)
                            (when (second opt) (lower-sexp-to-ast (second opt)))
                            (third opt)))  ; FR-696: preserve supplied-p name
                    optional)
            rest-param
            (mapcar (lambda (kp)
                      (list (first kp)
                            (when (second kp) (lower-sexp-to-ast (second kp)))
                            (third kp)))  ; FR-696: preserve supplied-p name
                    key-params))))

(defun %lower-local-fn-bindings (kind bindings)
  "Lower a list of FLET/LABELS bindings.  KIND is 'flet or 'labels for errors."
  (mapcar (lambda (binding)
            (unless (and (consp binding)
                         (>= (length binding) 3)
                         (symbolp (first binding))
                         (listp (second binding)))
              (error "Invalid ~A binding: ~S" kind binding))
            (list* (first binding)
                   (second binding)
                   (mapcar #'lower-sexp-to-ast (cddr binding))))
          bindings))

;;; ── Arithmetic and comparison binary operators ───────────────────────────────

(define-list-lowerer (+ - * / = < > <= >=) (node sf sl sc)
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
     :body (mapcar #'lower-sexp-to-ast (cddr node))
     :source-file sf :source-line sl :source-column sc)))

;;; ── Lambda expression ────────────────────────────────────────────────────────

(define-list-lowerer (lambda) (node sf sl sc)
  (unless (>= (length node) 3)
    (error "lambda requires parameters and body"))
  (let ((raw-params (second node)))
    (unless (listp raw-params)
      (error "lambda parameters must be a list"))
    (if (lambda-list-has-extended-p raw-params)
        (multiple-value-bind (required optional rest-param key-params)
            (%lower-extended-params raw-params)
          (make-ast-lambda :params required
                           :optional-params optional
                           :rest-param rest-param
                           :key-params key-params
                           :body (mapcar #'lower-sexp-to-ast (cddr node))
                           :source-file sf :source-line sl :source-column sc))
        (progn
          (unless (every #'symbolp raw-params)
            (error "lambda parameters must be symbols"))
          (make-ast-lambda :params raw-params
                           :body (mapcar #'lower-sexp-to-ast (cddr node))
                           :source-file sf :source-line sl :source-column sc)))))

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
    (let ((block-body (list (lower-sexp-to-ast (list* 'block name (cdddr node))))))
      (if (lambda-list-has-extended-p raw-params)
          (multiple-value-bind (required optional rest-param key-params)
              (%lower-extended-params raw-params)
            (make-ast-defun :name name :params required
                            :optional-params optional :rest-param rest-param
                            :key-params key-params   :body block-body
                            :source-file sf :source-line sl :source-column sc))
          (progn
            (unless (every #'symbolp raw-params)
              (error "defun parameters must be symbols"))
            (make-ast-defun :name name :params raw-params :body block-body
                            :source-file sf :source-line sl :source-column sc))))))

;;; ── Defvar / Defparameter ────────────────────────────────────────────────────

(define-list-lowerer (defvar defparameter) (node sf sl sc)
  (unless (>= (length node) 2)
    (error "~A requires at least a name" (car node)))
  (let ((name (second node)))
    (unless (symbolp name) (error "~A name must be a symbol" (car node)))
    (make-ast-defvar :name name
                     :value (when (>= (length node) 3)
                              (lower-sexp-to-ast (third node)))
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

;;; ── Dispatch engine ──────────────────────────────────────────────────────────

(defun lower-list-to-ast (node &key source-file source-line source-column)
  "Dispatch a list-form NODE to its registered handler, or lower as a call."
  (let ((handler (gethash (car node) *list-lowering-table*)))
    (if handler
        (funcall handler node source-file source-line source-column)
        ;; Default: treat as function call
        (make-ast-call :func (lower-sexp-to-ast (car node))
                       :args (mapcar #'lower-sexp-to-ast (cdr node))
                       :source-file source-file
                       :source-line source-line
                       :source-column source-column))))

(defmethod lower-sexp-to-ast ((node cons) &key source-file source-line source-column)
  (lower-list-to-ast node
                     :source-file source-file
                     :source-line source-line
                     :source-column source-column))

