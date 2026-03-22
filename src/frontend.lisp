;;;; frontend.lisp - Common Lisp Frontend with Full Special Forms Support
;;;; 
;;;; This module provides:
;;;; - AST node classes with source location tracking
;;;; - S-expression to AST transformation for all Common Lisp special forms
;;;; - Enhanced reader integration for source location capture

(in-package :cl-cc)

;;; AST Base Struct with Source Location Tracking

(defstruct (ast-node (:conc-name ast-))
  "Base struct for all AST nodes with optional source location tracking."
  (source-file nil)
  (source-line nil)
  (source-column nil))

;;; Intermediate AST Structs (shared slot groups)

(defstruct (ast-callable (:include ast-node))
  "Intermediate struct for callable forms (lambda, defun)."
  (params nil :type list)
  (optional-params nil :type list)
  (rest-param nil)
  (key-params nil :type list)
  (body nil :type list))

(defstruct (ast-local-fns (:include ast-node))
  "Intermediate struct for local function binding forms (flet, labels)."
  (bindings nil :type list)
  (body nil :type list))

;;; Core AST Structs

(defstruct (ast-int (:include ast-node))
  "Integer literal AST node."
  (value nil))

(defstruct (ast-var (:include ast-node))
  "Variable reference AST node."
  (name nil))

(defstruct (ast-binop (:include ast-node))
  "Binary operation AST node."
  (op nil)
  (lhs nil)
  (rhs nil))

(defstruct (ast-if (:include ast-node))
  "Conditional expression AST node."
  (cond nil)
  (then nil)
  (else nil))

(defstruct (ast-progn (:include ast-node))
  "Sequence of expressions AST node."
  (forms nil :type list))

(defstruct (ast-print (:include ast-node))
  "Print expression AST node."
  (expr nil))

(defstruct (ast-let (:include ast-node))
  "Let binding AST node."
  (bindings nil :type list)
  (body nil :type list))

;;; Function and Lambda AST Structs

(defstruct (ast-lambda (:include ast-callable))
  "Lambda expression AST node."
  (env nil))

(defstruct (ast-function (:include ast-node))
  "Function reference AST node for #'var."
  (name nil))

(defstruct (ast-flet (:include ast-local-fns))
  "Local non-recursive function bindings AST node.")

(defstruct (ast-labels (:include ast-local-fns))
  "Local recursive function bindings AST node.")

(defstruct (ast-defun (:include ast-callable))
  "Top-level function definition AST node."
  (name nil :type symbol))

(defstruct (ast-defvar (:include ast-node))
  "Top-level variable definition AST node (defvar/defparameter)."
  (name nil)
  (value nil))

(defstruct (ast-defmacro (:include ast-node))
  "Top-level macro definition AST node."
  (name nil)
  (lambda-list nil)
  (body nil :type list))

;;; Block and Control Flow AST Structs

(defstruct (ast-block (:include ast-node))
  "Named block AST node."
  (name nil)
  (body nil :type list))

(defstruct (ast-return-from (:include ast-node))
  "Return from named block AST node."
  (name nil)
  (value nil))

(defstruct (ast-tagbody (:include ast-node))
  "Tagged body for goto-style control flow AST node."
  (tags nil :type list))

(defstruct (ast-go (:include ast-node))
  "Go to tag AST node."
  (tag nil))

;;; Assignment and Variables AST Structs

(defstruct (ast-setq (:include ast-node))
  "Variable assignment AST node."
  (var nil)
  (value nil))

;;; Multiple Values AST Structs

(defstruct (ast-multiple-value-call (:include ast-node) (:conc-name ast-mv-call-))
  "Multiple-value-call special form AST node."
  (func nil)
  (args nil :type list))

(defstruct (ast-multiple-value-prog1 (:include ast-node) (:conc-name ast-mv-prog1-))
  "Multiple-value-prog1 special form AST node."
  (first nil)
  (forms nil :type list))

(defstruct (ast-values (:include ast-node))
  "Values form AST node."
  (forms nil :type list))

(defstruct (ast-multiple-value-bind (:include ast-node) (:conc-name ast-mvb-))
  "Multiple-value-bind AST node."
  (vars nil :type list)
  (values-form nil)
  (body nil :type list))

(defstruct (ast-apply (:include ast-node))
  "Apply form AST node."
  (func nil)
  (args nil :type list))

;;; Exception Handling AST Structs

(defstruct (ast-catch (:include ast-node))
  "Catch block AST node."
  (tag nil)
  (body nil :type list))

(defstruct (ast-throw (:include ast-node))
  "Throw AST node."
  (tag nil)
  (value nil))

(defstruct (ast-unwind-protect (:include ast-node) (:conc-name ast-unwind-))
  "Unwind-protect AST node."
  (protected nil)
  (cleanup nil :type list))

(defstruct (ast-handler-case (:include ast-node))
  "Handler-case AST node for condition handling."
  (form nil)
  (clauses nil :type list))

;;; Additional AST Structs for Common Operations

(defstruct (ast-call (:include ast-node))
  "Function call AST node (for non-special operator calls)."
  (func nil)
  (args nil :type list))

(defstruct (ast-quote (:include ast-node))
  "Quote special form AST node."
  (value nil))

(defstruct (ast-the (:include ast-node))
  "The type declaration AST node."
  (type nil)
  (value nil))

;;; CLOS AST Structs

(defstruct (ast-slot-def (:conc-name ast-slot-))
  "Slot definition for defclass."
  (name nil :type symbol)
  (initarg nil)
  (initform nil)
  (reader nil)
  (writer nil)
  (accessor nil)
  (type nil))

(defstruct (ast-defclass (:include ast-node))
  "Class definition AST node (defclass)."
  (name nil)
  (superclasses nil :type list)
  (slots nil :type list))

(defstruct (ast-defgeneric (:include ast-node))
  "Generic function definition AST node (defgeneric)."
  (name nil)
  (params nil :type list))

(defstruct (ast-defmethod (:include ast-node))
  "Method definition AST node (defmethod)."
  (name nil)
  (specializers nil :type list)
  (params nil :type list)
  (body nil :type list))

(defstruct (ast-make-instance (:include ast-node))
  "Object creation AST node (make-instance)."
  (class nil)
  (initargs nil :type list))

(defstruct (ast-slot-value (:include ast-node))
  "Slot access AST node (slot-value)."
  (object nil)
  (slot nil))

(defstruct (ast-set-slot-value (:include ast-node))
  "Slot mutation AST node (setf (slot-value obj 'slot) val)."
  (object nil)
  (slot nil)
  (value nil))

(defstruct (ast-set-gethash (:include ast-node))
  "Hash table mutation AST node (setf (gethash key table) value)."
  (key nil)
  (table nil)
  (value nil))

;;; Source Location Utilities

(defun make-ast-with-location (constructor &rest initargs)
  "Create an AST instance with source location information.
   CONSTRUCTOR is a struct constructor function (e.g. #'make-ast-int)."
  (apply constructor initargs))

(defun ast-location-string (node)
  "Return a human-readable string of the source location for NODE."
  (let ((file (ast-source-file node))
        (line (ast-source-line node))
        (col (ast-source-column node)))
    (cond
      ((and file line col)
       (format nil "~A:~D:~D" file line col))
      ((and file line)
       (format nil "~A:~D" file line))
      (file
       (format nil "~A" file))
      (t "<unknown location>"))))

(defun ast-error (node format-control &rest format-args)
  "Signal an error with source location information from NODE."
  (error 'ast-compilation-error
         :location (ast-location-string node)
         :format-control format-control
         :format-arguments format-args))

(define-condition ast-compilation-error (error)
  ((location :initarg :location :reader ast-error-location)
   (format-control :initarg :format-control :reader ast-error-format-control)
   (format-arguments :initarg :format-arguments :reader ast-error-format-arguments))
  (:report (lambda (condition stream)
             (format stream "Compilation error at ~A: ~?"
                     (ast-error-location condition)
                     (ast-error-format-control condition)
                     (ast-error-format-arguments condition)))))

;;; S-Expression Parser

(defun parse-source (source)
  "Parse SOURCE into one s-expression."
  (multiple-value-bind (form position)
      (read-from-string source nil :eof)
    (when (eq form :eof)
      (error "Empty source"))
    (let ((rest (subseq source position)))
      (unless (every (lambda (ch)
                       (member ch '(#\Space #\Tab #\Newline #\Return)))
                     rest)
        (error "Multiple top-level forms are not supported yet")))
    form))

(defun parse-all-forms (source)
  "Parse SOURCE into a list of all top-level s-expressions."
  (let ((forms nil)
        (position 0)
        (source-length (length source)))
    (loop
      (when (>= position source-length)
        (return))
      ;; Skip whitespace
      (loop while (and (< position source-length)
                       (member (char source position) '(#\Space #\Tab #\Newline #\Return)))
            do (incf position))
      (when (>= position source-length)
        (return))
      ;; Read next form
      (multiple-value-bind (form next-position)
          (handler-case
              (read-from-string source nil :eof :start position)
            (error (e)
              (error "Parse error at position ~D: ~A" position e)))
        (when (eq form :eof)
          (return))
        (push form forms)
        (setf position next-position)))
    (nreverse forms)))

;;; S-Expression to AST Transformation

(defgeneric lower-sexp-to-ast (node &key source-file source-line source-column)
  (:documentation "Convert an S-expression NODE to an AST node with optional source location."))

(defmethod lower-sexp-to-ast ((node integer) &key source-file source-line source-column)
  (make-ast-int :value node
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

(defmethod lower-sexp-to-ast ((node symbol) &key source-file source-line source-column)
  (make-ast-var :name node
                 :source-file source-file
                 :source-line source-line
                 :source-column source-column))

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
                            (push (list (first p) (second p)) optional)
                            (push (list p nil) optional)))
             (:rest     (unless (symbolp p)
                          (error "&rest parameter must be a symbol: ~S" p))
                        (setf rest-param p)
                        (setf state :post-rest))
             (:post-rest (cond ((eq p '&key) (setf state :key))
                               (t (error "Unexpected parameter after &rest: ~S" p))))
             (:key      (if (listp p)
                            (let ((name (if (listp (first p)) (second (first p)) (first p)))
                                  (default (second p)))
                              (push (list name default) key-params))
                            (push (list p nil) key-params)))))))
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
            (slot-type nil))
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
                       (otherwise (pop opts))))))
        (make-ast-slot-def
                       :name name
                       :initarg initarg
                       :initform initform
                       :reader reader
                       :writer writer
                       :accessor accessor
                       :type slot-type))))

(defun lower-list-to-ast (node &key source-file source-line source-column)
  "Helper to dispatch on list forms."
  (case (car node)
    ;; Arithmetic and comparison operators
    ((+ - * = < > <= >=)
     (unless (= (length node) 3)
       (error "~S takes exactly 2 args" (car node)))
     (make-ast-binop
                    :op (car node)
                    :lhs (lower-sexp-to-ast (second node))
                    :rhs (lower-sexp-to-ast (third node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))
    
    ;; Conditional
    (if
     (unless (member (length node) '(3 4))
       (error "if takes cond then [else]"))
     (make-ast-if
                    :cond (lower-sexp-to-ast (second node))
                    :then (lower-sexp-to-ast (third node))
                    :else (if (fourth node)
                              (lower-sexp-to-ast (fourth node))
                              (lower-sexp-to-ast nil))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))
    
    ;; Sequence
    (progn
     (when (< (length node) 2)
       (error "progn needs at least one form"))
     (make-ast-progn
                    :forms (mapcar #'lower-sexp-to-ast (cdr node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))
    
    ;; Print
    (print
     (unless (= (length node) 2)
       (error "print takes one arg"))
     (make-ast-print
                    :expr (lower-sexp-to-ast (second node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))
    
    ;; Let binding
    (let
     (unless (>= (length node) 3)
       (error "let requires bindings and body"))
     (let ((bindings (second node)))
       (unless (listp bindings)
         (error "let bindings must be a list"))
       (make-ast-let
        :bindings (mapcar (lambda (binding)
                            (unless (and (consp binding)
                                         (= (length binding) 2)
                                         (symbolp (first binding)))
                              (error "Invalid let binding: ~S" binding))
                            (cons (first binding)
                                  (lower-sexp-to-ast (second binding))))
                          bindings)
        :body (mapcar #'lower-sexp-to-ast (cddr node))
        :source-file source-file
        :source-line source-line
        :source-column source-column)))
    
    ;; Lambda expression
    (lambda
     (unless (>= (length node) 3)
       (error "lambda requires parameters and body"))
     (let ((raw-params (second node)))
       (unless (listp raw-params)
         (error "lambda parameters must be a list"))
       (if (lambda-list-has-extended-p raw-params)
           (multiple-value-bind (required optional rest-param key-params)
               (parse-compiler-lambda-list raw-params)
             (make-ast-lambda
                            :params required
                            :optional-params (mapcar (lambda (opt)
                                                       (list (first opt)
                                                             (when (second opt)
                                                               (lower-sexp-to-ast (second opt)))))
                                                     optional)
                            :rest-param rest-param
                            :key-params (mapcar (lambda (kp)
                                                  (list (first kp)
                                                        (when (second kp)
                                                          (lower-sexp-to-ast (second kp)))))
                                                key-params)
                            :body (mapcar #'lower-sexp-to-ast (cddr node))
                            :source-file source-file
                            :source-line source-line
                            :source-column source-column))
           (progn
             (unless (every #'symbolp raw-params)
               (error "lambda parameters must be symbols"))
             (make-ast-lambda
                            :params raw-params
                            :body (mapcar #'lower-sexp-to-ast (cddr node))
                            :source-file source-file
                            :source-line source-line
                            :source-column source-column)))))
    
    ;; Function reference (#'var)
    (function
     (unless (= (length node) 2)
       (error "function takes exactly one argument"))
     (let ((name (second node)))
       (unless (or (symbolp name)
                   (and (consp name) (eq (car name) 'setf)))
         (error "function argument must be a symbol or (setf name)"))
       (make-ast-function
                      :name name
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))
    
    ;; Block
    (block
     (unless (>= (length node) 3)
       (error "block requires a name and body"))
     (let ((name (second node)))
       (unless (symbolp name)
         (error "block name must be a symbol"))
       (make-ast-block
                      :name name
                      :body (mapcar #'lower-sexp-to-ast (cddr node))
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))
    
    ;; Return-from
    (return-from
     (unless (= (length node) 3)
       (error "return-from requires name and value"))
     (let ((name (second node)))
       (unless (symbolp name)
         (error "return-from name must be a symbol"))
       (make-ast-return-from
                      :name name
                      :value (lower-sexp-to-ast (third node))
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))
    
    ;; Tagbody
    (tagbody
     (when (< (length node) 2)
       (error "tagbody requires at least one tag or form"))
     (let ((tags nil)
           (current-tag nil)
           (current-forms nil))
       (dolist (item (cdr node))
         (if (or (symbolp item) (integerp item))
             ;; This is a tag
             (progn
               (when current-tag
                 (push (cons current-tag (nreverse current-forms)) tags))
               (setf current-tag item
                     current-forms nil))
             ;; This is a form
             (progn
               ;; Forms before any tag get a synthesized initial tag
               (unless current-tag
                 (setf current-tag (gensym "TAGBODY-START")))
               (push (lower-sexp-to-ast item) current-forms))))
       ;; Add last tag
       (when current-tag
         (push (cons current-tag (nreverse current-forms)) tags))
       (make-ast-tagbody
                      :tags (nreverse tags)
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))
    
    ;; Go
    (go
     (unless (= (length node) 2)
       (error "go requires exactly one tag"))
     (let ((tag (second node)))
       (unless (or (symbolp tag) (integerp tag))
         (error "go tag must be a symbol or integer"))
       (make-ast-go
                      :tag tag
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))
    
    ;; Setq
    (setq
     (unless (= (length node) 3)
       (error "setq requires exactly variable and value"))
     (let ((var (second node)))
       (unless (symbolp var)
         (error "setq variable must be a symbol"))
       (make-ast-setq
                      :var var
                      :value (lower-sexp-to-ast (third node))
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))
    
    ;; Setf - generalized assignment
    (setf
     (unless (= (length node) 3)
       (error "setf requires a place and a value"))
     (let ((place (second node))
           (value-form (third node)))
       (unless (and (consp place) (member (car place) '(slot-value gethash)))
         (error "setf only supports (slot-value obj 'slot) and (gethash key table) places"))
       (cond
         ((eq (car place) 'slot-value)
          (let ((obj-form (second place))
                (slot-form (third place)))
            (let ((slot-name (if (and (consp slot-form) (eq (car slot-form) 'quote))
                                 (second slot-form)
                                 (error "setf slot-value slot must be quoted"))))
              (make-ast-set-slot-value
                             :object (lower-sexp-to-ast obj-form)
                             :slot slot-name
                             :value (lower-sexp-to-ast value-form)
                             :source-file source-file
                             :source-line source-line
                             :source-column source-column))))
         ((eq (car place) 'gethash)
          (let ((key-form (second place))
                (table-form (third place)))
            (make-ast-set-gethash
                           :key (lower-sexp-to-ast key-form)
                           :table (lower-sexp-to-ast table-form)
                           :value (lower-sexp-to-ast value-form)
                           :source-file source-file
                           :source-line source-line
                           :source-column source-column))))))

    ;; Flet (non-recursive local functions)
    (flet
     (unless (>= (length node) 3)
       (error "flet requires bindings and body"))
     (let ((bindings (second node)))
       (unless (listp bindings)
         (error "flet bindings must be a list"))
       (make-ast-flet
                      :bindings (mapcar (lambda (binding)
                                          (unless (and (consp binding)
                                                       (>= (length binding) 3)
                                                       (symbolp (first binding))
                                                       (listp (second binding)))
                                            (error "Invalid flet binding: ~S" binding))
                                          (list* (first binding)
                                                 (second binding)
                                                 (mapcar #'lower-sexp-to-ast (cddr binding))))
                                        bindings)
                      :body (mapcar #'lower-sexp-to-ast (cddr node))
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))
    
    ;; Labels (mutually recursive local functions)
    (labels
     (unless (>= (length node) 3)
       (error "labels requires bindings and body"))
     (let ((bindings (second node)))
       (unless (listp bindings)
         (error "labels bindings must be a list"))
       (make-ast-labels
                      :bindings (mapcar (lambda (binding)
                                          (unless (and (consp binding)
                                                       (>= (length binding) 3)
                                                       (symbolp (first binding))
                                                       (listp (second binding)))
                                            (error "Invalid labels binding: ~S" binding))
                                          (list* (first binding)
                                                 (second binding)
                                                 (mapcar #'lower-sexp-to-ast (cddr binding))))
                                        bindings)
                      :body (mapcar #'lower-sexp-to-ast (cddr node))
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))
    
    ;; Defun (top-level function definition)
    (defun
     (unless (>= (length node) 3)
       (error "defun requires name, parameters and body"))
     (let ((name (second node))
           (raw-params (third node)))
       (unless (symbolp name)
         (error "defun name must be a symbol"))
       (unless (listp raw-params)
         (error "defun parameters must be a list"))
       (if (lambda-list-has-extended-p raw-params)
           (multiple-value-bind (required optional rest-param key-params)
               (parse-compiler-lambda-list raw-params)
             (make-ast-defun
                            :name name
                            :params required
                            :optional-params (mapcar (lambda (opt)
                                                       (list (first opt)
                                                             (when (second opt)
                                                               (lower-sexp-to-ast (second opt)))))
                                                     optional)
                            :rest-param rest-param
                            :key-params (mapcar (lambda (kp)
                                                  (list (first kp)
                                                        (when (second kp)
                                                          (lower-sexp-to-ast (second kp)))))
                                                key-params)
                            :body (mapcar #'lower-sexp-to-ast (cdddr node))
                            :source-file source-file
                            :source-line source-line
                            :source-column source-column))
           (progn
             (unless (every #'symbolp raw-params)
               (error "defun parameters must be symbols"))
             (make-ast-defun
                            :name name
                            :params raw-params
                            :body (mapcar #'lower-sexp-to-ast (cdddr node))
                            :source-file source-file
                            :source-line source-line
                            :source-column source-column)))))

    ;; Defvar / Defparameter (top-level variable definition)
    ((defvar defparameter)
     (unless (>= (length node) 2)
       (error "~A requires at least a name" (car node)))
     (let ((name (second node)))
       (unless (symbolp name)
         (error "~A name must be a symbol" (car node)))
       (make-ast-defvar
                      :name name
                      :value (when (>= (length node) 3)
                               (lower-sexp-to-ast (third node)))
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))
    
    ;; Defmacro (top-level macro definition)
    (defmacro
     (unless (>= (length node) 4)
       (error "defmacro requires name, lambda-list, and body"))
     (let ((name (second node))
           (lambda-list (third node))
           (body (cdddr node)))
       (unless (symbolp name)
         (error "defmacro name must be a symbol"))
       (make-ast-defmacro
                      :name name
                      :lambda-list lambda-list
                      :body body
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))

    ;; Defclass (CLOS class definition)
    (defclass
     (unless (>= (length node) 4)
       (error "defclass requires name, superclasses, and slot definitions"))
     (let ((name (second node))
           (superclasses (third node))
           (slot-specs (fourth node)))
       (unless (symbolp name)
         (error "defclass name must be a symbol"))
       (unless (listp superclasses)
         (error "defclass superclasses must be a list"))
       (unless (listp slot-specs)
         (error "defclass slots must be a list"))
       (let ((slots (mapcar #'parse-slot-spec slot-specs)))
         (make-ast-defclass
                        :name name
                        :superclasses superclasses
                        :slots slots
                        :source-file source-file
                        :source-line source-line
                        :source-column source-column))))

    ;; Defgeneric
    (defgeneric
     (unless (>= (length node) 3)
       (error "defgeneric requires name and lambda-list"))
     (let ((name (second node))
           (params (third node)))
       (unless (symbolp name)
         (error "defgeneric name must be a symbol"))
       (make-ast-defgeneric
                      :name name
                      :params params
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))

    ;; Defmethod
    (defmethod
     (unless (>= (length node) 4)
       (error "defmethod requires name, parameters, and body"))
     (let* ((name (second node))
            (raw-params (third node))
            (specializers nil)
            (param-names nil))
       (unless (symbolp name)
         (error "defmethod name must be a symbol"))
       ;; Parse specialized parameter list: ((x class-name) y z) -> specializers + names
       (dolist (p raw-params)
         (if (listp p)
             (progn
               (push (cons (first p) (second p)) specializers)
               (push (first p) param-names))
             (progn
               (push nil specializers)
               (push p param-names))))
       (make-ast-defmethod
                      :name name
                      :specializers (nreverse specializers)
                      :params (nreverse param-names)
                      :body (mapcar #'lower-sexp-to-ast (cdddr node))
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))

    ;; Make-instance
    (make-instance
     (unless (>= (length node) 2)
       (error "make-instance requires at least a class name"))
     (let ((class-expr (lower-sexp-to-ast (second node)))
           (initargs nil))
       ;; Parse keyword arguments: :key1 val1 :key2 val2 ...
       (let ((rest (cddr node)))
         (loop while rest
               do (let ((key (pop rest)))
                    (unless (keywordp key)
                      (error "make-instance initarg must be a keyword, got ~S" key))
                    (unless rest
                      (error "make-instance initarg ~S missing value" key))
                    (push (cons key (lower-sexp-to-ast (pop rest))) initargs))))
       (make-ast-make-instance
                      :class class-expr
                      :initargs (nreverse initargs)
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))

    ;; Slot-value
    (slot-value
     (unless (= (length node) 3)
       (error "slot-value requires object and slot-name"))
     (make-ast-slot-value
                    :object (lower-sexp-to-ast (second node))
                    :slot (let ((sn (third node)))
                                 (if (and (listp sn) (eq (car sn) 'quote))
                                     (second sn)
                                     sn))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))

    ;; Values
    (values
     (make-ast-values
                    :forms (mapcar #'lower-sexp-to-ast (cdr node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))

    ;; Multiple-value-bind
    (multiple-value-bind
     (unless (>= (length node) 4)
       (error "multiple-value-bind requires vars, values-form, and body"))
     (let ((vars (second node))
           (values-form (third node)))
       (unless (and (listp vars) (every #'symbolp vars))
         (error "multiple-value-bind vars must be a list of symbols"))
       (make-ast-multiple-value-bind
                      :vars vars
                      :values-form (lower-sexp-to-ast values-form)
                      :body (mapcar #'lower-sexp-to-ast (cdddr node))
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))

    ;; Apply
    (apply
     (unless (>= (length node) 3)
       (error "apply requires at least a function and one argument"))
     (make-ast-apply
                    :func (lower-sexp-to-ast (second node))
                    :args (mapcar #'lower-sexp-to-ast (cddr node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))

    ;; Multiple-value-call
    (multiple-value-call
     (unless (>= (length node) 3)
       (error "multiple-value-call requires function and arguments"))
     (make-ast-multiple-value-call
                    :func (lower-sexp-to-ast (second node))
                    :args (mapcar #'lower-sexp-to-ast (cddr node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))
    
    ;; Multiple-value-prog1
    (multiple-value-prog1
     (unless (>= (length node) 2)
       (error "multiple-value-prog1 requires at least one form"))
     (make-ast-multiple-value-prog1
                    :first (lower-sexp-to-ast (second node))
                    :forms (mapcar #'lower-sexp-to-ast (cddr node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))
    
    ;; Catch
    (catch
     (unless (>= (length node) 3)
       (error "catch requires tag and body"))
     (make-ast-catch
                    :tag (lower-sexp-to-ast (second node))
                    :body (mapcar #'lower-sexp-to-ast (cddr node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))
    
    ;; Throw
    (throw
     (unless (= (length node) 3)
       (error "throw requires tag and value"))
     (make-ast-throw
                    :tag (lower-sexp-to-ast (second node))
                    :value (lower-sexp-to-ast (third node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))
    
    ;; Unwind-protect
    (unwind-protect
     (unless (>= (length node) 3)
       (error "unwind-protect requires protected form and cleanup"))
     (make-ast-unwind-protect
                    :protected (lower-sexp-to-ast (second node))
                    :cleanup (mapcar #'lower-sexp-to-ast (cddr node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))

    ;; Handler-case
    (handler-case
     (unless (>= (length node) 3)
       (error "handler-case requires a form and at least one handler clause"))
     (let ((protected-form (lower-sexp-to-ast (second node)))
           (clauses (mapcar (lambda (clause)
                              ;; Each clause: (error-type (var) body...)
                              (unless (and (consp clause)
                                           (>= (length clause) 2)
                                           (symbolp (first clause))
                                           (listp (second clause)))
                                (error "Invalid handler-case clause: ~S" clause))
                              (let ((error-type (first clause))
                                    (var (if (second clause) (first (second clause)) nil))
                                    (body (mapcar #'lower-sexp-to-ast (cddr clause))))
                                (list* error-type var body)))
                            (cddr node))))
       (make-ast-handler-case
                      :form protected-form
                      :clauses clauses
                      :source-file source-file
                      :source-line source-line
                      :source-column source-column)))
    
    ;; Quote
    (quote
     (unless (= (length node) 2)
       (error "quote takes exactly one argument"))
     (make-ast-quote
                    :value (second node)
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))
    
    ;; The type declaration
    (the
     (unless (= (length node) 3)
       (error "the requires type and value"))
     (make-ast-the
                    :type (second node)
                    :value (lower-sexp-to-ast (third node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))

    ;; values: (values expr1 expr2 ...)
    (values
     (make-ast-values
                    :forms (mapcar #'lower-sexp-to-ast (cdr node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))

    ;; multiple-value-bind: (multiple-value-bind (v1 v2 ...) values-form body...)
    (multiple-value-bind
     (unless (>= (length node) 3)
       (error "multiple-value-bind requires vars, values-form, and body"))
     (make-ast-multiple-value-bind
                    :vars (second node)
                    :values-form (lower-sexp-to-ast (third node))
                    :body (mapcar #'lower-sexp-to-ast (cdddr node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))

    ;; funcall: (funcall fn args...) -> (ast-call fn args...)
    (funcall
     (unless (>= (length node) 2)
       (error "funcall requires at least a function argument"))
     (make-ast-call
                    :func (lower-sexp-to-ast (second node))
                    :args (mapcar #'lower-sexp-to-ast (cddr node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))

    ;; Default: treat as function call
    (otherwise
     (make-ast-call
                    :func (lower-sexp-to-ast (car node))
                    :args (mapcar #'lower-sexp-to-ast (cdr node))
                    :source-file source-file
                    :source-line source-line
                    :source-column source-column))))

(defmethod lower-sexp-to-ast ((node cons) &key source-file source-line source-column)
  (lower-list-to-ast node
                     :source-file source-file
                     :source-line source-line
                     :source-column source-column))

;;; AST Pretty Printing

(defgeneric ast-to-sexp (node)
  (:documentation "Convert an AST node back to an S-expression for debugging."))

(defmethod ast-to-sexp ((node ast-int))
  (ast-int-value node))

(defmethod ast-to-sexp ((node ast-var))
  (ast-var-name node))

(defmethod ast-to-sexp ((node ast-binop))
  (list (ast-binop-op node)
        (ast-to-sexp (ast-binop-lhs node))
        (ast-to-sexp (ast-binop-rhs node))))

(defmethod ast-to-sexp ((node ast-if))
  (list 'if
        (ast-to-sexp (ast-if-cond node))
        (ast-to-sexp (ast-if-then node))
        (ast-to-sexp (ast-if-else node))))

(defmethod ast-to-sexp ((node ast-progn))
  (cons 'progn (mapcar #'ast-to-sexp (ast-progn-forms node))))

(defmethod ast-to-sexp ((node ast-print))
  (list 'print (ast-to-sexp (ast-print-expr node))))

(defmethod ast-to-sexp ((node ast-let))
  (list* 'let
         (mapcar (lambda (binding)
                   (list (car binding) (ast-to-sexp (cdr binding))))
                 (ast-let-bindings node))
         (mapcar #'ast-to-sexp (ast-let-body node))))

(defmethod ast-to-sexp ((node ast-lambda))
  (list* 'lambda
         (ast-lambda-params node)
         (mapcar #'ast-to-sexp (ast-lambda-body node))))

(defmethod ast-to-sexp ((node ast-function))
  (list 'function (ast-function-name node)))

(defmethod ast-to-sexp ((node ast-block))
  (list* 'block
         (ast-block-name node)
         (mapcar #'ast-to-sexp (ast-block-body node))))

(defmethod ast-to-sexp ((node ast-return-from))
  (list 'return-from
        (ast-return-from-name node)
        (ast-to-sexp (ast-return-from-value node))))

(defmethod ast-to-sexp ((node ast-tagbody))
  (let ((result (list 'tagbody)))
    (dolist (tag-entry (ast-tagbody-tags node))
      (push (car tag-entry) result)
      (dolist (form (cdr tag-entry))
        (push (ast-to-sexp form) result)))
    (nreverse result)))

(defmethod ast-to-sexp ((node ast-go))
  (list 'go (ast-go-tag node)))

(defmethod ast-to-sexp ((node ast-setq))
  (list 'setq
        (ast-setq-var node)
        (ast-to-sexp (ast-setq-value node))))

(defmethod ast-to-sexp ((node ast-flet))
  (list* 'flet
         (mapcar (lambda (binding)
                   (list* (first binding)
                          (second binding)
                          (mapcar #'ast-to-sexp (cddr binding))))
                 (ast-flet-bindings node))
         (mapcar #'ast-to-sexp (ast-flet-body node))))

(defmethod ast-to-sexp ((node ast-labels))
  (list* 'labels
         (mapcar (lambda (binding)
                   (list* (first binding)
                          (second binding)
                          (mapcar #'ast-to-sexp (cddr binding))))
                 (ast-labels-bindings node))
         (mapcar #'ast-to-sexp (ast-labels-body node))))

(defmethod ast-to-sexp ((node ast-defun))
  (list* 'defun
         (ast-defun-name node)
         (ast-defun-params node)
         (mapcar #'ast-to-sexp (ast-defun-body node))))

(defmethod ast-to-sexp ((node ast-defvar))
  (if (ast-defvar-value node)
      (list 'defvar
            (ast-defvar-name node)
            (ast-to-sexp (ast-defvar-value node)))
      (list 'defvar
            (ast-defvar-name node))))

(defmethod ast-to-sexp ((node ast-defmacro))
  (list* 'defmacro
         (ast-defmacro-name node)
         (ast-defmacro-lambda-list node)
         (ast-defmacro-body node)))

(defmethod ast-to-sexp ((node ast-multiple-value-call))
  (list* 'multiple-value-call
         (ast-to-sexp (ast-mv-call-func node))
         (mapcar #'ast-to-sexp (ast-mv-call-args node))))

(defmethod ast-to-sexp ((node ast-multiple-value-prog1))
  (list* 'multiple-value-prog1
         (ast-to-sexp (ast-mv-prog1-first node))
         (mapcar #'ast-to-sexp (ast-mv-prog1-forms node))))

(defmethod ast-to-sexp ((node ast-values))
  (cons 'values (mapcar #'ast-to-sexp (ast-values-forms node))))

(defmethod ast-to-sexp ((node ast-multiple-value-bind))
  (list* 'multiple-value-bind
         (ast-mvb-vars node)
         (ast-to-sexp (ast-mvb-values-form node))
         (mapcar #'ast-to-sexp (ast-mvb-body node))))

(defmethod ast-to-sexp ((node ast-apply))
  (list* 'apply
         (ast-to-sexp (ast-apply-func node))
         (mapcar #'ast-to-sexp (ast-apply-args node))))

(defmethod ast-to-sexp ((node ast-catch))
  (list* 'catch
         (ast-to-sexp (ast-catch-tag node))
         (mapcar #'ast-to-sexp (ast-catch-body node))))

(defmethod ast-to-sexp ((node ast-throw))
  (list 'throw
        (ast-to-sexp (ast-throw-tag node))
        (ast-to-sexp (ast-throw-value node))))

(defmethod ast-to-sexp ((node ast-unwind-protect))
  (list* 'unwind-protect
         (ast-to-sexp (ast-unwind-protected node))
         (mapcar #'ast-to-sexp (ast-unwind-cleanup node))))

(defmethod ast-to-sexp ((node ast-handler-case))
  (list* 'handler-case
         (ast-to-sexp (ast-handler-case-form node))
         (mapcar (lambda (clause)
                   (let ((error-type (first clause))
                         (var (second clause))
                         (body (cddr clause)))
                     (list* error-type
                            (if var (list var) nil)
                            (mapcar #'ast-to-sexp body))))
                 (ast-handler-case-clauses node))))

(defmethod ast-to-sexp ((node ast-call))
  (let ((func (ast-call-func node))
        (args (mapcar #'ast-to-sexp (ast-call-args node))))
    (if (typep func 'ast-node)
        (cons (ast-to-sexp func) args)
        (cons func args))))

(defmethod ast-to-sexp ((node ast-quote))
  (list 'quote (ast-quote-value node)))

(defmethod ast-to-sexp ((node ast-the))
  (list 'the
        (ast-the-type node)
        (ast-to-sexp (ast-the-value node))))

(defmethod ast-to-sexp ((node ast-values))
  (cons 'values (mapcar #'ast-to-sexp (ast-values-forms node))))

(defmethod ast-to-sexp ((node ast-multiple-value-bind))
  (list* 'multiple-value-bind
         (ast-mvb-vars node)
         (ast-to-sexp (ast-mvb-values-form node))
         (mapcar #'ast-to-sexp (ast-mvb-body node))))

;;; CLOS AST to S-expression roundtrip

(defun slot-def-to-sexp (slot)
  "Convert an ast-slot-def back to a slot specification s-expression."
  (let ((opts nil))
    (when (ast-slot-accessor slot) (push (ast-slot-accessor slot) opts) (push :accessor opts))
    (when (ast-slot-writer slot) (push (ast-slot-writer slot) opts) (push :writer opts))
    (when (ast-slot-reader slot) (push (ast-slot-reader slot) opts) (push :reader opts))
    (when (ast-slot-initform slot) (push (ast-to-sexp (ast-slot-initform slot)) opts) (push :initform opts))
    (when (ast-slot-initarg slot) (push (ast-slot-initarg slot) opts) (push :initarg opts))
    (if opts
        (cons (ast-slot-name slot) opts)
        (ast-slot-name slot))))

(defmethod ast-to-sexp ((node ast-defclass))
  (list* 'defclass
         (ast-defclass-name node)
         (ast-defclass-superclasses node)
         (list (mapcar #'slot-def-to-sexp (ast-defclass-slots node)))))

(defmethod ast-to-sexp ((node ast-defgeneric))
  (list 'defgeneric
        (ast-defgeneric-name node)
        (ast-defgeneric-params node)))

(defmethod ast-to-sexp ((node ast-defmethod))
  (let ((params (loop for name in (ast-defmethod-params node)
                      for spec in (ast-defmethod-specializers node)
                      collect (if spec (list name (cdr spec)) name))))
    (list* 'defmethod
           (ast-defmethod-name node)
           params
           (mapcar #'ast-to-sexp (ast-defmethod-body node)))))

(defmethod ast-to-sexp ((node ast-make-instance))
  (let ((args (list 'make-instance (ast-to-sexp (ast-make-instance-class node)))))
    (dolist (pair (ast-make-instance-initargs node))
      (setf args (append args (list (car pair) (ast-to-sexp (cdr pair))))))
    args))

(defmethod ast-to-sexp ((node ast-slot-value))
  (list 'slot-value
        (ast-to-sexp (ast-slot-value-object node))
        (list 'quote (ast-slot-value-slot node))))

(defmethod ast-to-sexp ((node ast-set-slot-value))
  (list 'setf
        (list 'slot-value
              (ast-to-sexp (ast-set-slot-value-object node))
              (list 'quote (ast-set-slot-value-slot node)))
        (ast-to-sexp (ast-set-slot-value-value node))))

(defmethod ast-to-sexp ((node ast-set-gethash))
  (list 'setf
        (list 'gethash
              (ast-to-sexp (ast-set-gethash-key node))
              (ast-to-sexp (ast-set-gethash-table node)))
        (ast-to-sexp (ast-set-gethash-value node))))
