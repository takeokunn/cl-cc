;;;; frontend/cl/parser.lisp - Common Lisp S-Expression Parser
;;;;
;;;; This module provides:
;;;; - S-expression parsing from source strings (parse-source, parse-all-forms)
;;;; - S-expression to AST transformation (lower-sexp-to-ast)
;;;; - AST to S-expression roundtrip (ast-to-sexp)

(in-package :cl-cc/parse)

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

(defmethod lower-sexp-to-ast ((node ast-node) &key source-file source-line source-column)
  (declare (ignore source-file source-line source-column))
  node)

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

(defmethod lower-sexp-to-ast ((node pathname) &key source-file source-line source-column)
  ;; #P"..." pathname literals are self-evaluating.
  (make-ast-quote :value node
                  :source-file source-file
                  :source-line source-line
                  :source-column source-column))

(defmethod lower-sexp-to-ast ((node symbol) &key source-file source-line source-column)
  ;; nil and t are self-evaluating constants, not variable references.
  ;; '_' is reserved as an expression-level typed hole.
  (cond
    ((member node '(nil t))
     (make-ast-quote :value node
                     :source-file source-file
                     :source-line source-line
                     :source-column source-column))
    ((string= (symbol-name node) "_")
     (make-ast-hole :source-file source-file
                    :source-line source-line
                    :source-column source-column))
    (t
     (make-ast-var :name node
                   :source-file source-file
                   :source-line source-line
                   :source-column source-column))))

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
              (:post-rest (error "Unexpected parameter after &rest: ~S" p))
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

;;; Wire parse-all-forms into VM hook for runtime READ support
(defun %vm-install-parse-forms-hook-if-available ()
  (when cl-cc/bootstrap::*vm-parse-forms-hook-installer*
    (funcall cl-cc/bootstrap::*vm-parse-forms-hook-installer* #'parse-all-forms)))

#-cl-cc-self-hosting
(eval-when (:load-toplevel :execute)
  (%vm-install-parse-forms-hook-if-available))

;;; *list-lowering-table*, define-list-lowerer, *setf-place-simple-rewrites*,
;;; shared helpers (%lower-extended-params, %extract-leading-*,
;;; %apply-type-bindings-to-params, %lower-local-fn-bindings),
;;; and the dispatch engine (lower-list-to-ast, lower-sexp-to-ast cons method)
;;; are in parser-sexp-lowering.lisp (loads next).
