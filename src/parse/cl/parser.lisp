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

(defun %extract-leading-type-declarations (forms)
  "Extract leading (declare (type ...)) forms from FORMS.

Returns (values type-bindings stripped-forms), where TYPE-BINDINGS is an alist
mapping variable symbols to type specifiers. A leading docstring is preserved
in the stripped forms and declarations are extracted after it."
  (let ((bindings nil)
        (rest forms)
        (prefix nil))
    (when (and rest (stringp (first rest)))
      (push (first rest) prefix)
      (setf rest (rest rest)))
    (loop while (and rest (consp (first rest)) (eq (caar rest) 'declare)) do
      (dolist (spec (cdar rest))
        (when (and (consp spec)
                   (eq (car spec) 'type)
                   (>= (length spec) 3))
          (let ((type-spec (second spec)))
            (dolist (var (cddr spec))
              (setf bindings (acons var type-spec bindings))))))
      (setf rest (cdr rest)))
    (values bindings (nconc (nreverse prefix) rest))))

(defun %apply-type-bindings-to-params (params type-bindings)
  "Replace matching required PARAMS with typed parameter pairs."
  (mapcar (lambda (param)
            (let ((entry (and (symbolp param) (assoc param type-bindings))))
              (if entry
                  (list param (cdr entry))
                  param)))
          params))

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
