;;;; packages/frontend/parse/src/cl/parser-sexp-lowering.lisp — List-Form Lowering Machinery
;;;
;;; Extracted from parser.lisp.
;;; Contains:
;;;   - *list-lowering-table*      — hash table mapping head symbols to handlers
;;;   - define-list-lowerer        — macro to register lowering handlers
;;;   - *setf-place-simple-rewrites* — data-driven setf dispatch table
;;;   - %lower-extended-params     — parse extended lambda list + lower defaults
;;;   - %extract-leading-type-declarations
;;;   - %extract-leading-declarations
;;;   - %apply-type-bindings-to-params
;;;   - %lower-local-fn-bindings
;;;   - lower-list-to-ast          — dispatch-table lookup → call fallback
;;;   - lower-sexp-to-ast (cons method) — delegates to lower-list-to-ast
;;;
;;; Depends on parser.lisp (parse-compiler-lambda-list, make-ast-*, lower-sexp-to-ast
;;; generic function declared there). Load order: after parser.lisp, before lower.lisp.
(in-package :cl-cc/parse)

;;; ── List-form lowering dispatch table ───────────────────────────────────────
;;;
;;; Each special form has a named handler registered via DEFINE-LIST-LOWERER.
;;; The dispatch engine is a hash table lookup — same architecture as
;;; *expander-head-table* and *phase2-builtin-handlers*.

(defvar *list-lowering-table* (make-hash-table :test 'eq)
  "Maps form head symbols to handler functions (node sf sl sc) → ast-node.
Populated by DEFINE-LIST-LOWERER registrations below.")

(defmacro define-list-lowerer (heads (node sf sl sc) &body body)
  "Register a lowering handler for each symbol in HEADS.
Each handler is a lambda (node source-file source-line source-column)."
  (list 'let (list (list 'fn (cons 'lambda (cons (list node sf sl sc) body))))
        (list 'dolist (list 'head (list 'quote heads))
              (list 'setf (list 'gethash 'head '*list-lowering-table*) 'fn))))

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
      (let ((preserved-specs nil))
        (dolist (spec (cdar rest))
          (if (and (consp spec)
                   (eq (car spec) 'type)
                   (>= (length spec) 3))
              (let ((type-spec (second spec)))
                (dolist (var (cddr spec))
                  (setf bindings (acons var type-spec bindings))))
              (push spec preserved-specs)))
        (when preserved-specs
          (push `(declare ,@(nreverse preserved-specs)) prefix)))
      (setf rest (cdr rest)))
    (values bindings (nconc (nreverse prefix) rest))))

(defun %extract-leading-declarations (forms)
  "Extract leading DECLARE forms from FORMS.

Returns (values declarations stripped-forms), where DECLARATIONS is a list of
the declaration clauses found inside leading DECLARE forms. Leading docstrings
are preserved in the stripped forms and declarations are extracted after it."
  (let ((decls nil)
        (rest forms)
        (prefix nil))
    (when (and rest (stringp (first rest)))
      (push (first rest) prefix)
      (setf rest (rest rest)))
    (loop while (and rest (consp (first rest)) (eq (caar rest) 'declare)) do
      (dolist (spec (cdar rest))
        (push spec decls))
      (setf rest (cdr rest)))
    (values (nreverse decls) (nconc (nreverse prefix) rest))))

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
