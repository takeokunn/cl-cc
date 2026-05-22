(in-package :cl-cc/expand)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expander — Logic Layer
;;;
;;; Data lives in expander-data.lisp; defstruct helpers in expander-defstruct.lisp.
;;; This file: helper functions, define-expander-for registration macro,
;;; all handler registrations, and the short table-driven compiler-macroexpand-all.
;;;
;;; Design: *expander-head-table* is a Prolog-style clause database.
;;; Each (define-expander-for HEAD (form) body...) adds one clause.
;;; compiler-macroexpand-all is the inference engine — ~15 lines.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Registration macro ───────────────────────────────────────────────────

(defmacro define-expander-for (head (form) &body body)
  "Register a handler in *expander-head-table* for forms whose head is HEAD.
Contract: handler receives the full form and returns a fully-expanded form."
  (list 'setf
        (list 'gethash (list 'quote head) '*expander-head-table*)
        (cons 'lambda (cons (list form) body))))

;;; ── Expander handler registrations ──────────────────────────────────────
;;;
;;; Each (define-expander-for HEAD ...) corresponds to one clause in the
;;; Prolog sense: head(Form) :- body(Form).  The inference engine
;;; (compiler-macroexpand-all) queries this database by head symbol.

(define-expander-for defmethod (form)
  "Expand only DEFMETHOD body forms, preserving qualifier metadata verbatim."
  (let* ((name (second form))
         (tail (cddr form))
         (head (first tail))
         (has-qualifier (and head (symbolp head) (not (listp head))))
         (qualifier (and has-qualifier head))
         (lambda-list (if has-qualifier (second tail) head))
         (body (if has-qualifier (cddr tail) (cdr tail)))
         (expanded-body (mapcar #'compiler-macroexpand-all body)))
    (if has-qualifier
        (append (list 'defmethod name qualifier lambda-list) expanded-body)
        (append (list 'defmethod name lambda-list) expanded-body))))

;;; ── Main dispatcher ──────────────────────────────────────────────────────
;;;
;;; The inference engine: 4 clauses, ~15 lines.
;;; Handlers in *expander-head-table* take priority over *compiler-special-forms*.

(defun compiler-macroexpand-all (form)
  "Expand macros in FORM for the compiler pipeline.
Dispatch order: (1) atoms — symbol macros expanded, others pass through;
(2) *expander-head-table* handlers; (3) *compiler-special-forms* recurse-fallback;
(4) our-macroexpand-1."
  (cond
    ((atom form)
     (if (and (symbolp form)
              (not (keywordp form))
              (gethash form *symbol-macro-table*))
          (compiler-macroexpand-all (gethash form *symbol-macro-table*))
          form))
    ((and (symbolp (car form))
          (string= (symbol-name (car form)) "QUOTE"))
     form)
    ((and (symbolp (car form))
          (member (car form) *compiler-local-function-names* :test #'eq))
     (cons (car form) (mapcar #'compiler-macroexpand-all (cdr form))))
    (t
     (let ((handler (or (gethash (car form) *expander-head-table*)
                          (and (symbolp (car form))
                               (let* ((pkg (find-package :cl-cc/expand))
                                      (local (progn
                                               (when (cl-cc/vm::package-locked-p pkg)
                                                 (cl-cc/vm::check-package-lock pkg :intern))
                                               (intern (symbol-name (car form)) pkg))))
                                 (and local (gethash local *expander-head-table*)))))))
       (cond
         (handler
          (let ((expanded (funcall handler form)))
            (if (equal expanded form)
                form
                expanded)))
          ((and (symbolp (car form))
                (lookup-compiler-macro (car form)))
            (let ((expanded (invoke-registered-expander
                             (lookup-compiler-macro (car form))
                             form nil)))
              (if (equal expanded form)
                  form
                  (compiler-macroexpand-all expanded))))
          (t
           (multiple-value-bind (transformed transformed-p)
               (deftransform-expand-1 form nil)
             (cond
               (transformed-p
                (compiler-macroexpand-all transformed))
               ((%list-contains-eq (car form) *compiler-special-forms*)
                (cons (car form) (mapcar #'compiler-macroexpand-all (cdr form))))
               (t
                (multiple-value-bind (exp expanded-p) (our-macroexpand-1 form)
                  (if expanded-p
                      (compiler-macroexpand-all exp)
                      ;; FR-120: accessor read inlining — (accessor obj) → (slot-value obj 'slot)
                      (let ((mapping (when (and (symbolp (car form))
                                                (= (length (cdr form)) 1))
                                       (gethash (car form) *accessor-slot-map*))))
                        (if mapping
                            (compiler-macroexpand-all
                              (list 'slot-value (second form) (list 'quote (cdr mapping))))
                            (mapcar #'compiler-macroexpand-all form))))))))))))))

;;; ── FR-539: Deprecation API ──────────────────────────────────────────

(defvar *deprecation-registry* (make-hash-table :test #'eq)
  "Registry mapping function names to deprecation plists with keys
:SINCE, :REMOVED-IN, :REPLACEMENT, :MESSAGE.")

(defun deprecate (name &key since removed-in replacement message)
  "Register NAME as deprecated. When NAME is called, a diagnostic warning is emitted.
SINCE is the version when it was deprecated. REMOVED-IN is the planned removal version.
REPLACEMENT names the suggested alternative function. MESSAGE is an optional explanation."
  (setf (gethash name *deprecation-registry*)
        (list :since since :removed-in removed-in
              :replacement replacement :message message))
  name)

(defun deprecated-p (name)
  "Return non-NIL when NAME is a registered deprecated function."
  (values (gethash name *deprecation-registry*)))

(defun %warn-deprecated-use (name &optional context-stream)
  "Emit a deprecation warning for NAME using parse diagnostics when available."
  (declare (ignore context-stream))
  (let ((info (deprecated-p name)))
    (when info
      (let ((since (getf info :since))
            (replacement (getf info :replacement))
            (removed-in (getf info :removed-in)))
        ;; Use the diagnostics package if loaded
        (when (find-package :cl-cc/parse)
          (let ((output (list nil)))
            (push (format nil "function ~S is deprecated" name) output)
            (when since
              (push (format nil "  deprecated since: ~A" since) output))
            (when removed-in
              (push (format nil "  planned removal: ~A" removed-in) output))
            (when replacement
              (push (format nil "  consider using ~S instead" replacement) output))
            (values (nreverse output))))))))

;;; ── FR-540: Branch Probability Hints ─────────────────────────────────

(defmacro likely (form)
  "Annotate FORM as the likely path of a conditional. The code generator may
use this hint to place the likely branch as a fallthrough."
  `(progn ,form))

(defmacro unlikely (form)
  "Annotate FORM as the unlikely path of a conditional."
  `(progn ,form))
