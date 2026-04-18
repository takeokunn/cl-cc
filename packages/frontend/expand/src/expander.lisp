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

;;; Ensure cl-cc/type package exists at compile time so that qualified symbols
;;; like cl-cc/type:looks-like-type-specifier-p can be read before type/ loads.
;;; The full defpackage in packages/foundation/type/src/package.lisp will extend this stub later.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-cc/type)
    (defpackage :cl-cc/type
      (:use :cl)
      (:export #:looks-like-type-specifier-p
               #:parse-type-specifier
               #:+type-unknown+
               #:register-type-alias))))

;;; Helper functions moved to expander-helpers.lisp.

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

;; Control-flow special forms moved to expander-control.lisp.

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
          (string= (symbol-name (car form)) "BACKQUOTE"))
     (compiler-macroexpand-all (%expand-quasiquote (second form))))
    (t
     (let ((handler (or (gethash (car form) *expander-head-table*)
                        (and (symbolp (car form))
                             (let* ((local (find-symbol (symbol-name (car form)) :cl-cc/expand)))
                               (and local (gethash local *expander-head-table*)))))))
       (cond
         (handler
          (let ((expanded (funcall handler form)))
            (if (equal expanded form)
                form
                expanded)))
         ((and (symbolp (car form))
               (lookup-compiler-macro (car form)))
          (let ((expanded (funcall (lookup-compiler-macro (car form)) form nil)))
            (if (equal expanded form)
                form
                (compiler-macroexpand-all expanded))))
         ((member (car form) *compiler-special-forms*)
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
                       `(slot-value ,(second form) ',(cdr mapping)))
                      (mapcar #'compiler-macroexpand-all form)))))))))))
