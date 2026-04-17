;;;; packages/type/type/src/typeclass-compat.lisp - Typeclass Instance Registry, dict-env, and Backward-Compat Aliases
;;;;
;;;; Contains:
;;;;   typeclass-instance defstruct + *typeclass-instance-registry*
;;;;   register-typeclass-instance, lookup-typeclass-instance, has-typeclass-instance-p
;;;;   default-numeric-typeclass-p, check-typeclass-constraint
;;;;   dict-env-extend, dict-env-lookup
;;;;   Backward-compat: type-class, type-class-constraint, type-qualified-type,
;;;;                    type-effect, type-effectful-function, type-forall-type,
;;;;                    type-skolem, type-to-string extension
;;;;
;;;; typeclass-def struct and *typeclass-registry* are in typeclass.lisp (loads before).
;;;;
;;;; Load order: after typeclass.lisp.

(in-package :cl-cc/type)

;;; ─── typeclass-instance defstruct ─────────────────────────────────────────

(defstruct (typeclass-instance (:constructor %make-typeclass-instance))
  "A typeclass instance declaration.
CLASS-NAME:   the typeclass symbol.
INSTANCE-TYPE: the type-node this instance is for.
CONSTRAINTS:  list of type-constraint (the instance's context / supers).
METHODS:      alist of (method-name . function) — the method implementations."
  (class-name    nil :type symbol)
  (instance-type nil)
  (constraints   nil :type list)
  (methods       nil :type list))

;;; ─── *typeclass-instance-registry* ───────────────────────────────────────

(defvar *typeclass-instance-registry*
  (make-hash-table :test #'equal)
  "Maps (class-name . type-key-string) to typeclass-instance.
The key uses EQUAL comparison so different type-nodes with the same string
representation each get their own bucket.")

(defparameter *default-numeric-type* type-int
  "Default concrete type for unresolved numeric constraints.")

(defparameter *default-numeric-class-names* '("NUM" "NUMERIC")
  "Typeclass names eligible for numeric defaulting.")

(defun %type-instance-key (class-name type)
  "Build the hash key for (CLASS-NAME, TYPE)."
  (cons class-name (type-to-string type)))

(defun register-typeclass-instance (class-name type method-impls)
  "Register that TYPE implements CLASS-NAME with METHOD-IMPLS.
METHOD-IMPLS is an alist of (method-name . implementation)."
  (when (lookup-typeclass-instance class-name type)
    (error 'type-inference-error
           :message (format nil "Duplicate typeclass instance for ~A / ~A"
                            class-name (type-to-string type))))
  (dolist (existing (loop for k being the hash-keys of *typeclass-instance-registry*
                          using (hash-value inst)
                          when (and (consp k) (eq (car k) class-name))
                          collect inst))
    (when (%typeclass-instance-overlaps-p (typeclass-instance-instance-type existing) type)
      (error 'type-inference-error
             :message (format nil
                              "Overlapping typeclass instances for ~A: ~A and ~A"
                              class-name
                              (type-to-string (typeclass-instance-instance-type existing))
                              (type-to-string type)))))
  (let ((tc-def (lookup-typeclass class-name)))
    (when (typeclass-def-p tc-def)
      (dolist (existing (loop for k being the hash-keys of *typeclass-instance-registry*
                              using (hash-value inst)
                              when (and (consp k) (eq (car k) class-name))
                              collect inst))
        (when (%typeclass-fundep-violation-p tc-def
                                             (typeclass-instance-instance-type existing)
                                             type)
          (error 'type-inference-error
                 :message (format nil
                                  "Functional dependency violation for ~A: ~A vs ~A"
                                  class-name
                                  (type-to-string (typeclass-instance-instance-type existing))
                                  (type-to-string type)))))))
  (let ((inst (%make-typeclass-instance
                :class-name    class-name
                :instance-type type
                :constraints   nil
                :methods       (%merge-default-methods class-name method-impls))))
    (setf (gethash (%type-instance-key class-name type)
                   *typeclass-instance-registry*)
          inst)
    inst))

(defun lookup-typeclass-instance (class-name type)
  "Return the typeclass-instance for (CLASS-NAME, TYPE), or nil."
  (gethash (%type-instance-key class-name type)
           *typeclass-instance-registry*))

(defun has-typeclass-instance-p (class-name type)
  "Return T if TYPE has a registered instance for CLASS-NAME,
including instances inherited via superclass relationships."
  (or (not (null (lookup-typeclass-instance class-name type)))
      ;; Check superclasses via the typeclass-def or type-class if available
      (let ((tc-def (lookup-typeclass class-name)))
        (when tc-def
          (let ((supers (cond
                          ((typeclass-def-p tc-def)
                           (typeclass-def-superclasses tc-def))
                          ((type-class-p tc-def)
                           (type-class-superclasses tc-def))
                          (t nil))))
             (some (lambda (super)
                     (has-typeclass-instance-p super type))
                   supers))))))

(defun default-numeric-typeclass-p (class-name)
  "Return T if CLASS-NAME should default unresolved numeric type variables."
  (and (symbolp class-name)
       (member (symbol-name class-name)
               *default-numeric-class-names*
               :test #'string-equal)))

(defun check-typeclass-constraint (class-name type env)
  "Check if TYPE satisfies CLASS-NAME.
ENV is accepted for interface compatibility.
Returns nil on success, signals type-inference-error if unsatisfied.
Gradual typing: unknown and free type-vars are always accepted."
  (declare (ignore env))
  (unless (or (type-error-p type)
              (type-var-p type)
              (has-typeclass-instance-p class-name type))
    (error 'type-inference-error
           :message (format nil "No instance of ~A for ~A"
                             class-name (type-to-string type)))))

;;; ─── dict-env operations ──────────────────────────────────────────────────
;;;
;;; A dict-env is an alist of ((class-name . type-key-string) . methods)
;;; stored inside the type-env's dict-bindings slot.

(defun dict-env-extend (class-name type methods env)
  "Return a new type-env that extends ENV with a method dictionary for
(CLASS-NAME, TYPE).  METHODS is an alist of (method-name . implementation)."
  (let ((key (cons class-name (type-to-string type))))
    (make-type-env
     :bindings      (type-env-bindings env)
     :dict-bindings (acons key methods (type-env-dict-bindings env)))))

(defun dict-env-lookup (class-name type env)
  "Look up the method alist for (CLASS-NAME, TYPE) in ENV's dict-bindings.
Returns the methods alist or nil if missing."
  (let ((key (cons class-name (type-to-string type))))
    (let ((entry (assoc key (type-env-dict-bindings env) :test #'equal)))
      (when entry (cdr entry)))))

;;; ─── Built-in default numeric instance ───────────────────────────────────

(eval-when (:load-toplevel :execute)
  (unless (lookup-typeclass 'num)
    (register-typeclass 'num (make-typeclass-def
                              :name 'num
                              :type-params nil
                              :methods nil)))
  (unless (lookup-typeclass-instance 'num type-int)
    (register-typeclass-instance 'num type-int nil)))

;; Backward-compat type structs (type-class, type-class-constraint, type-effect,
;; type-effectful-function, type-forall-type, type-skolem, extended type-to-string)
;; are in typeclass-compat-legacy.lisp (loaded next).
