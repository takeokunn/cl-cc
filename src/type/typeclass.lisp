;;;; typeclass.lisp - Typeclass Definition Registry
;;;;
;;;; Contains:
;;;;   typeclass-def struct — name, type-params, superclasses, methods, defaults,
;;;;                          associated-types, functional-deps
;;;;   *typeclass-registry* — global typeclass-def lookup table
;;;;   register-typeclass, lookup-typeclass
;;;;   Helper predicates: %typeclass-instance-overlaps-p, %typeclass-default-methods,
;;;;     %typeclass-name-string, %merge-default-methods, %typeclass-param-name,
;;;;     %typeclass-instance-args, %typeclass-instance-arg-at,
;;;;     %typeclass-fundep-pairs, %typeclass-fundep-violation-p
;;;;
;;;; Instance registry, dict-env, and backward-compat aliases are in typeclass-compat.lisp (loads after).
;;;;
;;;; Load order: after representation.lisp, before typeclass-compat.lisp.

(in-package :cl-cc/type)

;;; ─── typeclass-def defstruct ──────────────────────────────────────────────

(defstruct typeclass-def
  "A multi-parameter typeclass definition.
NAME:             the typeclass symbol (e.g., 'EQ, 'NUM, 'FUNCTOR).
TYPE-PARAMS:      list of type-var nodes (the class's type parameters).
SUPERCLASSES:     list of typeclass name symbols that this class inherits from.
METHODS:          alist of (method-name . type-arrow) — the typeclass interface.
DEFAULTS:         alist of (method-name . implementation) used when instances
                  omit a method.
ASSOCIATED-TYPES: alist of (name . kind-node) — associated type families.
FUNCTIONAL-DEPS:  list of (from-params . to-params) — functional dependencies."
  (name             nil :type symbol)
  (type-params      nil :type list)
  (superclasses     nil :type list)
  (methods          nil :type list)
  (defaults         nil :type list)
  (associated-types nil :type list)
  (functional-deps  nil :type list))

;;; ─── *typeclass-registry* ─────────────────────────────────────────────────

(defvar *typeclass-registry*
  (make-hash-table :test #'eq)
  "Maps typeclass name symbol -> typeclass-def.")

(defun register-typeclass (name tc-def)
  "Register TC-DEF under NAME in *typeclass-registry*.
TC-DEF may be a typeclass-def or the old type-class struct — both are accepted."
  (setf (gethash name *typeclass-registry*) tc-def)
  name)

(defun %typeclass-instance-overlaps-p (type-a type-b)
  "Return T when TYPE-A and TYPE-B can unify, meaning the instances overlap.

This is a conservative coherence check: if two instances for the same class
could both match the same concrete type, we reject the later registration."
  (and (not (type-equal-p type-a type-b))
       (multiple-value-bind (_subst ok)
           (type-unify type-a type-b)
         (declare (ignore _subst))
         ok)))

(defun %typeclass-default-methods (class-name)
  "Return the default method alist for CLASS-NAME, or NIL."
  (let ((tc-def (lookup-typeclass class-name)))
    (cond
      ((typeclass-def-p tc-def) (typeclass-def-defaults tc-def))
      ((type-class-p tc-def)    (type-class-defaults tc-def))
      (t nil))))

(defun %typeclass-name-string (name)
  "Normalize NAME for dependency comparison."
  (cond ((symbolp name) (symbol-name name))
        ((stringp name) (string-upcase name))
        (t (string-upcase (princ-to-string name)))))

(defun %merge-default-methods (class-name method-impls)
  "Merge class defaults into METHOD-IMPLS, preserving explicit implementations."
  (let ((defaults (%typeclass-default-methods class-name)))
    (append method-impls
            (remove-if (lambda (pair)
                         (assoc (car pair) method-impls :test #'eq))
                       defaults))))

(defun %typeclass-param-name (param)
  "Return a stable identifier for PARAM, which may be a type-var or symbol."
  (cond
    ((type-var-p param) (%typeclass-name-string (type-var-name param)))
    ((symbolp param)    (%typeclass-name-string param))
    (t                  (%typeclass-name-string param))))

(defun %typeclass-instance-args (type)
  "Return the ordered instance arguments encoded by TYPE.
Single-parameter instances use TYPE itself; multi-parameter instances use a type-product."
  (if (type-product-p type)
      (type-product-elems type)
      (list type)))

(defun %typeclass-instance-arg-at (type index)
  (nth index (%typeclass-instance-args type)))

(defun %typeclass-fundep-pairs (tc-def)
  "Normalize functional dependencies from TC-DEF into ((from...) . (to...)) pairs."
  (mapcar (lambda (dep)
            (cons (mapcar #'%typeclass-name-string
                          (if (listp (car dep)) (car dep) (list (car dep))))
                  (mapcar #'%typeclass-name-string
                          (if (listp (cdr dep)) (cdr dep) (list (cdr dep))))))
           (typeclass-def-functional-deps tc-def)))

(defun %typeclass-fundep-violation-p (tc-def existing-type new-type)
  "Return T when NEW-TYPE would violate TC-DEF functional dependencies
relative to EXISTING-TYPE. Uses positional matching over type params."
  (let* ((params (typeclass-def-type-params tc-def))
         (param-names (mapcar #'%typeclass-param-name params))
         (existing-args (%typeclass-instance-args existing-type))
         (new-args (%typeclass-instance-args new-type)))
    (dolist (dep (%typeclass-fundep-pairs tc-def) nil)
      (let* ((from-names (car dep))
             (to-names   (cdr dep))
             (from-indices (mapcar (lambda (name)
                                     (position name param-names :test #'equal))
                                   from-names))
             (to-indices   (mapcar (lambda (name)
                                     (position name param-names :test #'equal))
                                   to-names)))
        (when (and (every #'numberp from-indices)
                   (every #'numberp to-indices))
          (let ((existing-from (mapcar (lambda (i) (nth i existing-args)) from-indices))
                (new-from      (mapcar (lambda (i) (nth i new-args)) from-indices)))
            (when (every #'type-equal-p existing-from new-from)
              (let ((existing-to (mapcar (lambda (i) (nth i existing-args)) to-indices))
                    (new-to      (mapcar (lambda (i) (nth i new-args)) to-indices)))
                (unless (every #'type-equal-p existing-to new-to)
                  (return t))))))))))

(defun lookup-typeclass (name)
  "Return the typeclass-def (or type-class) for NAME, or nil if not registered."
  (gethash name *typeclass-registry*))

;;; (typeclass-instance, *typeclass-instance-registry*, dict-env, and backward-compat
;;;  aliases are in typeclass-compat.lisp which loads after this file.)
