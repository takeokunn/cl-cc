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
;;;; Instance registry and dict-env helpers live here.
;;;;
;;;; Load order: after representation.lisp.

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

(defun %normalize-typeclass-definition (name tc-def)
  "Guard: signal an error unless TC-DEF is a canonical TYPECLASS-DEF."
  (unless (typeclass-def-p tc-def)
    (error 'type-inference-error
           :message (format nil "register-typeclass expected TYPECLASS-DEF for ~A, got ~S"
                            name tc-def)))
  tc-def)

(defun register-typeclass (name tc-def)
  "Register TC-DEF under NAME in *typeclass-registry*.
The registry stores only normalized TYPECLASS-DEF values."
  (setf (gethash name *typeclass-registry*)
        (%normalize-typeclass-definition name tc-def))
  name)

(defun %typeclass-instance-overlaps-p (type-a type-b)
  "Return T when TYPE-A and TYPE-B can unify (conservative coherence check)."
  (and (not (type-equal-p type-a type-b))
       (nth-value 1 (type-unify type-a type-b))))

(defun %typeclass-default-methods (class-name)
  "Return the default method alist for CLASS-NAME, or NIL."
  (let ((tc-def (lookup-typeclass class-name)))
    (and (typeclass-def-p tc-def) (typeclass-def-defaults tc-def))))

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
  (%typeclass-name-string
   (if (type-var-p param) (type-var-name param) param)))

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
  "Return the canonical typeclass-def for NAME, or nil if not registered."
  (gethash name *typeclass-registry*))

;;; ─── typeclass-instance registry / dictionary env ─────────────────────────

(defstruct (typeclass-instance (:constructor %make-typeclass-instance))
  "A typeclass instance declaration."
  (class-name    nil :type symbol)
  (instance-type nil)
  (constraints   nil :type list)
  (methods       nil :type list))

(defvar *typeclass-instance-registry*
  (make-hash-table :test #'equal)
  "Maps (class-name . type-key-string) to typeclass-instance.")

(defparameter *default-numeric-type* type-int
  "Default concrete type for unresolved numeric constraints.")

(defparameter *default-numeric-class-names* '("NUM" "NUMERIC")
  "Typeclass names eligible for numeric defaulting.")

(defun %typeclass-type-string (type)
  "Return a stable registry string for TYPE without depending on printer load order."
  (cond
    ((fboundp 'type-to-string) (type-to-string type))
    ((type-primitive-p type) (symbol-name (type-primitive-name type)))
    ((type-var-p type) (symbol-name (type-var-name type)))
    ((type-error-p type) "<error>")
    ((null type) "NIL")
    ((symbolp type) (symbol-name type))
    (t (princ-to-string type))))

(defun %type-instance-key (class-name type)
  "Build the hash key for (CLASS-NAME, TYPE)."
  (cons class-name (%typeclass-type-string type)))

(defun %existing-class-instances (class-name)
  "Return all registered instances for CLASS-NAME."
  (loop for k being the hash-keys of *typeclass-instance-registry*
        using (hash-value inst)
        when (and (consp k) (eq (car k) class-name))
        collect inst))

(defun register-typeclass-instance (class-name type method-impls)
  "Register that TYPE implements CLASS-NAME with METHOD-IMPLS."
  (when (lookup-typeclass-instance class-name type)
    (error 'type-inference-error
           :message (format nil "Duplicate typeclass instance for ~A / ~A"
                            class-name (%typeclass-type-string type))))
  (let ((existing-instances (%existing-class-instances class-name))
        (tc-def (lookup-typeclass class-name)))
    (dolist (existing existing-instances)
      (let ((existing-type (typeclass-instance-instance-type existing)))
        (when (%typeclass-instance-overlaps-p existing-type type)
          (error 'type-inference-error
                 :message (format nil "Overlapping typeclass instances for ~A: ~A and ~A"
                                   class-name (%typeclass-type-string existing-type)
                                   (%typeclass-type-string type))))
        (when (and (typeclass-def-p tc-def)
                   (%typeclass-fundep-violation-p tc-def existing-type type))
          (error 'type-inference-error
                 :message (format nil "Functional dependency violation for ~A: ~A vs ~A"
                                   class-name (%typeclass-type-string existing-type)
                                   (%typeclass-type-string type)))))))
  (let ((inst (%make-typeclass-instance
               :class-name    class-name
               :instance-type type
               :constraints   nil
               :methods       (%merge-default-methods class-name method-impls))))
    (setf (gethash (%type-instance-key class-name type) *typeclass-instance-registry*) inst)
    inst))

(defun lookup-typeclass-instance (class-name type)
  "Return the typeclass-instance for (CLASS-NAME, TYPE), or nil."
  (gethash (%type-instance-key class-name type)
           *typeclass-instance-registry*))

(defun has-typeclass-instance-p (class-name type)
  "Return T if TYPE has a registered instance for CLASS-NAME."
  (or (lookup-typeclass-instance class-name type)
      (let ((tc-def (lookup-typeclass class-name)))
        (and (typeclass-def-p tc-def)
             (some (lambda (super) (has-typeclass-instance-p super type))
                   (typeclass-def-superclasses tc-def))))))

(defun default-numeric-typeclass-p (class-name)
  "Return T if CLASS-NAME should default unresolved numeric type variables."
  (and (symbolp class-name)
       (member (symbol-name class-name)
               *default-numeric-class-names*
               :test #'string-equal)))

(defun check-typeclass-constraint (class-name type env)
  "Check if TYPE satisfies CLASS-NAME."
  (declare (ignore env))
  (unless (or (type-error-p type)
              (type-var-p type)
              (has-typeclass-instance-p class-name type))
    (error 'type-inference-error
           :message (format nil "No instance of ~A for ~A"
                            class-name (%typeclass-type-string type)))))

(defun dict-env-extend (class-name type methods env)
  "Return a new type-env that extends ENV with a method dictionary for (CLASS-NAME, TYPE)."
  (let ((key (cons class-name (%typeclass-type-string type))))
    (make-type-env
     :bindings      (type-env-bindings env)
     :dict-bindings (acons key methods (type-env-dict-bindings env)))))

(defun dict-env-lookup (class-name type env)
  "Look up the method alist for (CLASS-NAME, TYPE) in ENV's dict-bindings."
  (let ((key (cons class-name (%typeclass-type-string type))))
    (let ((entry (assoc key (type-env-dict-bindings env) :test #'equal)))
      (when entry (cdr entry)))))

(eval-when (:load-toplevel :execute)
  (unless (lookup-typeclass 'num)
    (register-typeclass 'num (make-typeclass-def
                              :name 'num
                              :type-params nil
                              :methods nil)))
  (unless (lookup-typeclass-instance 'num type-int)
    (register-typeclass-instance 'num type-int nil)))
