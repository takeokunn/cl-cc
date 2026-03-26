(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expander — DEFSTRUCT Expansion
;;;
;;; Translates (defstruct name slots...) to (progn defclass constructor predicate).
;;; Supports :conc-name, :constructor with BOA lambda list, :include inheritance.
;;;
;;; All functions here are pure code generators: they return Lisp forms,
;;; never evaluate them.  Only expand-defstruct calls compiler-macroexpand-all
;;; (via the caller in expander.lisp) to recurse into the output.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── BOA (By-Order-of-Arguments) lambda list helpers ──────────────────────

(defun %defstruct-extract-boa-parts (boa-args)
  "Split a BOA lambda list into (normal-params . aux-bindings)."
  (let ((normal nil) (aux nil) (in-aux nil))
    (dolist (arg boa-args)
      (cond ((eq arg '&aux) (setf in-aux t))
            (in-aux (push (if (consp arg) arg (list arg nil)) aux))
            (t       (push arg normal))))
    (cons (nreverse normal) (nreverse aux))))

(defun %defstruct-boa-param-names (normal-params)
  "Extract bound parameter names from a BOA lambda list (excludes &aux)."
  (let ((names nil))
    (dolist (p normal-params)
      (cond ((member p '(&key &optional &rest &body &allow-other-keys)) nil)
            ((consp p)  (push (if (consp (first p)) (second (first p)) (first p)) names))
            ((symbolp p) (push p names))))
    (nreverse names)))

;;; ── Constructor generation ───────────────────────────────────────────────

(defun %defstruct-make-constructor (ctor-name class-name boa-args all-slots)
  "Generate a DEFUN form for a defstruct constructor.
With BOA-ARGS: uses positional parameters.  Without: uses keyword parameters."
  (if boa-args
      ;; BOA constructor: positional params + &aux bindings
      (let* ((parts        (%defstruct-extract-boa-parts boa-args))
             (normal-params (car parts))
             (aux-bindings  (cdr parts))
             (param-names   (%defstruct-boa-param-names normal-params))
             (aux-names     (mapcar #'first aux-bindings))
             (bound-names   (append param-names aux-names))
             (initargs      (loop for (sname default) in all-slots
                                  append (list (intern (symbol-name sname) "KEYWORD")
                                               (if (member sname bound-names :test #'string=)
                                                   sname
                                                   default))))
             (aux-lets      (mapcar (lambda (b) (list (first b) (second b))) aux-bindings)))
        (list 'defun ctor-name normal-params
              (list 'let* aux-lets
                    (list* 'make-instance (list 'quote class-name) initargs))))
      ;; Keyword constructor: all slots become &key params
      (let ((key-params (mapcar (lambda (s) (list (first s) (second s))) all-slots))
            (initargs   (mapcan (lambda (s)
                                  (list (intern (symbol-name (first s)) "KEYWORD")
                                        (first s)))
                                all-slots)))
        (list 'defun ctor-name (cons '&key key-params)
              (list* 'make-instance (list 'quote class-name) initargs)))))

;;; ── Main defstruct expander ──────────────────────────────────────────────

(defun expand-defstruct (form)
  "Expand (defstruct name-or-options slot...) to (progn defclass constructor predicate).

Supported options:
  (:conc-name prefix)   — accessor prefix; defaults to NAME-
  (:constructor name lambda-list?) — constructor name and optional BOA list
  (:include parent)     — inherit parent slots"
  (let* ((name-and-options (second form))
         (slots-raw        (cddr form))
         ;; Parse name and option list
         (name    (if (listp name-and-options) (first name-and-options) name-and-options))
         (options (when (listp name-and-options) (rest name-and-options)))
         ;; :conc-name option
         (conc-opt  (find :conc-name options :key (lambda (o) (when (listp o) (first o)))))
         (conc-name (if conc-opt
                        (second conc-opt)
                        (intern (concatenate 'string (symbol-name name) "-"))))
         ;; :constructor option
         (ctor-opt  (find :constructor options :key (lambda (o) (when (listp o) (first o)))))
         (ctor-name (if ctor-opt
                        (second ctor-opt)
                        (intern (concatenate 'string "MAKE-" (symbol-name name)))))
         (boa-args  (when (and ctor-opt (cddr ctor-opt)) (third ctor-opt)))
         ;; :include option
         (incl-opt    (find :include options :key (lambda (o) (when (listp o) (first o)))))
         (parent-name (when incl-opt (second incl-opt)))
         (parent-slots (when parent-name (gethash parent-name *defstruct-slot-registry*)))
         ;; Slot normalization: (slot-name default) or bare slot-name; skip docstrings
         (own-slots (mapcar (lambda (s)
                              (if (listp s) (list (first s) (second s)) (list s nil)))
                            (remove-if #'stringp slots-raw)))
         (all-slots (append (or parent-slots nil) own-slots))
         ;; Predicate name
         (pred-name (intern (concatenate 'string (symbol-name name) "-P"))))
    ;; Register slot info for :include inheritance
    (setf (gethash name *defstruct-slot-registry*) all-slots)
    ;; Accessor name helper
    (flet ((accessor-name (slot-name)
             (if conc-name
                 (intern (concatenate 'string (symbol-name conc-name) (symbol-name slot-name)))
                 slot-name)))
      ;; Register accessors for setf expansion
      (dolist (slot all-slots)
        (setf (gethash (accessor-name (first slot)) *accessor-slot-map*)
              (cons name (first slot))))
      ;; Build DEFCLASS slot specs
      (let* ((defclass-slots
               (mapcar (lambda (slot)
                         (list (first slot)
                               :initarg  (intern (symbol-name (first slot)) "KEYWORD")
                               :initform (second slot)
                               :accessor (accessor-name (first slot))))
                       all-slots))
             (superclasses  (when parent-name (list parent-name)))
             (defclass-form `(defclass ,name ,superclasses ,defclass-slots))
             (ctor-form     (%defstruct-make-constructor ctor-name name boa-args all-slots))
             (pred-form     `(defun ,pred-name (obj) (typep obj ',name))))
        `(progn ,defclass-form ,ctor-form ,pred-form (quote ,name))))))
