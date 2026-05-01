(in-package :cl-cc/expand)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expander — DEFSTRUCT Expansion
;;;
;;; Translates (defstruct name slots...) to (progn defclass constructor predicate).
;;; Supports :conc-name, :constructor with BOA lambda list, :include inheritance.
;;;
;;; Pure model/naming helpers now live in expander-helpers.lisp.
;;; This file keeps the emitted DEFSTRUCT forms and the top-level dispatcher.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; FR-555: copy-structure — VM structs are hash-tables with :__class__.
;;; Typed defstructs (:type list/vector) are represented as sequences, so this
;;; must preserve shallow-copy semantics across all registered defstruct
;;; representations using expansion-time metadata.
(register-macro 'copy-structure
  (lambda (form env)
    (declare (ignore env))
    (let ((struct (second form))
          (s (gensym "STRUCT")))
      (list 'let (list (list s struct))
            (cons 'cond
                  (append
                   (loop for name being the hash-keys of *defstruct-slot-registry*
                         using (hash-value slots)
                         for struct-type = (gethash name *defstruct-type-registry*)
                         collect
                         (if struct-type
                             (if (eq struct-type 'list)
                                 (list (list 'and (list 'listp s)
                                             (list 'consp s)
                                             (list 'eq (list 'car s) (list 'quote name)))
                                       (cons 'list
                                             (cons (list 'quote name)
                                                   (loop for _slot in slots
                                                         for idx from 1
                                                         collect (list 'nth idx s)))))
                                 (list (list 'and (list 'vectorp s)
                                             (list '> (list 'length s) 0)
                                             (list 'eq (list 'aref s 0) (list 'quote name)))
                                       (cons 'vector
                                             (cons (list 'quote name)
                                                   (loop for _slot in slots
                                                         for idx from 1
                                                         collect (list 'aref s idx))))))
                             (list (list 'typep s (list 'quote name))
                                   (cons 'let
                                         (cons (list (list 'copy (list 'make-instance (list 'quote name))))
                                               (append
                                                (loop for slot in slots
                                                      for slot-name = (first slot)
                                                      collect (list 'setf
                                                                    (list 'slot-value 'copy (list 'quote slot-name))
                                                                    (list 'slot-value s (list 'quote slot-name))))
                                                (list 'copy)))))))
                   (list (list 't (list 'error "copy-structure: unsupported object ~S" s)))))))))

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

(defun %defstruct-build-constructor (ctor-name boa-args all-slots body-fn)
  "Build a DEFUN constructor form. BODY-FN receives the effective slot-value forms."
  (if boa-args
      (multiple-value-bind (normal-params _aux _pnames bound-names aux-lets)
          (%defstruct-boa-bindings boa-args)
        (declare (ignore _aux _pnames))
        (list 'defun ctor-name normal-params
              (list 'let* aux-lets
                    (funcall body-fn (%defstruct-resolve-slot-values all-slots bound-names)))))
      (list 'defun ctor-name
            (cons '&key (mapcar (lambda (s) (list (first s) (second s))) all-slots))
            (funcall body-fn (mapcar #'first all-slots)))))

(defun %defstruct-make-constructor (ctor-name class-name boa-args all-slots)
  "Generate a DEFUN form for a defstruct constructor using CLOS make-instance."
  (%defstruct-build-constructor
   ctor-name boa-args all-slots
   (lambda (slot-values)
     (list* 'make-instance (list 'quote class-name)
            (mapcan (lambda (slot sv) (list (%defstruct-make-keyword (first slot)) sv))
                    all-slots slot-values)))))

;;; ── List/Vector-based struct expansion (FR-546) ──────────────────────────

(defun %defstruct-typed-container-form (struct-type struct-name slot-values)
  "Build the concrete list/vector container form for a typed defstruct."
  (if (eq struct-type 'list)
      (cons 'list (cons (list 'quote struct-name) slot-values))
      (cons 'vector (cons (list 'quote struct-name) slot-values))))

(defun %defstruct-typed-constructor (ctor-name struct-name struct-type boa-args all-slots)
  "Generate a constructor for :type list or :type vector defstruct."
  (%defstruct-build-constructor
   ctor-name boa-args all-slots
   (lambda (slot-values)
     (%defstruct-typed-container-form struct-type struct-name slot-values))))

(defun %defstruct-typed-accessors (struct-type conc-name all-slots)
  "Generate accessor functions for :type list or :type vector defstruct."
  (loop for slot in all-slots
        for slot-name = (first slot)
        for idx from 1
        for acc-name = (%defstruct-accessor-name conc-name slot-name)
        collect (if (eq struct-type 'list)
                    (list 'defun acc-name '(obj) (list 'nth idx 'obj))
                    (list 'defun acc-name '(obj) (list 'aref 'obj idx)))))

(defun %defstruct-typed-predicate (pred-name struct-name struct-type slot-count)
  "Generate a predicate for :type list or :type vector defstruct."
  (when pred-name
    (let ((total-len (1+ slot-count)))
      (if (eq struct-type 'list)
          (list 'defun pred-name '(obj)
                (list 'and '(listp obj)
                      (list 'eq '(car obj) (list 'quote struct-name))
                      (list '= '(length obj) total-len)))
          (list 'defun pred-name '(obj)
                (list 'and '(vectorp obj)
                      (list '> '(length obj) 0)
                      (list 'eq '(aref obj 0) (list 'quote struct-name))
                      (list '= '(length obj) total-len)))))))

(defun %defstruct-typed-expansion (model)
  "Emit the full expansion for a :type list/vector defstruct MODEL."
  (let* ((name (getf model :name))
         (struct-type (getf model :struct-type))
         (ctor-name (getf model :ctor-name))
         (boa-args (getf model :boa-args))
         (all-slots (getf model :all-slots))
         (conc-name (getf model :conc-name))
         (pred-name (getf model :pred-name))
         (ctor-form (when ctor-name
                      (%defstruct-typed-constructor ctor-name name struct-type boa-args all-slots)))
         (accessor-forms (%defstruct-typed-accessors struct-type conc-name all-slots))
         (pred-form (when pred-name
                      (%defstruct-typed-predicate pred-name name struct-type (length all-slots)))))
    (cons 'progn
          (append (when ctor-form (list ctor-form))
                  accessor-forms
                  (when pred-form (list pred-form))
                  (list (list 'quote name))))))

;;; ── Standard CLOS-based defstruct expansion ───────────────────────────────

(defun %defstruct-register-accessors (name conc-name own-slots)
  "Register writable accessors for SETF expansion metadata."
  (dolist (slot own-slots)
    (unless (third slot)
      (setf (gethash (%defstruct-accessor-name conc-name (first slot))
                     *accessor-slot-map*)
            (cons name (first slot))))))

(defun %defstruct-defclass-slots (conc-name own-slots)
  "Build DEFCLASS slot specs for own defstruct slots."
  (mapcar (lambda (slot)
            (let ((acc-key (if (third slot) :reader :accessor)))
              (list (first slot)
                    :initarg (%defstruct-make-keyword (first slot))
                    :initform (second slot)
                    acc-key (%defstruct-accessor-name conc-name (first slot)))))
          own-slots))

(defun %defstruct-print-form (name print-fn print-fn-opt)
  "Emit an optional PRINT-OBJECT method for NAME."
  (when print-fn
    (let ((obj (gensym "OBJ"))
          (stream (gensym "STR")))
      (if print-fn-opt
          (list 'defmethod 'print-object (list (list obj name) stream)
                (list 'funcall (list 'function print-fn) obj stream 0))
          (list 'defmethod 'print-object (list (list obj name) stream)
                (list 'funcall (list 'function print-fn) obj stream))))))

(defun %defstruct-derive-form (name derived-classes)
  "Emit registration forms for :deriving classes."
  (when derived-classes
    (cons 'eval-when
          (cons '(:load-toplevel :execute)
                (mapcar (lambda (class)
                          (list 'cl-cc/type::register-typeclass-instance
                                 (list 'quote class) (list 'quote name) nil))
                        derived-classes)))))

(defun %defstruct-clos-expansion (model)
  "Emit the standard CLOS-based defstruct expansion for MODEL."
  (let* ((name (getf model :name))
         (conc-name (getf model :conc-name))
         (ctor-name (getf model :ctor-name))
         (boa-args (getf model :boa-args))
         (parent-name (getf model :parent-name))
         (own-slots (getf model :own-slots))
         (all-slots (getf model :all-slots))
         (pred-name (getf model :pred-name))
         (print-fn (getf model :print-fn))
         (print-fn-opt (getf model :print-fn-opt))
         (derived-classes (getf model :derived-classes)))
    (%defstruct-register-accessors name conc-name own-slots)
    (let* ((defclass-form (list 'defclass name
                                (if parent-name (list parent-name) '())
                                (%defstruct-defclass-slots conc-name own-slots)))
           (ctor-form (when ctor-name
                          (%defstruct-make-constructor ctor-name name boa-args all-slots)))
           (pred-form (when pred-name
                        (list 'defun pred-name '(obj) (list 'typep 'obj (list 'quote name)))))
            (print-form (%defstruct-print-form name print-fn print-fn-opt))
            (derive-form (%defstruct-derive-form name derived-classes)))
      (cons 'progn
            (append (list defclass-form)
                    (when ctor-form (list ctor-form))
                    (when pred-form (list pred-form))
                    (when print-form (list print-form))
                    (when derive-form (list derive-form))
                    (list (list 'quote name)))))))

;;; ── Main defstruct expander ──────────────────────────────────────────────

(defun expand-defstruct (form)
  "Expand (defstruct name-or-options slot...) to (progn defclass constructor predicate).

Supported options:
  (:conc-name prefix)   — accessor prefix; defaults to NAME-
  (:constructor name lambda-list?) — constructor name and optional BOA list
  (:include parent)     — inherit parent slots
  (:type list/vector)   — use list/vector representation instead of CLOS"
  (let* ((model (%defstruct-build-model form))
         (name (getf model :name))
         (all-slots (getf model :all-slots))
         (struct-type (getf model :struct-type)))
    ;; Register slot info for :include inheritance.
    (setf (gethash name *defstruct-slot-registry*) all-slots)
    (setf (gethash name *defstruct-type-registry*) struct-type)
    ;; Dispatch: :type list/vector → non-CLOS expansion; otherwise → CLOS expansion.
    (if struct-type
        (%defstruct-typed-expansion model)
        (%defstruct-clos-expansion model))))
