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
  (let ((tail boa-args)
        (arg nil)
        (normal nil)
        (aux nil)
        (in-aux nil))
    (tagbody
     scan
       (if (null tail) (go done))
       (setq arg (car tail))
       (if (eq arg '&aux)
           (setq in-aux t)
           (if in-aux
               (if (consp arg)
                   (setq aux (cons arg aux))
                   (setq aux (cons (%expander-form arg nil) aux)))
               (setq normal (cons arg normal))))
       (setq tail (cdr tail))
       (go scan)
     done)
    (cons (nreverse normal) (nreverse aux))))

(defun %defstruct-boa-param-names (normal-params)
  "Extract bound parameter names from a BOA lambda list (excludes &aux)."
  (let ((tail normal-params)
        (p nil)
        (names nil)
        (skip nil))
    (tagbody
     scan
       (if (null tail) (go done))
       (setq p (car tail))
       (setq skip nil)
       (if (eq p '&key) (setq skip t))
       (if (eq p '&optional) (setq skip t))
       (if (eq p '&rest) (setq skip t))
       (if (eq p '&body) (setq skip t))
       (if (eq p '&allow-other-keys) (setq skip t))
       (if skip
           nil
           (if (consp p)
               (if (consp (first p))
                   (setq names (cons (second (first p)) names))
                   (setq names (cons (first p) names)))
               (if (symbolp p)
                   (setq names (cons p names)))))
       (setq tail (cdr tail))
       (go scan)
     done)
    (nreverse names)))

;;; ── Constructor generation ───────────────────────────────────────────────

(defun %defstruct-build-constructor (ctor-name boa-args all-slots body-fn)
  "Build a DEFUN constructor form. BODY-FN receives the effective slot-value forms."
  (if boa-args
      (let ((boa-data nil)
            (normal-params nil)
            (bound-names nil)
            (aux-lets nil))
        (setq boa-data (%defstruct-boa-bindings boa-args))
        (setq normal-params (first boa-data))
        (setq bound-names (car (cdddr boa-data)))
        (setq aux-lets (car (cddddr boa-data)))
        (%expander-form 'defun ctor-name normal-params
                        (%expander-form
                         'let* aux-lets
                         (funcall body-fn
                                  (%defstruct-resolve-slot-values all-slots bound-names)))))
      (let ((tail all-slots)
            (slot nil)
            (key-bindings nil)
            (slot-values nil))
        (tagbody
         scan
           (if (null tail) (go done))
           (setq slot (car tail))
           (setq key-bindings
                 (cons (%expander-form (first slot) (second slot)) key-bindings))
           (setq slot-values (cons (first slot) slot-values))
           (setq tail (cdr tail))
           (go scan)
         done)
        (setq key-bindings (nreverse key-bindings))
        (setq slot-values (nreverse slot-values))
        (%expander-form 'defun ctor-name
                        (cons '&key key-bindings)
                        (funcall body-fn slot-values)))))

(defun %defstruct-make-constructor (ctor-name class-name boa-args all-slots)
  "Generate a DEFUN form for a defstruct constructor using CLOS make-instance."
  (%defstruct-build-constructor
   ctor-name boa-args all-slots
   (lambda (slot-values)
     (let ((slot-tail all-slots)
           (value-tail slot-values)
           (slot nil)
           (args nil))
       (tagbody
        scan
          (if (null slot-tail) (go done))
          (setq slot (car slot-tail))
          (setq args (cons (%defstruct-make-keyword (first slot)) args))
          (setq args (cons (car value-tail) args))
          (setq slot-tail (cdr slot-tail))
          (setq value-tail (cdr value-tail))
          (go scan)
        done)
       (cons 'make-instance
             (cons (%expander-form 'quote class-name)
                   (nreverse args)))))))

;;; ── List/Vector-based struct expansion (FR-546) ──────────────────────────

(defun %defstruct-typed-container-form (struct-type struct-name slot-values)
  "Build the concrete list/vector container form for a typed defstruct."
  (if (eq struct-type 'list)
      (cons 'list (cons (%expander-form 'quote struct-name) slot-values))
      (cons 'vector (cons (%expander-form 'quote struct-name) slot-values))))

(defun %defstruct-typed-constructor (ctor-name struct-name struct-type boa-args all-slots)
  "Generate a constructor for :type list or :type vector defstruct."
  (%defstruct-build-constructor
   ctor-name boa-args all-slots
   (lambda (slot-values)
     (%defstruct-typed-container-form struct-type struct-name slot-values))))

(defun %defstruct-typed-accessors (struct-type conc-name all-slots)
  "Generate accessor functions for :type list or :type vector defstruct."
  (let ((tail all-slots)
        (slot nil)
        (slot-name nil)
        (idx 1)
        (acc-name nil)
        (forms nil))
    (tagbody
     scan
       (if (null tail) (go done))
       (setq slot (car tail))
       (setq slot-name (first slot))
       (setq acc-name (%defstruct-accessor-name conc-name slot-name))
       (if (eq struct-type 'list)
           (setq forms
                 (cons (%expander-form 'defun acc-name
                                       (%expander-form 'obj)
                                       (%expander-form 'nth idx 'obj))
                       forms))
           (setq forms
                 (cons (%expander-form 'defun acc-name
                                       (%expander-form 'obj)
                                       (%expander-form 'aref 'obj idx))
                       forms)))
       (setq idx (+ idx 1))
       (setq tail (cdr tail))
       (go scan)
     done)
    (nreverse forms)))

(defun %defstruct-typed-predicate (pred-name struct-name struct-type slot-count)
  "Generate a predicate for :type list or :type vector defstruct."
  (let ((total-len nil)
        (quoted-name nil)
        (list-pred nil)
        (vector-pred nil)
        (pred-body nil))
    (if pred-name
        (progn
          (setq total-len (+ slot-count 1))
          (setq quoted-name (%expander-form 'quote struct-name))
          (setq list-pred
                (%expander-form 'and
                                (%expander-form 'listp 'obj)
                                (%expander-form 'eq (%expander-form 'car 'obj) quoted-name)
                                (%expander-form '= (%expander-form 'length 'obj) total-len)))
          (setq vector-pred
                (%expander-form 'and
                                (%expander-form 'vectorp 'obj)
                                (%expander-form '> (%expander-form 'length 'obj) 0)
                                (%expander-form 'eq (%expander-form 'aref 'obj 0) quoted-name)
                                (%expander-form '= (%expander-form 'length 'obj) total-len)))
          (if (eq struct-type 'list)
              (setq pred-body list-pred)
              (setq pred-body vector-pred))
          (%expander-form 'defun pred-name
                          (%expander-form 'obj)
                          pred-body))
        nil)))

(defun %defstruct-typed-expansion-parts (model)
  "Build ctor/accessor/predicate parts for typed defstruct expansion."
  (let ((tail model)
        (key nil)
        (value nil)
        (name nil)
        (struct-type nil)
        (ctor-name nil)
        (boa-args nil)
        (all-slots nil)
        (conc-name nil)
        (pred-name nil)
        (ctor-form nil)
        (accessor-forms nil)
        (pred-form nil))
    (tagbody
     scan
       (if (null tail) (go done))
       (setq key (car tail))
       (setq value (cadr tail))
       (if (eq key :name) (setq name value))
       (if (eq key :struct-type) (setq struct-type value))
       (if (eq key :ctor-name) (setq ctor-name value))
       (if (eq key :boa-args) (setq boa-args value))
       (if (eq key :all-slots) (setq all-slots value))
       (if (eq key :conc-name) (setq conc-name value))
       (if (eq key :pred-name) (setq pred-name value))
       (setq tail (cddr tail))
       (go scan)
     done)
    (if ctor-name
        (setq ctor-form
              (%defstruct-typed-constructor ctor-name name struct-type boa-args all-slots))
        (setq ctor-form nil))
    (setq accessor-forms (%defstruct-typed-accessors struct-type conc-name all-slots))
    (if pred-name
        (setq pred-form
              (%defstruct-typed-predicate pred-name name struct-type (length all-slots)))
        (setq pred-form nil))
    (%expander-form name ctor-form accessor-forms pred-form)))

(defun %defstruct-typed-expansion-forms (name ctor-form accessor-forms pred-form)
  "Build the ordered PROGN tail forms for typed expansion."
  (let ((tail nil)
        (forms nil))
    (if ctor-form
        (setq forms (cons ctor-form forms))
        (setq forms forms))
    (setq tail accessor-forms)
    (tagbody
     scan
       (if (null tail) (go done))
       (setq forms (cons (car tail) forms))
       (setq tail (cdr tail))
       (go scan)
     done)
    (if pred-form
        (setq forms (cons pred-form forms))
        (setq forms forms))
    (setq forms (cons (%expander-form 'quote name) forms))
    (nreverse forms)))

(defun %defstruct-typed-expansion (model)
  "Emit the full expansion for a :type list/vector defstruct MODEL."
  (let ((parts (%defstruct-typed-expansion-parts model)))
    (cons 'progn
          (%defstruct-typed-expansion-forms (first parts)
                                            (second parts)
                                            (third parts)
                                            (fourth parts)))))

;;; ── Standard CLOS-based defstruct expansion ───────────────────────────────

(defun %defstruct-register-accessors (name conc-name own-slots)
  "Register writable accessors for SETF expansion metadata."
  (let ((tail own-slots)
        (slot nil)
        (slot-name nil)
        (read-only-p nil)
        (acc-name nil))
    (tagbody
     scan
       (if (null tail) (go done))
       (setq slot (car tail))
       (setq slot-name (first slot))
       (setq read-only-p (third slot))
       (if read-only-p
           (setq acc-name nil)
           (progn
             (setq acc-name (%defstruct-accessor-name conc-name slot-name))
             (setf (gethash acc-name *accessor-slot-map*)
                   (cons name slot-name))))
       (setq tail (cdr tail))
       (go scan)
     done)))

(defun %defstruct-defclass-slots (conc-name own-slots)
  "Build DEFCLASS slot specs for own defstruct slots."
  (let ((tail own-slots)
        (slot nil)
        (slot-name nil)
        (slot-init nil)
        (acc-key nil)
        (spec nil)
        (result nil))
    (tagbody
     scan
       (if (null tail) (go done))
       (setq slot (car tail))
       (setq slot-name (first slot))
       (setq slot-init (second slot))
       (if (third slot)
           (setq acc-key :reader)
           (setq acc-key :accessor))
       (setq spec (%expander-form slot-name
                                  :initarg (%defstruct-make-keyword slot-name)
                                  :initform slot-init
                                  acc-key (%defstruct-accessor-name conc-name slot-name)))
       (setq result (cons spec result))
       (setq tail (cdr tail))
       (go scan)
     done)
    (nreverse result)))

(defun %defstruct-print-form (name print-fn print-fn-opt)
  "Emit an optional PRINT-OBJECT method for NAME."
  (let ((obj nil)
        (stream nil)
        (specializer nil)
        (args nil)
        (body nil))
    (if print-fn
        (progn
          (setq obj (gensym "OBJ"))
          (setq stream (gensym "STR"))
          (setq specializer (%expander-form obj name))
          (setq args (%expander-form specializer stream))
          (if print-fn-opt
              (setq body
                    (%expander-form 'funcall
                                    (%expander-form 'function print-fn)
                                    obj stream 0))
              (setq body
                    (%expander-form 'funcall
                                    (%expander-form 'function print-fn)
                                    obj stream)))
          (%expander-form 'defmethod 'print-object args body))
        nil)))

(defun %defstruct-derive-form (name derived-classes)
  "Emit registration forms for :deriving classes."
  (let ((tail derived-classes)
        (class nil)
        (forms nil)
        (entry nil))
    (if derived-classes
        (progn
          (tagbody
           scan
             (if (null tail) (go done))
             (setq class (car tail))
             (setq entry
                   (%expander-form 'cl-cc/type:register-typeclass-instance
                                   (%expander-form 'quote class)
                                   (%expander-form 'quote name)
                                   nil))
             (setq forms (cons entry forms))
             (setq tail (cdr tail))
             (go scan)
           done)
          (setq forms (nreverse forms))
          (cons 'eval-when
                (cons '(:load-toplevel :execute) forms)))
        nil)))

(defun %defstruct-clos-expansion (model)
  "Emit the standard CLOS-based defstruct expansion for MODEL."
  (let ((tail model)
        (key nil)
        (value nil)
        (name nil)
        (conc-name nil)
        (ctor-name nil)
        (boa-args nil)
        (parent-name nil)
        (own-slots nil)
        (all-slots nil)
        (pred-name nil)
        (print-fn nil)
        (print-fn-opt nil)
        (derived-classes nil)
        (defclass-parents nil)
        (defclass-form nil)
        (ctor-form nil)
        (pred-form nil)
        (print-form nil)
        (derive-form nil)
        (forms nil))
    (tagbody
     scan
       (if (null tail) (go done))
       (setq key (car tail))
       (setq value (cadr tail))
       (if (eq key :name) (setq name value))
       (if (eq key :conc-name) (setq conc-name value))
       (if (eq key :ctor-name) (setq ctor-name value))
       (if (eq key :boa-args) (setq boa-args value))
       (if (eq key :parent-name) (setq parent-name value))
       (if (eq key :own-slots) (setq own-slots value))
       (if (eq key :all-slots) (setq all-slots value))
       (if (eq key :pred-name) (setq pred-name value))
       (if (eq key :print-fn) (setq print-fn value))
       (if (eq key :print-fn-opt) (setq print-fn-opt value))
       (if (eq key :derived-classes) (setq derived-classes value))
       (setq tail (cddr tail))
       (go scan)
     done)
    (%defstruct-register-accessors name conc-name own-slots)
    (if parent-name
        (setq defclass-parents (%expander-form parent-name))
        (setq defclass-parents '()))
    (setq defclass-form
          (%expander-form 'defclass
                          name
                          defclass-parents
                          (%defstruct-defclass-slots conc-name own-slots)))
    (if ctor-name
        (setq ctor-form
              (%defstruct-make-constructor ctor-name name boa-args all-slots))
        (setq ctor-form nil))
    (if pred-name
        (setq pred-form
              (%expander-form 'defun
                              pred-name
                              (%expander-form 'obj)
                              (%expander-form 'typep
                                              'obj
                                              (%expander-form 'quote name))))
        (setq pred-form nil))
    (setq print-form (%defstruct-print-form name print-fn print-fn-opt))
    (setq derive-form (%defstruct-derive-form name derived-classes))
    (setq forms (cons defclass-form forms))
    (if ctor-form
        (setq forms (cons ctor-form forms))
        (setq forms forms))
    (if pred-form
        (setq forms (cons pred-form forms))
        (setq forms forms))
    (if print-form
        (setq forms (cons print-form forms))
        (setq forms forms))
    (if derive-form
        (setq forms (cons derive-form forms))
        (setq forms forms))
    (setq forms (cons (%expander-form 'quote name) forms))
    (setq forms (nreverse forms))
    (cons 'progn forms)))

;;; ── Main defstruct expander ──────────────────────────────────────────────

(defun expand-defstruct (form)
  "Expand (defstruct name-or-options slot...) to (progn defclass constructor predicate).

Supported options:
  (:conc-name prefix)   — accessor prefix; defaults to NAME-
  (:constructor name lambda-list?) — constructor name and optional BOA list
  (:include parent)     — inherit parent slots
  (:type list/vector)   — use list/vector representation instead of CLOS"
  (let ((model (%defstruct-build-model form))
        (tail nil)
        (key nil)
        (value nil)
        (name nil)
        (all-slots nil)
        (struct-type nil))
    (setq tail model)
    (tagbody
     scan
       (if (null tail) (go done))
       (setq key (car tail))
       (setq value (cadr tail))
       (if (eq key :name) (setq name value))
       (if (eq key :all-slots) (setq all-slots value))
       (if (eq key :struct-type) (setq struct-type value))
       (setq tail (cddr tail))
       (go scan)
     done)
    ;; Register slot info for :include inheritance.
    (setf (gethash name *defstruct-slot-registry*) all-slots)
    (setf (gethash name *defstruct-type-registry*) struct-type)
    ;; Dispatch: :type list/vector → non-CLOS expansion; otherwise → CLOS expansion.
    (if struct-type
        (%defstruct-typed-expansion model)
        (%defstruct-clos-expansion model))))
