(in-package :cl-cc/expand)
;;;; DEFSTRUCT :TYPE list/vector expansion helpers.

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
