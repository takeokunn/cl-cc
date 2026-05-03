(in-package :cl-cc/expand)
;;;; DEFSTRUCT CLOS-backed expansion helpers.

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
