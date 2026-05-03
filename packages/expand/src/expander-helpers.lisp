(in-package :cl-cc/expand)

(defun %expander-form (&rest args)
  "Build a source form without mentioning the selfhost LIST macro at call sites."
  args)

(defun expand-make-array-form (size rest-args)
  "Preserve MAKE-ARRAY keyword calls for the compiler backend.
Keyword handling belongs in codegen/VM, not in bootstrap macro expansion; keeping
this form small avoids recursively compiling large generated loops during
self-hosting."
  (cons 'make-array (cons size rest-args)))

(defun expand-setf-accessor (place value)
  "Expand (setf (ACCESSOR OBJ) VAL) via *accessor-slot-map* for known struct accessors,
or fall back to the runtime slot writer for unknown accessors."
  (let ((mapping (gethash (car place) *accessor-slot-map*)))
    (%expander-form 'rt-slot-set
                    (second place)
                    (%expander-form 'quote (if mapping (cdr mapping) (car place)))
                    (compiler-macroexpand-all value))))

;;; ── DEFSTRUCT helpers ─────────────────────────────────────────────────────

(defun %defstruct-parse-slot (slot-form)
  "Normalize SLOT-FORM into (slot-name default read-only-p)."
  (if (listp slot-form)
      (let ((tail (cddr slot-form))
            (read-only-p nil))
        (tagbody
         scan
           (if (null tail) (go done))
           (if (eq (car tail) :read-only)
               (progn
                 (setq read-only-p (cadr tail))
                 (go done)))
           (setq tail (cddr tail))
           (go scan)
         done)
        (%expander-form (first slot-form)
                        (second slot-form)
                        read-only-p))
      (%expander-form slot-form nil nil)))

(defun %defstruct-find-option (options key)
  "Return the option form in OPTIONS whose first element is KEY."
  (let ((tail options)
        (option nil)
        (found nil))
    (tagbody
     scan
       (if (null tail) (go done))
       (setq option (car tail))
       (if (listp option)
           (if (eq (first option) key)
               (progn
                 (setq found option)
                 (go done))))
       (setq tail (cdr tail))
       (go scan)
     done)
    found))

(defun %defstruct-default-conc-name (name)
  "Return the default accessor prefix NAME-."
  (intern (format nil "~A-" (symbol-name name))))

(defun %defstruct-default-constructor-name (name)
  "Return the default constructor name MAKE-NAME."
  (intern (format nil "MAKE-~A" (symbol-name name))))

(defun %defstruct-default-predicate-name (name)
  "Return the default predicate name NAME-P."
  (intern (format nil "~A-P" (symbol-name name))))

(defun %defstruct-accessor-name (conc-name slot-name)
  "Return the effective accessor symbol for SLOT-NAME under CONC-NAME."
  (if conc-name
      (intern (format nil "~A~A" (symbol-name conc-name) (symbol-name slot-name)))
      slot-name))

(defun %defstruct-make-keyword (sym)
  "Return the keyword symbol for SYM."
  (intern (symbol-name sym) "KEYWORD"))

(defun %defstruct-boa-bindings (boa-args)
  "Return normalized BOA binding data for constructor generation."
  (let ((parts nil)
        (normal-params nil)
        (aux-bindings nil)
        (param-names nil)
        (bound-names nil)
        (aux-tail nil)
        (aux-lets nil)
        (binding nil))
    (setq parts (%defstruct-extract-boa-parts boa-args))
    (setq normal-params (car parts))
    (setq aux-bindings (cdr parts))
    (setq param-names (%defstruct-boa-param-names normal-params))
    (setq bound-names param-names)
    (setq aux-tail aux-bindings)
    (tagbody
     scan
       (if (null aux-tail) (go done))
       (setq binding (car aux-tail))
       (setq bound-names (cons (first binding) bound-names))
       (setq aux-lets
             (cons (%expander-form (first binding) (second binding)) aux-lets))
       (setq aux-tail (cdr aux-tail))
       (go scan)
     done)
    (setq aux-lets (nreverse aux-lets))
    (%expander-form normal-params aux-bindings param-names bound-names aux-lets)))

(defun %defstruct-resolve-slot-values (all-slots bound-names)
  "Map ALL-SLOTS to effective constructor values: slot-name if bound, else default."
  (let ((slots-tail all-slots)
        (names-tail nil)
        (slot nil)
        (matched nil)
        (result nil))
    (tagbody
     scan-slots
       (if (null slots-tail) (go done))
       (setq slot (car slots-tail))
       (setq names-tail bound-names)
       (setq matched nil)
     scan-names
       (if (null names-tail) (go use-slot))
       (if (eq (first slot) (car names-tail))
           (progn
             (setq matched t)
             (go use-slot)))
       (setq names-tail (cdr names-tail))
       (go scan-names)
     use-slot
       (if matched
           (setq result (cons (first slot) result))
           (setq result (cons (second slot) result)))
       (setq slots-tail (cdr slots-tail))
       (go scan-slots)
     done)
    (nreverse result)))

(defun %defstruct-build-model (form)
  "Parse FORM into a plist consumed by defstruct emitters."
  (let ((name-and-options nil)
        (slots-raw nil)
        (name nil)
        (options nil)
        (conc-opt nil)
        (ctor-opt nil)
        (incl-opt nil)
        (type-opt nil)
        (pred-opt nil)
        (print-fn-opt nil)
        (print-obj-opt nil)
        (deriving-opt nil)
        (conc-name nil)
        (ctor-name nil)
        (boa-args nil)
        (parent-name nil)
        (parent-slots nil)
        (struct-type nil)
        (own-slots nil)
        (all-slots nil)
        (pred-name nil)
        (print-fn nil)
        (derived-classes nil)
        (tail nil)
        (item nil))
    (setq name-and-options (second form))
    (setq slots-raw (cddr form))
    (if (listp name-and-options)
        (progn
          (setq name (first name-and-options))
          (setq options (rest name-and-options)))
        (progn
          (setq name name-and-options)
          (setq options nil)))

    (setq conc-opt (%defstruct-find-option options :conc-name))
    (setq ctor-opt (%defstruct-find-option options :constructor))
    (setq incl-opt (%defstruct-find-option options :include))
    (setq type-opt (%defstruct-find-option options :type))
    (setq pred-opt (%defstruct-find-option options :predicate))
    (setq print-fn-opt (%defstruct-find-option options :print-function))
    (setq print-obj-opt (%defstruct-find-option options :print-object))
    (setq deriving-opt (%defstruct-find-option options :deriving))

    (if conc-opt
        (setq conc-name (second conc-opt))
        (setq conc-name (%defstruct-default-conc-name name)))

    (if ctor-opt
        (if (null (second ctor-opt))
            (setq ctor-name nil)
            (setq ctor-name (second ctor-opt)))
        (setq ctor-name (%defstruct-default-constructor-name name)))
    (if ctor-opt
        (if (cddr ctor-opt)
            (setq boa-args (third ctor-opt))))

    (if incl-opt
        (setq parent-name (second incl-opt)))
    (if parent-name
        (setq parent-slots (gethash parent-name *defstruct-slot-registry*)))
    (if type-opt
        (setq struct-type (second type-opt)))

    (setq tail slots-raw)
    (tagbody
     scan-own-slots
       (if (null tail) (go own-slots-done))
       (setq item (car tail))
       (if (stringp item)
           nil
           (setq own-slots (cons (%defstruct-parse-slot item) own-slots)))
       (setq tail (cdr tail))
       (go scan-own-slots)
     own-slots-done)
    (setq own-slots (nreverse own-slots))

    (setq tail parent-slots)
    (tagbody
     scan-parent-slots
       (if (null tail) (go parent-slots-done))
       (setq all-slots (cons (car tail) all-slots))
       (setq tail (cdr tail))
       (go scan-parent-slots)
     parent-slots-done)
    (setq tail own-slots)
    (tagbody
     scan-all-own-slots
       (if (null tail) (go all-slots-done))
       (setq all-slots (cons (car tail) all-slots))
       (setq tail (cdr tail))
       (go scan-all-own-slots)
     all-slots-done)
    (setq all-slots (nreverse all-slots))

    (if pred-opt
        (if (null (second pred-opt))
            (setq pred-name nil)
            (setq pred-name (second pred-opt)))
        (setq pred-name (%defstruct-default-predicate-name name)))

    (if print-fn-opt
        (setq print-fn (second print-fn-opt))
        (if print-obj-opt
            (setq print-fn (second print-obj-opt))))
    (if deriving-opt
        (setq derived-classes (rest deriving-opt)))

    (%expander-form :name name
                    :conc-name conc-name
                    :ctor-name ctor-name
                    :boa-args boa-args
                    :parent-name parent-name
                    :struct-type struct-type
                    :own-slots own-slots
                    :all-slots all-slots
                    :pred-name pred-name
                    :print-fn print-fn
                    :print-fn-opt print-fn-opt
                    :derived-classes derived-classes)))
