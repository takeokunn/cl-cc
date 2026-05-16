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
    (when (gethash (car place) *defstruct-read-only-accessor-map*)
      (error "SETF: ~S is read-only defstruct accessor" (car place)))
    (%expander-form 'rt-slot-set
                    (second place)
                    (%expander-form 'quote (if mapping (cdr mapping) (car place)))
                    (compiler-macroexpand-all value))))

;;; ── DEFSTRUCT helpers ─────────────────────────────────────────────────────

(defun %defstruct-parse-slot (slot-form)
  "Normalize SLOT-FORM into (slot-name default read-only-p)."
  (if (listp slot-form)
      (%expander-form (first slot-form)
                      (second slot-form)
                      (getf (cddr slot-form) :read-only))
      (%expander-form slot-form nil nil)))

(defun %defstruct-find-option (options key)
  "Return the option form in OPTIONS whose first element is KEY."
  (find key options :test #'eq :key (lambda (opt) (and (listp opt) (first opt)))))

(defun %defstruct-default-conc-name (name)
  "Return the default accessor prefix NAME-."
  (intern (format nil "~A-" (symbol-name name))))

(defun %defstruct-default-constructor-name (name)
  "Return the default constructor name MAKE-NAME."
  (intern (format nil "MAKE-~A" (symbol-name name))))

(defun %defstruct-default-predicate-name (name)
  "Return the default predicate name NAME-P."
  (intern (format nil "~A-P" (symbol-name name))))

(defun %defstruct-default-copier-name (name)
  "Return the default copier name COPY-NAME."
  (intern (format nil "COPY-~A" (symbol-name name))))

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
    (dolist (b aux-bindings)
      (setq bound-names (cons (first b) bound-names)))
    (setq aux-lets (loop for b in aux-bindings
                         collect (%expander-form (first b) (second b))))
    (%expander-form normal-params aux-bindings param-names bound-names aux-lets)))

(defun %defstruct-resolve-slot-values (all-slots bound-names)
  "Map ALL-SLOTS to effective constructor values: slot-name if bound, else default."
  (loop for slot in all-slots
        collect (if (member (first slot) bound-names :test #'eq)
                    (first slot)
                    (second slot))))

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
        (copier-opt nil)
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
        (copier-name nil)
        (print-fn nil)
        (derived-classes nil))
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
    (setq copier-opt (%defstruct-find-option options :copier))
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

    (setq own-slots (loop for item in slots-raw
                          unless (stringp item)
                            collect (%defstruct-parse-slot item)))
    (setq all-slots (append parent-slots own-slots))

    (if pred-opt
        (if (null (second pred-opt))
            (setq pred-name nil)
            (setq pred-name (second pred-opt)))
        (setq pred-name (%defstruct-default-predicate-name name)))

    (if copier-opt
        (if (null (second copier-opt))
            (setq copier-name nil)
            (setq copier-name (second copier-opt)))
        (setq copier-name (%defstruct-default-copier-name name)))

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
                    :copier-name copier-name
                    :print-fn print-fn
                    :print-fn-opt print-fn-opt
                    :derived-classes derived-classes)))
