(in-package :cl-cc/expand)

(defun expand-make-array-form (size rest-args)
  "Expand (make-array size &rest keyword-args).
Promotes to make-adjustable-vector when :fill-pointer or :adjustable is given.
Handles :initial-contents (FR-654) and :initial-element (FR-687) via loop expansion."
  (let (fp adj init-elem init-contents)
    (loop for (key val) on rest-args by #'cddr
          do (case key
               (:fill-pointer    (setf fp val))
               (:adjustable      (setf adj val))
               (:initial-element  (setf init-elem val))
               (:initial-contents (setf init-contents val))
               ;; :element-type, :displaced-to, :displaced-index-offset — silently ignored
               ))
    (cond
      ;; :initial-contents — build array from sequence
      (init-contents
       (let ((arr-g  (gensym "ARR"))
             (cont-g (gensym "CONT"))
             (i-g    (gensym "I")))
         (compiler-macroexpand-all
          `(let* ((,cont-g ,init-contents)
                  (,arr-g  (make-array (length ,cont-g))))
             (dotimes (,i-g (length ,cont-g) ,arr-g)
               (setf (aref ,arr-g ,i-g) (elt ,cont-g ,i-g)))))))
      ;; :fill-pointer / :adjustable — promote to adjustable vector
      ;; When :fill-pointer has a specific non-t value, set it after construction
      ((or fp adj)
       (if (and fp (not (eq fp t)))
           ;; Specific fill-pointer value: create vector then set fill-pointer
           (let ((arr-g (gensym "ARR"))
                 (fp-g  (gensym "FP")))
             (compiler-macroexpand-all
              `(let* ((,fp-g  ,fp)
                      (,arr-g (make-adjustable-vector ,size)))
                 (setf (fill-pointer ,arr-g) ,fp-g)
                 ,arr-g)))
           ;; Boolean fill-pointer (t) or :adjustable only
           (compiler-macroexpand-all `(make-adjustable-vector ,size))))
      ;; :initial-element — fill array with init value
      (init-elem
       (let ((arr-g (gensym "ARR"))
             (ie-g  (gensym "IE"))
             (i-g   (gensym "I")))
         (compiler-macroexpand-all
          `(let* ((,ie-g  ,init-elem)
                  (,arr-g (make-array ,size)))
             (dotimes (,i-g ,size ,arr-g)
               (setf (aref ,arr-g ,i-g) ,ie-g))))))
      ;; plain make-array
      (t
       (compiler-macroexpand-all `(make-array ,size))))))

(defun expand-setf-accessor (place value)
  "Expand (setf (ACCESSOR OBJ) VAL) via *accessor-slot-map* for known struct accessors,
or fall back to generic (setf (slot-value obj 'accessor-name) val)."
  (let ((mapping (gethash (car place) *accessor-slot-map*)))
    (if mapping
        (compiler-macroexpand-all
         `(setf (slot-value ,(second place) ',(cdr mapping)) ,value))
        (compiler-macroexpand-all
         `(setf (slot-value ,(second place) ',(car place)) ,value)))))

;;; ── DEFSTRUCT helpers ─────────────────────────────────────────────────────

(defun %defstruct-parse-slot (slot-form)
  "Normalize SLOT-FORM into (slot-name default read-only-p)."
  (if (listp slot-form)
      (list (first slot-form)
            (second slot-form)
            (getf (cddr slot-form) :read-only nil))
      (list slot-form nil nil)))

(defun %defstruct-find-option (options key)
  "Return the option form in OPTIONS whose first element is KEY."
  (find key options :key (lambda (option) (when (listp option) (first option)))))

(defun %defstruct-default-conc-name (name)
  "Return the default accessor prefix NAME-."
  (intern (concatenate 'string (symbol-name name) "-")))

(defun %defstruct-default-constructor-name (name)
  "Return the default constructor name MAKE-NAME."
  (intern (concatenate 'string "MAKE-" (symbol-name name))))

(defun %defstruct-default-predicate-name (name)
  "Return the default predicate name NAME-P."
  (intern (concatenate 'string (symbol-name name) "-P")))

(defun %defstruct-accessor-name (conc-name slot-name)
  "Return the effective accessor symbol for SLOT-NAME under CONC-NAME."
  (if conc-name
      (intern (concatenate 'string (symbol-name conc-name) (symbol-name slot-name)))
      slot-name))

(defun %defstruct-make-keyword (sym)
  "Return the keyword symbol for SYM."
  (intern (symbol-name sym) "KEYWORD"))

(defun %defstruct-boa-bindings (boa-args)
  "Return normalized BOA binding data for constructor generation."
  (let* ((parts (%defstruct-extract-boa-parts boa-args))
         (normal-params (car parts))
         (aux-bindings (cdr parts))
         (param-names (%defstruct-boa-param-names normal-params))
         (aux-names (mapcar #'first aux-bindings))
         (bound-names (append param-names aux-names))
         (aux-lets (mapcar (lambda (binding)
                             (list (first binding) (second binding)))
                           aux-bindings)))
    (values normal-params aux-bindings param-names bound-names aux-lets)))

(defun %defstruct-build-model (form)
  "Parse FORM into a plist consumed by defstruct emitters."
  (let* ((name-and-options (second form))
         (slots-raw (cddr form))
         (name (if (listp name-and-options) (first name-and-options) name-and-options))
         (options (when (listp name-and-options) (rest name-and-options)))
         (conc-opt (%defstruct-find-option options :conc-name))
         (ctor-opt (%defstruct-find-option options :constructor))
         (incl-opt (%defstruct-find-option options :include))
         (type-opt (%defstruct-find-option options :type))
         (pred-opt (%defstruct-find-option options :predicate))
         (print-fn-opt (%defstruct-find-option options :print-function))
         (print-obj-opt (%defstruct-find-option options :print-object))
         (deriving-opt (%defstruct-find-option options :deriving))
         (conc-name (if conc-opt
                        (second conc-opt)
                        (%defstruct-default-conc-name name)))
         (ctor-name (cond
                      ((and ctor-opt (null (second ctor-opt))) nil)
                      (ctor-opt (second ctor-opt))
                      (t (%defstruct-default-constructor-name name))))
         (boa-args (when (and ctor-opt (cddr ctor-opt)) (third ctor-opt)))
         (parent-name (when incl-opt (second incl-opt)))
         (parent-slots (when parent-name (gethash parent-name *defstruct-slot-registry*)))
         (struct-type (when type-opt (second type-opt)))
         (own-slots (mapcar #'%defstruct-parse-slot (remove-if #'stringp slots-raw)))
         (all-slots (append (or parent-slots nil) own-slots))
         (pred-name (cond
                      ((and pred-opt (null (second pred-opt))) nil)
                      (pred-opt (second pred-opt))
                      (t (%defstruct-default-predicate-name name))))
         (print-fn (or (when print-fn-opt (second print-fn-opt))
                       (when print-obj-opt (second print-obj-opt))))
         (derived-classes (when deriving-opt (rest deriving-opt))))
    (list :name name
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
