(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — MOP Introspection and Allocation Caches
;;;
;;; Contains: FR-930/FR-931 MOP helpers (%mop-*), slot-definition-*,
;;; class-direct-*, generic-function-methods, method-*, add-method,
;;; remove-method, ensure-generic-function, slot-value-using-class,
;;; and FR-888/FR-889 allocation cache functions
;;; (%vm-ensure-class-id through finalize-class-allocation-cache).
;;;
;;; Load order: after vm-clos.lisp, before vm-clos-execute.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── FR-930/FR-931 MOP introspection and method selection ────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow '(add-method compute-applicable-methods
            compute-applicable-methods-using-classes ensure-generic-function
            find-method method-qualifiers method-specializers remove-method)
          :cl-cc/vm))

(defvar *mop-generic-function-registry* (make-hash-table :test #'equal)
  "Host-side registry for generic functions created by ENSURE-GENERIC-FUNCTION.")

(defvar *unbound-slot-marker*)

(defun %mop-hash-table-values (table)
  "Return values from TABLE in an unspecified but stable traversal list."
  (let (values)
    (when (hash-table-p table)
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (push value values))
               table))
    (nreverse values)))

(defun %mop-method-value->list (value)
  "Normalize a method table VALUE to a flat list of methods."
  (cond
    ((null value) nil)
    ((and (listp value) (not (%eql-specializer-p value))) (copy-list value))
    (t (list value))))

(defun %mop-slot-initargs-for-slot (class slot-name)
  "Return initargs in CLASS that initialize SLOT-NAME."
  (let (initargs)
    (dolist (entry (and (hash-table-p class) (gethash :__initargs__ class)))
      (when (eq (cdr entry) slot-name)
        (pushnew (car entry) initargs :test #'eq)))
    (nreverse initargs)))

(defun %mop-slot-definition (class slot-name)
  "Build a lightweight slot-definition descriptor for SLOT-NAME in CLASS."
  (let ((slot (make-hash-table :test #'eq)))
    (setf (gethash :name slot) slot-name
          (gethash :initargs slot) (%mop-slot-initargs-for-slot class slot-name)
          (gethash :type slot) (or (cdr (assoc slot-name (gethash :__slot-types__ class)
                                               :test #'eq))
                                   t)
          (gethash :allocation slot)
          (if (member slot-name (gethash :__class-slots__ class) :test #'eq)
              :class
              :instance)
          (gethash :location slot)
          (cdr (assoc slot-name (gethash :__slot-locations__ class) :test #'eq)))
    (let ((initform (assoc slot-name (gethash :__initforms__ class) :test #'eq)))
      (when initform
        (setf (gethash :initform slot) (cdr initform))))
    slot))

(defun %mop-slot-definitions (class key)
  "Return slot-definition descriptors for slot names stored under KEY."
  (when (hash-table-p class)
    (mapcar (lambda (slot-name) (%mop-slot-definition class slot-name))
            (gethash key class))))

(defun class-slots (class)
  "Return effective slot-definition objects for CLASS."
  (%mop-slot-definitions class :__slots__))

(defun class-direct-slots (class)
  "Return direct slot-definition objects for CLASS."
  (%mop-slot-definitions class :__direct-slots__))

(defun slot-definition-name (slot)
  "Return SLOT's name."
  (cond ((hash-table-p slot) (gethash :name slot))
        ((symbolp slot) slot)
        (t nil)))

(defun slot-definition-type (slot)
  "Return SLOT's declared type, defaulting to T."
  (if (hash-table-p slot) (or (gethash :type slot) t) t))

(defun slot-definition-initform (slot)
  "Return SLOT's initform metadata, or NIL."
  (and (hash-table-p slot) (gethash :initform slot)))

(defun slot-definition-initargs (slot)
  "Return SLOT's initarg metadata."
  (and (hash-table-p slot) (gethash :initargs slot)))

(defun slot-definition-allocation (slot)
  "Return SLOT's allocation mode, defaulting to :INSTANCE."
  (if (hash-table-p slot) (or (gethash :allocation slot) :instance) :instance))

(defun slot-definition-location (slot)
  "Return SLOT's effective storage location, or NIL."
  (and (hash-table-p slot) (gethash :location slot)))

(defun class-direct-superclasses (class)
  "Return direct superclass designators for CLASS."
  (and (hash-table-p class) (gethash :__superclasses__ class)))

(defun class-direct-subclasses (class)
  "Return direct subclass designators recorded in CLASS metadata."
  (and (hash-table-p class) (gethash :__direct-subclasses__ class)))

(defun class-precedence-list (class)
  "Return CLASS's class precedence list."
  (when (hash-table-p class)
    (or (gethash :__cpl__ class)
        (let ((name (gethash :__name__ class)))
          (and name (cons name (copy-list (gethash :__superclasses__ class))))))))

(defun generic-function-methods (gf)
  "Return all method descriptors registered on GF, including qualified methods."
  (let (methods)
    (when (hash-table-p gf)
      (dolist (table-key '(:__methods__ :__BEFORE__ :__AFTER__ :__AROUND__))
        (dolist (value (%mop-hash-table-values (gethash table-key gf)))
          (dolist (method (%mop-method-value->list value))
            (pushnew method methods :test #'eq)))))
    (nreverse methods)))

(defun method-specializers (method)
  "Return METHOD specializers as a list."
  (when (hash-table-p method)
    (or (gethash :specializers method)
        (let ((specializer (gethash :specializer method)))
          (cond ((null specializer) nil)
                ((and (listp specializer) (not (%eql-specializer-p specializer)))
                 specializer)
                (t (list specializer)))))))

(defun method-qualifiers (method)
  "Return METHOD qualifiers."
  (if (hash-table-p method) (or (gethash :qualifiers method) nil) nil))

(defun generic-function-method-combination (gf)
  "Return GF's method-combination object, defaulting to STANDARD."
  (if (and (hash-table-p gf) (gethash :__method-combination__ gf))
      (gethash :__method-combination__ gf)
      'standard))

(defun satiating-gfs-p (&optional gf)
  "Return T when GF is a VM generic-function marked satiated, or when called
with no argument returns T if any GF has been satiated in the current session."
  (if gf
      (and (vm-generic-function-p gf) (gethash :__satiated__ gf) t)
      nil))

(defun method-combination-type (method-combination)
  "Return METHOD-COMBINATION's type designator."
  (if (hash-table-p method-combination)
      (or (gethash :type method-combination) (gethash :name method-combination) 'standard)
      method-combination))

(defun %mop-normalize-specializer-key (specializers)
  "Return the dispatch-table key for SPECIALIZERS."
  (if (and (listp specializers) (= (length specializers) 1))
      (first specializers)
      specializers))

(defun %mop-qualified-table-key (qualifiers)
  "Return the GF table key for QUALIFIERS."
  (if qualifiers
      (intern (format nil "__~A__" (string-upcase (string (first qualifiers)))) :keyword)
      :__methods__))

(defun %mop-class-cpl (class)
  "Return CLASS's CPL as symbol designators, always ending with T."
  (let ((cpl (cond ((hash-table-p class) (or (gethash :__cpl__ class)
                                             (list (gethash :__name__ class))))
                   ((symbolp class) (list class))
                   (t (list t)))))
    (if (member t cpl :test #'eq) cpl (append cpl (list t)))))

(defun %mop-arg-class (arg)
  "Return ARG's class descriptor/name for class-only MOP dispatch."
  (cond ((and (vectorp arg) (plusp (length arg)) (hash-table-p (aref arg 0)))
         (aref arg 0))
        ((hash-table-p arg) (or (gethash :__class__ arg) 'hash-table))
        ((integerp arg) 'integer)
        ((stringp arg) 'string)
        ((symbolp arg) 'symbol)
        (t t)))

(defun %mop-specializer-matches-arg-p (specializer arg)
  "Return true when SPECIALIZER accepts ARG."
  (cond ((eq specializer t) t)
        ((%eql-specializer-p specializer) (eql (second specializer) arg))
        (t (member specializer (%mop-class-cpl (%mop-arg-class arg)) :test #'eq))))

(defun %mop-method-applicable-to-args-p (method args)
  "Return true when METHOD is applicable to ARGS."
  (let ((specializers (method-specializers method)))
    (and (= (length specializers) (length args))
         (every #'%mop-specializer-matches-arg-p specializers args))))

(defun %mop-method-applicable-to-classes-p (method classes)
  "Return true when METHOD is applicable to argument CLASSES."
  (let ((specializers (method-specializers method)))
    (and (= (length specializers) (length classes))
         (every (lambda (specializer class)
                  (and (not (%eql-specializer-p specializer))
                       (or (eq specializer t)
                           (member specializer (%mop-class-cpl class) :test #'eq))))
                specializers classes))))

(defun compute-applicable-methods (gf args)
  "Return methods of GF applicable to ARGS."
  (remove-if-not (lambda (method) (%mop-method-applicable-to-args-p method args))
                 (generic-function-methods gf)))

(defun compute-applicable-methods-using-classes (gf classes)
  "Return (values METHODS DEFINITIVEP) for GF and argument CLASSES."
  (let* ((methods (generic-function-methods gf))
         (has-eql-p (some (lambda (method)
                            (some #'%eql-specializer-p (method-specializers method)))
                          methods)))
    (values (remove-if-not (lambda (method)
                             (%mop-method-applicable-to-classes-p method classes))
                           methods)
            (not has-eql-p))))

(defun find-method (gf qualifiers specializers &optional (errorp t))
  "Find the method on GF with QUALIFIERS and SPECIALIZERS."
  (let* ((table (and (hash-table-p gf) (gethash (%mop-qualified-table-key qualifiers) gf)))
         (key (%mop-normalize-specializer-key specializers))
         (candidates (%mop-method-value->list (and table (gethash key table))))
         (method (find-if (lambda (method)
                            (and (equal (method-qualifiers method) qualifiers)
                                 (equal (method-specializers method) specializers)))
                          candidates)))
    (cond (method method)
          (errorp (error "No method on ~S with qualifiers ~S and specializers ~S"
                         gf qualifiers specializers))
          (t nil))))

(defun %mop-ensure-method-table (gf qualifiers)
  "Return GF's method table for QUALIFIERS, creating it when needed."
  (let ((key (%mop-qualified-table-key qualifiers)))
    (or (gethash key gf)
        (setf (gethash key gf) (make-hash-table :test #'equal)))))

(defun %mop-invalidate-gf (gf)
  "Invalidate method-selection caches associated with GF."
  (setf (gethash :__satiated__ gf) nil
        (gethash :__cache-version__ gf) (1+ (or (gethash :__cache-version__ gf) 0)))
  (remhash :__dispatch-cache__ gf)
  gf)

(defun add-method (gf method)
  "Add METHOD to GF and invalidate dispatch metadata."
  (unless (hash-table-p gf) (error "add-method: ~S is not a generic function" gf))
  (let* ((qualifiers (method-qualifiers method))
         (specializers (method-specializers method))
         (table (%mop-ensure-method-table gf qualifiers))
         (key (%mop-normalize-specializer-key specializers)))
    (when (hash-table-p method)
      (setf (gethash :gf method) gf
            (gethash :generic-function method) gf
            (gethash :specializer method) key
            (gethash :specializers method) specializers))
    (if qualifiers
        (pushnew method (gethash key table) :test #'eq)
        (setf (gethash key table) method))
    (%mop-invalidate-gf gf)))

(defun remove-method (gf method)
  "Remove METHOD from GF and invalidate dispatch metadata."
  (unless (hash-table-p gf) (error "remove-method: ~S is not a generic function" gf))
  (dolist (table-key '(:__methods__ :__BEFORE__ :__AFTER__ :__AROUND__))
    (let ((table (gethash table-key gf)))
      (when (hash-table-p table)
        (let (updates removals)
          (maphash (lambda (key value)
                     (let ((remaining (remove method (%mop-method-value->list value) :test #'eq)))
                       (if remaining
                           (push (cons key (if (or (eq table-key :__methods__)
                                                   (= (length remaining) 1))
                                               (first remaining)
                                               remaining))
                                 updates)
                           (push key removals))))
                   table)
          (dolist (key removals)
            (remhash key table))
          (dolist (update updates)
            (setf (gethash (car update) table) (cdr update)))))))
  (%mop-invalidate-gf gf))

(defun ensure-generic-function (name &rest options
                                &key lambda-list method-class documentation
                                &allow-other-keys)
  "Return an existing or new generic-function descriptor for NAME."
  (declare (ignore options))
  (let ((gf (if (hash-table-p name)
                name
                (gethash name *mop-generic-function-registry*))))
    (unless gf
      (setf gf (make-hash-table :test #'equal)
            (gethash :__name__ gf) name
            (gethash :__methods__ gf) (make-hash-table :test #'equal)
            (gethash :__eql-index__ gf) (make-hash-table :test #'equal)
            (gethash :__method-combination__ gf) 'standard)
      (setf (gethash name *mop-generic-function-registry*) gf))
    (when lambda-list (setf (gethash :__lambda-list__ gf) lambda-list))
    (when method-class (setf (gethash :__method-class__ gf) method-class))
    (when documentation (setf (gethash :__documentation__ gf) documentation))
    gf))

(defun slot-value-using-class (class object slot-name)
  "Read SLOT-NAME from OBJECT using CLASS metadata."
  (let ((class-slots (and (hash-table-p class) (gethash :__class-slots__ class))))
    (cond ((and class-slots (member slot-name class-slots :test #'eq))
           (gethash slot-name class))
          ((and (vectorp object) (plusp (length object)) (hash-table-p class))
           (let ((index (class-slot-vector-index class slot-name)))
             (if index (aref object index) (error "Missing slot ~S" slot-name))))
          ((hash-table-p object)
           (multiple-value-bind (value found-p) (gethash slot-name object)
             (if found-p value (error "Unbound slot ~S" slot-name))))
          (t (error "slot-value-using-class: unsupported object ~S" object)))))

(defun (setf slot-value-using-class) (new-value class object slot-name)
  "Write NEW-VALUE to SLOT-NAME in OBJECT using CLASS metadata."
  (let ((class-slots (and (hash-table-p class) (gethash :__class-slots__ class))))
    (cond ((and class-slots (member slot-name class-slots :test #'eq))
           (setf (gethash slot-name class) new-value))
          ((and (vectorp object) (plusp (length object)) (hash-table-p class))
           (let ((index (class-slot-vector-index class slot-name)))
             (unless index (error "Missing slot ~S" slot-name))
             (setf (aref object index) new-value)))
          ((hash-table-p object) (setf (gethash slot-name object) new-value))
          (t (error "(setf slot-value-using-class): unsupported object ~S" object)))))

(defun slot-bound-using-class-p (class object slot-name)
  "Return true when SLOT-NAME is bound in OBJECT using CLASS metadata."
  (let ((class-slots (and (hash-table-p class) (gethash :__class-slots__ class))))
    (cond ((and class-slots (member slot-name class-slots :test #'eq))
           (nth-value 1 (gethash slot-name class)))
          ((and (vectorp object) (plusp (length object)) (hash-table-p class))
           (let ((index (class-slot-vector-index class slot-name)))
             (and index (not (eq (aref object index) *unbound-slot-marker*)))))
          ((hash-table-p object) (nth-value 1 (gethash slot-name object)))
          (t nil))))

(defun slot-makunbound-using-class (class object slot-name)
  "Make SLOT-NAME unbound in OBJECT using CLASS metadata and return OBJECT."
  (let ((class-slots (and (hash-table-p class) (gethash :__class-slots__ class))))
    (cond ((and class-slots (member slot-name class-slots :test #'eq))
           (remhash slot-name class))
          ((and (vectorp object) (plusp (length object)) (hash-table-p class))
           (let ((index (class-slot-vector-index class slot-name)))
             (unless index (error "Missing slot ~S" slot-name))
             (setf (aref object index) *unbound-slot-marker*)))
          ((hash-table-p object) (remhash slot-name object))
          (t (error "slot-makunbound-using-class: unsupported object ~S" object))))
  object)

;;; ── FR-888/FR-889 allocation caches ──────────────────────────────────────

(defvar *vm-next-class-id* 0
  "Monotonic class id source used by vector-backed instances.")

(defun %vm-ensure-class-id (class-ht)
  "Return CLASS-HT's stable numeric class id, assigning one if needed."
  (or (gethash :__class-id__ class-ht)
      (setf (gethash :__class-id__ class-ht) (incf *vm-next-class-id*))))

(defun %vm-build-slot-vector-index (slots)
  "Build a SLOT-NAME→storage-index hash table for vector instance slots.
Index 0 is reserved for the class header, so the first slot starts at 1."
  (let ((index (make-hash-table :test #'eq)))
    (loop for slot-name in slots
          for storage-index from 1
          do (setf (gethash slot-name index) storage-index))
    index))

(defun %vm-fixed-slot-layout-p (class-ht)
  "Return true when CLASS-HT can use vector-backed fixed slot storage."
  (and (hash-table-p class-ht)
       (not (gethash :__dynamic-slot-layout__ class-ht))
       (not (gethash :__obsolete__ class-ht))))

(defun class-slot-vector-index (class-ht slot-name)
  "Return SLOT-NAME's O(1) vector storage index in CLASS-HT, or NIL.
The cache is created during finalization and lazily rebuilt for older class
tables that predate FR-888 metadata."
  (when (hash-table-p class-ht)
    (let ((index (or (gethash :__slot-vector-index__ class-ht)
                     (setf (gethash :__slot-vector-index__ class-ht)
                           (%vm-build-slot-vector-index
                            (gethash :__slots__ class-ht))))))
      (gethash slot-name index))))

(defun allocate-instance-vector (class-ht &optional initial-element)
  "Allocate a vector-backed instance for CLASS-HT.
The vector header stores the class descriptor; the descriptor carries the
numeric class-id used by typep/class-of fast paths."
  (unless (%vm-fixed-slot-layout-p class-ht)
    (error "Class ~S does not have a fixed slot layout"
           (and (hash-table-p class-ht) (gethash :__name__ class-ht))))
  (%vm-ensure-class-id class-ht)
  (let ((instance (make-array (1+ (length (gethash :__slots__ class-ht)))
                              :initial-element initial-element)))
    (setf (aref instance 0) class-ht)
    instance))

(defun slot-value-by-index (instance index)
  "Return INSTANCE's slot value at vector storage INDEX in O(1)."
  (unless (and (vectorp instance) (integerp index)
               (<= 0 index) (< index (length instance)))
    (error "Invalid vector slot index ~S for ~S" index instance))
  (aref instance index))

(defun (setf slot-value-by-index) (value instance index)
  "Set INSTANCE's slot value at vector storage INDEX in O(1)."
  (unless (and (vectorp instance) (integerp index)
               (<= 0 index) (< index (length instance)))
    (error "Invalid vector slot index ~S for ~S" index instance))
  (setf (aref instance index) value))

(defun %vm-vector-instance-class-id (instance)
  "Return INSTANCE's class-id from its vector header, or NIL."
  (when (and (vectorp instance) (plusp (length instance))
             (hash-table-p (aref instance 0)))
    (gethash :__class-id__ (aref instance 0))))

(defun %vm-build-instance-template (class-ht &optional initial-element)
  "Build the zero-arg make-instance template for CLASS-HT."
  (allocate-instance-vector class-ht initial-element))

(defun %vm-copy-instance-template (class-ht &optional initial-element)
  "Return a fresh instance by copying CLASS-HT's zero-arg template."
  (copy-seq (or (gethash :__instance-template__ class-ht)
                (setf (gethash :__instance-template__ class-ht)
                      (%vm-build-instance-template class-ht initial-element)))))

(defun %vm-initargs-signature (initargs)
  "Return a compact cache signature for INITARGS.
Only keys affect the specialized constructor path; values remain per-call."
  (loop for entry in initargs
        for key = (car entry)
        unless (eq key :allow-other-keys)
          collect key))

(defun %vm-make-instance-cache (class-ht)
  "Return CLASS-HT's make-instance specialized path cache."
  (or (gethash :__make-instance-cache__ class-ht)
      (setf (gethash :__make-instance-cache__ class-ht)
            (make-hash-table :test #'equal))))

(defun %vm-cached-constructor-path (class-ht initargs)
  "Return the cached constructor path for CLASS-HT and INITARGS."
  (let* ((signature (%vm-initargs-signature initargs))
         (cache-key (cons class-ht signature))
         (cache (%vm-make-instance-cache class-ht)))
    (or (gethash cache-key cache)
        (setf (gethash cache-key cache)
              (if signature :standard-vector :zero-arg-template)))))

(defun merge-cached-default-initargs (cached-defaults initargs)
  "Merge evaluated CACHED-DEFAULTS with INITARGS in one pass.
Explicit INITARGS win over default-initargs.  Both arguments are alists."
  (let ((seen (make-hash-table :test #'eq))
        (merged nil))
    (dolist (entry initargs)
      (setf (gethash (car entry) seen) t)
      (push entry merged))
    (dolist (entry cached-defaults)
      (unless (gethash (car entry) seen)
        (push entry merged)))
    (nreverse merged)))

(defun finalize-class-allocation-cache (class-ht)
  "Finalize FR-888/FR-889 allocation metadata on CLASS-HT.
This records fixed-layout slot indexes, evaluated default-initargs, the class-id,
the zero-arg instance template, and the per-signature make-instance cache."
  (unless (hash-table-p class-ht)
    (error "Expected class hash table, got ~S" class-ht))
  (setf (gethash :__fixed-slot-layout-p__ class-ht) (%vm-fixed-slot-layout-p class-ht)
        (gethash :__slot-vector-index__ class-ht)
        (%vm-build-slot-vector-index (gethash :__slots__ class-ht))
        (gethash :__cached-default-initargs__ class-ht)
        (copy-list (gethash :__default-initargs__ class-ht))
        (gethash :__make-instance-cache__ class-ht)
        (make-hash-table :test #'equal))
  (%vm-ensure-class-id class-ht)
  (when (gethash :__fixed-slot-layout-p__ class-ht)
    (setf (gethash :__instance-template__ class-ht)
          (%vm-build-instance-template class-ht)))
  class-ht)
