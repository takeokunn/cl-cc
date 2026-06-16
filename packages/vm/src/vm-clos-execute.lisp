(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — CLOS Instruction Execution Methods
;;;
;;; Contains all execute-instruction methods for CLOS VM instructions:
;;; vm-class-def (class registration + CPL), vm-make-obj (instance creation
;;; with initarg validation), vm-slot-read/write, vm-slot-boundp/makunbound/
;;; exists-p, vm-register-method, vm-generic-call; plus FR-677 class-name/
;;; class-of/typep instructions and change-class.
;;;
;;; Instruction defstructs (define-vm-instruction forms) and MRO helpers
;;; (collect-inherited-slots, compute-class-precedence-list) are in
;;; vm-clos.lisp (loads before).
;;;
;;; Load order: after vm-clos.lisp, before vm-dispatch-gf.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defparameter *unbound-slot-marker* (gensym "UNBOUND-SLOT-")
  "Unique sentinel stored in vector-backed instances for unbound slots.")


(defun %vm-cdef-collect-slots (supers own-slots registry)
  "Merge inherited and own slot names; inherited slots come first."
  (let ((inherited (collect-inherited-slots supers registry)))
    (append inherited (remove-if (lambda (s) (member s inherited)) own-slots))))

(defun %vm-cdef-collect-initargs (supers own-initargs registry)
  "Merge inherited and own initarg→slot alist; inherited entries come first."
  (let ((inherited (collect-inherited-initargs supers registry)))
    (append inherited
            (remove-if (lambda (e) (assoc (car e) inherited :test #'eq)) own-initargs))))

(defun %vm-cdef-collect-initforms (supers own-initforms registry)
  "Merge slot initforms according to effective-slot precedence.
OWN-INITFORMS wins over inherited initforms.  Among inherited slots, earlier
superclasses are more specific than later superclasses, and each superclass
descriptor already carries its effective inherited initforms."
  (let ((inherited nil))
    (dolist (super supers)
      (let ((super-ht (gethash super registry)))
        (when super-ht
          (dolist (entry (gethash :__initforms__ super-ht))
            (unless (or (assoc (car entry) own-initforms :test #'eq)
                        (assoc (car entry) inherited :test #'eq))
              (push entry inherited))))))
    (append own-initforms (nreverse inherited))))

(defun %vm-cdef-collect-default-initargs (inst supers registry state)
  "Collect default-initarg key→value pairs; own values take precedence over inherited."
  (let ((own (loop for (key . reg) in (vm-default-initarg-regs inst)
                   collect (cons key (vm-reg-get state reg))))
        (inherited (loop for super in supers
                         for super-ht = (gethash super registry)
                         when super-ht
                           append (gethash :__default-initargs__ super-ht))))
    (append own (remove-if (lambda (e) (assoc (car e) own)) inherited))))

(defun %vm-cdef-collect-slot-types (inst supers registry)
  "Collect slot-name→type pairs; own slot type declarations take precedence."
  (let ((own (vm-slot-types inst))
        (inherited (loop for super in supers
                         for super-ht = (gethash super registry)
                         when super-ht
                           append (gethash :__slot-types__ super-ht))))
    (append own (remove-if (lambda (e) (assoc (car e) own)) inherited))))

(defun %vm-cdef-collect-class-slots (inst supers registry)
  "Union of own class-allocated slots with all inherited class-allocated slots."
  (let ((inherited (loop for super in supers
                         for super-ht = (gethash super registry)
                         when super-ht
                           append (gethash :__class-slots__ super-ht))))
    (union (vm-class-slots inst) inherited :test #'eq)))

(defun %vm-cdef-slot-locations (slots)
  "Return SLOT-NAME→zero-based-index metadata for the effective slot list SLOTS."
  (loop for slot in slots
        for index from 0
        collect (cons slot index)))

(defun %vm-cdef-init-class-slots (class-ht all-class-slots initform-values)
  "Initialize class-allocated slots on CLASS-HT from INITFORM-VALUES (skip existing values)."
  (dolist (slot-name all-class-slots)
    (unless (gethash slot-name class-ht)
      (let ((entry (assoc slot-name initform-values)))
        (setf (gethash slot-name class-ht)
               (if entry (cdr entry) nil))))))

(defun %vm-mark-class-obsolete (old-class replacement-class)
  "Record that OLD-CLASS should lazily migrate instances to REPLACEMENT-CLASS."
  (when (hash-table-p old-class)
    (setf (gethash :__obsolete__ old-class) t
          (gethash :__replacement-class__ old-class) replacement-class))
  old-class)

(defun %vm-hashlike-storage (table)
  "Return TABLE's host hash-table storage for host and VM hash tables."
  (cond
    ((hash-table-p table) table)
    ((typep table 'vm-hash-table-object) (vm-hash-table-internal table))
    (t nil)))

(defun %vm-hashlike-gethash (key table &optional default)
  "GETHASH for host hash tables and VM hash-table objects."
  (let ((storage (%vm-hashlike-storage table)))
    (if storage (gethash key storage default) default)))

(defun %vm-hashlike-sethash (key table value)
  "Set KEY in a host hash table or VM hash-table object."
  (let ((storage (%vm-hashlike-storage table)))
    (unless storage (error "Expected hash table, got ~S" table))
    (setf (gethash key storage) value)))

(defun %vm-vector-instance-p (object)
  "Return T when OBJECT is a vector-backed standard instance."
  (and (vectorp object)
       (plusp (length object))
       (hash-table-p (aref object 0))))

(defun %vm-slot-vector-index (class-ht slot-name)
  "Return SLOT-NAME's vector index in CLASS-HT instances, or NIL."
  (let ((location (%vm-effective-slot-location class-ht slot-name)))
    (and location (1+ location))))

(defun %vm-update-obsolete-instance (obj-ht)
  "Migrate OBJ-HT from an obsolete class descriptor to its replacement, if any."
  (let* ((old-class (cond
                      ((hash-table-p obj-ht) (gethash :__class__ obj-ht))
                      ((%vm-vector-instance-p obj-ht) (aref obj-ht 0))))
          (new-class (and (hash-table-p old-class)
                          (gethash :__obsolete__ old-class)
                          (gethash :__replacement-class__ old-class))))
    (when (hash-table-p new-class)
      (let ((old-slots (gethash :__slots__ old-class))
            (new-slots (gethash :__slots__ new-class)))
        (cond
          ((hash-table-p obj-ht)
           (dolist (slot old-slots)
             (unless (member slot new-slots :test #'eq)
               (remhash slot obj-ht)))
           (dolist (slot new-slots)
             (multiple-value-bind (value found-p) (gethash slot obj-ht)
               (declare (ignore value))
               (unless found-p
                 (setf (gethash slot obj-ht) nil))))
           (setf (gethash :__class__ obj-ht) new-class))
          ((and (%vm-vector-instance-p obj-ht)
                (<= (1+ (length new-slots)) (length obj-ht)))
           (let ((old-values nil))
             (dolist (slot old-slots)
               (let ((index (%vm-slot-vector-index old-class slot)))
                 (when (and index (< index (length obj-ht)))
                   (push (cons slot (aref obj-ht index)) old-values))))
             (loop for index from 1 below (length obj-ht)
                   do (setf (aref obj-ht index) *unbound-slot-marker*))
             (dolist (slot new-slots)
               (let ((index (%vm-slot-vector-index new-class slot))
                     (old-entry (assoc slot old-values :test #'eq)))
                 (when (and index (< index (length obj-ht)))
                   (setf (aref obj-ht index)
                         (if old-entry (cdr old-entry) nil)))))
             (setf (aref obj-ht 0) new-class))))))
    obj-ht))

(defun %vm-obj-class-ht (obj-ht)
  "Return the class hash-table for an instance OBJ-HT, or NIL."
  (cond
    ((%vm-vector-instance-p obj-ht)
     (%vm-update-obsolete-instance obj-ht)
     (aref obj-ht 0))
    ((%vm-hashlike-storage obj-ht)
     (%vm-update-obsolete-instance obj-ht)
     (%vm-hashlike-gethash :__class__ obj-ht))))

(defun %vm-class-slots-of (obj-ht)
  "Return (values class-ht class-slots) for class-allocation routing."
  (let ((own-class-slots (%vm-hashlike-gethash :__class-slots__ obj-ht)))
    (if own-class-slots
        (values obj-ht own-class-slots)
        (let ((class-ht (%vm-obj-class-ht obj-ht)))
          (values class-ht (when class-ht (%vm-hashlike-gethash :__class-slots__ class-ht)))))))

(defun %vm-apply-initarg (initarg-key value initarg-map class-slots class-ht obj-ht)
  "Write VALUE to the slot for INITARG-KEY, routing class-allocated slots to CLASS-HT."
  (let ((slot-entry (assoc initarg-key initarg-map)))
    (when slot-entry
      (let ((slot-name (cdr slot-entry)))
        (if (member slot-name class-slots :test #'eq)
            (setf (gethash slot-name class-ht) value)
            (%vm-raw-slot-write class-ht class-slots obj-ht slot-name value))))))

(defun %vm-standard-metaclass-p (metaclass)
  "Return T when METACLASS denotes the built-in standard metaclass path."
  (or (null metaclass)
      (eq metaclass 'standard-class)
      (and (hash-table-p metaclass)
           (eq (gethash :__name__ metaclass) 'standard-class))))

(defun %vm-resolve-class-designator (designator state)
  "Resolve a class DESIGNATOR (symbol or descriptor) to a class hash table when possible."
  (cond
    ((hash-table-p designator) designator)
    ((symbolp designator) (gethash designator (vm-class-registry state)))
    (t nil)))

(defun %vm-class-effective-metaclass (class-ht state)
  "Return the effective metaclass descriptor or symbol for CLASS-HT."
  (let ((metaclass (and (hash-table-p class-ht)
                        (gethash :__metaclass__ class-ht))))
    (or (%vm-resolve-class-designator metaclass state) metaclass)))

(defun %vm-class-nonstandard-metaclass-p (class-ht state)
  "Return T when CLASS-HT has a custom metaclass."
  (not (%vm-standard-metaclass-p (%vm-class-effective-metaclass class-ht state))))

(defun %vm-global-generic-function (state name)
  "Return global generic function NAME, or NIL if it is not available."
  (let ((value (or (gethash name (vm-global-vars state))
                   (when (consp name)
                     (let ((matched nil))
                       (maphash (lambda (key candidate)
                                  (when (and (null matched) (equal key name))
                                    (setf matched candidate)))
                                (vm-global-vars state))
                       matched)))))
    (when (and (hash-table-p value)
               (gethash :__methods__ value))
      value)))

(defun %vm-call-generic-sync (gf-ht state args &key default)
  "Synchronously call GF-HT with ARGS using the standard method combination.
DEFAULT is called when no primary method is applicable.  This helper is used by
VM primitives that need protocol hooks without introducing new instructions."
  (let* ((primary-methods (vm-get-all-applicable-methods gf-ht state args))
         (before-methods (%lookup-qualified-methods gf-ht :__BEFORE__ state args))
         (after-methods (%lookup-qualified-methods gf-ht :__AFTER__ state args))
         (around-methods (%lookup-qualified-methods gf-ht :__AROUND__ state args)))
     (cond
       (around-methods
        (%vm-call-closure-sync (%vm-method-function (car around-methods)) state args))
       (t
        (dolist (method before-methods)
          (%vm-call-closure-sync (%vm-method-function method) state args))
         (let ((result (if primary-methods
                           (%vm-call-closure-sync (%vm-method-function (car primary-methods))
                                                  state args
                                                  :method-context (list gf-ht primary-methods args))
                           (and default (funcall default)))))
          (dolist (method (reverse after-methods))
            (%vm-call-closure-sync (%vm-method-function method) state args))
          result)))))

(defun %vm-direct-primary-method-p (gf-ht key)
  "Return T when GF-HT has a primary method registered exactly for KEY."
  (let ((methods-ht (and (hash-table-p gf-ht) (gethash :__methods__ gf-ht))))
    (and methods-ht (gethash key methods-ht) t)))

(defun %vm-class-direct-slot-p (class-ht slot-name)
  "Return T when CLASS-HT directly declares SLOT-NAME."
  (and (hash-table-p class-ht)
       (member slot-name (gethash :__direct-slots__ class-ht) :test #'eq)
       t))

(defun %vm-class-slot-initargs-for-slot (class-ht slot-name)
  "Return all initargs in CLASS-HT metadata that initialize SLOT-NAME."
  (let ((result nil))
    (dolist (entry (and (hash-table-p class-ht) (gethash :__initargs__ class-ht)))
      (when (eq (cdr entry) slot-name)
        (pushnew (car entry) result :test #'eq)))
    (nreverse result)))

(defun %vm-cpl-class-tables (class-ht state)
  "Return CLASS-HT followed by its CPL class tables, most-specific first."
  (let ((cpl (and (hash-table-p class-ht) (gethash :__cpl__ class-ht))))
    (if cpl
        (loop for class-name in cpl
              for cpl-class = (gethash class-name (vm-class-registry state))
              when cpl-class collect cpl-class)
        (and (hash-table-p class-ht) (list class-ht)))))

(defun %vm-first-slot-metadata (classes slot-name key &optional default)
  "Return the first SLOT-NAME metadata value for KEY across CLASSES."
  (dolist (class classes default)
    (when (%vm-class-direct-slot-p class slot-name)
      (let ((entry (assoc slot-name (gethash key class))))
        (when entry
          (return (cdr entry)))))))

(defun %vm-effective-slot-location (class-ht slot-name)
  "Return SLOT-NAME's zero-based effective slot location in CLASS-HT, or NIL."
  (cdr (assoc slot-name
              (and (hash-table-p class-ht)
                   (gethash :__slot-locations__ class-ht))
              :test #'eq)))

(defun %vm-compute-effective-slot-definition (class-ht slot-name state)
  "Build effective slot-definition metadata for SLOT-NAME in CLASS-HT."
  (unless (hash-table-p class-ht)
    (error "compute-effective-slot-definition: ~S is not a class" class-ht))
  (unless (member slot-name (gethash :__slots__ class-ht) :test #'eq)
    (error "compute-effective-slot-definition: class ~S has no slot ~S"
           (gethash :__name__ class-ht) slot-name))
  (let* ((classes (remove-if-not (lambda (class)
                                   (%vm-class-direct-slot-p class slot-name))
                                 (%vm-cpl-class-tables class-ht state)))
         (slot (make-hash-table :test #'eq))
         (initform-present-p nil)
         (initform (%vm-first-slot-metadata classes slot-name :__initforms__ nil))
         (type (%vm-first-slot-metadata classes slot-name :__slot-types__ t))
         (allocation :instance)
         (initargs nil))
    (dolist (class classes)
      (when (%vm-class-direct-slot-p class slot-name)
        (setf allocation
              (if (member slot-name (gethash :__class-slots__ class) :test #'eq)
                  :class
                  :instance))
        (return)))
    (dolist (class classes)
      (dolist (initarg (%vm-class-slot-initargs-for-slot class slot-name))
        (pushnew initarg initargs :test #'eq)))
    (dolist (class classes)
      (when (and (not initform-present-p)
                 (%vm-class-direct-slot-p class slot-name)
                 (assoc slot-name (gethash :__initforms__ class)))
        (setf initform-present-p t)))
    (setf (gethash :name slot) slot-name
          (gethash :initargs slot) (nreverse initargs)
          (gethash :type slot) (or type t)
          (gethash :allocation slot) allocation
          (gethash :initfunction slot) (let ((value initform)) (lambda () value))
          (gethash :readers slot) nil
          (gethash :writers slot) nil
          (gethash :location slot) (%vm-effective-slot-location class-ht slot-name))
    (when initform-present-p
      (setf (gethash :initform slot) initform))
    slot))

(defun %vm-raw-allocate-instance (class-ht initarg-regs state)
  "Allocate and initialize raw storage for CLASS-HT without protocol dispatch."
  (let* ((slot-names       (gethash :__slots__            class-ht))
         (standard-layout-p (not (%vm-class-nonstandard-metaclass-p class-ht state)))
         (obj-ht           (if standard-layout-p
                               (make-array (1+ (length slot-names))
                                           :initial-element *unbound-slot-marker*)
                               (make-hash-table :test #'eq)))
         (initarg-map      (gethash :__initargs__         class-ht))
         (initform-values  (gethash :__initforms__        class-ht))
         (default-initargs (gethash :__default-initargs__ class-ht))
         (class-slots      (gethash :__class-slots__      class-ht))
         (provided-keys    (remove :allow-other-keys (mapcar #'car initarg-regs) :test #'eq)))
    (if standard-layout-p
        (setf (aref obj-ht 0) class-ht)
        (setf (gethash :__class__ obj-ht) class-ht))
    (%vm-validate-initargs initarg-regs initarg-map state)
    (dolist (slot-name slot-names)
      (unless (member slot-name class-slots :test #'eq)
        (let ((initform-entry (assoc slot-name initform-values)))
          (%vm-raw-slot-write class-ht class-slots obj-ht slot-name
                              (if initform-entry (cdr initform-entry) nil)))))
    (loop for (initarg-key . default-value) in default-initargs
          unless (member initarg-key provided-keys)
            do (%vm-apply-initarg initarg-key default-value initarg-map class-slots class-ht obj-ht))
    (loop for (initarg-key . value-reg) in initarg-regs
          do (%vm-apply-initarg initarg-key (vm-reg-get state value-reg)
                                initarg-map class-slots class-ht obj-ht))
    obj-ht))

(defun %vm-initarg-values (initarg-regs state)
  "Return INITARG-REGS as alternating key/value arguments."
  (loop for (initarg-key . value-reg) in initarg-regs
        append (list initarg-key (vm-reg-get state value-reg))))

(defun %vm-call-allocation-protocol (class-ht initarg-regs state)
  "Allocate CLASS-HT via custom metaclass hooks when present, otherwise raw allocate."
  (let* ((metaclass (%vm-class-effective-metaclass class-ht state))
         (metaclass-name (and (hash-table-p metaclass) (gethash :__name__ metaclass)))
         (initarg-values (%vm-initarg-values initarg-regs state))
         (allocate-gf (%vm-global-generic-function state 'allocate-instance))
         (initialize-gf (%vm-global-generic-function state 'initialize-instance))
         (obj-ht (if (and allocate-gf
                          metaclass-name
                          (%vm-direct-primary-method-p allocate-gf metaclass-name))
                     (%vm-call-generic-sync allocate-gf state
                                           (append (list class-ht) initarg-values))
                     (%vm-raw-allocate-instance class-ht initarg-regs state))))
    (when initialize-gf
      (%vm-call-generic-sync initialize-gf state
                             (append (list obj-ht) initarg-values)
                             :default (lambda () obj-ht)))
    obj-ht))

(defun %vm-raw-slot-read (class-ht class-slots obj-ht slot-name)
  "Read SLOT-NAME directly from OBJ-HT/CLASS-HT with standard error behavior."
  (if (and class-slots (member slot-name class-slots :test #'eq))
      (%vm-hashlike-gethash slot-name class-ht)
      (let ((all-slots  (when class-ht (%vm-hashlike-gethash :__slots__ class-ht)))
            (class-name (when class-ht (%vm-hashlike-gethash :__name__ class-ht))))
        (cond
          ((%vm-vector-instance-p obj-ht)
           (let ((index (%vm-slot-vector-index class-ht slot-name)))
             (cond
               ((and index (< index (length obj-ht)))
                (let ((value (aref obj-ht index)))
                  (if (eq value *unbound-slot-marker*)
                      (error (make-condition 'unbound-slot :name slot-name :instance obj-ht))
                      value)))
               ((and all-slots (member slot-name all-slots :test #'eq))
                (error (make-condition 'unbound-slot :name slot-name :instance obj-ht)))
               (t
                (error "The slot ~S is missing from the object~@[ of class ~S~]"
                       slot-name class-name)))))
          (t
           (let ((obj-storage (%vm-hashlike-storage obj-ht)))
             (unless obj-storage
               (error "The slot ~S is missing from non-object ~S" slot-name obj-ht))
             (multiple-value-bind (value found-p) (gethash slot-name obj-storage)
               (if found-p
                   value
                   ;; For PHP/JS objects that use string keys, try string conversion.
                   ;; This allows $obj->method to find "method" stored as a string key.
                   (let ((str-key (string-downcase (symbol-name slot-name))))
                     (multiple-value-bind (str-val str-found-p)
                         (gethash str-key obj-storage)
                       (if str-found-p
                           str-val
                           (if (and all-slots (member slot-name all-slots :test #'eq))
                               (error (make-condition 'unbound-slot :name slot-name :instance obj-ht))
                               nil))))))))))))

(defun %vm-raw-slot-write (class-ht class-slots obj-ht slot-name value)
  "Write VALUE directly to SLOT-NAME in OBJ-HT/CLASS-HT."
  (if (and class-slots (member slot-name class-slots :test #'eq))
      (%vm-hashlike-sethash slot-name class-ht value)
      (if (%vm-vector-instance-p obj-ht)
          (let ((index (%vm-slot-vector-index class-ht slot-name)))
            (unless (and index (< index (length obj-ht)))
              (error "The slot ~S is missing from the object~@[ of class ~S~]"
                     slot-name (and class-ht (%vm-hashlike-gethash :__name__ class-ht))))
            (setf (aref obj-ht index) value))
          (let ((obj-storage (%vm-hashlike-storage obj-ht)))
            (unless obj-storage
              (error "The slot ~S is missing from non-object ~S" slot-name obj-ht))
            (multiple-value-bind (_ sym-found-p) (gethash slot-name obj-storage)
              (declare (ignore _))
              (if sym-found-p
                  (%vm-hashlike-sethash slot-name obj-ht value)
                  ;; PHP/JS objects may store property names as strings.  Only
                  ;; use that path when the canonical symbol slot is absent.
                  (let ((str-key (string-downcase (symbol-name slot-name))))
                    (multiple-value-bind (_ str-found-p) (gethash str-key obj-storage)
                      (declare (ignore _))
                      (if str-found-p
                          (%vm-hashlike-sethash str-key obj-ht value)
                          (%vm-hashlike-sethash slot-name obj-ht value))))))))))

(defun %vm-raw-slot-boundp (class-ht class-slots obj-ht slot-name)
  "Return whether SLOT-NAME is bound using direct OBJ-HT/CLASS-HT storage."
  (let ((all-slots (when class-ht (gethash :__slots__ class-ht))))
    (cond
      ((and all-slots (not (member slot-name all-slots :test #'eq)))
       nil)
      ((and class-slots (member slot-name class-slots :test #'eq))
        (multiple-value-bind (value found-p) (gethash slot-name class-ht)
          (declare (ignore value))
          (if found-p t nil)))
      ((%vm-vector-instance-p obj-ht)
       (let ((index (%vm-slot-vector-index class-ht slot-name)))
         (and index
              (< index (length obj-ht))
              (not (eq (aref obj-ht index) *unbound-slot-marker*)))))
      (t
        (multiple-value-bind (value found-p) (gethash slot-name obj-ht)
          (declare (ignore value))
          (if found-p t nil))))))

(defun %vm-raw-slot-makunbound (class-ht class-slots obj-ht slot-name)
  "Make SLOT-NAME unbound using direct OBJ-HT/CLASS-HT storage."
  (if (and class-slots (member slot-name class-slots :test #'eq))
      (remhash slot-name class-ht)
      (if (%vm-vector-instance-p obj-ht)
          (let ((index (%vm-slot-vector-index class-ht slot-name)))
            (unless (and index (< index (length obj-ht)))
              (error "The slot ~S is missing from the object~@[ of class ~S~]"
                     slot-name (and class-ht (%vm-hashlike-gethash :__name__ class-ht))))
            (setf (aref obj-ht index) *unbound-slot-marker*))
          (remhash slot-name obj-ht)))
  obj-ht)

(defun %vm-sealed-superclass-name (superclasses registry)
  "Return the first sealed superclass designator in SUPERCLASSES, or NIL."
  (loop for superclass in superclasses
        for class-ht = (if (hash-table-p superclass)
                           superclass
                           (gethash superclass registry))
        when (and (hash-table-p class-ht)
                  (gethash :__sealed__ class-ht))
          return (or (gethash :__name__ class-ht) superclass)))

(defmethod execute-instruction ((inst vm-class-def) state pc labels)
  (declare (ignore labels))
  (let* ((class-ht  (make-hash-table :test #'eq))
         (registry  (vm-class-registry state))
         (supers    (vm-superclasses inst))
         (all-slots (%vm-cdef-collect-slots supers (vm-slot-names inst) registry))
         (all-initargs (%vm-cdef-collect-initargs supers (vm-slot-initargs inst) registry))
         (all-default-initargs (%vm-cdef-collect-default-initargs inst supers registry state))
         (all-slot-types (%vm-cdef-collect-slot-types inst supers registry))
         (all-class-slots (%vm-cdef-collect-class-slots inst supers registry))
         (all-slot-locations (%vm-cdef-slot-locations all-slots))
         (previous-class (gethash (vm-class-name-sym inst) registry))
          (own-initform-values (loop for (slot-name . reg) in (vm-slot-initform-regs inst)
                                     collect (cons slot-name (vm-reg-get state reg))))
          (all-initform-values (%vm-cdef-collect-initforms supers own-initform-values registry)))
    (when (and (hash-table-p previous-class)
               (gethash :__sealed__ previous-class))
      (error "Cannot redefine sealed class ~S" (vm-class-name-sym inst)))
    (let ((sealed-superclass (%vm-sealed-superclass-name supers registry)))
      (when sealed-superclass
        (error "Cannot subclass sealed class ~S" sealed-superclass)))
    (setf (gethash :__name__             class-ht) (vm-class-name-sym inst)
          (gethash :__superclasses__     class-ht) supers
          (gethash :__direct-slots__     class-ht) (vm-slot-names inst)
          (gethash :__slots__            class-ht) all-slots
          (gethash :__initargs__         class-ht) all-initargs
          (gethash :__methods__          class-ht) (make-hash-table :test #'equal)
          (gethash :__eql-index__        class-ht) (make-hash-table :test #'equal)
          (gethash :__satiated__         class-ht) nil
          (gethash :__ic-sites__         class-ht) nil
          (gethash :__initforms__        class-ht) all-initform-values
          (gethash :__default-initargs__ class-ht) all-default-initargs
          (gethash :__slot-types__       class-ht) all-slot-types
          (gethash :__class-slots__      class-ht) all-class-slots
          (gethash :__slot-locations__   class-ht) all-slot-locations
          (gethash :__previous-class__   class-ht) previous-class
          (gethash :__sealed__           class-ht) (if (vm-sealed-p inst) t nil)
          (gethash :__metaclass__        class-ht)
          (if (vm-metaclass-reg inst)
              (vm-reg-get state (vm-metaclass-reg inst))
              'standard-class))
    (let ((metaclass-class (%vm-resolve-class-designator
                            (gethash :__metaclass__ class-ht) state)))
      (when (and metaclass-class
                 (not (%vm-standard-metaclass-p metaclass-class)))
        (setf (gethash :__class__ class-ht) metaclass-class)))
    (%vm-mark-class-obsolete previous-class class-ht)
    (%vm-cdef-init-class-slots class-ht all-class-slots all-initform-values)
    (setf (gethash (vm-class-name-sym inst) registry) class-ht
          (gethash :__cpl__ class-ht)
          (compute-class-precedence-list (vm-class-name-sym inst) registry))
    (vm-reg-set state (vm-dst inst) class-ht)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-make-obj) state pc labels)
  (declare (ignore labels))
  (let* ((class-ht (vm-reg-get state (vm-class-reg inst)))
         (obj-ht (if (%vm-class-nonstandard-metaclass-p class-ht state)
                     (%vm-call-allocation-protocol class-ht (vm-initarg-regs inst) state)
                     (%vm-raw-allocate-instance class-ht (vm-initarg-regs inst) state))))
    (vm-reg-set state (vm-dst inst) obj-ht)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-read) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht    (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (multiple-value-bind (class-ht class-slots) (%vm-class-slots-of obj-ht)
      (let ((raw-reader (lambda () (%vm-raw-slot-read class-ht class-slots obj-ht slot-name))))
        (vm-reg-set state (vm-dst inst)
                    (if (and class-ht (%vm-class-nonstandard-metaclass-p class-ht state))
                        (let ((gf (%vm-global-generic-function state 'slot-value-using-class)))
                          (if gf
                              (%vm-call-generic-sync gf state (list class-ht obj-ht slot-name)
                                                     :default raw-reader)
                              (funcall raw-reader)))
                        (funcall raw-reader)))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-write) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht    (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst))
         (value     (vm-reg-get state (vm-value-reg inst))))
    (multiple-value-bind (class-ht class-slots) (%vm-class-slots-of obj-ht)
      (let ((raw-writer (lambda ()
                          (%vm-raw-slot-write class-ht class-slots obj-ht slot-name value))))
        (if (and class-ht (%vm-class-nonstandard-metaclass-p class-ht state))
            (let ((gf (%vm-global-generic-function state '(setf slot-value-using-class))))
              (if gf
                  (%vm-call-generic-sync gf state (list value class-ht obj-ht slot-name)
                                         :default raw-writer)
                  (funcall raw-writer)))
            (funcall raw-writer))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-boundp) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (multiple-value-bind (class-ht class-slots) (%vm-class-slots-of obj-ht)
      (let ((raw-boundp (lambda ()
                          (%vm-raw-slot-boundp class-ht class-slots obj-ht slot-name))))
        (vm-reg-set state (vm-dst inst)
                    (if (and class-ht (%vm-class-nonstandard-metaclass-p class-ht state))
                        (let ((gf (%vm-global-generic-function state 'slot-boundp-using-class)))
                          (if gf
                              (%vm-call-generic-sync gf state (list class-ht obj-ht slot-name)
                                                     :default raw-boundp)
                              (funcall raw-boundp)))
                        (funcall raw-boundp)))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-makunbound) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (multiple-value-bind (class-ht class-slots) (%vm-class-slots-of obj-ht)
      (let ((raw-makunbound (lambda ()
                              (%vm-raw-slot-makunbound class-ht class-slots obj-ht slot-name))))
        (vm-reg-set state (vm-dst inst)
                    (if (and class-ht (%vm-class-nonstandard-metaclass-p class-ht state))
                        (let ((gf (%vm-global-generic-function state 'slot-makunbound-using-class)))
                          (if gf
                              (%vm-call-generic-sync gf state (list class-ht obj-ht slot-name)
                                                     :default raw-makunbound)
                              (funcall raw-makunbound)))
                        (funcall raw-makunbound)))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-exists-p) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht    (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst))
         (class-ht  (%vm-obj-class-ht obj-ht))
         (slots     (when class-ht (gethash :__slots__ class-ht))))
    (vm-reg-set state (vm-dst inst) (if (and slots (member slot-name slots)) t nil))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-compute-effective-slot-definition) state pc labels)
  (declare (ignore labels))
  (let ((class (vm-reg-get state (vm-lhs inst)))
        (slot-name (vm-reg-get state (vm-rhs inst))))
    (vm-reg-set state (vm-dst inst)
                (%vm-compute-effective-slot-definition class slot-name state))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-register-method) state pc labels)
  (declare (ignore labels))
  (let* ((gf-ht (vm-reg-get state (vm-gf-reg inst)))
         (methods-ht (when (hash-table-p gf-ht) (gethash :__methods__ gf-ht)))
         (eql-index (when (hash-table-p gf-ht) (gethash :__eql-index__ gf-ht)))
         (specializer (vm-method-specializer inst))
         (qualifier (vm-method-qualifier inst))
         (method-closure (vm-reg-get state (vm-method-reg inst))))
    (when methods-ht
      ;; Adding a method after a GF was marked closed for optimization is
      ;; allowed, but it re-opens dispatch and invalidates existing ICs.
      (when (gethash :__satiated__ gf-ht)
        (setf (gethash :__satiated__ gf-ht) nil))
      ;; Create method descriptor with qualifier metadata
      (let ((desc (make-hash-table :test 'eq)))
        (setf (gethash :function desc) method-closure
              (gethash :qualifiers desc) (if (null qualifier) nil (list qualifier))
              (gethash :specializer desc) specializer
              (gethash :gf desc) gf-ht)
        (if (null qualifier)
            (progn
              (setf (gethash specializer methods-ht) desc)
              (when eql-index
                (dolist (eql-key (%vm-extract-eql-specializer-keys specializer))
                  (pushnew method-closure (gethash eql-key eql-index) :test #'eq))))
            (let ((qual-key (intern (format nil "__~A__" (string-upcase (string qualifier)))
                                    :keyword)))
              (unless (gethash qual-key gf-ht)
                (setf (gethash qual-key gf-ht) (make-hash-table :test 'equal)))
              (let ((qual-ht (gethash qual-key gf-ht)))
                (if (eq qualifier :around)
                    (setf (gethash specializer qual-ht) desc)
                    (push desc (gethash specializer qual-ht))))))))
    ;; FR-009: Method registration invalidates monomorphic IC entries.
    (%ic-clear-all-generic-caches state)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-generic-call) state pc labels)
  (let* ((gf-ht (vm-reg-get state (vm-gf-reg inst)))
          (arg-regs (vm-args inst))
          (dst-reg (vm-dst inst)))
    (%ic-lookup-or-dispatch gf-ht state pc arg-regs dst-reg labels inst)))

;;; FR-677: class-name and class-of

(define-vm-unary-instruction vm-class-name-fn :class-name
  "Return the name (symbol) of a class object.")

(defmethod execute-instruction ((inst vm-class-name-fn) state pc labels)
  (declare (ignore labels))
  (let ((class (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (%vm-hashlike-storage class)
                    (or (%vm-hashlike-gethash :__name__ class)
                        (error "class-name: not a class object"))
                    (error "class-name: ~A is not a class" class)))
    (values (1+ pc) nil nil)))

(define-vm-unary-instruction vm-class-of-fn :class-of
  "Return the class object of an instance.")

(defmethod execute-instruction ((inst vm-class-of-fn) state pc labels)
  (declare (ignore labels))
  (let ((obj (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (let ((class (%vm-obj-class-ht obj)))
                  (if class
                      (if (%vm-class-nonstandard-metaclass-p class state)
                          (%vm-class-effective-metaclass class state)
                          class)
                      (error "class-of: ~A has no class" obj))))
    (values (1+ pc) nil nil)))

;;; FR-552: find-class / (setf find-class)

(define-vm-unary-instruction vm-find-class :find-class
  "Look up a class by name in the class registry.")

(defmethod execute-instruction ((inst vm-find-class) state pc labels)
  (declare (ignore labels))
  (let* ((name (vm-reg-get state (vm-src inst)))
         (class (gethash name (vm-class-registry state))))
    (unless class
      ;; Also try global vars as fallback
      (setf class (gethash name (vm-global-vars state))))
    (vm-reg-set state (vm-dst inst) (or class nil))
    (values (1+ pc) nil nil)))
