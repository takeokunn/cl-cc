(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — CLOS Instruction Definitions and Execution
;;;
;;; Contains: vm-class-def, vm-make-obj, vm-slot-{read,write},
;;; vm-register-method, vm-generic-call, slot predicate instructions,
;;; MRO helpers (collect-inherited-slots, compute-class-precedence-list),
;;; and all corresponding execute-instruction methods.
;;;
;;; Load order: after vm.lisp, before vm-run.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── FR-821/838: Copy and extensible sequence protocol runtime helpers ───

(defclass sequence () ()
  (:documentation "Abstract base class for user-defined sequence-like objects."))

(defgeneric elt (sequence index)
  (:documentation "Return the element of SEQUENCE at INDEX."))

(defgeneric (setf elt) (value sequence index)
  (:documentation "Set the element of SEQUENCE at INDEX to VALUE."))

(defgeneric length (sequence)
  (:documentation "Return the number of elements in SEQUENCE."))

(defgeneric subseq (sequence start &optional end)
  (:documentation "Return a sequence slice from START below END."))

(defgeneric make-sequence-like (sequence size &key initial-element)
  (:documentation "Create a sequence with the same representation family as SEQUENCE."))

(defgeneric sequence-protocol-p (object)
  (:documentation "Return true when OBJECT supports the extensible sequence protocol."))

(defmethod length ((object list))
  (cl:length object))

(defmethod length ((object vector))
  (cl:length object))

(defmethod length ((object string))
  (cl:length object))

(defmethod length ((object sequence))
  (error "LENGTH is not implemented for sequence object ~S" object))

(defmethod elt ((object list) index)
  (cl:nth index object))

(defmethod elt ((object vector) index)
  (cl:aref object index))

(defmethod elt ((object string) index)
  (cl:char object index))

(defmethod elt ((object sequence) index)
  (declare (ignore index))
  (error "ELT is not implemented for sequence object ~S" object))

(defmethod (setf elt) (value (object list) index)
  (setf (cl:nth index object) value))

(defmethod (setf elt) (value (object vector) index)
  (setf (cl:aref object index) value))

(defmethod (setf elt) (value (object string) index)
  (setf (cl:char object index) value))

(defmethod (setf elt) (value (object sequence) index)
  (declare (ignore value index))
  (error "(SETF ELT) is not implemented for sequence object ~S" object))

(defmethod subseq ((object list) start &optional end)
  (cl:subseq object start end))

(defmethod subseq ((object vector) start &optional end)
  (cl:subseq object start end))

(defmethod subseq ((object string) start &optional end)
  (cl:subseq object start end))

(defmethod subseq ((object sequence) start &optional end)
  (let* ((limit (or end (length object)))
         (size (- limit start))
         (copy (make-sequence-like object size)))
    (dotimes (i size copy)
      (setf (elt copy i) (elt object (+ start i))))))

(defmethod make-sequence-like ((object list) size &key initial-element)
  (declare (ignore object))
  (make-list size :initial-element initial-element))

(defmethod make-sequence-like ((object vector) size &key initial-element)
  (declare (ignore object))
  (make-array size :initial-element initial-element))

(defmethod make-sequence-like ((object string) size &key (initial-element #\Space))
  (declare (ignore object))
  (make-string size :initial-element initial-element))

(defmethod make-sequence-like ((object sequence) size &key initial-element)
  (declare (ignore initial-element))
  (error "MAKE-SEQUENCE-LIKE is not implemented for ~S with size ~D" object size))

(defmethod sequence-protocol-p ((object sequence))
  t)

(defmethod sequence-protocol-p ((object list))
  t)

(defmethod sequence-protocol-p ((object vector))
  t)

(defmethod sequence-protocol-p ((object string))
  t)

(defmethod sequence-protocol-p ((object t))
  nil)

(defun copy-instance (instance)
  "Return a shallow copy of a CLOS/VM instance, preserving all slot contents."
  (cond
    ((hash-table-p instance)
     (let ((class (gethash :__class__ instance))
           (copy (make-hash-table :test (hash-table-test instance))))
       (unless class
         (error "copy-instance: ~S is not a VM CLOS instance" instance))
       (maphash (lambda (key value)
                  (setf (gethash key copy) value))
                instance)
       copy))
    ((and (vectorp instance)
          (> (cl:length instance) 0)
          (hash-table-p (cl:aref instance 0)))
     (copy-seq instance))
    ((typep instance 'standard-object)
     (let* ((class (class-of instance))
            (copy (allocate-instance class)))
        #+sbcl
         (dolist (slot (sb-mop:class-slots class) copy)
           (let ((slot-name (sb-mop:slot-definition-name slot)))
           (when (slot-boundp instance slot-name)
             (setf (slot-value copy slot-name)
                   (slot-value instance slot-name)))))
       #-sbcl
       (error "copy-instance for host STANDARD-OBJECT requires slot introspection support")))
    (t
     (error "copy-instance: ~S is not a CLOS instance" instance))))

(defun copy-structure (structure)
  "Return an ANSI shallow copy of STRUCTURE."
  (cond
    ((or (hash-table-p structure)
         (and (vectorp structure)
              (> (cl:length structure) 0)
              (hash-table-p (cl:aref structure 0))))
     (copy-instance structure))
    ((or (listp structure) (vectorp structure) (stringp structure))
     (copy-seq structure))
    ((typep structure 'structure-object)
     (cl:copy-structure structure))
    ((typep structure 'standard-object)
     (copy-instance structure))
    (t
     (error "copy-structure: unsupported object ~S" structure))))

(defun deep-copy (object &optional (seen (make-hash-table :test #'eq)))
  "Recursively copy OBJECT, preserving sharing and cycles reachable by EQ."
  (cond
    ((or (null object) (numberp object) (characterp object) (symbolp object) (functionp object))
     object)
    ((gethash object seen))
    ((consp object)
     (let ((copy (cons nil nil)))
       (setf (gethash object seen) copy)
       (setf (car copy) (deep-copy (car object) seen)
             (cdr copy) (deep-copy (cdr object) seen))
       copy))
    ((hash-table-p object)
     (let ((copy (make-hash-table :test (hash-table-test object))))
       (setf (gethash object seen) copy)
       (maphash (lambda (key value)
                  (setf (gethash (deep-copy key seen) copy)
                        (deep-copy value seen)))
                object)
       copy))
    ((stringp object)
     (copy-seq object))
    ((vectorp object)
     (let ((copy (copy-seq object)))
       (setf (gethash object seen) copy)
       (dotimes (i (cl:length copy) copy)
         (setf (cl:aref copy i) (deep-copy (cl:aref object i) seen)))))
    ((typep object 'standard-object)
     (let ((copy (copy-instance object)))
       (setf (gethash object seen) copy)
        #+sbcl
         (dolist (slot (sb-mop:class-slots (class-of object)) copy)
           (let ((slot-name (sb-mop:slot-definition-name slot)))
           (when (slot-boundp object slot-name)
             (setf (slot-value copy slot-name)
                   (deep-copy (slot-value object slot-name) seen)))))))
    ((typep object 'structure-object)
     (copy-structure object))
    (t object)))

;;; ── CLOS VM instruction defstructs ───────────────────────────────────────

(define-vm-instruction vm-class-def (vm-instruction)
  "Define a class. Creates a class descriptor hash table."
  (dst nil :reader vm-dst)
  (class-name nil :reader vm-class-name-sym)
  (superclasses nil :reader vm-superclasses)
  (slot-names nil :reader vm-slot-names)
  (slot-initargs nil :reader vm-slot-initargs)
  (slot-initform-regs nil :reader vm-slot-initform-regs)
  (slot-types nil :reader vm-slot-types)
  (default-initarg-regs nil :reader vm-default-initarg-regs)
  (class-slots nil :reader vm-class-slots)
  (metaclass-reg nil :reader vm-metaclass-reg)
  (sealed nil :reader vm-sealed-p)
  (:sexp-tag :class-def))

;;; FR-210 Wasm GC mapping: vm-make-obj/vm-slot-read/vm-slot-write are
;;; the VM-level CLOS object operations the Wasm backend maps to struct.new,
;;; struct.get, and struct.set when dedicated GC struct types are enabled.

(define-vm-instruction vm-make-obj (vm-instruction)
  "Create an instance of a class."
  (dst nil :reader vm-dst)
  (class-reg nil :reader vm-class-reg)
  (initarg-regs nil :reader vm-initarg-regs)
  (:sexp-tag :make-obj))

(define-vm-instruction vm-slot-read (vm-instruction)
  "Read a slot value from an object."
  (dst nil :reader vm-dst)
  (obj-reg nil :reader vm-obj-reg)
  (slot-name nil :reader vm-slot-name-sym)
  (:sexp-tag :slot-read))

(define-vm-instruction vm-slot-write (vm-instruction)
  "Write a value to an object slot."
  (obj-reg nil :reader vm-obj-reg)
  (slot-name nil :reader vm-slot-name-sym)
  (value-reg nil :reader vm-value-reg)
  (:sexp-tag :slot-write))

(define-vm-instruction vm-register-method (vm-instruction)
  "Register a method closure on a generic function dispatch table."
  (gf-reg nil :reader vm-gf-reg)
  (specializer nil :reader vm-method-specializer)
  (qualifier nil :reader vm-method-qualifier)
  (method-reg nil :reader vm-method-reg)
  (:sexp-tag :register-method))

(define-vm-instruction vm-generic-call (vm-instruction)
  "Dispatch and call a generic function method.
   FR-009: ic-cache stores (specializer-key . method-closure) for the
   monomorphic inline cache.  FR-058 records cache hit/miss counts, per-type
   frequencies, and may carry a PGO-selected specializer key for a type-check +
   direct-call fast path."
  (dst nil :reader vm-dst)
  (gf-reg nil :reader vm-gf-reg)
  (args nil :reader vm-args)
  (ic-cache nil :accessor vm-ic-cache)
  (ic-hit-count 0 :accessor vm-ic-hit-count :type integer)
  (ic-miss-count 0 :accessor vm-ic-miss-count :type integer)
  (ic-type-counters nil :accessor vm-ic-type-counters)
  (pgo-specializer nil :accessor vm-pgo-specializer)
  (:sexp-tag :generic-call)
  (:sexp-slots dst gf-reg args))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun vm-ic-cache (inst)
    "Return the inline-cache payload stored on VM-GENERIC-CALL INST."
    (vm-generic-call-ic-cache inst))

  (defun (setf vm-ic-cache) (value inst)
    "Set the inline-cache payload stored on VM-GENERIC-CALL INST."
    (setf (vm-generic-call-ic-cache inst) value))

  (defun vm-ic-hit-count (inst)
    "Return the number of inline-cache fast-path hits on VM-GENERIC-CALL INST."
    (vm-generic-call-ic-hit-count inst))

  (defun (setf vm-ic-hit-count) (value inst)
    "Set the inline-cache fast-path hit count on VM-GENERIC-CALL INST."
    (setf (vm-generic-call-ic-hit-count inst) value))

  (defun vm-ic-miss-count (inst)
    "Return the number of inline-cache misses on VM-GENERIC-CALL INST."
    (vm-generic-call-ic-miss-count inst))

  (defun (setf vm-ic-miss-count) (value inst)
    "Set the inline-cache miss count on VM-GENERIC-CALL INST."
    (setf (vm-generic-call-ic-miss-count inst) value))

  (defun vm-ic-type-counters (inst)
    "Return the type-frequency table stored on VM-GENERIC-CALL INST."
    (vm-generic-call-ic-type-counters inst))

  (defun (setf vm-ic-type-counters) (value inst)
    "Set the type-frequency table stored on VM-GENERIC-CALL INST."
    (setf (vm-generic-call-ic-type-counters inst) value))

  (defun vm-pgo-specializer (inst)
    "Return the PGO-selected specializer key stored on VM-GENERIC-CALL INST."
    (vm-generic-call-pgo-specializer inst))

  (defun (setf vm-pgo-specializer) (value inst)
    "Set the PGO-selected specializer key stored on VM-GENERIC-CALL INST."
    (setf (vm-generic-call-pgo-specializer inst) value)))

;;; ── Slot predicate instructions ──────────────────────────────────────────

(define-vm-instruction vm-slot-boundp (vm-instruction)
  "Test if slot SLOT-NAME is bound in object OBJ."
  (dst nil :reader vm-dst)
  (obj-reg nil :reader vm-obj-reg)
  (slot-name-sym nil :reader vm-slot-name-sym)
  (:sexp-tag :slot-boundp)
  (:sexp-slots dst obj-reg slot-name-sym))

(define-vm-instruction vm-slot-makunbound (vm-instruction)
  "Remove slot SLOT-NAME from object OBJ. Returns OBJ."
  (dst nil :reader vm-dst)
  (obj-reg nil :reader vm-obj-reg)
  (slot-name-sym nil :reader vm-slot-name-sym)
  (:sexp-tag :slot-makunbound)
  (:sexp-slots dst obj-reg slot-name-sym))

(define-vm-instruction vm-slot-exists-p (vm-instruction)
  "Test if class of OBJ has a slot named SLOT-NAME."
  (dst nil :reader vm-dst)
  (obj-reg nil :reader vm-obj-reg)
  (slot-name-sym nil :reader vm-slot-name-sym)
  (:sexp-tag :slot-exists-p)
  (:sexp-slots dst obj-reg slot-name-sym))

(define-vm-binary-instruction vm-compute-effective-slot-definition
  :compute-effective-slot-definition
  "Compute an effective slot-definition metadata hash table for CLASS and SLOT-NAME.")

;;; ── MRO and inheritance helpers ──────────────────────────────────────────

(defun %cis-walk (class-names registry seen result-cell)
  "Post-order DFS: accumulate unique slot names into (car RESULT-CELL), skipping SEEN."
  (dolist (cname class-names)
    (let ((super-ht (gethash cname registry)))
      (when super-ht
        (%cis-walk (gethash :__superclasses__ super-ht) registry seen result-cell)
        (dolist (slot (gethash :__slots__ super-ht))
          (unless (gethash slot seen)
            (setf (gethash slot seen) t)
            (push slot (car result-cell))))))))

(defun collect-inherited-slots (superclasses registry)
  "Collect slot names from superclasses in MRO order (depth-first, left-to-right).
Returns a list of slot names without duplicates, preserving order."
  (let ((seen        (make-hash-table :test #'eq))
        (result-cell (list nil)))
    (%cis-walk superclasses registry seen result-cell)
    (nreverse (car result-cell))))

(defun %cia-walk (class-names registry result-cell)
  "Post-order DFS: accumulate unique initarg entries into (car RESULT-CELL)."
  (dolist (cname class-names)
    (let ((super-ht (gethash cname registry)))
      (when super-ht
        (%cia-walk (gethash :__superclasses__ super-ht) registry result-cell)
        (dolist (entry (gethash :__initargs__ super-ht))
          (unless (assoc (car entry) (car result-cell) :test #'eq)
            (push entry (car result-cell))))))))

(defun collect-inherited-initargs (superclasses registry)
  "Collect initarg mappings from all superclasses, preserving first specificity."
  (let ((result-cell (list nil)))
    (%cia-walk superclasses registry result-cell)
    (nreverse (car result-cell))))

(defun %vm-allow-other-keys-p (initarg-regs state)
  "Return true when INITARG-REGS explicitly contains :ALLOW-OTHER-KEYS with a truthy value."
  (let ((entry (assoc :allow-other-keys initarg-regs)))
    (and entry (vm-reg-get state (cdr entry)) t)))

(defun %vm-validate-initargs (initarg-regs initarg-map state)
  "Signal an error for unknown initargs unless :ALLOW-OTHER-KEYS is true.
INITARG-REGS is an alist of keyword to value register. INITARG-MAP maps accepted
keywords to slot names."
  (unless (%vm-allow-other-keys-p initarg-regs state)
    (dolist (entry initarg-regs)
      (let ((key (car entry)))
        (unless (or (eq key :allow-other-keys)
                    (assoc key initarg-map))
          (error "Invalid initarg ~S" key))))))

(defun %c3-merge (linearizations)
  "Merge a list of linearizations using C3 linearization.
A 'good head' is a class that appears only in the head (first position)
of all linearizations where it occurs — never in any tail."
  (let ((result nil))
    (loop
      ;; Remove empty lists
      (setf linearizations (remove nil linearizations))
      (when (null linearizations)
        (return (nreverse result)))
      ;; Find a good head: first element of some list that does not appear
      ;; in the tail (cdr) of any list
      (let ((good-head nil))
        (dolist (lin linearizations)
          (let ((candidate (first lin)))
            (when (notany (lambda (other)
                            (member candidate (rest other) :test #'eq))
                          linearizations)
              (setf good-head candidate)
              (return))))
        (unless good-head
          (error "C3 linearization: inconsistent class precedence for ~{~S~^, ~}"
                 (mapcar #'first linearizations)))
        (push good-head result)
        ;; Remove good-head from all linearizations
        (setf linearizations
              (mapcar (lambda (lin)
                        (if (eq (first lin) good-head)
                            (rest lin)
                            lin))
                      linearizations))))))

(defun %cpl-linearize (name registry)
  "C3 linearization for NAME: list NAME then merge linearizations of its supers.
Algorithm: L(C) = C + merge(L(C1), L(C2), ..., (C1, C2, ...))"
  (let ((class-ht (gethash name registry)))
    (if (null class-ht)
        (list name)
        (let ((supers (gethash :__superclasses__ class-ht)))
          (if (null supers)
              (list name)
              (cons name
                    (%c3-merge
                     (append (mapcar (lambda (s) (%cpl-linearize s registry)) supers)
                             (list (copy-list supers))))))))))

(defun compute-class-precedence-list (class-name registry)
  "Compute the C3 class precedence list for CLASS-NAME using REGISTRY.
As a registry maintenance side effect, record direct-subclass links used by the
MOP CLASS-DIRECT-SUBCLASSES API."
  (let ((class-ht (and registry (gethash class-name registry))))
    (when (hash-table-p class-ht)
      (dolist (superclass (gethash :__superclasses__ class-ht))
        (let ((superclass-ht (gethash superclass registry)))
          (when (hash-table-p superclass-ht)
            (pushnew class-name (gethash :__direct-subclasses__ superclass-ht)
                     :test #'eq))))))
  (%cpl-linearize class-name registry))

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

;;; ── CLOS instruction execution ───────────────────────────────────────────
