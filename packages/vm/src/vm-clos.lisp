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
        (dolist (slot (sb-mop:class-slots class) copy)
           (let ((slot-name (sb-mop:slot-definition-name slot)))
           (when (slot-boundp instance slot-name)
             (setf (slot-value copy slot-name)
                   (slot-value instance slot-name)))))))
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

;;; ── CLOS instruction execution ───────────────────────────────────────────
