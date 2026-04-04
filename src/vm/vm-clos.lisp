(in-package :cl-cc)
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

;;; ── CLOS VM instruction defstructs ───────────────────────────────────────

(define-vm-instruction vm-class-def (vm-instruction)
  "Define a class. Creates a class descriptor hash table."
  (dst nil :reader vm-dst)
  (class-name nil :reader vm-class-name-sym)
  (superclasses nil :reader vm-superclasses)
  (slot-names nil :reader vm-slot-names)
  (slot-initargs nil :reader vm-slot-initargs)
  (slot-initform-regs nil :reader vm-slot-initform-regs)
  (default-initarg-regs nil :reader vm-default-initarg-regs)
  (class-slots nil :reader vm-class-slots)
  (:sexp-tag :class-def))

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
  "Dispatch and call a generic function method."
  (dst nil :reader vm-dst)
  (gf-reg nil :reader vm-gf-reg)
  (args nil :reader vm-args)
  (:sexp-tag :generic-call))

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

;;; ── MRO and inheritance helpers ──────────────────────────────────────────

(defun collect-inherited-slots (superclasses registry)
  "Collect slot names from superclasses in MRO order (depth-first, left-to-right).
Returns a list of slot names without duplicates, preserving order."
  (let ((seen (make-hash-table :test #'eq))
        (result nil))
    (labels ((walk (class-names)
               (dolist (cname class-names)
                 (let ((super-ht (gethash cname registry)))
                   (when super-ht
                     (walk (gethash :__superclasses__ super-ht))
                     (dolist (slot (gethash :__slots__ super-ht))
                       (unless (gethash slot seen)
                         (setf (gethash slot seen) t)
                         (push slot result))))))))
      (walk superclasses))
    (nreverse result)))

(defun collect-inherited-initargs (superclasses registry)
  "Collect initarg mappings from superclasses. Later entries override earlier ones."
  (let ((result nil))
    (labels ((walk (class-names)
               (dolist (cname class-names)
                 (let ((super-ht (gethash cname registry)))
                   (when super-ht
                     (walk (gethash :__superclasses__ super-ht))
                     (dolist (entry (gethash :__initargs__ super-ht))
                       (unless (assoc (car entry) result)
                         (push entry result))))))))
      (walk superclasses))
    (nreverse result)))

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

(defun compute-class-precedence-list (class-name registry)
  "Compute the class precedence list for CLASS-NAME using C3 linearization.
Returns a list of class names including CLASS-NAME itself.
Signals an error if the hierarchy has inconsistent orderings.
Algorithm: L(C) = C + merge(L(C1), L(C2), ..., (C1, C2, ...))"
  (labels ((linearize (name)
             (let ((class-ht (gethash name registry)))
               (if (null class-ht)
                   (list name)
                   (let ((supers (gethash :__superclasses__ class-ht)))
                     (if (null supers)
                         (list name)
                         (cons name
                               (%c3-merge
                                (append
                                 (mapcar #'linearize supers)
                                 (list (copy-list supers)))))))))))
    (linearize class-name)))

;;; ── CLOS instruction execution ───────────────────────────────────────────

(defmethod execute-instruction ((inst vm-class-def) state pc labels)
  (declare (ignore labels))
  (let* ((class-ht (make-hash-table :test #'eq))
         (registry (vm-class-registry state))
         (supers (vm-superclasses inst))
         (inherited-slots (collect-inherited-slots supers registry))
         (own-slots (vm-slot-names inst))
         (all-slots (append inherited-slots
                            (remove-if (lambda (s) (member s inherited-slots)) own-slots)))
         (inherited-initargs (collect-inherited-initargs supers registry))
         (own-initargs (vm-slot-initargs inst))
         (all-initargs (append inherited-initargs
                               (remove-if (lambda (e) (assoc (car e) inherited-initargs))
                                          own-initargs)))
         (initform-values (loop for (slot-name . reg) in (vm-slot-initform-regs inst)
                                collect (cons slot-name (vm-reg-get state reg)))))
    ;; Collect default-initargs: own + inherited (own takes precedence)
    (let* ((own-default-initargs
             (loop for (key . reg) in (vm-default-initarg-regs inst)
                   collect (cons key (vm-reg-get state reg))))
           (inherited-default-initargs
             (loop for super in supers
                   for super-ht = (gethash super registry)
                   when super-ht
                     append (gethash :__default-initargs__ super-ht)))
           (all-default-initargs
             (append own-default-initargs
                     (remove-if (lambda (e) (assoc (car e) own-default-initargs))
                                inherited-default-initargs))))
      ;; Collect class-allocated slot names (own + inherited)
      (let* ((own-class-slots (vm-class-slots inst))
             (inherited-class-slots
               (loop for super in supers
                     for super-ht = (gethash super registry)
                     when super-ht
                       append (gethash :__class-slots__ super-ht)))
             (all-class-slots (union own-class-slots inherited-class-slots :test #'eq)))
        (setf (gethash :__name__ class-ht) (vm-class-name-sym inst))
        (setf (gethash :__superclasses__ class-ht) supers)
        (setf (gethash :__slots__ class-ht) all-slots)
        (setf (gethash :__initargs__ class-ht) all-initargs)
         (setf (gethash :__methods__ class-ht) (make-hash-table :test #'equal))
         (setf (gethash :__eql-index__ class-ht) (make-hash-table :test #'equal))
         (setf (gethash :__initforms__ class-ht) initform-values)
        (setf (gethash :__default-initargs__ class-ht) all-default-initargs)
        (setf (gethash :__class-slots__ class-ht) all-class-slots)
        ;; Initialize class-allocated slot values on the class HT itself
        (dolist (slot-name all-class-slots)
          (unless (gethash slot-name class-ht)  ; don't overwrite inherited values
            (let ((initform-entry (assoc slot-name initform-values)))
              (setf (gethash slot-name class-ht)
                    (if initform-entry (cdr initform-entry) nil)))))
        (setf (gethash (vm-class-name-sym inst) registry) class-ht)))
    (setf (gethash :__cpl__ class-ht)
          (compute-class-precedence-list (vm-class-name-sym inst) registry))
    (vm-reg-set state (vm-dst inst) class-ht)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-make-obj) state pc labels)
  (declare (ignore labels))
  (let* ((class-ht (vm-reg-get state (vm-class-reg inst)))
         (obj-ht (make-hash-table :test #'eq))
         (slot-names (gethash :__slots__ class-ht))
         (initarg-map (gethash :__initargs__ class-ht))
         (initform-values (gethash :__initforms__ class-ht))
         (default-initargs (gethash :__default-initargs__ class-ht))
         (class-slots (gethash :__class-slots__ class-ht))
         ;; Collect explicitly provided initarg keys
         (provided-keys (mapcar #'car (vm-initarg-regs inst))))
    (setf (gethash :__class__ obj-ht) class-ht)
    ;; Initialize instance-allocated slots from initforms (skip class-allocated)
    (dolist (slot-name slot-names)
      (unless (member slot-name class-slots :test #'eq)
        (let ((initform-entry (assoc slot-name initform-values)))
          (setf (gethash slot-name obj-ht)
                (if initform-entry (cdr initform-entry) nil)))))
    ;; Apply :default-initargs for any initarg not explicitly provided
    (when default-initargs
      (loop for (initarg-key . default-value) in default-initargs
            unless (member initarg-key provided-keys)
              do (let ((slot-entry (assoc initarg-key initarg-map)))
                   (when slot-entry
                     (let ((slot-name (cdr slot-entry))
                           (target (if (member (cdr slot-entry) class-slots :test #'eq)
                                       class-ht obj-ht)))
                       (setf (gethash slot-name target) default-value))))))
    ;; Apply explicitly provided initargs (override defaults)
    (loop for (initarg-key . value-reg) in (vm-initarg-regs inst)
          for value = (vm-reg-get state value-reg)
          do (let ((slot-entry (assoc initarg-key initarg-map)))
               (when slot-entry
                 (let ((slot-name (cdr slot-entry))
                       (target (if (member (cdr slot-entry) class-slots :test #'eq)
                                   class-ht obj-ht)))
                   (setf (gethash slot-name target) value)))))
    (vm-reg-set state (vm-dst inst) obj-ht)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-read) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst))
         (class-ht (when (hash-table-p obj-ht) (gethash :__class__ obj-ht)))
         (class-slots (when class-ht (gethash :__class-slots__ class-ht))))
    ;; For class-allocated slots, read from the class HT
    (if (and class-slots (member slot-name class-slots :test #'eq))
        (progn
          (vm-reg-set state (vm-dst inst) (gethash slot-name class-ht))
          (values (1+ pc) nil nil))
        (multiple-value-bind (value found-p) (gethash slot-name obj-ht)
          (if found-p
              (vm-reg-set state (vm-dst inst) value)
              ;; Slot key absent: distinguish unbound (was slot-makunbound'd) vs missing
              (let* ((class-ht (gethash :__class__ obj-ht))
                     (class-name (when (hash-table-p class-ht) (gethash :__name__ class-ht)))
                     (all-slots  (when (hash-table-p class-ht) (gethash :__slots__ class-ht))))
                (if (and all-slots (member slot-name all-slots :test #'eq))
                    ;; FR-384: slot exists in class but is unbound in instance — signal unbound-slot
                    (error (make-condition 'unbound-slot
                                           :name slot-name
                                           :instance obj-ht))
                    ;; Slot not in class definition — slot-missing protocol (FR-554)
                    (error "The slot ~S is missing from the object~@[ of class ~S~]"
                           slot-name class-name))))
          (values (1+ pc) nil nil)))))

(defmethod execute-instruction ((inst vm-slot-write) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst))
         (value (vm-reg-get state (vm-value-reg inst)))
         (class-ht (when (hash-table-p obj-ht) (gethash :__class__ obj-ht)))
         (class-slots (when class-ht (gethash :__class-slots__ class-ht))))
    ;; For class-allocated slots, write to the class HT
    (if (and class-slots (member slot-name class-slots :test #'eq))
        (setf (gethash slot-name class-ht) value)
        (setf (gethash slot-name obj-ht) value))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-boundp) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (multiple-value-bind (value found-p) (gethash slot-name obj-ht)
      (declare (ignore value))
      (vm-reg-set state (vm-dst inst) (if found-p t nil))
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-slot-makunbound) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (remhash slot-name obj-ht)
    (vm-reg-set state (vm-dst inst) (vm-reg-get state (vm-obj-reg inst)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-exists-p) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst))
         (class-ht (when (hash-table-p obj-ht) (gethash :__class__ obj-ht)))
         (slots (when class-ht (gethash :__slots__ class-ht))))
    (vm-reg-set state (vm-dst inst)
                (if (and slots (member slot-name slots)) t nil))
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
      (if (null qualifier)
          ;; Primary method — store directly (backward compatible)
          (progn
            (setf (gethash specializer methods-ht) method-closure)
            (when eql-index
              (dolist (eql-key (%vm-extract-eql-specializer-keys specializer))
                (pushnew method-closure (gethash eql-key eql-index) :test #'eq))))
          ;; Qualified method (:before, :after, :around) — store in sub-table
          (let ((qual-key (intern (format nil "__~A__" (string-upcase (string qualifier)))
                                  :keyword)))
            (unless (gethash qual-key gf-ht)
              (setf (gethash qual-key gf-ht) (make-hash-table :test 'equal)))
            (let ((qual-ht (gethash qual-key gf-ht)))
              (if (eq qualifier :around)
                  ;; Around: single method per specializer
                  (setf (gethash specializer qual-ht) method-closure)
                  ;; Before/after: accumulate methods in a list
                  (push method-closure (gethash specializer qual-ht)))))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-generic-call) state pc labels)
  (let* ((gf-ht (vm-reg-get state (vm-gf-reg inst)))
         (arg-regs (vm-args inst))
         (dst-reg (vm-dst inst)))
    (vm-dispatch-generic-call gf-ht state pc arg-regs dst-reg labels)))

;;; FR-677: class-name and class-of

(define-vm-unary-instruction vm-class-name-fn :class-name
  "Return the name (symbol) of a class object.")

(defmethod execute-instruction ((inst vm-class-name-fn) state pc labels)
  (declare (ignore labels))
  (let ((class (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (if (hash-table-p class)
                    (or (gethash :__name__ class)
                        (error "class-name: not a class object"))
                    (error "class-name: ~A is not a class" class)))
    (values (1+ pc) nil nil)))

(define-vm-unary-instruction vm-class-of-fn :class-of
  "Return the class object of an instance.")

(defmethod execute-instruction ((inst vm-class-of-fn) state pc labels)
  (declare (ignore labels))
  (let ((obj (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (cond
                  ((and (hash-table-p obj) (gethash :__class__ obj))
                   (gethash :__class__ obj))
                  (t (error "class-of: ~A has no class" obj))))
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
