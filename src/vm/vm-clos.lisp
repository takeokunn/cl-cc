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

(defun compute-class-precedence-list (class-name registry)
  "Compute the class precedence list for CLASS-NAME (depth-first, left-to-right).
Returns a list of class names including CLASS-NAME itself."
  (let ((seen (make-hash-table :test #'eq))
        (result nil))
    (labels ((walk (name)
               (unless (gethash name seen)
                 (setf (gethash name seen) t)
                 (push name result)
                 (let ((class-ht (gethash name registry)))
                   (when class-ht
                     (dolist (super (gethash :__superclasses__ class-ht))
                       (walk super)))))))
      (walk class-name))
    (nreverse result)))

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
    (setf (gethash :__name__ class-ht) (vm-class-name-sym inst))
    (setf (gethash :__superclasses__ class-ht) supers)
    (setf (gethash :__slots__ class-ht) all-slots)
    (setf (gethash :__initargs__ class-ht) all-initargs)
    (setf (gethash :__methods__ class-ht) (make-hash-table :test #'equal))
    (setf (gethash :__initforms__ class-ht) initform-values)
    (setf (gethash (vm-class-name-sym inst) registry) class-ht)
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
         (initform-values (gethash :__initforms__ class-ht)))
    (setf (gethash :__class__ obj-ht) class-ht)
    (dolist (slot-name slot-names)
      (let ((initform-entry (assoc slot-name initform-values)))
        (setf (gethash slot-name obj-ht)
              (if initform-entry (cdr initform-entry) nil))))
    (loop for (initarg-key . value-reg) in (vm-initarg-regs inst)
          for value = (vm-reg-get state value-reg)
          do (let ((slot-entry (assoc initarg-key initarg-map)))
               (when slot-entry
                 (setf (gethash (cdr slot-entry) obj-ht) value))))
    (vm-reg-set state (vm-dst inst) obj-ht)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-read) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (multiple-value-bind (value found-p) (gethash slot-name obj-ht)
      (unless found-p
        (error "Slot ~S not found in object" slot-name))
      (vm-reg-set state (vm-dst inst) value)
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-slot-write) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst))
         (value (vm-reg-get state (vm-value-reg inst))))
    (setf (gethash slot-name obj-ht) value)
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
         (specializer (vm-method-specializer inst))
         (method-closure (vm-reg-get state (vm-method-reg inst))))
    (when methods-ht
      (setf (gethash specializer methods-ht) method-closure))
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
