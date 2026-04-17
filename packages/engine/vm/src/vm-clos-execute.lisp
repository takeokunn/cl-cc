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
         (provided-keys (remove :allow-other-keys (mapcar #'car (vm-initarg-regs inst)) :test #'eq)))
    (setf (gethash :__class__ obj-ht) class-ht)
    (%vm-validate-initargs (vm-initarg-regs inst) initarg-map state)
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
