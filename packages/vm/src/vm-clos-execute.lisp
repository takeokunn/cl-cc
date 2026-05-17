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


(defun %vm-cdef-collect-slots (supers own-slots registry)
  "Merge inherited and own slot names; inherited slots come first."
  (let ((inherited (collect-inherited-slots supers registry)))
    (append inherited (remove-if (lambda (s) (member s inherited)) own-slots))))

(defun %vm-cdef-collect-initargs (supers own-initargs registry)
  "Merge inherited and own initarg→slot alist; inherited entries come first."
  (let ((inherited (collect-inherited-initargs supers registry)))
    (append inherited
            (remove-if (lambda (e) (assoc (car e) inherited)) own-initargs))))

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

(defun %vm-update-obsolete-instance (obj-ht)
  "Migrate OBJ-HT from an obsolete class descriptor to its replacement, if any."
  (let* ((old-class (and (hash-table-p obj-ht) (gethash :__class__ obj-ht)))
         (new-class (and (hash-table-p old-class)
                         (gethash :__obsolete__ old-class)
                         (gethash :__replacement-class__ old-class))))
    (when (hash-table-p new-class)
      (let ((old-slots (gethash :__slots__ old-class))
            (new-slots (gethash :__slots__ new-class)))
        (dolist (slot old-slots)
          (unless (member slot new-slots :test #'eq)
            (remhash slot obj-ht)))
        (dolist (slot new-slots)
          (multiple-value-bind (value found-p) (gethash slot obj-ht)
            (declare (ignore value))
            (unless found-p
              (setf (gethash slot obj-ht) nil))))
        (setf (gethash :__class__ obj-ht) new-class)))
    obj-ht))

(defun %vm-obj-class-ht (obj-ht)
  "Return the class hash-table for an instance OBJ-HT, or NIL."
  (when (hash-table-p obj-ht)
    (%vm-update-obsolete-instance obj-ht)
    (gethash :__class__ obj-ht)))

(defun %vm-class-slots-of (obj-ht)
  "Return (values class-ht class-slots) for class-allocation routing."
  (let ((class-ht (%vm-obj-class-ht obj-ht)))
    (values class-ht (when class-ht (gethash :__class-slots__ class-ht)))))

(defun %vm-apply-initarg (initarg-key value initarg-map class-slots class-ht obj-ht)
  "Write VALUE to the slot for INITARG-KEY, routing class-allocated slots to CLASS-HT."
  (let ((slot-entry (assoc initarg-key initarg-map)))
    (when slot-entry
      (let* ((slot-name (cdr slot-entry))
             (target (if (member slot-name class-slots :test #'eq) class-ht obj-ht)))
        (setf (gethash slot-name target) value)))))

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
         (previous-class (gethash (vm-class-name-sym inst) registry))
         (initform-values (loop for (slot-name . reg) in (vm-slot-initform-regs inst)
                                 collect (cons slot-name (vm-reg-get state reg)))))
    (setf (gethash :__name__             class-ht) (vm-class-name-sym inst)
          (gethash :__superclasses__     class-ht) supers
          (gethash :__direct-slots__     class-ht) (vm-slot-names inst)
          (gethash :__slots__            class-ht) all-slots
          (gethash :__initargs__         class-ht) all-initargs
          (gethash :__methods__          class-ht) (make-hash-table :test #'equal)
          (gethash :__eql-index__        class-ht) (make-hash-table :test #'equal)
          (gethash :__initforms__        class-ht) initform-values
          (gethash :__default-initargs__ class-ht) all-default-initargs
          (gethash :__slot-types__       class-ht) all-slot-types
          (gethash :__class-slots__      class-ht) all-class-slots
          (gethash :__previous-class__   class-ht) previous-class
          (gethash :__metaclass__        class-ht)
          (if (vm-metaclass-reg inst)
              (vm-reg-get state (vm-metaclass-reg inst))
              'standard-class))
    (%vm-mark-class-obsolete previous-class class-ht)
    (%vm-cdef-init-class-slots class-ht all-class-slots initform-values)
    (setf (gethash (vm-class-name-sym inst) registry) class-ht
          (gethash :__cpl__ class-ht)
          (compute-class-precedence-list (vm-class-name-sym inst) registry))
    (vm-reg-set state (vm-dst inst) class-ht)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-make-obj) state pc labels)
  (declare (ignore labels))
  (let* ((class-ht        (vm-reg-get state (vm-class-reg inst)))
         (obj-ht          (make-hash-table :test #'eq))
         (slot-names      (gethash :__slots__            class-ht))
         (initarg-map     (gethash :__initargs__         class-ht))
         (initform-values (gethash :__initforms__        class-ht))
         (default-initargs (gethash :__default-initargs__ class-ht))
         (class-slots     (gethash :__class-slots__      class-ht))
         (provided-keys   (remove :allow-other-keys (mapcar #'car (vm-initarg-regs inst)) :test #'eq)))
    (setf (gethash :__class__ obj-ht) class-ht)
    (%vm-validate-initargs (vm-initarg-regs inst) initarg-map state)
    (dolist (slot-name slot-names)
      (unless (member slot-name class-slots :test #'eq)
        (let ((initform-entry (assoc slot-name initform-values)))
          (setf (gethash slot-name obj-ht) (if initform-entry (cdr initform-entry) nil)))))
    (loop for (initarg-key . default-value) in default-initargs
          unless (member initarg-key provided-keys)
            do (%vm-apply-initarg initarg-key default-value initarg-map class-slots class-ht obj-ht))
    (loop for (initarg-key . value-reg) in (vm-initarg-regs inst)
          do (%vm-apply-initarg initarg-key (vm-reg-get state value-reg) initarg-map class-slots class-ht obj-ht))
    (vm-reg-set state (vm-dst inst) obj-ht)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-read) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht    (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (multiple-value-bind (class-ht class-slots) (%vm-class-slots-of obj-ht)
      (if (and class-slots (member slot-name class-slots :test #'eq))
          (vm-reg-set state (vm-dst inst) (gethash slot-name class-ht))
          (multiple-value-bind (value found-p) (gethash slot-name obj-ht)
            (if found-p
                (vm-reg-set state (vm-dst inst) value)
                (let ((all-slots  (when class-ht (gethash :__slots__ class-ht)))
                      (class-name (when class-ht (gethash :__name__ class-ht))))
                  (if (and all-slots (member slot-name all-slots :test #'eq))
                      (error (make-condition 'unbound-slot :name slot-name :instance obj-ht)) ; FR-384
                      (error "The slot ~S is missing from the object~@[ of class ~S~]" ; FR-554
                             slot-name class-name)))))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-write) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht    (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst))
         (value     (vm-reg-get state (vm-value-reg inst))))
    (multiple-value-bind (class-ht class-slots) (%vm-class-slots-of obj-ht)
      (if (and class-slots (member slot-name class-slots :test #'eq))
          (setf (gethash slot-name class-ht) value)
          (setf (gethash slot-name obj-ht) value)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-boundp) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (multiple-value-bind (class-ht class-slots) (%vm-class-slots-of obj-ht)
      (let ((all-slots (when class-ht (gethash :__slots__ class-ht))))
        (cond
          ((and all-slots (not (member slot-name all-slots :test #'eq)))
           (vm-reg-set state (vm-dst inst) nil))
          ((and class-slots (member slot-name class-slots :test #'eq))
           (multiple-value-bind (value found-p) (gethash slot-name class-ht)
             (declare (ignore value))
             (vm-reg-set state (vm-dst inst) (if found-p t nil))))
          (t
           (multiple-value-bind (value found-p) (gethash slot-name obj-ht)
             (declare (ignore value))
             (vm-reg-set state (vm-dst inst) (if found-p t nil)))))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-makunbound) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (multiple-value-bind (class-ht class-slots) (%vm-class-slots-of obj-ht)
      (if (and class-slots (member slot-name class-slots :test #'eq))
          (remhash slot-name class-ht)
          (remhash slot-name obj-ht)))
    (vm-reg-set state (vm-dst inst) (vm-reg-get state (vm-obj-reg inst)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-exists-p) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht    (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst))
         (class-ht  (%vm-obj-class-ht obj-ht))
         (slots     (when class-ht (gethash :__slots__ class-ht))))
    (vm-reg-set state (vm-dst inst) (if (and slots (member slot-name slots)) t nil))
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
          ;; Primary method — store directly under its canonical specializer key
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
    ;; FR-009: Invalidate all IC caches for this GF
    (incf (gethash '__ic-gen__ gf-ht 0))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-generic-call) state pc labels)
  (let* ((gf-ht (vm-reg-get state (vm-gf-reg inst)))
         (arg-regs (vm-args inst))
         (dst-reg (vm-dst inst)))
    ;; FR-009: Check monomorphic inline cache before full dispatch
    (let ((cache (vm-ic-cache inst))
          (gf-gen (gethash '__ic-gen__ gf-ht 0)))
      (when (and cache (consp cache) (>= (length cache) 4)
                 (eql (nth 3 cache) gf-gen)
                 (equal (nth 0 cache)
                        (mapcar (lambda (r) (vm-classify-arg (vm-reg-get state r) state))
                                arg-regs)))
        (return-from execute-instruction
          (let* ((method (nth 1 cache))
                 (methods (nth 2 cache))
                 (vals (mapcar (lambda (r) (vm-reg-get state r)) arg-regs)))
            (vm-push-call-frame state (1+ pc) dst-reg)
            (push (list gf-ht methods vals) (vm-method-call-stack state))
            (vm-profile-enter-call state (vm-closure-entry-label method))
            (vm-bind-closure-args method state vals)
            (values (vm-label-table-lookup labels (vm-closure-entry-label method))
                    nil nil)))))
    ;; Full dispatch + update cache
    (multiple-value-bind (next-pc halt-p result)
        (vm-dispatch-generic-call gf-ht state pc arg-regs dst-reg labels)
      (declare (ignore halt-p result))
      (let* ((vals (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
             (methods (vm-get-all-applicable-methods gf-ht state vals))
             (primary (car methods)))
        (when primary
          (setf (vm-ic-cache inst)
                (list
                 (mapcar (lambda (r) (vm-classify-arg (vm-reg-get state r) state))
                         arg-regs)
                 primary
                 methods                     ; store full applicable methods for call-next-method
                 (gethash '__ic-gen__ gf-ht 0)))))
      (values next-pc halt-p result))))

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
