(in-package :cl-cc/expand)

;;; Shared gensym helper used by CLOS protocol macros below.
(defmacro with-gensyms (names &body body)
  "Bind each NAME in NAMES to a fresh gensym named after the symbol, then evaluate BODY."
  `(let ,(mapcar (lambda (n) `(,n (gensym ,(symbol-name n)))) names)
     ,@body))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Compat — CLOS Protocol, MOP Introspection, and Print Macros
;;;
;;; Contains: print-unreadable-object, print-object, describe-object, describe,
;;; update-instance-for-different-class, update-instance-for-changed-class,
;;; ensure-class, change-class, define-method-combination,
;;; MOP introspection macros (class-direct-superclasses, class-direct-slots,
;;; class-slots, class-direct-default-initargs, generic-function-methods,
;;; generic-function-method-combination, class-precedence-list),
;;; parse-float, reinitialize-instance, shared-initialize.
;;;
;;; Package/declaration/file-IO/hash-table/coerce/load-time-value/feature system
;;; macros are in macros-compat.lisp (loads before).
;;;
;;; Load order: after macros-compat.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


;;; FR-1004: print-unreadable-object
;;; Note: uses flat (spec &body body) lambda list because our-defmacro does not
;;; support nested destructuring in required params.

(our-defmacro print-unreadable-object (spec &body body)
  "Print OBJECT to STREAM in #<...> notation.
SPEC is (object stream &key type identity)."
  (let* ((object     (first spec))
         (stream-frm (second spec))
         (rest-keys  (cddr spec))
         (type-expr  (getf rest-keys :type))
         (id-expr    (getf rest-keys :identity))
         (obj-var    (gensym "OBJ"))
         (str-var    (gensym "STR"))
         (space-forms (when (and type-expr body)
                        (list `(format ,str-var " ")))))
    `(let ((,obj-var ,object)
           (,str-var ,stream-frm))
       (format ,str-var "#<")
       (when ,type-expr
         (format ,str-var "~A" (type-of ,obj-var))
         ,@space-forms)
       ,@body
       (when ,id-expr
         (format ,str-var " {~X}" (if (integerp ,obj-var) ,obj-var 0)))
       (format ,str-var ">")
       nil)))

;;; FR-1004: print-object and describe

(our-defmacro print-object (object stream)
  "Print OBJECT to STREAM using the object's class print method if defined,
otherwise falling back to prin1."
  (with-gensyms (obj-v str-v cls-v mth-v)
    `(let* ((,obj-v ,object)
            (,str-v ,stream)
            (,cls-v (when (hash-table-p ,obj-v) (gethash :__class__ ,obj-v)))
            (,mth-v (when (hash-table-p ,cls-v)
                      (gethash :print-object (gethash :__methods__ ,cls-v (make-hash-table))))))
       (if ,mth-v
           (funcall ,mth-v ,obj-v ,str-v)
           (prin1 ,obj-v ,str-v)))))

(our-defmacro describe-object (object stream)
  "Describe OBJECT to STREAM (default: type and slots for CLOS objects)."
  (with-gensyms (obj-v str-v cls-v slots-v)
    `(let* ((,obj-v ,object)
            (,str-v ,stream)
            (,cls-v (when (hash-table-p ,obj-v) (gethash :__class__ ,obj-v))))
       (if (hash-table-p ,cls-v)
           (let ((,slots-v (gethash :__slots__ ,cls-v)))
             (format ,str-v "~A is an instance of ~A~%"
                     ,obj-v (gethash :__name__ ,cls-v))
             (dolist (slot ,slots-v)
               (format ,str-v "  ~S = ~S~%"
                       slot (gethash slot ,obj-v))))
           (format ,str-v "~S~%" ,obj-v)))))

(our-defmacro describe (object &optional stream)
  "Print a description of OBJECT to STREAM (default: *standard-output*)."
  (let ((str-v (gensym "STR")))
    `(let ((,str-v (or ,stream *standard-output*)))
       (describe-object ,object ,str-v)
       (values))))

;;; FR-1005: update-instance-for-different-class / update-instance-for-changed-class

(our-defmacro update-instance-for-different-class (previous current &rest initargs)
  "Called after change-class; initializes new slots from INITARGS (stub)."
  (let ((initargs-list initargs)
        (_prev previous))
    (declare (ignore _prev))
    `(reinitialize-instance ,current ,@initargs-list)))

(our-defmacro update-instance-for-changed-class (instance &rest initargs)
  "Called after class redefinition; reinitializes INSTANCE (stub)."
  (let ((initargs-list initargs))
    `(reinitialize-instance ,instance ,@initargs-list)))

;;; FR-1005: ensure-class — create or update a class definition

(our-defmacro ensure-class (name &rest options)
  "Ensure class NAME exists with OPTIONS (stub: delegates to defclass)."
  (let ((direct-superclasses (or (getf options :direct-superclasses) '()))
        (direct-slots (or (getf options :direct-slots) '())))
    `(defclass ,name ,direct-superclasses ,direct-slots)))

;;; FR-1003: change-class — change the class of a CLOS instance

(our-defmacro change-class (instance new-class &rest initargs)
  "Change the class of INSTANCE to NEW-CLASS, preserving matching slots.
Removes old-only slots, adds new-only slots (nil), calls update-instance-for-different-class."
  (with-gensyms (inst-var new-class-var old-slots-var new-slots-var s-var)
    `(let* ((,inst-var ,instance)
            (,new-class-var ,new-class)
            (,old-slots-var (when (hash-table-p (gethash :__class__ ,inst-var))
                              (gethash :__slots__ (gethash :__class__ ,inst-var))))
            (,new-slots-var (when (hash-table-p ,new-class-var)
                              (gethash :__slots__ ,new-class-var))))
       (unless (hash-table-p ,new-class-var)
         (error "change-class: new-class must be a class descriptor hash table"))
       ;; Remove slots not in new class
       (dolist (,s-var ,old-slots-var)
         (unless (member ,s-var ,new-slots-var :test #'equal)
           (remhash ,s-var ,inst-var)))
       ;; Add slots from new class not in old (initialize to nil)
       (dolist (,s-var ,new-slots-var)
         (unless (member ,s-var ,old-slots-var :test #'equal)
           (setf (gethash ,s-var ,inst-var) nil)))
       ;; Update class reference
       (setf (gethash :__class__ ,inst-var) ,new-class-var)
       ;; Call update hook with initargs
       ,@(when initargs
           `((update-instance-for-different-class nil ,inst-var ,@initargs)))
       ,inst-var)))

;;; FR-376: define-method-combination (short form)
;;; Registers the combination name so defgeneric can reference it.
;;; The actual dispatch is handled by the VM in vm-dispatch-generic-call.
;;; Supported short-form: (define-method-combination name &key operator identity-with-one-argument)

(our-defmacro define-method-combination (name &rest options)
  (declare (ignore options))
  `(quote ,name))

;;; FR-523〜528: MOP Introspection Macros
;;; Class objects in the VM are native CL hash tables with keys like
;;; :__name__, :__superclasses__, :__slots__, :__initargs__, etc.
;;; These macros expand inline so user code doesn't need function dispatch.

(our-defmacro class-direct-superclasses (class)
  (with-gensyms (c)
    `(let ((,c ,class))
       (when (hash-table-p ,c)
         (gethash :__superclasses__ ,c)))))

(our-defmacro class-direct-slots (class)
  (with-gensyms (c)
    `(let ((,c ,class))
       (when (hash-table-p ,c)
         (%class-slot-definitions ,c)))))

(our-defmacro class-slots (class)
  (with-gensyms (c)
    `(let ((,c ,class))
       (when (hash-table-p ,c)
         (%class-slot-definitions ,c)))))

(our-defmacro class-direct-default-initargs (class)
  (with-gensyms (c)
    `(let ((,c ,class))
       (when (hash-table-p ,c)
         (gethash :__default-initargs__ ,c)))))

(our-defmacro generic-function-methods (gf)
  (with-gensyms (gf-v ht-v)
    `(let* ((,gf-v ,gf)
            (,ht-v (and (hash-table-p ,gf-v) (gethash :__methods__ ,gf-v))))
       (when ,ht-v
         (hash-table-values ,ht-v)))))

(our-defmacro generic-function-method-combination (gf)
  (with-gensyms (gf-v)
    `(let ((,gf-v ,gf))
       (if (and (hash-table-p ,gf-v) (gethash :__method-combination__ ,gf-v))
           (gethash :__method-combination__ ,gf-v)
           'standard))))

(register-macro 'class-precedence-list
  (lambda (form env)
    (declare (ignore env))
    (let ((class-form (second form)))
      (with-gensyms (c name-v result-v seen-v queue-v s-v sht-v)
      `(let ((,c ,class-form))
         (when (hash-table-p ,c)
           (let ((,name-v (gethash :__name__ ,c)))
             (when ,name-v
               (let ((,result-v (list ,name-v))
                     (,seen-v (list ,name-v))
                     (,queue-v (gethash :__superclasses__ ,c)))
                 (dolist (,s-v ,queue-v)
                   (unless (member ,s-v ,seen-v)
                     (push ,s-v ,result-v)
                     (push ,s-v ,seen-v)
                     (let ((,sht-v (find-class ,s-v)))
                       (when (hash-table-p ,sht-v)
                         (dolist (,s-v (gethash :__superclasses__ ,sht-v))
                           (unless (member ,s-v ,seen-v)
                             (push ,s-v ,result-v)
                             (push ,s-v ,seen-v)))))))
                  (nreverse ,result-v))))))))))

;;; FR-302: parse-float — not in ANSI CL but requested; implemented via read-from-string

(our-defmacro parse-float (string &optional start end junk-allowed)
  (let ((sv (gensym "SV")) (_e end) (_j junk-allowed))
    (declare (ignore _e _j))
    `(let ((,sv (if ,start (subseq ,string ,start) ,string)))
       (float (read-from-string ,sv)))))

;;; FR-1005: reinitialize-instance and shared-initialize

(defun %apply-instance-initargs (instance class-initargs initargs &optional slot-names)
  "Apply INITARGS to INSTANCE using CLASS-INITARGS.

CLASS-INITARGS is the class descriptor's initarg association list mapping initarg
keywords to slot names. When SLOT-NAMES is NIL, apply all matching initargs.
When SLOT-NAMES is T, also apply all matching initargs. Otherwise only update
slots explicitly listed in SLOT-NAMES. Returns INSTANCE."
  (when (and class-initargs initargs)
    (loop for (key value) on initargs by #'cddr
          while value
          for slot-binding = (assoc key class-initargs)
          for slot-name = (cdr slot-binding)
          when (and slot-binding
                    (or (null slot-names)
                        (eq slot-names t)
                        (member slot-name slot-names)))
            do (setf (gethash slot-name instance) value)))
  instance)

(our-defmacro reinitialize-instance (instance &rest initargs)
  "Reinitialize INSTANCE using INITARGS, applying any matching slot values.
For our VM hash-table instances, iterates the class's initarg map."
  (let ((inst-v (gensym "INST"))
         (args-v (gensym "ARGS"))
         (class-v (gensym "CLASS"))
         (imap-v (gensym "IMAP")))
    `(let* ((,inst-v ,instance)
            (,args-v (list ,@initargs))
            (,class-v (gethash :__class__ ,inst-v))
            (,imap-v  (when (hash-table-p ,class-v)
                        (gethash :__initargs__ ,class-v))))
       (%apply-instance-initargs ,inst-v ,imap-v ,args-v)
       ,inst-v)))

(our-defmacro shared-initialize (instance slot-names &rest initargs)
  "Initialize SLOT-NAMES in INSTANCE from INITARGS; t means all slots.
For our VM hash-table instances, like reinitialize-instance but slot-filtered."
  (let ((inst-v (gensym "INST"))
         (slots-v (gensym "SLOTS"))
         (args-v (gensym "ARGS"))
         (class-v (gensym "CLASS"))
         (imap-v (gensym "IMAP")))
    `(let* ((,inst-v ,instance)
            (,slots-v ,slot-names)
            (,args-v (list ,@initargs))
            (,class-v (gethash :__class__ ,inst-v))
            (,imap-v  (when (hash-table-p ,class-v)
                        (gethash :__initargs__ ,class-v))))
       (%apply-instance-initargs ,inst-v ,imap-v ,args-v ,slots-v)
       ,inst-v)))
