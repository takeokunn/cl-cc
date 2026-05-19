;;;; compile/stdlib-source-clos.lisp — Standard Library Source CLOS/condition extension
;;;;
;;;; Appends CLOS protocol, condition hierarchy, and reader/printer variables
;;;; to *standard-library-source* after stdlib-source-ext.lisp.

(in-package :cl-cc/stdlib)

(setf *standard-library-source*
      (concatenate 'string
        *standard-library-source*
;;; ── CLOS Protocol Functions (FR-379/524/525) ───────────────────────────

    "(defun ensure-generic-function (name &rest options)
       (declare (ignore options))
       name)"

    "(defgeneric shared-initialize (instance slot-names &rest initargs))"
    "(defmethod shared-initialize ((instance t) slot-names &rest initargs)
       (declare (ignore slot-names))
       (let* ((class (and (hash-table-p instance) (gethash :__class__ instance)))
              (initarg-map (and (hash-table-p class) (gethash :__initargs__ class)))
              (class-slots (and (hash-table-p class) (gethash :__class-slots__ class))))
         (loop for rest on initargs by #'cddr
               while (cdr rest)
               do (let ((slot-entry (assoc (car rest) initarg-map)))
                    (when slot-entry
                      (let ((slot-name (cdr slot-entry)))
                        (if (member slot-name class-slots)
                            (setf (gethash slot-name class) (cadr rest))
                            (setf (gethash slot-name instance) (cadr rest)))))))
         instance))"

    "(defgeneric initialize-instance (instance &rest initargs))"
    "(defmethod initialize-instance ((instance t) &rest initargs)
       (apply #'shared-initialize instance t initargs))"

     "(defgeneric allocate-instance (class &rest initargs))"
     "(defmethod allocate-instance ((class t) &rest initargs)
         (declare (ignore initargs))
         (let ((object (make-hash-table :test 'eq)))
           (setf (gethash :__class__ object) class)
           (when (hash-table-p class)
             (let ((class-slots (gethash :__class-slots__ class))
                   (initforms (gethash :__initforms__ class)))
               (dolist (slot-name (gethash :__slots__ class))
                 (unless (member slot-name class-slots)
                   (let ((initform-entry (assoc slot-name initforms)))
                     (setf (gethash slot-name object)
                           (if initform-entry (cdr initform-entry) nil)))))))
           object))"

     "(defgeneric slot-value-using-class (class object slot-name))"
     "(defmethod slot-value-using-class ((class t) object slot-name)
        (let ((class-slots (and (hash-table-p class) (gethash :__class-slots__ class))))
          (if (and class-slots (member slot-name class-slots))
              (gethash slot-name class)
              (multiple-value-bind (value found-p) (gethash slot-name object)
                (if found-p
                    value
                    (let ((all-slots (and (hash-table-p class) (gethash :__slots__ class))))
                      (if (and all-slots (member slot-name all-slots))
                          (slot-unbound class object slot-name)
                          (slot-missing class object slot-name 'slot-value))))))))"

     "(defgeneric (setf slot-value-using-class) (new-value class object slot-name))"
     "(defmethod (setf slot-value-using-class) (new-value (class t) object slot-name)
        (let ((class-slots (and (hash-table-p class) (gethash :__class-slots__ class))))
          (if (and class-slots (member slot-name class-slots))
              (setf (gethash slot-name class) new-value)
              (setf (gethash slot-name object) new-value))))"

     "(defgeneric slot-boundp-using-class (class object slot-name))"
     "(defmethod slot-boundp-using-class ((class t) object slot-name)
        (let ((class-slots (and (hash-table-p class) (gethash :__class-slots__ class)))
              (all-slots (and (hash-table-p class) (gethash :__slots__ class))))
          (if (and all-slots (not (member slot-name all-slots)))
              (slot-missing class object slot-name 'slot-boundp)
              (if (and class-slots (member slot-name class-slots))
                  (multiple-value-bind (value found-p) (gethash slot-name class)
                    (declare (ignore value))
                    (if found-p t nil))
                  (multiple-value-bind (value found-p) (gethash slot-name object)
                    (declare (ignore value))
                    (if found-p t nil))))))"

     "(defgeneric slot-makunbound-using-class (class object slot-name))"
     "(defmethod slot-makunbound-using-class ((class t) object slot-name)
        (let ((class-slots (and (hash-table-p class) (gethash :__class-slots__ class))))
          (if (and class-slots (member slot-name class-slots))
              (remhash slot-name class)
              (remhash slot-name object))
          object))"
        
     "(defgeneric slot-unbound (class instance slot-name))"
    "(defmethod slot-unbound ((class t) (instance t) slot-name)
       (error 'unbound-slot :name slot-name :instance instance))"

    "(defgeneric slot-missing (class object slot-name operation &optional new-value))"
    "(defmethod slot-missing ((class t) object slot-name operation &optional new-value)
       (declare (ignore new-value))
       (error (format nil \"The slot ~A is missing from object ~A of class ~A (operation: ~A)\"
                       slot-name object class operation)))"

    "(defgeneric no-applicable-method (gf &rest args))"
    "(defmethod no-applicable-method ((gf t) &rest args)
       (error (format nil \"No applicable method for ~A with args ~A\" gf args)))"

    "(defgeneric no-next-method (gf method &rest args))"
    "(defmethod no-next-method ((gf t) method &rest args)
       (declare (ignore method))
       (error (format nil \"There is no next method for ~A when called with ~A\" gf args)))"

     "(defgeneric print-object (object stream))"
     "(defmethod print-object ((object t) stream)
        (prin1 object stream))"

     "(defgeneric compute-effective-slot-definition (class slot-name &optional direct-slots))"
     "(defmethod compute-effective-slot-definition ((class t) slot-name &optional direct-slots)
        (declare (ignore direct-slots))
        (let ((slot (make-hash-table :test 'eq))
              (initforms (and (hash-table-p class) (gethash :__initforms__ class)))
              (slot-types (and (hash-table-p class) (gethash :__slot-types__ class)))
              (class-slots (and (hash-table-p class) (gethash :__class-slots__ class)))
              (initargs nil))
          (setf (gethash :name slot) slot-name)
          (let ((entry (assoc slot-name initforms)))
            (when entry
              (setf (gethash :initform slot) (cdr entry))))
          (dolist (entry (and (hash-table-p class) (gethash :__initargs__ class)))
            (when (eq (cdr entry) slot-name)
              (push (car entry) initargs)))
          (setf (gethash :initargs slot) (nreverse initargs))
          (setf (gethash :type slot) (or (cdr (assoc slot-name slot-types)) t))
          (setf (gethash :allocation slot)
                (if (member slot-name class-slots) :class :instance))
          (setf (gethash :initfunction slot)
                (let ((value (gethash :initform slot)))
                  (lambda () value)))
          (setf (gethash :readers slot) nil)
          (setf (gethash :writers slot) nil)
          (setf (gethash :location slot)
                (cdr (assoc slot-name
                            (and (hash-table-p class)
                                 (gethash :__slot-locations__ class)))))
          slot))"

     "(defgeneric slot-definition-location (slot))"
     "(defmethod slot-definition-location ((slot t))
        (if (hash-table-p slot)
            (gethash :location slot)
            nil))"

     "(defgeneric update-instance-for-different-class (previous current &rest initargs))"
     "(defmethod update-instance-for-different-class ((previous t) current &rest initargs)
        (declare (ignore previous))
       (apply #'reinitialize-instance current initargs))"

     "(defgeneric update-instance-for-changed-class (instance added-slots discarded-slots plist &rest initargs))"
     "(defmethod update-instance-for-changed-class ((instance t) added-slots discarded-slots plist &rest initargs)
        (declare (ignore added-slots discarded-slots plist))
        (apply #'reinitialize-instance instance initargs))"

    "(defun update-instance-for-redefined-class (instance added-slots discarded-slots plist &rest initargs)
       (declare (ignore discarded-slots plist))
       (let ((class (gethash :__class__ instance)))
         (loop for rest on initargs by #'cddr
               while (cdr rest)
               do (let* ((key (car rest))
                         (value (cadr rest))
                         (binding (assoc key (gethash :__initargs__ class))))
                    (when binding
                      (let ((slot (cdr binding)))
                        (when (or (null added-slots)
                                  (eq added-slots t)
                                  (member slot added-slots))
                          (setf (gethash slot instance) value))))))
         instance))"

     "(defgeneric compute-applicable-methods (gf args))"
    "(defmethod compute-applicable-methods ((gf t) args)
       (declare (ignore args))
       nil)"

    "(defgeneric find-method (gf qualifiers specializers &optional errorp))"
    "(defmethod find-method ((gf t) qualifiers specializers &optional errorp)
       (let* ((methods-ht (gethash :__methods__ gf))
              (spec-key (if (and (listp specializers) (= (length specializers) 1))
                            (let ((s (first specializers)))
                              (if (symbolp s) (symbol-name s) s))
                            specializers))
              (method (when methods-ht (gethash spec-key methods-ht))))
         (if method
             method
             (if errorp
                 (error (format nil \"No method on ~A with qualifiers ~A and specializers ~A\"
                                gf qualifiers specializers))
                 nil))))"

     "(defgeneric add-method (gf method))"
      "(defmethod add-method ((gf t) method)
         (let ((methods-ht (gethash :__methods__ gf)))
          (when methods-ht
            (setf (gethash (if (functionp method) \"T\" \"T\") methods-ht) method))
           gf))"

      "(defun sealed-class-p (class)
         (if (and (hash-table-p class) (gethash :__sealed__ class)) t nil))"

      "(defgeneric satiated-generic-function-p (gf))"
     "(defmethod satiated-generic-function-p ((gf t))
        (if (and (hash-table-p gf) (gethash :__satiated__ gf))
            t
            nil))"

     "(defgeneric remove-method (gf method))"
    "(defmethod remove-method ((gf t) method)
       (let ((methods-ht (gethash :__methods__ gf)))
         (when methods-ht
           (maphash (lambda (k v)
                      (when (eq v method)
                        (remhash k methods-ht)))
                    methods-ht))
          gf))"

;;; ── MOP Method Protocol ────────────────────────────────────────────────

    "(defun method-qualifiers (method)
       (if (hash-table-p method)
           (gethash :qualifiers method)
           nil))"

    "(defclass standard-method ()
       ((qualifiers :initarg :qualifiers :reader method-qualifiers)
        (specializers :initarg :specializers :reader method-specializers)
        (function :initarg :function :reader method-function)
        (generic-function :initarg :generic-function :reader method-generic-function)
        (lambda-list :initarg :lambda-list :reader method-lambda-list)))"

    "(defclass standard-accessor-method (standard-method) ())"
    "(defclass standard-reader-method (standard-accessor-method) ())"
    "(defclass standard-writer-method (standard-accessor-method) ())"

;;; ── Long-form method combination helpers ─────────────────────────────────

    "(defun call-method (method &rest next-methods)
       (declare (ignore next-methods))
       (let ((fn (if (hash-table-p method)
                     (gethash :function method)
                     method)))
         (unless fn (error \"call-method: not a valid method descriptor\"))
         (funcall fn)))"

    "(defun make-method (form)
       (eval `(lambda () ,form)))"

;;; ── copy-instance (shallow copy) ────────────────────────────────────────

    "(defun copy-instance (instance)
       (cond
         ((hash-table-p instance)
          (let ((class (gethash :__class__ instance))
                (copy (make-hash-table :test 'eq)))
            (unless class
              (error \"copy-instance: ~A is not a CLOS instance\" instance))
            (maphash (lambda (key value)
                       (setf (gethash key copy) value))
                     instance)
            copy))
         ((and (vectorp instance)
               (> (length instance) 0)
               (hash-table-p (aref instance 0)))
          (let ((copy (make-array (length instance))))
            (dotimes (i (length instance))
              (setf (aref copy i) (aref instance i)))
            copy))
         (t
          (error \"copy-instance: ~A is not a CLOS instance\" instance))))"

;;; ── ANSI Condition Type Hierarchy (FR-424) ─────────────────────────────

    "(define-condition serious-condition (condition) ())"
    "(define-condition simple-condition (condition)
       ((format-control :initarg :format-control :reader simple-condition-format-control)
        (format-arguments :initarg :format-arguments :reader simple-condition-format-arguments)))"
    "(define-condition simple-warning (simple-condition warning) ())"
    "(define-condition simple-error (simple-condition error) ())"
    "(define-condition type-error (error)
       ((datum :initarg :datum :reader type-error-datum)
        (expected-type :initarg :expected-type :reader type-error-expected-type)))"
    "(define-condition simple-type-error (simple-condition type-error) ())"
    "(define-condition arithmetic-error (error)
       ((operation :initarg :operation :reader arithmetic-error-operation)
        (operands :initarg :operands :reader arithmetic-error-operands)))"
    "(define-condition division-by-zero (arithmetic-error) ())"
    "(define-condition floating-point-overflow (arithmetic-error) ())"
    "(define-condition floating-point-underflow (arithmetic-error) ())"
    "(define-condition cell-error (error)
       ((name :initarg :name :reader cell-error-name)))"
    "(define-condition unbound-variable (cell-error) ())"
    "(define-condition undefined-function (cell-error) ())"
    "(define-condition unbound-slot (cell-error)
       ((instance :initarg :instance :reader unbound-slot-instance)))"
    "(define-condition control-error (error) ())"
    "(define-condition program-error (error) ())"
    "(define-condition package-error (error)
       ((package :initarg :package :reader package-error-package)))"
    "(define-condition stream-error (error)
       ((stream :initarg :stream :reader stream-error-stream)))"
    "(define-condition end-of-file (stream-error) ())"
    "(define-condition file-error (error)
       ((pathname :initarg :pathname :reader file-error-pathname)))"
    "(define-condition print-not-readable (error)
       ((object :initarg :object :reader print-not-readable-object)))"
    "(define-condition storage-condition (serious-condition) ())"
    "(define-condition parse-error (error) ())"
    "(define-condition reader-error (parse-error stream-error) ())"

    ;; ── FR-535: Reader control variables ─────────────────────────────────────
    "(defun %make-readtable ()
       (let ((readtable (make-hash-table :test 'equal)))
         (setf (gethash :readtable readtable) t)
         (setf (gethash :case readtable) :upcase)
         readtable))"
    "(defun readtablep (object)
       (and (hash-table-p object) (gethash :readtable object)))"
    "(defun %normalize-readtable (readtable)
       (if readtable readtable *readtable*))"
    "(defvar *readtable* (%make-readtable))"
    "(defvar *read-base* 10)"
    "(defvar *read-default-float-format* 'single-float)"
    "(defvar *read-suppress* nil)"
    "(defvar *standard-input* *standard-input*)"
    "(defvar *standard-output* *standard-output*)"
    "(defvar *terminal-io* *terminal-io*)"
    "(defvar *error-output* *error-output*)"
    "(defvar *trace-output* *trace-output*)"
    "(defvar *debug-io* *debug-io*)"
    "(defvar *query-io* *query-io*)"

    ;; ── FR-578: Interactive yes/no query helpers ────────────────────────────
    "(defun %query-answer-normalize (answer)
       (and answer
            (string-downcase
              (string-trim '(#\\Space #\\Tab #\\Newline #\\Return) answer))))"
    "(defun %query-answer-in-list-p (answer choices)
       (if (null choices)
           nil
           (if (string= answer (car choices))
               t
               (%query-answer-in-list-p answer (cdr choices)))))"
    "(defun %query-write-prompt (format-string args)
       (cond
         ((null args) (format *query-io* format-string))
         ((null (cdr args)) (format *query-io* format-string (car args)))
         ((null (cddr args)) (format *query-io* format-string (car args) (cadr args)))
         ((null (cdddr args)) (format *query-io* format-string (car args) (cadr args) (caddr args)))
         (t (write-string format-string *query-io*))))"
    "(defun %query-answer-loop (yes-answers no-answers invalid-help format-string args)
       (loop
         (when format-string
           (%query-write-prompt format-string args)
           (finish-output *query-io*))
         (let ((answer (%query-answer-normalize (read-line *query-io* nil nil))))
           (cond
             ((null answer) (return nil))
             ((%query-answer-in-list-p answer yes-answers) (return t))
             ((%query-answer-in-list-p answer no-answers) (return nil))
             (t
              (format *query-io* \"~&Please answer ~A.~%\" invalid-help)
              (finish-output *query-io*))))))"
    "(defun y-or-n-p (&optional format-string &rest args)
       (%query-answer-loop '(\"y\" \"yes\") '(\"n\" \"no\") \"y or n\" format-string args))"
    "(defun yes-or-no-p (&optional format-string &rest args)
       (%query-answer-loop '(\"yes\") '(\"no\") \"yes or no\" format-string args))"

    ;; ── FR-573: *read-eval* ───────────────────────────────────────────────────
    "(defvar *read-eval* t)"
    ;; ── FR-592: Readtable API compatibility layer ─────────────────────────────
    "(defun copy-readtable (&optional from-readtable to-readtable)
       (let ((source (%normalize-readtable from-readtable))
             (target (if to-readtable to-readtable (%make-readtable))))
         (when (and (readtablep source) (readtablep target))
           (maphash (lambda (key value) (setf (gethash key target) value))
                    source))
         target))"
    "(defun readtable-case (readtable)
       (gethash :case (%normalize-readtable readtable)))"
    "(defun set-readtable-case (readtable case)
       (let ((target (%normalize-readtable readtable)))
         (unless (member case '(:upcase :downcase :preserve :invert))
           (error \"Invalid readtable case\"))
         (setf (gethash :case target) case)
         case))"
    "(defun get-macro-character (char &optional readtable)
       (let ((entry (gethash (list :macro char) (%normalize-readtable readtable))))
         (if entry
             (values (car entry) (cdr entry))
             (values nil nil))))"
    "(defun set-macro-character (char new-function &optional non-terminating-p readtable)
       (setf (gethash (list :macro char) (%normalize-readtable readtable))
             (cons new-function non-terminating-p))
       t)"
    "(defun make-dispatch-macro-character (char &optional non-terminating-p readtable)
       (let ((target (%normalize-readtable readtable)))
         (setf (gethash (list :dispatch char) target) t)
         (set-macro-character char (lambda (&rest args) (declare (ignore args)) nil)
                              non-terminating-p target))
       t)"
    "(defun get-dispatch-macro-character (disp-char sub-char &optional readtable)
       (gethash (list :dispatch disp-char sub-char)
                (%normalize-readtable readtable)))"
    "(defun set-dispatch-macro-character (disp-char sub-char new-function &optional readtable)
       (setf (gethash (list :dispatch disp-char sub-char)
                      (%normalize-readtable readtable))
             new-function)
       t)"
    "(defun set-syntax-from-char (to-char from-char &optional to-readtable from-readtable)
       (let ((target (%normalize-readtable to-readtable))
             (source (%normalize-readtable from-readtable)))
         (multiple-value-bind (macro-function non-terminating-p)
             (get-macro-character from-char source)
           (if macro-function
               (set-macro-character to-char macro-function non-terminating-p target)
               (remhash (list :macro to-char) target))
           (let ((dispatchp (gethash (list :dispatch from-char) source)))
             (if dispatchp
                 (setf (gethash (list :dispatch to-char) target) dispatchp)
                 (remhash (list :dispatch to-char) target)))
           t)))"
    ;; ── FR-570: Printer control variables ────────────────────────────────────
    "(defvar *print-circle* nil)"
    "(defvar *print-gensym* t)"
    "(defvar *print-case* :upcase)"
    ;; ── FR-535: More printer variables ───────────────────────────────────────
    "(defvar *print-array* t)"
    "(defvar *print-readably* nil)"
    ;; ── FR-357: Pretty-printer variables ─────────────────────────────────────
    "(defvar *print-pretty* nil)"
    "(defvar *print-right-margin* nil)"
    "(defvar *print-miser-width* nil)"
    "(defvar *print-lines* nil)"
    "(defvar *print-pprint-dispatch* nil)"

    ;; ── ANSI CL implementation-defined: key mismatch warning ──────────────────
    "(defvar *key-mismatch-warning* nil)"

    ;; ── FR-566: Pathname variables ────────────────────────────────────────────
    "(defvar *default-pathname-defaults* nil)"
    ;; ── FR-574: Load/compile pathname variables ───────────────────────────────
    "(defvar *load-pathname* nil)"
    "(defvar *load-truename* nil)"
    "(defvar *compile-file-pathname* nil)"
    "(defvar *compile-file-truename* nil)"
    "(defvar *load-verbose* t)"
    "(defvar *load-print* nil)"
    "(defvar *compile-verbose* t)"
    "(defvar *compile-print* nil)"
    ;; ── FR-395: Compiler policy ───────────────────────────────────────────────
    "(defvar *compiler-policy* nil)"

      ))
