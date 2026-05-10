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

    "(defgeneric initialize-instance (instance &rest initargs))"
    "(defmethod initialize-instance ((instance t) &rest initargs)
       (declare (ignore initargs))
       instance)"

    "(defgeneric allocate-instance (class &rest initargs))"
    "(defmethod allocate-instance ((class t) &rest initargs)
       (declare (ignore initargs))
       (make-instance class))"

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

    "(defgeneric remove-method (gf method))"
    "(defmethod remove-method ((gf t) method)
       (let ((methods-ht (gethash :__methods__ gf)))
         (when methods-ht
           (maphash (lambda (k v)
                      (when (eq v method)
                        (remhash k methods-ht)))
                    methods-ht))
         gf))"

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
