;;;; macros-compat.lisp — ANSI CL compatibility and system macros
(in-package :cl-cc)

;;; Package System (no-ops in this compiler)

(our-defmacro in-package (name)
  `(quote ,name))

(our-defmacro defpackage (name &rest options)
  (declare (ignore options))
  `(quote ,name))

(our-defmacro export (symbols &optional package)
  (declare (ignore symbols package))
  nil)

;;; Declaration (silently ignored)

(our-defmacro declare (&rest decls)
  (declare (ignore decls))
  nil)

;;; FR-1201: Property List Macros (getf, remf, get-properties)

(our-defmacro getf (plist indicator &optional default)
  "Return the value for INDICATOR in PLIST, or DEFAULT if not found."
  (let ((pl (gensym "PL")) (ind (gensym "IND")) (found (gensym "FOUND")))
    `(let ((,pl ,plist) (,ind ,indicator))
       (let ((,found (member ,ind ,pl)))
         (if ,found (cadr ,found) ,default)))))

(our-defmacro remf (plist indicator)
  "Remove INDICATOR and its value from PLIST. Returns T if found, NIL otherwise."
  (let ((ind (gensym "IND")) (prev (gensym "PREV")) (cur (gensym "CUR"))
        (found (gensym "FOUND")))
    `(let ((,ind ,indicator) (,prev nil) (,cur ,plist) (,found nil))
       (tagbody
         :loop
         (when ,cur
           (cond
             ((eq (car ,cur) ,ind)
              (setq ,found t)
              (if ,prev
                  (rplacd (cdr ,prev) (cddr ,cur))
                  (setq ,plist (cddr ,cur)))
              (go :done))
             (t
              (setq ,prev ,cur)
              (setq ,cur (cddr ,cur))
              (go :loop))))
         :done)
       ,found)))

;;; Scope with Declarations

(our-defmacro locally (&body forms)
  `(progn ,@(remove-if (lambda (f) (and (consp f) (eq (car f) 'declare)))
                       forms)))

;; PROGV (FR-102) — dynamic variable binding
;; Uses vm-progv-enter/vm-progv-exit to save and restore global-vars around body.
(our-defmacro progv (symbols values &body body)
  "Bind SYMBOLS to VALUES dynamically for the duration of BODY."
  (let ((syms-var (gensym "SYMS"))
        (vals-var (gensym "VALS"))
        (saved-var (gensym "SAVED")))
    `(let* ((,syms-var ,symbols)
            (,vals-var ,values)
            (,saved-var (%progv-enter ,syms-var ,vals-var)))
       (unwind-protect
         (progn ,@body)
         (%progv-exit ,saved-var)))))

;;; File I/O

(our-defmacro with-open-file (stream-spec &body body)
  "Bind VAR to an open stream for PATH, execute BODY, then close the stream.
   STREAM-SPEC is (var path &rest open-options)."
  (let* ((var     (first stream-spec))
         (path    (second stream-spec))
         (options (cddr stream-spec)))
    `(let ((,var (open ,path ,@options)))
       (unwind-protect (progn ,@body)
         (close ,var)))))

;;; Warning Output

(our-defmacro warn (fmt &rest args)
  `(progn
     (format t ,(concatenate 'string "~&WARNING: "
                             (if (stringp fmt) fmt "~A"))
             ,@args)
     nil))

;;; Hash Table Utilities

(our-defmacro copy-hash-table (ht)
  (let ((ht-var (gensym "HT"))
        (new-var (gensym "NEW"))
        (k-var   (gensym "K"))
        (v-var   (gensym "V")))
    `(let ((,ht-var ,ht))
       (let ((,new-var (make-hash-table :test (hash-table-test ,ht-var))))
         (maphash (lambda (,k-var ,v-var)
                    (setf (gethash ,k-var ,new-var) ,v-var))
                  ,ht-var)
         ,new-var))))

;;; Type Coercion

(our-defmacro coerce (value type-form)
  (if (and (consp type-form) (eq (car type-form) 'quote))
      (let ((type (second type-form)))
        (cond
          ((and (symbolp type) (member type '(string simple-string base-string)))
           `(coerce-to-string ,value))
          ((eq type 'list)
           `(coerce-to-list ,value))
          ;; vector, simple-vector, (vector ...), (array ...), (simple-array ...)
          ((or (and (symbolp type) (member type '(vector simple-vector)))
               (and (consp type) (member (car type) '(vector simple-array array))))
           `(coerce-to-vector ,value))
          (t `(coerce-to-string ,value))))
      `(coerce-to-string ,value)))

;;; Compile-time Evaluation

;; LOAD-TIME-VALUE — evaluate at compile time, splice in the quoted result.
(our-defmacro load-time-value (form &optional read-only-p)
  (declare (ignore read-only-p))
  `(quote ,(eval form)))

;;; FR-1206: Module/feature system — *features*, *modules*, provide, require

(our-defmacro provide (module-name)
  "Mark MODULE-NAME as loaded by pushing its string name onto *modules*."
  (let ((mod (gensym "MOD")))
    `(let ((,mod (string ,module-name)))
       (pushnew ,mod *modules* :test #'string=)
       ,mod)))

(our-defmacro require (module-name &optional pathnames)
  "Signal a warning if MODULE-NAME is not already in *modules*.
PATHNAMES is accepted for compatibility but ignored."
  (let ((mod (gensym "MOD")))
    `(let ((,mod (string ,module-name)))
       (unless (member ,mod *modules* :test #'string=)
         (warn "Module ~A not loaded" ,mod))
       ,mod)))

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
  (let ((obj-v (gensym "OBJ"))
        (str-v (gensym "STR"))
        (cls-v (gensym "CLS"))
        (mth-v (gensym "MTH")))
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
  (let ((obj-v (gensym "OBJ"))
        (str-v (gensym "STR"))
        (cls-v (gensym "CLS"))
        (slots-v (gensym "SLS")))
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
INITARGS are passed to update-instance-for-different-class (no-op here)."
  (let ((inst-var (gensym "INST"))
        (new-class-var (gensym "NC")))
    `(let* ((,inst-var ,instance)
            (,new-class-var ,new-class))
       (unless (hash-table-p ,new-class-var)
         (error "change-class: new-class must be a class descriptor hash table"))
       (setf (gethash :__class__ ,inst-var) ,new-class-var)
       ,inst-var)))

;;; FR-302: parse-float — not in ANSI CL but requested; implemented via read-from-string

(our-defmacro parse-float (string &optional start end junk-allowed)
  (let ((sv (gensym "SV")) (_e end) (_j junk-allowed))
    (declare (ignore _e _j))
    `(let ((,sv (if ,start (subseq ,string ,start) ,string)))
       (float (read-from-string ,sv)))))

;;; FR-1005: reinitialize-instance and shared-initialize

(our-defmacro reinitialize-instance (instance &rest initargs)
  "Reinitialize INSTANCE using INITARGS, applying any matching slot values.
For our VM hash-table instances, iterates the class's initarg map."
  (let ((inst-v (gensym "INST"))
        (args-v (gensym "ARGS"))
        (class-v (gensym "CLASS"))
        (imap-v (gensym "IMAP"))
        (pair-v (gensym "PAIR"))
        (slot-v (gensym "SLOT")))
    `(let* ((,inst-v ,instance)
            (,args-v (list ,@initargs))
            (,class-v (gethash :__class__ ,inst-v))
            (,imap-v  (when (hash-table-p ,class-v)
                        (gethash :__initargs__ ,class-v))))
       (when (and ,imap-v ,args-v)
         (let ((,pair-v ,args-v))
           (tagbody
            reinit-loop
            (when (and ,pair-v (cdr ,pair-v))
              (let* ((k (car ,pair-v))
                     (v (cadr ,pair-v))
                     (,slot-v (assoc k ,imap-v)))
                (when ,slot-v
                  (setf (gethash (cdr ,slot-v) ,inst-v) v)))
              (setf ,pair-v (cddr ,pair-v))
              (go reinit-loop)))))
       ,inst-v)))

(our-defmacro shared-initialize (instance slot-names &rest initargs)
  "Initialize SLOT-NAMES in INSTANCE from INITARGS; t means all slots.
For our VM hash-table instances, like reinitialize-instance but slot-filtered."
  (let ((inst-v (gensym "INST"))
        (slots-v (gensym "SLOTS"))
        (args-v (gensym "ARGS"))
        (class-v (gensym "CLASS"))
        (imap-v (gensym "IMAP"))
        (pair-v (gensym "PAIR"))
        (slot-v (gensym "SLOT"))
        (sn-v (gensym "SN")))
    `(let* ((,inst-v ,instance)
            (,slots-v ,slot-names)
            (,args-v (list ,@initargs))
            (,class-v (gethash :__class__ ,inst-v))
            (,imap-v  (when (hash-table-p ,class-v)
                        (gethash :__initargs__ ,class-v))))
       (when (and ,imap-v ,args-v)
         (let ((,pair-v ,args-v))
           (tagbody
            si-loop
            (when (and ,pair-v (cdr ,pair-v))
              (let* ((k (car ,pair-v))
                     (v (cadr ,pair-v))
                     (,slot-v (assoc k ,imap-v)))
                (when ,slot-v
                  (let ((,sn-v (cdr ,slot-v)))
                    (when (or (eq ,slots-v t)
                              (member ,sn-v ,slots-v))
                      (setf (gethash ,sn-v ,inst-v) v)))))
              (setf ,pair-v (cddr ,pair-v))
              (go si-loop)))))
       ,inst-v)))

;;; %plist-put — non-destructive plist update (used by setf getf expansion)
(our-defmacro %plist-put (plist indicator value)
  (let ((p (gensym "P")) (result (gensym "R")) (found (gensym "F"))
        (k (gensym "K")) (v (gensym "V")))
    `(let ((,p ,plist) (,v ,value) (,result nil) (,found nil))
       (loop while ,p do
         (let ((,k (car ,p)))
           (if (eq ,k ,indicator)
               (progn (push ,indicator ,result) (push ,v ,result) (setf ,found t))
               (progn (push ,k ,result) (push (cadr ,p) ,result)))
           (setf ,p (cddr ,p))))
       (unless ,found
         (push ,v ,result) (push ,indicator ,result))
       (nreverse ,result))))
