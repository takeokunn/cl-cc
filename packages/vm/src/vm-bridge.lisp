(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Host Function Bridge and CLOS Slot Metadata
;;;
;;; Contains: *vm-host-bridge-functions*, vm-register-host-bridge,
;;; hash-table-values, %class-slot-initargs-for-slot, %class-slot-metadata,
;;; %class-slot-definitions, slot-definition-name, slot-definition-initargs,
;;; slot-definition-initform, slot-definition-allocation,
;;; slot-definition-type (and any other slot-def helpers),
;;; and the bootstrap vm-register-host-bridge calls for whitelisted symbols.
;;;
;;; Load order: after vm.lisp (vm-state, vm-generic-function-p).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; Host Function Bridge — whitelist of host CL functions callable from VM
;;;
;;; Only functions explicitly registered here can be resolved via fboundp.
;;; This prevents the VM from accidentally routing calls like `funcall` or
;;; `mapcar` to host CL (which would receive vm-closure-objects it can't handle).

(defvar *vm-host-bridge-functions*
  (make-hash-table :test #'eq #+sbcl :synchronized #+sbcl t)
  "Map of bridgeable symbols to their callable implementation.
All entries must store an explicit function object.
Synchronized: parallel test workers populate this concurrently.")

(defvar *vm-runtime-callables*
  (make-hash-table :test #'equal #+sbcl :synchronized #+sbcl t)
  "Runtime callable registry keyed by exported runtime helper name string.
Synchronized: parallel test workers populate this concurrently.")

(defvar *vm-current-state* nil
  "Dynamically bound to the VM state while invoking host bridge callables.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((package (or (find-package :cffi)
                     (make-package :cffi :use '(:cl)))))
    (dolist (name '("FOREIGN-FUNCALL" "DEFINE-FOREIGN-LIBRARY" "USE-FOREIGN-LIBRARY"))
      (export (intern name package) package))))

(defun vm-register-host-bridge (sym &optional callable)
  "Register SYM as a bridgeable function for the VM.
CALLABLE must be an explicit function object."
  (unless callable
    (error "Cannot register host bridge symbol without explicit callable: ~S" sym))
  (setf (gethash sym *vm-host-bridge-functions*) callable))

(defun vm-register-runtime-callable (name callable)
  "Register a runtime helper CALLABLE under NAME for VM bridge use."
  (unless callable
    (error "Cannot register runtime callable without a function: ~S" name))
  (setf (gethash name *vm-runtime-callables*) callable))

(defun %vm-bridge-symbol-aliases (sym)
  "Return same-name symbols that may have been imported through facade packages."
  (let ((name (and (symbolp sym) (symbol-name sym)))
        (result nil))
    (when name
      (dolist (package-name '(:cl-cc :cl-cc/vm :cl))
        (let ((package (find-package package-name)))
          (when package
            (multiple-value-bind (candidate status)
                (find-symbol name package)
              (declare (ignore status))
              (when candidate
                (pushnew candidate result :test #'eq)))))))
    (nreverse result)))

(defun vm-bridge-callable (sym)
  "Return the callable registered for SYM, or NIL if none is available."
  (labels ((lookup (candidate)
             (let ((entry (gethash candidate *vm-host-bridge-functions*)))
               (and (functionp entry) entry))))
    (or (lookup sym)
        (dolist (candidate (%vm-bridge-symbol-aliases sym) nil)
          (let ((callable (lookup candidate)))
            (when callable
              (return callable)))))))

(defun %vm-runtime-callable (runtime-symbol-name)
  "Resolve a callable from the VM runtime-callable registry by symbol name."
  (or (gethash runtime-symbol-name *vm-runtime-callables*)
      (error "CL-CC/RUNTIME:~A is unavailable" runtime-symbol-name)))

(defun hash-table-values (table)
  "Return TABLE values as a list preserving host iteration order."
  (let ((result nil))
    (maphash (lambda (k v)
                (declare (ignore k))
                (push v result))
              table)
    (nreverse result)))

(defparameter *hash-test-function-alist*
  '((eq     . eq)
    (eql    . eql)
    (equal  . equal)
    (equalp . equalp))
  "(designator-symbol . canonical-designator) pairs for VM hash-table tests.")

(defun hash-test-symbol (test-val)
  "Convert TEST-VAL to a canonical hash-test symbol without probing function cells."
  (if (assoc test-val *hash-test-function-alist* :test #'eq)
      test-val
      'eql))

(defun resolve-hash-test (test-symbol)
  "Convert TEST-SYMBOL to a canonical hash-test designator."
  (or (and (null test-symbol) 'eql)
      (let ((entry (assoc test-symbol *hash-test-function-alist* :test #'eq)))
        (and entry (cdr entry)))
      (error "Unknown hash table test: ~S" test-symbol)))

(defun %make-hash-table-with-test (test-designator)
  "Construct a native CL hash table honoring TEST-DESIGNATOR when supported.
Accepts symbols/functions for EQ/EQL/EQUAL/EQUALP and falls back to EQL." 
  (let* ((sym (cond ((symbolp test-designator) test-designator)
                    ((functionp test-designator)
                     (or (hash-test-symbol test-designator) 'eql))
                    (t 'eql)))
         (test (resolve-hash-test sym)))
    (make-hash-table :test test)))

(defun %class-slot-initargs-for-slot (class slot-name)
  "Return all initargs in CLASS that initialize SLOT-NAME."
  (let ((result nil)
        (imap (and (hash-table-p class) (gethash :__initargs__ class))))
    (dolist (entry imap)
      (when (eq (cdr entry) slot-name)
        (push (car entry) result)))
    (nreverse result)))

(defun %class-slot-metadata (class slot-name)
  "Build a lightweight slot-definition object for SLOT-NAME in CLASS."
  (let ((slot (make-hash-table :test #'eq))
        (initforms (and (hash-table-p class) (gethash :__initforms__ class)))
        (slot-types (and (hash-table-p class) (gethash :__slot-types__ class)))
        (class-slots (and (hash-table-p class) (gethash :__class-slots__ class))))
    (setf (gethash :name slot) slot-name)
    (let ((entry (assoc slot-name initforms)))
      (when entry
        (setf (gethash :initform slot) (cdr entry))))
    (setf (gethash :initargs slot) (%class-slot-initargs-for-slot class slot-name))
    (setf (gethash :type slot) (or (cdr (assoc slot-name slot-types)) t))
    (setf (gethash :allocation slot)
          (if (member slot-name class-slots :test #'eq) :class :instance))
    slot))

(defun %class-slot-definitions-from-key (class key)
  "Return public slot-definition objects for slot-name list stored at KEY."
  (let ((slots (and (hash-table-p class) (gethash key class))))
    (when slots
      (mapcar (lambda (slot-name)
                 (%class-slot-metadata class slot-name))
              slots))))

(defun %class-slot-definitions (class &optional (key :__slots__))
  "Return public slot-definition objects for CLASS using metadata list KEY."
  (%class-slot-definitions-from-key class key))

(defun %class-direct-slot-definitions (class)
  "Return public direct slot-definition objects for CLASS."
  (%class-slot-definitions-from-key class :__direct-slots__))

(defun slot-definition-name (slot)
  "Return the slot-definition name for SLOT.
Current lightweight CLOS metadata may represent slots as symbols; future
representations may use hash tables with structured metadata."
  (cond
    ((symbolp slot) slot)
    ((hash-table-p slot) (gethash :name slot))
    (t nil)))

(defun slot-definition-initform (slot)
  "Return the initform metadata for SLOT when available."
  (if (hash-table-p slot)
      (gethash :initform slot)
      nil))

(defun slot-definition-initargs (slot)
  "Return the initarg metadata for SLOT when available."
  (if (hash-table-p slot)
      (gethash :initargs slot)
      nil))

(defun slot-definition-type (slot)
  "Return the declared slot type for SLOT, defaulting to T."
  (if (hash-table-p slot)
      (or (gethash :type slot) t)
      t))

(defun slot-definition-allocation (slot)
  "Return the allocation mode for SLOT, defaulting to :INSTANCE."
  (if (hash-table-p slot)
      (or (gethash :allocation slot) :instance)
      :instance))

(defun slot-definition-initfunction (slot)
  "Return a zero-argument function yielding SLOT's initform value."
  (let ((initform (slot-definition-initform slot)))
    (lambda () initform)))

(defun class-metaclass (class)
  "Return CLASS metaclass metadata, defaulting to STANDARD-CLASS."
  (if (hash-table-p class)
      (or (gethash :__metaclass__ class) 'standard-class)
      'standard-class))

(defun compute-effective-slot-definition (class slot-name &optional direct-slots)
  "Return a lightweight effective slot-definition for SLOT-NAME in CLASS."
  (or (and (hash-table-p class)
           (member slot-name (gethash :__slots__ class) :test #'eq)
           (%class-slot-metadata class slot-name))
      (find slot-name direct-slots :key #'slot-definition-name :test #'eq)))

(defun %make-instances-obsolete (class)
  "Mark CLASS metadata obsolete. Existing VM instances migrate when possible."
  (let ((class-ht (if (symbolp class) (find-class class nil) class)))
    (when (hash-table-p class-ht)
      (setf (gethash :__obsolete__ class-ht) t))
    class-ht))

(defun generic-function-methods (gf)
  "Return the registered method objects for GF."
  (let ((methods-ht (and (hash-table-p gf) (gethash :__methods__ gf))))
    (when methods-ht
      (loop for value in (hash-table-values methods-ht)
            append (%vm-method-value->list value)))))

(defun generic-function-method-combination (gf)
  "Return the method-combination metadata for GF, defaulting to STANDARD."
  (if (and (hash-table-p gf) (gethash :__method-combination__ gf))
      (gethash :__method-combination__ gf)
      'standard))

(defun %normalize-text-encoding (encoding external-format)
  "Normalize ENCODING / EXTERNAL-FORMAT into the host designator we support."
  (case (or encoding external-format :utf-8)
    (:utf8 :utf-8)
    (otherwise (or encoding external-format :utf-8))))

(defun string-to-octets (string &key encoding external-format)
  "Encode STRING to octets using the requested host encoding."
  (let ((format-designator (%normalize-text-encoding encoding external-format)))
    #+sbcl
    (sb-ext:string-to-octets string :external-format format-designator)
    #-sbcl
    (error "STRING-TO-OCTETS is currently only implemented on SBCL (~S)."
           format-designator)))

(defun %coerce-octets-vector (octets)
  "Return OCTETS as an (UNSIGNED-BYTE 8) vector."
  (cond
    ((typep octets '(array (unsigned-byte 8) (*))) octets)
    ((vectorp octets)
     (let ((result (make-array (length octets)
                               :element-type '(unsigned-byte 8))))
       (dotimes (i (length octets) result)
         (setf (aref result i) (aref octets i)))))
    ((listp octets)
     (make-array (length octets)
                 :element-type '(unsigned-byte 8)
                 :initial-contents octets))
    (t
     (error "OCTETS-TO-STRING expects a list or vector of octets: ~S" octets))))

(defun octets-to-string (octets &key encoding external-format)
  "Decode OCTETS to a string using the requested host encoding."
  (let ((format-designator (%normalize-text-encoding encoding external-format))
        (octet-vector (%coerce-octets-vector octets)))
    #+sbcl
    (sb-ext:octets-to-string octet-vector :external-format format-designator)
    #-sbcl
    (error "OCTETS-TO-STRING is currently only implemented on SBCL (~S)."
           format-designator)))

(defun %utf-8-byte-length (string)
  "Return the UTF-8 byte length of STRING."
  (length (string-to-octets string :encoding :utf-8)))

(defun %vm-file-string-length (stream object)
  "Return the file string length for OBJECT.
For VM integer handles we ignore STREAM and compute the UTF-8 byte width."
  (declare (ignore stream))
  (typecase object
    (character (%utf-8-byte-length (string object)))
    (string (%utf-8-byte-length object))
    (t
     (error "FILE-STRING-LENGTH currently supports character/string objects only: ~S"
             object))))

(defun %foreign-alien-type-form (type return-type-p)
  "Map a minimal CFFI type keyword to an SBCL alien type form."
  (case type
    (:void (if return-type-p 'sb-alien:void
               (error ":VOID is only valid as a foreign return type")))
    (:string 'sb-alien:c-string)
    (:int 'sb-alien:int)
    (:unsigned-int 'sb-alien:unsigned-int)
    (:short 'sb-alien:short)
    (:unsigned-short 'sb-alien:unsigned-short)
    (:long 'sb-alien:long)
    (:unsigned-long 'sb-alien:unsigned-long)
    (:char 'sb-alien:char)
    (:unsigned-char 'sb-alien:unsigned-char)
    (:float 'sb-alien:float)
    (:double 'sb-alien:double)
    (otherwise (error "Unsupported foreign type: ~S" type))))

(defun %parse-foreign-funcall-arguments (arguments)
  "Split CFFI-style type/value pairs and final return type."
  (let ((items arguments)
        (arg-types nil)
        (arg-values nil))
    (loop
      (cond
        ((null items)
         (return (values (nreverse arg-types) (nreverse arg-values) :void)))
        ((null (cdr items))
         (return (values (nreverse arg-types) (nreverse arg-values) (car items))))
        (t
         (push (car items) arg-types)
         (push (cadr items) arg-values)
         (setf items (cddr items)))))))

(defun %foreign-funcall (name &rest arguments)
  "Minimal host-backed CFFI FOREIGN-FUNCALL implementation for the VM."
  #+sbcl
  (multiple-value-bind (arg-types arg-values return-type)
      (%parse-foreign-funcall-arguments arguments)
    (let ((alien-arg-types (mapcar (lambda (type)
                                     (%foreign-alien-type-form type nil))
                                   arg-types))
          (alien-return-type (%foreign-alien-type-form return-type t))
          (foreign-name (etypecase name
                          (string name)
                          (symbol (string-downcase (symbol-name name))))))
      (eval `(sb-alien:alien-funcall
              (sb-alien:extern-alien ,foreign-name
                (function ,alien-return-type ,@alien-arg-types))
              ,@arg-values))))
  #-sbcl
  (declare (ignore name arguments))
  #-sbcl
  (error "FOREIGN-FUNCALL is currently implemented only on SBCL."))

(defun %vm-safe-disassemble (object)
  "Disassemble function designators safely and always return NIL."
  (handler-case
      (progn
        (cond
          ((or (functionp object)
               (and (symbolp object) (cl:fboundp object))
               (and (consp object) (eq (car object) 'lambda)))
           (cl:disassemble object))
          (t
           (format *standard-output*
                   "; disassemble unavailable for non-function designator: ~S~%"
                   object)))
        nil)
    (error (condition)
      (format *trace-output* "~&[cl-cc] disassemble unavailable: ~A~%" condition)
      nil)))

(defun %vm-safe-inspect (object)
  "Describe OBJECT non-interactively and return it when successful."
  (handler-case
      (progn
        (cl:describe object)
        object)
    (error (condition)
      (format *trace-output* "~&[cl-cc] inspect unavailable: ~A~%" condition)
      nil)))

(defun %normalize-query-answer (answer)
  "Trim and lowercase ANSWER for y/n style prompts."
  (and answer
       (string-downcase
        (string-trim '(#\Space #\Tab #\Newline #\Return) answer))))

(defun %vm-query-answer (yes-answers no-answers invalid-help
                         &optional format-string args)
  "Prompt on *QUERY-IO* until a YES/NO answer or EOF is received."
  (loop
    (when format-string
      (apply #'format *query-io* format-string args)
      (finish-output *query-io*))
    (let ((answer (%normalize-query-answer (read-line *query-io* nil nil))))
      (cond
        ((null answer) (return nil))
        ((member answer yes-answers :test #'string=) (return t))
        ((member answer no-answers :test #'string=) (return nil))
        (t
         (format *query-io* "~&Please answer ~A.~%" invalid-help)
         (finish-output *query-io*))))))

(defun %vm-y-or-n-p (&optional format-string &rest args)
  "Read a y/n style answer from *QUERY-IO* without host interactivity."
  (%vm-query-answer '("y" "yes") '("n" "no") "y or n"
                    format-string args))

(defun %vm-yes-or-no-p (&optional format-string &rest args)
  "Read a yes/no answer from *QUERY-IO* without host interactivity."
  (%vm-query-answer '("yes") '("no") "yes or no"
                    format-string args))

;;; Package introspection helpers for do-symbols/do-external-symbols/do-all-symbols
(defun %vm-runtime-package-registry ()
  "Return the runtime package registry when available, else NIL." 
  (when cl-cc/bootstrap:*runtime-package-registry-provider*
    (funcall cl-cc/bootstrap:*runtime-package-registry-provider*)))

(defun %package-symbols (package)
  "Return a list of all symbols accessible in PACKAGE." 
  (when (hash-table-p package)
    (sort (loop for sym being the hash-values of (gethash :symbols package)
                collect sym)
          #'string< :key #'symbol-name)))

(defun %package-external-symbols (package)
  "Return a list of all external symbols in PACKAGE." 
  (when (hash-table-p package)
    (copy-list (gethash :exports package))))

(defun %all-symbols ()
  "Return a list of all symbols in all packages."
  (let ((registry (%vm-runtime-package-registry)))
    (if (hash-table-p registry)
        (sort (loop for descriptor being the hash-values of registry
                    append (loop for sym being the hash-values of (gethash :symbols descriptor)
                                 collect sym))
              #'string< :key #'symbol-name)
        nil)))

(defun vm-install-eval-hooks (eval-hook compile-string-hook)
  "Install the VM eval and compile-string hooks."
  (setf *vm-eval-hook* eval-hook)
  (setf *vm-compile-string-hook* compile-string-hook)
  t)

(defun vm-install-macroexpand-hooks (macroexpand-1-hook macroexpand-hook)
  "Install the VM macroexpansion hooks."
  (setf *vm-macroexpand-1-hook* macroexpand-1-hook)
  (setf *vm-macroexpand-hook* macroexpand-hook)
  t)

(defun vm-install-parse-forms-hook (parse-hook)
  "Install the VM parse-forms hook."
  (setf *vm-parse-forms-hook* parse-hook)
  t)

(eval-when (:load-toplevel :execute)
  (setf cl-cc/bootstrap:*vm-runtime-callable-installer* #'vm-register-runtime-callable)
  (setf cl-cc/bootstrap:*vm-eval-hook-installer* #'vm-install-eval-hooks)
  (setf cl-cc/bootstrap:*vm-macroexpand-hook-installer* #'vm-install-macroexpand-hooks)
  (setf cl-cc/bootstrap:*vm-parse-forms-hook-installer* #'vm-install-parse-forms-hook)
  (when cl-cc/bootstrap:*runtime-vm-callable-register-hook*
    (funcall cl-cc/bootstrap:*runtime-vm-callable-register-hook*)))

;;; FR-607: Documentation retrieval (CL-level, registered by defun expander)
(defun %documentation-symbol-candidates (value)
  "Return same-name symbol candidates for documentation table lookups."
  (if (symbolp value)
      (let ((result (list value))
            (name (symbol-name value)))
        (dolist (package-name '(:cl :cl-cc :cl-cc/vm :cl-cc/expand :cl-user))
          (let ((package (find-package package-name)))
            (when package
              (multiple-value-bind (candidate status)
                  (find-symbol name package)
                (declare (ignore status))
                (when candidate
                  (pushnew candidate result :test #'eq))))))
        (nreverse result))
      (list value)))

(defun %documentation-tables ()
  "Return documentation tables visible from the current VM/expander context."
  (let ((tables nil))
    (labels ((add-table (table)
               (when (hash-table-p table)
                 (pushnew table tables :test #'eq))))
      (add-table *documentation-table*)
      (when *vm-current-state*
        (add-table (gethash '*documentation-table*
                            (vm-global-vars *vm-current-state*)))
        (let* ((package (find-package :cl-cc/expand))
               (symbol (and package (find-symbol "*DOCUMENTATION-TABLE*" package))))
          (when symbol
            (add-table (gethash symbol (vm-global-vars *vm-current-state*))))))
      (let* ((package (find-package :cl-cc/expand))
             (symbol (and package (find-symbol "*DOCUMENTATION-TABLE*" package))))
        (when (and symbol (boundp symbol))
          (add-table (symbol-value symbol))))
      (nreverse tables))))

(defun %ensure-vm-documentation-table ()
  "Return the current VM documentation table, creating it if needed."
  (let ((table nil))
    (when *vm-current-state*
      (let* ((globals (vm-global-vars *vm-current-state*))
             (package (find-package :cl-cc/expand))
             (qualified-symbol (and package (find-symbol "*DOCUMENTATION-TABLE*" package))))
        (dolist (symbol (remove nil (list qualified-symbol '*documentation-table*)))
          (let ((candidate (gethash symbol globals)))
            (when (and (null table) (hash-table-p candidate))
              (setf table candidate))))
        (unless table
          (setf table (make-hash-table :test #'equal))
          (when qualified-symbol
            (setf (gethash qualified-symbol globals) table))
          (setf (gethash '*documentation-table* globals) table))))
    (or table *documentation-table*)))

(defun %register-documentation (name doc-type docstring)
  "Register DOCSTRING for (NAME DOC-TYPE) in VM and host documentation tables."
  (let ((table (%ensure-vm-documentation-table)))
    (setf (gethash (list name doc-type) table) docstring)
    (setf (gethash (list name doc-type) *documentation-table*) docstring)
    (let* ((package (find-package :cl-cc/expand))
           (symbol (and package (find-symbol "*DOCUMENTATION-TABLE*" package))))
      (when (and symbol (boundp symbol) (hash-table-p (symbol-value symbol)))
        (setf (gethash (list name doc-type) (symbol-value symbol)) docstring)))
    docstring))

(defun %get-documentation (name doc-type)
  "Return documentation string for (NAME DOC-TYPE) from *documentation-table*, or nil."
  (dolist (table (%documentation-tables) nil)
    (dolist (name-candidate (%documentation-symbol-candidates name))
      (dolist (type-candidate (%documentation-symbol-candidates doc-type))
        (multiple-value-bind (doc foundp)
            (gethash (list name-candidate type-candidate) table)
          (when foundp
            (return-from %get-documentation doc)))))))

(defun %vm-subtypep (type1 type2 &optional environment)
  "Return ANSI-style SUBTYPEP values using cl-cc/type when available."
  (declare (ignore environment))
  (let* ((package (find-package :cl-cc/type))
         (symbol (and package (find-symbol "SUBTYPEP" package))))
    (if (and symbol (fboundp symbol))
        (funcall (symbol-function symbol) type1 type2)
        (cl:subtypep type1 type2))))

;;; Runtime helpers for setf expansion
;;; rt-plist-put is defined in :cl-cc/bootstrap (bootstrap/package.lisp)
;;; and inherited here via (:use :cl-cc/bootstrap) in the defpackage.

;; Register self-hosting functions: these take strings/forms, not closures
;; CL standard symbols and VM-local symbols are quoted directly.
;; Cross-package cl-cc symbols are resolved via find-symbol to avoid
;; interning fresh symbols that conflict when the umbrella :cl-cc uses both packages.
(dolist (entry `((%package-symbols . ,#'%package-symbols)
                 (cons . ,#'cons)
                 (%make-hash-table-with-test . ,#'%make-hash-table-with-test)
                 (%package-external-symbols . ,#'%package-external-symbols)
                 (%all-symbols . ,#'%all-symbols)
                 (make-pathname . ,#'make-pathname)
                 (pathname . ,#'pathname)
                 (namestring . ,#'namestring)
                 (file-namestring . ,#'file-namestring)
                 (compile-file-pathname . ,#'compile-file-pathname)
                 (pathname-host . ,#'pathname-host)
                 (pathname-device . ,#'pathname-device)
                 (pathname-directory . ,#'pathname-directory)
                 (pathname-name . ,#'pathname-name)
                 (pathname-type . ,#'pathname-type)
                 (pathname-version . ,#'pathname-version)
                 (merge-pathnames . ,#'merge-pathnames)
                 (truename . ,#'truename)
                 (parse-namestring . ,#'parse-namestring)
                 (wild-pathname-p . ,#'wild-pathname-p)
                 (pathname-match-p . ,#'pathname-match-p)
                 (translate-pathname . ,#'translate-pathname)
                 (probe-file . ,#'probe-file)
                 (rename-file . ,#'rename-file)
                 (delete-file . ,#'delete-file)
                 (file-write-date . ,#'file-write-date)
                 (file-author . ,#'file-author)
                 (directory . ,#'directory)
                 (ensure-directories-exist . ,#'ensure-directories-exist)
                 (make-synonym-stream . ,#'make-synonym-stream)
                 (make-broadcast-stream . ,#'make-broadcast-stream)
                 (make-two-way-stream . ,#'make-two-way-stream)
                 (make-echo-stream . ,#'make-echo-stream)
                 (make-concatenated-stream . ,#'make-concatenated-stream)
                 (broadcast-stream-streams . ,#'broadcast-stream-streams)
                 (two-way-stream-input-stream . ,#'two-way-stream-input-stream)
                 (two-way-stream-output-stream . ,#'two-way-stream-output-stream)
                 (echo-stream-input-stream . ,#'echo-stream-input-stream)
                 (echo-stream-output-stream . ,#'echo-stream-output-stream)
                 (concatenated-stream-streams . ,#'concatenated-stream-streams)
                  (file-string-length . ,#'%vm-file-string-length)
                  (foreign-funcall . ,#'%foreign-funcall)
                  (%foreign-funcall . ,#'%foreign-funcall)
                  (disassemble . ,#'%vm-safe-disassemble)
                 (inspect . ,#'%vm-safe-inspect)
                 (y-or-n-p . ,#'%vm-y-or-n-p)
                 (yes-or-no-p . ,#'%vm-yes-or-no-p)
                  (string-to-octets . ,#'string-to-octets)
                  (octets-to-string . ,#'octets-to-string)
                  (character . ,#'character)
                  (subtypep . ,#'%vm-subtypep)
                  (%class-slot-definitions . ,#'%class-slot-definitions)
                 (slot-definition-name . ,#'slot-definition-name)
                 (slot-definition-initform . ,#'slot-definition-initform)
                 (slot-definition-initfunction . ,#'slot-definition-initfunction)
                 (slot-definition-initargs . ,#'slot-definition-initargs)
                 (slot-definition-type . ,#'slot-definition-type)
                 (slot-definition-allocation . ,#'slot-definition-allocation)
                 (class-metaclass . ,#'class-metaclass)
                 (compute-effective-slot-definition . ,#'compute-effective-slot-definition)
                 (make-instances-obsolete . ,#'%make-instances-obsolete)
                  (generic-function-methods . ,#'generic-function-methods)
                  (generic-function-method-combination . ,#'generic-function-method-combination)
                  (rt-plist-put . ,#'rt-plist-put)
                  (%register-documentation . ,#'%register-documentation)
                  (%get-documentation . ,#'%get-documentation)))
  (vm-register-host-bridge (car entry) (cdr entry)))

(dolist (entry '((1+              . "RT-1+")
                 (1-              . "RT-1-")
                 (+               . "RT-+")
                 (-               . "RT--")
                 (*               . "RT-*")
                 (/               . "RT-/")
                 (<               . "RT-<")
                 (>               . "RT->")
                 (<=              . "RT-<=")
                 (>=              . "RT->=")
                 (max             . "RT-MAX")
                 (min             . "RT-MIN")
                 (length          . "RT-LENGTH")
                 (char-equal      . "RT-CHAR-EQUAL")
                 (char=           . "RT-CHAR=")
                 (eql             . "RT-EQL")
                 (equal           . "RT-EQUAL")
                 (equalp          . "RT-EQUALP")
                 (typep           . "RT-TYPEP")
                 (elt             . "RT-ELT")
                 (append          . "RT-APPEND")
                 (fboundp         . "RT-FBOUNDP")
                 (intern          . "RT-INTERN")
                 (gensym          . "RT-GENSYM")
                 (symbol-value    . "RT-SYMBOL-VALUE")))
  (vm-register-host-bridge
   (car entry)
   (let ((runtime-name (cdr entry)))
     (lambda (&rest args)
       (apply (%vm-runtime-callable runtime-name) args)))))

;; Cross-package cl-cc symbols: resolve via find-symbol so we use the canonical
;; symbol from each defining package (avoids interning cl-cc/vm:: duplicates).
;;
;; LOAD-ORDER GUARD: :cl-cc-vm loads before :cl-cc-compile/:cl-cc-parse/:cl-cc-expand
;; when :cl-cc-optimize (which depends on :cl-cc-vm) is promoted to a real ASDF system.
;; The find-package guard silently skips registration if a package doesn't exist yet.
