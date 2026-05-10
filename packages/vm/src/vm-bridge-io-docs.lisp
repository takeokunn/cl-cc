;;;; vm-bridge-io-docs.lisp — I/O utilities, introspection, documentation, and bootstrap registrations
(in-package :cl-cc/vm)

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
