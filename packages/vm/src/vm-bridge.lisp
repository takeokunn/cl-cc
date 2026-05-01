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

(defun vm-bridge-callable (sym)
  "Return the callable registered for SYM, or NIL if none is available."
  (let ((entry (gethash sym *vm-host-bridge-functions*)))
    (cond
      ((functionp entry) entry)
      (t nil))))

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

(defun %make-hash-table-with-test (test-designator)
  "Construct a native CL hash table honoring TEST-DESIGNATOR when supported.
Accepts symbols/functions for EQ/EQL/EQUAL/EQUALP and falls back to EQL." 
  (let* ((sym (cond ((symbolp test-designator) test-designator)
                    ((functionp test-designator)
                     (or (hash-test-symbol test-designator) 'eql))
                    (t 'eql)))
         (fn (resolve-hash-test sym)))
    (make-hash-table :test fn)))

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
        (class-slots (and (hash-table-p class) (gethash :__class-slots__ class))))
    (setf (gethash :name slot) slot-name)
    (let ((entry (assoc slot-name initforms)))
      (when entry
        (setf (gethash :initform slot) (cdr entry))))
    (setf (gethash :initargs slot) (%class-slot-initargs-for-slot class slot-name))
    (setf (gethash :allocation slot)
          (if (member slot-name class-slots :test #'eq) :class :instance))
    slot))

(defun %class-slot-definitions (class)
  "Return public slot-definition objects for CLASS."
  (let ((slots (and (hash-table-p class) (gethash :__slots__ class))))
    (when slots
      (mapcar (lambda (slot-name)
                (%class-slot-metadata class slot-name))
              slots))))

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

(defun slot-definition-allocation (slot)
  "Return the allocation mode for SLOT, defaulting to :INSTANCE."
  (if (hash-table-p slot)
      (or (gethash :allocation slot) :instance)
      :instance))

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

;;; Package introspection helpers for do-symbols/do-external-symbols/do-all-symbols
(defun %vm-runtime-package-registry ()
  "Return the runtime package registry when available, else NIL." 
  (when cl-cc/bootstrap::*runtime-package-registry-provider*
    (funcall cl-cc/bootstrap::*runtime-package-registry-provider*)))

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
  (setf cl-cc/bootstrap::*vm-runtime-callable-installer* #'vm-register-runtime-callable)
  (setf cl-cc/bootstrap::*vm-eval-hook-installer* #'vm-install-eval-hooks)
  (setf cl-cc/bootstrap::*vm-macroexpand-hook-installer* #'vm-install-macroexpand-hooks)
  (setf cl-cc/bootstrap::*vm-parse-forms-hook-installer* #'vm-install-parse-forms-hook)
  (when cl-cc/bootstrap::*runtime-vm-callable-register-hook*
    (funcall cl-cc/bootstrap::*runtime-vm-callable-register-hook*)))

;;; FR-607: Documentation retrieval (CL-level, registered by defun expander)
(defun %get-documentation (name doc-type)
  "Return documentation string for (NAME DOC-TYPE) from *documentation-table*, or nil."
  (when (hash-table-p *documentation-table*)
    (gethash (list name doc-type) *documentation-table*)))

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
                 (compile-file-pathname . ,#'compile-file-pathname)
                 (pathname-name . ,#'pathname-name)
                 (pathname-type . ,#'pathname-type)
                 (%class-slot-definitions . ,#'%class-slot-definitions)
                 (slot-definition-name . ,#'slot-definition-name)
                 (slot-definition-initform . ,#'slot-definition-initform)
                 (slot-definition-initargs . ,#'slot-definition-initargs)
                 (slot-definition-allocation . ,#'slot-definition-allocation)
                 (generic-function-methods . ,#'generic-function-methods)
                 (generic-function-method-combination . ,#'generic-function-method-combination)
                 (rt-plist-put . ,#'rt-plist-put)
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
