(in-package :cl-cc)
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

(defvar *vm-host-bridge-functions* (make-hash-table :test #'eq)
  "Set of symbols whose host CL functions may be called directly from the VM.
Only functions that accept non-closure arguments (strings, numbers, symbols,
lists of data) should be registered here.")

(defun vm-register-host-bridge (sym)
  "Register SYM as a host-bridgeable function for the VM."
  (setf (gethash sym *vm-host-bridge-functions*) t))

(defun hash-table-values (table)
  "Return TABLE values as a list preserving host iteration order."
  (let ((result nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v result))
             table)
    (nreverse result)))

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
      (hash-table-values methods-ht))))

(defun generic-function-method-combination (gf)
  "Return the method-combination metadata for GF, defaulting to STANDARD."
  (if (and (hash-table-p gf) (gethash :__method-combination__ gf))
      (gethash :__method-combination__ gf)
      'standard))

;;; Package introspection helpers for do-symbols/do-external-symbols/do-all-symbols
(defun %package-symbols (package)
  "Return a list of all symbols accessible in PACKAGE."
  (let ((result nil))
    (do-symbols (s package) (push s result))
    (nreverse result)))

(defun %package-external-symbols (package)
  "Return a list of all external symbols in PACKAGE."
  (let ((result nil))
    (do-external-symbols (s package) (push s result))
    (nreverse result)))

(defun %all-symbols ()
  "Return a list of all symbols in all packages."
  (let ((result nil))
    (do-all-symbols (s) (push s result))
    (nreverse result)))

;;; Runtime helpers for setf expansion
(defun rt-plist-put (plist indicator value)
  "Return a new plist with INDICATOR set to VALUE. Non-destructive."
  (let ((result nil) (found nil) (p plist))
    (loop while p do
          (let ((k (car p)))
            (if (eq k indicator)
                (progn (push indicator result) (push value result) (setf found t))
                (progn (push k result) (push (cadr p) result)))
            (setf p (cddr p))))
    (unless found
      (push indicator result) (push value result))
    (nreverse result)))

;; Register self-hosting functions: these take strings/forms, not closures
(dolist (sym '(run-string run-string-repl our-eval our-load
              compile-expression compile-string
              parse-all-forms
              ;; Macro expansion support
              generate-lambda-bindings register-macro
              ;; CL functions needed by self-hosting code
              find-package symbol-function intern gensym
              ;; FR-647/FR-655/FR-428/FR-558: symbol-value, find-symbol, macro-function, symbol-package
              symbol-value find-symbol macro-function symbol-package
              ;; FR-624: subtypep — delegate to host CL for standard type hierarchy
              subtypep
              ;; FR-512: compile — delegate to host CL
              compile
              ;; FR-437/FR-438: Package introspection — delegate to host CL
              list-all-packages package-name package-nicknames
              package-use-list package-used-by-list package-shadowing-symbols
              make-package rename-package delete-package
              export import unexport shadow shadowing-import
              use-package unuse-package
               ;; FR-361: Package symbol iteration helpers
               %package-symbols %package-external-symbols %all-symbols
               %class-slot-definitions
               ;; CLOS / MOP metadata helpers
               slot-definition-name slot-definition-initform
               slot-definition-initargs slot-definition-allocation
               generic-function-methods generic-function-method-combination
               ;; Runtime helpers for setf expansion
               rt-plist-put
              ;; FR-479: File system operations
              probe-file rename-file delete-file file-write-date file-author
              directory ensure-directories-exist
              ;; FR-566: Pathname constructors + accessors
              make-pathname
              pathname pathname-name pathname-type pathname-host
              pathname-device pathname-directory pathname-version
              truename parse-namestring merge-pathnames namestring
              file-namestring directory-namestring host-namestring
              enough-namestring wild-pathname-p pathname-match-p
              translate-pathname
              ;; FR-659: Reader — read-delimited-list
              read-delimited-list
              ;; FR-435: Apropos
              apropos apropos-list
              ;; FR-393: Composite streams
              make-synonym-stream make-broadcast-stream
              make-two-way-stream make-echo-stream
              make-concatenated-stream
              ;; FR-567: Composite stream accessors
              broadcast-stream-streams two-way-stream-input-stream
              two-way-stream-output-stream echo-stream-input-stream
              echo-stream-output-stream concatenated-stream-streams
              synonym-stream-symbol
               ;; Generic sequence access (used by search, write-sequence, etc.)
               elt append
               ;; FR-566: compile-file-pathname
               compile-file-pathname
              ;; FR-579: Encoding/decoding (SBCL sb-ext wrappers)
              string-to-octets octets-to-string
              ;; FR-607: Documentation retrieval from CL-level table
              %get-documentation))
  (vm-register-host-bridge sym))

;;; FR-579: String encoding helpers (delegate to SBCL's sb-ext)
(defun string-to-octets (string &key (encoding :utf-8) (external-format :default)
                                      (start 0) end)
  "Convert STRING to a byte vector using ENCODING (default UTF-8)."
  (declare (ignore external-format))
  (let ((format (case encoding (:utf-8 :utf-8) (:ascii :ascii) (t :utf-8))))
    (sb-ext:string-to-octets string :external-format format
                                    :start start :end (or end (length string)))))

(defun octets-to-string (octets &key (encoding :utf-8) (external-format :default)
                                       (start 0) end)
  "Convert byte vector OCTETS to a string using ENCODING (default UTF-8)."
  (declare (ignore external-format))
  (let ((format (case encoding (:utf-8 :utf-8) (:ascii :ascii) (t :utf-8))))
    (sb-ext:octets-to-string octets :external-format format
                                    :start start :end (or end (length octets)))))

;;; FR-607: Documentation retrieval (CL-level, registered by defun expander)
(defun %get-documentation (name doc-type)
  "Return documentation string for (NAME DOC-TYPE) from *documentation-table*, or nil."
  (when (hash-table-p *documentation-table*)
    (gethash (list name doc-type) *documentation-table*)))
