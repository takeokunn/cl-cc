;;;; src/backend/wasm-ir.lisp - WASM Module Intermediate Representation
;;;
;;; Defstructs representing a WASM module under construction.
;;; A wasm-module-ir accumulates all sections; it is then serialized
;;; by the WAT emitter or binary encoder.

(in-package :cl-cc)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Wasm field descriptor (for struct/array type definitions)
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (wasm-field (:conc-name wasm-field-))
  "A field in a WASM struct type definition."
  ;; :i32 :i64 :f64 :eqref :anyref :funcref or (:ref N) or (:ref-null N)
  (type nil)
  ;; :immutable or :mutable
  (mutability :mutable))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Wasm type definitions (type section entries)
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (wasm-func-type (:conc-name wasm-func-type-))
  "A WASM function type signature."
  (params nil :type list)    ; list of type keywords/specs
  (results nil :type list))  ; list of type keywords/specs

(defstruct (wasm-struct-type (:conc-name wasm-struct-type-))
  "A WASM GC struct type definition."
  (fields nil :type list)   ; list of wasm-field structs
  ;; optional supertype index for subtyping
  (supertype nil))

(defstruct (wasm-array-type (:conc-name wasm-array-type-))
  "A WASM GC array type definition."
  (element-type nil)          ; type keyword/spec
  (mutability :mutable))

(defstruct (wasm-type-entry (:conc-name wasm-type-entry-))
  "A named entry in the type section."
  (index nil :type (or null integer))  ; assigned index (nil until finalized)
  ;; one of: wasm-func-type, wasm-struct-type, wasm-array-type
  (definition nil)
  ;; WAT name for comments/debugging (e.g. "$cons", "$string")
  (wat-name nil :type (or null string)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Import/export descriptors
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (wasm-import (:conc-name wasm-import-))
  "A WASM import declaration."
  (module nil :type string)   ; module name, e.g. "cl-io"
  (name nil :type string)     ; field name, e.g. "write_char"
  ;; :func, :table, :memory, :global
  (kind :func)
  (type-index nil))           ; for :func, the func-type index

(defstruct (wasm-export (:conc-name wasm-export-))
  "A WASM export declaration."
  (name nil :type string)     ; export name
  ;; :func, :table, :memory, :global
  (kind :func)
  (index nil :type (or null integer)))  ; index of the exported entity

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Global variable descriptors
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (wasm-global-def (:conc-name wasm-global-def-))
  "A WASM global variable definition."
  (index nil)               ; assigned global index
  (wat-name nil)            ; WAT name for debugging (e.g. "$g_myvar")
  (value-type nil)          ; type keyword/spec
  (mutability :mutable)
  ;; init expression — for simple types, a literal value; for refs, :null or a func index
  (init-value nil))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Local variable descriptor (for function locals)
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (wasm-local (:conc-name wasm-local-))
  "A WASM local variable declaration in a function."
  (index nil :type (or null integer))   ; local index (0 = first param, then extra locals)
  (wat-name nil)              ; WAT name (e.g. "$R0", "$pc")
  ;; type keyword/spec
  (value-type nil))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Function definition
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (wasm-function-def (:conc-name wasm-func-))
  "A WASM function definition in the code section."
  (index nil)               ; assigned function index (imports come first)
  (wat-name nil)            ; WAT name (e.g. "$fn_myname")
  (type-index nil)          ; index into type section for this function's signature
  (params nil :type list)   ; list of wasm-local (params)
  (locals nil :type list)   ; list of wasm-local (non-param locals)
  ;; the body as a list of WAT s-expression strings or nested lists
  ;; e.g. ("(local.set $pc (i32.const 0))" "(block $exit ...")
  (body nil :type list)
  ;; The vm-program-instructions subset assigned to this function
  (source-instructions nil :type list)
  ;; Whether this is exported
  (exported-p nil)
  ;; Export name (if exported-p)
  (export-name nil))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Complete WASM module IR
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (wasm-module-ir (:conc-name wasm-module-))
  "Intermediate representation of a complete WASM module."
  ;; type section entries (list of wasm-type-entry)
  (types nil :type list)
  ;; import section entries (list of wasm-import)
  (imports nil :type list)
  ;; function definitions (list of wasm-function-def, does NOT include imports)
  (functions nil :type list)
  ;; global variable definitions (list of wasm-global-def)
  (globals nil :type list)
  ;; export entries (list of wasm-export) — auto-populated from function exports
  (exports nil :type list)
  ;; table size (integer) for funcref table used by call_indirect
  (table-size 0 :type integer)
  ;; start function index (or nil for no start)
  (start-function nil)
  ;; counter for assigning function indices
  (next-func-index 0 :type integer)
  ;; counter for assigning global indices
  (next-global-index 0 :type integer)
  ;; map from lisp name (symbol) to wasm-global-def, for global variable lookup
  (global-name-table nil))

(defun make-empty-wasm-module ()
  "Create an empty wasm-module-ir with an initialized global-name-table."
  (make-wasm-module-ir
   :global-name-table (make-hash-table :test #'equal)))

;;; Helper: add a function to the module and assign its index
(defun wasm-module-add-function (module func)
  "Add a wasm-function-def to MODULE, assigning it the next function index."
  (setf (wasm-func-index func) (wasm-module-next-func-index module))
  (incf (wasm-module-next-func-index module))
  (push func (wasm-module-functions module))
  func)

;;; Helper: add a global to the module and assign its index
(defun wasm-module-add-global (module global-def)
  "Add a wasm-global-def to MODULE, assigning it the next global index."
  (setf (wasm-global-def-index global-def) (wasm-module-next-global-index module))
  (incf (wasm-module-next-global-index module))
  (push global-def (wasm-module-globals module))
  (when (wasm-global-def-wat-name global-def)
    (setf (gethash (wasm-global-def-wat-name global-def)
                   (wasm-module-global-name-table module))
          global-def))
  global-def)

;;; Helper: lookup a wasm-global-def by its WAT name
(defun wasm-module-find-global (module wat-name)
  "Look up a global by its WAT name. Returns wasm-global-def or nil."
  (gethash wat-name (wasm-module-global-name-table module)))

;;; Helper: convert a Lisp global variable name to a WAT identifier
(defun wasm-lisp-name-to-wat-id (name)
  "Downcase NAME and replace non-alphanumeric chars with underscores."
  (let ((str (string-downcase (if (symbolp name) (symbol-name name) (string name)))))
    (with-output-to-string (s)
      (loop for c across str
            do (write-char (if (or (alphanumericp c) (char= c #\_)) c #\_) s)))))

(defun vm-global-wat-name (name)
  "Return the WAT $global name for a Lisp global variable NAME (symbol or string)."
  (format nil "$g_~A" (wasm-lisp-name-to-wat-id name)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Register-to-local mapping context
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (wasm-reg-map (:conc-name wasm-reg-map-))
  "Maps VM virtual registers (:R0, :R1, ...) to WASM local variable indices."
  ;; hash table: register keyword -> integer local index
  (table nil)
  ;; next local index to assign (starts after params)
  (next-index 0 :type integer)
  ;; special locals: $pc (i32), $tmp (eqref)
  (pc-index nil)
  (tmp-index nil))

(defun make-wasm-reg-map-for-function (param-count)
  "Create a register map. Params occupy locals 0..param-count-1.
   $pc gets index param-count, $tmp gets param-count+1."
  (make-wasm-reg-map
   :table (make-hash-table)
   :next-index (+ param-count 2)  ; skip $pc and $tmp
   :pc-index param-count
   :tmp-index (1+ param-count)))

(defun wasm-reg-to-local (reg-map reg)
  "Return the WASM local index for VM register REG.
   Allocates a new local if not yet mapped."
  (or (gethash reg (wasm-reg-map-table reg-map))
      (let ((idx (wasm-reg-map-next-index reg-map)))
        (setf (gethash reg (wasm-reg-map-table reg-map)) idx)
        (incf (wasm-reg-map-next-index reg-map))
        idx)))
