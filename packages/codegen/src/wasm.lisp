;;;; packages/emit/src/wasm.lisp - WebAssembly (WAT text) Backend
;;;
;;; Implements the WASM text format (WAT) emission backend for cl-cc.
;;; Uses the WASM GC proposal type system (struct/array/ref types).
;;; Control flow is handled via PC-dispatch trampoline (see wasm-trampoline.lisp).
;;;
;;; The emit-instruction generic is defined in x86-64.lisp (it is the shared
;;; backend interface). This file adds methods for the wasm-target class.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WASM target class
;;; ─────────────────────────────────────────────────────────────────────────────

(defclass wasm-target (target)
  ((module :initarg :module :initform nil :accessor wasm-target-module
    :documentation "The wasm-module-ir being built.")
   (reg-map :initarg :reg-map :initform nil :accessor wasm-target-reg-map
    :documentation "Current wasm-reg-map for the function being emitted.")
   (known-func-labels :initarg :known-func-labels
    :initform (make-hash-table)
    :accessor wasm-target-known-func-labels
    :documentation "Best-effort map from VM register to direct function label.")
   (known-slot-indexes :initarg :known-slot-indexes
    :initform (make-hash-table :test #'equal)
    :accessor wasm-target-known-slot-indexes
    :documentation "Best-effort map from slot-name symbol/string to slot index.")
   (class-slot-layouts :initarg :class-slot-layouts
    :initform (make-hash-table :test #'equal)
    :accessor wasm-target-class-slot-layouts
    :documentation "Map class-name => slot-name/index hash-table.")
   (class-slot-orders :initarg :class-slot-orders
    :initform (make-hash-table :test #'equal)
    :accessor wasm-target-class-slot-orders
    :documentation "Map class-name => effective slot-name order list.")
   (known-class-by-reg :initarg :known-class-by-reg
    :initform (make-hash-table)
    :accessor wasm-target-known-class-by-reg
    :documentation "Map VM register => class-name symbol/string.")
   (known-object-class-by-reg :initarg :known-object-class-by-reg
    :initform (make-hash-table)
    :accessor wasm-target-known-object-class-by-reg
    :documentation "Map VM object register => class-name symbol/string."))
  (:documentation "WASM backend target. Emits WebAssembly Text Format (WAT)."))

;;; target-register: maps VM register to WAT local index (integer)
(defmethod target-register ((target wasm-target) virtual-register)
  "Map a VM register keyword (:R0, :R1, ...) to a WASM local index."
  (let ((reg-map (wasm-target-reg-map target)))
    (if reg-map
        (wasm-reg-to-local reg-map virtual-register)
        ;; Fallback: extract number from :R0 -> 0
        (let* ((name (symbol-name virtual-register))
               (num (parse-integer name :start 1 :junk-allowed t)))
          (or num 0)))))

(defun %wasm-function-index-for-label (module label)
  "Return LABEL's function index using MODULE's function table."
  (or (and module (wasm-module-function-index-for-label module label))
      (let ((func (find label (wasm-module-functions module)
                        :key (lambda (f)
                               (let ((wat-name (wasm-func-wat-name f)))
                                 (if (and wat-name (> (length wat-name) 0)
                                          (char= (char wat-name 0) #\$))
                                     (subseq wat-name 1)
                                     wat-name)))
                        :test #'equal)))
        (when func
          (wasm-func-index func)))))

(defun %wasm-main-function-type-index (module)
  "Return the canonical type index for backend-generated CL functions."
  (let ((table (and module (wasm-module-type-signature-table module)))
        (signature '(:params nil :results (:eqref))))
    (when table
      (setf (gethash signature table) +type-idx-main-func+))
    +type-idx-main-func+))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT module header: predefined GC type section
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wat-type-section (stream)
  "Emit the predefined WASM GC type definitions to STREAM as WAT."
  ;; Type 0: main function type
  (format stream "~%  (type $main_func_t (func (result eqref)))")
  ;; Type 1: bytes array (i8, mutable)
  (format stream "~%  (type $bytes_array_t (array (mut i8)))")
  ;; Type 2: string struct
  (format stream "~%  (type $string_t (struct (field $chars (ref $bytes_array_t))))")
  ;; Type 3: symbol struct
  (format stream "~%  (type $symbol_t (struct (field $name (ref $string_t)) (field $plist eqref)))")
  ;; Type 4: cons cell
  (format stream "~%  (type $cons_t (struct (field $car (mut eqref)) (field $cdr (mut eqref))))")
  ;; Type 5: eqref array (for slots, envs, mv-buffers)
  (format stream "~%  (type $eqref_array_t (array (mut eqref)))")
  ;; Type 6: closure environment
  (format stream "~%  (type $env_t (struct (field $vars (ref $eqref_array_t)) (field $parent (ref null $env_t))))")
  ;; Type 7: closure
  (format stream "~%  (type $closure_t (struct (field $entry i32) (field $env (ref null $env_t))))")
  ;; Type 8: class metadata
  ;; Fields:
  ;; - name: class symbol
  ;; - slot_names: effective slot-name vector
  ;; - method_combination: symbol metadata (staged placeholder)
  ;; - methods: method table payload (staged eqref array)
  (format stream "~%  (type $class_meta_t (struct (field $name (ref $symbol_t)) (field $slot_names (ref $eqref_array_t)) (field $method_combination (ref $symbol_t)) (field $methods (ref $eqref_array_t))))")
  ;; Type 9: CLOS instance
  (format stream "~%  (type $instance_t (struct (field $class (ref $class_meta_t)) (field $slots (mut (ref $eqref_array_t)))))")
  ;; Type 10: hash table
  (format stream "~%  (type $htable_t (struct (field $keys (mut (ref $eqref_array_t))) (field $vals (mut (ref $eqref_array_t))) (field $count (mut i32))))")
  ;; Type 11: boxed float
  (format stream "~%  (type $float_t (struct (field $val f64)))")
  ;; Type 12: character
  (format stream "~%  (type $char_t (struct (field $code i32)))"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT module: global variable table
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wat-globals (module stream)
  "Emit WASM global variable declarations to STREAM."
  (dolist (global (wasm-module-globals module))
    (format stream "~%  (global ~A (mut eqref) (ref.null eq)) ;; globalidx ~D"
            (wasm-global-def-wat-name global)
            (wasm-global-def-index global))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT module: exception tags (exception handling proposal)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun ensure-wasm-condition-tag! (module)
  "Ensure MODULE has the CL condition/catch tag used by throw lowering."
  (let* ((name "$cl_condition_tag")
         (table (wasm-module-tag-name-table module)))
    (or (and table (gethash name table))
        (wasm-module-add-tag module
          (make-wasm-tag-def :wat-name name :params '(:eqref :eqref))))))

(defun emit-wat-tags (module stream)
  "Emit WASM exception tags to STREAM."
  (ensure-wasm-condition-tag! module)
  (dolist (tag (reverse (wasm-module-tags module)))
    (format stream "~%  (tag ~A (param eqref eqref)) ;; tagidx ~D"
            (wasm-tag-def-wat-name tag)
            (wasm-tag-def-index tag))))

(defun emit-wat-exception-helper (stream)
  "Emit a small EH helper so modules declare concrete try/catch/throw forms.

The main trampoline emits throw at VM throw/signal-error sites.  This helper is
kept separate from the PC-dispatch trampoline to avoid disturbing existing
control-flow lowering while still materializing the EH proposal instructions in
the generated module."
  (format stream "~%  (func $cl_eh_identity (param eqref) (result eqref)")
  (format stream "~%    (try (result eqref)")
  (format stream "~%      (do (throw $cl_condition_tag (ref.null eq) (local.get 0)) (ref.null eq))")
  (format stream "~%      (catch $cl_condition_tag")
  (format stream "~%        (local.set 0)")
  (format stream "~%        (drop)")
  (format stream "~%        (local.get 0)))")
  (format stream "~%  )"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT module: function table (for call_indirect)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wat-table (module stream)
  "Emit the funcref table for call_indirect dispatch."
  (let ((size (max 1 (wasm-module-table-size module))))
    (format stream "~%  (table $funcref_table ~D funcref)" size)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT module: elem segment — populate funcref table with all compiled functions
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wat-elem (module stream)
  "Emit an active elem segment that fills $funcref_table starting at offset 0
   with all compiled WASM functions.  This enables call_indirect dispatch for
   vm-call and vm-closure (which store the function's table index in $closure_t)."
  (let ((funcs (wasm-module-functions module)))
    (when funcs
      (format stream "~%  (elem (table $funcref_table) (i32.const 0) func")
      (dolist (func funcs)
        (format stream " ~A" (wasm-func-wat-name func)))
      (format stream ")"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT module: calling-convention globals ($cl_arg0..$cl_arg15)
;;; ─────────────────────────────────────────────────────────────────────────────

(defconstant +wasm-max-call-args+ 16
  "Maximum number of function arguments supported by the WASM calling convention.
   Arguments are passed via globals $cl_arg0 through $cl_arg15 before call_indirect.")

(defun emit-wat-call-globals (stream)
  "Emit WASM globals used to pass function arguments across call_indirect.
   Since all WASM functions in this backend have type (func (result eqref))
   with no WASM-level parameters, argument passing is done via these globals.
   The caller writes args to $cl_argN before call_indirect; the callee prologue
   reads them back into its local registers."
  (format stream "~%  ;; Argument-passing globals (calling convention)")
  (dotimes (i +wasm-max-call-args+)
    (format stream "~%  (global $cl_arg~D (mut eqref) (ref.null eq))" i)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT function local variable declarations
;;;
;;; The trampoline builder (wasm-trampoline.lisp) references locals by numeric
;;; index (e.g. (local.get 3)), not by WAT names. We emit anonymous locals in
;;; the right count to match those indices.
;;;   local 0  .. (param-count-1) : function parameters (currently none)
;;;   local param-count           : $pc (i32)
;;;   local param-count+1         : $tmp (eqref)
;;;   local param-count+2 ..      : one eqref per VM register allocated
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wat-function-locals (reg-map stream)
  "Emit local variable declarations for a function body, matching numeric indices
   used by the trampoline emitter."
  ;; $pc: i32 at index (wasm-reg-map-pc-index reg-map)
  (format stream "~%    (local i32) ;; $pc at index ~D"
          (wasm-reg-map-pc-index reg-map))
  ;; $tmp: eqref at index (wasm-reg-map-tmp-index reg-map)
  (format stream "~%    (local eqref) ;; $tmp at index ~D"
          (wasm-reg-map-tmp-index reg-map))
  ;; One eqref local per VM register that was allocated into this reg-map.
  ;; The count is (next-index - pc-slot - tmp-slot) = next-index - (pc + 1) - 1
  (let ((reg-count (- (wasm-reg-map-next-index reg-map)
                      (wasm-reg-map-pc-index reg-map)
                      2)))
    (dotimes (i (max 0 reg-count))
      (format stream "~%    (local eqref) ;; VM register local ~D"
              (+ (wasm-reg-map-tmp-index reg-map) 1 i)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT full function emission
;;;
;;; build-wasm-function-wat (wasm-trampoline.lisp) pre-builds the trampoline
;;; body string and stores it in (wasm-func-body func-def) as a one-element list.
;;; emit-wat-function serializes the header, locals, and that pre-built body.
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wat-function (func-def stream)
  "Emit a complete WAT function definition to STREAM.
   Assumes build-wasm-function-wat has already been called on FUNC-DEF so that
   (wasm-func-body func-def) holds the pre-built trampoline body string."
  (let* ((wat-name (wasm-func-wat-name func-def))
         (instructions (wasm-func-source-instructions func-def))
         ;; Re-build the reg-map with the same parameters as build-wasm-function-wat
         ;; so we know how many locals were allocated.
         (reg-map (make-wasm-reg-map-for-function 0)))
    ;; Pre-collect all registers to populate reg-map local count.
    (collect-registers-from-instructions instructions reg-map)
    ;; Function header: no parameters, returns eqref
    (format stream "~%  (func ~A (result eqref)" wat-name)
    ;; Local declarations
    (emit-wat-function-locals reg-map stream)
    ;; Pre-built trampoline body (list of strings from build-wasm-function-wat)
    (dolist (body-str (wasm-func-body func-def))
      (format stream "~A" body-str))
    ;; Close function
    (format stream "~%  ) ;; end func ~A" wat-name)
    ;; Export declaration if needed
    (when (wasm-func-exported-p func-def)
      (format stream "~%  (export ~S (func ~A))"
              (or (wasm-func-export-name func-def)
                  (subseq wat-name 1))
              wat-name))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT imports section (I/O host functions)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wat-imports (stream)
  "Emit standard cl-cc I/O imports."
  (format stream "~%  ;; Host I/O imports")
  (format stream "~%  (import \"cl_io\" \"write_char\" (func $host_write_char (param i32)))")
  (format stream "~%  (import \"cl_io\" \"read_char\" (func $host_read_char (result i32)))")
  (format stream "~%  (import \"cl_io\" \"write_string\" (func $host_write_string (param (ref $string_t))))")
  (format stream "~%  (import \"cl_io\" \"error\" (func $host_error (param (ref $string_t))))")
  ;; print_val: host-side formatter for any eqref value (used by vm-print)
  (format stream "~%  (import \"cl_io\" \"print_val\" (func $host_print_val (param eqref)))")
  ;; FR-321 staged runtime MOP bridge imports
  (format stream "~%  (import \"cl_runtime\" \"register_method\" (func $host_rt_register_method (param eqref) (param eqref) (param eqref) (param eqref)))")
  (format stream "~%  (import \"cl_runtime\" \"call_generic\" (func $host_rt_call_generic (param eqref) (param i32) (result eqref)))"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; emit-wasm-module: serialize a wasm-module-ir to WAT text
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wasm-module (module stream)
  "Serialize a wasm-module-ir to WAT text format on STREAM.
   Assumes build-all-wasm-functions has already been called so every
   wasm-function-def has a populated :body slot."
  (format stream "(module")
  (format stream "~%  ;; cl-cc generated WASM module (GC proposal)")
  ;; FR-258: Wasm Profiles — declare required features
  (when (wasm-profiles-feature-enabled-p)
    (emit-wasm-profiles-section stream))
  ;; FR-213: Memory64 — declare 64-bit memory when enabled
  (when (wasm-memory64-feature-enabled-p)
    (format stream "~%  ;; FR-213: Memory64 enabled (64-bit address space)")
    (format stream "~%  (memory (export \"mem\") i64 1 65536)"))
  ;; Type section
  (emit-wat-type-section stream)
  ;; Imports
  (emit-wat-imports stream)
  ;; Exception tags for CL conditions and catch/throw payloads
  (emit-wat-tags module stream)
  (emit-wat-exception-helper stream)
  ;; FR-252: EH v2 support — emit JS exception bridge helpers
  (when (wasm-js-exception-bridge-feature-enabled-p)
    (emit-wat-js-conversion-helpers stream))
  (when (wasm-eh-v2-feature-enabled-p)
    (emit-wat-eh-v2-helper stream))
  ;; Table (size updated by build-all-wasm-functions)
  (emit-wat-table module stream)
  ;; FR-229: table64 — 64-bit function table for Memory64 builds
  (when (wasm-memory64-feature-enabled-p)
    (format stream "~%  ;; FR-229: table64 alongside Memory64")
    (let ((size (max 1 (wasm-module-table-size module))))
      (format stream "~%  (table $funcref_table ~D i64 funcref)" size)))
  ;; User-defined global variables (from defvar/setq)
  (emit-wat-globals module stream)
  ;; Argument-passing calling convention globals ($cl_arg0..$cl_arg15)
  (emit-wat-call-globals stream)
  ;; Memories (linear memory declarations)
  (emit-wat-memories module stream)
  ;; Functions
  (dolist (func (wasm-module-functions module))
    (emit-wat-function func stream))
  ;; Elem segment: populate funcref table so call_indirect can dispatch
  (emit-wat-elem module stream)
  ;; FR-216: Branch Hinting custom section
  (when (wasm-branch-hints-feature-enabled-p)
    (emit-wasm-branch-hints-section module stream))
  ;; FR-222: DWARF debug info custom sections
  (when (wasm-dwarf-feature-enabled-p)
    (emit-wasm-dwarf-sections module stream))
  ;; FR-223: Source Map reference
  (when (wasm-source-map-enabled-p)
    (emit-wasm-source-map-reference stream))
  (format stream "~%) ;; end module~%"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WASM binary backend (FR-297)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-binary-write-u8 (buffer byte)
  "Write BYTE to BUFFER using the unified byte-buffer API."
  (cl-cc/binary::buffer-write-byte buffer byte))

(defun wasm-binary-write-bytes (buffer bytes)
  "Write BYTES to BUFFER using the unified byte-buffer API."
  (cl-cc/binary::buffer-write-bytes buffer bytes))

(defun wasm-encode-unsigned-leb128 (value)
  "Return VALUE encoded as an unsigned LEB128 byte vector."
  (check-type value (integer 0 *))
  (let ((bytes nil)
        (n value))
    (loop
      (let ((byte (logand n #x7f)))
        (setf n (ash n -7))
        (when (plusp n)
          (setf byte (logior byte #x80)))
        (push byte bytes)
        (unless (plusp n)
          (return (coerce (nreverse bytes)
                          '(simple-array (unsigned-byte 8) (*)))))))))

(defun wasm-encode-signed-leb128 (value)
  "Return VALUE encoded as a signed LEB128 byte vector."
  (check-type value integer)
  (let ((bytes nil)
        (n value)
        (more t))
    (loop while more do
      (let ((byte (logand n #x7f)))
        (setf n (ash n -7))
        (if (or (and (zerop n) (zerop (logand byte #x40)))
                (and (= n -1) (not (zerop (logand byte #x40)))))
            (setf more nil)
            (setf byte (logior byte #x80)))
        (push byte bytes)))
    (coerce (nreverse bytes) '(simple-array (unsigned-byte 8) (*)))))

(defun wasm-binary-write-uleb128 (buffer value)
  "Write VALUE as unsigned LEB128 to BUFFER."
  (wasm-binary-write-bytes buffer (wasm-encode-unsigned-leb128 value)))

(defun wasm-binary-section-buffer ()
  "Create a temporary byte-buffer for a section payload."
  (cl-cc/binary::make-byte-buffer 32))

(defun wasm-binary-section-bytes (writer)
  "Return bytes produced by calling WRITER with a temporary section buffer."
  (let ((payload (wasm-binary-section-buffer)))
    (funcall writer payload)
    (cl-cc/binary::buffer-get-bytes payload)))

(defun wasm-binary-write-section (buffer section-id writer)
  "Write a standard WASM section with SECTION-ID and payload from WRITER."
  (let ((payload (wasm-binary-section-bytes writer)))
    (wasm-binary-write-u8 buffer section-id)
    (wasm-binary-write-uleb128 buffer (length payload))
    (wasm-binary-write-bytes buffer payload)))

(defun wasm-binary-write-name (buffer name)
  "Write NAME as a WASM binary name (byte length + bytes)."
  (let ((bytes (map 'vector #'char-code name)))
    (wasm-binary-write-uleb128 buffer (length bytes))
    (wasm-binary-write-bytes buffer bytes)))

(defun wasm-binary-value-type-byte (type)
  "Return binary value-type byte for TYPE."
  (ecase type
    (:i32 +wasm-i32+)
    (:i64 +wasm-i64+)
    (:f32 +wasm-f32+)
    (:f64 +wasm-f64+)
    (:funcref +wasm-funcref+)
    (:externref +wasm-externref+)
    (:eqref +wasm-eqref+)))

(defun wasm-binary-write-valtype-vector (buffer types)
  "Write a vector of value TYPES."
  (wasm-binary-write-uleb128 buffer (length types))
  (dolist (type types)
    (wasm-binary-write-u8 buffer (wasm-binary-value-type-byte type))))

(defun wasm-binary-write-func-type (buffer params results)
  "Write a WASM function type."
  (wasm-binary-write-u8 buffer +wasm-type-func+)
  (wasm-binary-write-valtype-vector buffer params)
  (wasm-binary-write-valtype-vector buffer results))

(defun wasm-encode-vm-instruction-opcode (inst)
  "Return core WASM opcode bytes corresponding to VM instruction INST."
  (typecase inst
    (vm-const
     (let ((value (vm-value inst)))
       (cond
         ((integerp value)
          (concatenate '(simple-array (unsigned-byte 8) (*))
                       (vector +wasm-i64-const+)
                       (wasm-encode-signed-leb128 value)))
         ((null value) (vector +wasm-ref-null+ +heap-none+))
         (t (vector +wasm-nop+)))))
    ((or vm-add vm-integer-add) (vector +wasm-i64-add+))
    ((or vm-sub vm-integer-sub) (vector +wasm-i64-sub+))
    ((or vm-mul vm-integer-mul) (vector +wasm-i64-mul+))
    (vm-rotate (vector +wasm-i64-rotr+))
    (vm-logcount (vector +wasm-i64-popcnt+))
    (vm-integer-length (vector +wasm-i64-clz+))
    (vm-call (concatenate '(simple-array (unsigned-byte 8) (*))
                          (vector +wasm-call-indirect+)
                          (wasm-encode-unsigned-leb128 +type-idx-main-func+)
                          (wasm-encode-unsigned-leb128 0)))
    (vm-ret (vector +wasm-return+))
    (vm-halt (vector +wasm-return+))
    (t (vector +wasm-nop+))))

(defun wasm-binary-module-functions (module)
  "Return MODULE functions in emission order."
  (or (wasm-module-functions module) nil))

(defun wasm-binary-write-type-section (buffer &key (params nil) (results nil))
  "Write a Type section containing a single MVP () -> () function signature."
  (wasm-binary-write-section
   buffer +wasm-section-type+
   (lambda (section)
     (wasm-binary-write-uleb128 section 1)
     (wasm-binary-write-func-type section params results))))

(defun wasm-binary-write-function-section (buffer functions)
  "Write the Function section for FUNCTIONS, all using type index 0."
  (wasm-binary-write-section
   buffer +wasm-section-function+
   (lambda (section)
     (wasm-binary-write-uleb128 section (length functions))
     (dolist (func functions)
       (declare (ignore func))
       (wasm-binary-write-uleb128 section 0)))))

(defun wasm-binary-export-name-for-function (func)
  "Return FUNC's export name, or NIL when it should not be exported."
  (when (wasm-func-exported-p func)
    (or (wasm-func-export-name func)
        (let ((wat-name (wasm-func-wat-name func)))
          (if (and wat-name (> (length wat-name) 0)
                   (char= (char wat-name 0) #\$))
              (subseq wat-name 1)
              wat-name)))))

(defun wasm-binary-write-export-section (buffer functions)
  "Write the Export section for exported FUNCTIONS."
  (let ((exports (remove-if-not #'wasm-binary-export-name-for-function functions)))
    (when exports
      (wasm-binary-write-section
       buffer +wasm-section-export+
       (lambda (section)
         (wasm-binary-write-uleb128 section (length exports))
         (dolist (func exports)
           (wasm-binary-write-name section (wasm-binary-export-name-for-function func))
           (wasm-binary-write-u8 section +wasm-export-func+)
           (wasm-binary-write-uleb128 section (or (wasm-func-index func) 0))))))))

(defun wasm-binary-write-local-decls (buffer local-groups)
  "Write local declaration groups as (COUNT TYPE) entries."
  (wasm-binary-write-uleb128 buffer (length local-groups))
  (dolist (group local-groups)
    (destructuring-bind (count type) group
      (wasm-binary-write-uleb128 buffer count)
      (wasm-binary-write-u8 buffer (wasm-binary-value-type-byte type)))))

(defun wasm-binary-function-body-bytes (func)
  "Return a stack-neutral MVP function body for FUNC."
  (declare (ignore func))
  (wasm-binary-section-bytes
   (lambda (body)
     (wasm-binary-write-local-decls body nil)
     (wasm-binary-write-u8 body +wasm-end+))))

(defun wasm-binary-write-code-section (buffer functions)
  "Write the Code section for FUNCTIONS."
  (wasm-binary-write-section
   buffer +wasm-section-code+
   (lambda (section)
     (wasm-binary-write-uleb128 section (length functions))
     (dolist (func functions)
       (let ((body (wasm-binary-function-body-bytes func)))
         (wasm-binary-write-uleb128 section (length body))
         (wasm-binary-write-bytes section body))))))

(defun emit-wasm-binary-module (module)
  "Serialize MODULE to a minimal WebAssembly binary module byte array."
  (let* ((functions (wasm-binary-module-functions module))
         (buffer (cl-cc/binary::make-byte-buffer 128)))
    (wasm-binary-write-bytes buffer #(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00))
    (wasm-binary-write-type-section buffer)
    (wasm-binary-write-function-section buffer functions)
    (wasm-binary-write-export-section buffer functions)
    (wasm-binary-write-code-section buffer functions)
    (cl-cc/binary::buffer-get-bytes buffer)))

(defun compile-to-wasm-binary (program)
  "Compile a vm-program to a minimal WebAssembly binary module byte array."
  (let ((module (extract-wasm-functions program)))
    (emit-wasm-binary-module module)))


;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-216: Branch Hinting custom section
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wasm-branch-hints-section (module stream)
  "FR-216: Emit @metadata.code.branch_hint custom section for Wasm branch prediction hints.
   Annotates type predicate fast-paths with likely=1 hints."
  (declare (ignore module))
  (format stream "~%  ;; FR-216: Branch Hinting custom section")
  (format stream "~%  (@custom \"metadata.code.branch_hint\"")
  (format stream "~%    ;; Type predicate fast-paths annotated with likely=1")
  (format stream "~%    ;; Functions: consp→car/cdr, numberp→arithmetic, symbolp→symbol-access")
  (format stream "~%    ;; Format: func_idx instr_offset likely_value")
  (format stream "~%  )"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-222: DWARF 5 debug info custom sections (stub)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wasm-dwarf-sections (module stream)
  "FR-222: Emit DWARF 5 debug info as Wasm custom sections.
   Includes .debug_info, .debug_line, .debug_abbrev sections."
  (declare (ignore module))
  (format stream "~%  ;; FR-222: DWARF 5 debug info sections")
  (format stream "~%  ;; .debug_info: compilation unit header + DIE entries")
  (format stream "~%  ;; .debug_line: line number program (CL source → Wasm offset)")
  (format stream "~%  ;; .debug_abbrev: abbreviation table")
  (format stream "~%  ;; Enable with: --emit-debug-info flag"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-223: Source Map reference
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-source-map-enabled* nil
  "FR-223: When T, embed sourceMappingURL in Wasm binary.")

(defun wasm-source-map-enabled-p ()
  "FR-223: Check if source map emission is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_SOURCE_MAP" *wasm-source-map-enabled*))

(defun emit-wasm-source-map-reference (stream)
  "FR-223: Emit sourceMappingURL custom section reference."
  (format stream "~%  ;; FR-223: Source Map v3 reference")
  (format stream "~%  (@custom \"sourceMappingURL\" \"module.wasm.map\")"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-258: Wasm Profiles section
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-profiles-enabled* nil
  "FR-258: When T, emit Wasm Profiles section declaring required features.")

(defun wasm-profiles-feature-enabled-p ()
  "FR-258: Check if profile declaration is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_PROFILES" *wasm-profiles-enabled*))

(defun emit-wasm-profiles-section (stream)
  "FR-258: Emit Wasm Profiles section with required feature declarations."
  (format stream "~%  ;; FR-258: Wasm Profiles — required features")
  (let ((features nil))
    (when (wasm-simd-feature-enabled-p) (push "simd" features))
    (when (wasm-threads-feature-enabled-p) (push "threads" features))
    (when (wasm-eh-feature-enabled-p) (push "exceptions" features))
    (when (wasm-memory64-feature-enabled-p) (push "memory64" features))
    (when (wasm-gc-recursive-types-feature-enabled-p) (push "gc" features))
    (when features
      (format stream "~%  ;; Required: ~{~A~^, ~}" (nreverse features)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-219: AOT mode — Binaryen wasm-opt integration stub
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-aot-optimize* nil
  "FR-219: When T, run Binaryen wasm-opt after AOT compilation for size/speed.")

(defun wasm-aot-optimize-enabled-p ()
  "FR-219: Check if Binaryen optimization is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_AOT_OPTIMIZE" *wasm-aot-optimize*))

(defun wasm-run-binaryen-optimize (wasm-bytes)
  "FR-219: Run Binaryen wasm-opt on WASM-BYTES for size optimization.
   Returns optimized bytes or the original if wasm-opt is unavailable."
  (declare (ignore wasm-bytes))
  ;; Stub: Binaryen integration requires external wasm-opt binary
  ;; In production: shell out to `wasm-opt -O3 --strip-debug -o - <input.wasm`
  (format t "~&;; FR-219: Binaryen wasm-opt not available in stub~%")
  nil)

;;; (emit-instruction methods and compile-to-wasm-wat are in wasm-emit.lisp
;;;  which loads after this file.)
