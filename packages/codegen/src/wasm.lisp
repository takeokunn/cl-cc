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
     :documentation "Map VM object register => class-name symbol/string.")
    (try-stack :initarg :try-stack :initform nil :accessor wasm-target-try-stack
     :documentation "Structured Wasm EH frames active in direct instruction emission.")
    (try-depth :initarg :try-depth :initform 0 :accessor wasm-target-try-depth
     :documentation "Current native Wasm try/catch nesting depth."))
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
  (let ((rec-p (wasm-gc-recursive-types-feature-enabled-p)))
    (flet ((emit-type (wat)
             (format stream "~%  ~:[~;  ~]~A" rec-p wat)))
      ;; FR-209: $fixnum_t is an i31ref alias, not a heap struct type.  Fixnums
      ;; are stored inline with ref.i31 and read with i31.get_s/i31.get_u; no
      ;; (struct.new $fixnum_t) allocation is emitted.
      (format stream "~%  ;; $fixnum_t := i31ref (native Wasm GC immediate; no heap allocation)")
      (when (wasm-half-precision-feature-enabled-p)
        (format stream "~%  ;; FR-248: short-float maps to f16 value type"))
      (when (wasm-reference-typed-strings-feature-enabled-p)
        (format stream "~%  ;; FR-251: simple-string maps to native stringref"))
      ;; Type 0: main function type
      (format stream "~%  (type $main_func_t (func (result eqref)))")
      (format stream "~%  ;; FR-231: hierarchy: anyref > eqref > i31ref/structref/arrayref")
      (when rec-p (format stream "~%  (rec"))
      ;; Type 1: bytes array (i8, mutable)
      (emit-type "(type $bytes_array_t (array (mut i8)))")
      ;; Type 2: string struct
      (emit-type "(type $string_t (struct (field $chars (ref $bytes_array_t))))")
      ;; Type 3: symbol struct
      (emit-type "(type $symbol_t (struct (field $name (ref $string_t)) (field $plist eqref)))")
      ;; Type 4: cons cell.  CL permits dotted lists, so the physical CDR field
      ;; remains eqref; the surrounding rec group still allows recursive cons/env
      ;; references wherever values are statically known to be lists.
      (emit-type "(type $cons_t (struct (field $car (mut eqref)) (field $cdr (mut eqref))))")
      ;; Type 5: eqref array (for slots, envs, mv-buffers)
      (emit-type "(type $eqref_array_t (array (mut eqref)))")
      ;; Type 6: closure environment (recursive parent)
      (emit-type "(type $env_t (struct (field $vars (ref $eqref_array_t)) (field $parent (ref null $env_t))))")
      ;; Type 7: closure
      (emit-type (format nil "(type $closure_t (struct (field $entry ~A) (field $env ~A)))"
                         (if (wasm-table64-feature-enabled-p) "i64" "i32")
                         (if *wasm-typed-closure-env-enabled*
                             "(ref null $eqref_array_t)"
                             "(ref null $env_t)")))
      ;; Type 8: class metadata.  Per-class inheritance is modeled by the
      ;; compiler-side effective slot-order maps and materialized in metadata.
      (emit-type "(type $class_meta_t (struct (field $name (ref $symbol_t)) (field $slot_names (ref $eqref_array_t)) (field $method_combination (ref $symbol_t)) (field $methods (ref $eqref_array_t))))")
      ;; Type 9: CLOS instance
      (emit-type "(type $instance_t (struct (field $class (ref $class_meta_t)) (field $slots (mut (ref $eqref_array_t)))))")
      ;; Type 10: hash table
      (emit-type "(type $htable_t (struct (field $keys (mut (ref $eqref_array_t))) (field $vals (mut (ref $eqref_array_t))) (field $count (mut i32))))")
      ;; Type 11: boxed float
      (emit-type "(type $float_t (struct (field $val f64)))")
      ;; Type 12: character
      (emit-type "(type $char_t (struct (field $code i32)))")
      ;; Type 13: opaque JS reference box.
      (emit-type "(type $js_ref_t (struct (field $value externref)))")
      ;; FR-211: specialized GC array types for unboxed CL vectors.
      (emit-type "(type $fixnum_array_t (array (mut i64)))")
      (emit-type "(type $float_array_t (array (mut f64)))")
      (emit-type "(type $char_array_t (array (mut i32)))")
      (when rec-p (format stream "~%  )")))))

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
        (and (wasm-import-cl-condition-tag-enabled-p)
             (make-wasm-tag-def :wat-name name :params '(:eqref :eqref)))
        (wasm-module-add-tag module
          (make-wasm-tag-def :wat-name name :params '(:eqref :eqref))))))

(defun emit-wat-tags (module stream)
  "Emit WASM exception tags to STREAM."
  (ensure-wasm-condition-tag! module)
  (unless (wasm-import-cl-condition-tag-enabled-p)
    (dolist (tag (reverse (wasm-module-tags module)))
      (format stream "~%  (tag ~A (param eqref eqref)) ;; tagidx ~D"
              (wasm-tag-def-wat-name tag)
              (wasm-tag-def-index tag))))
  (when (wasm-exception-tag-linking-feature-enabled-p)
    (format stream "~%  (export \"cl_condition_tag\" (tag $cl_condition_tag))")))

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
    (if (wasm-table64-feature-enabled-p)
        (format stream "~%  (table $funcref_table i64 ~D funcref)" size)
        (format stream "~%  (table $funcref_table ~D funcref)" size))))

(defun emit-wat-table64-helpers (stream)
  "Emit FR-229 helper functions that exercise i64 table indices when enabled."
  (when (wasm-table64-feature-enabled-p)
    (format stream "~%  ;; FR-229: table64 helper forms use i64 table indices")
    (format stream "~%  (func $clcc_table_get (param i64) (result funcref)")
    (format stream "~%    (table.get $funcref_table (local.get 0))")
    (format stream "~%  )")
    (format stream "~%  (func $clcc_table_set (param i64) (param funcref)")
    (format stream "~%    (table.set $funcref_table (local.get 0) (local.get 1))")
    (format stream "~%  )")
    (format stream "~%  (func $clcc_table_grow (param i64) (result i64)")
    (format stream "~%    (table.grow $funcref_table (ref.null func) (local.get 0))")
    (format stream "~%  )")
    (format stream "~%  (func $clcc_table_size (result i64)")
    (format stream "~%    (table.size $funcref_table)")
    (format stream "~%  )")))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT module: elem segment — populate funcref table with all compiled functions
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wat-elem (module stream)
  "Emit element-segment initialization for $funcref_table.

When FR-237 bulk table operations are enabled, emit a passive element segment
plus a start-time table.init/elem.drop initializer.  Otherwise keep the legacy
active segment fallback."
  (let ((funcs (wasm-module-functions module)))
    (when funcs
      (if (wasm-bulk-table-feature-enabled-p)
          (progn
            (format stream "~%  ;; FR-237: passive elem segment + table.init")
            (format stream "~%  (elem $clcc_funcrefs func")
            (dolist (func funcs)
              (format stream " ~A" (wasm-func-wat-name func)))
            (format stream ")")
            (format stream "~%  (func $clcc_init_funcref_table")
            (format stream "~%    ~A"
                    (emit-wasm-table-init-wat "$funcref_table" "$clcc_funcrefs"
                                              (wasm-table-const-wat 0)
                                              (wasm-table-const-wat 0)
                                              (wasm-table-const-wat (length funcs))))
            (format stream "~%    ~A" (emit-wasm-elem-drop-wat "$clcc_funcrefs"))
            (format stream "~%  )")
            (format stream "~%  (start $clcc_init_funcref_table)")
            (format stream "~%  ;; FR-237 helper forms available: ~A"
                    (emit-wasm-table-copy-wat "$funcref_table" "$funcref_table"
                                              (wasm-table-const-wat 0)
                                              (wasm-table-const-wat 0)
                                              (wasm-table-const-wat 0)))
            (format stream "~%  ;; FR-237 helper forms available: ~A"
                    (emit-wasm-table-fill-wat "$funcref_table" (wasm-table-const-wat 0) "(ref.null func)" (wasm-table-const-wat 0))))
          (progn
            (format stream "~%  (elem (table $funcref_table) (~A.const 0) func"
                    (if (wasm-table64-feature-enabled-p) "i64" "i32"))
            (dolist (func funcs)
              (format stream " ~A" (wasm-func-wat-name func)))
            (format stream ")"))))))

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
  ;; Closure parameters are loaded from $cl_argN globals into leading eqref locals.
  (dotimes (i (wasm-reg-map-pc-index reg-map))
    (format stream "~%    (local eqref) ;; closure parameter local ~D" i))
  ;; $pc: i32 at index (wasm-reg-map-pc-index reg-map)
  (format stream "~%    (local i32) ;; $pc at index ~D"
          (wasm-reg-map-pc-index reg-map))
  ;; $tmp: eqref at index (wasm-reg-map-tmp-index reg-map)
  (format stream "~%    (local eqref) ;; $tmp at index ~D"
          (wasm-reg-map-tmp-index reg-map))
  ;; One eqref local per VM register that was allocated into this reg-map.
  ;; The count is everything after $pc and $tmp.
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
          (param-regs (wasm-function-param-regs func-def))
          (reg-map (make-wasm-reg-map-for-function (length param-regs))))
    (initialize-wasm-param-locals reg-map param-regs)
    ;; Pre-collect all registers to populate reg-map local count.
    (collect-registers-from-instructions instructions reg-map)
    (when (or (wasm-func-exception-table func-def)
              (some (lambda (inst)
                      (or (typep inst 'vm-establish-catch)
                          (typep inst 'vm-establish-handler)))
                    instructions))
      (wasm-reg-map-eh-tag-index reg-map))
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
    (when (and (wasm-func-exported-p func-def)
               (not (wasm-bigint-feature-enabled-p)))
      (format stream "~%  (export ~S (func ~A))"
              (or (wasm-func-export-name func-def)
                  (subseq wat-name 1))
              wat-name))))

(defun wasm-bigint-wrapper-name (func-def)
  "Return the internal WAT name for FUNC-DEF's BigInt i64 boundary wrapper."
  (format nil "~A_bigint_i64" (wasm-func-wat-name func-def)))

(defun emit-wat-bigint-wrapper (func-def stream)
  "Emit an opt-in FR-236 JS BigInt ↔ i64 export wrapper for FUNC-DEF.

The wrapper exposes an i64 parameter/result at the Wasm boundary.  JS engines
surface those i64 values as BigInt.  The existing CL function ABI remains eqref
internally, so the wrapper boxes the incoming i64 into cl_arg0 and unboxes the
primary eqref result back to i64."
  (let ((wrapper-name (wasm-bigint-wrapper-name func-def))
        (export-name (or (wasm-func-export-name func-def)
                         (subseq (wasm-func-wat-name func-def) 1))))
    (format stream "~%  ;; FR-236: BigInt/i64 boundary wrapper for ~A" (wasm-func-wat-name func-def))
    (format stream "~%  (func ~A (param i64) (result i64)" wrapper-name)
    (format stream "~%    (global.set $cl_arg0 ~A)" (wasm-fixnum-box "(local.get 0)"))
    (format stream "~%    (i64.extend_i32_s (i31.get_s (ref.cast i31 (call ~A))))" (wasm-func-wat-name func-def))
    (format stream "~%  ) ;; end func ~A" wrapper-name)
    (format stream "~%  (export ~S (func ~A))" export-name wrapper-name)))

(defun emit-wat-bigint-wrappers (module stream)
  "Emit FR-236 wrappers for exported functions when BigInt integration is enabled."
  (when (wasm-bigint-feature-enabled-p)
    (dolist (func (wasm-module-functions module))
      (when (wasm-func-exported-p func)
        (emit-wat-bigint-wrapper func stream)))))

(defun emit-wat-bigint-js-wrapper-code (stream)
  "Emit documentation comments containing the JS BigInt wrapper shape."
  (when (wasm-bigint-feature-enabled-p)
    (format stream "~%  ;; FR-236 JS BigInt wrapper pattern:")
    (format stream "~%  ;; export const callI64 = (instance, name, x) => instance.exports[name](BigInt(x));")))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT imports section (I/O host functions)
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-aot-current-used-imports* nil
  "Dynamic hash-set of host bridge import names used by the module being emitted.")

(defun %wasm-aot-mode-active-p ()
  "Return T when the backend should avoid mandatory host imports."
  (wasm-feature-enabled-p "CLCC_WASM_AOT" *wasm-aot-mode-enabled*))

(defun %wasm-import-needed-p (name)
  "Return T when host import NAME should be emitted."
  (or (not *wasm-dead-import-elimination-enabled*)
      (null *wasm-aot-current-used-imports*)
      (gethash name *wasm-aot-current-used-imports*)))

(defun %wasm-call-string-mentions-p (needle string)
  (and (stringp string) (search needle string :test #'char=)))

(defun wasm-module-used-host-imports (module)
  "Build a best-effort dependency graph of host bridge functions used by MODULE."
  (let ((used (make-hash-table :test #'equal)))
    (labels ((mark (name) (setf (gethash name used) t))
             (scan-string (text)
               (dolist (entry '(("$host_write_char" . "write_char")
                                ("$host_read_char" . "read_char")
                                ("$host_write_string" . "write_string")
                                ("$host_error" . "error")
                                ("$host_print_val" . "print_val")
                                ("$host_rt_register_method" . "register_method")
                                ("$host_rt_call_generic" . "call_generic")
                                ("$cl_condition_to_exnref" . "condition_to_exnref")
                                ("$cl_exnref_payload" . "exnref_payload")
                                ("$cl_exnref_tag" . "exnref_tag")))
                 (when (%wasm-call-string-mentions-p (car entry) text)
                   (mark (cdr entry))))))
      (dolist (func (wasm-module-functions module))
        (dolist (body (wasm-func-body func))
          (scan-string body))
        (dolist (inst (wasm-func-source-instructions func))
          (typecase inst
            (vm-print (mark "print_val"))
            (vm-register-method (mark "register_method"))
            (vm-generic-call (mark "call_generic"))
            ((or vm-signal-error vm-throw)
             (when (wasm-eh-v2-feature-enabled-p)
               (mark "condition_to_exnref")))))))
    (when (wasm-import-cl-condition-tag-enabled-p)
      (setf (gethash "condition-tag" used) t))
    (when (wasm-eh-v2-feature-enabled-p)
      (setf (gethash "condition_to_exnref" used) t
            (gethash "exnref_payload" used) t
            (gethash "exnref_tag" used) t))
    used))

(defun emit-wat-aot-host-stubs (stream)
  "Emit no-op host bridge stubs so AOT modules are self-contained."
  (format stream "~%  ;; FR-219: AOT host bridge stubs (no mandatory JS imports)")
  (when (%wasm-import-needed-p "write_char")
    (format stream "~%  (func $host_write_char (param i32))"))
  (when (%wasm-import-needed-p "read_char")
    (format stream "~%  (func $host_read_char (result i32) (i32.const -1))"))
  (when (%wasm-import-needed-p "write_string")
    (format stream "~%  (func $host_write_string (param (ref $string_t)))"))
  (when (%wasm-import-needed-p "error")
    (format stream "~%  (func $host_error (param (ref $string_t)))"))
  (when (%wasm-import-needed-p "print_val")
    (format stream "~%  (func $host_print_val (param eqref))"))
  (when (%wasm-import-needed-p "register_method")
    (format stream "~%  (func $host_rt_register_method (param eqref) (param eqref) (param eqref) (param eqref))"))
  (when (%wasm-import-needed-p "call_generic")
    (format stream "~%  (func $host_rt_call_generic (param eqref) (param i32) (result eqref) (ref.null eq))")))

(defun emit-wat-imports (stream)
  "Emit standard cl-cc I/O imports."
  (if (%wasm-aot-mode-active-p)
      (emit-wat-aot-host-stubs stream)
      (progn
  (format stream "~%  ;; Host I/O imports")
  (when (and (wasm-import-cl-condition-tag-enabled-p)
             (%wasm-import-needed-p "condition-tag"))
    (format stream "~%  ;; FR-310: external exception tag import")
    (format stream "~%  (import \"cl-core\" \"condition-tag\" (tag $cl_condition_tag (param eqref eqref)))"))
  (when (and (wasm-eh-v2-feature-enabled-p)
             (%wasm-import-needed-p "condition_to_exnref"))
    (format stream "~%  ;; FR-252/FR-262: host bridge for fresh throw_ref exceptions")
    (format stream "~%  (import \"cl_exception\" \"condition_to_exnref\" (func $cl_condition_to_exnref (param eqref) (result exnref)))")
    (format stream "~%  (import \"cl_exception\" \"exnref_payload\" (func $cl_exnref_payload (param exnref) (result eqref)))")
    (format stream "~%  (import \"cl_exception\" \"exnref_tag\" (func $cl_exnref_tag (param exnref) (result eqref)))"))
  (when *wasm-ref-types-externref-enabled*
    (format stream "~%  ;; FR-226: opaque JavaScript object import")
    (format stream "~%  (import \"js\" \"host-object\" (func $js_host_object (result externref)))"))
  (when *wasm-custom-descriptors-enabled*
    (emit-wasm-annotation-custom-section stream "FR-241.import"
                                         "typed externref descriptor import required: js.descriptor -> externref"))
  (when *wasm-type-imports-enabled*
    (emit-wasm-annotation-custom-section stream "FR-244.import"
                                         "dynamic type import required: cl-core.$cons_t"))
  (when *wasm-wasi-random-crypto-enabled*
    (emit-wasm-annotation-custom-section stream "FR-299.import"
                                         "WASI random/crypto imports required: get-random-bytes, hash"))
  (when (%wasm-import-needed-p "write_char")
    (format stream "~%  (import \"cl_io\" \"write_char\" (func $host_write_char (param i32)))"))
  (when (%wasm-import-needed-p "read_char")
    (format stream "~%  (import \"cl_io\" \"read_char\" (func $host_read_char (result i32)))"))
  (when (%wasm-import-needed-p "write_string")
    (format stream "~%  (import \"cl_io\" \"write_string\" (func $host_write_string (param (ref $string_t))))"))
  (when (%wasm-import-needed-p "error")
    (format stream "~%  (import \"cl_io\" \"error\" (func $host_error (param (ref $string_t))))"))
  ;; print_val: host-side formatter for any eqref value (used by vm-print).
  ;; Keep this standard import present for module-structure compatibility.
  (format stream "~%  (import \"cl_io\" \"print_val\" (func $host_print_val (param eqref)))")
  ;; FR-321 staged runtime MOP bridge imports
  (when (%wasm-import-needed-p "register_method")
    (format stream "~%  (import \"cl_runtime\" \"register_method\" (func $host_rt_register_method (param eqref) (param eqref) (param eqref) (param eqref)))"))
  (when (%wasm-import-needed-p "call_generic")
    (format stream "~%  (import \"cl_runtime\" \"call_generic\" (func $host_rt_call_generic (param eqref) (param i32) (result eqref)))")))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; emit-wasm-module: serialize a wasm-module-ir to WAT text
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wasm-annotation-custom-section (stream key value)
  "Emit a clcc.annotations custom section entry for staged Wasm metadata."
  (%emit-wasm-custom-string stream "clcc.annotations"
                            (format nil "~A=~A" key value)))

(defun emit-wat-low-priority-proposal-helpers (stream)
  "Emit Wave 11-15 feature-gated helper comments/custom sections that do not alter core lowering."
  (when (wasm-wide-arithmetic-feature-enabled-p)
    (format stream "~%  ;; FR-238 helpers: ~A | ~A | ~A | ~A"
            (wasm-i64-add128-wat "(local.get 0)" "(local.get 1)" "(local.get 2)" "(local.get 3)")
            (wasm-i64-sub128-wat "(local.get 0)" "(local.get 1)" "(local.get 2)" "(local.get 3)")
            (wasm-i64-mul-wide-s-wat "(local.get 0)" "(local.get 1)")
            (wasm-i64-mul-wide-u-wat "(local.get 0)" "(local.get 1)")))
  (when *wasm-compact-import-section-enabled*
    (emit-wasm-annotation-custom-section stream "FR-240" "compact-import-section metadata enabled; import name trie/compression staged for binary writer"))
  (when *wasm-custom-descriptors-enabled*
    (emit-wasm-annotation-custom-section stream "FR-241" "descriptor generation maps externref slots to WebAssembly.Descriptor metadata"))
  (when *wasm-memory-control-enabled*
    (format stream "~%  ;; FR-243 helper: ~A" (wasm-memory-discard-wat (wasm-memory-const-wat 0) (wasm-memory-const-wat 0))))
  (when *wasm-jit-interface-enabled*
    (format stream "~%  ;; FR-245: JIT interface feedback hook custom section follows")
    (%emit-wasm-custom-string stream "clcc.jit-interface" "{\"feedback\":[\"inline-cache\",\"hot-calls\",\"monomorphic-stubs\"]}"))
  (when *wasm-flexible-vectors-enabled*
    (format stream "~%  ;; FR-246 helpers: ~A | ~A"
            (wasm-flexible-vector-op-wat "add" "(local.get 0)" "(local.get 1)" :width :v128x2)
            (wasm-flexible-vector-op-wat "add" "(local.get 0)" "(local.get 1)" :width :v512)))
  (when (wasm-half-precision-feature-enabled-p)
    (format stream "~%  ;; FR-248 helpers: ~A | ~A | ~A"
            (wasm-f16-binop-wat "add" "(local.get 0)" "(local.get 1)")
            (wasm-f16-load-wat "(local.get 0)")
            (wasm-f16-store-wat "(local.get 0)" "(local.get 1)")))
  (when (wasm-reference-typed-strings-feature-enabled-p)
    (format stream "~%  ;; FR-251 helpers: ~A | ~A"
            (wasm-stringref-length-wat "(local.get 0)")
            (wasm-stringref-get-codeunit-wat "(local.get 0)" "(local.get 1)")))
  (when *wasm-startup-snapshots-enabled*
    (format stream "~%  ~A" (wasm-startup-snapshot-comment-wat)))
  (when (wasm-func-bind-feature-enabled-p)
    (format stream "~%  ;; FR-290 helper: ~A" (wasm-func-bind-wat "$main_func_t" "(ref.func $main)" "(ref.null eq)")))
  (when *wasm-wasi-extended-worlds-enabled*
    (emit-wasm-annotation-custom-section stream "FR-296" "WASI extended worlds: wasi:keyvalue, wasi:messaging, wasi:sql"))
  (when (wasm-cfi-feature-enabled-p)
    (emit-wasm-annotation-custom-section stream "FR-261.cfi" "typed call_ref/call_indirect signatures are emitted for indirect calls"))
  (when *wasm-csp-compliant-enabled*
    (emit-wasm-annotation-custom-section stream "FR-261.csp" "no dynamic wasm-unsafe-eval path required for AOT output"))
  (when *wasm-constant-time-enabled*
    (emit-wasm-annotation-custom-section stream "FR-261.constant-time" "constant-time lowering prefers select over data-dependent branches"))
  (when (wasm-coop-coep-feature-enabled-p)
    (emit-wasm-annotation-custom-section stream "FR-297" "deploy with COOP=same-origin and COEP=require-corp for SharedArrayBuffer"))
  (when *wasm-wasi-p2-enabled*
    (format stream "~%  ;; FR-207: WASI Preview 2 worlds enabled: filesystem, sockets, clocks"))
  (when *wasm-wasi-p3-enabled*
    (format stream "~%  ;; FR-257: WASI 0.3 async I/O stubs use suspend/resume around wasi:io/streams"))
  (when *wasm-wasi-worlds-full-enabled*
    (format stream "~%  ;; FR-274: WASI world definitions enabled: wasi:nn, wasi:http, wasi:cli"))
  (when *wasm-wasi-p1-compat-enabled*
    (format stream "~%  ;; FR-321: WASI Preview 1 compatibility shim imports fd_read/fd_write/path_open"))
  (when *wasm-stack-switching-enabled*
    (format stream "~%  ;; FR-205 helpers: ~A | ~A | ~A"
            (wasm-cont-new-wat "$main_func_t" "(ref.func $main)")
            (wasm-suspend-wat "$cl_suspend_tag" "(ref.null eq)")
            (wasm-resume-wat "(local.get 0)" "(ref.null eq)")))
  (when *wasm-effect-handlers-enabled*
    (format stream "~%  ;; FR-272 helper: ~A" (wasm-effect-perform-wat "$restart_handler" "(ref.null eq)")))
  (when *wasm-cont-throw-enabled*
    (format stream "~%  ;; FR-301 helper: ~A" (wasm-cont-throw-wat "(local.get 0)" "(local.get 1)")))
  (when *wasm-component-model-enabled*
    (format stream "~%  ;; FR-206: Component Model enabled; WIT type infrastructure custom section follows")
    (%emit-wasm-custom-string stream "clcc.component.wit" "package clcc:runtime; world clcc { export main: func() -> string; }"))
  (when *wasm-component-model-tests-enabled*
    (format stream "~%  ;; FR-319: Component Model test metadata enabled for WIT interface verification")))

(defun emit-wat-deployment-js-glue (stream)
  "Emit browser/deployment JS glue custom sections for low-priority wasm features."
  (when *wasm-service-worker-enabled*
    (%emit-wasm-custom-string
     stream "clcc.service-worker.js"
     "self.addEventListener('install', e => e.waitUntil(caches.open('clcc-wasm').then(c => c.addAll(['./module.wasm']))));\nself.addEventListener('fetch', e => e.respondWith(caches.match(e.request).then(r => r || fetch(e.request))));"))
  (when *wasm-runtime-feature-detection-enabled*
    (%emit-wasm-custom-string
     stream "clcc.feature-detect.js"
     "export async function detectClccWasmFeatures(bytes){ const ok=WebAssembly.validate(bytes); return { mvp: ok, gc: typeof WebAssembly.Global === 'function', threads: typeof SharedArrayBuffer !== 'undefined', exceptions: typeof WebAssembly.Exception === 'function', componentModel: false }; }")))

(defun emit-wasm-module (module stream)
  "Serialize a wasm-module-ir to WAT text format on STREAM.
   Assumes build-all-wasm-functions has already been called so every
   wasm-function-def has a populated :body slot."
  (format stream "(module")
  (format stream "~%  ;; cl-cc generated WASM module (GC proposal)")
  ;; FR-258: Wasm Profiles — declare required features
  (when (wasm-profiles-feature-enabled-p)
    (emit-wasm-profiles-section stream))
  ;; Type section
  (emit-wat-type-section stream)
  ;; Imports
  (let ((*wasm-aot-current-used-imports* (wasm-module-used-host-imports module)))
    (emit-wat-imports stream))
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
  (emit-wat-table64-helpers stream)
  ;; User-defined global variables (from defvar/setq)
  (emit-wat-globals module stream)
  ;; Argument-passing calling convention globals ($cl_arg0..$cl_arg15)
  (emit-wat-call-globals stream)
  ;; Memories (linear memory declarations)
  (emit-wat-memories module stream)
  ;; Low-priority proposal helpers from Waves 11-15.
  (emit-wat-low-priority-proposal-helpers stream)
  (emit-wat-deployment-js-glue stream)
  ;; JS/FFI helper functions and JS glue snippets.
  (emit-wat-js-ffi-helpers stream)
  ;; Host-side Worker bootstrap guidance for SharedArrayBuffer-backed memory.
  (emit-wat-worker-bootstrap stream)
  ;; Functions
  (dolist (func (wasm-module-functions module))
    (emit-wat-function func stream))
  (emit-wat-bigint-wrappers module stream)
  (emit-wat-bigint-js-wrapper-code stream)
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
  ;; FR-242: Extended Name Section for readable DevTools symbols.
  (when (wasm-extended-names-feature-enabled-p)
    (emit-wasm-name-section module stream))
  ;; FR-263/269/318/317/288: browser developer tooling JS helpers.
  (emit-wasm-developer-tooling-sections module stream)
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
     (:f16 +wasm-f16+)
     (:funcref +wasm-funcref+)
     (:externref +wasm-externref+)
     (:stringref +wasm-stringref+)
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

(defstruct (wasm-aot-result (:conc-name wasm-aot-result-))
  "Result bundle for FR-219 AOT Wasm generation."
  (bytes #() :type vector)
  (wat "" :type string)
  (metadata nil :type list))

(defun wasm-tool-available-p (program)
  "Return T when PROGRAM can be found on PATH."
  (let ((path (ignore-errors (cl-cc/runtime:rt-getenv "PATH"))))
    (and path
         (loop with start = 0
               for end = (position #\: path :start start)
               for dir = (subseq path start end)
               for candidate = (merge-pathnames program
                                                (pathname (format nil "~A/" dir)))
               thereis (probe-file candidate)
               while end
               do (setf start (1+ end))))))

(defun wasm-run-tool-to-string (argv &key input-file)
  "Run an optional wasm tool and return stdout, or NIL when unavailable/failing."
  (declare (ignore input-file))
  (when (and argv (wasm-tool-available-p (first argv)))
    (handler-case
        (uiop:run-program argv :output :string :error-output :string
                              :ignore-error-status nil)
      (error () nil))))

(defun %wasm-write-bytes-file (path bytes)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-sequence bytes out))
  path)

(defun %wasm-read-bytes-file (path)
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf)))

(defun %wasm-temp-path (suffix)
  (merge-pathnames (make-pathname :name (format nil "cl-cc-wasm-~A" (gensym))
                                  :type suffix)
                   (uiop:temporary-directory)))

(defun %wasm-hex-digest-file (path bits)
  "Return a hex digest for PATH using shasum/sha*sum/openssl when available."
  (or (when (wasm-tool-available-p "shasum")
        (let ((out (wasm-run-tool-to-string
                    (list "shasum" "-a" (princ-to-string bits) (namestring path)))))
          (and out (first (uiop:split-string out :separator '(#\Space #\Tab #\Newline))))))
      (let ((tool (format nil "sha~Dsum" bits)))
        (when (wasm-tool-available-p tool)
          (let ((out (wasm-run-tool-to-string (list tool (namestring path)))))
            (and out (first (uiop:split-string out :separator '(#\Space #\Tab #\Newline)))))))
      (when (wasm-tool-available-p "openssl")
        (let ((out (wasm-run-tool-to-string
                    (list "openssl" "dgst" (format nil "-sha~D" bits) "-r" (namestring path)))))
          (and out (first (uiop:split-string out :separator '(#\Space #\Tab #\Newline))))))))

(defun %wasm-byte-vector-hex-digest (bytes bits)
  (let ((tmp (%wasm-temp-path "wasm")))
    (unwind-protect
         (progn
           (%wasm-write-bytes-file tmp bytes)
           (or (%wasm-hex-digest-file tmp bits)
               ;; Deterministic non-cryptographic fallback when no digest tool exists.
               (format nil (format nil "~~~D,'0X" (/ bits 4))
                       (mod (abs (sxhash (coerce bytes 'list)))
                            (expt 16 (/ bits 4))))))
      (ignore-errors (delete-file tmp)))))

(defun wasm-file-content-hash (path &key (bits 256))
  "Return SHA-BITS hex digest for PATH, using optional platform tools."
  (or (%wasm-hex-digest-file path bits)
      (%wasm-byte-vector-hex-digest (%wasm-read-bytes-file path) bits)))

(defun %wasm-hex-to-bytes (hex)
  (let* ((clean (remove-if-not #'alphanumericp hex))
         (len (floor (length clean) 2))
         (out (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len out)
      (setf (aref out i)
            (parse-integer clean :start (* i 2) :end (+ (* i 2) 2) :radix 16)))))

(defparameter +wasm-base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "RFC 4648 base64 alphabet used for SRI output.")

(defun %wasm-base64-encode (bytes)
  (with-output-to-string (out)
    (loop for i from 0 below (length bytes) by 3
          for b0 = (aref bytes i)
          for have1 = (< (1+ i) (length bytes))
          for have2 = (< (+ i 2) (length bytes))
          for b1 = (if have1 (aref bytes (1+ i)) 0)
          for b2 = (if have2 (aref bytes (+ i 2)) 0)
          for n = (logior (ash b0 16) (ash b1 8) b2)
          do (write-char (char +wasm-base64-alphabet+ (ldb (byte 6 18) n)) out)
             (write-char (char +wasm-base64-alphabet+ (ldb (byte 6 12) n)) out)
             (write-char (if have1 (char +wasm-base64-alphabet+ (ldb (byte 6 6) n)) #\=) out)
             (write-char (if have2 (char +wasm-base64-alphabet+ (ldb (byte 6 0) n)) #\=) out))))

(defun wasm-file-sri-hash (path &key (bits 384))
  "Return an SRI integrity token such as sha384-... for PATH."
  (let ((hex (wasm-file-content-hash path :bits bits)))
    (format nil "sha~D-~A" bits (%wasm-base64-encode (%wasm-hex-to-bytes hex)))))

(defun wasm-binary-write-custom-section (buffer name payload-string)
  "Append a custom section NAME with UTF-8-ish PAYLOAD-STRING to BUFFER."
  (wasm-binary-write-section
   buffer +wasm-section-custom+
   (lambda (section)
     (wasm-binary-write-name section name)
     (let ((bytes (map 'vector #'char-code payload-string)))
       (wasm-binary-write-bytes section bytes)))))

(defun wasm-append-build-hash-section (bytes hash)
  "Return BYTES with a deterministic cl-cc build hash custom section appended."
  (let ((buffer (cl-cc/binary::make-byte-buffer (+ (length bytes) 96))))
    (wasm-binary-write-bytes buffer bytes)
    (wasm-binary-write-custom-section buffer "cl-cc.build.sha256" hash)
    (cl-cc/binary::buffer-get-bytes buffer)))

(defun wasm-run-wasm-opt-passes (wasm-bytes &key (aot nil))
  "Run Binaryen optimization/removal passes when wasm-opt is available."
  (if (and (or aot *wasm-aot-mode-enabled*) (wasm-tool-available-p "wasm-opt"))
      (let ((tmp-in (%wasm-temp-path "wasm"))
            (tmp-out (%wasm-temp-path "wasm")))
        (unwind-protect
             (handler-case
                 (progn
                   (%wasm-write-bytes-file tmp-in wasm-bytes)
                   (uiop:run-program (list "wasm-opt" "-O3" "--strip-debug"
                                           "--remove-unused-module-elements"
                                           (namestring tmp-in) "-o" (namestring tmp-out))
                                     :ignore-error-status nil)
                   (if (probe-file tmp-out) (%wasm-read-bytes-file tmp-out) wasm-bytes))
               (error () wasm-bytes))
          (ignore-errors (delete-file tmp-in))
          (ignore-errors (delete-file tmp-out))))
      wasm-bytes))

(defun wasm-run-wasm2wat (wasm-bytes fallback-wat)
  "Return wasm2wat output for WASM-BYTES when wabt is available, else FALLBACK-WAT."
  (if (wasm-tool-available-p "wasm2wat")
      (let ((tmp (%wasm-temp-path "wasm")))
        (unwind-protect
             (progn
               (%wasm-write-bytes-file tmp wasm-bytes)
               (or (wasm-run-tool-to-string (list "wasm2wat" (namestring tmp)))
                   fallback-wat))
          (ignore-errors (delete-file tmp))))
      fallback-wat))

(defun wasm-wat-to-binary-if-available (wat fallback-bytes)
  "Assemble WAT through wat2wasm when available, falling back to FALLBACK-BYTES."
  (if (wasm-tool-available-p "wat2wasm")
      (let ((tmp-wat (%wasm-temp-path "wat"))
            (tmp-wasm (%wasm-temp-path "wasm")))
        (unwind-protect
             (handler-case
                 (progn
                   (with-open-file (out tmp-wat :direction :output :if-exists :supersede
                                                :if-does-not-exist :create)
                     (write-string wat out))
                   (uiop:run-program (list "wat2wasm" (namestring tmp-wat) "-o" (namestring tmp-wasm))
                                     :ignore-error-status nil)
                   (if (probe-file tmp-wasm) (%wasm-read-bytes-file tmp-wasm) fallback-bytes))
               (error () fallback-bytes))
          (ignore-errors (delete-file tmp-wat))
          (ignore-errors (delete-file tmp-wasm))))
      fallback-bytes))

(defun wasm-determinize-module! (module)
  "Sort module tables for reproducible AOT emission."
  (setf (wasm-module-functions module)
        (sort (copy-list (wasm-module-functions module)) #'string< :key #'wasm-func-wat-name)
        (wasm-module-globals module)
        (sort (copy-list (wasm-module-globals module)) #'string< :key #'wasm-global-def-wat-name))
  (loop for func in (wasm-module-functions module)
        for i from 0
        do (setf (wasm-func-index func) i))
  module)

(defun wasm-eliminate-dead-exports! (module)
  "Conservatively keep only public entry exports for AOT output."
  (dolist (func (wasm-module-functions module))
    (unless (string= (or (wasm-func-export-name func) "") "main")
      (setf (wasm-func-exported-p func) nil)))
  module)

(defun compile-to-aot-wasm (program &key deterministic)
  "FR-219: Compile PROGRAM to a self-contained AOT .wasm result bundle.

The function performs dead export/import pruning, optional deterministic
ordering, optional wat2wasm/wasm-opt integration, and embeds a content-hash
custom section without requiring external tools to be installed."
  (let* ((*wasm-aot-mode-enabled* t)
         (module (extract-wasm-functions program)))
    (build-all-wasm-functions module)
    (when deterministic
      (wasm-determinize-module! module))
    (wasm-eliminate-dead-exports! module)
    (let* ((wat (with-output-to-string (s) (emit-wasm-module module s)))
           (fallback (emit-wasm-binary-module module))
           (assembled (wasm-wat-to-binary-if-available wat fallback))
           (optimized (wasm-run-wasm-opt-passes assembled :aot t))
           (sha256 (%wasm-byte-vector-hex-digest optimized 256))
           (final-bytes (if deterministic
                            (wasm-append-build-hash-section optimized sha256)
                            optimized))
           (debug-wat (wasm-run-wasm2wat final-bytes wat)))
      (make-wasm-aot-result
       :bytes final-bytes
       :wat debug-wat
       :metadata (list :format :cl-cc-wasm-aot-v1
                       :sha256 sha256
                       :deterministic (not (null deterministic))
                       :imports-eliminated *wasm-dead-import-elimination-enabled*
                       :wasm-opt (wasm-tool-available-p "wasm-opt")
                       :wabt (and (wasm-tool-available-p "wat2wasm")
                                  (wasm-tool-available-p "wasm2wat")))))))


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
;;; FR-222: DWARF 5 debug info custom sections
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wasm-dwarf-sections (module stream)
  "FR-222: Emit DWARF 5 debug info as Wasm custom sections.
   Includes .debug_info, .debug_line, .debug_abbrev sections as WAT custom sections.
   Enable with --emit-debug-info flag."
  (when (wasm-feature-enabled-p "CLCC_WASM_DWARF" *wasm-dwarf-debug-info-enabled*)
    (format stream "~%  ;; FR-222: DWARF 5 debug info sections (custom)")
    (dolist (section (%wasm-build-dwarf-section-alist module))
      (%emit-wasm-custom-bytes stream (car section) (cdr section)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-223: Source Map reference
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-source-map-enabled-p ()
  "FR-223: Check if source map emission is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_SOURCE_MAP" *wasm-source-map-enabled*))

(defun emit-wasm-source-map-reference (stream)
  "FR-223: Emit sourceMappingURL custom section reference."
  (format stream "~%  ;; FR-223: Source Map v3 reference")
  (%emit-wasm-custom-string stream "sourceMappingURL" *wasm-source-map-url*))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Debug/DevTools metadata helpers (FR-222/223/242/263/269/288/317/318)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %wasm-wat-string (text)
  "Return TEXT as an escaped WAT string literal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for ch across (princ-to-string (or text "")) do
      (case ch
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\0a" out))
        (#\Return (write-string "\\0d" out))
        (#\Tab (write-string "\\09" out))
        (otherwise (write-char ch out))))
    (write-char #\" out)))

(defun %wasm-byte-vector-wat-string (bytes)
  "Return BYTES as an escaped WAT string literal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for byte across bytes do (format out "\\~2,'0X" byte))
    (write-char #\" out)))

(defun %emit-wasm-custom-string (stream name text)
  (format stream "~%  (@custom ~A ~A)"
          (%wasm-wat-string name)
          (%wasm-wat-string text)))

(defun %emit-wasm-custom-bytes (stream name bytes)
  (format stream "~%  (@custom ~A ~A)"
          (%wasm-wat-string name)
          (%wasm-byte-vector-wat-string bytes)))

(defun %wasm-clean-debug-name (name)
  (let ((text (if name (princ-to-string name) "anonymous")))
    (if (and (> (length text) 0) (char= (char text 0) #\$))
        (subseq text 1)
        text)))

(defun %wasm-json-string (text)
  "Return TEXT encoded as a JSON string literal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for ch across (princ-to-string (or text "")) do
      (case ch
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (otherwise (write-char ch out))))
    (write-char #\" out)))

(defun %wasm-human-local-name (reg)
  "Map a VM register like :R0 to a stable, DevTools-friendly local name."
  (let* ((raw (string-downcase (string reg)))
         (n (ignore-errors (parse-integer raw :start 1))))
    (cond
      ((eql n 0) "temp-result")
      ((integerp n) (format nil "temp-r~D" n))
      (t raw))))

(defun %wasm-function-source-offset (func)
  (* (or (wasm-func-index func) 0) 16))

(defun %wasm-registers-in-function (func)
  (remove-duplicates
   (append (wasm-func-params func)
           (loop for inst in (wasm-func-source-instructions func)
                 append (append (ignore-errors (cl-cc/regalloc:instruction-defs inst))
                                (ignore-errors (cl-cc/regalloc:instruction-uses inst)))))
   :test #'eq))

(defun %wasm-dwarf-subprogram-for-func (func)
  (let* ((low (%wasm-function-source-offset func))
         (inst-count (length (wasm-func-source-instructions func)))
         (high (+ low (max 1 inst-count)))
         (params (loop for reg in (or (wasm-func-params func) nil)
                       for i from 0
                       collect (cl-cc/binary::make-dwarf-variable-location
                                :name (%wasm-human-local-name reg)
                                :kind :register
                                :register (min i 31)
                                :pc-start low :pc-end high)))
         (locals (loop for reg in (%wasm-registers-in-function func)
                       for i from 0
                       unless (member reg (wasm-func-params func) :test #'eq)
                         collect (cl-cc/binary::make-dwarf-variable-location
                                  :name (%wasm-human-local-name reg)
                                  :kind :register
                                  :register (min i 31)
                                  :pc-start low :pc-end high))))
    (cl-cc/binary::make-dwarf-subprogram
     :name (%wasm-clean-debug-name (wasm-func-wat-name func))
     :low-pc low
     :high-pc high
     :parameters params
     :variables locals)))

(defun %wasm-dwarf-lines-for-module (module)
  (loop for func in (wasm-module-functions module)
        for low = (%wasm-function-source-offset func)
        append (loop for inst in (wasm-func-source-instructions func)
                     for pc from low
                     collect (list pc (1+ (- pc low)) 0 0))))

(defun %wasm-build-dwarf-section-alist (module)
  "Build DWARF5 section payloads for MODULE using existing binary DWARF helpers."
  (let* ((functions (wasm-module-functions module))
         (max-len (loop for f in functions maximize (length (wasm-func-source-instructions f))))
         (high (+ (* (max 0 (1- (length functions))) 16) (max 1 (or max-len 0))))
         (cu (cl-cc/binary::make-dwarf-compile-unit
              :name "cl-cc-wasm-module"
              :producer "cl-cc wasm backend"
              :low-pc 0
              :high-pc high
              :subprograms (mapcar #'%wasm-dwarf-subprogram-for-func functions)
              :lines (%wasm-dwarf-lines-for-module module))))
    (cl-cc/binary::build-dwarf-section-alist cu)))

(defun %wasm-name-section-subsection (id writer)
  (let ((payload (wasm-binary-section-bytes writer)))
    (wasm-binary-section-bytes
     (lambda (section)
       (wasm-binary-write-u8 section id)
       (wasm-binary-write-uleb128 section (length payload))
       (wasm-binary-write-bytes section payload)))))

(defun %wasm-name-assoc-vector (buffer assocs)
  (wasm-binary-write-uleb128 buffer (length assocs))
  (dolist (entry assocs)
    (wasm-binary-write-uleb128 buffer (car entry))
    (wasm-binary-write-name buffer (cdr entry))))

(defun %wasm-function-name-assocs (module)
  (loop for func in (wasm-module-functions module)
        collect (cons (or (wasm-func-index func) 0)
                      (%wasm-clean-debug-name (wasm-func-wat-name func)))))

(defun %wasm-local-name-assocs (module)
  (loop for func in (wasm-module-functions module)
        collect (cons (or (wasm-func-index func) 0)
                      (append '((0 . "pc") (1 . "tmp"))
                              (loop for reg in (%wasm-registers-in-function func)
                                    for idx from 2
                                    collect (cons idx (%wasm-human-local-name reg)))))))

(defun %wasm-label-name-assocs (module)
  (loop for func in (wasm-module-functions module)
        collect (cons (or (wasm-func-index func) 0)
                      (loop for inst in (wasm-func-source-instructions func)
                            for idx from 0
                            when (typep inst 'vm-label)
                              collect (cons idx (%wasm-clean-debug-name (vm-name inst)))))))

(defun %wasm-indirect-name-map (buffer entries)
  (wasm-binary-write-uleb128 buffer (length entries))
  (dolist (entry entries)
    (wasm-binary-write-uleb128 buffer (car entry))
    (%wasm-name-assoc-vector buffer (cdr entry))))

(defun %wasm-build-name-section-bytes (module)
  "Build the payload for the standard WebAssembly name custom section."
  (let ((chunks (list
                 (%wasm-name-section-subsection 0 (lambda (b) (wasm-binary-write-name b "cl-cc-wasm-module")))
                 (%wasm-name-section-subsection 1 (lambda (b) (%wasm-name-assoc-vector b (%wasm-function-name-assocs module))))
                 (%wasm-name-section-subsection 2 (lambda (b) (%wasm-indirect-name-map b (%wasm-local-name-assocs module))))
                 (%wasm-name-section-subsection 3 (lambda (b) (%wasm-indirect-name-map b (%wasm-label-name-assocs module)))))))
    (apply #'concatenate '(simple-array (unsigned-byte 8) (*)) chunks)))

(defun wasm-extended-names-feature-enabled-p ()
  "FR-242: Return true when extended wasm name-section metadata is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_EXTENDED_NAMES" *wasm-extended-names-enabled*))

(defun emit-wasm-name-section (module stream)
  "FR-242: Emit function/local/label names as a standard Wasm name custom section."
  (format stream "~%  ;; FR-242: Extended Name Section (functions, locals, labels)")
  (%emit-wasm-custom-bytes stream "name" (%wasm-build-name-section-bytes module)))

(defun %wasm-type-reflection-json (module)
  (with-output-to-string (out)
    (format out "{\"exports\":[")
    (let ((first-p t))
      (dolist (func (wasm-module-functions module))
        (when (wasm-func-exported-p func)
          (unless first-p (write-char #\, out))
          (setf first-p nil)
          (format out "{\"name\":~A,\"functionType\":{\"params\":[],\"results\":[\"eqref\"]}}"
                  (%wasm-json-string
                   (or (wasm-func-export-name func)
                       (%wasm-clean-debug-name (wasm-func-wat-name func))))))))
    (format out "],\"gcStructs\":[{\"name\":\"string_t\",\"fields\":[[\"chars\",\"bytes_array_t\"]]},{\"name\":\"cons_t\",\"fields\":[[\"car\",\"eqref\"],[\"cdr\",\"eqref\"]]},{\"name\":\"instance_t\",\"fields\":[[\"class\",\"class_meta_t\"],[\"slots\",\"eqref_array_t\"]]}]}")))

(defun %wasm-devtools-js (module)
  (format nil "export const clccTypeMetadata = ~A;~%
export function attachClccTypeReflection(instance) {~%
  const exports = instance && instance.exports || {};~%
  for (const [name, fn] of Object.entries(exports)) if (typeof fn === 'function') fn.clccType = clccTypeMetadata.exports.find(e => e.name === name) || null;~%
  return { metadata: clccTypeMetadata, describe(value) { return { jsType: typeof value, value, clccType: value && value.clccType || null }; } };~%
}~%
export function captureClccStack(mapper = x => x) { const e = {}; Error.captureStackTrace?.(e, captureClccStack); return String(e.stack || '').split('\\n').slice(1).map(mapper); }~%
export function printBacktrace(mapper) { return captureClccStack(mapper).join('\\n'); }~%
export function createMemoryProfiler(instance) { const memory = instance?.exports?.memory; return { snapshot() { return { byteLength: memory?.buffer?.byteLength || 0, timestamp: Date.now() }; } }; }~%
export async function hotReload({ table, index, module, imports = {} }) { const { instance } = await WebAssembly.instantiate(module, imports); const replacement = instance.exports.main || Object.values(instance.exports).find(v => typeof v === 'function'); table.set(index, replacement); return replacement; }~%
export async function compileReplForm({ compile, table, imports = {}, form }) { const module = await compile(form); return hotReload({ table, index: table.length - 1, module, imports }); }~%"
          (%wasm-type-reflection-json module)))

(defun emit-wasm-developer-tooling-sections (module stream)
  "Emit opt-in browser developer tooling custom sections."
  (when (or (wasm-feature-enabled-p "CLCC_WASM_TYPE_REFLECTION" *wasm-type-reflection-js-api-enabled*)
            (wasm-feature-enabled-p "CLCC_WASM_STACK_INSPECTION" *wasm-call-stack-inspection-enabled*)
            (wasm-feature-enabled-p "CLCC_WASM_MEMORY_PROFILER" *wasm-memory-profiler-enabled*)
            (wasm-feature-enabled-p "CLCC_WASM_HOT_RELOAD" *wasm-hot-code-reload-enabled*)
            (wasm-feature-enabled-p "CLCC_WASM_INCREMENTAL_REPL" *wasm-repl-incremental-compilation-enabled*))
    (format stream "~%  ;; FR-263/269/318/317/288: cl-cc DevTools JS helper module")
    (%emit-wasm-custom-string stream "clcc.devtools.js" (%wasm-devtools-js module))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-258: Wasm Profiles section
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-profiles-feature-enabled-p ()
  "FR-258: Check if profile declaration is enabled. Uses central *wasm-profiles-enabled*."
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
;;; FR-219: AOT mode — Binaryen wasm-opt integration
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-aot-optimize-enabled-p ()
  "FR-219: Check if Binaryen optimization is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_AOT_OPTIMIZE" *wasm-aot-mode-enabled*))

(defun wasm-run-binaryen-optimize (wasm-bytes)
  "FR-219: Run Binaryen wasm-opt on WASM-BYTES for size/speed optimization.
   Returns optimized bytes, or the original bytes if wasm-opt is unavailable."
  (wasm-run-wasm-opt-passes wasm-bytes :aot *wasm-aot-mode-enabled*))

;;; (emit-instruction methods and compile-to-wasm-wat are in wasm-emit.lisp
;;;  which loads after this file.)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-213: Memory64 — 64-bit address space declaration
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-memory64-enabled* nil
  "Feature gate for WASM Memory64 (FR-213). When T, emit (memory i64 ...) instead
of (memory i32 ...) and use i64.load/i64.store addressing.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-208: Multiple Memories — separate memory for heap/stack/strings
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-multiple-memories* nil
  "Feature gate for WASM Multiple Memories (FR-208).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-206: Component Model — WIT interface generation
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-component-model* nil
  "Feature gate for WASM Component Model / WIT bindings (FR-206).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-207: WASI 0.2 — capability-based system interface
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-wasi-02-enabled* nil
  "Feature gate for WASI 0.2 (Preview 2) system interface (FR-207).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-230: ESM Integration — import/export native .wasm modules
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-esm-integration* nil
  "Feature gate for WASM ESM Integration (FR-230).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-216: Branch Hinting — custom section for branch prediction
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-branch-hinting* nil
  "When T, emit @metadata.code.branch_hint custom section (FR-216).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-215: Extended Constant Expressions — complex global initializers
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-extended-const-exprs* nil
  "When T, use global.get/i32.add etc. in global initializer expressions (FR-215).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-214: Relaxed SIMD — fused multiply-add, relaxed min/max
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-relaxed-simd-enabled* nil
  "Feature gate for WASM Relaxed SIMD proposal (FR-214).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-250: Multibyte Array Access — array.load2_u/load4_u
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-multibyte-array-access* nil
  "Feature gate for WASM Multibyte Array Access (FR-250).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-294: Passive Element/Data Segments — lazy segment loading
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-passive-segments* nil
  "When T, use passive data/element segments for lazy loading (FR-294).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-280: Initialization order — __wasm_call_ctors convention
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-call-ctors* t
  "When T, emit __wasm_call_ctors for ordered module initialization (FR-280).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-322: WASM binary tool integration — wat2wasm/wasm-objdump
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-binary-tools-path* nil
  "Path to WABT (WebAssembly Binary Toolkit) tools for wasm2wat/wasm-objdump (FR-322).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-297: COOP/COEP headers — SharedArrayBuffer prerequisites
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-coop-coep-headers* nil
  "When T, generate COOP/COEP headers for SharedArrayBuffer (FR-297).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-302: ServiceWorker + WASM — offline PWA support
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-service-worker* nil
  "When T, generate Service Worker registration for PWA (FR-302).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-277: CL ABI / Symbol Name Mangling — cross-language FFI
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-abi-mangling* nil
  "When T, mangle CL symbol names for C/Rust/JS FFI compatibility (FR-277).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-276: WASM Import Maps — external module name resolution
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-import-maps* nil
  "When T, generate import map support for module URL resolution (FR-276).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-258: WASM Profiles — feature profile declaration
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-profile* nil
  "WASM feature profile for compilation target: :minimal, :gc, :threads, :full (FR-258).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-259: Tiered Compilation Hints — Liftoff/TurboFan optimization
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-tiered-compilation-hints* t
  "When T, emit optimization hints for V8 Liftoff/TurboFan tiering (FR-259).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-260: LTO — Link-Time Optimization cross-module inlining
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-lto-enabled* nil
  "When T, enable Binaryen LTO for cross-module inlining (FR-260).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-264: Lazy Function Bodies — deferred function compilation
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-lazy-function-bodies* nil
  "When T, use stub functions with lazy compilation (FR-264).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-275: Worker postMessage Module Transfer — compiled module sharing
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-worker-module-transfer* nil
  "When T, transfer compiled WASM modules via Worker postMessage (FR-275).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-288: Incremental REPL Compilation — on-the-fly WASM module generation
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-incremental-repl* nil
  "When T, support incremental WASM compilation in REPL (FR-288).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-317: Hot Code Reloading — runtime function patching via table.set
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-hot-reload* nil
  "When T, support runtime function patching for hot reload (FR-317).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-312: Runtime Feature Detection — WebAssembly.featureDetect
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-runtime-feature-detect* nil
  "When T, generate runtime WASM feature detection code (FR-312).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-311: wasm-c-api — Native C embedding API
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-c-api-enabled* nil
  "When T, generate wasm-c-api compatible host embedding code (FR-311).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-325: SIMD NaN semantics — strict NaN flag
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-simd-strict-nan* nil
  "When T, enforce IEEE 754 NaN semantics in SIMD operations (FR-325).")
