;;;; packages/codegen/src/wasm-sections.lisp - WAT module structural sections
;;;
;;; WAT module header: predefined GC type section, globals, exception tags,
;;; function table, table64 helpers, elem segment.

(in-package :cl-cc/codegen)

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
