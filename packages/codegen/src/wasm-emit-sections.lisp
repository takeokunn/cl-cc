(in-package :cl-cc/codegen)

(defun wasm-env-true-p (value)
  "Return T when VALUE represents an enabled boolean environment flag." 
  (and value
       (member (string-downcase value)
               '("1" "true" "yes" "on" "enabled")
               :test #'string=)))

(defun wasm-env-false-p (value)
  "Return T when VALUE explicitly disables a boolean environment flag."
  (and value
       (member (string-downcase value)
               '("0" "false" "no" "off" "disabled")
               :test #'string=)))

(defun wasm-feature-enabled-p (env-name default)
  "Return feature gate state with ENV-NAME overriding DEFAULT when present." 
  (let ((env (ignore-errors (sb-ext:posix-getenv env-name))))
    (cond ((wasm-env-true-p env) t)
          ((wasm-env-false-p env) nil)
          (t default))))

(defun wasm-tail-call-feature-enabled-p ()
  "Return T when wasm tail-call feature is enabled.
Environment override: CLCC_WASM_TAILCALL=1|0"
  (wasm-feature-enabled-p "CLCC_WASM_TAILCALL" *wasm-tail-call-enabled*))

(defun wasm-eh-feature-enabled-p ()
  "FR-204: Return T when Wasm Exception Handling is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_EH" *wasm-eh-enabled*))

(defun wasm-typed-refs-feature-enabled-p ()
  "FR-212: Return T when Wasm Typed Function References is enabled."
  (wasm-feature-enabled-p
   "CLCC_WASM_TYPED_REFS"
   (or (and (boundp '*wasm-typed-func-refs-enabled*)
            *wasm-typed-func-refs-enabled*)
       *wasm-typed-refs-enabled*)))

(defun wasm-eh-v2-feature-enabled-p ()
  "FR-252: Return T when Wasm EH v2 (try_table/throw_ref) is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_EH_V2"
                          (or *wasm-eh-v2-enabled*
                              *wasm-exception-handling-v2-enabled*)))

(defun wasm-js-exception-bridge-feature-enabled-p ()
  "FR-262: Return T when Wasm JS Exception Bridge is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_JS_EXCEPTION_BRIDGE"
                          (or *wasm-js-exception-bridge-enabled*
                              *wasm-exception-js-api-enabled*)))

(defun wasm-exnref-feature-enabled-p ()
  "FR-271: Return T when Wasm exnref support is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_EXNREF" *wasm-exnref-enabled*))

(defun wasm-gc-exception-payload-feature-enabled-p ()
  "FR-289: Return T when GC struct exception payload is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_GC_EXCEPTION_PAYLOAD"
                          (or *wasm-gc-exception-payload-enabled*
                              *wasm-struct-exception-payload-enabled*)))

(defun wasm-extended-const-ref-func-feature-enabled-p ()
  "FR-215/FR-291: Return T when Extended Const with ref.func is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_EXTENDED_CONST_REF_FUNC" *wasm-extended-const-ref-func-enabled*))

(defun wasm-continuation-exceptions-feature-enabled-p ()
  "FR-301: Return T when continuation exception injection is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_CONTINUATION_EXCEPTIONS"
                          (or *wasm-continuation-exceptions-enabled*
                              *wasm-cont-throw-enabled*)))

(defun wasm-exception-tag-linking-feature-enabled-p ()
  "FR-310: Return T when exception tag import/export is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_EXCEPTION_TAG_LINKING"
                          (or *wasm-exception-tag-linking-enabled*
                              *wasm-exception-tag-import-export-enabled*)))

(defun wasm-catch-all-ref-feature-enabled-p ()
  "Return T when catch_all_ref support is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_CATCH_ALL_REF" *wasm-catch-all-ref-enabled*))

(defun wasm-import-cl-condition-tag-enabled-p ()
  "FR-310: Return T when $cl_condition_tag is imported from cl-core instead of defined locally.
Default is NIL to preserve existing standalone modules."
  (wasm-env-true-p (ignore-errors (sb-ext:posix-getenv "CLCC_WASM_IMPORT_CL_CONDITION_TAG"))))

(defun wasm-simd-feature-enabled-p ()
  "FR-202: Return T when Wasm SIMD128 is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_SIMD" *wasm-simd-enabled*))

(defun wasm-strict-nan-enabled-p ()
  "Return T when strict IEEE 754 NaN semantics are enforced."
  (wasm-feature-enabled-p "CLCC_WASM_STRICT_NAN" *wasm-strict-nan*))

(defun wasm-relaxed-simd-feature-enabled-p ()
  "FR-214: Return T when Relaxed SIMD is enabled (requires SIMD, not strict NaN)."
  (and (wasm-simd-feature-enabled-p)
       (not (wasm-strict-nan-enabled-p))
       (wasm-feature-enabled-p "CLCC_WASM_RELAXED_SIMD" *wasm-relaxed-simd-enabled*)))

(defun wasm-threads-feature-enabled-p ()
  "FR-203: Return T when Wasm Threads/Atomics emission is enabled. Default NIL."
  (wasm-feature-enabled-p "CLCC_WASM_THREADS" *wasm-threads-enabled*))

(defun wasm-multiple-memories-feature-enabled-p ()
  "FR-208: Return T when Wasm Multiple Memories is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_MULTIPLE_MEMORIES" *wasm-multiple-memories-enabled*))

(defun wasm-shared-everything-feature-enabled-p ()
  "FR-224: Return T when Shared Everything Threads GC is enabled."
  (and (wasm-threads-feature-enabled-p)
       (wasm-feature-enabled-p "CLCC_WASM_SHARED_EVERYTHING" *wasm-shared-everything-enabled*)))

(defun wasm-memory64-feature-enabled-p ()
  "FR-213: Return T when Memory64 is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_MEMORY64" *wasm-memory64-enabled*))

(defun wasm-table64-feature-enabled-p ()
  "FR-229/FR-292: Return T when table64 emission is enabled."
  (or (wasm-feature-enabled-p "CLCC_WASM_TABLE64" *wasm-table64-enabled*)
      (wasm-memory64-feature-enabled-p)))

(defun wasm-multi-memory-atomics-feature-enabled-p ()
  "FR-293: Return T when atomics should carry an explicit memory index."
  (and (wasm-threads-feature-enabled-p)
       (wasm-multiple-memories-feature-enabled-p)
       (wasm-feature-enabled-p "CLCC_WASM_MULTI_MEMORY_ATOMICS" t)))

(defun wasm-multi-memory-bulk-copy-feature-enabled-p ()
  "FR-300: Return T when cross-memory memory.copy forms are enabled."
  (and (wasm-multiple-memories-feature-enabled-p)
       (wasm-feature-enabled-p "CLCC_WASM_MULTI_MEMORY_BULK_COPY" t)))

(defun wasm-branch-hints-feature-enabled-p ()
  "FR-216: Return T when Branch Hinting is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_BRANCH_HINTS" *wasm-branch-hints-enabled*))

(defun wasm-js-promise-feature-enabled-p ()
  "FR-217: Return T when JS Promise Integration is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_JS_PROMISE" *wasm-js-promise-enabled*))

(defun wasm-string-builtins-feature-enabled-p ()
  "FR-218: Return T when String Builtins is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_STRING_BUILTINS" *wasm-string-builtins-enabled*))

(defun wasm-dwarf-feature-enabled-p ()
  "FR-222: Return T when DWARF debug info is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_DWARF" *wasm-dwarf-enabled*))

(defun wasm-wide-arithmetic-feature-enabled-p ()
  "FR-238: Return T when Wide Arithmetic (128-bit) is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_WIDE_ARITH" *wasm-wide-arithmetic-enabled*))

(defun wasm-gc-packed-fields-feature-enabled-p ()
  "FR-283: Return T when GC packed fields (i8/i16) are enabled."
  (wasm-feature-enabled-p "CLCC_WASM_GC_PACKED_FIELDS" *wasm-gc-packed-fields-enabled*))

(defun wasm-gc-bulk-array-ops-feature-enabled-p ()
  "FR-284: Return T when GC bulk array operations are enabled."
  (wasm-feature-enabled-p "CLCC_WASM_GC_BULK_ARRAY_OPS" *wasm-gc-bulk-array-ops-enabled*))

(defun wasm-gc-recursive-types-feature-enabled-p ()
  "FR-254: Return T when GC recursive types are enabled."
  (wasm-feature-enabled-p
   "CLCC_WASM_GC_RECURSIVE_TYPES"
   (if (boundp '*wasm-gc-recursive-types-enabled*)
       *wasm-gc-recursive-types-enabled*
       *wasm-gc-rec-types-enabled*)))

(defun wasm-gc-null-safety-feature-enabled-p ()
  "FR-270: Return T when GC null safety instructions are enabled."
  (wasm-feature-enabled-p "CLCC_WASM_GC_NULL_SAFETY" *wasm-gc-null-safety-enabled*))

(defun wasm-multi-value-feature-enabled-p ()
  "FR-235: Return T when Multi-value returns are enabled."
  (wasm-feature-enabled-p "CLCC_WASM_MULTI_VALUE" *wasm-multi-value-enabled*))

(defun wasm-bigint-feature-enabled-p ()
  "FR-236: Return T when JS BigInt integration is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_BIGINT"
                          (or *wasm-bigint-enabled*
                              *wasm-js-bigint-i64-enabled*)))

(defun wasm-bulk-table-feature-enabled-p ()
  "FR-237: Return T when Bulk Table Operations are enabled."
  (wasm-feature-enabled-p "CLCC_WASM_BULK_TABLE" *wasm-bulk-table-enabled*))

(defun wasm-extern-to-any-feature-enabled-p ()
  "FR-226/FR-286: Return T when externref ↔ anyref conversion is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_EXTERN_TO_ANY"
                          (and *wasm-extern-to-any-enabled*
                               *wasm-any-extern-convert-enabled*)))

(defun wasm-ref-eq-feature-enabled-p ()
  "FR-285: Return T when ref.eq identity comparison is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_REF_EQ" *wasm-ref-eq-enabled*))

(defun wasm-typed-select-feature-enabled-p ()
  "FR-279: Return T when typed select is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_TYPED_SELECT" *wasm-typed-select-enabled*))

(defun wasm-func-bind-feature-enabled-p ()
  "FR-290: Return T when func.bind partial application is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_FUNC_BIND" *wasm-func-bind-enabled*))

(defun wasm-custom-page-sizes-feature-enabled-p ()
  "FR-239: Return T when custom page-size metadata is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_CUSTOM_PAGE_SIZES" *wasm-custom-page-sizes-enabled*))

(defun wasm-half-precision-feature-enabled-p ()
  "FR-248: Return T when f16 helper emission is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_F16" *wasm-half-precision-enabled*))

(defun wasm-reference-typed-strings-feature-enabled-p ()
  "FR-251: Return T when stringref helpers are enabled."
  (wasm-feature-enabled-p "CLCC_WASM_STRINGREF" *wasm-reference-typed-strings-enabled*))

(defun wasm-cfi-feature-enabled-p ()
  "FR-261: Return T when CFI helper metadata is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_CFI" *wasm-cfi-enabled*))

(defun wasm-coop-coep-feature-enabled-p ()
  "FR-297: Return T when COOP/COEP deployment guidance is enabled."
  (or (wasm-feature-enabled-p "CLCC_WASM_COOP_COEP" *wasm-coop-coep-headers-enabled*)
      (wasm-feature-enabled-p "CLCC_WASM_COOP" *wasm-coop-enabled*)
      (wasm-feature-enabled-p "CLCC_WASM_COEP" *wasm-coep-enabled*)))

(defun wasm-element-init-feature-enabled-p ()
  "FR-281/FR-294: Return T when element segment init is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_ELEMENT_INIT" *wasm-element-init-enabled*))

(defun wasm-import-maps-feature-enabled-p ()
  "FR-276: Return T when Import Maps are enabled."
  (wasm-feature-enabled-p "CLCC_WASM_IMPORT_MAPS" *wasm-import-maps-enabled*))

(defun wasm-pgo-feature-enabled-p ()
  "FR-220: Return T when PGO is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_PGO" *wasm-pgo-enabled*))

(defun wasm-tiered-compilation-feature-enabled-p ()
  "FR-259: Return T when Tiered Compilation hints are enabled."
  (wasm-feature-enabled-p "CLCC_WASM_TIERED_COMPILATION" *wasm-tiered-compilation-enabled*))

(defun wasm-memory-index-type-wat ()
  "Return the index type for linear memory address operands."
  (if (wasm-memory64-feature-enabled-p) "i64" "i32"))

(defun wasm-memory-const-wat (value)
  "Return VALUE as an address-width WAT constant."
  (format nil "(~A.const ~D)" (wasm-memory-index-type-wat) value))

(defun wasm-memory-load-opcode-wat (&key (value-type :i32))
  "Return the scalar load opcode for linear-memory helpers."
  (if (wasm-memory64-feature-enabled-p)
      "i64.load"
      (ecase value-type
        (:i32 "i32.load")
        (:i64 "i64.load"))))

(defun wasm-memory-store-opcode-wat (&key (value-type :i32))
  "Return the scalar store opcode for linear-memory helpers."
  (if (wasm-memory64-feature-enabled-p)
      "i64.store"
      (ecase value-type
        (:i32 "i32.store")
        (:i64 "i64.store"))))

(defun wasm-memory-memarg-wat (&key (memory-index nil) (align 4) (offset 0))
  "Return a WAT memarg with an optional multi-memory index."
  (format nil "~@[ (memory ~D)~] align=~D offset=~D"
          (and (wasm-multiple-memories-feature-enabled-p) memory-index)
          align offset))

(defun wasm-linear-load-wat (addr-wat &key (memory-index +wasm-memory-gc-heap+)
                                      (value-type :i32) (offset 0))
  "Return a linear-memory load WAT expression honoring Memory64/FR-208."
  (let ((opcode (wasm-memory-load-opcode-wat :value-type value-type))
        (align (if (or (wasm-memory64-feature-enabled-p) (eq value-type :i64)) 8 4)))
    (format nil "(~A~A ~A)"
            opcode
            (wasm-memory-memarg-wat :memory-index memory-index :align align :offset offset)
            addr-wat)))

(defun wasm-linear-store-wat (addr-wat value-wat &key (memory-index +wasm-memory-gc-heap+)
                                             (value-type :i32) (offset 0))
  "Return a linear-memory store WAT expression honoring Memory64/FR-208."
  (let ((opcode (wasm-memory-store-opcode-wat :value-type value-type))
        (align (if (or (wasm-memory64-feature-enabled-p) (eq value-type :i64)) 8 4)))
    (format nil "(~A~A ~A ~A)"
            opcode
            (wasm-memory-memarg-wat :memory-index memory-index :align align :offset offset)
            addr-wat value-wat)))

(defun wasm-memory-size-wat (&key (memory-index nil))
  "Return memory.size or memory.size64 with an optional memory index."
  (format nil "(~A~@[ (memory ~D)~])"
          (if (wasm-memory64-feature-enabled-p) "memory.size64" "memory.size")
          (and (wasm-multiple-memories-feature-enabled-p) memory-index)))

(defun wasm-memory-grow-wat (pages-wat &key (memory-index nil))
  "Return memory.grow or memory.grow64 with an optional memory index."
  (format nil "(~A~@[ (memory ~D)~] ~A)"
          (if (wasm-memory64-feature-enabled-p) "memory.grow64" "memory.grow")
          (and (wasm-multiple-memories-feature-enabled-p) memory-index)
          pages-wat))

(defun wasm-memory-copy-wat* (dst-offset-wat src-offset-wat size-wat
                              &key (dst-memory +wasm-memory-gc-heap+)
                                   (src-memory +wasm-memory-gc-heap+))
  "Return memory.copy, using explicit memories for FR-300 when enabled."
  (if (wasm-multi-memory-bulk-copy-feature-enabled-p)
      (format nil "(memory.copy (memory ~D) ~A (memory ~D) ~A ~A)"
              dst-memory dst-offset-wat src-memory src-offset-wat size-wat)
      (format nil "(memory.copy ~A ~A ~A)" dst-offset-wat src-offset-wat size-wat)))

(defun wasm-memory-declaration-wat (name export-name min-pages &key max-pages shared-p)
  "Return a single WAT memory declaration."
  (format nil "(memory~@[ ~A~] (export ~S)~@[ i64~] ~D~@[ ~D~]~@[ shared~])~@[ ;; FR-239 page_size=~D~]"
          name export-name (wasm-memory64-feature-enabled-p) min-pages max-pages shared-p
          (and (wasm-custom-page-sizes-feature-enabled-p) *wasm-custom-page-size*)))

(defun emit-wat-linear-memory-helpers (stream)
  "Emit helper functions that materialize Memory64/multi-memory opcodes."
  (let ((addr-type (wasm-memory-index-type-wat))
        (value-type (if (wasm-memory64-feature-enabled-p) "i64" "i32")))
    (format stream "~%  ;; FR-213/FR-208: linear memory access helpers")
    (when (wasm-custom-page-sizes-feature-enabled-p)
      (format stream "~%  ;; FR-239: custom memory page size requested: ~D bytes" *wasm-custom-page-size*))
    (format stream "~%  (func $clcc_heap_load_word (param ~A) (result ~A)" addr-type value-type)
    (format stream "~%    ~A" (wasm-linear-load-wat "(local.get 0)" :memory-index +wasm-memory-gc-heap+))
    (format stream "~%  )")
    (format stream "~%  (func $clcc_heap_store_word (param ~A) (param ~A)" addr-type value-type)
    (format stream "~%    ~A" (wasm-linear-store-wat "(local.get 0)" "(local.get 1)" :memory-index +wasm-memory-gc-heap+))
    (format stream "~%  )")
    (format stream "~%  (func $clcc_memory_size (result ~A)" addr-type)
    (format stream "~%    ~A" (wasm-memory-size-wat :memory-index +wasm-memory-gc-heap+))
    (format stream "~%  )")
    (format stream "~%  (func $clcc_memory_grow (param ~A) (result ~A)" addr-type addr-type)
    (format stream "~%    ~A" (wasm-memory-grow-wat "(local.get 0)" :memory-index +wasm-memory-gc-heap+))
    (format stream "~%  )")
    (when (wasm-multi-memory-bulk-copy-feature-enabled-p)
      (format stream "~%  (func $clcc_heap_to_stack_copy (param ~A) (param ~A) (param ~A)" addr-type addr-type addr-type)
      (format stream "~%    ~A" (wasm-memory-copy-wat* "(local.get 0)" "(local.get 1)" "(local.get 2)"
                                                        :dst-memory +wasm-memory-stack+
                                                        :src-memory +wasm-memory-gc-heap+))
      (format stream "~%  )")
      (format stream "~%  (func $clcc_string_intern_copy (param ~A) (param ~A) (param ~A)" addr-type addr-type addr-type)
      (format stream "~%    ~A" (wasm-memory-copy-wat* "(local.get 0)" "(local.get 1)" "(local.get 2)"
                                                        :dst-memory +wasm-memory-strings+
                                                        :src-memory +wasm-memory-gc-heap+))
      (format stream "~%  )"))))

(defun emit-wat-memories (module stream)
  "Emit linear memory declarations for MODULE."
  (declare (ignore module))
  (let ((shared-p (wasm-threads-feature-enabled-p)))
    (cond
      ((wasm-multiple-memories-feature-enabled-p)
       (format stream "~%  ;; FR-208: multiple memories: 0=stack, 1=GC heap, 2=strings")
       (format stream "~%  ~A" (wasm-memory-declaration-wat "$stack_memory" "memory" 1 :max-pages (and shared-p 65536) :shared-p shared-p))
       (format stream "~%  ~A" (wasm-memory-declaration-wat "$gc_heap_memory" "gc_heap" 1 :max-pages (and shared-p 65536) :shared-p shared-p))
       (format stream "~%  ~A" (wasm-memory-declaration-wat "$string_memory" "strings" 1 :max-pages (and shared-p 65536) :shared-p shared-p)))
      (shared-p
       (format stream "~%  ;; FR-203: shared linear memory")
       (format stream "~%  ~A" (wasm-memory-declaration-wat nil "memory" 1 :max-pages 65536 :shared-p t)))
      (t
       (format stream "~%  ~A" (wasm-memory-declaration-wat nil "memory" 1))))
    (emit-wat-linear-memory-helpers stream)))

(defun emit-wat-eh-v2-helper (stream)
  "FR-252: Emit Wasm EH v2 (try_table/throw_ref) helper declarations."
  (format stream "~%  ;; FR-252/FR-271/FR-315: EH v2 try_table/throw_ref/exnref helpers")
  (format stream "~%  (func $cl_exception_get_tag (param exnref) (result eqref)")
  (format stream "~%    (call $cl_exnref_tag (local.get 0)))")
  (format stream "~%  (func $cl_exception_get_payload (param exnref) (result eqref)")
  (format stream "~%    (call $cl_exnref_payload (local.get 0)))")
  (format stream "~%  (func $cl_throw_condition_ref (param eqref)")
  (format stream "~%    (throw_ref (call $cl_condition_to_exnref (local.get 0))))")
  (format stream "~%  (func $cl_cont_throw (param eqref)")
  (if (wasm-continuation-exceptions-feature-enabled-p)
      (progn
        (format stream "~%    ;; FR-301: cont.throw opcode #x~X; host continuation object is staged as eqref" +wasm-cont-throw+)
        (format stream "~%    (throw_ref (call $cl_condition_to_exnref (local.get 0))))")
      (format stream "~%    (call $cl_throw_condition_ref (local.get 0))"))
  (format stream ")")))

(defun emit-wat-js-conversion-helpers (stream)
  "FR-262: Emit JS ↔ Wasm exception bridge helper functions."
  (format stream "~%  ;; FR-262: JS exception bridge helpers")
  (format stream "~%  ;; JS host snippet:")
  (format stream "~%  ;; const clConditionTag = new WebAssembly.Tag({ parameters: ['externref', 'externref'] });")
  (format stream "~%  ;; const imports = { 'cl-core': { 'condition-tag': clConditionTag },")
  (format stream "~%  ;;   cl_exception: {")
  (format stream "~%  ;;     condition_to_exnref: value => new WebAssembly.Exception(clConditionTag, [null, value]),")
  (format stream "~%  ;;     exnref_payload: exn => exn.getArg(clConditionTag, 1),")
  (format stream "~%  ;;     exnref_tag: exn => exn.getArg(clConditionTag, 0) } };")
  (format stream "~%  ;; try { instance.exports.entry(); } catch (e) { if (e instanceof WebAssembly.Exception) throw e; throw new WebAssembly.Exception(clConditionTag, [null, e]); }"))

(defun wasm-record-class-slot-layout! (target slot-names)
  "Record slot-name -> index mapping from a vm-class-def SLOT-NAMES list." 
  (loop for s in slot-names
        for idx from 0
        do (setf (gethash s (wasm-target-known-slot-indexes target)) idx)))

(defun wasm-record-class-slot-layout-for-class! (target class-name slot-names)
  "Record class-specific slot-name -> index mapping." 
  (let ((ht (make-hash-table :test #'equal)))
    (loop for s in slot-names
          for idx from 0
          do (setf (gethash s ht) idx))
    (setf (gethash class-name (wasm-target-class-slot-layouts target)) ht)
    (setf (gethash class-name (wasm-target-class-slot-orders target)) slot-names)
    ht))

(defun wasm-build-effective-slot-order (target class-name superclasses own-slot-names)
  "Build effective slot order: inherited (left-to-right DFS-ish) then own slots.

This is a conservative MOP-aware approximation for slot index lowering." 
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (labels ((push-unique (s)
               (unless (gethash s seen)
                 (setf (gethash s seen) t)
                 (push s result))))
      (dolist (sc superclasses)
        (let ((super-order (gethash sc (wasm-target-class-slot-orders target))))
          (dolist (s super-order)
            (push-unique s))))
      (dolist (s own-slot-names)
        (push-unique s))
      (nreverse result))))

(defun wasm-slot-index-for-object-slot (target obj-reg slot-name)
  "Resolve SLOT-NAME index using object->class->slot layout when available.

Fallback order:
1) object register class mapping + class slot layout
2) global known-slot-indexes table
3) 0 (staged fallback)"
  (let* ((class-name (gethash obj-reg (wasm-target-known-object-class-by-reg target)))
         (layout (and class-name
                      (gethash class-name (wasm-target-class-slot-layouts target)))))
    (or (and layout (gethash slot-name layout))
        (gethash slot-name (wasm-target-known-slot-indexes target))
        0)))

(defun wasm-slot-index-for (target slot-name)
  "Lookup slot index for SLOT-NAME recorded by prior vm-class-def emission.

Falls back to 0 when unknown to preserve staged lowering behavior." 
  (or (gethash slot-name (wasm-target-known-slot-indexes target)) 0))

(defmethod emit-instruction ((target wasm-target) (inst vm-class-def) stream)
  "Record class slot layout metadata for downstream slot lowering.

Wasm backend records static slot ordering and materializes a staged
$class_meta_t object for DST." 
  (let ((reg-map (wasm-target-reg-map target)))
    (let* ((effective-order (wasm-build-effective-slot-order
                             target
                             (vm-class-name-sym inst)
                             (or (vm-superclasses inst) nil)
                             (or (vm-slot-names inst) nil)))
            (slot-count (length effective-order)))
      (wasm-record-class-slot-layout! target effective-order)
      (wasm-record-class-slot-layout-for-class! target (vm-class-name-sym inst) effective-order)
      (setf (gethash (vm-dst inst) (wasm-target-known-class-by-reg target))
            (vm-class-name-sym inst))
      (format stream "~%    ~A"
              (reg-local-set
               reg-map
               (vm-dst inst)
               (format nil
                        "(struct.new $class_meta_t ~
 (struct.new $symbol_t (struct.new $string_t (array.new $bytes_array_t (i32.const 0) (i32.const 0))) (ref.null eq)) ~
 (array.new $eqref_array_t (ref.null eq) (i32.const ~D)) ~
 (struct.new $symbol_t (struct.new $string_t (array.new $bytes_array_t (i32.const 0) (i32.const 0))) (ref.null eq)) ~
 (array.new $eqref_array_t (ref.null eq) (i32.const 0)))"
                        slot-count))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-find-class) stream)
  "Track class-reg provenance for downstream vm-make-obj lowering." 
  (let* ((reg-map (wasm-target-reg-map target))
         (src-reg (vm-src inst))
         (class-name (or (gethash src-reg (wasm-target-known-class-by-reg target))
                         src-reg)))
    (setf (gethash (vm-dst inst) (wasm-target-known-class-by-reg target)) class-name)
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst) (reg-local-ref reg-map src-reg)))))

(defmethod emit-instruction ((target wasm-target) (inst vm-make-obj) stream)
  "Emit staged instance allocation and track object register class provenance." 
  (let* ((reg-map (wasm-target-reg-map target))
         (class-name (gethash (vm-class-reg inst) (wasm-target-known-class-by-reg target)))
         (layout (and class-name (gethash class-name (wasm-target-class-slot-layouts target))))
         (slot-count (if layout (hash-table-count layout) 0))
         (dst (vm-dst inst)))
    (setf (gethash dst (wasm-target-known-object-class-by-reg target)) class-name)
    (format stream "~%    ~A"
            (reg-local-set reg-map dst
                           (format nil
                                   "(struct.new $instance_t ~A (array.new $eqref_array_t (ref.null eq) (i32.const ~D)))"
                                   (reg-local-ref reg-map (vm-class-reg inst))
                                   slot-count)))))

(defmethod emit-instruction ((target wasm-target) (inst vm-slot-read) stream)
  "Emit CLOS slot read via Wasm GC struct/array ops.

Current staged lowering reads from $instance_t slots array and uses a
conservative slot index 0 placeholder until per-class slot index mapping is
threaded through the Wasm backend." 
  (let* ((reg-map (wasm-target-reg-map target))
         (obj-reg (vm-obj-reg inst))
         (obj (reg-local-ref reg-map (vm-obj-reg inst)))
         (slot-idx (wasm-slot-index-for-object-slot target obj-reg (vm-slot-name-sym inst)))
          (slots (format nil "(struct.get $instance_t 1 ~A)"
                         (format nil "(ref.cast (ref $instance_t) ~A)"
                                 (wasm-ref-as-non-null-wat obj)))))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (format nil "(array.get $eqref_array_t ~A (i32.const ~D))" slots slot-idx)))))

(defmethod emit-instruction ((target wasm-target) (inst vm-slot-write) stream)
  "Emit CLOS slot write via Wasm GC struct/array ops.

Uses staged slot index 0 placeholder pending class-layout-driven index mapping." 
  (let* ((reg-map (wasm-target-reg-map target))
         (obj-reg (vm-obj-reg inst))
         (obj (reg-local-ref reg-map (vm-obj-reg inst)))
         (val (reg-local-ref reg-map (vm-value-reg inst)))
         (slot-idx (wasm-slot-index-for-object-slot target obj-reg (vm-slot-name-sym inst)))
          (slots-local (wasm-reg-map-tmp-index reg-map))
          (obj-instance (format nil "(ref.cast (ref $instance_t) ~A)"
                                (wasm-ref-as-non-null-wat obj)))
          (slots-get (format nil "(struct.get $instance_t 1 ~A)" obj-instance)))
    ;; Stage through local tmp, update array cell, then write back via struct.set.
    ;; This makes the instance-slot field mutation explicit in Wasm GC IR.
    (format stream "~%    (local.set ~D ~A)" slots-local slots-get)
    (format stream "~%    (array.set $eqref_array_t (ref.cast (ref $eqref_array_t) (local.get ~D)) (i32.const ~D) ~A)" slots-local slot-idx val)
    (format stream "~%    (struct.set $instance_t 1 ~A (ref.cast (ref $eqref_array_t) (local.get ~D)))" obj-instance slots-local)))

(defmethod emit-instruction ((target wasm-target) (inst vm-slot-boundp) stream)
  "Emit SLOT-BOUNDP with ref.test/null-safe GC instance and slot-cell check."
  (let* ((reg-map (wasm-target-reg-map target))
         (obj-reg (vm-obj-reg inst))
         (obj (reg-local-ref reg-map obj-reg))
         (slot-idx (wasm-slot-index-for-object-slot target obj-reg (vm-slot-name-sym inst)))
         (instance (format nil "(ref.cast (ref $instance_t) ~A)"
                           (wasm-ref-as-non-null-wat obj)))
         (slots (format nil "(struct.get $instance_t 1 ~A)" instance))
         (cell (format nil "(array.get $eqref_array_t ~A (i32.const ~D))" slots slot-idx)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (wasm-bool-to-i31
                            (format nil "(i32.and ~A (i32.eqz (ref.is_null ~A)))"
                                    (wasm-ref-test-wat "(ref $instance_t)" obj)
                                    cell))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Primary entry points
;;; ─────────────────────────────────────────────────────────────────────────────

(defun compile-to-wasm-wat (program)
  "Compile a vm-program to a WAT text string using the WASM GC backend.
   Returns a string containing the complete WAT module."
  (let ((module (extract-wasm-functions program)))
    ;; Build WAT trampoline bodies for all functions
    (build-all-wasm-functions module)
    ;; Serialize to WAT
    (with-output-to-string (s)
      (emit-wasm-module module s))))
