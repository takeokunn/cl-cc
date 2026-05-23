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
  (wasm-feature-enabled-p "CLCC_WASM_TYPED_REFS" *wasm-typed-refs-enabled*))

(defun wasm-eh-v2-feature-enabled-p ()
  "FR-252: Return T when Wasm EH v2 (try_table/throw_ref) is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_EH_V2" *wasm-eh-v2-enabled*))

(defun wasm-js-exception-bridge-feature-enabled-p ()
  "FR-262: Return T when Wasm JS Exception Bridge is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_JS_EXCEPTION_BRIDGE" *wasm-js-exception-bridge-enabled*))

(defun wasm-exnref-feature-enabled-p ()
  "FR-271: Return T when Wasm exnref support is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_EXNREF" *wasm-exnref-enabled*))

(defun wasm-gc-exception-payload-feature-enabled-p ()
  "FR-289: Return T when GC struct exception payload is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_GC_EXCEPTION_PAYLOAD" *wasm-gc-exception-payload-enabled*))

(defun wasm-extended-const-ref-func-feature-enabled-p ()
  "FR-215/FR-291: Return T when Extended Const with ref.func is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_EXTENDED_CONST_REF_FUNC" *wasm-extended-const-ref-func-enabled*))

(defun wasm-continuation-exceptions-feature-enabled-p ()
  "FR-301: Return T when continuation exception injection is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_CONTINUATION_EXCEPTIONS" *wasm-continuation-exceptions-enabled*))

(defun wasm-exception-tag-linking-feature-enabled-p ()
  "FR-310: Return T when exception tag import/export is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_EXCEPTION_TAG_LINKING" *wasm-exception-tag-linking-enabled*))

(defun wasm-catch-all-ref-feature-enabled-p ()
  "Return T when catch_all_ref support is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_CATCH_ALL_REF" *wasm-catch-all-ref-enabled*))

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
  (wasm-feature-enabled-p "CLCC_WASM_GC_RECURSIVE_TYPES" *wasm-gc-recursive-types-enabled*))

(defun wasm-gc-null-safety-feature-enabled-p ()
  "FR-270: Return T when GC null safety instructions are enabled."
  (wasm-feature-enabled-p "CLCC_WASM_GC_NULL_SAFETY" *wasm-gc-null-safety-enabled*))

(defun wasm-multi-value-feature-enabled-p ()
  "FR-235: Return T when Multi-value returns are enabled."
  (wasm-feature-enabled-p "CLCC_WASM_MULTI_VALUE" *wasm-multi-value-enabled*))

(defun wasm-bigint-feature-enabled-p ()
  "FR-236: Return T when JS BigInt integration is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_BIGINT" *wasm-bigint-enabled*))

(defun wasm-bulk-table-feature-enabled-p ()
  "FR-237: Return T when Bulk Table Operations are enabled."
  (wasm-feature-enabled-p "CLCC_WASM_BULK_TABLE" *wasm-bulk-table-enabled*))

(defun wasm-extern-to-any-feature-enabled-p ()
  "FR-226/FR-286: Return T when externref ↔ anyref conversion is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_EXTERN_TO_ANY" *wasm-extern-to-any-enabled*))

(defun wasm-ref-eq-feature-enabled-p ()
  "FR-285: Return T when ref.eq identity comparison is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_REF_EQ" *wasm-ref-eq-enabled*))

(defun wasm-typed-select-feature-enabled-p ()
  "FR-279: Return T when typed select is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_TYPED_SELECT" *wasm-typed-select-enabled*))

(defun wasm-func-bind-feature-enabled-p ()
  "FR-290: Return T when func.bind partial application is enabled."
  (wasm-feature-enabled-p "CLCC_WASM_FUNC_BIND" *wasm-func-bind-enabled*))

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
         (slots (format nil "(struct.get $instance_t 1 ~A)" obj)))
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
         (slots-local "$tmp")
         (slots-get (format nil "(struct.get $instance_t 1 ~A)" obj)))
    ;; Stage through local tmp, update array cell, then write back via struct.set.
    ;; This makes the instance-slot field mutation explicit in Wasm GC IR.
    (format stream "~%    (local.set ~A ~A)" slots-local slots-get)
    (format stream "~%    (array.set $eqref_array_t (local.get ~A) (i32.const ~D) ~A)" slots-local slot-idx val)
    (format stream "~%    (struct.set $instance_t 1 ~A (local.get ~A))" obj slots-local)))

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
