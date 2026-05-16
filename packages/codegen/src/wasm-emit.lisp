;;;; packages/emit/src/wasm-emit.lisp — WASM emit-instruction Methods and Entry Points
;;;;
;;;; Per-instruction emit-instruction methods for the wasm-target class,
;;;; plus the compile-to-wasm-wat entry point.
;;;;
;;;; WAT module structure emitters (emit-wat-type-section, emit-wasm-module, etc.)
;;;; and the wasm-target class are in wasm.lisp (loads before this file).
;;;;
;;;; Load order: after wasm.lisp.

(in-package :cl-cc/codegen)

(defparameter *wasm-tail-call-enabled* t
  "Feature gate for wasm tail-call opcodes.

When NIL, wasm tail-call lowering falls back to call_indirect.")

(defun wasm-env-true-p (value)
  "Return T when VALUE represents an enabled boolean environment flag." 
  (and value
       (member (string-downcase value)
               '("1" "true" "yes" "on" "enabled")
               :test #'string=)))

(defun wasm-tail-call-feature-enabled-p ()
  "Return T when wasm tail-call feature is enabled.

Environment override:
- CLCC_WASM_TAILCALL=1|0"
  (let ((env (ignore-errors (sb-ext:posix-getenv "CLCC_WASM_TAILCALL"))))
    (if env
        (wasm-env-true-p env)
        *wasm-tail-call-enabled*)))

(defmethod emit-instruction ((target wasm-target) (inst vm-const) stream)
  (let* ((reg-map (wasm-target-reg-map target))
         (val (vm-value inst))
         (dst (vm-dst inst))
         (wat-val (typecase val
                    (integer
                     (wasm-fixnum-box (format nil "(i64.const ~D)" val)))
                    (null "(ref.null eq)")
                    ((eql t) "(ref.i31 (i32.const 1))")
                    (t "(ref.null eq)"))))
    (format stream "~%    (local.set ~D ~A)"
            (wasm-reg-to-local reg-map dst)
            wat-val)))

(defmethod emit-instruction ((target wasm-target) (inst vm-move) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    (local.tee ~D (local.get ~D))"
            (wasm-reg-to-local reg-map (vm-dst inst))
            (wasm-reg-to-local reg-map (vm-src inst)))))

(defmethod emit-instruction ((target wasm-target) (inst vm-add) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (wasm-fixnum-box
                            (format nil "(i64.add ~A ~A)"
                                    (wasm-fixnum-unbox reg-map (vm-lhs inst))
                                    (wasm-fixnum-unbox reg-map (vm-rhs inst))))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-integer-add) stream)
  (emit-instruction target (make-vm-add :dst (vm-dst inst) :lhs (vm-lhs inst) :rhs (vm-rhs inst)) stream))

(defmethod emit-instruction ((target wasm-target) (inst vm-sub) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (wasm-fixnum-box
                            (format nil "(i64.sub ~A ~A)"
                                    (wasm-fixnum-unbox reg-map (vm-lhs inst))
                                    (wasm-fixnum-unbox reg-map (vm-rhs inst))))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-integer-sub) stream)
  (emit-instruction target (make-vm-sub :dst (vm-dst inst) :lhs (vm-lhs inst) :rhs (vm-rhs inst)) stream))

(defmethod emit-instruction ((target wasm-target) (inst vm-mul) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (wasm-fixnum-box
                            (format nil "(i64.mul ~A ~A)"
                                    (wasm-fixnum-unbox reg-map (vm-lhs inst))
                                    (wasm-fixnum-unbox reg-map (vm-rhs inst))))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-integer-mul) stream)
  (emit-instruction target (make-vm-mul :dst (vm-dst inst) :lhs (vm-lhs inst) :rhs (vm-rhs inst)) stream))

(defmethod emit-instruction ((target wasm-target) (inst vm-rotate) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (wasm-fixnum-box
                            (format nil "(i64.rotr ~A ~A)"
                                    (wasm-fixnum-unbox reg-map (vm-lhs inst))
                                    (wasm-fixnum-unbox reg-map (vm-rhs inst))))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-label) stream)
  (format stream "~%    ;; label: ~A" (vm-name inst)))

(defmethod emit-instruction ((target wasm-target) (inst vm-jump) stream)
  (format stream "~%    ;; jump ~A (trampoline handles this)" (vm-label-name inst)))

(defmethod emit-instruction ((target wasm-target) (inst vm-jump-zero) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ;; jump-zero ~A if local ~D (trampoline handles this)"
            (vm-label-name inst)
            (wasm-reg-to-local reg-map (vm-reg inst)))))

(defmethod emit-instruction ((target wasm-target) (inst vm-ret) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A (return)"
            (reg-local-ref reg-map (vm-reg inst)))))

(defmethod emit-instruction ((target wasm-target) (inst vm-halt) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A (return)"
            (reg-local-ref reg-map (vm-reg inst)))))

(defmethod emit-instruction ((target wasm-target) (inst vm-print) stream)
  (declare (ignore stream))
  ;; Actual print goes via $host_write_string import in the trampoline path.
  )

(defmethod emit-instruction ((target wasm-target) (inst vm-set-global) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    (global.set ~A ~A)"
            (vm-global-wat-name (vm-global-name inst))
            (reg-local-ref reg-map (vm-src inst)))))

(defmethod emit-instruction ((target wasm-target) (inst vm-get-global) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (format nil "(global.get ~A)"
                                   (vm-global-wat-name (vm-global-name inst)))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-call) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (let* ((func-ref (reg-local-ref reg-map (vm-func-reg inst))))
      (loop for arg in (vm-args inst) for i from 0 do
        (format stream "~%    (global.set $cl_arg~D ~A)" i (reg-local-ref reg-map arg)))
      (format stream "~%    ~A"
              (reg-local-set reg-map (vm-dst inst)
                              (format nil "(call_indirect (type $main_func_t) (table $funcref_table) ~A)"
                                      func-ref))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-func-ref) stream)
  "Materialize function table index in DST and track direct-call label hint." 
  (let* ((reg-map (wasm-target-reg-map target))
         (module (wasm-target-module target))
         (idx (or (and module (%wasm-function-index-for-label module (vm-label-name inst))) 0))
         (dst (vm-dst inst)))
    (setf (gethash dst (wasm-target-known-func-labels target))
          (vm-label-name inst))
    (format stream "~%    ~A"
            (reg-local-set reg-map dst (format nil "(i32.const ~D)" idx)))))

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

(defmethod emit-instruction ((target wasm-target) (inst vm-tail-call) stream)
  "Emit wasm tail-call instruction form when available.

Uses optimize helper to select opcode; defaults to return_call_indirect for
tail-position indirect call sites."
  (let ((reg-map (wasm-target-reg-map target)))
    (let* ((func-reg (vm-func-reg inst))
           (func-ref (reg-local-ref reg-map func-reg))
           (known-label (gethash func-reg (wasm-target-known-func-labels target)))
           (tailcall-enabled-p (wasm-tail-call-feature-enabled-p))
            (opcode (cl-cc/optimize:opt-wasm-select-tailcall-opcode
                     :tail-position-p t
                     :indirect-p t
                     :enabled-p tailcall-enabled-p))
            (opcode-str (if (eq opcode :return-call-indirect)
                            "return_call_indirect"
                            "call_indirect")))
      (loop for arg in (vm-args inst) for i from 0 do
        (format stream "~%    (global.set $cl_arg~D ~A)" i (reg-local-ref reg-map arg)))
      (cond
        ;; Direct tail-call path (FR-320 follow-up): if the callee register is
        ;; known to come from vm-func-ref and tail-call feature is enabled,
        ;; emit return_call with symbolic function name.
        ((and tailcall-enabled-p known-label)
         (let* ((direct-opcode (cl-cc/optimize:opt-wasm-select-direct-tailcall-opcode
                                :tail-position-p t
                                :enabled-p tailcall-enabled-p))
                (direct-opcode-str (if (eq direct-opcode :return-call)
                                       "return_call"
                                       "call")))
           (format stream "~%    ~A"
                   (reg-local-set reg-map (vm-dst inst)
                                  (format nil "(~A $~A)" direct-opcode-str known-label)))))
        ;; Direct non-tail fallback when feature disabled.
        (known-label
         (let* ((direct-opcode (cl-cc/optimize:opt-wasm-select-direct-tailcall-opcode
                                :tail-position-p nil
                                :enabled-p tailcall-enabled-p))
                (direct-opcode-str (if (eq direct-opcode :return-call)
                                       "return_call"
                                       "call")))
           (format stream "~%    ~A"
                   (reg-local-set reg-map (vm-dst inst)
                                  (format nil "(~A $~A)" direct-opcode-str known-label)))))
        ;; Indirect fallback.
        (t
         (format stream "~%    ~A"
                 (reg-local-set reg-map (vm-dst inst)
                                (format nil "(~A (type $main_func_t) (table $funcref_table) ~A)"
                                        opcode-str
                                        func-ref))))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-closure) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (let ((entry-idx (or (%wasm-function-index-for-label (wasm-target-module target)
                                                         (vm-label-name inst))
                         0)))
      (format stream "~%    ~A"
              (reg-local-set reg-map (vm-dst inst)
                             (format nil "(struct.new $closure_t (i32.const ~D) (ref.null $env_t))"
                                     entry-idx))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-cons) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (format nil "(struct.new $cons_t ~A ~A)"
                                   (reg-local-ref reg-map (vm-car-reg inst))
                                    (reg-local-ref reg-map (vm-cdr-reg inst)))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-make-array) stream)
  "Emit wasm-gc array allocation via array.new.

Uses $eqref_array_t as the canonical mutable eqref array representation."
  (let* ((reg-map (wasm-target-reg-map target))
         (init (if (vm-initial-element inst)
                   (reg-local-ref reg-map (vm-initial-element inst))
                   "(ref.null eq)"))
         (size (wasm-fixnum-unbox reg-map (vm-size-reg inst))))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (format nil "(array.new $eqref_array_t ~A ~A)" init size)))))

(defmethod emit-instruction ((target wasm-target) (inst vm-aref) stream)
  "Emit wasm-gc array element read via array.get." 
  (let* ((reg-map (wasm-target-reg-map target))
         (arr (reg-local-ref reg-map (vm-array-reg inst)))
         (idx (wasm-fixnum-unbox reg-map (vm-index-reg inst))))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (format nil "(array.get $eqref_array_t ~A ~A)" arr idx)))))

(defmethod emit-instruction ((target wasm-target) (inst vm-aset) stream)
  "Emit wasm-gc array element write via array.set." 
  (let* ((reg-map (wasm-target-reg-map target))
         (arr (reg-local-ref reg-map (vm-array-reg inst)))
         (idx (wasm-fixnum-unbox reg-map (vm-index-reg inst)))
         (val (reg-local-ref reg-map (vm-val-reg inst))))
    (format stream "~%    (array.set $eqref_array_t ~A ~A ~A)" arr idx val)))

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

(defun %wasm-empty-symbol-eqref ()
  "Return a staged symbol eqref literal.

Current backend keeps symbol payload conservative (empty string, null value cell)
while preserving non-null symbol identity at the Wasm GC type level." 
  "(struct.new $symbol_t (struct.new $string_t (array.new $bytes_array_t (i32.const 0) (i32.const 0))) (ref.null eq))")

(defun %wasm-reg-or-null-eqref (reg-map value)
  "Return eqref for VALUE in staged Wasm lowering.

- VM register keyword -> local reference
- symbol literal -> staged symbol object
- NIL/other literal -> `(ref.null eq)`" 
  (cond
    ((keywordp value)
     (reg-local-ref reg-map value))
    ((symbolp value)
     (%wasm-empty-symbol-eqref))
    (t
     "(ref.null eq)")))

(defmethod emit-instruction ((target wasm-target) (inst vm-register-method) stream)
  "Emit vm-register-method via staged runtime bridge import.

This preserves explicit integration points for FR-321 while allowing wasm code
to call into host/runtime generic-function registration." 
  (let* ((reg-map (wasm-target-reg-map target))
         (gf (reg-local-ref reg-map (vm-gf-reg inst)))
         (specializer (%wasm-reg-or-null-eqref reg-map (vm-method-specializer inst)))
         (qualifier (%wasm-reg-or-null-eqref reg-map (vm-method-qualifier inst)))
         (method (reg-local-ref reg-map (vm-method-reg inst))))
    (format stream "~%    (call $host_rt_register_method ~A ~A ~A ~A)"
            gf specializer qualifier method)))

(defmethod emit-instruction ((target wasm-target) (inst vm-generic-call) stream)
  "Emit vm-generic-call via staged runtime bridge import.

Arguments are marshaled through the standard wasm calling-convention globals
($cl_arg0..$cl_arg15) and passed with argc to the runtime bridge." 
  (let ((reg-map (wasm-target-reg-map target)))
    (let* ((args (vm-args inst))
           (argc (min (length args) +wasm-max-call-args+)))
      (loop for arg in args
            for i from 0
            while (< i +wasm-max-call-args+)
            do (format stream "~%    (global.set $cl_arg~D ~A)" i
                       (%wasm-reg-or-null-eqref reg-map arg)))
      (format stream "~%    ~A"
              (reg-local-set reg-map (vm-dst inst)
                             (format nil "(call $host_rt_call_generic ~A (i32.const ~D))"
                                     (reg-local-ref reg-map (vm-gf-reg inst))
                                     argc))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-car) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (format nil "(struct.get $cons_t 0 ~A)"
                                   (reg-local-ref reg-map (vm-src inst)))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-cdr) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (format nil "(struct.get $cons_t 1 ~A)"
                                   (reg-local-ref reg-map (vm-src inst)))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-null-p) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (wasm-bool-to-i31
                            (format nil "(ref.is_null ~A)"
                                    (reg-local-ref reg-map (vm-src inst))))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-not) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (format nil
                                    "(if (result eqref) (ref.is_null ~A) (then (ref.i31 (i32.const 1))) (else (ref.null eq)))"
                                    (reg-local-ref reg-map (vm-src inst)))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-logcount) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (wasm-fixnum-box
                            (format nil "(i64.popcnt ~A)"
                                    (wasm-fixnum-unbox reg-map (vm-src inst))))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-integer-length) stream)
  (let* ((reg-map (wasm-target-reg-map target))
         (src (wasm-fixnum-unbox reg-map (vm-src inst))))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (format nil
                                   "(if (result eqref) (i64.eqz ~A) (then ~A) (else ~A))"
                                   src
                                   (wasm-fixnum-box "(i64.const 0)")
                                   (wasm-fixnum-box
                                    (format nil "(i64.sub (i64.const 64) (i64.clz ~A))" src)))))))

;;; Catch-all for unsupported instructions
(defmethod emit-instruction ((target wasm-target) instruction stream)
  (declare (ignore target stream))
  (error "Unsupported WASM instruction: ~A" (type-of instruction)))

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
