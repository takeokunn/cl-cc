(in-package :cl-cc/codegen)

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
