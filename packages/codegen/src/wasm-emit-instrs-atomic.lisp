(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-203/FR-256/FR-327: Atomic and fence instruction emitters
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %wasm-atomic-disabled-comment (stream inst)
  "Emit a conservative comment when threads are disabled for INST."
  (format stream "~%    ;; Wasm threads disabled; non-atomic fallback for ~A"
          (type-of inst)))

(defmethod emit-instruction ((target wasm-target) (inst cl-cc/vm::vm-atomic-cas) stream)
  "Lower VM CAS to Wasm atomic.rmw.cmpxchg when FR-203 is enabled."
  (let* ((reg-map (wasm-target-reg-map target))
         (width (wasm-atomic-width-for-inst inst 64)))
    (if (wasm-threads-feature-enabled-p)
        (progn
          (format stream "~%    ;; FR-203/FR-327 atomic CAS width=~D" width)
          (format stream "~%    ~A"
                  (reg-local-set reg-map (vm-dst inst)
                                 (wasm-atomic-result-box-wat
                                  (wasm-atomic-rmw-cmpxchg-wat
                                   reg-map
                                   (cl-cc/vm::vm-acas-addr inst)
                                   (cl-cc/vm::vm-acas-expected inst)
                                   (cl-cc/vm::vm-acas-newval inst)
                                   :width width)
                                  :width width))))
        (progn
          (%wasm-atomic-disabled-comment stream inst)
          (format stream "~%    ~A"
                  (reg-local-set reg-map (vm-dst inst)
                                 (reg-local-ref reg-map (cl-cc/vm::vm-acas-addr inst))))))))

(defmethod emit-instruction ((target wasm-target) (inst cl-cc/vm::vm-atomic-load) stream)
  "Lower VM atomic load to i32/i64.atomic.load when FR-203 is enabled."
  (let* ((reg-map (wasm-target-reg-map target))
         (width (wasm-atomic-width-for-inst inst 64)))
    (if (wasm-threads-feature-enabled-p)
        (progn
          (format stream "~%    ;; FR-203 atomic load width=~D" width)
          (format stream "~%    ~A"
                  (reg-local-set reg-map (vm-dst inst)
                                 (wasm-atomic-result-box-wat
                                  (wasm-atomic-load-wat reg-map (cl-cc/vm::vm-aload-addr inst)
                                                        :width width)
                                  :width width))))
        (progn
          (%wasm-atomic-disabled-comment stream inst)
          (format stream "~%    ~A"
                  (reg-local-set reg-map (vm-dst inst)
                                 (reg-local-ref reg-map (cl-cc/vm::vm-aload-addr inst))))))))

(defmethod emit-instruction ((target wasm-target) (inst cl-cc/vm::vm-atomic-store) stream)
  "Lower VM atomic store to i32/i64.atomic.store when FR-203 is enabled."
  (let* ((reg-map (wasm-target-reg-map target))
         (width (wasm-atomic-width-for-inst inst 64)))
    (if (wasm-threads-feature-enabled-p)
        (progn
          (format stream "~%    ;; FR-203 atomic store width=~D" width)
          (format stream "~%    ~A"
                  (wasm-atomic-store-wat reg-map
                                         (cl-cc/vm::vm-astore-addr inst)
                                         (cl-cc/vm::vm-astore-val inst)
                                         :width width)))
        (progn
          (%wasm-atomic-disabled-comment stream inst)
          (format stream "~%    (local.set ~D ~A)"
                  (wasm-reg-to-local reg-map (cl-cc/vm::vm-astore-addr inst))
                  (reg-local-ref reg-map (cl-cc/vm::vm-astore-val inst)))))))

(defmethod emit-instruction ((target wasm-target) (inst cl-cc/vm::vm-atomic-incf) stream)
  "Lower VM atomic increment/fetch-add to Wasm atomic.rmw.add."
  (let* ((reg-map (wasm-target-reg-map target))
         (width (wasm-atomic-width-for-inst inst 64)))
    (if (wasm-threads-feature-enabled-p)
        (progn
          (format stream "~%    ;; FR-203/FR-327 atomic fetch-add width=~D" width)
          (format stream "~%    ~A"
                  (reg-local-set reg-map (vm-dst inst)
                                 (wasm-atomic-result-box-wat
                                  (wasm-atomic-rmw-add-wat
                                   reg-map (cl-cc/vm::vm-aincf-addr inst) (cl-cc/vm::vm-aincf-delta inst)
                                   :width width)
                                  :width width))))
        (let ((old (reg-local-ref reg-map (cl-cc/vm::vm-aincf-addr inst)))
              (new (wasm-fixnum-box
                    (format nil "(i64.add ~A ~A)"
                            (wasm-fixnum-unbox reg-map (cl-cc/vm::vm-aincf-addr inst))
                            (wasm-fixnum-unbox reg-map (cl-cc/vm::vm-aincf-delta inst))))))
          (%wasm-atomic-disabled-comment stream inst)
          (format stream "~%    ~A" (reg-local-set reg-map (vm-dst inst) old))
          (format stream "~%    ~A" (reg-local-set reg-map (cl-cc/vm::vm-aincf-addr inst) new))))))

(defmethod emit-instruction ((target wasm-target) (inst cl-cc/vm::vm-atomic-swap) stream)
  "Lower VM atomic swap.  Wasm uses cmpxchg fallback shape until xchg is wired."
  (let ((reg-map (wasm-target-reg-map target)))
    (%wasm-atomic-disabled-comment stream inst)
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (reg-local-ref reg-map (cl-cc/vm::vm-aswap-addr inst))))
    (format stream "~%    ~A"
            (reg-local-set reg-map (cl-cc/vm::vm-aswap-addr inst)
                           (reg-local-ref reg-map (cl-cc/vm::vm-aswap-newval inst))))))

(defun %wasm-emit-fence-method (stream inst)
  "Shared helper for all fence variant emit-instruction methods.
   Emits atomic.fence when Wasm threads are enabled; otherwise emits a disabled comment."
  (if (wasm-threads-feature-enabled-p)
      (format stream "~%    atomic.fence")
      (%wasm-atomic-disabled-comment stream inst)))

;;; Data-driven fence emitters: all three fence classes share the same body.
(defmacro define-wasm-fence-emitters (&rest classes)
  "Generate identical emit-instruction methods for each fence class in CLASSES.
Each method ignores TARGET and delegates to %wasm-emit-fence-method."
  `(progn
     ,@(mapcar (lambda (class)
                 `(defmethod emit-instruction ((target wasm-target) (inst ,class) stream)
                    (declare (ignore target))
                    (%wasm-emit-fence-method stream inst)))
               classes)))

(define-wasm-fence-emitters
  cl-cc/vm::vm-memory-barrier
  cl-cc/vm::vm-load-fence
  cl-cc/vm::vm-store-fence)

(eval-when (:load-toplevel :execute)
  ;; Optional future VM classes requested by the Wasm threads feature plan.  The
  ;; current VM names fetch-add as VM-ATOMIC-INCF and fences as VM-MEMORY-BARRIER;
  ;; these methods attach automatically if/when aliases/classes are present.
  (when (find-class 'vm-atomic-add nil)
    (eval
     '(defmethod emit-instruction ((target wasm-target) (inst vm-atomic-add) stream)
        (let* ((reg-map (wasm-target-reg-map target))
               (width (wasm-atomic-width-for-inst inst 64))
               (dst (wasm-vm-slot-value inst 'dst))
               (addr (wasm-vm-slot-value inst 'addr-reg))
               (delta (or (wasm-vm-slot-value inst 'delta-reg)
                          (wasm-vm-slot-value inst 'val-reg))))
          (if (wasm-threads-feature-enabled-p)
              (format stream "~%    ~A"
                      (reg-local-set reg-map dst
                                     (wasm-atomic-result-box-wat
                                      (wasm-atomic-rmw-add-wat reg-map addr delta
                                                               :width width)
                                      :width width)))
              (%wasm-atomic-disabled-comment stream inst))))))
  (when (find-class 'vm-memory-fence nil)
    (eval
     '(defmethod emit-instruction ((target wasm-target) (inst vm-memory-fence) stream)
        (declare (ignore target))
        (if (wasm-threads-feature-enabled-p)
            (format stream "~%    atomic.fence")
            (%wasm-atomic-disabled-comment stream inst)))))
  (when (find-class 'vm-atomic-wait nil)
    (eval
     '(defmethod emit-instruction ((target wasm-target) (inst vm-atomic-wait) stream)
        (let* ((reg-map (wasm-target-reg-map target))
               (dst (wasm-vm-slot-value inst 'dst))
               (addr (wasm-vm-slot-value inst 'addr-reg))
               (expected (wasm-vm-slot-value inst 'expected-reg))
               (timeout (wasm-vm-slot-value inst 'timeout-reg)))
          (if (wasm-threads-feature-enabled-p)
              (format stream "~%    ~A"
                      (reg-local-set reg-map dst
                                     (wasm-atomic-result-box-wat
                                      (wasm-atomic-wait-wat reg-map addr expected timeout)
                                      :width 32)))
              (%wasm-atomic-disabled-comment stream inst))))))
  (when (find-class 'vm-atomic-notify nil)
    (eval
     '(defmethod emit-instruction ((target wasm-target) (inst vm-atomic-notify) stream)
        (let* ((reg-map (wasm-target-reg-map target))
               (dst (wasm-vm-slot-value inst 'dst))
               (addr (wasm-vm-slot-value inst 'addr-reg))
               (count (wasm-vm-slot-value inst 'count-reg)))
          (if (wasm-threads-feature-enabled-p)
              (let ((wat (wasm-atomic-notify-wat reg-map addr count)))
                (if dst
                    (format stream "~%    ~A"
                            (reg-local-set reg-map dst
                                           (wasm-atomic-result-box-wat wat :width 32)))
                    (format stream "~%    (drop ~A)" wat)))
              (%wasm-atomic-disabled-comment stream inst)))))))
