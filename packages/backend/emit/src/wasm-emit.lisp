;;;; packages/backend/emit/src/wasm-emit.lisp — WASM emit-instruction Methods and Entry Points
;;;;
;;;; Per-instruction emit-instruction methods for the wasm-target class,
;;;; plus the compile-to-wasm-wat entry point.
;;;;
;;;; WAT module structure emitters (emit-wat-type-section, emit-wasm-module, etc.)
;;;; and the wasm-target class are in wasm.lisp (loads before this file).
;;;;
;;;; Load order: after wasm.lisp.

(in-package :cl-cc/emit)

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
    (let* ((module (wasm-target-module target))
           (entry-idx (or (%wasm-function-index-for-label module (vm-label-name inst)) 0)))
      (loop for arg in (vm-args inst) for i from 0 do
        (format stream "~%    (global.set $cl_arg~D ~A)" i (reg-local-ref reg-map arg)))
      (format stream "~%    ~A"
              (reg-local-set reg-map (vm-dst inst)
                             (format nil "(call_indirect (type $main_func_t) (table $funcref_table) (i32.const ~D))"
                                     entry-idx))))))

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
  (format stream "~%    ;; WASM: unsupported ~A" (type-of instruction)))

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
