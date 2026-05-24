;;;; packages/codegen/src/wasm-simd.lisp --- Wasm SIMD128 WAT emission helpers

(in-package :cl-cc/codegen)

;;; SIMD128 value type.  The core wasm-types.lisp file intentionally keeps only
;;; the prefix/opcode constants; text emission still needs the v128 valtype.
(defconstant +wasm-v128+ #x7b)

(defvar *wasm-relaxed-simd-nan-warning-issued* nil)

(defun wasm-simd128-emission-enabled-p ()
  "Return true when SIMD128 emission is enabled.

Both historical feature variables are honored: wasm-features.lisp exposes
*WASM-SIMD128-ENABLED*, while wasm-emit-data.lisp and feature predicates use
*WASM-SIMD-ENABLED*.  Treat either variable being NIL as disabling emission."
  (and (if (boundp '*wasm-simd128-enabled*) *wasm-simd128-enabled* t)
       (if (boundp '*wasm-simd-enabled*) *wasm-simd-enabled* t)))

(defun wasm-relaxed-simd-emission-enabled-p ()
  "Return true when relaxed SIMD emission is enabled and strict NaN is off."
  (and (wasm-simd128-emission-enabled-p)
       (if (boundp '*wasm-relaxed-simd-enabled*) *wasm-relaxed-simd-enabled* t)
       (not (wasm-strict-nan-emission-enabled-p))))

(defun wasm-strict-nan-emission-enabled-p ()
  "Return true when strict IEEE NaN behavior should be preserved.

FR-325 note: core Wasm SIMD follows the WebAssembly SIMD floating-point NaN
rules: arithmetic may propagate an operand NaN or produce an arithmetic NaN as
specified by the engine, and relaxed SIMD operations are explicitly allowed to
choose target-dependent results.  When this guard is true, relaxed SIMD emitters
signal instead of emitting target-dependent operations."
  (and (boundp '*wasm-strict-nan*) *wasm-strict-nan*))

(defun %wasm-require-simd (&optional (feature "SIMD128"))
  (unless (wasm-simd128-emission-enabled-p)
    (error "Wasm ~A emission requested while SIMD128 is disabled" feature)))

(defun %wasm-require-relaxed-simd (&optional (feature "Relaxed SIMD"))
  (%wasm-require-simd feature)
  (when (wasm-strict-nan-emission-enabled-p)
    (error "Wasm ~A emission requested while *WASM-STRICT-NAN* is enabled" feature))
  (unless (wasm-relaxed-simd-emission-enabled-p)
    (error "Wasm ~A emission requested while relaxed SIMD is disabled" feature))
  (unless *wasm-relaxed-simd-nan-warning-issued*
    (warn "Wasm relaxed SIMD may produce target-dependent, non-canonical NaN results; set *WASM-STRICT-NAN* to disable it.")
    (setf *wasm-relaxed-simd-nan-warning-issued* t)))

(defun %wasm-memory-args (&key offset align)
  (format nil "~@[ offset=~D~]~@[ align=~D~]" offset align))

(defun wasm-simd-expr (opcode operands &key relaxed-p)
  "Return folded WAT for SIMD OPCODE applied to OPERANDS."
  (if relaxed-p
      (%wasm-require-relaxed-simd opcode)
      (%wasm-require-simd opcode))
  (format nil "(~A~{ ~A~})" opcode operands))

(defun emit-wasm-simd-expr (stream opcode operands &key relaxed-p (indent 2))
  "Emit folded WAT for SIMD OPCODE to STREAM."
  (format stream "~%~v@T~A" indent (wasm-simd-expr opcode operands :relaxed-p relaxed-p)))

(defmacro define-wasm-simd-emitter (name opcode &key relaxed)
  `(defun ,name (stream &rest operands)
     ,(format nil "Emit Wasm SIMD instruction ~A." opcode)
     (emit-wasm-simd-expr stream ,opcode operands :relaxed-p ,relaxed)))

(defun emit-wasm-v128-load (stream address-wat &key offset (align 16))
  "Emit v128.load from linear memory ADDRESS-WAT.  ALIGN defaults to 16 bytes; pass ALIGN=1 for unaligned loads."
  (%wasm-require-simd "v128.load")
  (format stream "~%  (v128.load~A ~A)" (%wasm-memory-args :offset offset :align align) address-wat))

(defun emit-wasm-v128-store (stream address-wat value-wat &key offset (align 16))
  "Emit v128.store to linear memory ADDRESS-WAT."
  (%wasm-require-simd "v128.store")
  (format stream "~%  (v128.store~A ~A ~A)" (%wasm-memory-args :offset offset :align align) address-wat value-wat))

(defun emit-wasm-v128-load-splat (stream lane-width address-wat &key offset align)
  "Emit v128.load{8,16,32,64}_splat."
  (%wasm-require-simd "v128.load_splat")
  (unless (member lane-width '(8 16 32 64))
    (error "Unsupported SIMD load_splat lane width: ~S" lane-width))
  (format stream "~%  (v128.load~D_splat~A ~A)"
          lane-width (%wasm-memory-args :offset offset :align align) address-wat))

(defun emit-wasm-v128-load8-splat (stream address-wat &key offset align)
  (emit-wasm-v128-load-splat stream 8 address-wat :offset offset :align align))

(defun emit-wasm-v128-load16-splat (stream address-wat &key offset align)
  (emit-wasm-v128-load-splat stream 16 address-wat :offset offset :align align))

(defun emit-wasm-v128-load32-splat (stream address-wat &key offset align)
  (emit-wasm-v128-load-splat stream 32 address-wat :offset offset :align align))

(defun emit-wasm-v128-load64-splat (stream address-wat &key offset align)
  (emit-wasm-v128-load-splat stream 64 address-wat :offset offset :align align))

(defun emit-wasm-v128-load-zero (stream lane-width address-wat &key offset align)
  "FR-313: Emit v128.load32_zero or v128.load64_zero."
  (%wasm-require-simd "v128.load_zero")
  (unless (member lane-width '(32 64))
    (error "Unsupported SIMD load_zero lane width: ~S" lane-width))
  (format stream "~%  (v128.load~D_zero~A ~A)"
          lane-width (%wasm-memory-args :offset offset :align align) address-wat))

(defun emit-wasm-v128-load32-zero (stream address-wat &key offset align)
  (emit-wasm-v128-load-zero stream 32 address-wat :offset offset :align align))

(defun emit-wasm-v128-load64-zero (stream address-wat &key offset align)
  (emit-wasm-v128-load-zero stream 64 address-wat :offset offset :align align))

(defun emit-wasm-v128-load-lane (stream lane-width lane address-wat vector-wat &key offset align)
  "FR-303: Emit v128.load{8,16,32,64}_lane."
  (%wasm-require-simd "v128.load_lane")
  (unless (member lane-width '(8 16 32 64))
    (error "Unsupported SIMD load_lane width: ~S" lane-width))
  (format stream "~%  (v128.load~D_lane~A ~D ~A ~A)"
          lane-width (%wasm-memory-args :offset offset :align align) lane address-wat vector-wat))

(defun emit-wasm-v128-load8-lane (stream lane address-wat vector-wat &key offset align)
  (emit-wasm-v128-load-lane stream 8 lane address-wat vector-wat :offset offset :align align))

(defun emit-wasm-v128-load16-lane (stream lane address-wat vector-wat &key offset align)
  (emit-wasm-v128-load-lane stream 16 lane address-wat vector-wat :offset offset :align align))

(defun emit-wasm-v128-load32-lane (stream lane address-wat vector-wat &key offset align)
  (emit-wasm-v128-load-lane stream 32 lane address-wat vector-wat :offset offset :align align))

(defun emit-wasm-v128-load64-lane (stream lane address-wat vector-wat &key offset align)
  (emit-wasm-v128-load-lane stream 64 lane address-wat vector-wat :offset offset :align align))

(defun emit-wasm-v128-store-lane (stream lane-width lane address-wat vector-wat &key offset align)
  "FR-303: Emit v128.store{8,16,32,64}_lane."
  (%wasm-require-simd "v128.store_lane")
  (unless (member lane-width '(8 16 32 64))
    (error "Unsupported SIMD store_lane width: ~S" lane-width))
  (format stream "~%  (v128.store~D_lane~A ~D ~A ~A)"
          lane-width (%wasm-memory-args :offset offset :align align) lane address-wat vector-wat))

(defun emit-wasm-v128-store8-lane (stream lane address-wat vector-wat &key offset align)
  (emit-wasm-v128-store-lane stream 8 lane address-wat vector-wat :offset offset :align align))

(defun emit-wasm-v128-store16-lane (stream lane address-wat vector-wat &key offset align)
  (emit-wasm-v128-store-lane stream 16 lane address-wat vector-wat :offset offset :align align))

(defun emit-wasm-v128-store32-lane (stream lane address-wat vector-wat &key offset align)
  (emit-wasm-v128-store-lane stream 32 lane address-wat vector-wat :offset offset :align align))

(defun emit-wasm-v128-store64-lane (stream lane address-wat vector-wat &key offset align)
  (emit-wasm-v128-store-lane stream 64 lane address-wat vector-wat :offset offset :align align))

(defun wasm-v128-const (shape &rest lanes)
  "Return a v128.const expression, e.g. (wasm-v128-const \"i32x4\" 0 0 0 0)."
  (%wasm-require-simd "v128.const")
  (format nil "(v128.const ~A~{ ~A~})" shape lanes))

(defun wasm-i32x4-splat-from-array (reg-map array-reg index-reg)
  "Construct an i32x4 vector from four consecutive fixnum-array elements."
  (let* ((arr (wasm-array-cast-wat (reg-local-ref reg-map array-reg) :fixnum))
         (idx (wasm-fixnum-unbox reg-map index-reg :result-type :i32))
         (vec (wasm-v128-const "i32x4" 0 0 0 0)))
    (dotimes (lane 4 vec)
      (let* ((lane-idx (if (zerop lane)
                           idx
                           (format nil "(i32.add ~A (i32.const ~D))" idx lane)))
             (raw (format nil "(i32.wrap_i64 (array.get $fixnum_array_t ~A ~A))" arr lane-idx)))
        (setf vec (format nil "(i32x4.replace_lane ~D ~A ~A)" lane vec raw))))))

(defun %wasm-i32x4-store-to-array-wat (reg-map array-reg index-reg vector-wat)
  "Return WAT statements storing VECTOR-WAT into four consecutive fixnum-array elements."
  (let* ((arr (wasm-array-cast-wat (reg-local-ref reg-map array-reg) :fixnum))
         (idx (wasm-fixnum-unbox reg-map index-reg :result-type :i32)))
    (with-output-to-string (stream)
      (dotimes (lane 4)
        (let ((lane-idx (if (zerop lane)
                            idx
                            (format nil "(i32.add ~A (i32.const ~D))" idx lane))))
          (format stream "~%      (array.set $fixnum_array_t ~A ~A (i64.extend_i32_s (i32x4.extract_lane ~D ~A)))"
                  arr lane-idx lane vector-wat))))))

(defun wasm-i32x4-vector-op-wat (reg-map op lhs-array rhs-array index-reg)
  "Return folded WAT for the VM SIMD i32x4 binary OP."
  (let ((lhs (wasm-i32x4-splat-from-array reg-map lhs-array index-reg))
        (rhs (wasm-i32x4-splat-from-array reg-map rhs-array index-reg)))
    (case op
      (:add (wasm-simd-expr "i32x4.add" (list lhs rhs)))
      (:sub (wasm-simd-expr "i32x4.sub" (list lhs rhs)))
      (:mul (wasm-simd-expr "i32x4.mul" (list lhs rhs)))
      (:logand (wasm-simd-expr "v128.and" (list lhs rhs)))
      (:logior (wasm-simd-expr "v128.or" (list lhs rhs)))
      (:logxor (wasm-simd-expr "v128.xor" (list lhs rhs)))
      (otherwise (error "Unsupported Wasm SIMD VM op ~S for i32x4" op)))))

(defun emit-wasm-vm-simd-vector-op (reg-map inst stream &key (indent 4))
  "Lower VM-SIMD-VECTOR-OP to Wasm SIMD128 WAT.

The VM marker describes an array-map over four i32 fixnum lanes.  Wasm GC arrays
cannot be the direct operand of v128.load, so this path constructs the v128 value
with i32x4.replace_lane, performs a real SIMD operation, and writes lanes back
with i32x4.extract_lane.  Linear-memory v128.load/store emission is provided by
the public emit-wasm-v128-* helpers above for lowerings that operate on memory
addresses."
  (declare (ignore indent))
  (%wasm-require-simd "vm-simd-vector-op")
  (unless (and (= (vm-simd-vector-op-lanes inst) 4)
               (eq (vm-simd-vector-op-element-type inst) :i32))
    (error "Wasm SIMD VM lowering supports only 4 lanes of :I32, got ~D lanes of ~S"
           (vm-simd-vector-op-lanes inst)
           (vm-simd-vector-op-element-type inst)))
  (let ((vector-wat (wasm-i32x4-vector-op-wat reg-map
                                              (vm-simd-vector-op-op inst)
                                              (vm-simd-vector-op-lhs-array inst)
                                              (vm-simd-vector-op-rhs-array inst)
                                              (vm-simd-vector-op-index-reg inst))))
    (format stream "~%      ;; Wasm SIMD128 VM vector op: ~S i32x4"
            (vm-simd-vector-op-op inst))
    (format stream "~A"
            (%wasm-i32x4-store-to-array-wat reg-map
                                            (vm-simd-vector-op-dst-array inst)
                                            (vm-simd-vector-op-index-reg inst)
                                            vector-wat))))

;;; Arithmetic, comparisons, boolean reductions, bitwise operations, shuffle and
;;; conversions.  These definitions intentionally expose one function per WAT
;;; opcode family so VM and future MIR lowerings can emit instructions directly.
(define-wasm-simd-emitter emit-wasm-i8x16-add "i8x16.add")
(define-wasm-simd-emitter emit-wasm-i8x16-sub "i8x16.sub")
(define-wasm-simd-emitter emit-wasm-i8x16-mul "i8x16.mul")
(define-wasm-simd-emitter emit-wasm-i16x8-add "i16x8.add")
(define-wasm-simd-emitter emit-wasm-i16x8-sub "i16x8.sub")
(define-wasm-simd-emitter emit-wasm-i16x8-mul "i16x8.mul")
(define-wasm-simd-emitter emit-wasm-i32x4-add "i32x4.add")
(define-wasm-simd-emitter emit-wasm-i32x4-sub "i32x4.sub")
(define-wasm-simd-emitter emit-wasm-i32x4-mul "i32x4.mul")
(define-wasm-simd-emitter emit-wasm-i64x2-add "i64x2.add")
(define-wasm-simd-emitter emit-wasm-i64x2-sub "i64x2.sub")
(define-wasm-simd-emitter emit-wasm-i64x2-mul "i64x2.mul")
(define-wasm-simd-emitter emit-wasm-f32x4-add "f32x4.add")
(define-wasm-simd-emitter emit-wasm-f32x4-sub "f32x4.sub")
(define-wasm-simd-emitter emit-wasm-f32x4-mul "f32x4.mul")
(define-wasm-simd-emitter emit-wasm-f32x4-div "f32x4.div")
(define-wasm-simd-emitter emit-wasm-f32x4-sqrt "f32x4.sqrt")
(define-wasm-simd-emitter emit-wasm-f64x2-add "f64x2.add")
(define-wasm-simd-emitter emit-wasm-f64x2-sub "f64x2.sub")
(define-wasm-simd-emitter emit-wasm-f64x2-mul "f64x2.mul")
(define-wasm-simd-emitter emit-wasm-f64x2-div "f64x2.div")
(define-wasm-simd-emitter emit-wasm-f64x2-sqrt "f64x2.sqrt")

(define-wasm-simd-emitter emit-wasm-i8x16-neg "i8x16.neg")
(define-wasm-simd-emitter emit-wasm-i8x16-abs "i8x16.abs")
(define-wasm-simd-emitter emit-wasm-i16x8-neg "i16x8.neg")
(define-wasm-simd-emitter emit-wasm-i16x8-abs "i16x8.abs")
(define-wasm-simd-emitter emit-wasm-i32x4-neg "i32x4.neg")
(define-wasm-simd-emitter emit-wasm-i32x4-abs "i32x4.abs")
(define-wasm-simd-emitter emit-wasm-i64x2-neg "i64x2.neg")
(define-wasm-simd-emitter emit-wasm-i64x2-abs "i64x2.abs")
(define-wasm-simd-emitter emit-wasm-f32x4-abs "f32x4.abs")
(define-wasm-simd-emitter emit-wasm-f32x4-neg "f32x4.neg")
(define-wasm-simd-emitter emit-wasm-f32x4-ceil "f32x4.ceil")
(define-wasm-simd-emitter emit-wasm-f32x4-floor "f32x4.floor")
(define-wasm-simd-emitter emit-wasm-f32x4-trunc "f32x4.trunc")
(define-wasm-simd-emitter emit-wasm-f32x4-nearest "f32x4.nearest")
(define-wasm-simd-emitter emit-wasm-f64x2-abs "f64x2.abs")
(define-wasm-simd-emitter emit-wasm-f64x2-neg "f64x2.neg")
(define-wasm-simd-emitter emit-wasm-f64x2-ceil "f64x2.ceil")
(define-wasm-simd-emitter emit-wasm-f64x2-floor "f64x2.floor")
(define-wasm-simd-emitter emit-wasm-f64x2-trunc "f64x2.trunc")
(define-wasm-simd-emitter emit-wasm-f64x2-nearest "f64x2.nearest")

(dolist (shape '("i8x16" "i16x8" "i32x4" "i64x2"))
  (dolist (cmp '("eq" "ne" "lt_s" "gt_s" "le_s" "ge_s"))
    (let* ((fname (intern (string-upcase (format nil "EMIT-WASM-~A-~A" shape cmp))))
           (opcode (format nil "~A.~A" shape cmp)))
      (setf (symbol-function fname)
            (lambda (stream &rest operands)
              (emit-wasm-simd-expr stream opcode operands))))))

(dolist (shape '("f32x4" "f64x2"))
  (dolist (cmp '("eq" "ne" "lt" "gt" "le" "ge"))
    (let* ((fname (intern (string-upcase (format nil "EMIT-WASM-~A-~A" shape cmp))))
           (opcode (format nil "~A.~A" shape cmp)))
      (setf (symbol-function fname)
            (lambda (stream &rest operands)
              (emit-wasm-simd-expr stream opcode operands))))))

(define-wasm-simd-emitter emit-wasm-v128-any-true "v128.any_true")
(define-wasm-simd-emitter emit-wasm-i8x16-all-true "i8x16.all_true")
(define-wasm-simd-emitter emit-wasm-i16x8-all-true "i16x8.all_true")
(define-wasm-simd-emitter emit-wasm-i32x4-all-true "i32x4.all_true")
(define-wasm-simd-emitter emit-wasm-i64x2-all-true "i64x2.all_true")
(define-wasm-simd-emitter emit-wasm-i8x16-bitmask "i8x16.bitmask")
(define-wasm-simd-emitter emit-wasm-i16x8-bitmask "i16x8.bitmask")
(define-wasm-simd-emitter emit-wasm-i32x4-bitmask "i32x4.bitmask")
(define-wasm-simd-emitter emit-wasm-i64x2-bitmask "i64x2.bitmask")

(define-wasm-simd-emitter emit-wasm-v128-and "v128.and")
(define-wasm-simd-emitter emit-wasm-v128-or "v128.or")
(define-wasm-simd-emitter emit-wasm-v128-xor "v128.xor")
(define-wasm-simd-emitter emit-wasm-v128-not "v128.not")
(define-wasm-simd-emitter emit-wasm-v128-andnot "v128.andnot")
(define-wasm-simd-emitter emit-wasm-i8x16-swizzle "i8x16.swizzle")

(defun emit-wasm-i8x16-shuffle (stream a-wat b-wat lanes)
  "FR-278: Emit static i8x16.shuffle with LANES as 16 integers in [0,31]."
  (%wasm-require-simd "i8x16.shuffle")
  (unless (and (= (length lanes) 16)
               (every (lambda (lane) (and (integerp lane) (<= 0 lane 31))) lanes))
    (error "i8x16.shuffle requires 16 lane indexes in [0,31], got ~S" lanes))
  (format stream "~%  (i8x16.shuffle~{ ~D~} ~A ~A)" lanes a-wat b-wat))

(define-wasm-simd-emitter emit-wasm-i32x4-trunc-sat-f32x4-s "i32x4.trunc_sat_f32x4_s")
(define-wasm-simd-emitter emit-wasm-i32x4-trunc-sat-f32x4-u "i32x4.trunc_sat_f32x4_u")
(define-wasm-simd-emitter emit-wasm-f32x4-convert-i32x4-s "f32x4.convert_i32x4_s")
(define-wasm-simd-emitter emit-wasm-f32x4-convert-i32x4-u "f32x4.convert_i32x4_u")
(define-wasm-simd-emitter emit-wasm-f64x2-promote-low-f32x4 "f64x2.promote_low_f32x4")
(define-wasm-simd-emitter emit-wasm-f32x4-demote-f64x2-zero "f32x4.demote_f64x2_zero")
(define-wasm-simd-emitter emit-wasm-i16x8-narrow-i32x4-s "i16x8.narrow_i32x4_s")
(define-wasm-simd-emitter emit-wasm-i16x8-narrow-i32x4-u "i16x8.narrow_i32x4_u")
(define-wasm-simd-emitter emit-wasm-i32x4-widen-low-i16x8-s "i32x4.widen_low_i16x8_s")
(define-wasm-simd-emitter emit-wasm-i32x4-widen-low-i16x8-u "i32x4.widen_low_i16x8_u")
(define-wasm-simd-emitter emit-wasm-i32x4-dot-i16x8-s "i32x4.dot_i16x8_s")

(define-wasm-simd-emitter emit-wasm-f32x4-relaxed-madd "f32x4.relaxed_madd" :relaxed t)
(define-wasm-simd-emitter emit-wasm-f32x4-relaxed-nmadd "f32x4.relaxed_nmadd" :relaxed t)
(define-wasm-simd-emitter emit-wasm-f64x2-relaxed-min "f64x2.relaxed_min" :relaxed t)
(define-wasm-simd-emitter emit-wasm-f64x2-relaxed-max "f64x2.relaxed_max" :relaxed t)
(define-wasm-simd-emitter emit-wasm-i32x4-relaxed-laneselect "i32x4.relaxed_laneselect" :relaxed t)
(define-wasm-simd-emitter emit-wasm-i16x8-relaxed-q15mulr-s "i16x8.relaxed_q15mulr_s" :relaxed t)
(define-wasm-simd-emitter emit-wasm-i32x4-relaxed-dot-i8x16-i7x16-s "i32x4.relaxed_dot_i8x16_i7x16_s" :relaxed t)
(define-wasm-simd-emitter emit-wasm-f32x4-relaxed-dot-bf16x8-add-f32x4 "f32x4.relaxed_dot_bf16x8_add_f32x4" :relaxed t)

(defmethod emit-instruction ((target wasm-target) (inst vm-simd-vector-op) stream)
  (emit-wasm-vm-simd-vector-op (wasm-target-reg-map target) inst stream))
