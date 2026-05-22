(in-package :cl-cc/codegen)

(defmethod emit-instruction ((target wasm-target) (inst vm-const) stream)
  (let* ((reg-map (wasm-target-reg-map target))
         (val (vm-value inst))
         (dst (vm-dst inst))
         (wat-val (typecase val
                    (integer
                     (wasm-fixnum-box (format nil "(i64.const ~D)" val)))
                    (null "(ref.null eq)")
                    ((eql t) "(ref.i31 (i32.const 1))")
                    ;; FR-297: String literals — emit as staged string objects
                    (string
                     (%wasm-string-literal-eqref val))
                    ;; FR-297: Symbol literals — emit staged symbol objects
                    (symbol
                     (%wasm-symbol-literal-eqref val))
                    ;; FR-297: Character literals — emit as char-code i31 ref
                    (character
                     (format nil "(ref.i31 (i32.const ~D))" (char-code val)))
                    ;; FR-297: Float literals — emit as f64.const + struct
                    (float
                     (%wasm-float-literal-eqref val))
                    (t
                     ;; Fallback for unsupported literal types
                     "(ref.null eq)"))))
    (format stream "~%    (local.set ~D ~A)"
            (wasm-reg-to-local reg-map dst)
            wat-val)))

(defmethod emit-instruction ((target wasm-target) (inst vm-move) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (let ((known-label (gethash (vm-src inst) (wasm-target-known-func-labels target))))
      (if known-label
          (setf (gethash (vm-dst inst) (wasm-target-known-func-labels target)) known-label)
          (remhash (vm-dst inst) (wasm-target-known-func-labels target))))
    (format stream "~%    (local.tee ~D (local.get ~D))"
            (wasm-reg-to-local reg-map (vm-dst inst))
            (wasm-reg-to-local reg-map (vm-src inst)))))

(defun %wasm-direct-call (label)
  "Return a direct call expression for LABEL."
  (format nil "(call $~A)" label))

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
  "Emit WASM code for vm-print: call the host runtime print function with the register value.
The host import $host_print_val takes a single eqref parameter."
  (let ((reg-map (wasm-target-reg-map target))
        (val-reg (vm-reg inst)))
    (format stream "~%    (call $host_print_val ~A)"
            (reg-local-ref reg-map val-reg))))

(defmethod emit-instruction ((target wasm-target) (inst vm-set-global) stream)
  (let ((reg-map (wasm-target-reg-map target))
        (globalidx (wasm-module-global-index-for-name (wasm-target-module target)
                                                      (vm-global-name inst))))
    (format stream "~%    ;; global.set globalidx ~D"
            (or globalidx 0))
    (format stream "~%    (global.set ~A ~A)"
            (vm-global-wat-name (vm-global-name inst))
            (reg-local-ref reg-map (vm-src inst)))))

(defmethod emit-instruction ((target wasm-target) (inst vm-get-global) stream)
  (let ((reg-map (wasm-target-reg-map target))
        (globalidx (wasm-module-global-index-for-name (wasm-target-module target)
                                                      (vm-global-name inst))))
    (format stream "~%    ;; global.get globalidx ~D"
            (or globalidx 0))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (format nil "(global.get ~A)"
                                   (vm-global-wat-name (vm-global-name inst)))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-call) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (let* ((func-reg (vm-func-reg inst))
          (func-ref (reg-local-ref reg-map func-reg))
          (known-label (gethash func-reg (wasm-target-known-func-labels target)))
          (typeidx (%wasm-main-function-type-index (wasm-target-module target))))
      (loop for arg in (vm-args inst) for i from 0 do
        (format stream "~%    (global.set $cl_arg~D ~A)" i (reg-local-ref reg-map arg)))
      (format stream "~%    ;; ~A"
              (if known-label
                  (format nil "call funcidx ~D" (or (%wasm-function-index-for-label (wasm-target-module target) known-label) 0))
                  (format nil "call_indirect typeidx ~D tableidx 0" typeidx)))
      (format stream "~%    ~A"
              (reg-local-set reg-map (vm-dst inst)
                              (if known-label
                                  (%wasm-direct-call known-label)
                                  (format nil "(call_indirect (type $main_func_t) (table $funcref_table) ~A)"
                                          func-ref)))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-func-ref) stream)
  "Materialize a closure for LABEL in DST and track a direct-call label hint."
  (let* ((reg-map (wasm-target-reg-map target))
          (module (wasm-target-module target))
          (idx (or (and module (%wasm-function-index-for-label module (vm-label-name inst))) 0))
          (dst (vm-dst inst)))
    (setf (gethash dst (wasm-target-known-func-labels target))
          (vm-label-name inst))
    (format stream "~%    ;; struct.new typeidx ~D; funcidx ~D"
            +type-idx-closure+ idx)
    (emit-wasm-closure-allocation reg-map dst idx nil stream 4)))

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
      (setf (gethash (vm-dst inst) (wasm-target-known-func-labels target))
            (vm-label-name inst))
      (format stream "~%    ;; struct.new typeidx ~D; array.new typeidx ~D"
              +type-idx-closure+ +type-idx-eqref-array+)
      (emit-wasm-closure-allocation reg-map (vm-dst inst) entry-idx
                                    (vm-captured-vars inst) stream 4))))

(defmethod emit-instruction ((target wasm-target) (inst vm-closure-ref-idx) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (wasm-closure-ref-wat reg-map
                                                 (vm-closure-reg inst)
                                                 (vm-closure-index inst))))))

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
    ;; BCE metadata suppresses any extra explicit bounds guard here.  Wasm GC has
    ;; no standard unchecked array.get; the opcode retains mandatory trap
    ;; semantics, so we emit a marker documenting that compiler-side BCE fired.
    (when (opt-bounds-check-eliminable-marked-p inst)
      (format stream "~%    ;; BCE: explicit bounds check eliminated; array.get remains spec-checked"))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (format nil "(array.get $eqref_array_t ~A ~A)" arr idx)))))

(defmethod emit-instruction ((target wasm-target) (inst vm-aset) stream)
  "Emit wasm-gc array element write via array.set."
  (let* ((reg-map (wasm-target-reg-map target))
         (arr (reg-local-ref reg-map (vm-array-reg inst)))
         (idx (wasm-fixnum-unbox reg-map (vm-index-reg inst)))
         (val (reg-local-ref reg-map (vm-val-reg inst))))
    ;; BCE metadata suppresses any extra explicit bounds guard here.  Wasm GC has
    ;; no standard unchecked array.set; the opcode retains mandatory trap
    ;; semantics, so we emit a marker documenting that compiler-side BCE fired.
    (when (opt-bounds-check-eliminable-marked-p inst)
      (format stream "~%    ;; BCE: explicit bounds check eliminated; array.set remains spec-checked"))
    (format stream "~%    (array.set $eqref_array_t ~A ~A ~A)" arr idx val)))

(defun %wasm-string-literal-eqref (str)
  "Return wasm-gc eqref construction for a string literal STR.
Builds a $string_t struct with a $bytes_array_t containing the UTF-8 bytes."
  (let* ((bytes (map 'list #'char-code str))
         (byte-elems (format nil "~{~A~^ ~}" 
                              (mapcar (lambda (b) (format nil "(i32.const ~D)" b)) bytes))))
    (format nil "(struct.new $string_t (array.new_fixed $bytes_array_t ~D ~A))"
            (length bytes)
            byte-elems)))

(defun %wasm-symbol-literal-eqref (sym)
  "Return wasm-gc eqref for a symbol literal SYM.
Builds a staged symbol object: $symbol_t with the symbol's name string
and null value cell."
  (let ((name-str (string sym)))
    (format nil "(struct.new $symbol_t ~A (ref.null eq))"
            (%wasm-string-literal-eqref name-str))))

(defun %wasm-float-literal-eqref (val)
  "Return wasm-gc eqref for a float literal VAL.
Wraps the f64.const in a boxed heap float struct."
  (format nil "(struct.new $float_t (f64.const ~F))" val))

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

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-233: Non-trapping Float-to-int — safe floor/truncate/round codegen
;;; ─────────────────────────────────────────────────────────────────────────────
(defmethod emit-instruction ((target wasm-target) (inst vm-floor) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (wasm-fixnum-box
                            (format nil "(i64.trunc_sat_f64_s ~A)"
                                    (format nil "(f64.convert_i64_s ~A)"
                                            (wasm-fixnum-unbox reg-map (vm-lhs inst)))))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-323: MVP Bit Operations — integer-length uses i64.clz directly
;;; FR-324: f64.copysign — float-sign operation
;;; ─────────────────────────────────────────────────────────────────────────────
(defmethod emit-instruction ((target wasm-target) (inst vm-float-sign) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (format nil "(struct.new $float_t (f64.copysign ~A ~A))"
                                   (reg-local-ref reg-map (vm-lhs inst))
                                   (reg-local-ref reg-map (vm-rhs inst)))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-212: Typed Function References — call_ref codegen (Chrome 113+)
;;; Uses +wasm-call-ref+ (#x14) for direct typed function calls without
;;; the call_indirect table lookup overhead (15-30% faster per V8 benchmarks).
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-call-ref-enabled* t
  "Feature gate for WASM typed function references call_ref (FR-212).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-143/FR-253: return_call / return_call_ref feature gate
;;; ─────────────────────────────────────────────────────────────────────────────
;; (See wasm-emit-data.lisp for *wasm-tail-call-enabled*)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-209: i31ref — native fixnum boxing (i31ref already used in fixnum helpers)
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-i31ref-enabled* t
  "Feature gate for WASM GC i31ref fixnum representation (FR-209).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-226: externref — opaque JS object references (Chrome 91+)
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-externref-enabled* nil
  "Feature gate for WASM externref JS object reference passing (FR-226).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-222: DWARF debug info emission flag
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-emit-debug-info* nil
  "When T, emit DWARF 5 debug info in WASM custom sections (FR-222).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-223: Source Maps emission flag
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-emit-source-maps* nil
  "When T, emit Source Map v3 for browser DevTools (FR-223).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-219: AOT mode flag — static WASM binary generation
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-aot-mode* nil
  "When T, generate fully static .wasm binary in single pass (FR-219).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-220: PGO mode — profile-guided optimization
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-pgo-mode* nil
  "When T, use profile-guided optimization feedback (FR-220).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-221: Dead Import Elimination — link-time GC
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-dead-import-elimination* t
  "When T, eliminate unused WASM imports at link time (FR-221).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-232: Streaming Compilation / Module Cache
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-streaming-compilation* nil
  "When T, generate code compatible with WebAssembly.instantiateStreaming (FR-232).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-261: Security features — CFI / CSP / Constant-Time
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-cfi-enabled* t
  "When T, emit CFI (Control Flow Integrity) via typed call_ref (FR-261).")
(defparameter *wasm-constant-time* nil
  "When T, use select-based constant-time patterns for crypto (FR-261).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-265: Deterministic build
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-deterministic-build* nil
  "When T, produce reproducible WASM binaries (FR-265).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-305: WASM validation — WebAssembly.validate integration
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-validate-output* t
  "When T, validate generated WASM binary after compilation (FR-305).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-307: Subresource Integrity — SRI hash for .wasm
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-emit-sri-hash* nil
  "When T, compute and output SRI integrity hash for .wasm file (FR-307).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-326: memory.grow OOM detection — storage-condition on allocation failure
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-oom-detection* t
  "When T, check memory.grow return value and signal CL storage-condition (FR-326).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-308: Relaxed Dead Code Validation (Phase 2)
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-relaxed-dead-code-validation* nil
  "When T, skip dummy type coercion in unreachable code paths (FR-308).")

;;; Catch-all for unsupported instructions
(defmethod emit-instruction ((target wasm-target) instruction stream)
  (declare (ignore target stream))
  (error "Unsupported WASM instruction: ~A" (type-of instruction)))
