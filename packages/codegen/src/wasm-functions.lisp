;;;; packages/codegen/src/wasm-functions.lisp - WAT function emission
;;;
;;; WAT calling-convention globals, function local variable declarations,
;;; full WAT function emission, and BigInt wrapper generation.

(in-package :cl-cc/codegen)

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
