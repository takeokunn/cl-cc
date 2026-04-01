;;;; src/backend/wasm.lisp - WebAssembly (WAT text) Backend
;;;
;;; Implements the WASM text format (WAT) emission backend for cl-cc.
;;; Uses the WASM GC proposal type system (struct/array/ref types).
;;; Control flow is handled via PC-dispatch trampoline (see wasm-trampoline.lisp).
;;;
;;; The emit-instruction generic is defined in x86-64.lisp (it is the shared
;;; backend interface). This file adds methods for the wasm-target class.

(in-package :cl-cc)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WASM target class
;;; ─────────────────────────────────────────────────────────────────────────────

(defclass wasm-target (target)
  ((module :initarg :module :initform nil :accessor wasm-target-module
    :documentation "The wasm-module-ir being built.")
   (reg-map :initarg :reg-map :initform nil :accessor wasm-target-reg-map
    :documentation "Current wasm-reg-map for the function being emitted."))
  (:documentation "WASM backend target. Emits WebAssembly Text Format (WAT)."))

;;; target-register: maps VM register to WAT local index (integer)
(defmethod target-register ((target wasm-target) virtual-register)
  "Map a VM register keyword (:R0, :R1, ...) to a WASM local index."
  (let ((reg-map (wasm-target-reg-map target)))
    (if reg-map
        (wasm-reg-to-local reg-map virtual-register)
        ;; Fallback: extract number from :R0 -> 0
        (let* ((name (symbol-name virtual-register))
               (num (parse-integer name :start 1 :junk-allowed t)))
          (or num 0)))))

(defun %wasm-function-index-for-label (module label)
  (let ((func (find label (wasm-module-functions module)
                    :key (lambda (f)
                           (let ((wat-name (wasm-func-wat-name f)))
                             (if (and wat-name (> (length wat-name) 0)
                                      (char= (char wat-name 0) #\$))
                                 (subseq wat-name 1)
                                 wat-name)))
                    :test #'equal)))
    (when func
      (wasm-func-index func))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT module header: predefined GC type section
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wat-type-section (stream)
  "Emit the predefined WASM GC type definitions to STREAM as WAT."
  ;; Type 0: main function type
  (format stream "~%  (type $main_func_t (func (result eqref)))")
  ;; Type 1: bytes array (i8, mutable)
  (format stream "~%  (type $bytes_array_t (array (mut i8)))")
  ;; Type 2: string struct
  (format stream "~%  (type $string_t (struct (field $chars (ref $bytes_array_t))))")
  ;; Type 3: symbol struct
  (format stream "~%  (type $symbol_t (struct (field $name (ref $string_t)) (field $plist eqref)))")
  ;; Type 4: cons cell
  (format stream "~%  (type $cons_t (struct (field $car (mut eqref)) (field $cdr (mut eqref))))")
  ;; Type 5: eqref array (for slots, envs, mv-buffers)
  (format stream "~%  (type $eqref_array_t (array (mut eqref)))")
  ;; Type 6: closure environment
  (format stream "~%  (type $env_t (struct (field $vars (ref $eqref_array_t)) (field $parent (ref null $env_t))))")
  ;; Type 7: closure
  (format stream "~%  (type $closure_t (struct (field $entry i32) (field $env (ref null $env_t))))")
  ;; Type 8: class metadata
  (format stream "~%  (type $class_meta_t (struct (field $name (ref $symbol_t)) (field $slot_names (ref $eqref_array_t))))")
  ;; Type 9: CLOS instance
  (format stream "~%  (type $instance_t (struct (field $class (ref $class_meta_t)) (field $slots (mut (ref $eqref_array_t)))))")
  ;; Type 10: hash table
  (format stream "~%  (type $htable_t (struct (field $keys (mut (ref $eqref_array_t))) (field $vals (mut (ref $eqref_array_t))) (field $count (mut i32))))")
  ;; Type 11: boxed float
  (format stream "~%  (type $float_t (struct (field $val f64)))")
  ;; Type 12: character
  (format stream "~%  (type $char_t (struct (field $code i32)))"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT module: global variable table
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wat-globals (module stream)
  "Emit WASM global variable declarations to STREAM."
  (dolist (global (wasm-module-globals module))
    (format stream "~%  (global ~A (mut eqref) (ref.null eq))"
            (wasm-global-def-wat-name global))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT module: function table (for call_indirect)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wat-table (module stream)
  "Emit the funcref table for call_indirect dispatch."
  (let ((size (max 1 (wasm-module-table-size module))))
    (format stream "~%  (table $funcref_table ~D funcref)" size)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT module: elem segment — populate funcref table with all compiled functions
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wat-elem (module stream)
  "Emit an active elem segment that fills $funcref_table starting at offset 0
   with all compiled WASM functions.  This enables call_indirect dispatch for
   vm-call and vm-closure (which store the function's table index in $closure_t)."
  (let ((funcs (wasm-module-functions module)))
    (when funcs
      (format stream "~%  (elem (table $funcref_table) (i32.const 0) func")
      (dolist (func funcs)
        (format stream " ~A" (wasm-func-wat-name func)))
      (format stream ")"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT module: calling-convention globals ($cl_arg0..$cl_arg7)
;;; ─────────────────────────────────────────────────────────────────────────────

(defconstant +wasm-max-call-args+ 8
  "Maximum number of function arguments supported by the WASM calling convention.
   Arguments are passed via globals $cl_arg0 through $cl_arg7 before call_indirect.")

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
  ;; $pc: i32 at index (wasm-reg-map-pc-index reg-map)
  (format stream "~%    (local i32) ;; $pc at index ~D"
          (wasm-reg-map-pc-index reg-map))
  ;; $tmp: eqref at index (wasm-reg-map-tmp-index reg-map)
  (format stream "~%    (local eqref) ;; $tmp at index ~D"
          (wasm-reg-map-tmp-index reg-map))
  ;; One eqref local per VM register that was allocated into this reg-map.
  ;; The count is (next-index - pc-slot - tmp-slot) = next-index - (pc + 1) - 1
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
         (reg-map (make-wasm-reg-map-for-function 0)))
    ;; Pre-collect all registers to populate reg-map local count.
    (collect-registers-from-instructions instructions reg-map)
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
    (when (wasm-func-exported-p func-def)
      (format stream "~%  (export ~S (func ~A))"
              (or (wasm-func-export-name func-def)
                  (subseq wat-name 1))
              wat-name))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WAT imports section (I/O host functions)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wat-imports (stream)
  "Emit standard cl-cc I/O imports."
  (format stream "~%  ;; Host I/O imports")
  (format stream "~%  (import \"cl_io\" \"write_char\" (func $host_write_char (param i32)))")
  (format stream "~%  (import \"cl_io\" \"read_char\" (func $host_read_char (result i32)))")
  (format stream "~%  (import \"cl_io\" \"write_string\" (func $host_write_string (param (ref $string_t))))")
  (format stream "~%  (import \"cl_io\" \"error\" (func $host_error (param (ref $string_t))))")
  ;; print_val: host-side formatter for any eqref value (used by vm-print)
  (format stream "~%  (import \"cl_io\" \"print_val\" (func $host_print_val (param eqref)))"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; emit-wasm-module: serialize a wasm-module-ir to WAT text
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wasm-module (module stream)
  "Serialize a wasm-module-ir to WAT text format on STREAM.
   Assumes build-all-wasm-functions has already been called so every
   wasm-function-def has a populated :body slot."
  (format stream "(module")
  (format stream "~%  ;; cl-cc generated WASM module (GC proposal)")
  ;; Type section
  (emit-wat-type-section stream)
  ;; Imports
  (emit-wat-imports stream)
  ;; Table (size updated by build-all-wasm-functions)
  (emit-wat-table module stream)
  ;; User-defined global variables (from defvar/setq)
  (emit-wat-globals module stream)
  ;; Argument-passing calling convention globals ($cl_arg0..$cl_arg7)
  (emit-wat-call-globals stream)
  ;; Functions
  (dolist (func (wasm-module-functions module))
    (emit-wat-function func stream))
  ;; Elem segment: populate funcref table so call_indirect can dispatch
  (emit-wat-elem module stream)
  (format stream "~%) ;; end module~%"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; emit-instruction methods for wasm-target
;;;
;;; These methods implement the shared backend interface (emit-instruction
;;; generic defined in x86-64.lisp). For the WASM backend the primary entry
;;; point is compile-to-wasm-wat below, which uses the module-level pipeline.
;;; These methods are provided for compatibility with any caller that drives
;;; instruction-by-instruction emission through the generic interface.
;;; ─────────────────────────────────────────────────────────────────────────────

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
    (format stream "~%    (local.set ~D (local.get ~D))"
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

(defmethod emit-instruction ((target wasm-target) (inst vm-sub) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (wasm-fixnum-box
                            (format nil "(i64.sub ~A ~A)"
                                    (wasm-fixnum-unbox reg-map (vm-lhs inst))
                                    (wasm-fixnum-unbox reg-map (vm-rhs inst))))))))

(defmethod emit-instruction ((target wasm-target) (inst vm-mul) stream)
  (let ((reg-map (wasm-target-reg-map target)))
    (format stream "~%    ~A"
            (reg-local-set reg-map (vm-dst inst)
                           (wasm-fixnum-box
                            (format nil "(i64.mul ~A ~A)"
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
