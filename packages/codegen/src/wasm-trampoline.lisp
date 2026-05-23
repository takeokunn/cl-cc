;;;; packages/emit/src/wasm-trampoline.lisp - PC-Dispatch Trampoline Builder
;;;
;;; Converts a flat list of VM instructions (a function body) into a WAT
;;; body using a PC-dispatch trampoline: loop { block { br_table } }.
;;; This bridges the VM's flat label-jump model to WASM structured control flow.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 1: Group instructions into basic blocks by label
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (wasm-basic-block (:conc-name wasm-bb-))
  "A basic block in the function: a label and its instructions."
  (label nil :type (or null string))   ; nil for the implicit entry block
  (pc-index nil :type (or null integer)) ; the $pc value that dispatches here
  (instructions nil :type list))       ; the instructions in this block

(defun group-into-basic-blocks (instructions)
  "Split INSTRUCTIONS into basic blocks at vm-label boundaries.
   Returns a list of wasm-basic-block structs in order."
  (let ((blocks nil)
        (current-label nil)
        (current-instrs nil)
        (pc-counter 0))
    (flet ((flush ()
             (when (or current-label current-instrs)
               (push (make-wasm-basic-block
                      :label current-label
                      :pc-index (prog1 pc-counter (incf pc-counter))
                      :instructions (nreverse current-instrs))
                     blocks)
               (setf current-label nil
                     current-instrs nil))))
      (dolist (inst instructions)
        (if (typep inst 'vm-label)
            (progn
              (flush)
              (setf current-label (vm-name inst)))
            (push inst current-instrs)))
      (flush))
    (nreverse blocks)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 2: Build a label -> pc-index map
;;; ─────────────────────────────────────────────────────────────────────────────

(defun build-label-pc-map (basic-blocks)
  "Build a hash table from label name (string) -> pc-index (integer)."
  (let ((map (make-hash-table :test #'equal)))
    (dolist (bb basic-blocks map)
      (when (wasm-bb-label bb)
        (setf (gethash (wasm-bb-label bb) map) (wasm-bb-pc-index bb))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Dynamic calling-convention state
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *wasm-label-to-table-idx* nil
  "Dynamic binding: hash table mapping function entry-label name (string) to its
   WASM funcref table index (= wasm-func-index).  Bound in build-all-wasm-functions
   so that emit-trampoline-instruction can emit real table indices for vm-closure.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 3: Emit WAT for a single instruction
;;; ─────────────────────────────────────────────────────────────────────────────
;;; Returns T if instruction was handled, NIL if not supported (emits comment).
;;; REG-MAP is a wasm-reg-map for mapping :R0 etc. to local indices.
;;; LABEL-PC-MAP maps label names to pc-index integers.
;;; NUM-BLOCKS is total number of basic blocks.

(defun reg-local-ref (reg-map reg)
  "Return WAT for getting a register's local variable, e.g. '(local.get 3)'."
  (format nil "(local.get ~D)" (wasm-reg-to-local reg-map reg)))

(defun reg-local-set (reg-map reg value-wat)
  "Return WAT for setting a register's local variable.
   Also clears any known type for the destination register (FR-142)."
  (let ((known-types (wasm-reg-map-known-types reg-map)))
    (when known-types (remhash reg known-types)))
  (let ((dst (wasm-reg-to-local reg-map reg)))
    (if (and (stringp value-wat)
             (search "(local.get " value-wat)
             (= (position #\( value-wat) 0))
        (format nil "(local.tee ~D ~A)" dst value-wat)
        (format nil "(local.set ~D ~A)" dst value-wat))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-142: ref.cast elimination — type tracking helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun reg-record-type (reg-map reg type-keyword)
  "Record that VM register REG holds a value of wasm type TYPE-KEYWORD.
   TYPE-KEYWORD examples: :closure, :cons, :i31ref, :string, :symbol."
  (let ((known-types (wasm-reg-map-known-types reg-map)))
    (when known-types
      (setf (gethash reg known-types) type-keyword))))

(defun reg-known-type (reg-map reg)
  "Return the known wasm type for VM register REG, or NIL if unknown."
  (let ((known-types (wasm-reg-map-known-types reg-map)))
    (and known-types (gethash reg known-types))))

(defun reg-clear-type (reg-map reg)
  "Clear any known type for VM register REG."
  (let ((known-types (wasm-reg-map-known-types reg-map)))
    (when known-types (remhash reg known-types))))

(defun wasm-ref-cast-maybe (type-wat reg-map reg)
  "Return WAT for a ref.cast to TYPE-WAT for register REG, 
   SKIPPING the cast when FR-142 determines the register's type is already known
   to match TYPE-WAT."
  (if *wasm-ref-cast-elimination-enabled*
      (let ((known (reg-known-type reg-map reg)))
        (let ((type-str (if (stringp type-wat) type-wat (format nil "~A" type-wat))))
          (if (search type-str (or (format nil "~A" known) "") :test #'char-equal)
              (format nil "~A" (reg-local-ref reg-map reg))
              (format nil "(ref.cast ~A ~A)" type-wat (reg-local-ref reg-map reg)))))
      (format nil "(ref.cast ~A ~A)" type-wat (reg-local-ref reg-map reg))))

(defun wasm-fixnum-unbox (reg-map reg)
  "Unbox a fixnum from i31ref to i64. Assumes reg holds an i31ref fixnum.
   FR-142: When register is known to hold i31ref, uses direct i31.get_s;
   FR-145: When register holds a known constant, skips unbox entirely."
  (if *wasm-ref-cast-elimination-enabled*
      (let ((known (reg-known-type reg-map reg)))
        (case known
          (:i31ref (format nil "(i64.extend_i32_s (i31.get_s ~A))" (reg-local-ref reg-map reg)))
          (:i64-unboxed (format nil "~A" (reg-local-ref reg-map reg)))
          (otherwise (format nil "(i64.extend_i32_s (i31.get_s ~A))" (reg-local-ref reg-map reg)))))
      (format nil "(i64.extend_i32_s (i31.get_s ~A))" (reg-local-ref reg-map reg))))

(defun wasm-fixnum-box (i64-wat)
  "Box an i64 as an i31ref fixnum."
  (format nil "(ref.i31 (i32.wrap_i64 ~A))" i64-wat))

(defun wasm-bool-to-i31 (cond-wat)
  "Convert a WASM i32 boolean (0/1) to i31ref (nil/t).
   0 -> ref.null eq (nil), 1 -> (ref.i31 (i32.const 1)) (truthy)."
  (format nil "(if (result eqref) ~A (then (ref.i31 (i32.const 1))) (else (ref.null eq)))"
          cond-wat))

(defun %wasm-captured-value-reg (capture)
  "Return the VM register carrying CAPTURE's value."
  (if (consp capture) (cdr capture) capture))

(defun emit-wasm-closure-allocation (reg-map dst entry-index captured stream indent)
  "Emit closure allocation using $closure_t and $env_t GC structs.
   FR-144: When *wasm-typed-closure-env-enabled*, uses array.new_fixed directly
   for the environment instead of wrapping in $env_t struct + array.new/set loop.
   FR-142: Records the closure type on the destination register for ref.cast elimination.
   
Captured values are materialized into a mutable $eqref_array_t with array.new and
array.set before the closure struct is created."
  (let ((prefix (make-string indent :initial-element #\Space)))
    (if captured
        (if *wasm-typed-closure-env-enabled*
            (progn
              ;; FR-144: Typed env path — use array.new_fixed directly
              (format stream "~%~A;; FR-144: typed closure env using array.new_fixed" prefix)
              (format stream "~%~A(local.set ~D (call $host_alloc_env ~A ~{~A~^ ~}))"
                      prefix (wasm-reg-map-tmp-index reg-map)
                      (length captured)
                      (loop for capture in captured
                            for reg = (%wasm-captured-value-reg capture)
                            collect (reg-local-ref reg-map reg)))
              (format stream "~%~A~A"
                      prefix
                      (reg-local-set
                       reg-map dst
                       (format nil "(struct.new $closure_t (i32.const ~D) ~A)"
                               entry-index
                               (reg-local-ref reg-map (wasm-reg-map-tmp-index reg-map)))))
              (reg-record-type reg-map dst :closure))
            (progn
              ;; Original env struct path
              (let ((tmp (wasm-reg-map-tmp-index reg-map)))
                (format stream "~%~A(local.set ~D (array.new $eqref_array_t (ref.null eq) (i32.const ~D)))"
                        prefix tmp (length captured))
                (loop for capture in captured
                      for idx from 0
                      for reg = (%wasm-captured-value-reg capture)
                      do (format stream "~%~A(array.set $eqref_array_t (ref.cast (ref $eqref_array_t) (local.get ~D)) (i32.const ~D) ~A)"
                                 prefix tmp idx (reg-local-ref reg-map reg)))
                (format stream "~%~A~A"
                        prefix
                        (reg-local-set
                         reg-map dst
                         (format nil "(struct.new $closure_t (i32.const ~D) (struct.new $env_t (ref.cast (ref $eqref_array_t) (local.get ~D)) (ref.null $env_t)))"
                                 entry-index tmp)))
                (reg-record-type reg-map dst :closure))))
        (progn
          (format stream "~%~A~A"
                  prefix
                  (reg-local-set
                   reg-map dst
                   (format nil "(struct.new $closure_t (i32.const ~D) (ref.null $env_t))"
                           entry-index)))
          (reg-record-type reg-map dst :closure)))))

;;; FR-237: host_alloc_env — host function stub for typed closure env allocation
;;; In production, this would generate array.new_fixed directly.
;;; For now it's a bridge function called from the wasm module.

(defun wasm-closure-ref-wat (reg-map closure-reg index)
  "Return WAT for reading captured INDEX from CLOSURE-REG.
   FR-144: When typed env is enabled and the closure-reg is known to be :closure,
   uses array.get directly on the env field (skipping $env_t wrapping)."
  (if *wasm-typed-closure-env-enabled*
      ;; FR-144 typed env path: closure.env is directly the array
      (format nil "(array.get $eqref_array_t (struct.get $closure_t 1 ~A) (i32.const ~D))"
              (wasm-ref-cast-maybe "(ref $closure_t)" reg-map closure-reg)
              index)
      ;; Original path: closure → env → vars array → indexed element
      (format nil "(array.get $eqref_array_t (struct.get $env_t 0 (ref.cast (ref $env_t) (struct.get $closure_t 1 ~A))) (i32.const ~D))"
              (wasm-ref-cast-maybe "(ref $closure_t)" reg-map closure-reg)
              index)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Fixnum operation helpers (used by emit-trampoline-instruction)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-i64-binop (reg-map dst lhs rhs op)
  "WAT for: dst = box(op(unbox(lhs), unbox(rhs)))."
  (reg-local-set reg-map dst
                 (wasm-fixnum-box
                  (format nil "(~A ~A ~A)" op
                          (wasm-fixnum-unbox reg-map lhs)
                          (wasm-fixnum-unbox reg-map rhs)))))

(defun wasm-i64-cmp (reg-map dst lhs rhs cmp-op)
  "WAT for: dst = T if cmp-op(unbox(lhs), unbox(rhs)), else NIL."
  (reg-local-set reg-map dst
                 (wasm-bool-to-i31
                  (format nil "(~A ~A ~A)" cmp-op
                          (wasm-fixnum-unbox reg-map lhs)
                          (wasm-fixnum-unbox reg-map rhs)))))

(defun emit-trampoline-jump-to-label (label-name label-pc-map reg-map stream)
  "Emit WAT to set $pc to the index for LABEL-NAME and branch back to $dispatch."
  (let ((pc-idx (gethash label-name label-pc-map)))
    (if pc-idx
        (format stream "~%      (local.set ~D (i32.const ~D))"
                (wasm-reg-map-pc-index reg-map) pc-idx)
        (format stream "~%      ;; WARNING: unknown label ~S" label-name))
    (format stream "~%      (br $dispatch)")))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-228: Bulk Memory Operations helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-bulk-memory-enabled* t
  "Feature flag for Wasm Bulk Memory Operations proposal (FR-228).")

(defun wasm-memory-copy-wat (dst-offset-wat src-offset-wat size-wat)
  "Return WAT for memory.copy (dst src size)."
  (format nil "(memory.copy ~A ~A ~A)" dst-offset-wat src-offset-wat size-wat))

(defun wasm-memory-fill-wat (dst-offset-wat value-wat size-wat)
  "Return WAT for memory.fill (dst value size)."
  (format nil "(memory.fill ~A ~A ~A)" dst-offset-wat value-wat size-wat))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-324: copysign — float-sign implementation via f64.copysign
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-copysign-wat (magnitude-wat sign-wat)
  "Return WAT for f64.copysign (magnitude, sign) — IEEE 754 copySign operation."
  (format nil "(f64.copysign ~A ~A)" magnitude-wat sign-wat))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-326: memory.grow OOM detection — storage-condition on allocation failure
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-memory-grow-oom-check-enabled* t
  "Feature flag for memory.grow OOM detection (FR-326).")

(defun wasm-memory-grow-checked-wat (pages-wat)
  "Return WAT for safe memory.grow with OOM check.
   Returns: i32 result or branches to $oom on failure."
  (format nil "(if (result i32) (i32.eq (memory.grow ~A) (i32.const -1)) (then (call $host_error (ref.null $string_t)) (unreachable)) (else (memory.size)))"
          pages-wat))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-233: Non-trapping float-to-int — saturating conversion helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-trunc-sat-f64-i64-wat (f64-wat)
  "Return WAT for non-trapping f64→i64 conversion (saturating)."
  (format nil "(i64.trunc_sat_f64_s ~A)" f64-wat))

(defun wasm-trunc-sat-f64-i32-wat (f64-wat)
  "Return WAT for non-trapping f64→i32 conversion (saturating)."
  (format nil "(i32.trunc_sat_f64_s ~A)" f64-wat))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-234: Sign-extension — 1-instruction replacements for shift pairs
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-sign-extend-32-8-wat (i32-wat)
  "Return WAT for i32.extend8_s."
  (format nil "(i32.extend8_s ~A)" i32-wat))

(defun wasm-sign-extend-32-16-wat (i32-wat)
  "Return WAT for i32.extend16_s."
  (format nil "(i32.extend16_s ~A)" i32-wat))

(defun wasm-sign-extend-64-32-wat (i64-wat)
  "Return WAT for i64.extend32_s."
  (format nil "(i64.extend32_s ~A)" i64-wat))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-323: MVP Bit Operations — clz/ctz/popcnt for integer-length/logcount
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-i64-clz-wat (i64-wat)
  "Return WAT for i64.clz — count leading zeros."
  (format nil "(i64.clz ~A)" i64-wat))

(defun wasm-i64-ctz-wat (i64-wat)
  "Return WAT for i64.ctz — count trailing zeros."
  (format nil "(i64.ctz ~A)" i64-wat))
