;;;; compile/ir/types.lisp — Compile-Level IR Base Types
;;;;
;;;; Core SSA value, instruction, basic block, function and module defstructs
;;;; for the HIR/LIR compile pipeline.
;;;;
;;;; Naming convention: ir-  prefix for struct types, irv-/iri-/irb-/irf-/irm-
;;;; conc-name prefixes for accessors.  Distinct from packages/foundation/mir/src/mir.lisp
;;;; (mir-* = target-neutral emit-layer MIR for native codegen).

(in-package :cl-cc/ir)

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; IR Value (SSA virtual register)
;;;; ─────────────────────────────────────────────────────────────────────────

(defstruct (ir-value (:conc-name irv-))
  "An SSA value — defined exactly once, used zero or more times.
   Use IR-NEW-VALUE to allocate; never construct directly."
  (id   0   :type fixnum)  ; unique ID within owning function
  (type nil)               ; inferred/declared type annotation (nil = untyped)
  (def  nil))              ; back-pointer to the ir-inst that defines this value

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; IR Instruction (base)
;;;; ─────────────────────────────────────────────────────────────────────────

(defstruct (ir-inst (:conc-name iri-))
  "Base for all compile-level IR instructions.
   HIR subclasses (:include ir-inst) add structured control-flow fields.
   Specialize IR-OPERANDS to enumerate input ir-values."
  (result nil)   ; ir-value destination (nil for void/terminator)
  (block  nil))  ; back-pointer to owning ir-block

(defgeneric ir-operands (inst)
  (:documentation "Return list of ir-values used as inputs by INST.")
  (:method ((inst ir-inst)) nil))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; IR Basic Block  (block-argument style, not phi nodes)
;;;; ─────────────────────────────────────────────────────────────────────────

(defstruct (ir-block (:conc-name irb-))
  "A basic block: straight-line instructions with one entry and explicit exits.
   Uses block arguments (MLIR/Cranelift style) rather than phi nodes.
   All predecessors must be finalised before SEALED-P is set to T."
  (id           0    :type fixnum)  ; unique ID within function
  (label        nil  :type symbol)  ; debug label (:entry :then :loop-body ...)
  (params       nil  :type list)    ; list of ir-value  (block arguments / phi equiv.)
  (insts        nil  :type list)    ; ordered list of ir-inst (excludes terminator)
  (terminator   nil)                ; final ir-inst  (branch / return / unreachable)
  (predecessors nil  :type list)    ; predecessor ir-blocks
  (successors   nil  :type list)    ; successor ir-blocks
  (sealed-p     nil)                ; t when all predecessors are known
  (incomplete-phis                  ; var-name -> ir-value placeholder (pending seal)
   (make-hash-table :test #'eq)
   :type hash-table))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; IR Function
;;;; ─────────────────────────────────────────────────────────────────────────

(defstruct (ir-function (:conc-name irf-))
  "A function in compile IR form: entry block + all blocks."
  (name          nil              )   ; symbol
  (params        nil :type list   )   ; ir-value parameters at entry
  (entry         nil              )   ; entry ir-block
  (blocks        nil :type list   )   ; all ir-blocks in creation order
  (return-type   nil              )   ; declared or inferred return type
  (value-counter 0   :type fixnum )   ; monotone counter for ir-value IDs
  (block-counter 0   :type fixnum )   ; monotone counter for ir-block IDs
  (current-defs                       ; SSA variable tracking: (var . block-id) -> ir-value
   (make-hash-table :test #'equal)
   :type hash-table))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; IR Module
;;;; ─────────────────────────────────────────────────────────────────────────

(defstruct (ir-module (:conc-name irm-))
  "A compilation unit: list of IR functions plus global data."
  (functions nil :type list)   ; list of ir-function
  (globals   nil :type list))  ; list of (name . ir-value) global vars

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Allocators
;;;; ─────────────────────────────────────────────────────────────────────────

(defun ir-new-value (fn &key type)
  "Allocate a fresh ir-value in function FN with optional TYPE annotation."
  (let ((id (irf-value-counter fn)))
    (incf (irf-value-counter fn))
    (make-ir-value :id id :type type)))

(defun ir-new-block (fn &optional label)
  "Allocate a fresh ir-block in FN with an optional debug LABEL keyword.
   If LABEL is omitted, generates :BLOCK<id>."
  (let* ((id  (irf-block-counter fn))
         (sym (or label (intern (format nil "BLOCK~D" id) :keyword)))
         (blk (make-ir-block :id id :label sym)))
    (incf (irf-block-counter fn))
    (setf (irf-blocks fn) (nconc (irf-blocks fn) (list blk)))
    blk))

(defun ir-make-function (name &key return-type)
  "Create a new ir-function named NAME with a pre-allocated entry block.
   RETURN-TYPE is an optional type annotation for the function's return value."
  (let* ((fn    (make-ir-function :name name :return-type return-type))
         (entry (ir-new-block fn :entry)))
    (setf (irf-entry fn) entry)
    fn))
