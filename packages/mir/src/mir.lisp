;;;; packages/mir/src/mir.lisp — MIR: Target-Neutral SSA Intermediate Representation
;;;
;;; Three-stage pipeline:
;;;   VM Instructions → [Stage 1: lower.lisp] →
;;;   Target-Abstract MIR (this layer, SSA / CFG) → [Stage 2: isel.lisp] →
;;;   Target-Specific MIR → [Stage 3: regalloc + encode]
;;;
;;; Design principles:
;;;   - CFG-based SSA (V8 abandoned Sea of Nodes in 2024; CFG is simpler + faster)
;;;   - Every dst is a unique mir-value (single-assignment)
;;;   - Phi nodes at block entries (Braun et al. 2013 incremental SSA construction)
;;;   - Generic op keywords are target-independent; isel rules rewrite them
;;;   - mir-safepoint carries GC root metadata (frontend-managed, per Cranelift 2024)
;;;   - mir-tail-call is first-class, lowered per target

(in-package :cl-cc/mir)

;;;; ────────────────────────────────────────────────────────────────────────
;;;; MIR Value (SSA Virtual Register)
;;;; ────────────────────────────────────────────────────────────────────────

(defstruct (mir-value (:conc-name mirv-))
  "An SSA value — the unique result of exactly one definition.
   Use MIR-NEW-VALUE to allocate; never construct directly."
  (id       0   :type fixnum)   ; globally unique ID within the function
  (name     nil)                ; optional debug name symbol
  (type     :any)               ; type annotation: :integer :pointer :boolean :any ...
  (def-inst nil)                ; back-pointer to the mir-inst that defines this value
  (use-count 0  :type fixnum))  ; number of uses (informational; updated by lowering)

;;;; ────────────────────────────────────────────────────────────────────────
;;;; MIR Constant (Immediate Operand)
;;;; ────────────────────────────────────────────────────────────────────────

(defstruct (mir-const (:conc-name mirc-))
  "A compile-time constant. Can appear as a src in any mir-inst."
  (value nil)   ; the CL value (fixnum, string, keyword, nil, t, ...)
  (type  :any)) ; type annotation

;;;; ────────────────────────────────────────────────────────────────────────
;;;; MIR Instruction
;;;; ────────────────────────────────────────────────────────────────────────

(defstruct (mir-inst (:conc-name miri-))
  "A single MIR instruction. Structural fields are immutable after construction;
   analysis passes may refine TYPE as facts become available.
   SRCS elements are mir-value, mir-const, or (pred-block . mir-value) for phi nodes."
  (op   :nop  :type keyword) ; operation (see *MIR-GENERIC-OPS*)
  (dst  nil)                 ; mir-value result, or nil for terminators/side-effects
  (srcs nil   :type list)    ; list of operands (mir-value / mir-const / phi-pairs)
  (type :any)                ; result type annotation
  (block nil)                ; back-pointer to owning mir-block
  (meta nil))                ; target-specific metadata (GC stack-map, debug info, ...)

;;;; ────────────────────────────────────────────────────────────────────────
;;;; MIR Basic Block
;;;; ────────────────────────────────────────────────────────────────────────

(defstruct (mir-block (:conc-name mirb-))
  "A basic block — a straight-line sequence of instructions with a single entry
   and one or more exits (the last instruction is always a terminator).

   SEALED-P becomes T when all predecessors are known, enabling phi resolution
   (Braun et al. 2013 incremental SSA construction)."
  (id   0   :type fixnum)      ; unique ID within function
  (label nil)                  ; keyword label (e.g. :block0, :then, :loop-header)
  (insts nil :type list)       ; ordered list of mir-inst (phi nodes NOT included here)
  (preds nil :type list)       ; predecessor mir-blocks
  (succs nil :type list)       ; successor mir-blocks
  (sealed-p nil)               ; t when all predecessors are finalised
  (phis nil :type list)        ; phi mir-insts at block entry (prepended during sealing)
  (incomplete-phis             ; var-name → phi-mir-inst (pending resolution)
   (make-hash-table)
   :type hash-table))

;;;; ────────────────────────────────────────────────────────────────────────
;;;; MIR Function
;;;; ────────────────────────────────────────────────────────────────────────

(defstruct (mir-function (:conc-name mirf-))
  "A function in MIR form.

   CURRENT-DEFS maps (var-name . block-id) → mir-value for Braun et al. SSA
   construction. LOWER.LISP uses this to thread definitions across blocks."
  (name         nil)                       ; function name symbol
  (params       nil :type list)            ; entry-block parameter mir-values
  (blocks       nil :type list)            ; all mir-blocks (prepended; reverse for RPO)
  (entry        nil)                       ; entry mir-block
  (current-defs                            ; SSA variable tracking: (var . block-id) → mirv
   (make-hash-table :test #'equal)
   :type hash-table)
  (value-counter 0 :type fixnum)           ; counter for fresh mir-value IDs
  (block-counter 0 :type fixnum))          ; counter for fresh mir-block IDs

;;;; ────────────────────────────────────────────────────────────────────────
;;;; MIR Module
;;;; ────────────────────────────────────────────────────────────────────────

(defstruct (mir-module (:conc-name mirm-))
  "A compilation unit: one or more MIR functions plus global data."
  (functions    nil :type list)            ; list of mir-function
  (globals      nil :type list)            ; list of (name . mir-value) for global vars
  (string-table                            ; string literal → label keyword
   (make-hash-table :test #'equal)
   :type hash-table))

;;;; ────────────────────────────────────────────────────────────────────────
;;;; Generic Op Keyword Vocabulary
;;;; ────────────────────────────────────────────────────────────────────────
;;;
;;; These are target-INDEPENDENT operations.  The isel layer (Stage 2) rewrites
;;; each generic op to one or more target-specific machine ops via isel rules.
;;; Legalization rules may decompose ops not supported on a given target
;;; (e.g. :mod on AArch64 → :sub/:mul/:div sequence).

(defvar *mir-generic-ops*
  '(;; Constants and moves
    :const           ; (dst const)         — load compile-time constant into dst
    :move            ; (dst src)            — register copy
    ;; Arithmetic (integer)
    :add             ; (dst a b)            — integer add
    :sub             ; (dst a b)            — integer subtract
    :mul             ; (dst a b)            — integer multiply
    :div             ; (dst a b)            — signed integer divide
    :mod             ; (dst a b)            — signed integer modulo
    :neg             ; (dst a)              — unary negate
    ;; Bitwise
    :band            ; (dst a b)            — bitwise AND
    :bor             ; (dst a b)            — bitwise OR
    :bxor            ; (dst a b)            — bitwise XOR
    :bnot            ; (dst a)              — bitwise NOT
    :shl             ; (dst a shift)        — left shift
    :shr             ; (dst a shift)        — arithmetic right shift
    :ushr            ; (dst a shift)        — logical (unsigned) right shift
    ;; Comparisons (produce boolean result)
    :lt              ; (dst a b)            — signed less-than
    :le              ; (dst a b)            — signed less-equal
    :gt              ; (dst a b)            — signed greater-than
    :ge              ; (dst a b)            — signed greater-equal
    :eq              ; (dst a b)            — equal
    :ne              ; (dst a b)            — not-equal
    ;; Memory
    :load            ; (dst ptr offset)     — memory load
    :store           ; (ptr offset val)     — memory store (no dst)
    :alloca          ; (dst size)           — stack allocation; returns pointer
    ;; Calls
    :call            ; (dst fn . args)      — regular function call
    :tail-call       ; (fn . args)          — tail call; terminates block (no dst)
    ;; Control flow (all terminate their basic block)
    :ret             ; (val)                — return value; terminates block
    :jump            ; (target-block)       — unconditional branch; terminates block
    :branch          ; (cond then-blk else-blk) — conditional branch; terminates block
    ;; SSA
    :phi             ; (dst (pred1 . v1) (pred2 . v2) ...) — phi node
    ;; Multiple values
    :values          ; (dst . vals)         — pack multiple values
    :mv-bind         ; (dsts . src)         — unpack multiple values
    ;; GC
    :safepoint       ; (. gc-roots)         — GC safepoint; attaches stack-map metadata
    ;; Misc
     :nop)            ; ()                   — no operation (placeholder)
  "All target-independent MIR operation keywords.")

;;;; ────────────────────────────────────────────────────────────────────────
;;;; MIR Effect Classification and Type Propagation
;;;; ────────────────────────────────────────────────────────────────────────

(defun %mir-effect-table (&rest groups)
  "Build an OP → effect-kind hash table from GROUPS.
Each group is `(effect-kind op...)'.  Effect-kind names mirror the optimizer
effect lattice so MIR can carry target-neutral side-effect facts without
depending on the optimizer system."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (group groups table)
      (destructuring-bind (effect-kind &rest ops) group
        (dolist (op ops)
          (setf (gethash op table) effect-kind))))))

(defparameter *mir-effect-kind-table*
  (%mir-effect-table
   '(:pure :const :move :add :sub :mul :neg
     :band :bor :bxor :bnot :shl :shr :ushr
     :lt :le :gt :ge :eq :ne :phi :values :mv-bind :nop)
   '(:read-only :load)
   '(:alloc :alloca)
   '(:write-global :store)
   '(:control :div :mod :ret :jump :branch :tail-call :safepoint)
   '(:unknown :call))
  "Maps MIR op keywords to target-neutral effect-kind keywords.")

(defun mir-op-effect-kind (op)
  "Return the target-neutral effect-kind for MIR operation OP."
  (gethash op *mir-effect-kind-table* :unknown))

(defun %mir-meta-effect-kind (meta)
  "Return an explicit :EFFECT-KIND override from META when META is a plist."
  (loop for rest = meta then (cddr rest)
        while (consp rest)
        when (not (consp (cdr rest)))
          do (return nil)
        when (eq (car rest) :effect-kind)
          do (return (cadr rest))
        finally (return nil)))

(defun mir-inst-effect-kind (inst)
  "Return the effect-kind for MIR instruction INST.

Instruction metadata may override the default with `(:effect-kind KIND)`, which
lets lowering or target selection preserve more precise facts for calls and
target-specific operations."
  (or (%mir-meta-effect-kind (miri-meta inst))
      (mir-op-effect-kind (miri-op inst))))

(defun mir-inst-pure-p (inst)
  "Return T when INST is side-effect free and deterministic."
  (eq (mir-inst-effect-kind inst) :pure))

(defun mir-inst-dce-eligible-p (inst)
  "Return T when unused INST may be removed by dead-code elimination.

Pure instructions and allocation-only instructions are eligible when their
result is unused; observable reads, writes, control flow, and unknown calls are
not."
  (not (null (member (mir-inst-effect-kind inst) '(:pure :alloc) :test #'eq))))

(defun mir-operand-type (operand)
  "Return the MIR type annotation for OPERAND, defaulting to :ANY."
  (cond
    ((mir-value-p operand) (mirv-type operand))
    ((mir-const-p operand) (mirc-type operand))
    ((and (consp operand)
          (mir-value-p (cdr operand)))
     (mirv-type (cdr operand)))
    (t :any)))

(defun %mir-join-type-list (types)
  "Return a conservative join for a list of MIR type annotations."
  (let ((filtered (remove nil types)))
    (cond
      ((null filtered) :any)
      ((member :any filtered :test #'eq) :any)
      ((every (lambda (type) (eq type (first filtered))) (rest filtered))
       (first filtered))
      (t :any))))

(defun mir-join-types (&rest types)
  "Return a conservative join for TYPES.

The current MIR type vocabulary is intentionally small, so conflicting concrete
types join to :ANY instead of inventing a union representation."
  (%mir-join-type-list types))

(defun %mir-operands-all-type-p (operands expected-type)
  "Return T when every OPERANDS entry has EXPECTED-TYPE."
  (and operands
       (every (lambda (operand)
                (eq (mir-operand-type operand) expected-type))
              operands)))

(defun mir-infer-inst-type (inst)
  "Infer a conservative result type for MIR instruction INST."
  (let ((srcs (miri-srcs inst)))
    (case (miri-op inst)
      (:const
       (if srcs (mir-operand-type (first srcs)) (miri-type inst)))
      (:move
       (if srcs (mir-operand-type (first srcs)) (miri-type inst)))
      ((:add :sub :mul :div :mod :neg
        :band :bor :bxor :bnot :shl :shr :ushr)
       (if (%mir-operands-all-type-p srcs :integer)
           :integer
           (miri-type inst)))
      ((:lt :le :gt :ge :eq :ne)
       :boolean)
      (:load
       (miri-type inst))
      (:alloca
       :pointer)
      (:phi
       (%mir-join-type-list (mapcar #'mir-operand-type srcs)))
      (:values
       :values)
      (:mv-bind
       (miri-type inst))
      ((:store :ret :jump :branch :tail-call :safepoint :nop)
       :void)
      (otherwise
       (miri-type inst)))))

(defun mir-propagate-inst-type (inst)
  "Infer and store INST's result type, updating its destination value if any."
  (let ((type (mir-infer-inst-type inst)))
    (setf (miri-type inst) type)
    (when (miri-dst inst)
      (setf (mirv-type (miri-dst inst)) type))
    type))

(defun %mir-propagate-inst-type-changed-p (inst)
  "Propagate INST's type and return T when any stored annotation changed."
  (let ((old-inst-type (miri-type inst))
        (dst (miri-dst inst)))
    (let ((old-dst-type (and dst (mirv-type dst)))
          (new-type (mir-propagate-inst-type inst)))
      (or (not (eq old-inst-type new-type))
          (and dst (not (eq old-dst-type (mirv-type dst))))))))

(defun %mir-propagate-block-types-changed-p (block)
  "Propagate type facts through BLOCK and return T when any fact changed."
  (let ((changed nil))
    (dolist (phi (mirb-phis block))
      (when (%mir-propagate-inst-type-changed-p phi)
        (setf changed t)))
    (dolist (inst (mirb-insts block))
      (when (%mir-propagate-inst-type-changed-p inst)
        (setf changed t)))
    changed))

(defun mir-propagate-types (fn &key (max-iterations 32))
  "Conservatively propagate MIR value types through FN to a fixed point."
  (let ((blocks (mir-rpo fn)))
    (loop repeat max-iterations
          for changed = nil
          do (progn
               (dolist (block blocks)
                 (when (%mir-propagate-block-types-changed-p block)
                   (setf changed t))))
          until (not changed)
          finally (return fn))))

;;; Builder API (mir-new-value, mir-new-block, mir-make-function,
;;; mir-emit, mir-add-pred, mir-add-succ) and SSA variable tracking
;;; (mir-write-var, mir-read-var, mir-read-var-recursive, mir-seal-block)
;;; are in mir-builder.lisp (loaded next).
;;; RPO traversal, dominator analysis, and printing functions
;;; are in mir-analysis.lisp (loaded after mir-builder).
