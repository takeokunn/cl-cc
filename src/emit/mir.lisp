;;;; src/emit/mir.lisp — MIR: Target-Neutral SSA Intermediate Representation
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

(in-package :cl-cc)

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
  "A single MIR instruction. Immutable after construction.
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
;;;; Builder API — Allocating Values and Blocks
;;;; ────────────────────────────────────────────────────────────────────────

(defun mir-new-value (fn &key (name nil) (type :any))
  "Allocate a fresh SSA value in function FN."
  (let* ((id  (mirf-value-counter fn))
         (val (make-mir-value :id id :name name :type type)))
    (incf (mirf-value-counter fn))
    val))

(defun mir-new-block (fn &key (label nil))
  "Allocate a new basic block in function FN.
   LABEL is an optional keyword; defaults to :BLOCK<id>."
  (let* ((id  (mirf-block-counter fn))
         (lbl (or label (intern (format nil "BLOCK~D" id) :keyword)))
         (blk (make-mir-block :id id :label lbl)))
    (incf (mirf-block-counter fn))
    ;; Prepend; caller can reverse for RPO order later.
    (push blk (mirf-blocks fn))
    blk))

(defun mir-make-function (name &key (params nil))
  "Create a fresh mir-function with NAME and a pre-allocated entry block.
   PARAMS is a list of mir-values for the entry block parameters."
  (let* ((fn  (make-mir-function :name name :params params))
         (entry (mir-new-block fn :label :entry)))
    (setf (mirf-entry fn) entry)
    fn))

;;;; ────────────────────────────────────────────────────────────────────────
;;;; Builder API — Emitting Instructions
;;;; ────────────────────────────────────────────────────────────────────────

(defun mir-emit (block op &key (dst nil) (srcs nil) (type :any) (meta nil))
  "Emit a new instruction with OP into BLOCK. Returns the instruction.
   If DST is not provided, creates one automatically for non-terminator ops.
   Terminators (:ret :jump :branch :tail-call :safepoint) have no dst."
  (let* ((terminators '(:ret :jump :branch :tail-call))
         (no-dst-ops  (append terminators '(:store :safepoint :nop)))
         (actual-dst
           (cond
             ;; Caller explicitly passed a dst
             ((and dst (not (member op no-dst-ops))) dst)
             ;; Ops with no result
             ((member op no-dst-ops) nil)
             ;; Phi placeholder (dst must be provided by caller)
             ((eq op :phi) dst)
             ;; Otherwise nil (caller should pass dst or use return value)
             (t dst)))
         (inst (make-mir-inst :op op :dst actual-dst :srcs srcs
                              :type type :block block :meta meta)))
    (when actual-dst
      (setf (mirv-def-inst actual-dst) inst))
    (if (eq op :phi)
        ;; Phi nodes go to the block's phi list, not insts
        (push inst (mirb-phis block))
        (setf (mirb-insts block)
              (nconc (mirb-insts block) (list inst))))
    inst))

(defun mir-add-pred (block pred)
  "Register PRED as a predecessor of BLOCK."
  (pushnew pred (mirb-preds block) :test #'eq))

(defun mir-add-succ (block succ)
  "Register SUCC as a successor of BLOCK and BLOCK as a predecessor of SUCC."
  (pushnew succ (mirb-succs block) :test #'eq)
  (mir-add-pred succ block))

;;;; ────────────────────────────────────────────────────────────────────────
;;;; SSA Variable Tracking (Braun et al. 2013)
;;;; ────────────────────────────────────────────────────────────────────────
;;;
;;; MIR-WRITE-VAR / MIR-READ-VAR implement incremental SSA construction.
;;; The lowering pass (lower.lisp) calls these instead of managing phi
;;; placement manually.
;;;
;;; Algorithm summary:
;;;   write: record current definition for (var, block) in fn's current-defs
;;;   read: look up current def; if undefined and block has multiple preds,
;;;         insert an (incomplete) phi; if single pred, recurse into pred.
;;;         Blocks must be SEALED before reads can be fully resolved.

(defun mir-write-var (fn var block value)
  "Record that VAR is defined as VALUE at the end of BLOCK in function FN."
  (setf (gethash (cons var (mirb-id block))
                 (mirf-current-defs fn))
        value))

(defun mir-read-var (fn var block)
  "Read the current SSA definition of VAR in BLOCK.
   May insert phi nodes (or incomplete phi placeholders if block is not sealed).
   Returns the mir-value that represents VAR in BLOCK's scope."
  (let ((local (gethash (cons var (mirb-id block))
                        (mirf-current-defs fn))))
    (if local
        local
        (mir-read-var-recursive fn var block))))

(defun mir-read-var-recursive (fn var block)
  "Internal: VAR has no local definition in BLOCK. Look up predecessors."
  (let ((val
          (cond
            ;; Block not yet sealed: insert incomplete phi for later resolution
            ((not (mirb-sealed-p block))
             (let* ((phi-dst (mir-new-value fn :name var))
                    (phi     (mir-emit block :phi :dst phi-dst)))
               (setf (gethash var (mirb-incomplete-phis block)) phi)
               phi-dst))
            ;; Single predecessor: just recurse, no phi needed
            ((= (length (mirb-preds block)) 1)
             (mir-read-var fn var (first (mirb-preds block))))
            ;; Multiple predecessors: insert phi and gather operands
            (t
             (let* ((phi-dst (mir-new-value fn :name var))
                    (phi     (mir-emit block :phi :dst phi-dst)))
               ;; Temporarily write to break cycles
               (mir-write-var fn var block phi-dst)
               ;; Gather operand from each predecessor
               (setf (miri-srcs phi)
                     (loop for pred in (mirb-preds block)
                           collect (cons pred (mir-read-var fn var pred))))
               phi-dst)))))
    (mir-write-var fn var block val)
    val))

(defun mir-seal-block (fn block)
  "Seal BLOCK — declare that all predecessors are finalised.
   Resolves any incomplete phi nodes inserted before sealing."
  (maphash
    (lambda (var phi-inst)
      ;; Fill in phi operands from all predecessors
      (setf (miri-srcs phi-inst)
            (loop for pred in (mirb-preds block)
                  collect (cons pred (mir-read-var fn var pred)))))
    (mirb-incomplete-phis block))
  (clrhash (mirb-incomplete-phis block))
  (setf (mirb-sealed-p block) t))

;;;; ────────────────────────────────────────────────────────────────────────
;;;; CFG Utilities
;;;; ────────────────────────────────────────────────────────────────────────

(defun mir-rpo (fn)
  "Return all blocks of FN in reverse post-order (standard IR traversal order).
   RPO ensures every block appears before its successors (except back edges)."
  (let ((visited (make-hash-table))
        (result  nil))
    (labels ((dfs (blk)
               (unless (gethash (mirb-id blk) visited)
                 (setf (gethash (mirb-id blk) visited) t)
                 (dolist (succ (mirb-succs blk))
                   (dfs succ))
                 (push blk result))))
      (when (mirf-entry fn)
        (dfs (mirf-entry fn))))
    result))

(defun mir-dominators (fn)
  "Compute immediate dominators for FN's blocks.
   Returns a hash table: block-id → immediate-dominator-block.
   Uses Cooper et al. 2001 (simple, fast, correct for most CFGs)."
  (let* ((blocks    (mir-rpo fn))
         (idom      (make-hash-table))
         (rpo-index (make-hash-table)))
    ;; Assign RPO numbers
    (loop for blk in blocks for i from 0
          do (setf (gethash (mirb-id blk) rpo-index) i))
    ;; Entry dominates itself
    (when (mirf-entry fn)
      (setf (gethash (mirb-id (mirf-entry fn)) idom)
            (mirf-entry fn)))
    ;; Fixed-point iteration
    (let ((changed t))
      (loop while changed do
        (setf changed nil)
        (dolist (blk (rest blocks))           ; skip entry
          (let ((new-idom nil))
            (dolist (pred (mirb-preds blk))
              (when (gethash (mirb-id pred) idom)
                (setf new-idom
                      (if new-idom
                          (mir--intersect pred new-idom idom rpo-index)
                          pred))))
            (when (and new-idom
                       (not (eq new-idom (gethash (mirb-id blk) idom))))
              (setf (gethash (mirb-id blk) idom) new-idom)
              (setf changed t))))))
    idom))

(defun mir--intersect (b1 b2 idom rpo-index)
  "Cooper et al. dominator intersection helper. Internal use only."
  (let ((f1 b1) (f2 b2))
    (loop until (eq f1 f2) do
      (loop while (> (gethash (mirb-id f1) rpo-index 0)
                     (gethash (mirb-id f2) rpo-index 0))
            do (setf f1 (gethash (mirb-id f1) idom f1)))
      (loop while (> (gethash (mirb-id f2) rpo-index 0)
                     (gethash (mirb-id f1) rpo-index 0))
            do (setf f2 (gethash (mirb-id f2) idom f2))))
    f1))

;;;; ────────────────────────────────────────────────────────────────────────
;;;; Printer (Debuggability)
;;;; ────────────────────────────────────────────────────────────────────────

(defun mir-format-value (v)
  "Return a human-readable string for MIR value/const operand V."
  (cond
    ((mir-value-p v)
     (format nil "%~D~@[/~A~]" (mirv-id v) (mirv-name v)))
    ((mir-const-p v)
     (format nil "#~S" (mirc-value v)))
    ((and (consp v) (mir-block-p (car v)) (mir-value-p (cdr v)))
     ;; phi operand: (pred-block . value)
     (format nil "[~A:~A]"
             (mirb-label (car v))
             (mir-format-value (cdr v))))
    (t (format nil "~S" v))))

(defun mir-print-inst (inst &optional (stream *standard-output*) (indent 2))
  "Print a MIR instruction in readable pseudo-assembly form."
  (format stream "~vT" indent)
  (when (miri-dst inst)
    (format stream "~A = " (mir-format-value (miri-dst inst))))
  (format stream "~A" (miri-op inst))
  (dolist (src (miri-srcs inst))
    (format stream "  ~A" (mir-format-value src)))
  (when (miri-meta inst)
    (format stream "   ; ~S" (miri-meta inst)))
  (terpri stream))

(defun mir-print-block (blk &optional (stream *standard-output*))
  "Print a MIR basic block with label, preds annotation, phi nodes, and body."
  (format stream "~&~A:" (mirb-label blk))
  (when (mirb-preds blk)
    (format stream "  ; preds: ~{~A~^, ~}"
            (mapcar #'mirb-label (mirb-preds blk))))
  (terpri stream)
  (dolist (phi (mirb-phis blk))
    (mir-print-inst phi stream))
  (dolist (inst (mirb-insts blk))
    (mir-print-inst inst stream))
  (terpri stream))

(defun mir-print-function (fn &optional (stream *standard-output*))
  "Print a complete MIR function to STREAM for debugging."
  (format stream "~&;;; MIR ~A (~{~A~^, ~})~%"
          (mirf-name fn)
          (mapcar #'mir-format-value (mirf-params fn)))
  (dolist (blk (mir-rpo fn))
    (mir-print-block blk stream)))
