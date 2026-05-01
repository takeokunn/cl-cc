;;;; packages/mir/src/mir-builder.lisp — MIR Builder API and SSA Variable Tracking
;;;
;;; Extracted from mir.lisp.
;;; Contains:
;;;   - Builder API: mir-new-value, mir-new-block, mir-make-function,
;;;     mir-emit, mir-add-pred, mir-add-succ
;;;   - SSA variable tracking (Braun et al. 2013):
;;;     mir-write-var, mir-read-var, mir-read-var-recursive, mir-seal-block
;;;
;;; Depends on mir.lisp (mir-value/mir-block/mir-function/mir-inst structs,
;;;   mirv-*/mirb-*/mirf-*/miri-* accessors, make-mir-value/make-mir-block/
;;;   make-mir-function/make-mir-inst constructors).
;;; Load order: immediately after mir.lisp.

(in-package :cl-cc/mir)

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
      (setf (miri-srcs phi-inst)
            (loop for pred in (mirb-preds block)
                  collect (cons pred (mir-read-var fn var pred)))))
    (mirb-incomplete-phis block))
  (clrhash (mirb-incomplete-phis block))
  (setf (mirb-sealed-p block) t))
