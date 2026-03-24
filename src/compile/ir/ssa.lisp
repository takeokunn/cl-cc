;;;; compile/ir/ssa.lisp — SSA Construction via Braun et al. 2013
;;;;
;;;; "Simple and Efficient Construction of Static Single Assignment Form"
;;;; Matthias Braun et al., CC 2013.
;;;;
;;;; Incremental SSA construction: as the HIR builder emits instructions it
;;;; records variable definitions (IR-WRITE-VAR) and looks them up
;;;; (IR-READ-VAR).  When multiple definitions reach a join point, a block
;;;; argument (equivalent to a phi node) is inserted automatically.
;;;;
;;;; Key difference from emit/mir.lisp: we use block-argument style rather than
;;;; explicit phi instructions.  The block argument sits in IRB-PARAMS; the
;;;; corresponding values to pass from each predecessor are recorded when the
;;;; block is sealed (IR-SEAL-BLOCK).

(in-package :cl-cc)

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Public API
;;;; ─────────────────────────────────────────────────────────────────────────

(defun ir-write-var (fn var block val)
  "Record that VAR is defined as ir-value VAL at the end of BLOCK in FN.
   Subsequent IR-READ-VAR calls from BLOCK or its successors will find VAL
   (unless another IR-WRITE-VAR for the same VAR intervenes)."
  (setf (gethash (cons var (irb-id block))
                 (irf-current-defs fn))
        val))

(defun ir-read-var (fn var block)
  "Return the current SSA ir-value for VAR as visible from BLOCK in FN.
   Inserts block arguments (phi equivalents) at join points as needed.
   May signal an error if VAR is genuinely unbound in all predecessors."
  (let ((local (gethash (cons var (irb-id block))
                        (irf-current-defs fn))))
    (if local
        local
        (ir--read-var-recursive fn var block))))

(defun ir-seal-block (fn block)
  "Seal BLOCK: declare that all predecessors are now known.
   Resolves any incomplete block arguments (phis) that were speculatively
   inserted before all predecessor edges were added.
   Must be called after adding all predecessor edges to BLOCK."
  (maphash
    (lambda (var placeholder)
      ;; Fill the predecessor-value associations now that preds are complete
      (ir--fill-block-arg fn var block placeholder))
    (irb-incomplete-phis block))
  (clrhash (irb-incomplete-phis block))
  (setf (irb-sealed-p block) t)
  block)

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Internal Helpers
;;;; ─────────────────────────────────────────────────────────────────────────

(defun ir--read-var-recursive (fn var block)
  "Internal: VAR has no local definition in BLOCK. Look up predecessors."
  (let ((preds (irb-predecessors block)))
    (cond
      ;; No predecessors: undefined variable — return nil (caller decides)
      ((null preds)
       nil)

      ;; Block not yet sealed: add an incomplete placeholder block argument.
      ;; It will be resolved when IR-SEAL-BLOCK is called.
      ((not (irb-sealed-p block))
       (let ((placeholder (ir-new-value fn)))
         (push placeholder (irb-params block))
         (setf (gethash var (irb-incomplete-phis block)) placeholder)
         (ir-write-var fn var block placeholder)
         placeholder))

      ;; Single predecessor: no merge needed, just recurse up.
      ((null (cdr preds))
       (let ((val (ir-read-var fn var (car preds))))
         (ir-write-var fn var block val)
         val))

      ;; Multiple predecessors: need a block argument to merge values.
      (t
       (let ((arg (ir-new-value fn)))
         (push arg (irb-params block))
         ;; Record immediately to break potential cycles (e.g. loops)
         (ir-write-var fn var block arg)
         ;; Try trivial elimination: if all preds give the same value,
         ;; replace the block arg with that value
         (ir--try-remove-trivial-arg fn var block arg))))))

(defun ir--fill-block-arg (fn var block placeholder)
  "Associate PLACEHOLDER with predecessor values for VAR in BLOCK.
   Returns the list of (pred-block . ir-value) pairs for documentation."
  (loop for pred in (irb-predecessors block)
        collect (cons pred (ir-read-var fn var pred))))

(defun ir--try-remove-trivial-arg (fn var block arg)
  "If all predecessors define VAR as the same ir-value, replace BLOCK ARG
   with that value and remove it from IRB-PARAMS.  Returns the canonical value."
  (let ((unique nil))
    (dolist (pred (irb-predecessors block))
      (let ((val (ir-read-var fn var pred)))
        (when (and val (not (eq val arg)))
          (if unique
              (when (not (eq val unique))
                ;; Multiple distinct values — cannot eliminate
                (return-from ir--try-remove-trivial-arg arg))
              (setf unique val)))))
    (cond
      ;; All preds give the same non-self value: replace
      ((and unique (not (eq unique arg)))
       (setf (irb-params block)
             (remove arg (irb-params block) :test #'eq))
       (ir-write-var fn var block unique)
       unique)
      ;; Only self-reference found (e.g. loop variable with no outer def)
      (t arg))))
