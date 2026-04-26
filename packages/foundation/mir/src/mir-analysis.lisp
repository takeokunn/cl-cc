;;;; packages/foundation/mir/src/mir-analysis.lisp — MIR Analysis and Printing
;;;;
;;;; Contains:
;;;;   mir-rpo — reverse post-order traversal
;;;;   mir-dominators — dominator computation (Cooper et al.)
;;;;   mir--intersect — dominator intersection helper
;;;;   mir-format-value, mir-print-inst, mir-print-block, mir-print-function — formatters
;;;;
;;;; MIR data structures (mir-value, mir-inst, mir-block, mir-function, mir-module)
;;;; and the builder API are in mir.lisp (loads before).
;;;;
;;;; Load order: after mir.lisp.

(in-package :cl-cc/mir)

(defun %mir-rpo-dfs (blk visited result-cell)
  "Post-order DFS: push BLK into (car RESULT-CELL) after visiting successors."
  (unless (gethash (mirb-id blk) visited)
    (setf (gethash (mirb-id blk) visited) t)
    (dolist (succ (mirb-succs blk))
      (%mir-rpo-dfs succ visited result-cell))
    (push blk (car result-cell))))

(defun mir-rpo (fn)
  "Return all blocks of FN in reverse post-order (standard IR traversal order)."
  (let ((visited     (make-hash-table))
        (result-cell (list nil)))
    (when (mirf-entry fn)
      (%mir-rpo-dfs (mirf-entry fn) visited result-cell))
    (car result-cell)))

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
