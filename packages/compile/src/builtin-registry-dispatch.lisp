;;;; compile/builtin-registry-dispatch.lisp — Convention Dispatcher
;;;
;;; Extracted from builtin-registry-emitters.lisp.
;;; Contains:
;;;   - *builtin-emitter-table*   (convention keyword → emitter function)
;;;   - *convention-arity*        (convention → (min . max) arg bounds)
;;;   - emit-registered-builtin   (public entry point: unified dispatcher)
;;;
;;; All emit-builtin-* emitter functions are in builtin-registry-emitters.lisp
;;; (loads before this file).
;;;
;;; Load order: after builtin-registry-emitters.lisp.
(in-package :cl-cc/compile)

;;; ─── Convention Dispatcher ─────────────────────────────────────────────────

(defparameter *builtin-emitter-table*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (pair '((:unary              . emit-builtin-unary)
                    (:binary             . emit-builtin-binary)
                    (:string-cmp         . emit-builtin-string-cmp)
                    (:char-cmp           . emit-builtin-char-cmp)
                    (:table-query        . emit-builtin-table-query)
                    (:handle-input       . emit-builtin-handle-input)
                    (:side-effect        . emit-builtin-side-effect)
                    (:void-side-eff      . emit-builtin-void-side-eff)
                    (:nullary            . emit-builtin-nullary)
                    (:string-trim        . emit-builtin-string-trim)
                    (:handle-effect      . emit-builtin-handle-effect)
                    (:binary-custom      . emit-builtin-binary-custom)
                    (:binary-move-first  . emit-builtin-binary-move-first)
                    (:binary-void        . emit-builtin-binary-void)
                    (:unary-custom-void  . emit-builtin-unary-custom-void)
                    (:unary-custom       . emit-builtin-unary-custom)
                    (:unary-opt-nil      . emit-builtin-unary-opt-nil)
                    (:binary-opt-one     . emit-builtin-binary-opt-one)
                    (:ternary-opt-nil-custom . emit-builtin-ternary-opt-nil-custom)
                    (:binary-opt-nil-slot . emit-builtin-binary-opt-nil-slot)
                    (:binary-synth-zero  . emit-builtin-binary-synth-zero)
                    (:zero-compare       . emit-builtin-zero-compare)
                    (:stream-input-opt   . emit-builtin-stream-input-opt)
                    (:stream-void-opt    . emit-builtin-stream-void-opt)
                    (:stream-write-val   . emit-builtin-stream-write-val)
                    (:ternary-custom     . emit-builtin-ternary-custom)))
      (setf (gethash (car pair) ht)
            (symbol-function (cdr pair))))
    ht)
  "Maps convention keywords to their emitter functions.")

(defparameter *convention-arity*
  '((:unary          1 . 1)  (:binary          2 . 2)  (:binary-custom    2 . 2)
    (:string-cmp     2 . 2)  (:char-cmp        2 . 2)  (:table-query      1 . 1)
    (:handle-input   1 . 1)  (:side-effect     1 . 1)  (:void-side-eff    0 . 0)
    (:nullary        0 . 0)  (:string-trim     2 . 2)  (:handle-effect    1 . 1)
    (:zero-compare   1 . 1)  (:unary-custom    1 . 1)  (:unary-custom-void 1 . 1)
    (:binary-move-first 2 . 2) (:binary-void  2 . 2)  (:binary-synth-zero 2 . 2)
    (:unary-opt-nil  0 . 1)  (:binary-opt-one  1 . 2)  (:binary-opt-nil-slot 1 . 2)
    (:ternary-custom 3 . 3)  (:ternary-opt-nil-custom 2 . 3)
    (:stream-input-opt 0 . 1) (:stream-void-opt 0 . 1) (:stream-write-val 1 . 2))
  "Alist of (convention min-args . max-args) for argument count validation.")

(defun emit-registered-builtin (entry args result-reg ctx)
  "Dispatch to the correct emitter for ENTRY's calling convention.
   Returns result-reg on success, or NIL if arg count is out of range."
  (let* ((conv (be-convention entry))
         (arity (cdr (assoc conv *convention-arity* :test #'eq)))
         (nargs (length args)))
    (when (and arity (or (< nargs (car arity)) (> nargs (cdr arity))))
      (return-from emit-registered-builtin nil))
    (let ((emitter (gethash conv *builtin-emitter-table*)))
      (when emitter
        (funcall emitter entry args result-reg ctx)))))
