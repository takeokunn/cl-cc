(in-package :cl-cc/compile)

;;; ─── FR-860 / FR-861 Numeric Compile-Time Helpers ─────────────────────────
;;;
;;; Append-only helpers for numeric contagion inference and inline arithmetic
;;; dispatch planning.  The existing AST binop compiler remains untouched; these
;;; functions give later lowering passes and tests a stable codegen-facing API
;;; for selecting the VM dispatch-table entry introduced in primitives.lisp.

(defparameter *codegen-inline-arith-dispatch-enabled* t
  "When true, FR-861 planning helpers may choose VM-ARITH-DISPATCH for VM code.")

;;; ── FR-860 Contagion type map ─────────────────────────────────────────────

(defparameter *codegen-contagion-type-map*
  '((fixnum       . integer)
    (bignum       . integer)
    (integer      . integer)
    (ratio        . rational)
    (rational     . rational)
    (single-float . single-float)
    (double-float . double-float)
    (float        . double-float)
    (complex      . complex))
  "Maps compiler/CL numeric type designators to FR-860 contagion symbols.
Used by %codegen-normalize-contagion-type for fast alist lookup.")

(defun %codegen-normalize-contagion-type (type-designator)
  "Map compiler/CL numeric type designators to FR-860 contagion symbols."
  (if (and (consp type-designator) (eq (first type-designator) 'complex))
      'complex
      (cdr (assoc type-designator *codegen-contagion-type-map* :test #'eq))))

(defun codegen-infer-numeric-contagion-type (left-type right-type)
  "Return the FR-860 result type for LEFT-TYPE × RIGHT-TYPE, or NIL if unknown."
  (let ((left (%codegen-normalize-contagion-type left-type))
        (right (%codegen-normalize-contagion-type right-type)))
    (and left right (cl-cc/vm:infer-numeric-result-type left right))))

;;; ── FR-861 Arithmetic type tag map ───────────────────────────────────────

(defparameter *codegen-arith-type-tag-map*
  '((fixnum       . 0)
    (bignum       . 1)
    (integer      . 1)
    (ratio        . 2)
    (rational     . 2)
    (single-float . 3)
    (double-float . 3)
    (float        . 3)
    (complex      . 4))
  "Maps type designators to FR-861 inline dispatch type tags.
Used by %codegen-arith-type-tag-from-type for fast alist lookup.")

(defun %codegen-arith-type-tag-from-type (type-designator)
  "Return the FR-861 inline dispatch type tag implied by TYPE-DESIGNATOR."
  (if (or (eq type-designator 'complex)
          (and (consp type-designator) (eq (first type-designator) 'complex)))
      4
      (cdr (assoc type-designator *codegen-arith-type-tag-map* :test #'eq))))

;;; ── FR-861 Dispatch planning ─────────────────────────────────────────────

(defun codegen-inline-arith-dispatch-index (op left-type right-type)
  "Return the flattened FR-861 dispatch-table index for OP and operand types."
  (let ((left-tag (%codegen-arith-type-tag-from-type left-type))
        (right-tag (%codegen-arith-type-tag-from-type right-type)))
    (and left-tag right-tag
         (cl-cc/vm:arithmetic-dispatch-index
          (cl-cc/vm:arithmetic-op-tag op) left-tag right-tag))))

(defun codegen-inline-arith-dispatch-entry (op left-type right-type)
  "Return the VM inline arithmetic dispatch entry selected at compile time."
  (let ((index (codegen-inline-arith-dispatch-index op left-type right-type)))
    (and index (aref cl-cc/vm:*arith-dispatch-table* index))))

(defun codegen-inline-arith-dispatch-plan (op left-type right-type &key (target :vm))
  "Return a plist describing the FR-861 codegen plan for OP.

The plan records the flattened dispatch-table INDEX and selected ENTRY.  When a
specialized entry is present, VM backends can lower to VM-ARITH-DISPATCH; when
absent, callers should keep the existing generic arithmetic lowering."
  (let* ((index (codegen-inline-arith-dispatch-index op left-type right-type))
         (entry (and index (aref cl-cc/vm:*arith-dispatch-table* index)))
         (result-type (codegen-infer-numeric-contagion-type left-type right-type)))
    (list :op op
          :target target
          :left-type left-type
          :right-type right-type
          :result-type result-type
          :index index
          :entry entry
          :instruction (and *codegen-inline-arith-dispatch-enabled*
                            (eq target :vm)
                            entry
                            'cl-cc/vm:vm-arith-dispatch))))

(defun emit-inline-arith-dispatch (ctx op dst lhs-reg rhs-reg)
  "Emit a VM-ARITH-DISPATCH instruction for OP and return DST.
This helper is intentionally explicit so existing binop lowering is not changed
unless a caller opts into FR-861 dispatch lowering."
  (emit ctx (cl-cc/vm:make-vm-arith-dispatch
             :dst dst :lhs lhs-reg :rhs rhs-reg :op op))
  dst)

(export '(codegen-infer-numeric-contagion-type
          codegen-inline-arith-dispatch-index
          codegen-inline-arith-dispatch-entry
          codegen-inline-arith-dispatch-plan
          emit-inline-arith-dispatch
          *forward-reference-patch-table*
          record-forward-reference
          resolve-forward-references
          unresolved-forward-reference-error
          *unresolved-forward-refs*
          *codegen-inline-arith-dispatch-enabled*
          ;; FR-542: hot/cold code annotations
          declare-hot
          declare-cold
          cold-path))

;;; ── FR-542: Hot/Cold Code Annotations ─────────────────────────────────

(defvar *code-temperature-registry* (make-hash-table :test 'eq)
  "Maps function names to :hot or :cold temperature hints for code placement.")

(defun %declare-function-temperature (name temperature)
  "Register NAME with TEMPERATURE (:hot or :cold) for code placement.
Hot functions go to .text.hot section; cold functions to .text.cold."
  (setf (gethash name *code-temperature-registry*) temperature))

(defmacro declare-hot ()
  "FR-542: Declare the current function as hot-path.
Equivalent to GCC __attribute__((hot)). Hot functions are placed in
the .text.hot section for I-cache locality."
  `(push (cons :code-placement :hot) (compilation-result-code-placement-hints *compilation-result*)))

(defmacro declare-cold ()
  "FR-542: Declare the current function as cold-path.
Equivalent to GCC __attribute__((cold)). Cold functions are placed in
the .text.cold section, away from hot code to improve I-cache density."
  `(push (cons :code-placement :cold) (compilation-result-code-placement-hints *compilation-result*)))

(defmacro cold-path (&body body)
  "FR-542: Mark BODY as a cold execution path.
Used for error handlers and rarely-taken branches. The compiler
may outline this code to .text.cold and place it away from the
hot instruction stream."
  `(progn
     (push (cons :code-placement :cold) (compilation-result-code-placement-hints *compilation-result*))
     ,@body))
