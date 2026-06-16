;;;; src/jit/stack-map.lisp — FR-550 Stack Map Generation
;;;; Maps PC offsets to live GC roots in JIT-compiled stack frames.
;;;; LLVM StatepointGC / JVM GC map / V8 StackMaps equivalent.

(in-package :cl-cc/jit)

;;; ──── Stack map entry ────
(defstruct (stack-map-entry (:conc-name sme-))
  "A single stack map entry: for a given PC offset, which registers
and stack slots contain live heap references (GC roots)."
  (pc-offset 0 :type fixnum)        ; byte offset from function start
  (live-registers nil :type list)    ; list of register ids (e.g., RAX, RBX)
  (live-stack-slots nil :type list)) ; list of SP-relative slot offsets

;;; ──── Global stack map table ────
(defvar *stack-map-table* (make-hash-table :test #'eq)
  "Function label → sorted list of stack-map-entry structs.")

(defun register-stack-map (func-label entries)
  "Register STACK-MAP-ENTRIES for FUNC-LABEL in the global table."
  (setf (gethash func-label *stack-map-table*)
        (sort (copy-list entries) #'< :key #'sme-pc-offset)))

(defun lookup-stack-map (func-label pc-offset)
  "Find the stack map entry at PC-OFFSET in FUNC-LABEL's stack maps.
Returns the entry with the largest PC-OFFSET ≤ the given offset,
or NIL if no entry found before that offset."
  (let ((entries (gethash func-label *stack-map-table*)))
    (when entries
      (loop with best = nil
            for entry in entries
            while (<= (sme-pc-offset entry) pc-offset)
            do (setf best entry)
            finally (return best)))))

;;; ──── Live set tracking during codegen ────
(defvar *current-live-registers* nil
  "Set of registers containing live GC roots at the current codegen point.")

(defvar *current-live-stack-slots* nil
  "Set of SP-relative stack slots containing live GC roots.")

(defvar *pending-stack-maps* nil
  "Accumulated list of (pc . entry) pending for current function.")

(defun register-live-set (reg-id)
  "Mark REG-ID as containing a live GC root."
  (pushnew reg-id *current-live-registers*))

(defun register-kill-set (reg-id)
  "Mark REG-ID as no longer containing a live GC root."
  (setf *current-live-registers* (remove reg-id *current-live-registers*)))

(defun stack-slot-live (sp-offset)
  "Mark stack slot at SP-OFFSET as containing a live GC root."
  (pushnew sp-offset *current-live-stack-slots*))

(defun stack-slot-dead (sp-offset)
  "Mark stack slot at SP-OFFSET as dead."
  (setf *current-live-stack-slots* (remove sp-offset *current-live-stack-slots*)))

;;; ──── Emit stack map at current codegen point ────
(defun emit-stack-map (current-pc func-label)
  "Record a stack map entry at CURRENT-PC for FUNC-LABEL.
Captures the current live register/slot sets."
  (push (make-stack-map-entry
         :pc-offset current-pc
         :live-registers (copy-list *current-live-registers*)
         :live-stack-slots (copy-list *current-live-stack-slots*))
        *pending-stack-maps*))

(defun finalize-stack-maps (func-label)
  "Register all pending stack maps for FUNC-LABEL and reset state."
  (register-stack-map func-label *pending-stack-maps*)
  (setf *pending-stack-maps* nil
        *current-live-registers* nil
        *current-live-stack-slots* nil))

;;; ──── Stack frame walking ────
(defun walk-stack-frame (func-label pc-offset rsp rbp)
  "Walk a native stack frame at PC-OFFSET in FUNC-LABEL.
Returns a list of (location . value) for all live GC roots.
RSP and RBP are the current stack/frame pointers."
  (let* ((entry (lookup-stack-map func-label pc-offset))
         (roots nil))
    (when entry
      ;; Collect live register values
      (dolist (reg (sme-live-registers entry))
        (push (cons (list :register reg) (read-register-value reg)) roots))
      ;; Collect live stack slot values
      (dolist (slot (sme-live-stack-slots entry))
        (push (cons (list :stack-slot slot)
                    (read-stack-slot rsp slot))
              roots)))
    roots))

;;; ──── Low-level register/stack access ────
(defvar *register-values* (make-hash-table)
  "Saved register values for stack walking (set by signal handler).")

(defun read-register-value (reg-id)
  "Read the saved value of register REG-ID."
  (gethash reg-id *register-values* 0))

(defun read-stack-slot (rsp slot-offset)
  "Read a value from stack at RSP + SLOT-OFFSET.
Uses SBCL's SAP (System Area Pointer) for native memory access."
  (sb-sys:sap-ref-word (sb-sys:int-sap rsp) slot-offset)

;;; ──── Integration with codegen ────
(defmacro with-stack-map-tracking ((func-label) &body body)
  "Execute BODY with stack map tracking for FUNC-LABEL.
Automatically finalizes stack maps on exit."
  `(let ((*pending-stack-maps* nil)
         (*current-live-registers* nil)
         (*current-live-stack-slots* nil))
     (unwind-protect (progn ,@body)
       (finalize-stack-maps ,func-label))))
