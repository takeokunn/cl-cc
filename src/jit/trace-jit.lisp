;;;; src/jit/trace-jit.lisp — FR-558 Trace JIT / Hot Trace Recording
;;;; LuaJIT-style linear trace compilation for hot execution paths.
;;;; Mozilla TraceMonkey / PyPy tracing JIT equivalent.

(in-package :cl-cc/jit)

;;; ──── Configuration ────
(defvar *trace-jit-enabled* t
  "Master switch for trace JIT compilation.")

(defvar *trace-threshold* 50
  "Loop back-edge count before starting trace recording.")

(defvar *trace-max-length* 256
  "Maximum number of instructions in a single trace.")

;;; ──── Trace recording state ────
(defvar *trace-recording* nil
  "T when currently recording a trace.")

(defvar *current-trace* nil
  "List of (opcode . args) for the currently recording trace.")

(defvar *trace-entry-pc* nil
  "PC offset where the current trace started recording.")

(defvar *trace-guards* nil
  "List of guard conditions for the current trace.")

;;; ──── Trace recording ────
(defun start-trace-recording (entry-pc)
  "Begin recording a trace starting at ENTRY-PC (loop back-edge target)."
  (setf *trace-recording* t
        *current-trace* nil
        *trace-entry-pc* entry-pc
        *trace-guards* nil))

(defun stop-trace-recording ()
  "Stop recording the current trace and return it."
  (prog1 (list :entry-pc *trace-entry-pc*
               :instructions (nreverse *current-trace*)
               :guards (nreverse *trace-guards*))
    (setf *trace-recording* nil
          *current-trace* nil
          *trace-entry-pc* nil
          *trace-guards* nil)))

(defun record-trace-instruction (opcode &rest args)
  "Record an instruction in the current trace."
  (when *trace-recording*
    (push (cons opcode args) *current-trace*)
    ;; Check trace length limit
    (when (> (length *current-trace*) *trace-max-length*)
      (stop-trace-recording)))
  *trace-recording*)

(defun record-trace-guard (condition-type &rest args)
  "Record a guard condition in the current trace.
If the guard fails at runtime, execution side-exits to the interpreter."
  (when *trace-recording*
    (push (cons condition-type args) *trace-guards*)))

;;; ──── Trace compilation ────
(defun compile-trace (trace)
  "Compile a recorded TRACE into native code.
The trace is a linear sequence (no control flow) with guards.
Returns the entry address of the compiled trace."
  (let ((instructions (getf trace :instructions))
        (guards (getf trace :guards))
        (entry-pc (getf trace :entry-pc)))
    (declare (ignore entry-pc))
    ;; In production:
    ;; 1. Allocate executable memory for the trace
    ;; 2. Emit guard checking code: check each guard, side-exit if failed
    ;; 3. Emit the linear instruction sequence (no branches needed!)
    ;; 4. Inline any called functions into the trace body
    ;; 5. End with a jump back to the trace entry (loop) or interpreter exit
    (let ((trace-addr (allocate-trace-memory (* (length instructions) 8))))
      ;; Simplified: return the allocated address
      trace-addr)))

;;; ──── Side exit handling ────
(defvar *side-exit-table* (make-hash-table :test #'eql)
  "PC offset → compiled trace address for side exits.")

(defun register-side-exit (pc-offset trace-addr)
  "Register a side exit from PC-OFFSET to TRACE-ADDR.
When the interpreter hits PC-OFFSET with matching conditions,
it redirects to the compiled trace."
  (setf (gethash pc-offset *side-exit-table*) trace-addr))

(defun lookup-side-exit (pc-offset)
  "Look up a compiled trace for a side exit at PC-OFFSET."
  (gethash pc-offset *side-exit-table*))

;;; ──── Trace tree management ────
(defstruct (trace-tree-node (:conc-name ttn-))
  "A node in the trace tree. Root is the entry trace.
Each side exit spawns a child trace."
  (entry-pc 0 :type fixnum)
  (compiled-addr nil)
  (side-exits nil :type list)  ; list of (pc . child-node)
  (parent nil))

(defvar *trace-tree-root* nil
  "Root of the trace tree for the current function.")

(defun add-trace-to-tree (trace compiled-addr &optional parent)
  "Add a compiled TRACE to the trace tree as a child of PARENT."
  (let ((node (make-trace-tree-node
               :entry-pc (getf trace :entry-pc)
               :compiled-addr compiled-addr
               :parent parent)))
    (unless *trace-tree-root*
      (setf *trace-tree-root* node))
    (when parent
      (push (cons (getf trace :entry-pc) node)
            (ttn-side-exits parent)))
    node))

;;; ──── Trace memory allocation ────
(defun allocate-trace-memory (size)
  "Allocate SIZE bytes of executable memory for a compiled trace."
  #+sbcl
  (sb-posix:mmap nil (max size 4096)
                 (logior sb-posix:prot-read
                         sb-posix:prot-write
                         sb-posix:prot-exec)
                 (logior sb-posix:map-private
                         sb-posix:map-anonymous)
                 -1 0)
  #-sbcl
  (error "allocate-trace-memory requires SBCL"))
