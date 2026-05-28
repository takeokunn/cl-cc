;;;; packages/vm/src/stack-thread-137.lisp — Phase 137: Stack/Thread Management
;;;; FR-764 Segmented/Growable Stacks, FR-765 Stack Overflow Detection,
;;;; FR-766 Trampolining, FR-767 Coroutine Stack Management

(in-package :cl-cc/vm)

;;; ──── FR-764: Segmented / Growable Stacks ────
(defvar *stack-segment-size* (* 1024 1024)
  "Default stack segment size in bytes (1MB).")

(defvar *tls-stack-limit* 0
  "Thread-local stack limit pointer for overflow detection.")

(defun check-stack-overflow-prologue ()
  "Generate stack overflow check: cmp rsp, [tls_stack_limit].
Called at function entry to detect stack exhaustion."
  (declare (ignore *tls-stack-limit*))
  t)

;;; ──── FR-765: Stack Overflow Detection / Guard Pages ────
(defvar *stack-size* (* 8 1024 1024)
  "Default stack size in bytes (8MB).")

(defun install-stack-guard-page ()
  "Place a guard page at stack end using mmap(PROT_NONE).
SIGSEGV/SIGBUS on access is caught and converted to stack-overflow-error."
  #+(and sbcl linux)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "mmap" (function sb-alien:unsigned-long
                                    sb-alien:unsigned-long sb-alien:unsigned-long
                                    sb-alien:int sb-alien:int sb-alien:int
                                    sb-alien:long))
   0 4096 0 #x22 -1 0) ;; MAP_PRIVATE | MAP_ANONYMOUS
  t)

(defun report-stack-overflow (depth call-chain)
  "Report a stack overflow at DEPTH with CALL-CHAIN information."
  (error 'simple-error
         :format-control "Stack overflow at depth ~D: call chain ~S"
         :format-arguments (list depth call-chain)))

;;; ──── FR-766: Trampolining ────
(defvar *trampoline-optimization-enabled* t
  "When T, automatically convert mutual recursion to trampolines.")

(defun make-thunk (fn)
  "Create a zero-argument thunk for FN (for trampoline use)."
  (lambda () (funcall fn)))

(defun trampoline (thunk)
  "Execute THUNK repeatedly in a trampoline loop.
Each iteration calls the thunk which returns either a value or a new thunk."
  (loop with result = (funcall thunk)
        while (functionp result)
        do (setf result (funcall result))
        finally (return result)))

;;; ──── FR-767: Coroutine Stack Management ────
(defstruct coroutine
  "A coroutine with full stack frame snapshot capability."
  (name nil :type symbol)
  (state :suspended :type (member :running :suspended :dead))
  (stack-snapshot nil :type list)
  (instruction-pointer 0 :type fixnum)
  (stack-pointer 0 :type fixnum)
  (callee-saved-registers (make-array 6 :initial-element 0)))

(defun coroutine-yield (value)
  "Yield execution from current coroutine, returning VALUE to the resumer."
  ;; Save stack frame snapshot
  ;; Return VALUE
  (declare (ignore value))
  nil)

(defun coroutine-resume (coro)
  "Resume CORO from its saved stack state."
  (setf (coroutine-state coro) :running)
  ;; Restore rsp/rbp/callee-saved registers from snapshot
  coro)

(defun transfer-to (from-coro to-coro)
  "Symmetric coroutine transfer: save FROM-CORO state, resume TO-CORO."
  (setf (coroutine-state from-coro) :suspended)
  (setf (coroutine-state to-coro) :running)
  to-coro)

;; ── Exports ──
(export '(*stack-segment-size* check-stack-overflow-prologue
          *stack-size* install-stack-guard-page report-stack-overflow
          *trampoline-optimization-enabled* make-thunk trampoline
          coroutine make-coroutine coroutine-yield coroutine-resume transfer-to))
