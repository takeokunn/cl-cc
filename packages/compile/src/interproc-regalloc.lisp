;;;; packages/compile/src/interproc-regalloc.lisp — FR-571 Interprocedural RA
;;;; Call-graph-based register allocation: shared register sets for leaf functions.
;;;; GCC IPRA (-fipa-ra) / calling convention merging equivalent.

(in-package :cl-cc/compile)

;;; ──── Configuration ────
(defvar *ipra-enabled* nil
  "When T, interprocedural register allocation is enabled.")

(defvar *ipra-functions* (make-hash-table :test #'eq)
  "Function name → ipra-info struct for IPRA-eligible functions.")

;;; ──── IPRA Info ────
(defstruct (ipra-info (:conc-name ipra-))
  "Interprocedural register allocation info for a function."
  (name nil :type symbol)
  (is-leaf-p nil)                  ; T if function calls no other functions
  (callers nil :type list)         ; list of direct caller function names
  (callees nil :type list)         ; list of functions this function calls
  (clobbered-regs nil :type list)  ; registers this function clobbers
  (callee-saved-regs nil :type list) ; callee-saved regs actually needed
  (custom-reg-mask nil))           ; merged reg mask for caller+callee

;;; ──── Call Graph Analysis ────
(defun build-call-graph (functions)
  "Build a call graph from FUNCTIONS (list of (name . body) pairs).
Populates *ipra-functions* with IPRA info for each function."
  (clrhash *ipra-functions*)
  ;; First pass: create entries
  (dolist (fn functions)
    (let ((name (car fn)))
      (setf (gethash name *ipra-functions*)
            (make-ipra-info :name name))))
  ;; Second pass: analyze call relationships
  (dolist (fn functions)
    (destructuring-bind (name . body) fn
      (let* ((info (gethash name *ipra-functions*))
             (callees (find-function-calls body))
             (is-leaf (null callees)))
        (setf (ipra-callees info) callees
              (ipra-is-leaf-p info) is-leaf)
        ;; Register this as caller for each callee
        (dolist (callee callees)
          (let ((callee-info (gethash callee *ipra-functions*)))
            (when callee-info
              (pushnew name (ipra-callers callee-info) :test #'eq))))))))

;;; ──── Register merging ────
(defun compute-custom-calling-convention (func-name)
  "Compute a custom calling convention for FUNC-NAME based on call graph.
Merges caller-clobbered + callee-saved into a unified register set.
Reduces save/restore overhead for leaf and internal-only functions."
  (let ((info (gethash func-name *ipra-functions*)))
    (unless info
      (return-from compute-custom-calling-convention nil))
    ;; Leaf functions: all caller-saved regs are available (no callees to preserve)
    ;; No need to save callee-saved regs that aren't used
    (let ((clobbered (ipra-clobbered info))
          (needed-callee-saved
           ;; Compute which callee-saved regs this function actually uses
           (compute-needed-callee-saved func-name)))
      (setf (ipra-callee-saved-regs info) needed-callee-saved)
      ;; The custom convention tells callers:
      ;; "These regs are live across calls to me" (caller doesn't need to save others)
      (list :function func-name
            :leaf-p (ipra-is-leaf-p info)
            :clobbered (or clobbered
                           (default-clobbered-regs))
            :preserved (or needed-callee-saved
                           (default-callee-saved-regs))))))

;;; ──── Helpers ────
(defun find-function-calls (body)
  "Extract list of function names called in BODY."
  (let ((calls nil))
    (labels ((walk (form)
               (typecase form
                 (cons
                  (when (symbolp (car form))
                    (pushnew (car form) calls :test #'eq))
                  (dolist (child (cdr form))
                    (walk child))))))
      (walk body))
    calls))

(defun compute-needed-callee-saved (func-name)
  "Compute which callee-saved registers FUNC-NAME actually needs.
Returns list of register names that must be preserved."
  (declare (ignore func-name))
  ;; In production: analyze function body for register usage
  ;; Simplified: return default set
  (default-callee-saved-regs))

(defun default-clobbered-regs ()
  "Default caller-saved (clobbered) registers for x86-64 ABI."
  '(rax rcx rdx rsi rdi r8 r9 r10 r11))

(defun default-callee-saved-regs ()
  "Default callee-saved registers for x86-64 ABI."
  '(rbx rbp r12 r13 r14 r15))

;;; ──── Integration ────
(defmacro with-ipra ((&key (enabled t)) &body body)
  "Execute BODY with IPRA enabled (or disabled)."
  `(let ((*ipra-enabled* ,enabled))
     ,@body))

(defun ipra-declaration-p (decl)
  "Check if a declaration form is an IPRA declaration.
Usage: (declare (cl-cc:ipra))"
  (and (consp decl) (eq (car decl) 'cl-cc:ipra)))
