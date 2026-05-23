;;;; packages/compile/src/inline-asm.lisp — FR-704 Inline Assembly
;;;; GCC-style extended inline assembly for cl-cc.
;;;; asm volatile / clobber / input/output operand support.

(in-package :cl-cc/compile)

(defvar *inline-asm-enabled* t)

(defstruct (asm-operand (:conc-name asm-))
  "An inline assembly operand."
  (constraint "" :type string)    ; GCC-style constraint (r, m, i, etc.)
  (expression nil))                ; Lisp expression providing the value

(defun parse-asm-constraint (constraint-str)
  "Parse a GCC-style constraint string.
'r' = register, 'm' = memory, 'i' = immediate, 'g' = general."
  (let ((modifiers nil)
        (base nil))
    (loop for ch across constraint-str
          do (case ch
               (#\= (push :write-only modifiers))
               (#\+ (push :read-write modifiers))
               (#\& (push :early-clobber modifiers))
               (t (setf base ch))))
    (values base modifiers)))

;;; ──── Inline assembly macro ────
(defmacro asm (template &key (inputs nil) (outputs nil) (clobbers nil) (volatile t))
  "Emit inline assembly TEMPLATE.
INPUTS: ((constraint expr) ...)
OUTPUTS: ((constraint var) ...)
CLOBBERS: list of clobbered register names
VOLATILE: T = prevent reordering/elimination (default T)"
  `(emit-inline-asm ,template
                    :inputs ',inputs
                    :outputs ',outputs
                    :clobbers ',clobbers
                    :volatile ,volatile))

(defun emit-inline-asm (template &key inputs outputs clobbers volatile)
  "Emit the inline assembly TEMPLATE into the code stream."
  (declare (ignore template inputs outputs clobbers volatile))
  ;; In production: parse template, allocate registers for operands,
  ;; emit actual assembly bytes, handle clobbers for register allocator.
  (values))

;;; ──── Common asm snippets ────
(defmacro asm-rdtsc ()
  "Read the timestamp counter (RDTSC instruction)."
  `(asm "rdtsc" :outputs (("=a" lo) ("=d" hi)) :clobbers nil))

(defmacro asm-cpuid (leaf)
  "Execute CPUID instruction with LEAF in EAX."
  `(asm "cpuid"
     :outputs (("=a" eax) ("=b" ebx) ("=c" ecx) ("=d" edx))
     :inputs (("a" ,leaf))))

(defmacro asm-pause ()
  "PAUSE instruction (spin-wait hint)."
  `(asm "pause"))
