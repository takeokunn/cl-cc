(in-package :cl-cc/codegen)

(defstruct calling-convention
  "Backend calling-convention policy used by native code generation."
  (name :external :type keyword)
  (arg-regs nil :type list)
  (callee-saved nil :type list)
  (omit-frame-pointer-p nil :type boolean))

(defparameter *external-calling-convention*
  (make-calling-convention :name :external)
  "Use the platform ABI unchanged for exported or globally registered functions.")

(defparameter *internal-calling-convention*
  (make-calling-convention
   :name :internal
   :arg-regs '((:x86-64 . (:rdi :rsi :rdx :rcx :r8 :r9 :r10))
               (:aarch64 . (:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
                            :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15)))
   :callee-saved '((:x86-64 . (:rbx))
                   (:aarch64 . (:x19 :x20)))
   :omit-frame-pointer-p t)
  "Fast internal convention for non-exported functions that do not escape ABI boundaries.")

(defparameter *current-calling-convention* *external-calling-convention*
  "Calling convention selected for the native function currently being emitted.")

(defun calling-convention-for-name (name)
  "Return the configured calling convention object for NAME."
  (case name
    (:internal *internal-calling-convention*)
    (otherwise *external-calling-convention*)))

(defun calling-convention-target-value (convention target accessor)
  "Return target-specific CONVENTION data selected by ACCESSOR."
  (let ((entry (assoc (target-name target) (funcall accessor convention) :test #'eq)))
    (cdr entry)))

(defun calling-convention-internal-p (convention)
  "Return T when CONVENTION is the internal fast convention."
  (eq (calling-convention-name convention) :internal))

(defun apply-calling-convention-to-target (target convention)
  "Return TARGET adjusted for CONVENTION argument and preservation policy."
  (if (not (calling-convention-internal-p convention))
      target
      (make-target-desc
       :name (target-name target)
       :word-size (target-word-size target)
       :endianness (target-endianness target)
       :gpr-count (target-gpr-count target)
       :gpr-names (target-gpr-names target)
       :arg-regs (or (calling-convention-target-value
                      convention target #'calling-convention-arg-regs)
                     (target-arg-regs target))
       :ret-reg (target-ret-reg target)
       :fp-arg-regs (target-fp-arg-regs target)
       :fp-ret-reg (target-fp-ret-reg target)
       :callee-saved (or (calling-convention-target-value
                          convention target #'calling-convention-callee-saved)
                         (target-callee-saved target))
       :scratch-regs (target-scratch-regs target)
       :stack-alignment (target-stack-alignment target)
       :legal-ops (target-legal-ops target)
       :features (adjoin :internal-calling-convention
                         (target-features target)
                         :test #'eq))))

(defun vm-program-calling-convention-object (program)
  "Return the calling-convention object selected by PROGRAM metadata."
  (calling-convention-for-name (vm-program-calling-convention program)))
