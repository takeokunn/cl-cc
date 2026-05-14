(in-package :cl-cc/compile)

;;;; Shared helpers for value-producing codegen forms.

(defun %compile-forms/k (forms ctx continuation)
  "Compile FORMS left-to-right, then call CONTINUATION with result registers."
  (funcall continuation
           (loop for form in forms collect (compile-ast form ctx))))

(defmacro %with-compiled-registers ((registers forms ctx) &body body)
  "Bind REGISTERS to compiled FORMS using the CPS helper %COMPILE-FORMS/K."
  `(%compile-forms/k ,forms ,ctx
     (lambda (,registers)
       ,@body)))

(defun %compile-body/k (forms ctx continuation)
  "Compile FORMS as a body, then call CONTINUATION with the last register."
  (let ((last nil))
    (dolist (form forms)
      (setf last (compile-ast form ctx)))
    (funcall continuation last)))

(defun %emit-nil-register (ctx)
  "Emit NIL into a fresh register and return it."
  (let ((nil-reg (make-register ctx)))
    (emit ctx (make-vm-const :dst nil-reg :value nil))
    nil-reg))

(defun %registers-for-vars (vars source-regs ctx)
  "Return one register per VAR, padding missing SOURCE-REGS with NIL registers."
  (loop for _ in vars
        for src = source-regs then (cdr src)
        collect (if src (car src) (%emit-nil-register ctx))))

(defun %allocate-registers-for-vars (vars ctx)
  "Allocate one fresh destination register for each VAR."
  (loop for _ in vars collect (make-register ctx)))

(defun %install-register-bindings (vars regs ctx)
  "Extend CTX's lexical environment with VARS bound to REGS."
  (setf (ctx-env ctx)
        (nconc (mapcar #'cons vars regs) (ctx-env ctx))))

(defun %compile-mvb-value-registers (vars values-form ctx)
  "Compile a multiple-value source and return destination registers for VARS."
  (if (typep values-form 'ast-values)
      (%with-compiled-registers (source-regs (ast-values-forms values-form) ctx)
        (%registers-for-vars vars source-regs ctx))
      (progn
        (compile-ast values-form ctx)
        (let ((var-regs (%allocate-registers-for-vars vars ctx)))
          (emit ctx (make-vm-mv-bind :dst-regs var-regs))
          var-regs))))

(defun %resolve-apply-function-register (func-node ctx)
  "Compile or resolve the function designator used by APPLY."
  (cond
    ((typep func-node 'ast-function)
     (compile-ast func-node ctx))
    ((and (typep func-node 'ast-quote)
          (symbolp (ast-quote-value func-node)))
     (%resolve-func-sym-reg (ast-quote-value func-node) ctx))
    ((symbolp func-node)
     (%resolve-func-sym-reg func-node ctx))
    (t
     (compile-ast func-node ctx))))

(defun %apply-argument-plan (args)
  "Return a data plist describing APPLY's leading args and final spread arg."
  (labels ((walk (tail leading)
             (if (cdr tail)
                 (walk (cdr tail) (cons (car tail) leading))
                 (list :leading (nreverse leading)
                       :spread  (car tail)))))
    (if args
        (walk args nil)
        (list :leading nil :spread nil))))

(defun %proper-list-p (value)
  "Return T when VALUE is a finite proper list.
Reject dotted and circular lists so APPLY literal lowering stays conservative."
  (labels ((walk (slow fast)
             (cond
               ((null fast) t)
               ((atom fast) nil)
               ((null (cdr fast)) t)
               ((atom (cdr fast)) nil)
               ((eq slow fast) nil)
               (t (walk (cdr slow) (cddr fast))))))
    (or (null value)
        (and (consp value)
             (walk value (cdr value))))))

(defun %literal-apply-spread-values (spread-arg)
  "Return (values T VALUES) when SPREAD-ARG is a quoted finite proper list."
  (if (and spread-arg
            (typep spread-arg 'ast-quote)
            (%proper-list-p (ast-quote-value spread-arg)))
       (values t (ast-quote-value spread-arg))
       (values nil nil)))

(defun %quoted-value-forms (values)
  "Convert literal VALUES into AST quote forms."
  (mapcar (lambda (value) (make-ast-quote :value value)) values))

(defun %all-values-args-p (args)
  "Return T when every multiple-value-call arg is an AST-VALUES node."
  (every (lambda (arg) (typep arg 'ast-values)) args))

(defun %flat-values-forms (args)
  "Flatten AST-VALUES argument forms left-to-right."
  (loop for arg in args append (ast-values-forms arg)))

(defun %compile-values-list-register (arg ctx)
  "Compile ARG and capture all produced values into a list register."
  (emit ctx (make-vm-clear-values))
  (let ((primary-reg (compile-ast arg ctx))
        (list-reg (make-register ctx)))
    (emit ctx (make-vm-ensure-values :src primary-reg))
    (emit ctx (make-vm-values-to-list :dst list-reg))
    list-reg))

(defun %compile-values-list-registers (args ctx)
  "Compile ARGS into registers that each contain a list of produced values."
  (loop for arg in args collect (%compile-values-list-register arg ctx)))

(defun %append-list-registers (regs ctx)
  "Append list-valued REGS left-to-right and return the combined register."
  (let ((combined-reg (car regs)))
    (dolist (reg (cdr regs) combined-reg)
      (let ((next-reg (make-register ctx)))
        (emit ctx (make-vm-append :dst next-reg :src1 combined-reg :src2 reg))
        (setf combined-reg next-reg)))))
