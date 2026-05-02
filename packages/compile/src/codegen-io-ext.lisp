(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Phase 2: Array / String / Format / File Handlers
;;;
;;; Extracted from codegen-io.lisp to keep array-access, string-formatting,
;;; and file-open/close handlers separate from stream read/write primitives.
;;; Depends on codegen-io.lisp (%emit-format-result-to-dest is defined here).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Multi-dimensional aref: (aref arr i j ...) — Phase 1 handles (aref arr i) exactly
(define-phase2-handler "AREF" (args result-reg ctx)
  (when (>= (length args) 3)  ; array + 2+ indices
    (let ((arr-reg  (compile-ast (first args) ctx))
          (idx-regs (mapcar (lambda (a) (compile-ast a ctx)) (rest args))))
      (emit ctx (make-vm-aref-multi :dst result-reg
                                    :array-reg arr-reg
                                    :index-regs idx-regs))
      result-reg)))

;; FR-649: write-to-string — accept keyword args, use only the object
(define-phase2-handler "WRITE-TO-STRING" (args result-reg ctx)
  (when args
    (let ((obj-reg (compile-ast (first args) ctx)))
      ;; Evaluate keyword values for side effects, but ignore them.
      (loop for (key val) on (cdr args) by #'cddr
            when (and (typep key 'ast-var)
                      (keywordp (ast-var-name key)))
              do (compile-ast val ctx))
      (emit ctx (make-vm-write-to-string-inst :dst result-reg :src obj-reg))
      result-reg)))

;; write-string: optional stream arg
(define-phase2-handler "WRITE-STRING" (args result-reg ctx)
  (let ((str-reg (compile-ast (first args) ctx)))
    (if (>= (length args) 2)
        (let ((stream-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-stream-write-string-inst :stream-reg stream-reg :src str-reg))
          (emit ctx (make-vm-move :dst result-reg :src str-reg)))
        (emit ctx (make-vm-princ :src str-reg)))
    result-reg))

;; format: destination dispatch — nil (string), t (stdout), stream
;;
;; %emit-format-result-to-dest handles the output routing.
;; GET-STR-REG is a continuation: (ctx) → str-reg, called to produce the string.
;; This eliminates the repeated nil/t/stream cond in both the const and dynamic paths.
(defun %emit-format-result-to-dest (dest-sym dest-arg get-str-reg result-reg ctx)
  "Route a formatted string to its destination.
DEST-SYM: nil (return string), t (print to stdout), or :stream (write to stream).
GET-STR-REG: a (ctx) → str-reg continuation producing the string register.
Emits instructions and returns RESULT-REG."
  (cond
    ((null dest-sym)
     ;; (format nil ...) — return the string directly
     (let ((str-reg (funcall get-str-reg ctx)))
       (emit ctx (make-vm-move :dst result-reg :src str-reg))))
    ((eq dest-sym t)
     ;; (format t ...) — print to stdout, return nil
     (let ((str-reg (funcall get-str-reg ctx)))
       (emit ctx (make-vm-princ :src str-reg))
       (emit ctx (make-vm-const :dst result-reg :value nil))))
    (t
     ;; (format stream ...) — write to named stream, return nil
     (let ((str-reg    (funcall get-str-reg ctx))
           (stream-reg (compile-ast dest-arg ctx)))
       (emit ctx (make-vm-stream-write-string-inst :stream-reg stream-reg :src str-reg))
       (emit ctx (make-vm-const :dst result-reg :value nil)))))
  result-reg)

(define-phase2-handler "FORMAT" (args result-reg ctx)
  (when (>= (length args) 2)
    (let* ((dest-arg        (first args))
           (fmt-arg         (second args))
           (format-arg-regs (mapcar (lambda (a) (compile-ast a ctx)) (cddr args)))
           (dest-sym        (cond ((and (typep dest-arg 'ast-var)
                                        (member (ast-var-name dest-arg) '(nil t)))
                                   (ast-var-name dest-arg))
                                  ((typep dest-arg 'ast-quote)
                                   (ast-quote-value dest-arg))
                                  (t :stream))))
      (if (and (null format-arg-regs)
               (typep fmt-arg 'ast-quote)
               (stringp (ast-quote-value fmt-arg)))
          ;; Constant format string, no args — emit a string constant
          (let ((fmt-value (ast-quote-value fmt-arg)))
            (%emit-format-result-to-dest
             dest-sym dest-arg
             (lambda (ctx) (let ((r (make-register ctx)))
                             (emit ctx (make-vm-const :dst r :value fmt-value))
                             r))
             result-reg ctx))
          ;; Dynamic format — compile fmt + args, call vm-format-inst
          (%emit-format-result-to-dest
           dest-sym dest-arg
           (lambda (ctx) (let ((fmt-reg (compile-ast fmt-arg ctx))
                                (str-reg (make-register ctx)))
                           (emit ctx (make-vm-format-inst :dst str-reg :fmt fmt-reg
                                                          :arg-regs format-arg-regs))
                           str-reg))
           result-reg ctx))
      result-reg)))

;; open: parse :direction keyword arg
(define-phase2-handler "OPEN" (args result-reg ctx)
  ;; Parse :direction, :if-exists, :if-does-not-exist from keyword args.
  ;; Other keyword args (:element-type, :external-format) are compiled but ignored at runtime.
  (flet ((keyword-ast-value (ast)
           (cond
             ((and (typep ast 'ast-var) (keywordp (ast-var-name ast)))
              (ast-var-name ast))
             ((and (typep ast 'ast-quote) (keywordp (ast-quote-value ast)))
              (ast-quote-value ast))
             (t nil))))
    (let* ((path-reg  (compile-ast (first args) ctx))
           (direction :input)
           (if-exists :supersede)
           (if-not-exists nil))
      (loop for (key val) on (cdr args) by #'cddr
            for k = (keyword-ast-value key)
            when k
              do (let ((v (keyword-ast-value val)))
                   (cond
                     ((eq k :direction)
                      (setf direction (or v :input)))
                     ((eq k :if-exists)
                      (setf if-exists (or v :supersede)))
                     ((eq k :if-does-not-exist)
                      (setf if-not-exists v))
                     ;; Compile but discard :element-type, :external-format args
                     (t (compile-ast val ctx)))))
    ;; if-not-exists default: :create for output, :error for input (computed in execute-instruction)
      (emit ctx (make-vm-open-file :dst result-reg :path path-reg :direction direction
                                   :if-exists if-exists :if-not-exists if-not-exists))
      result-reg)))

;; close: (close stream &key abort) — :abort accepted but ignored (FR-589)
(define-phase2-handler "CLOSE" (args result-reg ctx)
  (when args
    (let ((handle-reg (compile-ast (first args) ctx)))
      ;; Compile and discard :abort keyword arg value
      (loop for (key val) on (cdr args) by #'cddr
            when (and (typep key 'ast-var) (keywordp (ast-var-name key)))
              do (compile-ast val ctx))
      (emit ctx (make-vm-close-file :handle handle-reg))
      ;; close returns t per ANSI CL
      (emit ctx (make-vm-const :dst result-reg :value t))
      result-reg)))

;; concatenate: batch-fold constant strings; otherwise lower to vm-concatenate
(define-phase2-handler "CONCATENATE" (args result-reg ctx)
  (when (and (>= (length args) 3)
             (typep (first args) 'ast-quote)
             (string= (symbol-name (ast-quote-value (first args))) "STRING"))
    (let ((string-args (rest args)))
      (if (every (lambda (arg)
                   (and (typep arg 'ast-quote)
                        (stringp (ast-quote-value arg))))
                 string-args)
          (progn
            (emit ctx (make-vm-const :dst result-reg
                                     :value (apply #'concatenate 'string
                                                   (mapcar #'ast-quote-value string-args))))
            result-reg)
          (let ((current-reg (compile-ast (second args) ctx))
                (tail (cddr args)))
            (loop while tail
                  for next-arg = (first tail)
                  for more = (rest tail)
                  do (let* ((next-reg (compile-ast next-arg ctx))
                            (dst-reg (if more (make-register ctx) result-reg)))
                       (emit ctx (make-vm-concatenate :dst dst-reg :str1 current-reg :str2 next-reg))
                       (setf current-reg dst-reg
                             tail more)))
            result-reg)))))
