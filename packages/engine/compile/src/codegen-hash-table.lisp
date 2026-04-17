(in-package :cl-cc/compile)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — hash-table handlers
;;;
;;; Small, cohesive handler cluster split from codegen-phase2.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; make-hash-table: extract :test keyword and convert #'fn → 'fn
(define-phase2-handler "MAKE-HASH-TABLE" (args result-reg ctx)
  (let ((test-reg nil))
    (when (>= (length args) 2)
      (let ((kw-arg (first args)))
        (when (and (typep kw-arg 'ast-var) (eq (ast-var-name kw-arg) :test))
          (let* ((test-arg (second args))
                 (test-sym (cond
                              ((and (typep test-arg 'ast-quote)
                                    (symbolp (ast-quote-value test-arg)))
                               (ast-quote-value test-arg))
                              ((and (typep test-arg 'ast-var)
                                    (member (ast-var-name test-arg) '(eq eql equal equalp)))
                               (ast-var-name test-arg))
                              ((and (typep test-arg 'ast-function)
                                    (symbolp (ast-function-name test-arg))
                                    (member (ast-function-name test-arg) '(eq eql equal equalp)))
                               (ast-function-name test-arg))
                              (t nil))))
            (when test-sym
              (setf test-reg (make-register ctx))
              (emit ctx (make-vm-const :dst test-reg :value test-sym)))))))
    (emit ctx (make-vm-make-hash-table :dst result-reg :test test-reg))
    result-reg))

;; gethash: 2-arg (key table) or 3-arg (key table default)
(define-phase2-handler "GETHASH" (args result-reg ctx)
  (let ((key-reg     (compile-ast (first args)  ctx))
        (table-reg   (compile-ast (second args) ctx))
        (default-reg (when (third args) (compile-ast (third args) ctx))))
    (emit ctx (make-vm-gethash :dst result-reg :found-dst nil
                               :key key-reg :table table-reg :default default-reg))
    result-reg))

;; maphash: inline loop — keys snapshot → iterate with fn call
(define-phase2-handler "MAPHASH" (args result-reg ctx)
  (let* ((fn-reg     (compile-ast (first args)  ctx))
         (table-reg  (compile-ast (second args) ctx))
         (keys-reg   (make-register ctx))
         (loop-start (make-label ctx "MAPHASH_START"))
         (loop-end   (make-label ctx "MAPHASH_END"))
         (key-reg    (make-register ctx))
         (val-reg    (make-register ctx)))
    (emit ctx (make-vm-hash-table-keys :dst keys-reg :table table-reg))
    (emit ctx (make-vm-label :name loop-start))
    (emit ctx (make-vm-jump-zero :reg keys-reg :label loop-end))
    (emit ctx (make-vm-car :dst key-reg :src keys-reg))
    (emit ctx (make-vm-gethash :dst val-reg :key key-reg :table table-reg))
    (let ((call-dst (make-register ctx)))
      (emit ctx (make-vm-call :dst call-dst :func fn-reg :args (list key-reg val-reg))))
    (emit ctx (make-vm-cdr :dst keys-reg :src keys-reg))
    (emit ctx (make-vm-jump :label loop-start))
    (emit ctx (make-vm-label :name loop-end))
    (emit ctx (make-vm-const :dst result-reg :value nil))
    result-reg))
