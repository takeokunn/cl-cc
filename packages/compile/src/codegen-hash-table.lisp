(in-package :cl-cc/compile)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — hash-table handlers
;;;
;;; Small, cohesive handler cluster split from codegen-phase2.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; make-hash-table: extract :test keyword and convert #'fn → 'fn
(defun %hash-table-keyword-ast-p (ast keyword)
  (or (and (typep ast 'ast-var) (eq (ast-var-name ast) keyword))
      (and (typep ast 'ast-quote) (eq (ast-quote-value ast) keyword))))

(defun %hash-table-test-symbol-p (symbol)
  (or (eq symbol 'eq)
      (eq symbol 'eql)
      (eq symbol 'equal)
      (eq symbol 'equalp)))

(defun %hash-table-test-symbol-from-ast (ast)
  (if (typep ast 'ast-quote)
      (let ((name (ast-quote-value ast)))
        (if (and (symbolp name) (%hash-table-test-symbol-p name))
            name
            nil))
      (if (typep ast 'ast-var)
          (let ((name (ast-var-name ast)))
            (if (%hash-table-test-symbol-p name) name nil))
          (if (typep ast 'ast-function)
              (let ((name (ast-function-name ast)))
                (if (and (symbolp name) (%hash-table-test-symbol-p name))
                    name
                    nil))
              nil))))

(setf (gethash "MAKE-HASH-TABLE" *phase2-builtin-handlers*)
      (lambda (args result-reg ctx)
        (let* ((test-sym nil)
               (tail args)
               (test-reg (when test-sym
                           (let ((reg (make-register ctx)))
                             (emit ctx (make-vm-const :dst reg :value test-sym))
                             reg))))
          (tagbody
           scan-make-hash-table-options
             (when (or (null tail) (null (cdr tail)))
               (go scan-make-hash-table-options-done))
             (let ((k (car tail))
                   (v (cadr tail)))
               (when (%hash-table-keyword-ast-p k :test)
                 (setf test-sym (%hash-table-test-symbol-from-ast v))
                 (go scan-make-hash-table-options-done)))
             (setf tail (cddr tail))
             (go scan-make-hash-table-options)
           scan-make-hash-table-options-done)
          (when test-sym
            (setf test-reg (make-register ctx))
            (emit ctx (make-vm-const :dst test-reg :value test-sym)))
          (emit ctx (make-vm-make-hash-table :dst result-reg :test test-reg))
          result-reg)))

;; gethash: 2-arg (key table) or 3-arg (key table default)
(setf (gethash "GETHASH" *phase2-builtin-handlers*)
      (lambda (args result-reg ctx)
        (let ((key-reg     (compile-ast (first args)  ctx))
              (table-reg   (compile-ast (second args) ctx))
              (default-reg (when (third args) (compile-ast (third args) ctx))))
          (emit ctx (make-vm-gethash :dst result-reg :found-dst nil
                                     :key key-reg :table table-reg :default default-reg))
          result-reg)))

;; maphash: inline loop — keys snapshot → iterate with fn call
(setf (gethash "MAPHASH" *phase2-builtin-handlers*)
      (lambda (args result-reg ctx)
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
          result-reg)))
