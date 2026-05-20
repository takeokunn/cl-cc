(in-package :cl-cc/compile)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — hash-table handlers
;;;
;;; Small, cohesive handler cluster split from codegen-phase2.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; make-hash-table: extract supported keywords and convert #'fn → 'fn
(defun %hash-table-static-test-for-table-ast (ast ctx)
  "Return a statically known hash-table test for AST in CTX, if available."
  (or (%hash-table-static-test-from-make-hash-table-ast ast)
      (and (typep ast 'ast-var)
           (cdr (%assoc-eq (ast-var-name ast)
                           (ctx-hash-table-test-bindings ctx))))))

(defun %hash-table-gethash-constructor-for-test (test-sym)
  "Return the specialized GETHASH constructor for TEST-SYM, or NIL."
  (case test-sym
    (eq #'make-vm-gethash-eq)
    (eql #'make-vm-gethash-eql)
    (equal #'make-vm-gethash-equal)
    (otherwise nil)))

(setf (gethash "MAKE-HASH-TABLE" *phase2-builtin-handlers*)
      (lambda (args result-reg ctx)
        (let* ((test-ast (%phase2-find-keyword-arg args :test))
               (test-sym (and test-ast
                              (%hash-table-test-symbol-from-ast test-ast)))
               (test-reg (cond
                           (test-sym
                            (let ((reg (make-register ctx)))
                              (emit ctx (make-vm-const :dst reg :value test-sym))
                              reg))
                           (test-ast
                            (compile-ast test-ast ctx))))
               (size-ast (%phase2-find-keyword-arg args :size))
               (rehash-size-ast (%phase2-find-keyword-arg args :rehash-size))
               (rehash-threshold-ast (%phase2-find-keyword-arg args :rehash-threshold))
               (size-reg (when size-ast (compile-ast size-ast ctx)))
               (rehash-size-reg (when rehash-size-ast (compile-ast rehash-size-ast ctx)))
               (rehash-threshold-reg (when rehash-threshold-ast
                                       (compile-ast rehash-threshold-ast ctx))))
          (emit ctx (make-vm-make-hash-table :dst result-reg
                                             :test test-reg
                                             :size size-reg
                                             :rehash-size rehash-size-reg
                                             :rehash-threshold rehash-threshold-reg))
          result-reg)))

;; gethash: 2-arg (key table) or 3-arg (key table default)
(setf (gethash "GETHASH" *phase2-builtin-handlers*)
      (lambda (args result-reg ctx)
        (let* ((table-ast (second args))
               (test-sym (%hash-table-static-test-for-table-ast table-ast ctx))
               (ctor (or (%hash-table-gethash-constructor-for-test test-sym)
                         #'make-vm-gethash))
               (key-reg (compile-ast (first args) ctx))
               (table-reg (compile-ast table-ast ctx))
               (default-reg (when (third args) (compile-ast (third args) ctx))))
          (emit ctx (funcall ctor :dst result-reg :found-dst nil
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
