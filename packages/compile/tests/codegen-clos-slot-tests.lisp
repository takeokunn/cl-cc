;;;; tests/unit/compile/codegen-clos-slot-tests.lisp — Codegen CLOS Slot & Phase2 Tests

(in-package :cl-cc/test)
(in-suite cl-cc-codegen-unit-serial-suite)

;;; ─── compile-ast: ast-slot-value ─────────────────────────────────────────────

(deftest codegen-slot-value
  "slot-value emits vm-slot-read with correct slot name and returns a register."
  (let* ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile::ctx-env ctx) (list (cons 'obj :R42)))
    (let* ((reg  (compile-ast (cl-cc/ast:make-ast-slot-value
                                :object (cl-cc/ast:make-ast-var :name 'obj)
                                :slot 'radius)
                               ctx))
           (inst (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read)))
      (assert-true inst)
      (assert-eq 'radius (cl-cc/vm::vm-slot-name-sym inst))
      (assert-true (keywordp reg)))))

(deftest codegen-noescape-make-instance-slot-value-bypasses-heap-object
  "A non-escaping local make-instance binding can serve slot-value from split slot registers."
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (cl-cc/ast:make-ast-let
                 :bindings (list (cons 'obj (cl-cc/ast:make-ast-make-instance
                                            :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                                            :initargs (list (cons :name (cl-cc/ast:make-ast-quote :value 'rex))))))
                 :body (list (cl-cc/ast:make-ast-slot-value
                              :object (cl-cc/ast:make-ast-var :name 'obj)
                              :slot 'name)))
                ctx)))
      (assert-true (keywordp reg))
      (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj))
      (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read))
      (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-move)))))

(deftest codegen-escaped-make-instance-slot-value-falls-back
  "Captured make-instance bindings keep the normal heap-backed slot path."
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (cl-cc/ast:make-ast-let
                 :bindings (list (cons 'obj (cl-cc/ast:make-ast-make-instance
                                            :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                                            :initargs (list (cons :name (cl-cc/ast:make-ast-quote :value 'rex))))))
                 :body (list (cl-cc/ast:make-ast-lambda :params '() :body (list (cl-cc/ast:make-ast-var :name 'obj)))
                             (cl-cc/ast:make-ast-slot-value
                              :object (cl-cc/ast:make-ast-var :name 'obj)
                              :slot 'name)))
                ctx)))
       (assert-true (keywordp reg))
       (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj))
       (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read)))))

(deftest codegen-branch-local-make-instance-sinks-initarg-evaluation
  "A branch-local non-escaping make-instance delays initarg evaluation into the used branch."
  (let* ((ctx (make-codegen-ctx))
         (ast nil)
         (reg nil)
         (insts nil)
         (jump-pos nil)
         (cons-pos nil))
    (setf (cl-cc/compile::ctx-env ctx) (list (cons 'flag :R10)))
    (setf ast
          (cl-cc/ast:make-ast-let
           :bindings
           (list (cons 'obj
                       (cl-cc/ast:make-ast-make-instance
                        :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                        :initargs
                        (list (cons :name
                                    (cl-cc/ast:make-ast-call
                                     :func 'cons
                                     :args (list (cl-cc/ast:make-ast-int :value 1)
                                                 (cl-cc/ast:make-ast-int :value 2))))))))
           :body
           (list (cl-cc/ast:make-ast-if
                  :cond (cl-cc/ast:make-ast-var :name 'flag)
                   :then (cl-cc/ast:make-ast-slot-value
                          :object (cl-cc/ast:make-ast-var :name 'obj)
                          :slot 'name)
                   :else (cl-cc/ast:make-ast-int :value 0)))))

    (setf reg (compile-ast ast ctx))
    (setf insts (codegen-instructions ctx)
          jump-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) insts)
          cons-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-cons)) insts))
    (assert-true (keywordp reg))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read))
    (assert-true jump-pos)
    (assert-true cons-pos)
    (assert-true (> cons-pos jump-pos))))

(deftest codegen-multibinding-branch-local-make-instance-sinks-initarg-evaluation
  "The generalized branch sink handles a non-escaping make-instance binding with sibling let bindings."
  (let* ((ctx (make-codegen-ctx))
         (ast (cl-cc/ast:make-ast-let
               :bindings (list (cons 'obj
                                     (cl-cc/ast:make-ast-make-instance
                                      :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                                      :initargs (list (cons :name
                                                            (cl-cc/ast:make-ast-call
                                                             :func 'cons
                                                             :args (list (cl-cc/ast:make-ast-int :value 1)
                                                                         (cl-cc/ast:make-ast-int :value 2)))))))
                               (cons 'flag (cl-cc/ast:make-ast-int :value 1)))
               :body (list (cl-cc/ast:make-ast-if
                            :cond (cl-cc/ast:make-ast-var :name 'flag)
                            :then (cl-cc/ast:make-ast-slot-value
                                   :object (cl-cc/ast:make-ast-var :name 'obj)
                                   :slot 'name)
                            :else (cl-cc/ast:make-ast-int :value 0)))))
         (reg (compile-ast ast ctx))
         (insts (codegen-instructions ctx))
         (jump-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) insts))
         (cons-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-cons)) insts)))
    (assert-true (keywordp reg))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read))
    (assert-true jump-pos)
    (assert-true cons-pos)
    (assert-true (> cons-pos jump-pos))))

(deftest codegen-branch-local-make-instance-shadowed-binding-does-not-sink
  "Shadowed branch-local bindings must not count as uses of the outer make-instance binding."
  (let* ((ctx (make-codegen-ctx))
         (ast nil)
         (reg nil)
         (insts nil)
         (jump-pos nil)
         (cons-pos nil))
    (setf (cl-cc/compile::ctx-env ctx) (list (cons 'flag :R10)))
    (setf ast
          (cl-cc/ast:make-ast-let
           :bindings
           (list (cons 'obj
                       (cl-cc/ast:make-ast-make-instance
                        :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                        :initargs
                        (list (cons :name
                                    (cl-cc/ast:make-ast-call
                                     :func 'cons
                                     :args (list (cl-cc/ast:make-ast-int :value 1)
                                                 (cl-cc/ast:make-ast-int :value 2))))))))
           :body
           (list (cl-cc/ast:make-ast-if
                  :cond (cl-cc/ast:make-ast-var :name 'flag)
                  :then (cl-cc/ast:make-ast-let
                         :bindings (list (cons 'obj
                                                (cl-cc/ast:make-ast-make-instance
                                                 :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                                                 :initargs (list (cons :name (cl-cc/ast:make-ast-quote :value "shadow"))))))
                          :body (list (cl-cc/ast:make-ast-slot-value
                                       :object (cl-cc/ast:make-ast-var :name 'obj)
                                       :slot 'name)))
                   :else (cl-cc/ast:make-ast-int :value 0)))))

    (setf reg (compile-ast ast ctx))
    (setf insts (codegen-instructions ctx)
          jump-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) insts)
          cons-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-cons)) insts))
    (assert-true (keywordp reg))
    (assert-true jump-pos)
    (assert-true cons-pos)
    (assert-true (< cons-pos jump-pos))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read))))

;;; ─── compile-ast: ast-set-slot-value ─────────────────────────────────────────

(deftest codegen-set-slot-value
  "set-slot-value emits vm-slot-write with correct slot name and returns a register."
  (let* ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile::ctx-env ctx) (list (cons 'obj :R60)))
    (let* ((reg  (compile-ast (cl-cc/ast:make-ast-set-slot-value
                                :object (cl-cc/ast:make-ast-var :name 'obj)
                                :slot 'weight
                                :value (cl-cc/ast:make-ast-int :value 42))
                               ctx))
           (inst (codegen-find-inst ctx 'cl-cc/vm::vm-slot-write)))
      (assert-true inst)
      (assert-eq 'weight (cl-cc/vm::vm-slot-name-sym inst))
      (assert-true (keywordp reg)))))

(deftest codegen-noescape-make-instance-set-slot-value-bypasses-slot-write
  "A non-escaping local make-instance binding updates split slot registers directly."
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (cl-cc/ast:make-ast-let
                 :bindings (list (cons 'obj (cl-cc/ast:make-ast-make-instance
                                            :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                                            :initargs (list (cons :weight (cl-cc/ast:make-ast-int :value 1))))))
                 :body (list (cl-cc/ast:make-ast-set-slot-value
                              :object (cl-cc/ast:make-ast-var :name 'obj)
                              :slot 'weight
                              :value (cl-cc/ast:make-ast-int :value 42))
                             (cl-cc/ast:make-ast-slot-value
                              :object (cl-cc/ast:make-ast-var :name 'obj)
                              :slot 'weight)))
                ctx)))
      (assert-true (keywordp reg))
      (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj))
      (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-slot-write))
      (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read))
      (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-move)))))

;;; ─── phase2 CLOS helpers ─────────────────────────────────────────────────────

(deftest-each phase2-slot-ops-emit-instruction
  "slot-boundp, slot-exists-p, and slot-makunbound each emit their respective VM instruction."
  :cases (("slot-boundp"     'slot-boundp     'cl-cc/vm::vm-slot-boundp)
          ("slot-exists-p"   'slot-exists-p   'cl-cc/vm::vm-slot-exists-p)
          ("slot-makunbound" 'slot-makunbound 'cl-cc/vm::vm-slot-makunbound))
  (fn-name vm-type)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn-name (make-int 0) (make-quoted 'name)) ctx)
    (assert-true (codegen-find-inst ctx vm-type))))

(deftest phase2-slot-boundp-stores-slot-name
  "(slot-boundp obj 'foo) stores the slot symbol in the instruction"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'slot-boundp (make-int 0) (make-quoted 'foo)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-slot-boundp)))
      (assert-eq 'foo (cl-cc/vm::vm-slot-name-sym inst)))))

(deftest-each phase2-call-next-method-args-reg
  "call-next-method: args-reg is nil with no arguments, non-nil when arguments are present."
  :cases (("no-args"   (make-call 'call-next-method)           nil)
          ("with-args" (make-call 'call-next-method (make-int 42)) t))
  (ast args-reg-truthy-p)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast ast ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-call-next-method)))
      (assert-true inst)
      (if args-reg-truthy-p
          (assert-true  (cl-cc::vm-call-next-method-args-reg inst))
          (assert-false (cl-cc::vm-call-next-method-args-reg inst))))))

(deftest phase2-call-next-method-args-is-cons-list
  "(call-next-method x y) builds cons list for args"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'call-next-method (make-int 1) (make-int 2)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-cons))))
