;;;; tests/unit/compile/codegen-clos-tests.lisp — Codegen CLOS Unit Tests

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

(defun make-test-slot (name &key initarg)
  "Build a minimal ast-slot-def for use in codegen tests."
  (cl-cc::make-ast-slot-def :name name :initarg initarg))

;;; ─── compile-ast: ast-defclass ───────────────────────────────────────────────

(deftest codegen-defclass-compilation
  "Compiling defclass: emits vm-class-def with correct slot names, registers globally, returns register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-defclass
                             :name 'my-rect
                             :superclasses nil
                             :slots (list (make-test-slot 'w :initarg :w)
                                          (make-test-slot 'h :initarg :h)))
                           ctx))
         (inst (codegen-find-inst ctx 'cl-cc::vm-class-def)))
    (assert-true inst)
    (assert-equal '(w h) (cl-cc::vm-slot-names inst))
    (assert-true (gethash 'my-rect (cl-cc::ctx-global-classes ctx)))
    (assert-true (keywordp reg))))

;;; ─── compile-ast: ast-defgeneric ─────────────────────────────────────────────

(deftest codegen-defgeneric-compilation
  "Compiling defgeneric emits vm-class-def dispatch table and registers in global-generics."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defgeneric :name 'my-speak :params '(animal))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-class-def))
    (assert-true (gethash 'my-speak (cl-cc::ctx-global-generics ctx)))))

(deftest codegen-defgeneric-idempotent
  "Compiling the same defgeneric twice reuses the existing dispatch register."
  (let ((ctx (make-codegen-ctx)))
    (let ((r1 (compile-ast (cl-cc::make-ast-defgeneric :name 'my-compute :params '(x)) ctx))
          (r2 (compile-ast (cl-cc::make-ast-defgeneric :name 'my-compute :params '(x)) ctx)))
      (assert-eq r1 r2))))

;;; ─── compile-ast: ast-defmethod ──────────────────────────────────────────────

(deftest codegen-defmethod-compilation
  "Compiling defmethod emits vm-register-method (with correct specializer) and vm-closure."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defgeneric :name 'my-greet :params '(obj)) ctx)
    (compile-ast (cl-cc::make-ast-defmethod
                  :name 'my-greet
                  :specializers (list '(obj . dog))
                  :params '(obj)
                  :body (list (cl-cc::make-ast-int :value 99)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-register-method)))
      (assert-true inst)
      (assert-equal '(dog) (cl-cc::vm-method-specializer inst)))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-closure))))

;;; ─── compile-ast: ast-make-instance ─────────────────────────────────────────

(deftest-each codegen-make-instance-emits-vm-make-obj
  "make-instance emits vm-make-obj regardless of static vs dynamic class reference."
  :cases (("static"  (cl-cc::make-ast-make-instance
                       :class (cl-cc::make-ast-quote :value 'my-dog)
                       :initargs (list (cons :name (cl-cc::make-ast-quote :value 'rex))))
                     nil)
          ("dynamic" (cl-cc::make-ast-make-instance
                       :class (cl-cc::make-ast-var :name 'cls)
                       :initargs nil)
                     (list (cons 'cls :R50))))
  (ast env-setup)
  (let ((ctx (make-codegen-ctx)))
    (when env-setup (setf (cl-cc::ctx-env ctx) env-setup))
    (compile-ast ast ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-obj))))

(deftest codegen-make-instance-static-loads-class-globally
  "Static make-instance emits vm-get-global to load the class descriptor."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-make-instance
                  :class (cl-cc::make-ast-quote :value 'my-cat)
                  :initargs nil)
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-get-global))))

;;; ─── compile-ast: ast-slot-value ─────────────────────────────────────────────

(deftest codegen-slot-value
  "slot-value emits vm-slot-read with correct slot name and returns a register."
  (let* ((ctx (make-codegen-ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'obj :R42)))
    (let* ((reg  (compile-ast (cl-cc::make-ast-slot-value
                                :object (cl-cc::make-ast-var :name 'obj)
                                :slot 'radius)
                               ctx))
           (inst (codegen-find-inst ctx 'cl-cc::vm-slot-read)))
      (assert-true inst)
      (assert-eq 'radius (cl-cc::vm-slot-name-sym inst))
      (assert-true (keywordp reg)))))

(deftest codegen-noescape-make-instance-slot-value-bypasses-heap-object
  "A non-escaping local make-instance binding can serve slot-value from split slot registers."
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (cl-cc::make-ast-let
                 :bindings (list (cons 'obj (cl-cc::make-ast-make-instance
                                            :class (cl-cc::make-ast-quote :value 'my-dog)
                                            :initargs (list (cons :name (cl-cc::make-ast-quote :value 'rex))))))
                 :body (list (cl-cc::make-ast-slot-value
                              :object (cl-cc::make-ast-var :name 'obj)
                              :slot 'name)))
                ctx)))
      (assert-true (keywordp reg))
      (assert-null (codegen-find-inst ctx 'cl-cc::vm-make-obj))
      (assert-null (codegen-find-inst ctx 'cl-cc::vm-slot-read))
      (assert-true (codegen-find-inst ctx 'cl-cc::vm-move)))))

(deftest codegen-escaped-make-instance-slot-value-falls-back
  "Captured make-instance bindings keep the normal heap-backed slot path."
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (cl-cc::make-ast-let
                 :bindings (list (cons 'obj (cl-cc::make-ast-make-instance
                                            :class (cl-cc::make-ast-quote :value 'my-dog)
                                            :initargs (list (cons :name (cl-cc::make-ast-quote :value 'rex))))))
                 :body (list (cl-cc::make-ast-lambda :params '() :body (list (cl-cc::make-ast-var :name 'obj)))
                             (cl-cc::make-ast-slot-value
                              :object (cl-cc::make-ast-var :name 'obj)
                              :slot 'name)))
                ctx)))
       (assert-true (keywordp reg))
       (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-obj))
       (assert-true (codegen-find-inst ctx 'cl-cc::vm-slot-read)))))

(deftest codegen-branch-local-make-instance-sinks-initarg-evaluation
  "A branch-local non-escaping make-instance delays initarg evaluation into the used branch."
  (let* ((ctx (make-codegen-ctx))
         (ast nil)
         (reg nil)
         (insts nil)
         (jump-pos nil)
         (cons-pos nil))
    (setf (cl-cc::ctx-env ctx) (list (cons 'flag :R10)))
    (setf ast
          (cl-cc::make-ast-let
           :bindings
           (list (cons 'obj
                       (cl-cc::make-ast-make-instance
                        :class (cl-cc::make-ast-quote :value 'my-dog)
                        :initargs
                        (list (cons :name
                                    (cl-cc::make-ast-call
                                     :func 'cons
                                     :args (list (cl-cc::make-ast-int :value 1)
                                                 (cl-cc::make-ast-int :value 2))))))))
           :body
           (list (cl-cc::make-ast-if
                  :cond (cl-cc::make-ast-var :name 'flag)
                   :then (cl-cc::make-ast-slot-value
                          :object (cl-cc::make-ast-var :name 'obj)
                          :slot 'name)
                   :else (cl-cc::make-ast-int :value 0))))
    
    (setf reg (compile-ast ast ctx))
    (setf insts (codegen-instructions ctx)
          jump-pos (position-if (lambda (inst) (typep inst 'cl-cc::vm-jump-zero)) insts)
          cons-pos (position-if (lambda (inst) (typep inst 'cl-cc::vm-cons)) insts))
    (assert-true (keywordp reg))
    (assert-null (codegen-find-inst ctx 'cl-cc::vm-make-obj))
    (assert-null (codegen-find-inst ctx 'cl-cc::vm-slot-read))
    (assert-true jump-pos)
    (assert-true cons-pos)
    (assert-true (> cons-pos jump-pos)))))

(deftest codegen-multibinding-branch-local-make-instance-sinks-initarg-evaluation
  "The generalized branch sink handles a non-escaping make-instance binding with sibling let bindings."
  (let* ((ctx (make-codegen-ctx))
         (ast (cl-cc::make-ast-let
               :bindings (list (cons 'obj
                                     (cl-cc::make-ast-make-instance
                                      :class (cl-cc::make-ast-quote :value 'my-dog)
                                      :initargs (list (cons :name
                                                            (cl-cc::make-ast-call
                                                             :func 'cons
                                                             :args (list (cl-cc::make-ast-int :value 1)
                                                                         (cl-cc::make-ast-int :value 2)))))))
                               (cons 'flag (cl-cc::make-ast-int :value 1)))
               :body (list (cl-cc::make-ast-if
                            :cond (cl-cc::make-ast-var :name 'flag)
                            :then (cl-cc::make-ast-slot-value
                                   :object (cl-cc::make-ast-var :name 'obj)
                                   :slot 'name)
                            :else (cl-cc::make-ast-int :value 0))))
         (reg (compile-ast ast ctx))
         (insts (codegen-instructions ctx))
         (jump-pos (position-if (lambda (inst) (typep inst 'cl-cc::vm-jump-zero)) insts))
         (cons-pos (position-if (lambda (inst) (typep inst 'cl-cc::vm-cons)) insts)))
    (assert-true (keywordp reg))
    (assert-null (codegen-find-inst ctx 'cl-cc::vm-make-obj))
    (assert-null (codegen-find-inst ctx 'cl-cc::vm-slot-read))
    (assert-true jump-pos)
    (assert-true cons-pos)
    (assert-true (> cons-pos jump-pos)))))

(deftest codegen-branch-local-make-instance-shadowed-binding-does-not-sink
  "Shadowed branch-local bindings must not count as uses of the outer make-instance binding."
  (let* ((ctx (make-codegen-ctx))
         (ast nil)
         (reg nil)
         (insts nil)
         (jump-pos nil)
         (cons-pos nil))
    (setf (cl-cc::ctx-env ctx) (list (cons 'flag :R10)))
    (setf ast
          (cl-cc::make-ast-let
           :bindings
           (list (cons 'obj
                       (cl-cc::make-ast-make-instance
                        :class (cl-cc::make-ast-quote :value 'my-dog)
                        :initargs
                        (list (cons :name
                                    (cl-cc::make-ast-call
                                     :func 'cons
                                     :args (list (cl-cc::make-ast-int :value 1)
                                                 (cl-cc::make-ast-int :value 2))))))))
           :body
           (list (cl-cc::make-ast-if
                  :cond (cl-cc::make-ast-var :name 'flag)
                  :then (cl-cc::make-ast-let
                         :bindings (list (cons 'obj
                                                (cl-cc::make-ast-make-instance
                                                 :class (cl-cc::make-ast-quote :value 'my-dog)
                                                 :initargs (list (cons :name (cl-cc::make-ast-quote :value "shadow"))))))
                          :body (list (cl-cc::make-ast-slot-value
                                       :object (cl-cc::make-ast-var :name 'obj)
                                       :slot 'name)))
                   :else (cl-cc::make-ast-int :value 0))))

    (setf reg (compile-ast ast ctx))
    (setf insts (codegen-instructions ctx)
          jump-pos (position-if (lambda (inst) (typep inst 'cl-cc::vm-jump-zero)) insts)
          cons-pos (position-if (lambda (inst) (typep inst 'cl-cc::vm-cons)) insts))
    (assert-true (keywordp reg))
    (assert-true jump-pos)
    (assert-true cons-pos)
    (assert-true (< cons-pos jump-pos))
    (assert-null (codegen-find-inst ctx 'cl-cc::vm-make-obj))
    (assert-null (codegen-find-inst ctx 'cl-cc::vm-slot-read)))))

;;; ─── compile-ast: ast-set-slot-value ─────────────────────────────────────────

(deftest codegen-set-slot-value
  "set-slot-value emits vm-slot-write with correct slot name and returns a register."
  (let* ((ctx (make-codegen-ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'obj :R60)))
    (let* ((reg  (compile-ast (cl-cc::make-ast-set-slot-value
                                :object (cl-cc::make-ast-var :name 'obj)
                                :slot 'weight
                                :value (cl-cc::make-ast-int :value 42))
                               ctx))
           (inst (codegen-find-inst ctx 'cl-cc::vm-slot-write)))
      (assert-true inst)
      (assert-eq 'weight (cl-cc::vm-slot-name-sym inst))
      (assert-true (keywordp reg)))))

(deftest codegen-noescape-make-instance-set-slot-value-bypasses-slot-write
  "A non-escaping local make-instance binding updates split slot registers directly." 
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (cl-cc::make-ast-let
                 :bindings (list (cons 'obj (cl-cc::make-ast-make-instance
                                            :class (cl-cc::make-ast-quote :value 'my-dog)
                                            :initargs (list (cons :weight (cl-cc::make-ast-int :value 1))))))
                 :body (list (cl-cc::make-ast-set-slot-value
                              :object (cl-cc::make-ast-var :name 'obj)
                              :slot 'weight
                              :value (cl-cc::make-ast-int :value 42))
                             (cl-cc::make-ast-slot-value
                              :object (cl-cc::make-ast-var :name 'obj)
                              :slot 'weight)))
                ctx)))
      (assert-true (keywordp reg))
      (assert-null (codegen-find-inst ctx 'cl-cc::vm-make-obj))
      (assert-null (codegen-find-inst ctx 'cl-cc::vm-slot-write))
      (assert-null (codegen-find-inst ctx 'cl-cc::vm-slot-read))
      (assert-true (codegen-find-inst ctx 'cl-cc::vm-move)))))

;;; ─── phase2 CLOS helpers ─────────────────────────────────────────────────────

(deftest phase2-slot-boundp-emits-instruction
  "(slot-boundp obj 'slot) emits vm-slot-boundp"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'slot-boundp (make-int 0) (make-quoted 'name)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-slot-boundp))))

(deftest phase2-slot-boundp-stores-slot-name
  "(slot-boundp obj 'foo) stores the slot symbol in the instruction"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'slot-boundp (make-int 0) (make-quoted 'foo)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-slot-boundp)))
      (assert-eq 'foo (cl-cc::vm-slot-name-sym inst)))))

(deftest phase2-slot-exists-p-emits-instruction
  "(slot-exists-p obj 'slot) emits vm-slot-exists-p"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'slot-exists-p (make-int 0) (make-quoted 'name)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-slot-exists-p))))

(deftest phase2-slot-makunbound-emits-instruction
  "(slot-makunbound obj 'slot) emits vm-slot-makunbound"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'slot-makunbound (make-int 0) (make-quoted 'name)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-slot-makunbound))))

(deftest phase2-call-next-method-no-args
  "(call-next-method) with no args emits vm-call-next-method with nil args-reg"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'call-next-method) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-call-next-method)))
      (assert-true inst)
      (assert-true (null (cl-cc::vm-call-next-method-args-reg inst))))))

(deftest phase2-call-next-method-with-args
  "(call-next-method x) with args emits vm-call-next-method with args-reg set"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'call-next-method (make-int 42)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-call-next-method)))
      (assert-true inst)
      (assert-true (cl-cc::vm-call-next-method-args-reg inst)))))

(deftest phase2-call-next-method-args-is-cons-list
  "(call-next-method x y) builds cons list for args"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'call-next-method (make-int 1) (make-int 2)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-cons))))
