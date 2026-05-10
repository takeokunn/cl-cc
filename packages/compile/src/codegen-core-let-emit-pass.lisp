;;;; codegen-core-let-emit-pass.lisp — Let-binding emitters and compile-ast for ast-let
(in-package :cl-cc/compile)


;;; ── Binding emitters ─────────────────────────────────────────────────────

(defun %emit-let-noescape-array (ctx name size)
  "Emit element registers for a noescape array of SIZE and register the binding."
  (let* ((zero-reg (make-register ctx))
         (element-regs nil))
    (emit ctx (make-vm-const :dst zero-reg :value 0))
    (dotimes (index size)
      (declare (ignore index))
      (push zero-reg element-regs))
    (push (cons name (cons size (nreverse element-regs)))
          (ctx-noescape-array-bindings ctx))))

(defun %emit-let-noescape-cons (ctx name expr)
  "Compile the two cons args and register the noescape cons binding."
  (let ((car-reg (compile-ast (first  (ast-call-args expr)) ctx))
        (cdr-reg (compile-ast (second (ast-call-args expr)) ctx)))
    (push (cons name (cons car-reg cdr-reg))
          (ctx-noescape-cons-bindings ctx))))

;; NOTE: The following three helpers must be MACROS, not defuns. They push onto
;; a local accumulator variable (new-bindings / special-restores) in the caller's
;; scope; as defuns, the push would mutate a copy and be lost, producing the
;; "Unbound variable" class of compile-ast failures for normal/boxed/special let
;; bindings. Converted to macros so the push happens in the outer lexical env.

(defmacro %emit-let-special (ctx name expr special-restores)
  "Save/restore a dynamic special binding, pushing the save to SPECIAL-RESTORES."
  (let ((old-reg (gensym "OLD-REG"))
        (new-reg (gensym "NEW-REG")))
    (list 'let
          (list (list old-reg (list 'make-register ctx))
                (list new-reg (list 'compile-ast expr ctx)))
          (list 'emit ctx (list 'make-vm-get-global :dst old-reg :name name))
          (list 'emit ctx (list 'make-vm-set-global :name name :src new-reg))
          (list 'push (list 'cons name old-reg) special-restores))))

(defmacro %emit-let-boxed (ctx name expr new-bindings)
  "Compile EXPR and wrap it in a cons box for a mutable captured variable."
  (let ((val-reg (gensym "VAL"))
        (own-reg (gensym "OWN"))
        (box-reg (gensym "BOX"))
        (nil-reg (gensym "NIL-REG")))
    (list 'let
          (list (list val-reg (list 'compile-ast expr ctx))
                (list own-reg (list 'make-register ctx))
                (list box-reg (list 'make-register ctx))
                (list nil-reg (list 'make-register ctx)))
          (list 'emit ctx (list 'make-vm-move :dst own-reg :src val-reg))
          (list 'emit ctx (list 'make-vm-const :dst nil-reg :value nil))
          (list 'emit ctx (list 'make-vm-cons :dst box-reg :car-src own-reg :cdr-src nil-reg))
          (list 'push (list 'cons name box-reg) new-bindings))))

(defmacro %emit-let-normal (ctx name expr new-bindings)
  "Compile EXPR and copy it into a fresh owned register."
  (let ((val-reg (gensym "VAL"))
        (own-reg (gensym "OWN")))
    (list 'let
          (list (list val-reg (list 'compile-ast expr ctx))
                (list own-reg (list 'make-register ctx)))
          (list 'emit ctx (list 'make-vm-move :dst own-reg :src val-reg))
          (list 'push (list 'cons name own-reg) new-bindings))))

;;; ── compile-ast (ast-let) ────────────────────────────────────────────────
;;;
;;; Dispatcher: tries optimization strategies in order (sink-if → noescape
;;; closure → noescape instance → noescape array → noescape cons → special
;;; → needs-boxing → ignored → normal).

(defun %ast-let-binding-inline-policy (name expr declarations)
  (and (typep expr 'ast-lambda)
       (%merge-inline-policies (%local-optimize-inline-policy declarations)
                               (%declaration-inline-policy declarations name))))

(defun %compile-let-binding-expr (name expr declarations ctx)
  (let ((inline-policy (%ast-let-binding-inline-policy name expr declarations)))
    (if inline-policy
        (let ((old-policy (ctx-pending-inline-policy ctx)))
          (unwind-protect
               (progn
                 (setf (ctx-pending-inline-policy ctx)
                       (%merge-inline-policies old-policy inline-policy))
                 (compile-ast expr ctx))
            (setf (ctx-pending-inline-policy ctx) old-policy)))
        (compile-ast expr ctx))))

(defmethod compile-ast ((node ast-let) ctx)
  (let ((sunk (%ast-let-sink-if-candidate node)))
    (if sunk
        (return-from compile-ast (compile-ast sunk ctx))))
  (let ((old-env (ctx-env ctx))
        (old-boxed (ctx-boxed-vars ctx))
        (old-noescape-cons (ctx-noescape-cons-bindings ctx))
        (old-noescape-arrays (ctx-noescape-array-bindings ctx))
        (old-noescape-instances (ctx-noescape-instance-bindings ctx))
        (old-noescape-closures (ctx-noescape-closure-bindings ctx))
        (old-type-env (ctx-type-env ctx)))
    (unwind-protect
         (let ((bindings (ast-let-bindings node))
               (binding-names nil)
               (declarations (ast-let-declarations node))
               (body-forms (ast-let-body node))
               (mutated nil)
               (captured nil)
               (needs-boxing nil)
               (new-bindings nil)
               (special-restores nil))
           (setq binding-names (mapcar #'car bindings))
           (dolist (form body-forms)
             (setq mutated (%list-union-eq (find-mutated-variables form) mutated)))
           (setq captured (find-captured-in-children body-forms binding-names))
           (setq needs-boxing (%list-intersection-eq mutated captured))
           (dolist (binding bindings)
             (let* ((name (car binding))
                    (expr (cdr binding))
                    (noescape-closure
                     (%let-noescape-closure name expr declarations mutated captured body-forms))
                    (noescape-instance-slots
                     (if noescape-closure
                         nil
                         (%let-noescape-instance-slots name expr mutated
                                                       captured body-forms
                                                       ctx)))
                    (noescape-array-size
                     (if (or noescape-closure noescape-instance-slots)
                         nil
                         (%let-noescape-array-size name expr declarations mutated
                                                   captured body-forms)))
                    (noescape-cons-p
                     (if (or noescape-closure noescape-instance-slots
                             noescape-array-size)
                         nil
                         (%let-noescape-cons-p name expr declarations mutated captured
                                               body-forms))))
               (cond
                 (noescape-closure
                  (setf (ctx-noescape-closure-bindings ctx)
                        (cons (cons name noescape-closure)
                              (ctx-noescape-closure-bindings ctx))))
                 (noescape-instance-slots
                  (setf (ctx-noescape-instance-bindings ctx)
                        (cons (cons name noescape-instance-slots)
                              (ctx-noescape-instance-bindings ctx))))
                 (noescape-array-size
                  (%emit-let-noescape-array ctx name noescape-array-size))
                 (noescape-cons-p
                  (%emit-let-noescape-cons ctx name expr))
                 ((%let-binding-special-p name ctx)
                  (let ((old-reg (make-register ctx))
                        (new-reg (%compile-let-binding-expr name expr declarations ctx)))
                    (emit ctx (make-vm-get-global :dst old-reg :name name))
                    (emit ctx (make-vm-set-global :name name :src new-reg))
                    (setq special-restores
                          (cons (cons name old-reg) special-restores))))
                 ((%member-eq-p name needs-boxing)
                  (let ((val-reg (%compile-let-binding-expr name expr declarations ctx))
                        (own-reg (make-register ctx))
                        (box-reg (make-register ctx))
                        (nil-reg (make-register ctx)))
                    (emit ctx (make-vm-move :dst own-reg :src val-reg))
                    (emit ctx (make-vm-const :dst nil-reg :value nil))
                    (emit ctx (make-vm-cons :dst box-reg
                                            :car-src own-reg
                                            :cdr-src nil-reg))
                    (setq new-bindings
                          (cons (cons name box-reg) new-bindings))))
                 ((%ast-let-binding-ignored-p name declarations)
                  (setq new-bindings
                        (cons (cons name (%compile-let-binding-expr name expr declarations ctx))
                              new-bindings)))
                 (t
                  (let ((val-reg (%compile-let-binding-expr name expr declarations ctx))
                        (own-reg (make-register ctx)))
                    (emit ctx (make-vm-move :dst own-reg :src val-reg))
                    (setq new-bindings
                          (cons (cons name own-reg) new-bindings)))))))
           (setf (ctx-env ctx) (append (nreverse new-bindings) (ctx-env ctx)))
           (setf (ctx-boxed-vars ctx)
                 (%list-union-eq needs-boxing (ctx-boxed-vars ctx)))
           (dolist (binding bindings)
             (let ((binding-type (%ast-proven-type ctx (cdr binding))))
               (when binding-type
                 (setf (ctx-type-env ctx)
                       (type-env-extend (car binding)
                                        (type-to-scheme binding-type)
                                        (ctx-type-env ctx))))))
           (%call-with-declaration-policies
            ctx
            declarations
            (lambda ()
              (let ((tail (ctx-tail-position ctx))
                    (last nil))
                (loop for rest on body-forms
                      do (setf (ctx-tail-position ctx)
                               (if (null (cdr rest)) tail nil))
                         (setq last (compile-ast (car rest) ctx)))
                (dolist (restore (nreverse special-restores))
                  (emit ctx (make-vm-set-global :name (car restore)
                                                :src (cdr restore))))
                last))))
      (setf (ctx-env ctx) old-env)
      (setf (ctx-boxed-vars ctx) old-boxed)
      (setf (ctx-noescape-cons-bindings ctx) old-noescape-cons)
      (setf (ctx-noescape-array-bindings ctx) old-noescape-arrays)
      (setf (ctx-noescape-instance-bindings ctx) old-noescape-instances)
      (setf (ctx-noescape-closure-bindings ctx) old-noescape-closures)
      (setf (ctx-type-env ctx) old-type-env))))
