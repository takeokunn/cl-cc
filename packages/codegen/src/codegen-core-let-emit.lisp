(in-package :cl-cc/codegen)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Let-Binding Classification Predicates, Emitters, and compile-ast
;;;
;;; Contains:
;;;   Five classification predicates (%let-binding-special-p, %let-noescape-*)
;;;   Five binding emitters (%emit-let-*)
;;;   compile-ast (ast-let) dispatch method — the only public entry point
;;;
;;; AST predicate helpers, sink-if analysis, and the %define-binding-walker
;;; macro + generated walkers are in codegen-core-let.lisp (loads before).
;;;
;;; Load order: after codegen-core-let.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Let-binding classification predicates ────────────────────────────────
;;;
;;; Prolog-style or-chain: compile-ast (ast-let) tries each in order;
;;; first match wins, falling back to normal heap-allocated binding.

(defun %let-binding-special-p (name ctx)
  "T when NAME is a dynamic special (earmuffs + registered as global)."
  (and (gethash name (ctx-global-variables ctx))
       (let ((s (symbol-name name)))
         (and (> (length s) 1)
              (char= (char s 0) #\*)
              (char= (char s (1- (length s))) #\*)))))

(defun %let-noescape-closure (name expr mutated captured body-forms)
  "Return EXPR if the binding can be inlined as a noescape closure, else NIL."
  (and (typep expr 'ast-lambda)
       (null (ast-lambda-optional-params expr))
       (null (ast-lambda-rest-param expr))
       (null (ast-lambda-key-params expr))
       (not (member name mutated))
       (not (member name captured))
       (%closure-binding-direct-call-only-p body-forms name
                                            (length (ast-lambda-params expr)))
       expr))

(defun %let-noescape-instance-slots (name expr mutated captured body-forms ctx)
  "Return compiled slot alist when the binding can skip heap allocation, else NIL."
  (and (typep expr 'ast-make-instance)
       (not (member name mutated))
       (not (member name captured))
       (let ((slot-names (loop for (key . _value-ast) in (ast-make-instance-initargs expr)
                               collect (symbol-name key))))
         (and (%instance-binding-static-slot-only-p body-forms name slot-names)
              (loop for (key . value-ast) in (ast-make-instance-initargs expr)
                    collect (cons (symbol-name key) (compile-ast value-ast ctx)))))))

(defun %let-noescape-array-size (name expr mutated captured body-forms)
  "Return the array size integer when the binding can skip heap allocation, else NIL."
  (and (%ast-make-array-int-call-p expr)
       (not (member name mutated))
       (not (member name captured))
       (let ((size (ast-int-value (first (ast-call-args expr)))))
         (and (%array-binding-static-access-p body-forms name size)
              size))))

(defun %let-noescape-cons-p (name expr mutated captured body-forms)
  "T when the cons binding never escapes (only CAR/CDR consumers)."
  (and (%ast-cons-call-p expr)
       (not (member name mutated))
       (not (member name captured))
       (not (binding-escapes-in-body-p body-forms name
                                       :safe-consumers '("CAR" "CDR")))))

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

(defmethod compile-ast ((node ast-let) ctx)
  (let ((sunk (%ast-let-sink-if-candidate node)))
    (when sunk
      (return-from compile-ast (compile-ast sunk ctx))))
  (let ((old-env (ctx-env ctx))
        (old-boxed (ctx-boxed-vars ctx))
        (old-noescape-cons (ctx-noescape-cons-bindings ctx))
        (old-noescape-arrays (ctx-noescape-array-bindings ctx))
        (old-noescape-instances (ctx-noescape-instance-bindings ctx))
        (old-noescape-closures (ctx-noescape-closure-bindings ctx))
        (old-type-env (ctx-type-env ctx)))
    (unwind-protect
         (let* ((binding-names (mapcar #'car (ast-let-bindings node)))
                (declarations (ast-let-declarations node))
                (body-forms (ast-let-body node))
                (mutated (reduce #'union (mapcar #'find-mutated-variables body-forms)
                                 :initial-value nil))
                (captured (find-captured-in-children body-forms binding-names))
                (needs-boxing (intersection mutated captured))
                (new-bindings nil)
                (special-restores nil))
           (dolist (binding (ast-let-bindings node))
             (let* ((name (car binding))
                    (expr (cdr binding))
                    (noescape-closure
                      (%let-noescape-closure name expr mutated captured body-forms))
                    (noescape-instance-slots
                      (unless noescape-closure
                        (%let-noescape-instance-slots name expr mutated captured body-forms ctx)))
                    (noescape-array-size
                      (unless (or noescape-closure noescape-instance-slots)
                        (%let-noescape-array-size name expr mutated captured body-forms)))
                    (noescape-cons-p
                      (unless (or noescape-closure noescape-instance-slots noescape-array-size)
                        (%let-noescape-cons-p name expr mutated captured body-forms))))
               (cond
                 (noescape-closure
                  (push (cons name noescape-closure) (ctx-noescape-closure-bindings ctx)))
                 (noescape-instance-slots
                  (push (cons name noescape-instance-slots) (ctx-noescape-instance-bindings ctx)))
                 (noescape-array-size
                  (%emit-let-noescape-array ctx name noescape-array-size))
                 (noescape-cons-p
                  (%emit-let-noescape-cons ctx name expr))
                 ((%let-binding-special-p name ctx)
                  (%emit-let-special ctx name expr special-restores))
                 ((member name needs-boxing)
                  (%emit-let-boxed ctx name expr new-bindings))
                 ((%ast-let-binding-ignored-p name declarations)
                  (push (cons name (compile-ast expr ctx)) new-bindings))
                 (t
                  (%emit-let-normal ctx name expr new-bindings)))))
           (setf (ctx-env ctx) (append (nreverse new-bindings) (ctx-env ctx)))
           (setf (ctx-boxed-vars ctx) (union needs-boxing (ctx-boxed-vars ctx)))
           (dolist (binding (ast-let-bindings node))
             (let ((binding-type (%ast-proven-type ctx (cdr binding))))
               (when binding-type
                 (setf (ctx-type-env ctx)
                       (cl-cc/type:type-env-extend
                        (car binding)
                        (cl-cc/type:type-to-scheme binding-type)
                        (ctx-type-env ctx))))))
           (let ((last nil)
                 (tail (ctx-tail-position ctx)))
             (dolist (form body-forms)
               (setf (ctx-tail-position ctx)
                     (if (eq form (car (last body-forms))) tail nil))
               (setf last (compile-ast form ctx)))
             (dolist (restore (nreverse special-restores))
               (emit ctx (make-vm-set-global :name (car restore) :src (cdr restore))))
             last))
      (setf (ctx-env ctx) old-env)
      (setf (ctx-boxed-vars ctx) old-boxed)
      (setf (ctx-noescape-cons-bindings ctx) old-noescape-cons)
      (setf (ctx-noescape-array-bindings ctx) old-noescape-arrays)
      (setf (ctx-noescape-instance-bindings ctx) old-noescape-instances)
      (setf (ctx-noescape-closure-bindings ctx) old-noescape-closures)
      (setf (ctx-type-env ctx) old-type-env))))
