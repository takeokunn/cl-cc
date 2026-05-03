(in-package :cl-cc/compile)
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
       (not (%member-eq-p name mutated))
       (not (%member-eq-p name captured))
       (%closure-binding-direct-call-only-p body-forms name
                                            (length (ast-lambda-params expr)))
       expr))

(defun %let-noescape-instance-slots (name expr mutated captured body-forms ctx)
  "Return compiled slot alist when the binding can skip heap allocation, else NIL."
  (if (and (typep expr 'ast-make-instance)
           (not (%member-eq-p name mutated))
           (not (%member-eq-p name captured)))
      (let ((slot-names nil)
            (xs (ast-make-instance-initargs expr)))
        (tagbody
         scan-names
           (if (null xs) (go done-names))
           (setq slot-names (cons (symbol-name (car (car xs))) slot-names))
           (setq xs (cdr xs))
           (go scan-names)
         done-names)
        (setq slot-names (nreverse slot-names))
        (if (%instance-binding-static-slot-only-p body-forms name slot-names)
            (let ((compiled-slots nil)
                  (ys (ast-make-instance-initargs expr)))
              (tagbody
               scan-values
                 (if (null ys) (return-from %let-noescape-instance-slots
                                (nreverse compiled-slots)))
                 (let ((entry (car ys)))
                   (setq compiled-slots
                         (cons (cons (symbol-name (car entry))
                                     (compile-ast (cdr entry) ctx))
                               compiled-slots)))
                 (setq ys (cdr ys))
                 (go scan-values)))
            nil))
      nil))

(defun %let-noescape-array-size (name expr mutated captured body-forms)
  "Return the array size integer when the binding can skip heap allocation, else NIL."
  (and (%ast-make-array-int-call-p expr)
       (not (%member-eq-p name mutated))
       (not (%member-eq-p name captured))
       (let ((size (ast-int-value (first (ast-call-args expr)))))
         (and (%array-binding-static-access-p body-forms name size)
              size))))

(defun %let-noescape-cons-p (name expr mutated captured body-forms)
  "T when the cons binding never escapes (only CAR/CDR consumers)."
  (and (%ast-cons-call-p expr)
       (not (%member-eq-p name mutated))
       (not (%member-eq-p name captured))
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
           (let ((xs bindings))
             (tagbody
              scan-binding-names
                (if (null xs) (go done-binding-names))
                (setq binding-names (cons (car (car xs)) binding-names))
                (setq xs (cdr xs))
                (go scan-binding-names)
              done-binding-names))
           (setq binding-names (nreverse binding-names))
           (let ((xs body-forms))
             (tagbody
              scan-mutated
                (if (null xs) (go done-mutated))
                (setq mutated (%list-union-eq (find-mutated-variables (car xs)) mutated))
                (setq xs (cdr xs))
                (go scan-mutated)
              done-mutated))
           (setq captured (find-captured-in-children body-forms binding-names))
           (setq needs-boxing (%list-intersection-eq mutated captured))
           (let ((xs bindings))
             (tagbody
              scan-bindings
                (if (null xs) (go done-bindings))
                (let* ((binding (car xs))
                       (name (car binding))
                       (expr (cdr binding))
                       (noescape-closure
                        (%let-noescape-closure name expr mutated captured body-forms))
                       (noescape-instance-slots
                        (if noescape-closure
                            nil
                            (%let-noescape-instance-slots name expr mutated
                                                          captured body-forms
                                                          ctx)))
                       (noescape-array-size
                        (if (or noescape-closure noescape-instance-slots)
                            nil
                            (%let-noescape-array-size name expr mutated
                                                      captured body-forms)))
                       (noescape-cons-p
                        (if (or noescape-closure noescape-instance-slots
                                noescape-array-size)
                            nil
                            (%let-noescape-cons-p name expr mutated captured
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
                           (new-reg (compile-ast expr ctx)))
                       (emit ctx (make-vm-get-global :dst old-reg :name name))
                       (emit ctx (make-vm-set-global :name name :src new-reg))
                       (setq special-restores
                             (cons (cons name old-reg) special-restores))))
                    ((%member-eq-p name needs-boxing)
                     (let ((val-reg (compile-ast expr ctx))
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
                           (cons (cons name (compile-ast expr ctx))
                                 new-bindings)))
                    (t
                     (let ((val-reg (compile-ast expr ctx))
                           (own-reg (make-register ctx)))
                       (emit ctx (make-vm-move :dst own-reg :src val-reg))
                       (setq new-bindings
                             (cons (cons name own-reg) new-bindings))))))
                (setq xs (cdr xs))
                (go scan-bindings)
              done-bindings))
           (setf (ctx-env ctx) (append (nreverse new-bindings) (ctx-env ctx)))
           (setf (ctx-boxed-vars ctx)
                 (%list-union-eq needs-boxing (ctx-boxed-vars ctx)))
           (let ((xs bindings))
             (tagbody
              scan-types
                (if (null xs) (go done-types))
                (let* ((binding (car xs))
                       (binding-type (%ast-proven-type ctx (cdr binding))))
                  (if binding-type
                      (setf (ctx-type-env ctx)
                            (type-env-extend (car binding)
                                             (type-to-scheme binding-type)
                                             (ctx-type-env ctx)))))
                (setq xs (cdr xs))
                (go scan-types)
              done-types))
           (let ((last nil)
                 (tail (ctx-tail-position ctx))
                 (xs body-forms))
             (tagbody
              scan-body
                (if (null xs) (go done-body))
                (let ((form (car xs)))
                  (setf (ctx-tail-position ctx)
                        (if (null (cdr xs)) tail nil))
                  (setq last (compile-ast form ctx)))
                (setq xs (cdr xs))
                (go scan-body)
              done-body)
             (let ((rs (nreverse special-restores)))
               (tagbody
                scan-restores
                  (if (null rs) (return-from compile-ast last))
                  (let ((restore (car rs)))
                    (emit ctx (make-vm-set-global :name (car restore)
                                                  :src (cdr restore))))
                  (setq rs (cdr rs))
                  (go scan-restores)))))
      (setf (ctx-env ctx) old-env)
      (setf (ctx-boxed-vars ctx) old-boxed)
      (setf (ctx-noescape-cons-bindings ctx) old-noescape-cons)
      (setf (ctx-noescape-array-bindings ctx) old-noescape-arrays)
      (setf (ctx-noescape-instance-bindings ctx) old-noescape-instances)
      (setf (ctx-noescape-closure-bindings ctx) old-noescape-closures)
      (setf (ctx-type-env ctx) old-type-env))))
