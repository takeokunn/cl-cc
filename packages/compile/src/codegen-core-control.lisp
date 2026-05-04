(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Control Flow, Assignment, and Type Assertions
;;;
;;; Contains: lookup-block, compile-ast for ast-block/ast-return-from,
;;; lookup-tag, compile-ast for ast-tagbody/ast-go,
;;; compile-ast for ast-setq/ast-quote/ast-the,
;;; type-error-message-from-mismatch, %emit-the-runtime-assertion.
;;;
;;; Primitive and if-form compilation (compile-ast for ast-int through ast-if)
;;; plus the binop dispatch table and helpers are in codegen-core.lisp (loads before).
;;;
;;; Load order: after codegen-core.lisp, before codegen-core-let.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Control flow: block / return-from ────────────────────────────────────
;;; (Let-binding optimization subsystem is in codegen-core-let.lisp.)

(defun lookup-block (ctx name)
  "Look up a block by name, returning (exit-label . result-reg) or error."
  (let ((entry (%assoc-eq name (ctx-block-env ctx))))
    (if entry
        (cdr entry)
        (error "Unknown block: ~S" name))))

(defmethod compile-ast ((node ast-block) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let* ((block-name (ast-block-name node))
         (exit-label (make-label ctx "block_exit"))
         (result-reg (make-register ctx))
         (old-block-env (ctx-block-env ctx)))
    (unwind-protect
         (progn
           (setf (ctx-block-env ctx)
                 (cons (cons block-name (cons exit-label result-reg))
                       (ctx-block-env ctx)))
           (let ((last nil)
                 (xs (ast-block-body node)))
             (tagbody
              scan
                (if (null xs) (go done))
                (setf last (compile-ast (car xs) ctx))
                (setq xs (cdr xs))
                (go scan)
              done)
             (emit ctx (make-vm-move :dst result-reg :src last))))
      (setf (ctx-block-env ctx) old-block-env))
    (emit ctx (make-vm-label :name exit-label))
    result-reg))

(defmethod compile-ast ((node ast-return-from) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let* ((block-info (lookup-block ctx (ast-return-from-name node)))
         (exit-label (car block-info))
         (result-reg (cdr block-info))
         (value-reg (compile-ast (ast-return-from-value node) ctx)))
    (emit ctx (make-vm-move :dst result-reg :src value-reg))
    (emit ctx (make-vm-jump :label exit-label))
    result-reg))

;;; ── Control flow: tagbody / go ───────────────────────────────────────────

(defun lookup-tag (ctx tag)
  "Look up a tag within the current tagbody, returning its label or error."
  (let ((entry (%assoc-eq tag (ctx-tagbody-env ctx))))
    (if entry
        (cdr entry)
        (error "Unknown tag: ~S" tag))))

(defmethod compile-ast ((node ast-tagbody) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let* ((tags (ast-tagbody-tags node))
         (end-label (make-label ctx "tagbody_end"))
         (result-reg (make-register ctx))
         (old-tagbody-env (ctx-tagbody-env ctx))
         (tag-labels nil))
    (let ((xs tags))
      (tagbody
       build
         (if (null xs) (go built))
         (let ((tag-entry (car xs)))
           (setq tag-labels
                 (cons (cons (car tag-entry) (make-label ctx "tag")) tag-labels)))
         (setq xs (cdr xs))
         (go build)
       built))
    (setq tag-labels (nreverse tag-labels))
    (unwind-protect
         (progn
           (setf (ctx-tagbody-env ctx) (append tag-labels (ctx-tagbody-env ctx)))
           (if tag-labels
               (emit ctx (make-vm-jump :label (cdr (car tag-labels))))
               (emit ctx (make-vm-const :dst result-reg :value nil)))
           (let ((xs tags))
             (tagbody
              scan-tags
                (if (null xs) (go done-tags))
                (let* ((tag-entry (car xs))
                       (tag (car tag-entry))
                       (forms (cdr tag-entry))
                       (label (cdr (%assoc-eq tag tag-labels))))
                  (emit ctx (make-vm-label :name label))
                  (let ((ys forms))
                    (tagbody
                     scan-forms
                       (if (null ys) (go done-forms))
                       (compile-ast (car ys) ctx)
                       (setq ys (cdr ys))
                       (go scan-forms)
                     done-forms))
                  (if forms (emit ctx (make-vm-jump :label end-label))))
                (setq xs (cdr xs))
                (go scan-tags)
              done-tags)))
      (setf (ctx-tagbody-env ctx) old-tagbody-env))
    (emit ctx (make-vm-label :name end-label))
    (emit ctx (make-vm-const :dst result-reg :value nil))
    result-reg))

(defmethod compile-ast ((node ast-go) ctx)
  (emit ctx (make-vm-jump :label (lookup-tag ctx (ast-go-tag node))))
  (make-register ctx))

;;; ── Assignment: setq / quote / the ──────────────────────────────────────

(defmethod compile-ast ((node ast-setq) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let* ((var-name (ast-setq-var node))
         (value-reg (compile-ast (ast-setq-value node) ctx))
         (local-entry (%assoc-eq var-name (ctx-env ctx))))
    (cond
      ((and local-entry (%member-eq-p var-name (ctx-boxed-vars ctx)))
       ;; Boxed variable: write via (rplaca box new-val)
       (emit ctx (make-vm-rplaca :cons (cdr local-entry) :val value-reg))
       value-reg)
      (local-entry
       (emit ctx (make-vm-move :dst (cdr local-entry) :src value-reg))
       (cdr local-entry))
      ((gethash var-name (ctx-global-variables ctx))
       (emit ctx (make-vm-set-global :name var-name :src value-reg))
       value-reg)
      (t
       (error "Unbound variable for setq: ~S" var-name)))))

(defmethod compile-ast ((node ast-quote) ctx)
  (let ((dst (make-register ctx)))
    (emit ctx (make-vm-const :dst dst :value (ast-quote-value node)))
    dst))

(defun type-error-message-from-mismatch (e)
  "Extract a human-readable message from a type-mismatch-error condition."
  (format nil "expected ~A but got ~A"
          (type-to-string (type-mismatch-error-expected e))
          (type-to-string (type-mismatch-error-actual e))))

(defun %values-type-specifier-p (declared-spec)
  (and (consp declared-spec)
       (eq (car declared-spec) 'values)))

(defun %emit-the-runtime-assertion (ctx value-reg declared-spec &key (emit-failure-p t))
  "Emit a runtime assertion for (the DECLARED-TYPE VALUE-REG).
When EMIT-FAILURE-P is NIL, keep the lightweight type check but omit failure handling."
  (unless (or (null declared-spec)
              (eq declared-spec 't)
              (type-unknown-p declared-spec))
    (when (> (ctx-safety ctx) 0)
      (let ((check-reg (make-register ctx))
            (values-type-p (%values-type-specifier-p declared-spec)))
        (if values-type-p
            (emit ctx (make-vm-values-typep :dst check-reg :src value-reg :type-name declared-spec))
            (emit ctx (make-vm-typep :dst check-reg :src value-reg :type-name declared-spec)))
        (when emit-failure-p
          (let ((fail-label (make-label ctx "the_fail"))
                (done-label (make-label ctx "the_done"))
                (error-reg (make-register ctx)))
            (emit ctx (make-vm-jump-zero :reg check-reg :label fail-label))
            (emit ctx (make-vm-jump :label done-label))
            (emit ctx (make-vm-label :name fail-label))
            (emit ctx (make-vm-type-error-condition
                       :dst error-reg
                       :expected-type declared-spec
                       :datum-reg value-reg
                       :values-p values-type-p))
            (emit ctx (make-vm-signal-error :error-reg error-reg))
            (emit ctx (make-vm-label :name done-label)))))))
  value-reg)

(defmethod compile-ast ((node ast-the) ctx)
  "Compile a type declaration. In typed-function mode, verifies the type at compile time."
  (let ((reg (compile-ast (ast-the-value node) ctx)))
    (let* ((declared (ast-the-type node))
           (declared-spec (and declared (parse-type-specifier declared))))
      (when *compiling-typed-fn*
        (when (and declared-spec
                   (not (and (typep (ast-the-value node) 'ast-var)
                             (let ((proven (%ast-proven-type ctx (ast-the-value node))))
                               (and proven
                                    (type-equal-p proven declared-spec))))))
          (handler-case
              (check (ast-the-value node) declared-spec
                                (or (ctx-type-env ctx)
                                    (type-env-empty)))
            (type-mismatch-error (e)
              (error 'ast-compilation-error
                     :location (format nil "~A:~A"
                                       (ast-source-file node)
                                       (ast-source-line node))
                     :format-control "Type error in ~A: ~A"
                     :format-arguments (list *compiling-typed-fn*
                                             (type-error-message-from-mismatch e))))
            (type-inference-error () nil))))
      (if (and declared-spec
               (typep (ast-the-value node) 'ast-var)
               (let ((proven (%ast-proven-type ctx (ast-the-value node))))
                 (and proven (type-equal-p proven declared-spec))))
           (%emit-the-runtime-assertion ctx reg declared :emit-failure-p nil)
           (%emit-the-runtime-assertion ctx reg declared))
      reg)))
