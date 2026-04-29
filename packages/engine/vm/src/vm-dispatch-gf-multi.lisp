(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Qualified-Method Returns and Multi-Dispatch Resolution
;;;
;;; Contains: qualified-return micro-helpers (%vm-return-to-caller,
;;; %vm-jump-to-next-method, %vm-ret-qualified-dispatch), and
;;; multi-dispatch resolution helpers (%vm-gf-uses-composite-keys-p,
;;; %vm-resolve-single-dispatch, %vm-resolve-composite-dispatch,
;;; vm-resolve-gf-method, vm-resolve-multi-dispatch,
;;; vm-try-dispatch-combinations, vm-try-dispatch-sub).
;;;
;;; Core dispatch (vm-classify-arg, EQL helpers, applicable-methods,
;;; combo-collect) is in vm-dispatch-gf.lisp (loads before this file).
;;; Call dispatch (vm-dispatch-generic-call, %vm-dispatch-call) is in
;;; vm-dispatch-gf-call.lisp (loads after this file).
;;;
;;; Load order: after vm-dispatch-gf.lisp, before vm-dispatch-gf-call.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Qualified-method return helpers ──────────────────────────────────────
;;;
;;; Two micro-patterns appear three times each inside %vm-ret-qualified-dispatch.
;;; Extract them as named helpers so the dispatcher reads as a pure phase table.

(defun %vm-return-to-caller (state result)
  "Pop the top call frame and resume the caller, returning RESULT.
Returns (values new-pc halt-p halt-result)."
  (if (vm-call-stack state)
      (destructuring-bind (return-pc dst-reg old-closure-env saved-regs)
          (pop (vm-call-stack state))
        (vm-profile-return state)
        (vm-restore-registers state saved-regs)
        (vm-reg-set state dst-reg result)
        (when old-closure-env
          (setf (vm-closure-env state) old-closure-env))
        (values return-pc nil nil))
      (values nil t result)))

(defun %vm-jump-to-next-method (state labels method-closure arg-values)
  "Tail-call METHOD-CLOSURE with ARG-VALUES and return the new PC triple."
  (vm-profile-enter-call state (vm-closure-entry-label method-closure) :tail-p t)
  (vm-bind-closure-args method-closure state arg-values)
  (values (vm-label-table-lookup labels (vm-closure-entry-label method-closure)) nil nil))

(defun %vm-ret-qualified-dispatch (state result labels method-entry)
  "Handle return from a qualified method using standard method combination:
:around → :before → primary → :after. Around methods wrap everything; their return
value is the final result. call-next-method from an around triggers before/primary/after."
  (let* ((props         (cdddr method-entry))
         (before-pending (getf props :before-pending))
         (primary        (getf props :primary))
         (after-pending  (getf props :after-pending))
         (arg-values     (getf props :arg-values))
         (phase          (getf props :phase)))
    (cond
      ;; Around method returned without calling call-next-method.
      ((eq phase :around)
       (pop (vm-method-call-stack state))
       (%vm-return-to-caller state result))

      ;; More :before methods pending — chain next one.
      (before-pending
       (let ((next-before (car before-pending)))
         (setf (getf (cdddr method-entry) :before-pending) (cdr before-pending))
         (%vm-jump-to-next-method state labels next-before arg-values)))

      ;; All befores done — invoke the primary.
      ((and primary (eq phase :before))
       (setf (getf (cdddr method-entry) :primary) nil)
       (setf (getf (cdddr method-entry) :phase) :primary)
       (%vm-jump-to-next-method state labels primary arg-values))

      ;; Primary done — chain :after methods, saving the primary result.
      (after-pending
       (let ((next-after (car after-pending)))
         (setf (getf (cdddr method-entry) :after-pending) (cdr after-pending))
         (when (eq phase :primary)
           (setf (getf (cdddr method-entry) :primary-result) result)
           (setf (getf (cdddr method-entry) :phase) :after))
         (%vm-jump-to-next-method state labels next-after arg-values)))

      ;; All phases done — return to around or to original caller.
      (t
       (let ((primary-result (if (eq phase :primary)
                                 result
                                 (or (getf props :primary-result) result))))
         (if (getf props :around-return-phase)
             ;; Resume around method's call-next-method continuation.
             (progn
               (setf (getf (cdddr method-entry) :phase) :around)
               (setf (getf (cdddr method-entry) :around-return-phase) nil)
               (vm-reg-set state (getf props :around-cnm-dst) primary-result)
               (values (getf props :around-return-pc) nil nil))
             ;; Normal return to the original caller.
             (progn
               (pop (vm-method-call-stack state))
               (%vm-return-to-caller state primary-result))))))))

;;; ── GF dispatch helpers ───────────────────────────────────────────────────

(defun %vm-gf-uses-composite-keys-p (methods-ht)
  "T when METHODS-HT contains a true multi-dispatch tuple key.
Single-argument EQL specializers like (eql 42) are list-valued keys but should
still use the single-dispatch path."
  (block check
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (and (listp k)
                          (not (%eql-specializer-p k)))
                 (return-from check t)))
             methods-ht)
    nil))

(defun %vm-resolve-single-dispatch (gf-ht methods-ht state first-arg)
  "Resolve a method by single dispatch on FIRST-ARG class + eql specializers."
  (let ((class-name (vm-classify-arg first-arg state)))
    (or (car (%vm-gf-eql-methods gf-ht first-arg))
        (block eql-single
          (maphash (lambda (key method)
                     (when (%eql-specializer-matches-p key first-arg)
                       (return-from eql-single method)))
                   methods-ht)
          nil)
        (gethash class-name methods-ht)
        (let ((class-ht (gethash class-name (vm-class-registry state))))
          (when class-ht
            (loop for ancestor in (cdr (gethash :__cpl__ class-ht))
                  for m = (gethash ancestor methods-ht)
                  when m return m)))
        (gethash t methods-ht))))

(defun %vm-resolve-composite-dispatch (gf-ht methods-ht state first-arg all-args)
  "Resolve a method by multi-dispatch on the full arg class tuple."
  (let ((arg-classes (mapcar (lambda (arg) (vm-classify-arg arg state)) all-args)))
    (or (gethash arg-classes methods-ht)
        (car (%vm-gf-eql-methods gf-ht first-arg))
        (block eql-scan
          (maphash (lambda (key method)
                     (when (and (listp key) (= (length key) (length all-args))
                                (every (lambda (spec arg)
                                         (or (eq spec t)
                                             (eq spec (vm-classify-arg arg state))
                                             (%eql-specializer-matches-p spec arg)))
                                       key all-args))
                       (return-from eql-scan method)))
                   methods-ht)
          nil)
        (vm-resolve-multi-dispatch methods-ht state arg-classes)
        (gethash (make-list (length arg-classes) :initial-element t) methods-ht))))

(defun vm-resolve-gf-method (gf-ht state first-arg &optional all-args)
  "Resolve the applicable method closure for GF-HT.
Dispatches via composite keys when ALL-ARGS is provided and the table uses
list keys; otherwise falls back to single dispatch on FIRST-ARG."
  (let* ((methods-ht (gethash :__methods__ gf-ht))
         (composite-p (and all-args (%vm-gf-uses-composite-keys-p methods-ht))))
    (if composite-p
        (let* ((arg-classes (mapcar (lambda (a) (vm-classify-arg a state)) all-args))
               (method-closure (%vm-resolve-composite-dispatch gf-ht methods-ht state first-arg all-args)))
          (unless method-closure
            (error "No applicable method for generic function ~S on classes ~S"
                   (gethash :__name__ gf-ht) arg-classes))
          method-closure)
        (let* ((class-name (vm-classify-arg first-arg state))
               (method-closure (%vm-resolve-single-dispatch gf-ht methods-ht state first-arg)))
          (unless method-closure
            (error "No applicable method for generic function ~S on class ~S"
                   (gethash :__name__ gf-ht) class-name))
          method-closure))))

(defun vm-resolve-multi-dispatch (methods-ht state arg-classes)
  "Try to find a method by substituting ancestor classes in each position.
Uses class precedence lists for inheritance-based fallback."
  (let ((cpls (mapcar (lambda (class-name)
                        (let* ((class-ht (gethash class-name (vm-class-registry state)))
                               (cpl (if class-ht
                                        (gethash :__cpl__ class-ht)
                                        (list class-name))))
                          (if (member t cpl)
                              cpl
                              (append cpl (list t)))))
                      arg-classes)))
    (vm-try-dispatch-combinations methods-ht cpls (length arg-classes))))

(defun vm-try-dispatch-combinations (methods-ht cpls n)
  "Try dispatch key combinations from CPLs, most-specific first."
  (when (= n 0)
    (return-from vm-try-dispatch-combinations (gethash nil methods-ht)))
  (let ((first-cpl (car cpls))
        (rest-cpls (cdr cpls)))
    (dolist (class first-cpl)
      (if (= n 1)
          (let* ((m-list (gethash (list class) methods-ht))
                  (m-symbol (gethash class methods-ht))
                  (m (or m-list m-symbol)))
             (when m (return-from vm-try-dispatch-combinations m)))
          (let ((sub-result (vm-try-dispatch-sub methods-ht rest-cpls (list class))))
            (when sub-result
              (return-from vm-try-dispatch-combinations sub-result))))))
  nil)

(defun vm-try-dispatch-sub (methods-ht cpls prefix)
  "Recursive helper for multi-dispatch combination search."
  (if (null cpls)
      (gethash prefix methods-ht)
      (let ((first-cpl (car cpls))
            (rest-cpls (cdr cpls)))
        (dolist (class first-cpl)
          (let ((result (vm-try-dispatch-sub methods-ht rest-cpls (append prefix (list class)))))
            (when result
              (return-from vm-try-dispatch-sub result))))
        nil)))
