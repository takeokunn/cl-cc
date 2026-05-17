(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — execute-instruction for Multiple Values, Apply, and Globals
;;;
;;; Contains: %vm-apply-spread-args, execute-instruction methods for vm-values,
;;; vm-mv-bind, vm-values-to-list, vm-spread-values, vm-clear-values,
;;; vm-ensure-values, vm-next-method-p, vm-call-next-method, vm-apply,
;;; vm-register-function, vm-set-global, vm-get-global; and
;;; vm-resolve-function, vm-save/restore-registers, vm-push-call-frame,
;;; vm-bind-closure-args, vm-list-to-lisp-list.
;;;
;;; Core instruction execution (vm-const through vm-closure-ref-idx) is in
;;; vm-execute.lisp (loads before this file).
;;;
;;; Load order: after vm-execute.lisp, before vm-clos.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Multiple Values and Apply ────────────────────────────────────────────

(defun %vm-apply-spread-args (state arg-values)
  "Return APPLIED arguments with the last VM argument spliced as a list."
  (cond
    ((null arg-values) nil)
    ((null (cdr arg-values))
     (vm-list-to-lisp-list state (car arg-values)))
    (t
     (let ((prefix nil)
           (rest arg-values))
       (loop while (cdr rest) do
         (push (car rest) prefix)
         (setf rest (cdr rest)))
       (nconc (nreverse prefix)
              (vm-list-to-lisp-list state (car rest)))))))

(defmethod execute-instruction ((inst vm-values) state pc labels)
  (declare (ignore labels))
  (let* ((src-regs (vm-src-regs inst))
         (all-values (mapcar (lambda (reg) (vm-reg-get state reg)) src-regs)))
    ;; Store all values in state
    (setf (vm-values-list state) all-values)
    ;; Primary value goes to dst
    (vm-reg-set state (vm-dst inst) (if all-values (first all-values) nil))
    (values (1+ pc) nil nil)))

(defun %vm-values-type-parts (type-spec)
  "Split a VALUES type specifier into required, optional, and rest parts."
  (let ((required nil) (optional nil) (rest-type nil) (state :required) (tail (cdr type-spec)))
    (loop while tail do
      (let ((entry (car tail)))
        (setf tail (cdr tail))
        (cond
          ((eq entry '&optional) (setf state :optional))
          ((eq entry '&rest)
           (setf state :rest)
           (when tail (setf rest-type (car tail) tail (cdr tail))))
          ((eq entry '&allow-other-keys))
          ((eq state :required) (push entry required))
          ((eq state :optional) (push entry optional)))))
    (values (nreverse required) (nreverse optional) rest-type)))

(defun vm-values-typep-check (values-list type-spec)
  "Return T when VALUES-LIST satisfies a VALUES type specifier."
  (unless (and (consp type-spec) (eq (car type-spec) 'values))
    (return-from vm-values-typep-check t))
  (multiple-value-bind (required optional rest-type)
      (%vm-values-type-parts type-spec)
    (let ((vals values-list))
      ;; Required: must have at least this many values, each type-matching
      (loop for type in required
            do (unless vals (return-from vm-values-typep-check nil))
               (unless (vm-typep-check (pop vals) type)
                 (return-from vm-values-typep-check nil)))
      ;; Optional: consume matching values while available
      (loop for type in optional
            while vals
            do (unless (vm-typep-check (pop vals) type)
                 (return-from vm-values-typep-check nil)))
      ;; Rest: check remaining values against rest type, or require none remain
      (if rest-type
          (every (lambda (v) (vm-typep-check v rest-type)) vals)
          (null vals)))))

(defmethod execute-instruction ((inst vm-values-typep) state pc labels)
  (declare (ignore labels))
  (let* ((values-list (or (vm-values-list state)
                          (list (vm-reg-get state (vm-src inst)))))
         (result (if (vm-values-typep-check values-list (vm-type-name inst)) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-mv-bind) state pc labels)
  (declare (ignore labels))
  (let ((vals (vm-values-list state))
        (dst-regs (vm-dst-regs inst)))
    (loop for reg in dst-regs
          for i from 0
          do (vm-reg-set state reg (if (< i (length vals))
                                       (nth i vals)
                                       nil)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-values-to-list) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (copy-list (vm-values-list state)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-spread-values) state pc labels)
  (declare (ignore labels))
  (let ((lst (vm-reg-get state (vm-src inst))))
    (setf (vm-values-list state) (if (listp lst) lst (list lst)))
    (vm-reg-set state (vm-dst inst) (if (listp lst) (first lst) lst))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-clear-values) state pc labels)
  (declare (ignore labels))
  (setf (vm-values-list state) nil)
  (values (1+ pc) nil nil))

;;; FR-073: Multiple values via registers (≤3)
(defmethod execute-instruction ((inst vm-values-regs) state pc labels)
  "Store up to 3 values in the VM values buffer without heap allocation.
   The native codegen maps these to physical return registers (RAX/RDX/RCX)."
  (declare (ignore labels))
  (let ((vals (list (vm-reg-get state (vm-vr0 inst)))))
    (when (>= (vm-vr-count inst) 2)
      (push (vm-reg-get state (vm-vr1 inst)) vals))
    (when (>= (vm-vr-count inst) 3)
      (push (vm-reg-get state (vm-vr2 inst)) vals))
    (setf (vm-values-list state) (nreverse vals)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-ensure-values) state pc labels)
  (declare (ignore labels))
  (unless (vm-values-list state)
    (setf (vm-values-list state) (list (vm-reg-get state (vm-src inst)))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-next-method-p) state pc labels)
  (declare (ignore labels))
  ;; Look for the innermost method context in method-call-stack
  (let ((ctx (car (vm-method-call-stack state))))
    (vm-reg-set state (vm-dst inst)
                (if (and ctx (cddr ctx) (cadr ctx) (cdadr ctx))
                    t
                    nil)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-call-next-method) state pc labels)
  ;; Find current method context (top of method-call-stack)
  (let ((ctx (car (vm-method-call-stack state))))
    (unless ctx
      (error "call-next-method called outside of a generic function method"))
    ;; Check if we're in an :around method (qualified dispatch)
    (let ((props (when (and (listp ctx) (> (length ctx) 3)) (cdddr ctx))))
      (if (and props (eq (getf props :phase) :around))
          ;; Around method's call-next-method: transition to before→primary→after chain
          (let* ((arg-values (getf props :arg-values))
                 (before-methods (getf props :before-methods))
                 (primary (getf props :primary))
                 (has-before (not (null before-methods)))
                 (first-method (if has-before (car before-methods) primary)))
            (unless first-method
              (error "no-next-method: no primary method available"))
            ;; Save around context so we can return to it
            (setf (getf (cdddr ctx) :around-return-phase) t)
            (setf (getf (cdddr ctx) :around-return-pc) (1+ pc))
            (setf (getf (cdddr ctx) :around-cnm-dst) (vm-dst inst))
            ;; Transition phase to before or primary
            (setf (getf (cdddr ctx) :phase) (if has-before :before :primary))
            (setf (getf (cdddr ctx) :before-pending)
                  (if has-before (cdr before-methods) nil))
            (when has-before
              (setf (getf (cdddr ctx) :primary) primary))
             (vm-bind-closure-args first-method state arg-values)
             (values (vm-label-table-lookup labels (vm-closure-entry-label first-method)) nil nil))
          ;; Normal call-next-method (non-around)
          (let* ((gf-ht (first ctx))
                 (methods-list (second ctx))
                 (orig-args (third ctx))
                 (next-method (cadr methods-list)))
            (unless next-method
              (error "no-next-method: There is no next method for the generic function ~S when called with arguments ~S"
                     (gethash :__name__ gf-ht) orig-args))
            (let* ((call-args (if (vm-cnm-args-reg inst)
                                  (vm-reg-get state (vm-cnm-args-reg inst))
                                  orig-args)))
              (vm-push-call-frame state (1+ pc) (vm-dst inst))
              (push (list gf-ht (cdr methods-list) call-args) (vm-method-call-stack state))
               (vm-bind-closure-args next-method state call-args)
               (values (vm-label-table-lookup labels (vm-closure-entry-label next-method)) nil nil)))))))

(defmethod execute-instruction ((inst vm-apply) state pc labels)
  (let* ((func      (vm-resolve-function state (vm-reg-get state (vm-func-reg inst))))
         (arg-regs  (vm-args inst))
         (dst-reg   (vm-dst inst))
         ;; Spread the last argument: (apply fn a b list) → args are (a b . list)
         (arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
         (spread-args (%vm-apply-spread-args state arg-values)))
    (cond
      ;; Host CL function — apply directly, no frame push
      ((functionp func)
       (let ((all-values (multiple-value-list (apply func spread-args))))
         (setf (vm-values-list state) all-values)
         (vm-reg-set state dst-reg (if all-values (first all-values) nil)))
       (values (1+ pc) nil nil))
      ;; Generic function or closure
      (t
       (let ((closure (if (vm-generic-function-p func)
                          (vm-resolve-gf-method func state (car spread-args) spread-args)
                          func)))
         (vm-push-call-frame state (1+ pc) dst-reg)
         (push nil (vm-method-call-stack state))
         (vm-bind-closure-args closure state spread-args)
         (values (vm-label-table-lookup labels (vm-closure-entry-label closure)) nil nil))))))

(defmethod execute-instruction ((inst vm-register-function) state pc labels)
  (let ((name (vm-func-name inst))
         (closure (vm-reg-get state (vm-src inst))))
    (when (typep closure 'vm-closure-object)
      (unless (vm-closure-program-flat closure)
        (setf (vm-closure-program-flat closure) *vm-exec-flat*))
      (unless (vm-closure-label-table closure)
        (setf (vm-closure-label-table closure) labels))
      (setf (vm-closure-dispatch-tag closure) (cons :known-function name)))
    (setf (gethash name (vm-function-registry state)) closure)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-set-global) state pc labels)
  (declare (ignore labels))
  (let ((name (vm-global-name inst))
        (value (vm-reg-get state (vm-src inst))))
    (setf (gethash name (vm-global-vars state)) value)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-get-global) state pc labels)
  (declare (ignore labels))
  (let ((name (vm-global-name inst)))
    (multiple-value-bind (value found-p) (gethash name (vm-global-vars state))
      (unless found-p
        (error "Unbound global variable: ~S" name))
      (vm-reg-set state (vm-dst inst) value)
      (values (1+ pc) nil nil))))
