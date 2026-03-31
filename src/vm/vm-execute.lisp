(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — execute-instruction Methods for Core Instructions
;;;
;;; Contains: vm-resolve-function, vm-falsep, call-frame helpers
;;; (vm-save/restore-registers, vm-push-call-frame, vm-bind-closure-args),
;;; vm-list-to-lisp-list, multi-dispatch helpers (vm-classify-arg,
;;; vm-resolve-gf-method, vm-resolve-multi-dispatch, vm-get-all-applicable-methods,
;;; vm-dispatch-generic-call, %vm-dispatch-call),
;;; and execute-instruction methods for: const, move, add/sub/mul, label,
;;; jump, jump-zero, print, halt, closure, call, tail-call, ret, func-ref,
;;; make-closure, closure-ref-idx, values, mv-bind, values-to-list,
;;; spread-values, clear-values, ensure-values, next-method-p,
;;; call-next-method, apply, register-function, set-global, get-global.
;;;
;;; Load order: after vm-dispatch.lisp, before vm-clos.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defmethod execute-instruction ((inst vm-const) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (vm-value inst))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-move) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (vm-reg-get state (vm-src inst)))
  (values (1+ pc) nil nil))

(define-simple-instruction vm-add :binary +)
(define-simple-instruction vm-sub :binary -)
(define-simple-instruction vm-mul :binary *)

(defmethod execute-instruction ((inst vm-label) state pc labels)
  (declare (ignore state labels))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-jump) state pc labels)
  (declare (ignore state pc))
  (values (gethash (vm-label-name inst) labels) nil nil))

(defun vm-falsep (value)
  "Return T if VALUE is falsy (nil, 0, or false)."
  (or (null value) (eql value 0)))

(defmethod execute-instruction ((inst vm-jump-zero) state pc labels)
  (if (vm-falsep (vm-reg-get state (vm-reg inst)))
      (values (gethash (vm-label-name inst) labels) nil nil)
      (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-print) state pc labels)
  (declare (ignore labels))
  (format (vm-output-stream state) "~A~%" (vm-reg-get state (vm-reg inst)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-halt) state pc labels)
  (declare (ignore pc labels))
  (values nil t (vm-reg-get state (vm-reg inst))))

(defmethod execute-instruction ((inst vm-closure) state pc labels)
  (declare (ignore labels))
  ;; captured is a list of (symbol . register) pairs from the compiler
  ;; We store values keyed by REGISTER name so vm-call restores to the correct register
  (let* ((dst-reg (vm-dst inst))
         (captured-vals (mapcar (lambda (binding)
                                  (cons (cdr binding) (vm-reg-get state (cdr binding))))
                                (vm-captured-vars inst)))
         (closure (make-instance 'vm-closure-object
                                 :entry-label (vm-label-name inst)
                                 :params (vm-closure-params inst)
                                 :optional-params (vm-closure-optional-params inst)
                                 :rest-param (vm-closure-rest-param inst)
                                 :key-params (vm-closure-key-params inst)
                                 :captured-values captured-vals)))
    (vm-reg-set state dst-reg closure)
    ;; Fix self-references for recursive labels: if a captured register
    ;; is the same as dst-reg, the closure captures itself (not yet created at lookup time)
    (dolist (cv-pair captured-vals)
      (when (eql (car cv-pair) dst-reg)
        (setf (cdr cv-pair) closure)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-call) state pc labels)
  (let ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst)))))
    (%vm-dispatch-call func state pc labels (vm-args inst) (vm-dst inst) nil)))

(defmethod execute-instruction ((inst vm-tail-call) state pc labels)
  (let ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst)))))
    (%vm-dispatch-call func state pc labels (vm-args inst) (vm-dst inst) t)))

(defmethod execute-instruction ((inst vm-ret) state pc labels)
  (declare (ignore pc))
  (let ((result (vm-reg-get state (vm-reg inst))))
    ;; Check for qualified method dispatch (standard method combination)
    (let ((method-entry (car (vm-method-call-stack state))))
      (when (and method-entry (listp method-entry)
                 (getf (cdddr method-entry) :qualified))
        ;; We're returning from a qualified method — continue the chain
        (return-from execute-instruction
          (%vm-ret-qualified-dispatch state result labels method-entry))))
    ;; Normal return path
    (if (vm-call-stack state)
        (destructuring-bind (return-pc dst-reg old-closure-env saved-regs)
            (pop (vm-call-stack state))
          ;; Pop method-call-stack in sync with call-stack
          (when (vm-method-call-stack state)
            (pop (vm-method-call-stack state)))
          (vm-restore-registers state saved-regs)
          ;; Write the return value into the destination register
          (vm-reg-set state dst-reg result)
          (when old-closure-env
            (setf (vm-closure-env state) old-closure-env))
          (values return-pc nil nil))
        (values nil t result))))

(defmethod execute-instruction ((inst vm-func-ref) state pc labels)
  (declare (ignore labels))
  ;; Try the function registry first — user-defined functions (defun) register
  ;; closures with proper entry-label, params, and captured-values.
  ;; Fall back to a bare closure for local labels in the same compilation unit.
  (let* ((label-str (vm-label-name inst))
         (sym (find-symbol label-str :cl-cc))
         (registered (when sym (gethash sym (vm-function-registry state)))))
    (vm-reg-set state (vm-dst inst)
                (or registered
                    (make-instance 'vm-closure-object
                                   :entry-label label-str
                                   :params nil
                                   :captured-values nil))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-make-closure) state pc labels)
  (declare (ignore labels))
  (let* ((captured-values (mapcar (lambda (reg) (vm-reg-get state reg)) (vm-env-regs inst)))
         (closure (make-instance 'vm-closure-object
                                 :entry-label (vm-label-name inst)
                                 :params (vm-make-closure-params inst)
                                 :captured-values captured-values))
         (addr (vm-heap-alloc state closure)))
    (vm-reg-set state (vm-dst inst) addr)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-closure-ref-idx) state pc labels)
  (declare (ignore labels))
  (let* ((addr (vm-reg-get state (vm-closure-reg inst)))
         (closure (vm-heap-get state addr))
         (idx (vm-closure-index inst))
         (values-list (vm-closure-captured-values closure)))
    (when (>= idx (length values-list))
      (error "Closure ref index ~D out of bounds (captured ~D values)" idx (length values-list)))
    (vm-reg-set state (vm-dst inst) (nth idx values-list))
    (values (1+ pc) nil nil)))

;;; ── Multiple Values and Apply ────────────────────────────────────────────

(defmethod execute-instruction ((inst vm-values) state pc labels)
  (declare (ignore labels))
  (let* ((src-regs (vm-src-regs inst))
         (all-values (mapcar (lambda (reg) (vm-reg-get state reg)) src-regs)))
    ;; Store all values in state
    (setf (vm-values-list state) all-values)
    ;; Primary value goes to dst
    (vm-reg-set state (vm-dst inst) (if all-values (first all-values) nil))
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
  (declare (ignore labels inst))
  (setf (vm-values-list state) nil)
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
            (values (gethash (vm-closure-entry-label first-method) labels) nil nil))
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
              (values (gethash (vm-closure-entry-label next-method) labels) nil nil)))))))

(defmethod execute-instruction ((inst vm-apply) state pc labels)
  (let* ((func      (vm-resolve-function state (vm-reg-get state (vm-func-reg inst))))
         (arg-regs  (vm-args inst))
         (dst-reg   (vm-dst inst))
         ;; Spread the last argument: (apply fn a b list) → args are (a b . list)
         (arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
         (spread-args (if arg-values
                          (append (butlast arg-values)
                                  (vm-list-to-lisp-list state (car (last arg-values))))
                          nil)))
    (cond
      ;; Host CL function — apply directly, no frame push
      ((functionp func)
       (vm-reg-set state dst-reg (apply func spread-args))
       (values (1+ pc) nil nil))
      ;; Generic function or closure
      (t
       (let ((closure (if (vm-generic-function-p func)
                          (vm-resolve-gf-method func state (car spread-args) spread-args)
                          func)))
         (vm-push-call-frame state (1+ pc) dst-reg)
         (push nil (vm-method-call-stack state))
         (vm-bind-closure-args closure state spread-args)
         (values (gethash (vm-closure-entry-label closure) labels) nil nil))))))

(defmethod execute-instruction ((inst vm-apply) state pc labels)
  (let* ((func      (vm-resolve-function state (vm-reg-get state (vm-func-reg inst))))
         (arg-regs  (vm-args inst))
         (dst-reg   (vm-dst inst))
         ;; Spread the last argument: (apply fn a b list) → args are (a b . list)
         (arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
         (spread-args (if arg-values
                          (append (butlast arg-values)
                                  (vm-list-to-lisp-list state (car (last arg-values))))
                          nil)))
    (cond
      ;; Host CL function — apply directly, no frame push
      ((functionp func)
       (vm-reg-set state dst-reg (apply func spread-args))
       (values (1+ pc) nil nil))
      ;; Generic function or closure
      (t
       (let ((closure (if (vm-generic-function-p func)
                          (vm-resolve-gf-method func state (car spread-args) spread-args)
                          func)))
         (vm-push-call-frame state (1+ pc) dst-reg)
         (push nil (vm-method-call-stack state))
         (vm-bind-closure-args closure state spread-args)
         (values (gethash (vm-closure-entry-label closure) labels) nil nil))))))

(defmethod execute-instruction ((inst vm-register-function) state pc labels)
  (declare (ignore labels))
  (let ((name (vm-func-name inst))
        (closure (vm-reg-get state (vm-src inst))))
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
