(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Generic Function Call Dispatch
;;;
;;; Contains: %vm-dispatch-custom-combination, vm-dispatch-generic-call,
;;; %vm-dispatch-call.
;;;
;;; Helpers + method lookup + combination data are in vm-dispatch-gf.lisp.
;;; Multi-dispatch resolution (vm-resolve-gf-method) is in
;;; vm-dispatch-gf-multi.lisp (loads before this file).
;;;
;;; Load order: after vm-dispatch-gf-multi.lisp, before vm-execute.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun %vm-dispatch-custom-combination (gf-ht state pc arg-regs dst-reg labels combination)
  "Dispatch a generic function with custom method combination.
COMBINATION is the combination name (e.g. +, LIST, APPEND).
Calls all applicable methods with the matching qualifier synchronously
and folds results using the combination's operator."
  (let* ((all-arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
         (qual-key (intern (format nil "__~A__" (string-upcase (string combination))) :keyword))
         (combo-methods (%collect-combo-methods gf-ht qual-key state (car all-arg-values)))
         ;; Also try primary methods as fallback (some users just define primary methods)
         (primary-methods (vm-get-all-applicable-methods gf-ht state all-arg-values))
         (methods (or combo-methods primary-methods))
         (operator (%resolve-combination-operator combination)))
    (if (null methods)
        (error "No applicable methods for ~S with combination ~S" gf-ht combination)
        (if *vm-exec-flat*
            ;; We have execution context — call each method synchronously
            (let ((results (mapcar (lambda (m)
                                     (%vm-call-closure-sync m state all-arg-values))
                                   methods)))
              (vm-reg-set state dst-reg (apply operator results))
              (values (1+ pc) nil nil))
            ;; No execution context (shouldn't happen in practice) — call first method only
            (let ((method-closure (car methods)))
              (vm-push-call-frame state (1+ pc) dst-reg)
              (push (list gf-ht methods all-arg-values) (vm-method-call-stack state))
              (vm-profile-enter-call state (vm-closure-entry-label method-closure))
              (vm-bind-closure-args method-closure state all-arg-values)
          (values (vm-label-table-lookup labels (vm-closure-entry-label method-closure)) nil nil))))))

(defun vm-dispatch-generic-call (gf-ht state pc arg-regs dst-reg labels)
  "Dispatch a generic function call. GF-HT is the generic function dispatch table.
Supports multiple dispatch by passing all argument values for composite key lookup.
Handles standard method combination: :around → :before → primary → :after.
Custom method combination: calls all qualified methods and folds with operator.
Returns (values next-pc halt-p result) like execute-instruction."
  ;; Check for custom method combination
  (let ((combination (gethash :__method-combination__ gf-ht)))
    (when (and combination (not (eq combination 'standard)))
      (return-from vm-dispatch-generic-call
        (%vm-dispatch-custom-combination gf-ht state pc arg-regs dst-reg labels combination))))
  (let* ((all-arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
         (all-methods    (vm-get-all-applicable-methods gf-ht state all-arg-values))
         (method-closure (or (car all-methods)
                             (vm-resolve-gf-method gf-ht state
                                                   (car all-arg-values) all-arg-values)))
         ;; Look up qualified methods
         (before-methods (%lookup-qualified-methods gf-ht :__BEFORE__ state all-arg-values))
         (after-methods  (%lookup-qualified-methods gf-ht :__AFTER__ state all-arg-values))
         (around-methods (%lookup-qualified-methods gf-ht :__AROUND__ state all-arg-values)))
    (if (and (null before-methods) (null after-methods) (null around-methods))
        ;; Fast path: no qualified methods — original behavior
        (progn
          (vm-push-call-frame state (1+ pc) dst-reg)
          (push (list gf-ht all-methods all-arg-values) (vm-method-call-stack state))
          (vm-profile-enter-call state (vm-closure-entry-label method-closure))
          (vm-bind-closure-args method-closure state all-arg-values)
          (values (vm-label-table-lookup labels (vm-closure-entry-label method-closure)) nil nil))
        ;; Standard method combination with around/before/after
        (let* ((has-around (not (null around-methods)))
               (has-before (not (null before-methods)))
               ;; If around exists, call it first; otherwise start with before or primary
               (first-method (cond (has-around (car around-methods))
                                   (has-before (car before-methods))
                                   (t method-closure))))
          (vm-push-call-frame state (1+ pc) dst-reg)
          (push (list gf-ht all-methods all-arg-values
                      :qualified t
                      ;; Around method tracking
                      :around-pending (if has-around (cdr around-methods) nil)
                      ;; Before/after tracking (deferred until around calls next-method)
                      :before-pending (if (and (not has-around) has-before)
                                          (cdr before-methods) nil)
                      :primary (cond (has-around method-closure)
                                     (has-before method-closure)
                                     (t nil))
                      :after-pending after-methods
                      :arg-values all-arg-values
                      :before-methods before-methods
                      ;; Phase tracking
                      :phase (cond (has-around :around)
                                   (has-before :before)
                                   (t :primary)))
                 (vm-method-call-stack state))
          (vm-profile-enter-call state (vm-closure-entry-label first-method))
          (vm-bind-closure-args first-method state all-arg-values)
          (values (vm-label-table-lookup labels (vm-closure-entry-label first-method)) nil nil)))))

(defun %vm-dispatch-call (func state pc labels arg-regs dst-reg tail-p)
  "Shared call dispatch for vm-call and vm-tail-call.
   TAIL-P suppresses frame push for TCO: the current frame's return address
   is reused, keeping the call stack O(1) for tail-recursive functions."
  (cond
    ;; Generic function: delegate to multi-dispatch resolver (always non-tail for safety)
    ((vm-generic-function-p func)
     (vm-dispatch-generic-call func state pc arg-regs dst-reg labels))
    ;; Host CL function (whitelist bridge) — apply directly, no frame ops needed
    ((functionp func)
     (vm-reg-set state dst-reg
                 (apply func (mapcar (lambda (r) (vm-reg-get state r)) arg-regs)))
     (values (1+ pc) nil nil))
    ;; Normal closure — optionally push frame (skipped for TCO), bind args, jump
    (t
      (let ((arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs)))
        (if (and (vm-closure-program-flat func)
                 (vm-closure-label-table func)
                 (or (not *vm-exec-flat*)
                     (not (eq (vm-closure-program-flat func) *vm-exec-flat*))
                     (not (eq (vm-closure-label-table func) *vm-exec-labels*))))
            (progn
              (vm-reg-set state dst-reg (%vm-call-closure-sync func state arg-values))
              (values (1+ pc) nil nil))
            (progn
              (unless tail-p
                (vm-push-call-frame state (1+ pc) dst-reg)
                (push nil (vm-method-call-stack state)))
              (vm-profile-enter-call state (vm-closure-entry-label func) :tail-p tail-p)
              (vm-bind-closure-args func state arg-values)
              (values (vm-label-table-lookup labels (vm-closure-entry-label func)) nil nil)))))))
