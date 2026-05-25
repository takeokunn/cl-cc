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
Short-form combinations: call all methods, fold results with operator.
Long-form combinations (registered with :long-form tag): pass method descriptors
and args to the combiner function."
  (let* ((all-arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
         (qual-key (intern (format nil "__~A__" (string-upcase (string combination))) :keyword))
         (combo-methods (%collect-combo-methods gf-ht qual-key state all-arg-values))
         (primary-methods (vm-get-all-applicable-methods gf-ht state all-arg-values))
         (methods (or combo-methods primary-methods))
         (op-entry (%resolve-combination-operator-entry combination)))
    (if (null methods)
        (error "No applicable methods for ~S with combination ~S" gf-ht combination)
        (if (and *vm-exec-flat* (not (eq (car op-entry) :long-form)))
            ;; Short form: call methods synchronously and fold
            (let ((results (mapcar (lambda (m)
                                     (%vm-call-closure-sync (%vm-method-function m) state all-arg-values))
                                   methods)))
              (vm-reg-set state dst-reg (funcall (cdr op-entry) results))
              (values (1+ pc) nil nil))
            ;; Long form or no execution context: pass methods+args to combiner
            (let* ((combiner (cdr op-entry))
                   (result (if (eq (car op-entry) :long-form)
                               ;; Long form: combiners receive (methods args) and handle dispatch internally
                               (funcall combiner methods all-arg-values)
                               ;; Short form fallback: call first method
                               (let ((method-closure (%vm-method-function (car methods))))
                                 (vm-push-call-frame state (1+ pc) dst-reg)
                                 (push (list gf-ht methods all-arg-values) (vm-method-call-stack state))
                                 (vm-profile-enter-call state (vm-closure-entry-label method-closure))
                                 (vm-bind-closure-args method-closure state all-arg-values)
                                 (values (vm-label-table-lookup labels
                                         (vm-closure-entry-label method-closure)) nil nil)))))
              (if (eq (car op-entry) :long-form)
                  (progn (vm-reg-set state dst-reg result) (values (1+ pc) nil nil))
                  result))))))

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
         (method (or (car all-methods)
                     (vm-resolve-gf-method gf-ht state
                                           (car all-arg-values) all-arg-values)))
         (method-closure (%vm-method-function method))
         ;; Look up qualified methods
         (before-methods (%lookup-qualified-methods gf-ht :__BEFORE__ state all-arg-values))
         (after-methods  (%lookup-qualified-methods gf-ht :__AFTER__ state all-arg-values))
         (around-methods (%lookup-qualified-methods gf-ht :__AROUND__ state all-arg-values))
         (combination-plan (list :gf gf-ht
                                 :args all-arg-values
                                 :around around-methods
                                 :before before-methods
                                 :primary all-methods
                                 :after after-methods)))
    (if (and (null before-methods) (null after-methods) (null around-methods))
        ;; Fast path: no qualified methods — original behavior
        (progn
          (vm-push-call-frame state (1+ pc) dst-reg)
          (push combination-plan (vm-clos-shadow-stack state))
          (push (list gf-ht all-methods all-arg-values :combination combination-plan)
                (vm-method-call-stack state))
          (vm-profile-enter-call state (vm-closure-entry-label method-closure))
          (vm-bind-closure-args method-closure state all-arg-values)
          (values (vm-label-table-lookup labels (vm-closure-entry-label method-closure)) nil nil))
        ;; Standard method combination with around/before/after
        (let* ((has-around (not (null around-methods)))
               (has-before (not (null before-methods)))
               ;; If around exists, call it first; otherwise start with before or primary
                (first-method (cond (has-around (car around-methods))
                                    (has-before (car before-methods))
                                    (t method-closure)))
                (first-closure (%vm-method-function first-method)))
          (vm-push-call-frame state (1+ pc) dst-reg)
          (push combination-plan (vm-clos-shadow-stack state))
          (push (list gf-ht all-methods all-arg-values
                      :qualified t
                      :around-pending (if has-around (cdr around-methods) nil)
                      :before-pending (if (and (not has-around) has-before)
                                          (cdr before-methods) nil)
                      :primary (cond (has-around method-closure)
                                     (has-before method-closure)
                                     (t nil))
                      :after-pending after-methods
                      :arg-values all-arg-values
                      :before-methods before-methods
                      :phase (cond (has-around :around)
                                   (has-before :before)
                                   (t :primary)))
                 (vm-method-call-stack state))
          (vm-profile-enter-call state (vm-closure-entry-label first-closure))
          (vm-bind-closure-args first-closure state all-arg-values)
           (values (vm-label-table-lookup labels (vm-closure-entry-label first-closure)) nil nil)))))

(defun %vm-defunctionalized-dispatch (state closure)
  "Resolve CLOSURE through a lightweight defunctionalized dispatch tag.

This keeps an explicit tagged-dispatch step for known global closures while
preserving existing closure semantics."
  (let ((tag (and (typep closure 'vm-closure-object)
                  (vm-closure-dispatch-tag closure))))
    (if (null tag)
        closure
        (ecase (car tag)
          (:known-function
           (or (gethash (cdr tag) (vm-function-registry state))
               closure))
          (:anonymous
           closure)))))

(defun %vm-dispatch-call (func state pc labels arg-regs dst-reg tail-p &optional live-regs)
  "Shared call dispatch for vm-call and vm-tail-call.
   TAIL-P suppresses frame push for TCO: the current frame's return address
   is reused, keeping the call stack O(1) for tail-recursive functions."
  (cond
    ;; Full and delimited continuations are callable VM objects. Invoking one
    ;; restores the heap-copied interpreter stack snapshot and resumes at the
    ;; saved PC. The object is reusable, so call/cc supports multi-shot use.
    ((vm-continuation-p func)
     (let ((value (if arg-regs (vm-reg-get state (first arg-regs)) nil)))
       (values (vm-invoke-continuation state func value) nil nil)))
    ;; Generic function: delegate to multi-dispatch resolver (always non-tail for safety)
    ((vm-generic-function-p func)
     (vm-dispatch-generic-call func state pc arg-regs dst-reg labels))
    ;; Host CL function (whitelist bridge) — apply directly, no frame ops needed
     ((functionp func)
        (let* ((*vm-current-state* state)
               (*vm-current-pc* (1+ pc))
               (*vm-current-dst-reg* dst-reg)
               (*vm-current-labels* labels)
               (all-values (multiple-value-list
                            (apply func (mapcar (lambda (r) (vm-reg-get state r)) arg-regs)))))
         (setf (vm-values-list state) all-values)
         (vm-reg-set state dst-reg (if all-values (first all-values) nil)))
       (values (1+ pc) nil nil))
    ;; Normal closure — optionally push frame (skipped for TCO), bind args, jump
    (t
      (let* ((resolved-func (%vm-defunctionalized-dispatch state func))
              (arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs)))
        (vm-closure-note-invocation resolved-func)
        (vm-maybe-tier-upgrade-closure resolved-func)
        (if (and (vm-closure-program-flat resolved-func)
                 (vm-closure-label-table resolved-func)
                 (or (not *vm-exec-flat*)
                     (not (eq (vm-closure-program-flat resolved-func) *vm-exec-flat*))
                     (not (eq (vm-closure-label-table resolved-func) *vm-exec-labels*))))
            (progn
              (vm-reg-set state dst-reg (%vm-call-closure-sync resolved-func state arg-values))
              (values (1+ pc) nil nil))
            (progn
              (unless (and tail-p (vm-tail-call-optimization-enabled-p state))
                (vm-push-call-frame state (1+ pc) dst-reg live-regs)
                (push nil (vm-method-call-stack state)))
              (vm-store-multiple-values state nil)
              (vm-profile-enter-call state (vm-closure-entry-label resolved-func) :tail-p tail-p)
              (vm-bind-closure-args resolved-func state arg-values)
              (values (vm-label-table-lookup labels (vm-closure-entry-label resolved-func)) nil nil)))))))
