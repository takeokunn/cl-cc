;;;; tests/unit/vm/conditions-tests.lisp — VM Condition System Unit Tests
;;;;
;;;; Tests for handler stack management, restart bindings,
;;;; condition construction, and signal dispatch.

(in-package :cl-cc/test)

(defsuite conditions-suite :description "VM condition system unit tests")

;;; ─── Condition Construction ───────────────────────────────────────────────

(deftest make-vm-type-error-slots
  "make-vm-type-error creates condition with correct slots."
  (let* ((state (make-instance 'cl-cc::vm-state))
         (cond (cl-cc::make-vm-type-error state 'fixnum "hello")))
    (assert-true (typep cond 'cl-cc::vm-type-error))
    (assert-equal 'fixnum (cl-cc::vm-expected-type cond))
    (assert-equal "hello" (cl-cc::vm-datum cond))))

(deftest make-vm-unbound-variable-slots
  "make-vm-unbound-variable creates condition with variable name."
  (let* ((state (make-instance 'cl-cc::vm-state))
         (cond (cl-cc::make-vm-unbound-variable state 'x)))
    (assert-true (typep cond 'cl-cc::vm-unbound-variable))
    (assert-equal 'x (cl-cc::vm-variable-name cond))))

(deftest make-vm-undefined-function-slots
  "make-vm-undefined-function creates condition with function name."
  (let* ((state (make-instance 'cl-cc::vm-state))
         (cond (cl-cc::make-vm-undefined-function state 'foo)))
    (assert-true (typep cond 'cl-cc::vm-undefined-function))
    (assert-equal 'foo (cl-cc::vm-function-name cond))))

(deftest make-vm-division-by-zero-slots
  "make-vm-division-by-zero creates condition with dividend."
  (let* ((state (make-instance 'cl-cc::vm-state))
         (cond (cl-cc::make-vm-division-by-zero state 42)))
    (assert-true (typep cond 'cl-cc::vm-division-by-zero))
    (assert-equal 42 (cl-cc::vm-dividend cond))))

;;; ─── Condition Hierarchy ──────────────────────────────────────────────────

(deftest condition-type-error-is-vm-error
  "vm-type-error inherits from vm-error."
  (let* ((state (make-instance 'cl-cc::vm-state))
         (cond (cl-cc::make-vm-type-error state 'fixnum 42)))
    (assert-true (typep cond 'cl-cc::vm-error))))

(deftest condition-unbound-var-is-vm-error
  "vm-unbound-variable inherits from vm-error."
  (let* ((state (make-instance 'cl-cc::vm-state))
         (cond (cl-cc::make-vm-unbound-variable state 'x)))
    (assert-true (typep cond 'cl-cc::vm-error))))

(deftest condition-division-by-zero-is-vm-error
  "vm-division-by-zero inherits from vm-error."
  (let* ((state (make-instance 'cl-cc::vm-state))
         (cond (cl-cc::make-vm-division-by-zero state 42)))
    (assert-true (typep cond 'cl-cc::vm-error))))

;;; ─── Condition Report ─────────────────────────────────────────────────────

(deftest type-error-is-printable
  "vm-type-error can be printed without error."
  (let* ((state (make-instance 'cl-cc::vm-state))
         (cond (cl-cc::make-vm-type-error state 'fixnum "hello"))
         (msg (format nil "~A" cond)))
    (assert-true (stringp msg))))

;;; ─── Handler Stack ────────────────────────────────────────────────────────

(deftest handler-stack-initially-empty
  "Fresh vm-state has empty handler stack."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (assert-equal nil (cl-cc::vm-get-handler-stack state))))

(deftest push-pop-handler
  "Push and pop handler round-trips correctly."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (cl-cc::vm-push-handler-to-stack state 'cl-cc::vm-error #'identity)
    (let ((handler (cl-cc::vm-pop-handler-from-stack state)))
      (assert-true (not (null handler)))
      (assert-equal 'cl-cc::vm-error (cl-cc::vm-handler-type handler)))))

(deftest pop-empty-stack-returns-nil
  "Popping empty handler stack returns nil."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (assert-equal nil (cl-cc::vm-pop-handler-from-stack state))))

(deftest push-multiple-handlers-lifo
  "Multiple pushed handlers are popped in LIFO order."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (cl-cc::vm-push-handler-to-stack state 'cl-cc::vm-error #'identity)
    (cl-cc::vm-push-handler-to-stack state 'cl-cc::vm-warning #'identity)
    (let ((first (cl-cc::vm-pop-handler-from-stack state)))
      (assert-equal 'cl-cc::vm-warning (cl-cc::vm-handler-type first)))
    (let ((second (cl-cc::vm-pop-handler-from-stack state)))
      (assert-equal 'cl-cc::vm-error (cl-cc::vm-handler-type second)))))

(deftest find-handler-matching
  "vm-find-handler finds handler matching condition type."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (cl-cc::vm-push-handler-to-stack state 'cl-cc::vm-error #'identity)
    (let* ((cond (cl-cc::make-vm-type-error state 'fixnum 42))
           (handler (cl-cc::vm-find-handler state cond)))
      (assert-true (not (null handler))))))

(deftest find-handler-no-match
  "vm-find-handler returns nil when no handler matches."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (cl-cc::vm-push-handler-to-stack state 'cl-cc::vm-warning #'identity)
    (let* ((cond (cl-cc::make-vm-type-error state 'fixnum 42))
           (handler (cl-cc::vm-find-handler state cond)))
      (assert-equal nil handler))))

;;; ─── Restart Bindings ─────────────────────────────────────────────────────

(deftest restarts-initially-nil
  "Fresh vm-state has no restarts."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (assert-equal nil (cl-cc::vm-get-restarts state))))

(deftest add-and-find-restart
  "vm-add-restart makes restart findable by name."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (cl-cc::vm-add-restart state 'continue #'identity)
    (let ((restart (cl-cc::vm-find-restart state 'continue)))
      (assert-true (not (null restart)))
      (assert-equal 'continue (cl-cc::vm-restart-name restart)))))

(deftest find-restart-not-found
  "vm-find-restart returns nil for unknown name."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (assert-equal nil (cl-cc::vm-find-restart state 'nonexistent))))

;;; ─── Signal Dispatch ──────────────────────────────────────────────────────

(deftest signal-condition-with-handler
  "vm-signal-condition returns t when handler exists."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (cl-cc::vm-push-handler-to-stack state 'cl-cc::vm-error #'identity)
    (let ((cond (cl-cc::make-vm-type-error state 'fixnum 42)))
      (multiple-value-bind (found handler)
          (cl-cc::vm-signal-condition cond state)
        (assert-true found)
        (assert-true (not (null handler)))))))

(deftest signal-condition-no-handler
  "vm-signal-condition returns nil when no handler."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (let ((cond (cl-cc::make-vm-type-error state 'fixnum 42)))
      (multiple-value-bind (found handler)
          (cl-cc::vm-signal-condition cond state)
        (assert-equal nil found)
        (assert-equal nil handler)))))

(deftest signal-condition-error-p-signals
  "vm-signal-condition with :error-p t signals when no handler."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (let ((cond (cl-cc::make-vm-type-error state 'fixnum 42)))
      (assert-true
       (handler-case
           (progn (cl-cc::vm-signal-condition cond state :error-p t) nil)
         (cl-cc::vm-type-error () t))))))

;;; ─── Clear Context ────────────────────────────────────────────────────────

(deftest clear-condition-context
  "vm-clear-condition-context removes handlers and restarts."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (cl-cc::vm-push-handler-to-stack state 'cl-cc::vm-error #'identity)
    (cl-cc::vm-add-restart state 'continue #'identity)
    (cl-cc::vm-clear-condition-context state)
    (assert-equal nil (cl-cc::vm-get-handler-stack state))
    (assert-equal nil (cl-cc::vm-get-restarts state))))
