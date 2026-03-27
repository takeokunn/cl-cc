;;;; tests/unit/vm/conditions-tests.lisp — VM Condition System Unit Tests
;;;;
;;;; Tests for handler stack management, restart bindings,
;;;; condition construction, and signal dispatch.

(in-package :cl-cc/test)

(defsuite conditions-suite :description "VM condition system unit tests")

;;; ─── Condition Construction ───────────────────────────────────────────────

(deftest-each vm-condition-constructor-slots
  "Each VM condition constructor stores the correct slots."
  :cases (("type-error"
           (lambda (s) (cl-cc::make-vm-type-error s 'fixnum "hello"))
           'cl-cc::vm-type-error
           (lambda (c) (and (equal 'fixnum (cl-cc::vm-expected-type c))
                            (equal "hello" (cl-cc::vm-datum c)))))
          ("unbound-variable"
           (lambda (s) (cl-cc::make-vm-unbound-variable s 'x))
           'cl-cc::vm-unbound-variable
           (lambda (c) (equal 'x (cl-cc::vm-variable-name c))))
          ("undefined-function"
           (lambda (s) (cl-cc::make-vm-undefined-function s 'foo))
           'cl-cc::vm-undefined-function
           (lambda (c) (equal 'foo (cl-cc::vm-function-name c))))
          ("division-by-zero"
           (lambda (s) (cl-cc::make-vm-division-by-zero s 42))
           'cl-cc::vm-division-by-zero
           (lambda (c) (equal 42 (cl-cc::vm-dividend c)))))
  (make-fn expected-type check-slots-fn)
  (let* ((state (make-instance 'cl-cc::vm-state))
         (cond (funcall make-fn state)))
    (assert-true (typep cond expected-type))
    (assert-true (funcall check-slots-fn cond))))

;;; ─── Condition Hierarchy ──────────────────────────────────────────────────

(deftest-each condition-inherits-vm-error
  "Each VM condition subtype is a vm-error."
  :cases (("type-error"      (lambda (s) (cl-cc::make-vm-type-error s 'fixnum 42)))
          ("unbound-var"     (lambda (s) (cl-cc::make-vm-unbound-variable s 'x)))
          ("division-by-zero" (lambda (s) (cl-cc::make-vm-division-by-zero s 42))))
  (make-cond-fn)
  (let* ((state (make-instance 'cl-cc::vm-state))
         (c (funcall make-cond-fn state)))
    (assert-true (typep c 'cl-cc::vm-error))))

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

(deftest find-handler-behavior
  "vm-find-handler returns handler on type match; nil when no handler matches."
  (let ((state (make-instance 'cl-cc::vm-state))
        (cond-val nil))
    (cl-cc::vm-push-handler-to-stack state 'cl-cc::vm-error #'identity)
    (setf cond-val (cl-cc::make-vm-type-error state 'fixnum 42))
    (assert-true (not (null (cl-cc::vm-find-handler state cond-val))))
    ;; replace with non-matching handler
    (cl-cc::vm-pop-handler-from-stack state)
    (cl-cc::vm-push-handler-to-stack state 'cl-cc::vm-warning #'identity)
    (assert-equal nil (cl-cc::vm-find-handler state cond-val))))

;;; ─── Restart Bindings ─────────────────────────────────────────────────────

(deftest restarts-initially-nil
  "Fresh vm-state has no restarts."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (assert-equal nil (cl-cc::vm-get-restarts state))))

(deftest vm-restart-operations
  "vm-add-restart makes restart findable by name; vm-find-restart returns nil for unknown name."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (cl-cc::vm-add-restart state 'continue #'identity)
    (let ((restart (cl-cc::vm-find-restart state 'continue)))
      (assert-true (not (null restart)))
      (assert-equal 'continue (cl-cc::vm-restart-name restart))))
  (let ((state (make-instance 'cl-cc::vm-state)))
    (assert-equal nil (cl-cc::vm-find-restart state 'nonexistent))))

;;; ─── Signal Dispatch ──────────────────────────────────────────────────────

(deftest signal-condition-dispatch
  "vm-signal-condition returns (t handler) with a matching handler; (nil nil) without."
  (let* ((state (make-instance 'cl-cc::vm-state))
         (cond-val (cl-cc::make-vm-type-error state 'fixnum 42)))
    (cl-cc::vm-push-handler-to-stack state 'cl-cc::vm-error #'identity)
    (multiple-value-bind (found handler)
        (cl-cc::vm-signal-condition cond-val state)
      (assert-true found)
      (assert-true (not (null handler))))
    ;; remove handler, signal again — no match
    (cl-cc::vm-pop-handler-from-stack state)
    (multiple-value-bind (found handler)
        (cl-cc::vm-signal-condition cond-val state)
      (assert-equal nil found)
      (assert-equal nil handler))))

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
