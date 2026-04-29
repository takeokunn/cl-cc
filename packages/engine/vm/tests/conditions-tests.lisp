;;;; tests/unit/vm/conditions-tests.lisp — VM Condition System Unit Tests
;;;;
;;;; Tests for handler stack management, restart bindings,
;;;; condition construction, and signal dispatch.

(in-package :cl-cc/test)

(defsuite conditions-suite
  :description "VM condition system unit tests"
  :parent cl-cc-unit-suite)

(in-suite conditions-suite)

;;; ─── Condition Construction ───────────────────────────────────────────────

(deftest-each vm-condition-constructor-slots
  "Each VM condition constructor stores the correct slots."
  :cases (("type-error"
           (lambda (s) (cl-cc/vm::make-vm-type-error s 'fixnum "hello"))
           'cl-cc::vm-type-error
           (lambda (c) (and (equal 'fixnum (type-error-expected-type c))
                            (equal "hello" (type-error-datum c)))))
          ("unbound-variable"
           (lambda (s) (cl-cc/vm::make-vm-unbound-variable s 'x))
           'cl-cc::vm-unbound-variable
           (lambda (c) (equal 'x (cell-error-name c))))
          ("undefined-function"
           (lambda (s) (cl-cc/vm::make-vm-undefined-function s 'foo))
           'cl-cc::vm-undefined-function
           (lambda (c) (equal 'foo (cell-error-name c))))
          ("division-by-zero"
           (lambda (s) (cl-cc/vm::make-vm-division-by-zero s 42))
           'cl-cc::vm-division-by-zero
           (lambda (c) (equal 42 (cl-cc/vm::vm-dividend c)))))
  (make-fn expected-type check-slots-fn)
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (cond (funcall make-fn state)))
    (assert-true (typep cond expected-type))
    (assert-true (funcall check-slots-fn cond))))

;;; ─── Condition Hierarchy ──────────────────────────────────────────────────

(deftest-each condition-inherits-vm-error
  "Each VM condition subtype is a vm-error."
  :cases (("type-error"      (lambda (s) (cl-cc/vm::make-vm-type-error s 'fixnum 42)))
          ("unbound-var"     (lambda (s) (cl-cc/vm::make-vm-unbound-variable s 'x)))
          ("division-by-zero" (lambda (s) (cl-cc/vm::make-vm-division-by-zero s 42))))
  (make-cond-fn)
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (c (funcall make-cond-fn state)))
    (assert-true (typep c 'cl-cc::vm-error))))

;;; ─── Condition Report ─────────────────────────────────────────────────────

(deftest type-error-is-printable
  "vm-type-error can be printed without error."
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (cond (cl-cc/vm::make-vm-type-error state 'fixnum "hello"))
         (msg (format nil "~A" cond)))
    (assert-true (stringp msg))))

;;; ─── Handler Stack ────────────────────────────────────────────────────────

(deftest handler-stack-push-pop-cases
  "Handler stack: fresh empty; push+pop round-trips; empty pop→nil; LIFO order."
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (assert-equal nil (cl-cc/vm::vm-get-handler-stack state)))
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc/vm::vm-push-handler-to-stack state 'cl-cc::vm-error #'identity)
    (let ((handler (cl-cc/vm::vm-pop-handler-from-stack state)))
      (assert-true (not (null handler)))
      (assert-equal 'cl-cc::vm-error (cl-cc/vm::vm-handler-type handler))))
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (assert-equal nil (cl-cc/vm::vm-pop-handler-from-stack state)))
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc/vm::vm-push-handler-to-stack state 'cl-cc::vm-error #'identity)
    (cl-cc/vm::vm-push-handler-to-stack state 'cl-cc::vm-warning #'identity)
    (let ((first (cl-cc/vm::vm-pop-handler-from-stack state)))
      (assert-equal 'cl-cc::vm-warning (cl-cc/vm::vm-handler-type first)))
    (let ((second (cl-cc/vm::vm-pop-handler-from-stack state)))
      (assert-equal 'cl-cc::vm-error (cl-cc/vm::vm-handler-type second)))))

(deftest find-handler-behavior
  "vm-find-handler returns handler on type match; nil when no handler matches."
  (let ((state (make-instance 'cl-cc/vm::vm-io-state))
        (cond-val nil))
    (cl-cc/vm::vm-push-handler-to-stack state 'cl-cc::vm-error #'identity)
    (setf cond-val (cl-cc/vm::make-vm-type-error state 'fixnum 42))
    (assert-true (not (null (cl-cc/vm::vm-find-handler state cond-val))))
    ;; replace with non-matching handler
    (cl-cc/vm::vm-pop-handler-from-stack state)
    (cl-cc/vm::vm-push-handler-to-stack state 'cl-cc::vm-warning #'identity)
    (assert-equal nil (cl-cc/vm::vm-find-handler state cond-val))))

;;; ─── Restart Bindings ─────────────────────────────────────────────────────


(deftest vm-restart-operations
  "Restarts: fresh state has none; add-restart makes findable by name; unknown name returns nil."
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (assert-equal nil (cl-cc/vm::vm-get-restarts state)))
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc/vm::vm-add-restart state 'continue #'identity)
    (let ((restart (cl-cc/vm::vm-find-restart state 'continue)))
      (assert-true (not (null restart)))
      (assert-equal 'continue (cl-cc/vm::vm-restart-name restart))))
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (assert-equal nil (cl-cc/vm::vm-find-restart state 'nonexistent))))

;;; ─── Signal Dispatch ──────────────────────────────────────────────────────

(deftest signal-condition-dispatch
  "vm-signal-condition returns (t handler) with a matching handler; (nil nil) without."
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (cond-val (cl-cc/vm::make-vm-type-error state 'fixnum 42)))
    (cl-cc/vm::vm-push-handler-to-stack state 'cl-cc::vm-error #'identity)
    (multiple-value-bind (found handler)
        (cl-cc/vm::vm-signal-condition cond-val state)
      (assert-true found)
      (assert-true (not (null handler))))
    ;; remove handler, signal again — no match
    (cl-cc/vm::vm-pop-handler-from-stack state)
    (multiple-value-bind (found handler)
        (cl-cc/vm::vm-signal-condition cond-val state)
      (assert-equal nil found)
      (assert-equal nil handler))))

(deftest signal-condition-error-p-signals
  "vm-signal-condition with :error-p t signals when no handler."
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (let ((cond (cl-cc/vm::make-vm-type-error state 'fixnum 42)))
      (assert-true
       (handler-case
           (progn (cl-cc/vm::vm-signal-condition cond state :error-p t) nil)
         (cl-cc::vm-type-error () t))))))

;;; ─── Clear Context ────────────────────────────────────────────────────────

(deftest clear-condition-context
  "vm-clear-condition-context removes handlers and restarts."
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc/vm::vm-push-handler-to-stack state 'cl-cc::vm-error #'identity)
    (cl-cc/vm::vm-add-restart state 'continue #'identity)
    (cl-cc/vm::vm-clear-condition-context state)
    (assert-equal nil (cl-cc/vm::vm-get-handler-stack state))
    (assert-equal nil (cl-cc/vm::vm-get-restarts state))))

(deftest vm-sync-handler-regs-cases
  "vm-sync-handler-regs: reuses one snapshot across handler entries; updates catch frame's saved-regs."
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc/vm::vm-reg-set state :r1 10)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-establish-handler :handler-label "h1" :result-reg :r0 :error-type 'error)
     state 0 nil)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-establish-handler :handler-label "h2" :result-reg :r0 :error-type 'error)
     state 1 nil)
    (cl-cc/vm::vm-reg-set state :r1 42)
    (cl-cc/vm::execute-instruction (cl-cc::make-vm-sync-handler-regs) state 2 nil)
    (let* ((entries (cl-cc/vm::vm-handler-stack state))
           (snapshot-a (cl-cc/vm::vm-handler-entry-saved-regs (first entries)))
           (snapshot-b (cl-cc/vm::vm-handler-entry-saved-regs (second entries))))
      (assert-true (eq snapshot-a snapshot-b))
      (assert-= 42 (gethash :r1 snapshot-a))))
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc/vm::vm-reg-set state :tag 7)
    (cl-cc/vm::vm-reg-set state :r1 99)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-establish-catch :tag-reg :tag :handler-label "catch" :result-reg :r0)
     state 0 nil)
    (cl-cc/vm::vm-reg-set state :r1 123)
    (cl-cc/vm::execute-instruction (cl-cc::make-vm-sync-handler-regs) state 1 nil)
    (let* ((entry (first (cl-cc/vm::vm-handler-stack state)))
           (snapshot (cl-cc/vm::vm-handler-entry-saved-regs entry)))
      (assert-true (hash-table-p snapshot))
      (assert-= 123 (gethash :r1 snapshot)))))

(deftest vm-establish-call-stack-cases
  "vm-establish-handler and vm-establish-catch both retain call/method stacks by shared reference."
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (setf (cl-cc/vm::vm-call-stack state) '((1 :r0 nil nil)))
    (setf (cl-cc/vm::vm-method-call-stack state) '((gf nil args)))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-establish-handler :handler-label "h" :result-reg :r0 :error-type 'error)
     state 0 nil)
    (let ((entry (first (cl-cc/vm::vm-handler-stack state))))
      (assert-true (eq (fourth entry) (cl-cc/vm::vm-call-stack state)))
      (assert-true (eq (fifth entry) (cl-cc/vm::vm-handler-entry-saved-regs entry)))
      (assert-true (eq (sixth entry) (cl-cc/vm::vm-method-call-stack state)))))
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (setf (cl-cc/vm::vm-call-stack state) '((7 :r1 nil nil)))
    (setf (cl-cc/vm::vm-method-call-stack state) '((gf2 nil args2)))
    (cl-cc/vm::vm-reg-set state :tag 9)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-establish-catch :tag-reg :tag :handler-label "c" :result-reg :r0)
     state 0 nil)
    (let ((entry (first (cl-cc/vm::vm-handler-stack state))))
      (assert-true (eq (fifth entry) (cl-cc/vm::vm-call-stack state)))
      (assert-true (eq (seventh entry) (cl-cc/vm::vm-method-call-stack state))))))
