;;;; tests/unit/vm/vm-environment-tests.lisp
;;;; Unit tests for src/vm/vm-environment.lisp
;;;;
;;;; Covers: vm-boundp, vm-fboundp, vm-makunbound,
;;;;   vm-fdefinition, vm-random, vm-make-random-state,
;;;;   vm-get-universal-time, vm-get-internal-real-time,
;;;;   vm-get-internal-run-time, vm-decode-universal-time,
;;;;   vm-encode-universal-time.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── helpers ─────────────────────────────────────────────────────────────

(defun %env-unary (ctor-fn sym)
  "Run a unary environment instruction using SYM as the :src register value."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 sym)
    (exec1 (funcall ctor-fn :dst 0 :src 1) s)
    (cl-cc:vm-reg-get s 0)))

;;; ─── vm-boundp ───────────────────────────────────────────────────────────

(deftest vm-boundp-unbound-symbol-returns-nil
  "vm-boundp returns NIL for a symbol not in the global vars table."
  (let ((result (%env-unary #'cl-cc::make-vm-boundp 'totally-unbound-sym-xyz)))
    (assert-false result)))

(deftest vm-boundp-bound-symbol-returns-t
  "vm-boundp returns T for a symbol with a value in the global vars table."
  (let ((s (make-test-vm)))
    (setf (gethash 'my-test-var (cl-cc/vm::vm-global-vars s)) 42)
    (cl-cc:vm-reg-set s 1 'my-test-var)
    (exec1 (cl-cc::make-vm-boundp :dst 0 :src 1) s)
    (assert-true (cl-cc:vm-reg-get s 0))))

(deftest vm-boundp-nil-value-still-bound
  "vm-boundp returns T even when the variable's value is NIL (ANSI semantics)."
  (let ((s (make-test-vm)))
    (setf (gethash 'nil-valued-var (cl-cc/vm::vm-global-vars s)) nil)
    (cl-cc:vm-reg-set s 1 'nil-valued-var)
    (exec1 (cl-cc::make-vm-boundp :dst 0 :src 1) s)
    (assert-true (cl-cc:vm-reg-get s 0))))

;;; ─── vm-fboundp ──────────────────────────────────────────────────────────

(deftest vm-fboundp-unregistered-symbol-returns-nil
  "vm-fboundp returns NIL for a symbol not in the function registry."
  (let ((result (%env-unary #'cl-cc::make-vm-fboundp 'no-such-function-xyz)))
    (assert-false result)))

(deftest vm-fboundp-registered-function-returns-t
  "vm-fboundp returns T for a symbol registered in the VM function table."
  (let ((s (make-test-vm)))
    (setf (gethash 'my-fn (cl-cc/vm::vm-function-registry s)) #'identity)
    (cl-cc:vm-reg-set s 1 'my-fn)
    (exec1 (cl-cc::make-vm-fboundp :dst 0 :src 1) s)
    (assert-true (cl-cc:vm-reg-get s 0))))

;;; ─── vm-makunbound ───────────────────────────────────────────────────────

(deftest vm-makunbound-removes-binding-and-returns-sym
  "vm-makunbound removes a global variable and returns the symbol name."
  (let ((s (make-test-vm)))
    (setf (gethash 'to-unbind (cl-cc/vm::vm-global-vars s)) 99)
    (cl-cc:vm-reg-set s 1 'to-unbind)
    (exec1 (cl-cc::make-vm-makunbound :dst 0 :src 1) s)
    (assert-eq 'to-unbind (cl-cc:vm-reg-get s 0))
    (assert-false (nth-value 1 (gethash 'to-unbind (cl-cc/vm::vm-global-vars s))))))

(deftest vm-makunbound-already-unbound-returns-sym
  "vm-makunbound on an already-unbound symbol still returns the symbol."
  (let ((result (%env-unary #'cl-cc::make-vm-makunbound 'never-was-bound)))
    (assert-eq 'never-was-bound result)))

;;; ─── vm-fdefinition ──────────────────────────────────────────────────────

(deftest vm-fdefinition-retrieves-registered-function
  "vm-fdefinition returns the function object for a registered symbol."
  (let ((s (make-test-vm))
        (fn #'identity))
    (setf (gethash 'my-ident (cl-cc/vm::vm-function-registry s)) fn)
    (cl-cc:vm-reg-set s 1 'my-ident)
    (exec1 (cl-cc::make-vm-fdefinition :dst 0 :src 1) s)
    (assert-eq fn (cl-cc:vm-reg-get s 0))))

(deftest vm-fdefinition-undefined-signals-error
  "vm-fdefinition signals an error when the symbol has no function binding."
  (assert-signals error
    (%env-unary #'cl-cc::make-vm-fdefinition 'undefined-fn-xyz)))

;;; ─── vm-random ───────────────────────────────────────────────────────────

(deftest vm-random-returns-integer-in-range
  "vm-random returns a non-negative integer less than the limit."
  (let ((result (%env-unary #'cl-cc::make-vm-random 100)))
    (assert-true (integerp result))
    (assert-true (>= result 0))
    (assert-true (< result 100))))

(deftest vm-random-float-limit
  "vm-random with a float limit returns a float in [0.0, limit)."
  (let ((result (%env-unary #'cl-cc::make-vm-random 1.0)))
    (assert-true (floatp result))
    (assert-true (>= result 0.0))
    (assert-true (< result 1.0))))

;;; ─── vm-make-random-state ────────────────────────────────────────────────

(deftest-each vm-make-random-state-cases
  "vm-make-random-state produces a random-state for T, NIL, and another state."
  :cases (("fresh"  t)
          ("copy"   nil))
  (arg)
  (let ((result (%env-unary #'cl-cc::make-vm-make-random-state arg)))
    (assert-true (typep result 'random-state))))

;;; ─── vm-get-universal-time ───────────────────────────────────────────────

(deftest vm-get-universal-time-returns-positive-integer
  "vm-get-universal-time returns a positive integer (seconds since 1900)."
  (let ((s (make-test-vm)))
    (exec1 (cl-cc::make-vm-get-universal-time :dst 0) s)
    (let ((result (cl-cc:vm-reg-get s 0)))
      (assert-true (integerp result))
      (assert-true (> result 0)))))

;;; ─── vm-get-internal-real-time ───────────────────────────────────────────

(deftest vm-get-internal-real-time-returns-non-negative-integer
  "vm-get-internal-real-time returns a non-negative integer."
  (let ((s (make-test-vm)))
    (exec1 (cl-cc::make-vm-get-internal-real-time :dst 0) s)
    (let ((result (cl-cc:vm-reg-get s 0)))
      (assert-true (integerp result))
      (assert-true (>= result 0)))))

;;; ─── vm-get-internal-run-time ────────────────────────────────────────────

(deftest vm-get-internal-run-time-returns-non-negative-integer
  "vm-get-internal-run-time returns a non-negative integer."
  (let ((s (make-test-vm)))
    (exec1 (cl-cc::make-vm-get-internal-run-time :dst 0) s)
    (let ((result (cl-cc:vm-reg-get s 0)))
      (assert-true (integerp result))
      (assert-true (>= result 0)))))

;;; ─── vm-decode-universal-time ────────────────────────────────────────────

(deftest vm-decode-universal-time-stores-9-values
  "vm-decode-universal-time stores exactly 9 multiple-values."
  (let ((s (make-test-vm))
        (epoch (encode-universal-time 0 0 0 1 1 2000)))
    (cl-cc:vm-reg-set s 1 epoch)
    (exec1 (cl-cc::make-vm-decode-universal-time :dst 0 :src 1) s)
    (assert-= 9 (length (cl-cc:vm-values-list s)))))

(deftest vm-decode-universal-time-primary-value-is-seconds
  "vm-decode-universal-time sets dst to the seconds component."
  (let ((s (make-test-vm))
        (epoch (encode-universal-time 30 15 12 1 1 2000)))
    (cl-cc:vm-reg-set s 1 epoch)
    (exec1 (cl-cc::make-vm-decode-universal-time :dst 0 :src 1) s)
    (assert-= 30 (cl-cc:vm-reg-get s 0))))

;;; ─── vm-encode-universal-time ────────────────────────────────────────────

(deftest vm-encode-universal-time-round-trips-decode
  "encode then decode gives back the same components."
  (let* ((original (encode-universal-time 5 30 10 15 6 2023 0))
         (s (make-test-vm))
         (args (list 5 30 10 15 6 2023 0)))
    (cl-cc:vm-reg-set s 1 args)
    (exec1 (cl-cc::make-vm-encode-universal-time :dst 0 :args-reg 1) s)
    (assert-= original (cl-cc:vm-reg-get s 0))))

(deftest vm-encode-universal-time-without-timezone
  "vm-encode-universal-time without timezone arg produces an integer."
  (let* ((s (make-test-vm))
         (args (list 0 0 12 1 1 2000)))
    (cl-cc:vm-reg-set s 1 args)
    (exec1 (cl-cc::make-vm-encode-universal-time :dst 0 :args-reg 1) s)
    (assert-true (integerp (cl-cc:vm-reg-get s 0)))))
