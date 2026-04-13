;;;; tests/unit/vm/vm-dispatch-gf-multi-tests.lisp
;;;; Coverage for src/vm/vm-dispatch-gf-multi.lisp:
;;;;   %vm-gf-uses-composite-keys-p, %vm-resolve-single-dispatch,
;;;;   %vm-resolve-composite-dispatch, vm-resolve-gf-method,
;;;;   vm-resolve-multi-dispatch, vm-try-dispatch-combinations,
;;;;   vm-try-dispatch-sub.

(in-package :cl-cc/test)

(defsuite vm-dispatch-gf-multi-suite
  :description "Tests for vm-dispatch-gf-multi.lisp: composite-key detection and multi-dispatch resolution"
  :parent cl-cc-suite)

(in-suite vm-dispatch-gf-multi-suite)

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun make-single-dispatch-gf-ht (methods-plist)
  "Build a minimal generic-function hash-table for single dispatch.
METHODS-PLIST is a list of (class-name . method-fn) pairs."
  (let ((gf-ht (make-hash-table :test #'equal))
        (methods-ht (make-hash-table :test #'equal)))
    (dolist (pair methods-plist)
      (setf (gethash (car pair) methods-ht) (cdr pair)))
    (setf (gethash :__methods__ gf-ht) methods-ht)
    (setf (gethash :__name__ gf-ht) 'test-gf)
    (setf (gethash :__eql-methods__ gf-ht) nil)
    gf-ht))

(defun make-composite-dispatch-gf-ht (methods-plist)
  "Build a generic-function hash-table with list keys for composite dispatch."
  (let ((gf-ht (make-hash-table :test #'equal))
        (methods-ht (make-hash-table :test #'equal)))
    (dolist (pair methods-plist)
      (setf (gethash (car pair) methods-ht) (cdr pair)))
    (setf (gethash :__methods__ gf-ht) methods-ht)
    (setf (gethash :__name__ gf-ht) 'test-multi-gf)
    (setf (gethash :__eql-methods__ gf-ht) nil)
    gf-ht))

;;; ─── %vm-gf-uses-composite-keys-p ────────────────────────────────────────

(deftest gf-multi-composite-keys-empty-table
  "%vm-gf-uses-composite-keys-p returns nil for an empty hash table."
  (let ((ht (make-hash-table :test #'equal)))
    (assert-false (cl-cc::%vm-gf-uses-composite-keys-p ht))))

(deftest gf-multi-composite-keys-symbol-keys-only
  "%vm-gf-uses-composite-keys-p returns nil when all keys are symbols (single dispatch)."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash 'integer ht) #'identity)
    (setf (gethash 'string  ht) #'identity)
    (assert-false (cl-cc::%vm-gf-uses-composite-keys-p ht))))

(deftest gf-multi-composite-keys-list-key-present
  "%vm-gf-uses-composite-keys-p returns T when at least one list key exists."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash '(integer string) ht) #'identity)
    (assert-true (cl-cc::%vm-gf-uses-composite-keys-p ht))))

(deftest gf-multi-composite-keys-mixed-keys
  "%vm-gf-uses-composite-keys-p returns T when symbol and list keys coexist."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash 'integer         ht) #'identity)
    (setf (gethash '(integer string) ht) #'identity)
    (assert-true (cl-cc::%vm-gf-uses-composite-keys-p ht))))

;;; ─── %vm-resolve-single-dispatch ──────────────────────────────────────────

(deftest gf-multi-single-dispatch-integer-hit
  "%vm-resolve-single-dispatch finds an integer method by class name."
  (let* ((s (make-test-vm))
         (my-fn #'identity)
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'integer my-fn))))
         (methods-ht (gethash :__methods__ gf-ht))
         (result (cl-cc::%vm-resolve-single-dispatch gf-ht methods-ht s 42)))
    (assert-eq my-fn result)))

(deftest gf-multi-single-dispatch-string-hit
  "%vm-resolve-single-dispatch finds a string method."
  (let* ((s (make-test-vm))
         (my-fn #'string-upcase)
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'string my-fn))))
         (methods-ht (gethash :__methods__ gf-ht))
         (result (cl-cc::%vm-resolve-single-dispatch gf-ht methods-ht s "hello")))
    (assert-eq my-fn result)))

(deftest gf-multi-single-dispatch-t-fallback
  "%vm-resolve-single-dispatch falls back to T method when no exact match."
  (let* ((s (make-test-vm))
         (fallback-fn #'not)
         (gf-ht (make-single-dispatch-gf-ht (list (cons t fallback-fn))))
         (methods-ht (gethash :__methods__ gf-ht))
         (result (cl-cc::%vm-resolve-single-dispatch gf-ht methods-ht s 99)))
    (assert-eq fallback-fn result)))

(deftest gf-multi-single-dispatch-no-match-returns-nil
  "%vm-resolve-single-dispatch returns nil when no method applies."
  (let* ((s (make-test-vm))
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'string #'identity))))
         (methods-ht (gethash :__methods__ gf-ht))
         (result (cl-cc::%vm-resolve-single-dispatch gf-ht methods-ht s 42)))
    (assert-null result)))

;;; ─── vm-try-dispatch-combinations ────────────────────────────────────────

(deftest gf-multi-try-dispatch-n-zero-nil-key
  "vm-try-dispatch-combinations with n=0 looks up nil key in table."
  (let ((ht (make-hash-table :test #'equal)))
    ;; When n=0, it looks up nil — not typical, but tests boundary
    (let ((result (cl-cc::vm-try-dispatch-combinations ht '() 0)))
      (assert-null result))))

(deftest gf-multi-try-dispatch-n1-exact-match
  "vm-try-dispatch-combinations with n=1 finds (class) key."
  (let ((ht (make-hash-table :test #'equal))
        (my-fn #'identity))
    (setf (gethash '(integer) ht) my-fn)
    (let ((result (cl-cc::vm-try-dispatch-combinations ht '((integer string)) 1)))
      (assert-eq my-fn result))))

(deftest gf-multi-try-dispatch-n1-fallback-to-t
  "vm-try-dispatch-combinations with n=1 falls back to (t) key."
  (let ((ht (make-hash-table :test #'equal))
        (fallback #'not))
    (setf (gethash '(t) ht) fallback)
    (let ((result (cl-cc::vm-try-dispatch-combinations ht '((symbol integer t)) 1)))
      (assert-eq fallback result))))

(deftest gf-multi-try-dispatch-no-match-returns-nil
  "vm-try-dispatch-combinations returns nil when no combination matches."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash '(string) ht) #'identity)
    (let ((result (cl-cc::vm-try-dispatch-combinations ht '((integer)) 1)))
      (assert-null result))))

;;; ─── vm-try-dispatch-sub ─────────────────────────────────────────────────

(deftest gf-multi-try-dispatch-sub-null-cpls-finds-prefix
  "vm-try-dispatch-sub with null cpls looks up the prefix directly."
  (let ((ht (make-hash-table :test #'equal))
        (my-fn #'identity))
    (setf (gethash '(integer string) ht) my-fn)
    (let ((result (cl-cc::vm-try-dispatch-sub ht nil '(integer string))))
      (assert-eq my-fn result))))

(deftest gf-multi-try-dispatch-sub-not-found-returns-nil
  "vm-try-dispatch-sub returns nil when the prefix/key is not in the table."
  (let ((ht (make-hash-table :test #'equal)))
    (let ((result (cl-cc::vm-try-dispatch-sub ht nil '(integer string))))
      (assert-null result))))

;;; ─── vm-resolve-gf-method (integration) ──────────────────────────────────

(deftest gf-multi-resolve-gf-single-dispatch-integer
  "vm-resolve-gf-method dispatches correctly on integer argument."
  (let* ((s (make-test-vm))
         (int-fn #'1+)
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'integer int-fn)))))
    (let ((result (cl-cc::vm-resolve-gf-method gf-ht s 42)))
      (assert-eq int-fn result))))

(deftest gf-multi-resolve-gf-single-dispatch-no-match-errors
  "vm-resolve-gf-method signals an error when no method matches."
  (let* ((s (make-test-vm))
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'string #'identity)))))
    (assert-signals error
      (cl-cc::vm-resolve-gf-method gf-ht s 42))))

(deftest gf-multi-resolve-gf-composite-dispatch-list-key
  "vm-resolve-gf-method uses composite dispatch when all-args is provided and list keys exist."
  (let* ((s (make-test-vm))
         (multi-fn #'cons)
         (gf-ht (make-composite-dispatch-gf-ht
                 (list (cons '(integer integer) multi-fn)))))
    (let ((result (cl-cc::vm-resolve-gf-method gf-ht s 1 '(1 2))))
      (assert-eq multi-fn result))))
