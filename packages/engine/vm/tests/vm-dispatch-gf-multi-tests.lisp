;;;; tests/unit/vm/vm-dispatch-gf-multi-tests.lisp
;;;; Coverage for src/vm/vm-dispatch-gf-multi.lisp:
;;;;   %vm-gf-uses-composite-keys-p, %vm-resolve-single-dispatch,
;;;;   %vm-resolve-composite-dispatch, vm-resolve-gf-method,
;;;;   vm-resolve-multi-dispatch, vm-try-dispatch-combinations,
;;;;   vm-try-dispatch-sub.

(in-package :cl-cc/test)

(defsuite vm-dispatch-gf-multi-suite
  :description "Tests for vm-dispatch-gf-multi.lisp: composite-key detection and multi-dispatch resolution"
  :parent cl-cc-unit-suite)

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

(deftest gf-multi-composite-keys-empty-table-returns-false
  "%vm-gf-uses-composite-keys-p returns NIL for an empty methods hash table."
  (assert-false (cl-cc/vm::%vm-gf-uses-composite-keys-p (make-hash-table :test #'equal))))

(deftest gf-multi-composite-keys-symbol-only-keys-return-false
  "%vm-gf-uses-composite-keys-p returns NIL when all keys are plain symbols."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash 'integer ht) #'identity)
    (setf (gethash 'string  ht) #'identity)
    (assert-false (cl-cc/vm::%vm-gf-uses-composite-keys-p ht))))

(deftest gf-multi-composite-keys-list-key-returns-true
  "%vm-gf-uses-composite-keys-p returns T when any key is a list."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash '(integer string) ht) #'identity)
    (assert-true (cl-cc/vm::%vm-gf-uses-composite-keys-p ht))))

(deftest gf-multi-composite-keys-mixed-keys-return-true
  "%vm-gf-uses-composite-keys-p returns T when keys are mixed symbol and list."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash 'integer          ht) #'identity)
    (setf (gethash '(integer string) ht) #'identity)
    (assert-true (cl-cc/vm::%vm-gf-uses-composite-keys-p ht))))

;;; ─── %vm-resolve-single-dispatch ──────────────────────────────────────────

(deftest gf-multi-single-dispatch-exact-integer-hit
  "%vm-resolve-single-dispatch returns the method when the argument type matches 'integer."
  (let* ((s (make-test-vm))
         (my-fn #'identity)
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'integer my-fn))))
         (methods-ht (gethash :__methods__ gf-ht)))
    (assert-eq my-fn (cl-cc/vm::%vm-resolve-single-dispatch gf-ht methods-ht s 42))))

(deftest gf-multi-single-dispatch-exact-string-hit
  "%vm-resolve-single-dispatch returns the method when the argument type matches 'string."
  (let* ((s (make-test-vm))
         (my-fn #'string-upcase)
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'string my-fn))))
         (methods-ht (gethash :__methods__ gf-ht)))
    (assert-eq my-fn (cl-cc/vm::%vm-resolve-single-dispatch gf-ht methods-ht s "hello"))))

(deftest gf-multi-single-dispatch-t-fallback
  "%vm-resolve-single-dispatch returns the T-keyed fallback method when no exact type match exists."
  (let* ((s (make-test-vm))
         (fallback-fn #'not)
         (gf-ht (make-single-dispatch-gf-ht (list (cons t fallback-fn))))
         (methods-ht (gethash :__methods__ gf-ht)))
    (assert-eq fallback-fn (cl-cc/vm::%vm-resolve-single-dispatch gf-ht methods-ht s 99))))

(deftest gf-multi-single-dispatch-no-match-returns-nil
  "%vm-resolve-single-dispatch returns NIL when no method covers the argument type."
  (let* ((s (make-test-vm))
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'string #'identity))))
         (methods-ht (gethash :__methods__ gf-ht)))
    (assert-null (cl-cc/vm::%vm-resolve-single-dispatch gf-ht methods-ht s 42))))

;;; ─── vm-try-dispatch-combinations ────────────────────────────────────────

(deftest gf-multi-try-dispatch-zero-arity-returns-nil
  "vm-try-dispatch-combinations returns NIL when arity is 0."
  (assert-null (cl-cc/vm::vm-try-dispatch-combinations (make-hash-table :test #'equal) '() 0)))

(deftest gf-multi-try-dispatch-exact-class-match
  "vm-try-dispatch-combinations returns the method for an exact class-list key hit."
  (let ((ht (make-hash-table :test #'equal))
        (my-fn #'identity))
    (setf (gethash '(integer) ht) my-fn)
    (assert-eq my-fn (cl-cc/vm::vm-try-dispatch-combinations ht '((integer string)) 1))))

(deftest gf-multi-try-dispatch-t-fallback
  "vm-try-dispatch-combinations returns the T-keyed fallback when no exact match is found."
  (let ((ht (make-hash-table :test #'equal))
        (fallback #'not))
    (setf (gethash '(t) ht) fallback)
    (assert-eq fallback (cl-cc/vm::vm-try-dispatch-combinations ht '((symbol integer t)) 1))))

(deftest gf-multi-try-dispatch-no-match-returns-nil
  "vm-try-dispatch-combinations returns NIL when no method covers the CPL combination."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash '(string) ht) #'identity)
    (assert-null (cl-cc/vm::vm-try-dispatch-combinations ht '((integer)) 1))))

;;; ─── vm-try-dispatch-sub ─────────────────────────────────────────────────

(deftest gf-multi-try-dispatch-sub-null-cpls-matches-prefix-directly
  "vm-try-dispatch-sub with null CPLs looks up the prefix key directly in the methods table."
  (let ((ht (make-hash-table :test #'equal))
        (my-fn #'identity))
    (setf (gethash '(integer string) ht) my-fn)
    (assert-eq my-fn (cl-cc/vm::vm-try-dispatch-sub ht nil '(integer string)))))

(deftest gf-multi-try-dispatch-sub-missing-key-returns-nil
  "vm-try-dispatch-sub returns NIL when the prefix key is absent from the methods table."
  (let ((ht (make-hash-table :test #'equal)))
    (assert-null (cl-cc/vm::vm-try-dispatch-sub ht nil '(integer string)))))

;;; ─── vm-resolve-gf-method (integration) ──────────────────────────────────

(deftest gf-multi-resolve-gf-exact-integer-dispatch
  "vm-resolve-gf-method resolves to the integer method for an integer argument."
  (let* ((s (make-test-vm))
         (int-fn #'1+)
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'integer int-fn)))))
    (assert-eq int-fn (cl-cc/vm::vm-resolve-gf-method gf-ht s 42))))

(deftest gf-multi-resolve-gf-no-match-signals-error
  "vm-resolve-gf-method signals an error when no method covers the argument type."
  (let* ((s (make-test-vm))
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'string #'identity)))))
    (assert-signals error (cl-cc/vm::vm-resolve-gf-method gf-ht s 42))))

(deftest gf-multi-resolve-gf-composite-list-key-dispatch
  "vm-resolve-gf-method resolves via composite (list) keys for multi-argument dispatch."
  (let* ((s (make-test-vm))
         (multi-fn #'cons)
         (gf-ht (make-composite-dispatch-gf-ht
                 (list (cons '(integer integer) multi-fn)))))
    (assert-eq multi-fn (cl-cc/vm::vm-resolve-gf-method gf-ht s 1 '(1 2)))))
