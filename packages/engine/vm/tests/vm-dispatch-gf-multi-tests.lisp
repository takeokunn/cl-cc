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

(deftest gf-multi-composite-keys-cases
  "%vm-gf-uses-composite-keys-p: nil for empty/symbol-only; T for list key or mixed."
  (assert-false (cl-cc/vm::%vm-gf-uses-composite-keys-p (make-hash-table :test #'equal)))
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash 'integer ht) #'identity)
    (setf (gethash 'string  ht) #'identity)
    (assert-false (cl-cc/vm::%vm-gf-uses-composite-keys-p ht)))
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash '(integer string) ht) #'identity)
    (assert-true (cl-cc/vm::%vm-gf-uses-composite-keys-p ht)))
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash 'integer          ht) #'identity)
    (setf (gethash '(integer string) ht) #'identity)
    (assert-true (cl-cc/vm::%vm-gf-uses-composite-keys-p ht))))

;;; ─── %vm-resolve-single-dispatch ──────────────────────────────────────────

(deftest gf-multi-single-dispatch-cases
  "%vm-resolve-single-dispatch: exact integer hit; exact string hit; T fallback; nil on no match."
  (let* ((s (make-test-vm))
         (my-fn #'identity)
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'integer my-fn))))
         (methods-ht (gethash :__methods__ gf-ht)))
    (assert-eq my-fn (cl-cc/vm::%vm-resolve-single-dispatch gf-ht methods-ht s 42)))
  (let* ((s (make-test-vm))
         (my-fn #'string-upcase)
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'string my-fn))))
         (methods-ht (gethash :__methods__ gf-ht)))
    (assert-eq my-fn (cl-cc/vm::%vm-resolve-single-dispatch gf-ht methods-ht s "hello")))
  (let* ((s (make-test-vm))
         (fallback-fn #'not)
         (gf-ht (make-single-dispatch-gf-ht (list (cons t fallback-fn))))
         (methods-ht (gethash :__methods__ gf-ht)))
    (assert-eq fallback-fn (cl-cc/vm::%vm-resolve-single-dispatch gf-ht methods-ht s 99)))
  (let* ((s (make-test-vm))
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'string #'identity))))
         (methods-ht (gethash :__methods__ gf-ht)))
    (assert-null (cl-cc/vm::%vm-resolve-single-dispatch gf-ht methods-ht s 42))))

;;; ─── vm-try-dispatch-combinations ────────────────────────────────────────

(deftest gf-multi-try-dispatch-cases
  "vm-try-dispatch-combinations: n=0 → nil; n=1 exact hit; n=1 T-fallback; n=1 no match → nil."
  (assert-null (cl-cc/vm::vm-try-dispatch-combinations (make-hash-table :test #'equal) '() 0))
  (let ((ht (make-hash-table :test #'equal))
        (my-fn #'identity))
    (setf (gethash '(integer) ht) my-fn)
    (assert-eq my-fn (cl-cc/vm::vm-try-dispatch-combinations ht '((integer string)) 1)))
  (let ((ht (make-hash-table :test #'equal))
        (fallback #'not))
    (setf (gethash '(t) ht) fallback)
    (assert-eq fallback (cl-cc/vm::vm-try-dispatch-combinations ht '((symbol integer t)) 1)))
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash '(string) ht) #'identity)
    (assert-null (cl-cc/vm::vm-try-dispatch-combinations ht '((integer)) 1))))

;;; ─── vm-try-dispatch-sub ─────────────────────────────────────────────────

(deftest gf-multi-try-dispatch-sub-cases
  "vm-try-dispatch-sub: null cpls finds prefix directly; missing key → nil."
  (let ((ht (make-hash-table :test #'equal))
        (my-fn #'identity))
    (setf (gethash '(integer string) ht) my-fn)
    (assert-eq my-fn (cl-cc/vm::vm-try-dispatch-sub ht nil '(integer string))))
  (let ((ht (make-hash-table :test #'equal)))
    (assert-null (cl-cc/vm::vm-try-dispatch-sub ht nil '(integer string)))))

;;; ─── vm-resolve-gf-method (integration) ──────────────────────────────────

(deftest gf-multi-resolve-gf-cases
  "vm-resolve-gf-method: exact integer dispatch; no match → error; composite list-key dispatch."
  (let* ((s (make-test-vm))
         (int-fn #'1+)
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'integer int-fn)))))
    (assert-eq int-fn (cl-cc/vm::vm-resolve-gf-method gf-ht s 42)))
  (let* ((s (make-test-vm))
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'string #'identity)))))
    (assert-signals error (cl-cc/vm::vm-resolve-gf-method gf-ht s 42)))
  (let* ((s (make-test-vm))
         (multi-fn #'cons)
         (gf-ht (make-composite-dispatch-gf-ht
                 (list (cons '(integer integer) multi-fn)))))
    (assert-eq multi-fn (cl-cc/vm::vm-resolve-gf-method gf-ht s 1 '(1 2)))))
