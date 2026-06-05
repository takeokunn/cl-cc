(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defmacro with-clean-deadlock-state (&body body)
  "Run BODY with a freshly initialized deadlock detector, restoring state after."
  `(let ((cl-cc/runtime::*rt-dl-enabled* nil)
         (cl-cc/runtime::*rt-dl-thread-locks* (make-hash-table :test #'eq))
         (cl-cc/runtime::*rt-dl-thread-waits* (make-hash-table :test #'eq)))
     ,@body))

(deftest deadlock-detect-disabled-by-default
  "rt-deadlock-detect returns nil when *rt-dl-enabled* is nil (the default)."
  (with-clean-deadlock-state
    (assert-false cl-cc/runtime::*rt-dl-enabled*)
    (assert-false (cl-cc/runtime:rt-deadlock-detect))))

(deftest deadlock-detect-no-cycle-single-thread
  "rt-deadlock-detect returns nil when only one thread holds a lock with no waits."
  (with-clean-deadlock-state
    (setf cl-cc/runtime::*rt-dl-enabled* t)
    (cl-cc/runtime:rt-deadlock-after-lock 'lock-a 'thread-1 t)
    (assert-false (cl-cc/runtime:rt-deadlock-detect))))

(deftest deadlock-detect-two-thread-cycle
  "rt-deadlock-detect identifies a two-thread A-waits-B, B-waits-A cycle."
  (with-clean-deadlock-state
    (setf cl-cc/runtime::*rt-dl-enabled* t)
    ;; thread-1 holds lock-a, waits for lock-b
    (cl-cc/runtime:rt-deadlock-after-lock 'lock-a 'thread-1 t)
    (cl-cc/runtime:rt-deadlock-before-lock 'lock-b 'thread-1)
    ;; thread-2 holds lock-b, waits for lock-a
    (cl-cc/runtime:rt-deadlock-after-lock 'lock-b 'thread-2 t)
    (cl-cc/runtime:rt-deadlock-before-lock 'lock-a 'thread-2)
    (assert-true (cl-cc/runtime:rt-deadlock-detect))))

(deftest deadlock-after-unlock-removes-held-lock
  "rt-deadlock-after-unlock removes the lock from the thread's held-lock list."
  (with-clean-deadlock-state
    (setf cl-cc/runtime::*rt-dl-enabled* t)
    (cl-cc/runtime:rt-deadlock-after-lock 'lock-x 'thread-1 t)
    (cl-cc/runtime:rt-deadlock-after-unlock 'lock-x 'thread-1)
    ;; After unlock, thread-1 should have no held locks — no deadlock possible
    (assert-false (cl-cc/runtime:rt-deadlock-detect))))

(deftest deadlock-init-clears-all-state
  "rt-deadlock-init resets all tracking tables and leaves detection disabled."
  (with-clean-deadlock-state
    (setf cl-cc/runtime::*rt-dl-enabled* t)
    (cl-cc/runtime:rt-deadlock-after-lock 'lock-a 'thread-1 t)
    (cl-cc/runtime:rt-deadlock-init)
    (assert-false cl-cc/runtime::*rt-dl-enabled*)
    (assert-= 0 (hash-table-count cl-cc/runtime::*rt-dl-thread-locks*))
    (assert-= 0 (hash-table-count cl-cc/runtime::*rt-dl-thread-waits*))))
