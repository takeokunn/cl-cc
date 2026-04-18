(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;;; Invariant tests for with-fresh-prolog (framework-fixtures.lisp).
;;;;
;;;; The fixture snapshots cl-cc/prolog::*prolog-rules*, clears the DB,
;;;; runs BODY, then restores every saved predicate via unwind-protect.
;;;; These tests lock in that contract so that future edits to the
;;;; fixture cannot silently leak state between tests.

(defun %snapshot-prolog-keys ()
  "Return a fresh sorted list of predicate keys in *prolog-rules*."
  (let ((keys '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k keys))
             cl-cc/prolog::*prolog-rules*)
    (sort keys #'string< :key #'symbol-name)))

(defun %assert-prolog-state-restored (thunk &key absent-key)
  "Run THUNK and assert the global Prolog DB matches its pre-run state."
  (let ((pre-count (hash-table-count cl-cc/prolog::*prolog-rules*))
        (pre-keys (%snapshot-prolog-keys)))
    (funcall thunk)
    (assert-= pre-count
              (hash-table-count cl-cc/prolog::*prolog-rules*))
    (assert-equal pre-keys (%snapshot-prolog-keys))
    (when absent-key
      (assert-null (gethash absent-key cl-cc/prolog::*prolog-rules*)))))

(deftest prolog-fixture-restores-count-when-body-adds
  "with-fresh-prolog restores hash-table-count after BODY adds rules."
  (%assert-prolog-state-restored
   (lambda ()
     (with-fresh-prolog
       (cl-cc/prolog:add-rule
         'prolog-fixture-invariant-dummy
         (cl-cc/prolog:make-prolog-rule
           :head '(prolog-fixture-invariant-dummy ?x)
           :body nil))
       ;; Inside the fixture, the DB is isolated and only our dummy key lives.
       (assert-= 1 (hash-table-count cl-cc/prolog::*prolog-rules*)))))
   :absent-key 'prolog-fixture-invariant-dummy)

(deftest prolog-fixture-restores-keys-when-body-retracts
  "with-fresh-prolog restores the exact key set after BODY mutates the DB.
   The public :cl-cc/prolog API exposes clear-prolog-database and add-rule,
   but no retract-rule symbol; we emulate a retraction by clearing inside
   the fixture, which is the most aggressive mutation the public API
   allows. The post-state must still equal the pre-state."
  (%assert-prolog-state-restored
   (lambda ()
     (with-fresh-prolog
       ;; Simulate a wholesale retract by clearing the isolated copy.
       (cl-cc/prolog:clear-prolog-database)
       (assert-= 0 (hash-table-count cl-cc/prolog::*prolog-rules*))))))

(deftest prolog-fixture-restores-on-non-local-exit
  "unwind-protect in with-fresh-prolog restores state even when BODY errors."
  (%assert-prolog-state-restored
   (lambda ()
     (handler-case
         (with-fresh-prolog
           (cl-cc/prolog:add-rule
             'prolog-fixture-invariant-nle
             (cl-cc/prolog:make-prolog-rule
               :head '(prolog-fixture-invariant-nle ?x)
               :body nil))
           (error "intentional non-local exit from with-fresh-prolog body"))
       (error () nil)))
   :absent-key 'prolog-fixture-invariant-nle))
