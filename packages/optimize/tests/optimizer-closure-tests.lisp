;;;; tests/optimizer-closure-tests.lisp — Closure optimization pass tests
;;; FR-330 closure capture dedup and FR-079 closure thunk sharing.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ── FR-330 closure-capture-dedup ──────────────────────────────────────────

(deftest closure-capture-dedup-shares-duplicate-environments
  "Two closures sharing entry-label + capture set → vm-move to first."
  (let* ((c1 (cl-cc/vm:make-vm-closure
              :dst :r1 :label "L0" :params () :captured (list (cons 'a :r5))))
         (c2 (cl-cc/vm:make-vm-closure
              :dst :r2 :label "L0" :params () :captured (list (cons 'a :r5))))
         (out (cl-cc/optimize::opt-pass-closure-capture-dedup
               (list c1 c2))))
    (assert-eq (length out) 2)
    (assert-eq (cl-cc/vm::vm-dst (second out)) :r2)
    (assert-eq (cl-cc/vm::vm-src (second out)) :r1)))

(deftest closure-capture-dedup-preserves-non-shareable
  "Closures with different capture sets stay untouched."
  (let* ((c1 (cl-cc/vm:make-vm-closure
              :dst :r1 :label "L0" :params () :captured (list (cons 'a :r5))))
         (c2 (cl-cc/vm:make-vm-closure
              :dst :r2 :label "L0" :params () :captured (list (cons 'a :r6))))
         (out (cl-cc/optimize::opt-pass-closure-capture-dedup
               (list c1 c2))))
    (assert-eq (length out) 2)
    (assert-eq (cl-cc/vm::vm-dst (second out)) :r2)))

;;; ── FR-079 closure-thunk-sharing ──────────────────────────────────────────

(deftest closure-thunk-sharing-deduplicates-safe-siblings
  "Safe sibling closures sharing capture set → vm-move replacement."
  (let* ((c1 (cl-cc/vm:make-vm-closure
              :dst :r1 :label "L0" :params () :captured (list (cons 'a :r5))))
         (c2 (cl-cc/vm:make-vm-closure
              :dst :r2 :label "L0" :params () :captured (list (cons 'a :r5))))
         (out (cl-cc/optimize::opt-pass-closure-thunk-sharing
               (list c1 c2))))
    (assert-eq (length out) 2)
    (assert-eq (cl-cc/vm::vm-dst (second out)) :r2)
    (assert-eq (cl-cc/vm::vm-src (second out)) :r1)))

(deftest closure-thunk-sharing-noops-on-register-overwrite
  "Intermediate register overwrite blocks thunk sharing."
  (let* ((c1 (cl-cc/vm:make-vm-closure
              :dst :r1 :label "L0" :params () :captured (list (cons 'a :r5))))
         (kill (cl-cc/vm:make-vm-const :dst :r1 :value 0))
         (c2 (cl-cc/vm:make-vm-closure
              :dst :r2 :label "L0" :params () :captured (list (cons 'a :r5))))
         (out (cl-cc/optimize::opt-pass-closure-thunk-sharing
               (list c1 kill c2))))
    (assert-eq (length out) 3)
    (assert-eq (cl-cc/vm::vm-dst (third out)) :r2)))

(deftest closure-thunk-sharing-preserves-different-capture
  "Closures with different capture sets → no sharing."
  (let* ((c1 (cl-cc/vm:make-vm-closure
              :dst :r1 :label "L0" :params () :captured (list (cons 'a :r5))))
         (c2 (cl-cc/vm:make-vm-closure
              :dst :r2 :label "L0" :params () :captured (list (cons 'a :r6))))
         (out (cl-cc/optimize::opt-pass-closure-thunk-sharing
               (list c1 c2))))
    (assert-eq (length out) 2)
    (assert-eq (cl-cc/vm::vm-dst (second out)) :r2)))

(deftest closure-thunk-sharing-noops-on-env-reg-write
  "Writing to a captured environment register blocks sharing."
  (let* ((c1 (cl-cc/vm:make-vm-closure
              :dst :r1 :label "L0" :params ()
              :captured (list (cons 'a :r5) (cons 'b :r7))))
         (kill-env (cl-cc/vm:make-vm-const :dst :r5 :value 0))
         (c2 (cl-cc/vm:make-vm-closure
              :dst :r2 :label "L0" :params ()
              :captured (list (cons 'a :r5) (cons 'b :r7))))
         (out (cl-cc/optimize::opt-pass-closure-thunk-sharing
               (list c1 kill-env c2))))
    (assert-eq (length out) 3)
    (assert-eq (cl-cc/vm::vm-dst (third out)) :r2)))
