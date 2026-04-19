;;;; optimizer-purity-tests.lisp — Unit tests for optimizer-purity.lisp
;;;;
;;;; Covers: opt-function-body-transitively-pure-p (empty body, pure arithmetic,
;;;; unknown call → impure, known impure call, known pure call),
;;;; opt-infer-transitive-function-purity (empty program, pure leaf,
;;;; recursive → not pure, two-function chain, multi-hop purity).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ─────────────────────────────────────────────────────────────────

(defun %make-func-defs (&rest label-body-pairs)
  "Build a func-defs hash-table from alternating label/body-list args."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (label body) on label-body-pairs by #'cddr
          do (setf (gethash label ht)
                   (list :closure nil :params nil :body body)))
    ht))

(defun %make-pure-labels (&rest labels)
  "Build a pure-labels hash-table with each label marked T."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (l labels ht)
      (setf (gethash l ht) t))))

;;; ─── opt-function-body-transitively-pure-p ───────────────────────────────────

(deftest opt-body-pure-empty
  "Empty body is trivially pure."
  (assert-true
   (cl-cc/optimize::opt-function-body-transitively-pure-p
    nil
    (%make-func-defs)
    (make-hash-table :test #'eq)
    (%make-pure-labels))))

(deftest opt-body-pure-arithmetic-only
  "Body containing only arithmetic + vm-ret is pure."
  (let ((body (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                    (make-vm-ret :reg :r2))))
    (assert-true
     (cl-cc/optimize::opt-function-body-transitively-pure-p
      body
      (%make-func-defs)
      (make-hash-table :test #'eq)
      (%make-pure-labels)))))

(deftest opt-body-pure-const-and-move
  "Body with vm-const and vm-move is pure."
  (let ((body (list (make-vm-const :dst :r0 :value 42)
                    (make-vm-move  :dst :r1 :src :r0)
                    (make-vm-ret   :reg :r1))))
    (assert-true
     (cl-cc/optimize::opt-function-body-transitively-pure-p
      body
      (%make-func-defs)
      (make-hash-table :test #'eq)
      (%make-pure-labels)))))

(deftest opt-body-impure-unknown-call
  "Unknown vm-call (func-reg not tracked) makes body impure."
  (let ((body (list (make-vm-call :dst :r1 :func :r0 :args nil)
                    (make-vm-ret  :reg :r1))))
    (assert-null
     (cl-cc/optimize::opt-function-body-transitively-pure-p
      body
      (%make-func-defs)
      (make-hash-table :test #'eq)
      (%make-pure-labels)))))

(deftest opt-body-impure-known-call-not-pure
  "vm-call to a known callee that is NOT in pure-labels makes body impure."
  (let* ((callee-label "callee-fn")
         (closure (make-vm-closure :dst :r0 :label callee-label :params nil :captured nil))
         (call    (make-vm-call :dst :r1 :func :r0 :args nil))
         (ret     (make-vm-ret :reg :r1))
         (fdefs   (%make-func-defs callee-label (list ret))))
    (assert-null
     (cl-cc/optimize::opt-function-body-transitively-pure-p
      (list closure call ret)
      fdefs
      (make-hash-table :test #'eq)
      (%make-pure-labels)))))   ; callee NOT in pure-labels

(deftest opt-body-pure-known-call-pure
  "vm-call to a known callee that IS in pure-labels keeps body pure."
  (let* ((callee-label "pure-callee")
         (closure (make-vm-closure :dst :r0 :label callee-label :params nil :captured nil))
         (call    (make-vm-call :dst :r1 :func :r0 :args nil))
         (ret     (make-vm-ret :reg :r1))
         (fdefs   (%make-func-defs callee-label (list ret))))
    (assert-true
     (cl-cc/optimize::opt-function-body-transitively-pure-p
      (list closure call ret)
      fdefs
      (make-hash-table :test #'eq)
      (%make-pure-labels callee-label)))))   ; callee IS in pure-labels

;;; ─── opt-infer-transitive-function-purity ────────────────────────────────────

(deftest opt-infer-purity-empty-program
  "Empty instruction list yields an empty pure-labels set."
  (let ((pure (cl-cc/optimize::opt-infer-transitive-function-purity nil)))
    (assert-= 0 (hash-table-count pure))))

(deftest opt-infer-purity-pure-leaf-function
  "A single arithmetic-only function is inferred pure."
  (let* ((body   (list (make-vm-add :dst :r1 :lhs :r0 :rhs :r0)
                       (make-vm-ret :reg :r1)))
         (label  "add-fn")
         (insts  (list (make-vm-closure :dst :r9 :label label
                                        :params '(:r0) :captured nil)
                       (make-vm-label :name label)
                       (make-vm-add   :dst :r1 :lhs :r0 :rhs :r0)
                       (make-vm-ret   :reg :r1))))
    (declare (ignore body))
    (let ((pure (cl-cc/optimize::opt-infer-transitive-function-purity insts)))
      (assert-true (gethash label pure)))))

(deftest opt-infer-purity-recursive-not-pure
  "A directly self-recursive function is NOT inferred pure."
  (let* ((label "rec-fn")
         (insts (list (make-vm-closure :dst :r0 :label label :params '(:r0) :captured nil)
                      (make-vm-label  :name label)
                      ;; calls itself
                      (make-vm-closure :dst :r1 :label label :params nil :captured nil)
                      (make-vm-call   :dst :r2 :func :r1 :args nil)
                      (make-vm-ret    :reg :r2))))
    (let ((pure (cl-cc/optimize::opt-infer-transitive-function-purity insts)))
      (assert-null (gethash label pure)))))

(deftest opt-infer-purity-callee-then-caller
  "When callee is pure, caller that only calls callee is also inferred pure."
  (let* ((callee-label "callee")
         (caller-label "caller")
         (insts (list
                 ;; callee: arithmetic-only, pure leaf
                 (make-vm-closure :dst :r9 :label callee-label :params '(:r0) :captured nil)
                 (make-vm-label   :name callee-label)
                 (make-vm-add     :dst :r1 :lhs :r0 :rhs :r0)
                 (make-vm-ret     :reg :r1)
                 ;; caller: calls callee then returns
                 (make-vm-closure :dst :r8 :label caller-label :params '(:r0) :captured nil)
                 (make-vm-label   :name caller-label)
                 (make-vm-closure :dst :r3 :label callee-label :params nil :captured nil)
                 (make-vm-call    :dst :r4 :func :r3 :args nil)
                 (make-vm-ret     :reg :r4))))
    (let ((pure (cl-cc/optimize::opt-infer-transitive-function-purity insts)))
      (assert-true  (gethash callee-label pure))
      (assert-true  (gethash caller-label pure)))))

(deftest opt-infer-purity-mutually-recursive-not-pure
  "Mutually recursive functions are not inferred pure."
  (let* ((even-label "even-fn")
         (odd-label  "odd-fn")
         (insts (list
                 (make-vm-closure :dst :r9 :label even-label :params '(:r0) :captured nil)
                 (make-vm-label   :name even-label)
                 (make-vm-closure :dst :r1 :label odd-label :params nil :captured nil)
                 (make-vm-call    :dst :r2 :func :r1 :args nil)
                 (make-vm-ret     :reg :r2)
                 (make-vm-closure :dst :r8 :label odd-label :params '(:r0) :captured nil)
                 (make-vm-label   :name odd-label)
                 (make-vm-closure :dst :r3 :label even-label :params nil :captured nil)
                 (make-vm-call    :dst :r4 :func :r3 :args nil)
                 (make-vm-ret     :reg :r4))))
    (let ((pure (cl-cc/optimize::opt-infer-transitive-function-purity insts)))
      (assert-null (gethash even-label pure))
      (assert-null (gethash odd-label  pure)))))
