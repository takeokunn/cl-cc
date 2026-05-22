;;;; Unit tests for FR-682 loop peeling.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest fr-682-loop-peel-peels-array-boundary-first-iteration
  "FR-682: a counted loop with an array access on the IV is peeled once before the header."
  (let* ((aref (make-vm-aref :dst :elt :array-reg :arr :index-reg :i))
         (insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :limit :value 8)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-label :name "loop")
                      (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                      (make-vm-jump-zero :reg :cond :label "exit")
                      aref
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "loop")
                      (make-vm-label :name "exit")
                      (make-vm-ret :reg :elt)))
         (out (cl-cc/optimize::opt-pass-loop-peel insts))
         (loop-pos (%test-label-position out "loop")))
    (assert-true loop-pos)
    (assert-true (> loop-pos 3))
    (assert-true (typep (nth (- loop-pos 3) out) 'cl-cc/vm::vm-lt))
    (assert-true (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-aref)
                              (cl-cc/optimize::opt-bounds-check-eliminable-marked-p inst)))
                       out))))

(deftest fr-682-loop-peel-skips-call-bearing-loop
  "FR-682: loops with calls are not peeled because first-iteration semantics may differ."
  (let* ((call (make-vm-call :dst :tmp :func :fn :args (list :i)))
         (insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :limit :value 8)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-label :name "loop")
                      (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                      (make-vm-jump-zero :reg :cond :label "exit")
                      call
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "loop")
                      (make-vm-label :name "exit")
                      (make-vm-ret :reg :tmp)))
         (out (cl-cc/optimize::opt-pass-loop-peel insts)))
    (assert-equal (mapcar #'cl-cc/vm::instruction->sexp insts)
                  (mapcar #'cl-cc/vm::instruction->sexp out))))
