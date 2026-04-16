(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest prolog-data-built-in-handler-specs
  "The built-in predicate table stays data-only and exposes the expected handlers."
  (assert-equal `((cl-cc/prolog::! cl-cc/prolog::prolog-cut-handler)
                  (,(find-symbol "AND" :cl) cl-cc/prolog::prolog-and-handler)
                  (,(find-symbol "OR" :cl) cl-cc/prolog::prolog-or-handler)
                  (,(find-symbol "=" :cl) cl-cc/prolog::prolog-unify-handler)
                  (,(find-symbol "/=" :cl) cl-cc/prolog::prolog-not-unify-handler)
                  (:when cl-cc/prolog::prolog-when-handler)
                  (,(find-symbol "WHEN" :cl) cl-cc/prolog::prolog-when-handler))
                cl-cc/prolog::*builtin-predicate-specs*))

(deftest prolog-data-peephole-rules-present
  "The peephole rule data remains available after the data/logic split."
  (assert-true (listp cl-cc/prolog::*peephole-rules*))
  (assert-true (>= (length cl-cc/prolog::*peephole-rules*) 30))
  (assert-equal '((:const cl-cc/prolog::?src cl-cc/prolog::?val)
                  (:move cl-cc/prolog::?dst cl-cc/prolog::?src)
                  ((:const cl-cc/prolog::?dst cl-cc/prolog::?val)))
                (first cl-cc/prolog::*peephole-rules*)))

;;; P5: %atomic-cut-goal-p — negative branches

(deftest prolog-builtin-atomic-cut-goal-positive
  "The bare symbol ! is recognized as an atomic cut goal."
  (assert-true (cl-cc/prolog::%atomic-cut-goal-p '!)))

(deftest prolog-builtin-atomic-cut-goal-non-cut-symbol
  "A non-cut symbol is not an atomic cut goal."
  (assert-false (cl-cc/prolog::%atomic-cut-goal-p 'foo)))

(deftest prolog-builtin-atomic-cut-goal-prolog-goal-struct
  "A prolog-goal struct (even with predicate !) is not an atomic cut goal."
  (let ((goal (cl-cc/prolog::make-prolog-goal :predicate '! :args nil)))
    (assert-false (cl-cc/prolog::%atomic-cut-goal-p goal))))

(deftest prolog-builtin-atomic-cut-goal-non-symbol
  "A non-symbol value is not an atomic cut goal."
  (assert-false (cl-cc/prolog::%atomic-cut-goal-p 42))
  (assert-false (cl-cc/prolog::%atomic-cut-goal-p '(list)))
  (assert-false (cl-cc/prolog::%atomic-cut-goal-p nil)))

;;; P6a: %goal-predicate-and-args — struct branch

(deftest prolog-builtin-goal-predicate-and-args-list
  "For a raw list goal, returns (car . cdr)."
  (multiple-value-bind (pred args)
      (cl-cc/prolog::%goal-predicate-and-args '(member ?x (1 2)))
    (assert-eq 'member pred)
    (assert-equal '(?x (1 2)) args)))

(deftest prolog-builtin-goal-predicate-and-args-struct
  "For a prolog-goal struct, returns (goal-predicate . goal-args)."
  (let ((goal (cl-cc/prolog::make-prolog-goal :predicate 'foo :args '(1 2))))
    (multiple-value-bind (pred args)
        (cl-cc/prolog::%goal-predicate-and-args goal)
      (assert-eq 'foo pred)
      (assert-equal '(1 2) args))))

;;; P6b: %invoke-builtin-goal — predicate miss

(deftest prolog-builtin-invoke-missing-predicate
  "Invoking a non-existent builtin returns NIL (no handler found)."
  (assert-null (cl-cc/prolog::%invoke-builtin-goal 'nonexistent-predicate-xyz nil nil (lambda (env) env))))
