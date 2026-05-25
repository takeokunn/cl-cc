;;;; runtime-stdlib-3-sequence-tests.lisp — FR-1076 sequence completion

(in-package :cl-cc/test)

(defsuite runtime-stdlib-3-sequence-suite
  :description "Runtime stdlib sequence operation completion tests"
  :parent cl-cc-unit-suite)

(in-suite runtime-stdlib-3-sequence-suite)

(deftest sequence-every-some-accept-multiple-sequences
  "EVERY/SOME/NOTANY/NOTEVERY accept &rest sequence arguments."
  (assert-true (%tree-contains-head-p 'apply (our-macroexpand-1 '(every #'< '(1 2) '(3 4)))))
  (assert-true (%tree-contains-head-p 'apply (our-macroexpand-1 '(some #'= '(1 2) '(0 2)))))
  (assert-eq 'not (car (our-macroexpand-1 '(notany #'= '(1) '(2)))))
  (assert-eq 'not (car (our-macroexpand-1 '(notevery #'< '(1) '(2))))))

(deftest sequence-substitute-count-from-end-expands
  "SUBSTITUTE/NSUBSTITUTE preserve :COUNT and :FROM-END keyword support."
  (let ((expanded (our-macroexpand-1 '(substitute 9 1 '(1 2 1) :count 1 :from-end t))))
    (assert-eq 'let* (car expanded)))
  (assert-equal (our-macroexpand-1 '(nsubstitute 9 1 xs :count 1 :from-end t))
                '(substitute 9 1 xs :count 1 :from-end t)))

(deftest sequence-mismatch-start-end-expands
  "MISMATCH supports START/END keyword arguments."
  (let ((expanded (our-macroexpand-1 '(mismatch xs ys :start1 1 :end1 3 :start2 2 :end2 4))))
    (assert-eq 'let (car expanded))))

(deftest sequence-remove-duplicates-from-end-expands
  "REMOVE-DUPLICATES accepts :FROM-END."
  (let ((expanded (our-macroexpand-1 '(remove-duplicates xs :from-end t))))
    (assert-eq 'reverse (car expanded))))

(deftest sequence-concatenate-coerces-inputs-for-list-vector
  "CONCATENATE normalizes non-list inputs before APPEND for list/vector results."
  (let ((list-exp (our-macroexpand-1 '(concatenate 'list #(1 2) '(3))))
        (vec-exp  (our-macroexpand-1 '(concatenate 'vector '(1) #(2)))))
    (assert-true (%tree-contains-head-p 'coerce list-exp))
    (assert-true (%tree-contains-head-p 'coerce vec-exp))))

(deftest loop-arithmetic-iota-and-type-hints
  "FR-1124: LOOP recognizes simple arithmetic collect and typed iteration hints."
  (assert-eq 'iota (car (our-macroexpand-1 '(loop for i from 0 below n collect i))))
  (let ((typed (our-macroexpand-1 '(loop for i of-type fixnum from 0 below n sum i))))
    (assert-true (%tree-contains-head-p 'the typed))))
