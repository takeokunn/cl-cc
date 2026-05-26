(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest fr-353-shrink-integer-minimizes-failing-counterexample
  "FR-353: integer shrinking minimizes a failing input toward the failure boundary."
  :tags '(:fr-353)
  (assert-eql 4
              (minimize-failing-input 37
                                      (lambda (value) (> value 3)))))

(deftest fr-353-shrink-list-removes-elements-and-shrinks-members
  "FR-353: list shrinking deletes chunks and recursively shrinks member values."
  :tags '(:fr-353)
  (let ((candidates (shrink '(8 2))))
    (assert-true (member nil candidates :test #'equal))
    (assert-true (member '(2) candidates :test #'equal))
    (assert-true (member '(8) candidates :test #'equal))
    (assert-true (member '(0 2) candidates :test #'equal))))

(deftest fr-353-property-check-reports-minimized-failure
  "FR-353: property checks report the original and minimized counterexample."
  :tags '(:fr-353)
  (let ((result (check-property-with-shrinking
                 (lambda () 42)
                 (lambda (value) (<= value 5))
                 :trials 1)))
    (assert-eq :fail (getf result :status))
    (assert-eql 42 (getf result :value))
    (assert-eql 6 (getf result :minimized-value))))

(deftest fr-353-integer-shrinking-binary-searches-boundary
  "FR-353: integer shrinking deterministically binary-searches to the minimal failing boundary."
  :tags '(:fr-353)
  (assert-eql 6
              (minimize-failing-input 1000
                                      (lambda (value) (> value 5))))
  (assert-eql -6
              (minimize-failing-input -1000
                                      (lambda (value) (< value -5)))))

(deftest fr-353-type-annotation-generators-derive-values
  "FR-353: type annotations resolve to registered property generators."
  :tags '(:fr-353)
  (deftype-generator fr-353-small-positive-integer
    7)
  (assert-type integer (generate-from-type-annotation 'integer))
  (assert-eql 7 (generate-for-type 'fr-353-small-positive-integer))
  (assert-true (functionp (generator-for-type 'fr-353-small-positive-integer))))

(deftest fr-353-stateful-command-sequences-respect-preconditions
  "FR-353: stateful PBT command generation emits executable command sequences."
  :tags '(:fr-353)
  (let* ((inc (make-pbt-command
               :name :inc
               :precondition (lambda (state) (< state 3))
               :run (lambda (state) (1+ state))))
         (dec (make-pbt-command
               :name :dec
               :precondition (lambda (state) (> state 0))
               :run (lambda (state) (1- state))))
         (sequence (generate-stateful-command-sequence (list inc dec) 0 :length 20)))
    (multiple-value-bind (final-state trace)
        (run-stateful-command-sequence sequence 0)
      (assert-true (<= 0 final-state 3))
      (assert-equal (length sequence) (length trace))
      (assert-true (every (lambda (name) (member name '(:inc :dec))) trace)))))

(deftest fr-353-stateful-runner-detects-invalid-command
  "FR-353: stateful PBT execution rejects command sequences that violate preconditions."
  :tags '(:fr-353)
  (let ((dec (make-pbt-command
              :name :dec
              :precondition (lambda (state) (> state 0))
              :run (lambda (state) (1- state)))))
    (assert-signals error
      (run-stateful-command-sequence (list dec) 0))))
