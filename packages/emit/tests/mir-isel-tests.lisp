;;;; packages/emit/tests/mir-isel-tests.lisp — MIR pipeline / ISel smoke tests

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest mir-isel-maximal-munch-prefers-largest-x86-tile
  "FR-175: maximal munch selects the largest matching x86-64 LEA tile."
  (let* ((tree '(:add (:reg r1) (:add (:mul (:reg r2) (:const 4)) (:const 8))))
         (tiles (cl-cc/codegen:isel-maximal-munch tree :x86-64))
         (root-rule (car (last tiles))))
    (assert-eq :x86-lea-address
               (cl-cc/codegen:isel-rule-name (car root-rule)))))

(deftest mir-isel-maximal-munch-prefers-aarch64-scaled-address
  "FR-175: maximal munch selects the largest matching AArch64 scaled address tile."
  (let* ((tree '(:add (:reg x1) (:mul (:reg x2) (:const 8))))
         (tiles (cl-cc/codegen:isel-maximal-munch tree :aarch64))
         (root-rule (car (last tiles))))
    (assert-eq :a64-add-scaled-address
               (cl-cc/codegen:isel-rule-name (car root-rule)))))

(deftest mir-vm-program-roundtrip-preserves-unsupported-instructions
  "FR-057/FR-299: unsupported VM ops round-trip via MIR metadata fallback."
  (let* ((instructions (list (make-vm-const :dst 0 :value 1)
                             (make-vm-print :reg 0)
                             (make-vm-halt :reg 0)))
         (program (make-vm-program :instructions instructions :result-register 0))
         (selected (cl-cc/codegen:isel-vm-program program :target :x86-64)))
    (assert-= (length instructions)
              (length (vm-program-instructions selected)))
    (assert-true (typep (second (vm-program-instructions selected)) 'vm-print))))

(deftest mir-vm-program-constant-folds-before-isel
  "FR-057: MIR SSA optimization folds constants before selecting VM output."
  (let* ((program (make-vm-program
                   :instructions (list (make-vm-const :dst 0 :value 2)
                                       (make-vm-const :dst 1 :value 3)
                                       (make-vm-add :dst 2 :lhs 0 :rhs 1)
                                       (make-vm-halt :reg 2))
                   :result-register 2))
         (selected (cl-cc/codegen:isel-vm-program program :target :x86-64))
         (insts (vm-program-instructions selected)))
    (assert-true (typep (third insts) 'vm-const))
    (assert-equal 5 (vm-value (third insts)))))

(deftest mir-pipeline-flag-compilation-produces-same-vm-stream-shape
  "FR-299: enabling the MIR path keeps output in the existing VM instruction format."
  (let* ((result (cl-cc/compile:compile-toplevel-forms '((+ 1 2)) :target :x86_64))
         (program (cl-cc/compile:compilation-result-program result))
         (selected (cl-cc/codegen:isel-vm-program program :target :x86-64)))
    (assert-true (typep selected 'vm-program))
    (assert-true (every (lambda (inst) (typep inst 'vm-instruction))
                        (vm-program-instructions selected)))))
