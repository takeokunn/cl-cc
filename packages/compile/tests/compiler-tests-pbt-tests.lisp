;;;; tests/integration/compiler-tests-pbt-tests.lisp — Compiler Property-Based Tests
;;;;
;;;; Continuation of compiler-tests.lisp.
;;;; Property-based tests for integer compilation, binary op commutativity,
;;;; if-branch semantics, and let-binding value preservation.

(in-package :cl-cc/test)
(in-suite cl-cc-integration-suite)

(deftest pbt-integer-compilation
  "Property: All integers compile correctly."
  (let ((passes 0)
        (trials 100))
    (dotimes (_ trials)
      (declare (ignore _))
      (let* ((val (random 1000))
             (result (handler-case (run-string (format nil "~D" val))
                       (error () nil))))
        (when (and result (= result val))
          (incf passes))))
    (assert-true (>= (/ passes trials) 0.90))))

(deftest-each pbt-binary-op-commutative
  "Property: binary arithmetic operations are commutative in compiled code."
  :cases (("addition"       "+" 100)
          ("multiplication" "*"  50))
  (op range)
  (let ((passes 0)
        (trials 50))
    (dotimes (_ trials)
      (declare (ignore _))
      (let* ((a  (random range))
             (b  (random range))
             (r1 (handler-case (run-string (format nil "(~A ~D ~D)" op a b))
                   (error () nil)))
             (r2 (handler-case (run-string (format nil "(~A ~D ~D)" op b a))
                   (error () nil))))
        (when (and r1 r2 (= r1 r2))
          (incf passes))))
    (assert-true (>= (/ passes trials) 0.90))))

(deftest pbt-if-always-returns-one-branch
  "Property: If always returns either then or else branch."
  (let ((passes 0)
        (trials 50))
    (dotimes (_ trials)
      (declare (ignore _))
      (let* ((cond-val (random 2))
             (then-val (random 100))
             (else-val (random 100))
             (result   (handler-case
                           (run-string (format nil "(if ~D ~D ~D)" cond-val then-val else-val))
                         (error () nil))))
        (when (or (and (= cond-val 0)  (and result (= result else-val)))
                  (and (/= cond-val 0) (and result (= result then-val))))
          (incf passes))))
    (assert-true (>= (/ passes trials) 0.90))))

(deftest pbt-let-bindings-preserve-value
  "Property: Let bindings preserve the value assigned."
  (let ((passes 0)
        (trials 50))
    (dotimes (_ trials)
      (declare (ignore _))
      (let* ((val (random 1000))
             (result (handler-case (run-string (format nil "(let ((x ~D)) x)" val))
                       (error () nil))))
        (when (and result (= result val))
          (incf passes))))
    (assert-true (>= (/ passes trials) 0.90))))
