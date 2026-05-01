;;;; framework-pbt.lisp — Property-Based Test DSL
;;;;
;;;; %pbt-run, assert-pbt, deftest-pbt: statistical property testing
;;;; integrated with the compiler test helpers in framework-compiler.lisp.
;;;;
;;;; Load order: after framework-compiler.lisp.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Section N-1: Property-Based Test DSL
;;; ------------------------------------------------------------
;;;
;;; Many compiler correctness properties follow the same shape:
;;;   generate N random inputs, compile+run each, count passes, assert ratio.
;;; deftest-pbt captures that shape so tests read as specifications.

(defun %pbt-run (trials threshold check-fn)
  "Run CHECK-FN TRIALS times; assert the success ratio meets THRESHOLD.
Returns the pass ratio for inspection."
  (let ((passes 0))
    (dotimes (i trials)
      (when (funcall check-fn) (incf passes)))
    (let ((ratio (/ passes trials)))
      (assert-true (>= ratio threshold))
      ratio)))

(defmacro assert-pbt ((&key (trials 50) (threshold 0.90)) generate &body check)
  "Assert a property holds with probability >= THRESHOLD over TRIALS random samples.
GENERATE is a LET* binding list; CHECK body runs in that scope and returns boolean."
  `(%pbt-run ,trials ,threshold
             (lambda ()
               (let* ,generate
                 ,@check))))

(defmacro deftest-pbt (name docstring &key (trials 50) (threshold 0.90) generate check)
  "Define a property-based test. See assert-pbt for argument semantics."
  `(deftest ,name
     ,docstring
     (assert-pbt (:trials ,trials :threshold ,threshold) ,generate ,check)))
