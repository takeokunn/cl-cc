(in-package :cl-cc/optimize)

(defstruct (opt-partial-specialization (:conc-name opt-partial-spec-))
  "Residual helper result for conservative constant-argument specialization."
  original-name
  specialized-name
  (signature nil :type list)
  (static-args nil :type list)
  (dynamic-args nil :type list)
  (residual-body nil :type list))

(defstruct (opt-partial-eval-result (:conc-name opt-partial-eval-))
  "Function-level partial-evaluation report used by FR-209/210 orchestration."
  function-name
  (parameters nil :type list)
  (signature nil :type list)
  (binding-times nil :type list)
  (form-kinds nil :type list)
  (residual-body nil :type list)
  (dynamic-body nil :type list)
  specialization)

(defstruct (opt-partial-program-result (:conc-name opt-partial-program-))
  "Program/module-level partial-evaluation report keyed by function name.

FUNCTION-RESULTS is an alist of:
  (function-name . opt-partial-eval-result)"
  (function-results nil :type list))

(defstruct (opt-binding-time (:conc-name opt-binding-time-))
  "Binding-time classification for one parameter under the SCCP lattice."
  parameter
  (kind :dynamic :type keyword)
  value
  lattice)

(defstruct (opt-specialization-plan (:conc-name opt-specialization-plan-))
  "Known-callee specialization plan keyed by a constant-argument signature."
  callee-label
  specialized-name
  (signature nil :type list)
  (static-args nil :type list)
  (dynamic-args nil :type list)
  (clone-needed-p nil :type boolean)
  (cache-hit-p nil :type boolean))

(defparameter *opt-offline-bta-pure-operators*
  '(+ - * / 1+ 1- = /= < > <= >= min max abs
    logand logior logxor lognot ash
    eq eql equal not and or)
  "Conservative operator set considered static-evaluable in offline BTA.")
