(in-package :cl-cc/optimize)

(defparameter +opt-strength-reduce-max-verified-dividend-hi+ 65535
  "Largest dividend upper bound FR-282 verifies exhaustively for reciprocal division.

Non-power-of-two division strength reduction stays intentionally conservative:
the pass first searches exact multiply/shift reciprocals for small, finite
intervals. For proved non-negative 64-bit word ranges it falls back to
unsigned magic-number lowering with vm-integer-mul-high-u. Unknown or
out-of-word ranges keep vm-div.")

(defconstant +opt-strength-reduce-word64-bits+ 64
  "Native word width used by FR-282 unsigned magic-number division.")

(defconstant +opt-strength-reduce-word64-modulus+
  (ash 1 +opt-strength-reduce-word64-bits+)
  "2^64, used when deriving fixed-width unsigned division magic numbers.")

(defconstant +opt-strength-reduce-word64-max+
  (1- +opt-strength-reduce-word64-modulus+)
  "Largest unsigned 64-bit dividend accepted by FR-282 magic lowering.")
