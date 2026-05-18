(in-package :cl-cc/optimize)

(defstruct (opt-canonical-loop (:conc-name opt-loop-))
  "Canonical reducible loop slice detected from linear VM instructions."
  head-index
  cmp-index
  jz-index
  back-index
  exit-index
  head-label
  exit-label
  iv-reg
  limit-reg
  step-reg
  cond-reg
  body)

(defparameter *opt-last-affine-loop-summaries* nil
  "Last affine loop summaries produced by opt-pass-affine-loop-analysis.")
