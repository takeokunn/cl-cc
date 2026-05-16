(in-package :cl-cc/emit)

(defstruct (apple-ane-plan (:constructor make-apple-ane-plan))
  model-name
  ops
  input-shapes
  output-shapes
  precision
  backend)

(defun plan-apple-ane-offload (model-name ops &key input-shapes output-shapes
                                            (precision :fp16) (backend :coreml))
  "Build an Apple ANE/Core ML execution plan for ML-like kernels (FR-439)."
  (unless (member backend '(:coreml :ane) :test #'eq)
    (error "Unsupported Apple ANE backend: ~S" backend))
  (make-apple-ane-plan :model-name model-name
                       :ops ops
                       :input-shapes input-shapes
                       :output-shapes output-shapes
                       :precision precision
                       :backend backend))
