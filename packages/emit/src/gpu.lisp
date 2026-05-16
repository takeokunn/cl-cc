(in-package :cl-cc/emit)

(defstruct (gpu-offload-plan (:constructor make-gpu-offload-plan))
  target
  kernel-name
  grid-dims
  block-dims
  shared-memory-bytes
  lowered-ir)

(defun plan-gpu-offload (kernel-name &key (target :cuda) (grid-dims '(1 1 1))
                                      (block-dims '(1 1 1)) (shared-memory-bytes 0)
                                      (lowered-ir nil))
  "Build a backend-agnostic GPU offload plan used by CUDA/ROCm emitters.
This is a planning primitive for heterogeneous lowering (FR-438)."
  (unless (member target '(:cuda :rocm) :test #'eq)
    (error "Unsupported GPU target: ~S" target))
  (make-gpu-offload-plan :target target
                         :kernel-name kernel-name
                         :grid-dims grid-dims
                         :block-dims block-dims
                         :shared-memory-bytes shared-memory-bytes
                         :lowered-ir lowered-ir))
