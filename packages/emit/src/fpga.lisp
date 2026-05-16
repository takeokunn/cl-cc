(in-package :cl-cc/emit)

(defstruct (fpga-hls-plan (:constructor make-fpga-hls-plan))
  module-name
  pipeline-initiation-interval
  target-frequency-mhz
  resource-budget
  rtl-kind
  lowered-ops)

(defun plan-fpga-hls (module-name lowered-ops &key (pipeline-initiation-interval 1)
                                                  (target-frequency-mhz 250)
                                                  resource-budget
                                                  (rtl-kind :verilog))
  "Build a high-level synthesis planning artifact for FPGA export (FR-442)."
  (unless (member rtl-kind '(:verilog :vhdl) :test #'eq)
    (error "Unsupported FPGA RTL kind: ~S" rtl-kind))
  (make-fpga-hls-plan :module-name module-name
                      :pipeline-initiation-interval pipeline-initiation-interval
                      :target-frequency-mhz target-frequency-mhz
                      :resource-budget resource-budget
                      :rtl-kind rtl-kind
                      :lowered-ops lowered-ops))
