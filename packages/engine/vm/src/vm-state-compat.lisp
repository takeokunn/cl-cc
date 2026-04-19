(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — vm2-state Compatibility Surface
;;;
;;; Contains the compatibility adapters that let vm2-state participate in the
;;; older vm-state-oriented register/output/global access API.
;;; The flat-vector interpreter core lives in vm-opcodes-run.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun make-vm-state (&key (output-stream *standard-output*))
  "Create a vm2-state. Preferred constructor for new code; vm-io-state is
for the legacy CLOS execute-instruction pipeline."
  (make-vm2-state :output-stream output-stream))

(defmethod vm-state-registers ((s vm2-state))
  (vm2-state-registers s))

(defmethod vm-output-stream ((s vm2-state))
  (vm2-state-output-stream s))

(defmethod vm-global-vars ((s vm2-state))
  (vm2-state-global-vars s))

;;; vm-reg-get/vm-reg-set dispatch on vm2-state (svref) vs vm-state (gethash)
(defun vm-reg-get (state reg)
  (if (vm2-state-p state)
      (svref (vm2-state-registers state) reg)
      (gethash reg (slot-value state 'registers) 0)))

(defun vm-reg-set (state reg value)
  (if (vm2-state-p state)
      (setf (svref (vm2-state-registers state) reg) value)
      (setf (gethash reg (slot-value state 'registers)) value))
  value)
