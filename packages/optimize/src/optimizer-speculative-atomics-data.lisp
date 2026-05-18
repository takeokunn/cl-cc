(in-package :cl-cc/optimize)

(defstruct (opt-tls-plan (:conc-name opt-tls-plan-))
  "Thread-local access lowering plan."
  target
  (uses-inline-tls-p nil :type boolean)
  (base-register nil :type (or null keyword)))

(defstruct (opt-atomic-plan (:conc-name opt-atomic-plan-))
  "Atomic lowering plan for architecture + memory ordering."
  target
  operation
  memory-order
  opcode)

(defstruct (opt-htm-plan (:conc-name opt-htm-plan-))
  "Hardware Transactional Memory lock-elision plan."
  target
  (uses-htm-p nil :type boolean)
  (begin-opcode nil :type (or null keyword))
  (end-opcode nil :type (or null keyword))
  (abort-opcode nil :type (or null keyword))
  (fallback-lock-p t :type boolean))

(defstruct (opt-concurrent-gc-plan (:conc-name opt-conc-gc-plan-))
  "Concurrent GC planning record."
  (concurrent-mark-p t :type boolean)
  (write-barrier :satb :type keyword)
  (mutator-assist-p t :type boolean)
  (stw-phases '(:initial-mark :final-remark) :type list))
