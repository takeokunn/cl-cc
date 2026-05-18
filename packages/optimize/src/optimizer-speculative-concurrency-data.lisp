(in-package :cl-cc/optimize)

(defstruct (opt-async-state-machine (:conc-name opt-async-sm-))
  "Minimal async/await lowering plan as an explicit state machine graph."
  entry-state
  (states nil :type list)
  (await-points nil :type list)
  (transitions nil :type list))

(defstruct (opt-channel-site (:conc-name opt-chan-site-))
  "Channel/CSP optimization metadata for one send/recv site."
  (buffer-size 0 :type integer)
  (queue-depth 0 :type integer)
  (contention 0 :type integer)
  (select-arity 1 :type integer))

(defstruct (opt-stm-plan (:conc-name opt-stm-plan-))
  "STM lowering plan for one `(atomically ...)` region."
  (reads nil :type list)
  (writes nil :type list)
  (pure-p nil :type boolean)
  (inline-log-p t :type boolean))

(defstruct (opt-lockfree-plan (:conc-name opt-lockfree-plan-))
  "Lock-free lowering support plan for CAS-based data structures."
  (operation :cas :type keyword)
  (aba-risk-p nil :type boolean)
  (reclamation :none :type keyword))
