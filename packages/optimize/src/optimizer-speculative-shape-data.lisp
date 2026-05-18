(in-package :cl-cc/optimize)

(defstruct (opt-lattice-value (:conc-name opt-lattice-value-))
  "Three-point SCCP/IPSCCP lattice value."
  (kind :bottom :type keyword)
  value)

(defstruct (opt-function-summary (:conc-name opt-function-summary-))
  "Small interprocedural summary used by conservative IPO helpers."
  name
  (inst-count 0 :type integer)
  (exported-p nil :type boolean)
  (importable-p t :type boolean)
  (pure-p nil :type boolean)
  (effects nil :type list)
  (constants nil :type list)
  (callees nil :type list)
  (return-lattice (opt-lattice-bottom)))

(defstruct (opt-slab-pool (:conc-name opt-slab-pool-))
  "Fixed-size object pool for cons/slab allocation modelling."
  (object-size 1 :type integer)
  (free-list nil :type list)
  (next-id 0 :type integer)
  (allocated-count 0 :type integer))

(defstruct (opt-bump-region (:conc-name opt-bump-region-))
  "Bump-pointer allocation region used by allocation planning helpers."
  (cursor 0 :type integer)
  (limit 0 :type integer)
  (marks nil :type list))

(defstruct (opt-stack-map (:conc-name opt-stack-map-))
  "Safepoint stack-map metadata: VM PC and live roots."
  (pc 0 :type integer)
  (roots nil :type list))

(defstruct (opt-guard-state (:conc-name opt-guard-state-))
  "Speculative guard state for guard weakening and deopt accounting."
  (kind :type-check :type keyword)
  (strength :full-type-check :type keyword)
  (executions 0 :type integer)
  (failures 0 :type integer))

(defstruct (opt-jit-cache-entry (:conc-name opt-jit-cache-entry-))
  "One JIT code-cache entry for conservative eviction decisions."
  id
  (size 0 :type integer)
  (warmth 0 :type integer)
  (active-p t :type boolean))

(defstruct (opt-cow-object (:conc-name opt-cow-object-))
  "Copy-on-write wrapper for planner/runtime-independent optimization metadata.

PAYLOAD is treated as immutable by readers; writers must call OPT-COW-WRITE to
ensure uniqueness before mutation.  REFCOUNT models shared aliases."
  payload
  (refcount 1 :type integer))

(defstruct (opt-module-summary (:conc-name opt-module-summary-))
  "ThinLTO-style module summary for parallel whole-program planning."
  module
  (exports nil :type list)
  (function-count 0 :type integer)
  (type-summaries nil :type list))

(defstruct (opt-sea-node (:conc-name opt-sea-node-))
  "Schedule-free Sea-of-Nodes placeholder used by MIR/SSA bridge planning."
  id
  op
  (inputs nil :type list)
  (controls nil :type list))

(defstruct (opt-deopt-frame (:conc-name opt-deopt-frame-))
  "VM materialization metadata for a deoptimization point."
  (vm-pc 0 :type integer)
  (register-map nil :type list)
  (inlined-frames nil :type list))

(defstruct (opt-osr-point (:conc-name opt-osr-point-))
  "On-Stack Replacement metadata at loop back-edge safe points."
  loop-id
  (vm-pc 0 :type integer)
  (live-registers nil :type list)
  (hotness 0 :type integer))

(defstruct (opt-shape-descriptor (:conc-name opt-shape-))
  "Hidden-class style slot layout descriptor."
  (shape-id 0 :type integer)
  (slots nil :type list)
  (offsets nil :type list))

(defstruct (opt-shape-transition-cache (:conc-name opt-shape-trans-))
  "Forward-only shape transition cache (parent-shape, slot) -> child-shape."
  (table (make-hash-table :test #'equal))
  (order nil :type list)
  (max-size 256 :type integer))
