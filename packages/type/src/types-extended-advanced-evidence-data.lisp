;;;; types-extended-advanced-evidence-data.lisp — Implementation evidence table
;;;; Extracted from types-extended-advanced-data.lisp.
;;;; Load order: after types-extended-advanced-data.
(in-package :cl-cc/type)

(defparameter +type-advanced-implementation-evidence-specs+
  (append
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-1501"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier type-union-p type-union-types)
     '(advanced-null-safety-option-parses-nullable-union advanced-feature-implementation-evidence-covers-all-fr-ids))
    (%type-advanced-implementation-evidence-spec
     "FR-1502"
     '("packages/type/src/types-extended-nodes.lisp")
     '(register-concurrency-traits sendable-type-p shareable-type-p validate-spawn-argument validate-shared-reference)
     '(concurrency-send-sync-registry-is-concrete concrete-generics-registry-and-structural-traversal-work))
    (%type-advanced-implementation-evidence-spec
     "FR-1503"
     '("packages/type/src/types-extended-nodes.lisp")
     '(type-advanced-security-label<= make-labeled-value sanitize-labeled-value declassify-labeled-value labeled-value-flow-allowed-p)
     '(advanced-information-flow-enforces-security-lattice security-label-lattice-and-declassification-are-enforced))
    (%type-advanced-implementation-evidence-spec
     "FR-1504"
     '("packages/type/src/types-extended-nodes.lisp")
     '(with-region region-alloc region-ref-valid-p region-deref)
     '(region-tokens-enforce-lifetimes concrete-utility-type-helpers-operate-on-nats-strings-and-record-transforms))
    (%type-advanced-implementation-evidence-spec
     "FR-1505"
     '("packages/type/src/types-extended-nodes.lisp")
     '(make-capability restrict-capability capability-allows-p capability-implies-p capability-effects)
     '(capabilities-support-implication-restriction-and-effects concrete-utility-type-helpers-operate-on-nats-strings-and-record-transforms)))
   (%type-advanced-implementation-evidence-specs
    '("FR-1601" "FR-1603" "FR-1604" "FR-1605")
    '("packages/type/src/types-extended-nodes.lisp")
    '(make-type-advanced type-children type-free-vars zonk type-unify)
    '(advanced-node-children-and-free-vars-follow-nested-type-payload
      advanced-node-zonk-updates-args-properties-and-evidence
      advanced-node-unification-is-structural-and-fr-scoped
      concrete-generics-registry-and-structural-traversal-work))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-1602"
     '("packages/type/src/generics.lisp")
     '(register-generic-instance lookup-generic-instance generic-representation-of generic-show generic-transform generic-query generic-representation-valid-p)
     '(concrete-generics-registry-and-structural-traversal-work
      advanced-feature-implementation-evidence-covers-all-fr-ids)))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-1606"
     '("packages/type/src/types-extended-nodes.lisp")
     '(make-type-advanced validate-type-advanced %type-advanced-validate-incremental-checking)
     '(advanced-contracts-enforce-incremental-staging-optics-and-test-generation
      advanced-feature-semantic-completion-requires-implementation-evidence)))
   (%type-advanced-implementation-evidence-specs
    '("FR-1701" "FR-1702" "FR-1704" "FR-1705")
    '("packages/type/src/utils.lisp" "packages/type/src/types-extended-nodes.lisp")
    '(make-type-level-natural type-level-natural-value make-type-level-string template-literal-type)
     '(concrete-utility-type-helpers-operate-on-nats-strings-and-record-transforms
       advanced-feature-implementation-evidence-covers-all-fr-ids))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-1703"
     '("packages/type/src/types-extended-nodes.lisp")
     '(make-type-advanced validate-type-advanced %type-advanced-validate-staging)
     '(advanced-contracts-enforce-incremental-staging-optics-and-test-generation
      advanced-feature-implementation-evidence-covers-all-fr-ids)))
   (%type-advanced-implementation-evidence-specs
    '("FR-1802" "FR-1803" "FR-1804" "FR-1805" "FR-1806")
    '("packages/type/src/utils.lisp" "packages/type/src/types-extended-nodes.lisp")
    '(make-hlist-type hlist-head-type hlist-tail-type format-type parse-type-specifier)
     '(concrete-utility-type-helpers-operate-on-nats-strings-and-record-transforms
       advanced-feature-parser-roundtrips))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-1801"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier validate-type-advanced %type-advanced-validate-optics)
     '(advanced-contracts-enforce-incremental-staging-optics-and-test-generation
      advanced-feature-implementation-evidence-covers-all-fr-ids)))
   (%type-advanced-implementation-evidence-specs
    '("FR-1901" "FR-1902" "FR-1903" "FR-1904" "FR-1905" "FR-1906")
    '("packages/type/src/types-extended-nodes.lisp")
    '(make-termination-evidence termination-evidence-valid-p make-proof-evidence verify-proof-obligation verify-proof-evidence)
    '(termination-and-pcc-evidence-check-real-obligations
      advanced-validators-now-use-concrete-semantic-modules))
   (%type-advanced-implementation-evidence-specs
    '("FR-2001" "FR-2002" "FR-2003" "FR-2004" "FR-2005")
    '("packages/type/src/types-extended-nodes.lisp")
    '(make-proof-carrying-code verify-proof-carrying-code make-proof-evidence verify-proof-evidence)
    '(advanced-proof-like-features-require-evidence
      termination-and-pcc-evidence-check-real-obligations))
   (%type-advanced-implementation-evidence-specs
    '("FR-2102" "FR-2104" "FR-2105" "FR-2106" "FR-2107")
    '("packages/type/src/types-extended-nodes.lisp")
    '(parse-type-specifier make-type-advanced validate-type-advanced)
    '(advanced-feature-parser-roundtrips advanced-feature-implementation-evidence-covers-all-fr-ids))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-2101"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier validate-type-advanced %type-advanced-validate-test-generation)
     '(advanced-contracts-enforce-incremental-staging-optics-and-test-generation
      advanced-feature-implementation-evidence-covers-all-fr-ids))
    (%type-advanced-implementation-evidence-spec
     "FR-2103"
     '("packages/type/src/types-extended-nodes.lisp")
     '(make-ffi-scalar-type make-ffi-pointer-type make-ffi-callback-type make-ffi-function-descriptor ffi-type-valid-p)
     '(advanced-validation-rejects-malformed-units-routes-and-ffi
      ffi-descriptors-validate-recursively)))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-2201"
     '("packages/type/src/coroutines.lisp")
     '(make-type-advanced parse-type-specifier validate-type-advanced)
     '(advanced-feature-parser-roundtrips concrete-coroutines-generators-and-coroutines-enforce-runtime-types))
    (%type-advanced-implementation-evidence-spec
     "FR-2202"
     '("packages/type/src/channels.lisp")
     '(make-typed-channel channel-send channel-recv close-typed-channel)
     '(concrete-channels-enforce-capacity-type-and-close-semantics advanced-feature-implementation-evidence-covers-all-fr-ids))
    (%type-advanced-implementation-evidence-spec
     "FR-2203"
     '("packages/type/src/actors.lisp")
     '(make-actor-ref actor-message-accepted-p actor-send actor-stop)
     '(concrete-actors-accept-typed-messages-and-stop-cleanly advanced-feature-implementation-evidence-covers-all-fr-ids))
    (%type-advanced-implementation-evidence-spec
     "FR-2204"
     '("packages/type/src/stm.lisp")
     '(make-tvar stm-read stm-write stm-bind atomically)
     '(concrete-stm-actions-sequence-and-reject-io-effects advanced-feature-implementation-evidence-covers-all-fr-ids))
    (%type-advanced-implementation-evidence-spec
     "FR-2205"
     '("packages/type/src/coroutines.lisp")
     '(make-generator generator-next make-coroutine coroutine-resume)
     '(concrete-coroutines-generators-and-coroutines-enforce-runtime-types advanced-feature-implementation-evidence-covers-all-fr-ids))
    (%type-advanced-implementation-evidence-spec
     "FR-2206"
     '("packages/type/src/simd.lisp")
     '(make-simd-vector simd-map simd-add)
     '(concrete-simd-vectors-preserve-lanes-and-element-types advanced-feature-implementation-evidence-covers-all-fr-ids)))
   (%type-advanced-implementation-evidence-specs
    '("FR-2301" "FR-2303" "FR-2304" "FR-2305")
    '("packages/type/src/types-extended-nodes.lisp")
    '(parse-type-specifier make-type-advanced validate-type-advanced)
    '(advanced-feature-parser-roundtrips advanced-feature-implementation-evidence-covers-all-fr-ids))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-2302"
     '("packages/type/src/types-extended-nodes.lisp")
     '(make-measure measure+ measure/ convert-unit unit-compatible-p)
     '(units-of-measure-perform-dimension-checking advanced-validation-rejects-malformed-units-routes-and-ffi)))
   (%type-advanced-implementation-evidence-specs
    '("FR-2401" "FR-2402" "FR-2403" "FR-2404")
    '("packages/type/src/types-extended-nodes.lisp")
    '(type-unify parse-type-specifier validate-type-advanced)
    '(advanced-node-unification-is-structural-and-fr-scoped advanced-feature-parser-roundtrips))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-2405"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier validate-type-advanced %type-advanced-validate-interface-files)
     '(advanced-contracts-enforce-constraint-analysis-and-tooling-families
      advanced-feature-implementation-evidence-covers-all-fr-ids))
    (%type-advanced-implementation-evidence-spec
     "FR-2406"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier validate-type-advanced %type-advanced-validate-smt-integration)
     '(advanced-contracts-enforce-constraint-analysis-and-tooling-families
      advanced-feature-implementation-evidence-covers-all-fr-ids)))
   (%type-advanced-implementation-evidence-specs
    '("FR-2503" "FR-2504" "FR-2505")
    '("packages/type/src/types-extended-nodes.lisp")
    '(parse-type-specifier make-type-advanced type-advanced-valid-p)
    '(advanced-feature-parser-roundtrips advanced-feature-implementation-evidence-covers-all-fr-ids))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-2501"
     '("packages/type/src/types-extended-nodes.lisp")
     '(make-type-dynamic make-type-advanced type-unify)
     '(advanced-node-unification-is-structural-and-fr-scoped
      advanced-feature-semantic-completion-requires-implementation-evidence))
    (%type-advanced-implementation-evidence-spec
     "FR-2502"
     '("packages/type/src/types-extended-nodes.lisp")
     '(make-type-type-rep parse-type-specifier validate-type-advanced)
     '(advanced-feature-parser-roundtrips advanced-feature-implementation-evidence-covers-all-fr-ids)))
   (%type-advanced-implementation-evidence-specs
    '("FR-2601" "FR-2602" "FR-2604" "FR-2605" "FR-2606")
    '("packages/type/src/types-extended-nodes.lisp")
    '(parse-type-specifier make-type-advanced type-advanced-valid-p)
    '(advanced-feature-parser-roundtrips advanced-feature-implementation-evidence-covers-all-fr-ids))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-2603"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier make-type-advanced validate-type-advanced)
     '(advanced-feature-parser-roundtrips advanced-feature-implementation-evidence-covers-all-fr-ids)))
   (%type-advanced-implementation-evidence-specs
    '("FR-2701" "FR-2702" "FR-2703" "FR-2704" "FR-2705" "FR-2706")
    '("packages/type/src/types-extended-nodes.lisp")
    '(parse-type-specifier make-type-advanced validate-type-advanced)
    '(advanced-feature-parser-roundtrips advanced-feature-implementation-evidence-covers-all-fr-ids))
   (%type-advanced-implementation-evidence-specs
    '("FR-2801" "FR-2802" "FR-2803" "FR-2805")
    '("packages/type/src/types-extended-nodes.lisp")
    '(parse-type-specifier make-type-advanced validate-type-advanced)
    '(advanced-feature-parser-roundtrips advanced-feature-implementation-evidence-covers-all-fr-ids))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-2804"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier validate-type-advanced %type-advanced-validate-abstract-interpretation)
     '(advanced-contracts-enforce-constraint-analysis-and-tooling-families
      advanced-feature-implementation-evidence-covers-all-fr-ids)))
   (%type-advanced-implementation-evidence-specs
    '("FR-2901" "FR-2903" "FR-2904" "FR-2905")
    '("packages/type/src/types-extended-nodes.lisp")
    '(parse-type-specifier make-type-advanced validate-type-advanced)
    '(advanced-feature-parser-roundtrips advanced-feature-implementation-evidence-covers-all-fr-ids))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-2902"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier validate-type-advanced %type-advanced-validate-alias-analysis)
     '(advanced-contracts-enforce-constraint-analysis-and-tooling-families
      advanced-feature-implementation-evidence-covers-all-fr-ids)))
   (%type-advanced-implementation-evidence-specs
    '("FR-3001" "FR-3004" "FR-3005")
    '("packages/type/src/types-extended-nodes.lisp")
    '(parse-type-specifier make-type-advanced validate-type-advanced)
    '(advanced-feature-parser-roundtrips advanced-feature-implementation-evidence-covers-all-fr-ids))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-3002"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier validate-type-advanced %type-advanced-validate-plugins)
     '(advanced-contracts-enforce-constraint-analysis-and-tooling-families
      advanced-feature-implementation-evidence-covers-all-fr-ids))
    (%type-advanced-implementation-evidence-spec
     "FR-3003"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier validate-type-advanced %type-advanced-validate-synthesis)
     '(advanced-contracts-enforce-constraint-analysis-and-tooling-families
      advanced-feature-implementation-evidence-covers-all-fr-ids)))
   (%type-advanced-implementation-evidence-specs
    '("FR-3102" "FR-3103" "FR-3104")
    '("packages/type/src/types-extended-nodes.lisp")
    '(parse-type-specifier make-type-advanced validate-type-advanced)
     '(advanced-feature-parser-roundtrips advanced-feature-implementation-evidence-covers-all-fr-ids))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-3101"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier make-type-advanced validate-type-advanced)
     '(advanced-feature-parser-roundtrips advanced-feature-implementation-evidence-covers-all-fr-ids)))
   (%type-advanced-implementation-evidence-specs
    '("FR-3201" "FR-3202" "FR-3203" "FR-3204")
    '("packages/type/src/types-extended-nodes.lisp")
    '(parse-type-specifier make-type-advanced validate-type-advanced)
    '(advanced-feature-parser-roundtrips advanced-feature-implementation-evidence-covers-all-fr-ids))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-3205"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier validate-type-advanced %type-advanced-validate-brand)
     '(advanced-graded-and-branded-types-have-semantic-shape
      advanced-feature-implementation-evidence-covers-all-fr-ids)))
   (%type-advanced-implementation-evidence-specs
    '("FR-3301" "FR-3302" "FR-3303" "FR-3304")
    '("packages/type/src/utils.lisp" "packages/type/src/types-extended-nodes.lisp")
    '(readonly-type partial-type required-type pick-type omit-type exclude-type extract-type non-nullable-type return-type-of)
     '(advanced-contracts-enforce-typescript-encodings-effects-and-equality
      concrete-utility-type-helpers-operate-on-nats-strings-and-record-transforms))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-3305"
     '("packages/type/src/routing.lisp" "packages/type/src/types-extended-nodes.lisp")
     '(make-api-type api-route-lookup route-response-type-for make-route build-route-path match-route-path)
     '(routing-validates-path-parameters-and-roundtrips
      concrete-routing-api-lookup-and-response-type-work)))
   (list
    (%type-advanced-implementation-evidence-spec
     "FR-3401"
     '("packages/type/src/types-extended-nodes.lisp")
     '(make-qtt-semiring make-graded-value make-qtt-binding valid-multiplicity-p qtt-erased-p)
     '(qtt-and-graded-semantics-check-semiring-behavior
      advanced-contracts-enforce-typescript-encodings-effects-and-equality))
    (%type-advanced-implementation-evidence-spec
     "FR-3402"
     '("packages/type/src/types-extended-nodes.lisp")
     '(make-graded-value graded-add graded-compose valid-multiplicity-p)
     '(qtt-and-graded-semantics-check-semiring-behavior
      advanced-graded-and-branded-types-have-semantic-shape))
    (%type-advanced-implementation-evidence-spec
     "FR-3403"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier validate-type-advanced %type-advanced-validate-encodings)
     '(advanced-contracts-enforce-typescript-encodings-effects-and-equality
      advanced-feature-implementation-evidence-covers-all-fr-ids))
    (%type-advanced-implementation-evidence-spec
     "FR-3404"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier validate-type-advanced %type-advanced-validate-extensible-effects)
     '(advanced-contracts-enforce-typescript-encodings-effects-and-equality
      advanced-feature-implementation-evidence-covers-all-fr-ids))
    (%type-advanced-implementation-evidence-spec
     "FR-3405"
     '("packages/type/src/types-extended-nodes.lisp")
     '(parse-type-specifier validate-type-advanced %type-advanced-validate-type-theory-equality)
     '(advanced-contracts-enforce-typescript-encodings-effects-and-equality
      advanced-feature-implementation-evidence-covers-all-fr-ids))
    (%type-advanced-implementation-evidence-spec
     "FR-3406"
     '("packages/type/src/types-extended-nodes.lisp")
     '(make-universe-sort universe<= make-cic-proposition make-cic-proof cic-proof-valid-p proof-erasable-p)
     '(cic-scaffolding-validates-universes-and-proof-erasure
      advanced-feature-implementation-evidence-covers-all-fr-ids))))
  "Concrete helper-API and regression-test evidence for every tracked advanced FR id.")
