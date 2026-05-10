;;;; types-extended-advanced-data.lisp — Semantic contract specs for every tracked advanced FR id
;;;; Evidence specs live in types-extended-advanced-evidence-data.lisp.
(in-package :cl-cc/type)

(defparameter +type-advanced-contract-specs+
  (append
   (%type-advanced-contract-specs '("FR-1501" "FR-1502" "FR-1504" "FR-1505")
                                  :safety
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-1503" :safety
                                       :min-args 1
                                       :custom-validator '%type-advanced-validate-information-flow))
   (%type-advanced-contract-specs '("FR-1601" "FR-1602" "FR-1603" "FR-1604" "FR-1605")
                                  :inference
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-1606" :inference
                                       :min-args 1
                                       :required-properties '(:dependency-graph :cache)
                                       :property-predicates '((:dependency-graph . %type-advanced-symbolic-designator-p)
                                                              (:cache . %type-advanced-symbolic-designator-p)
                                                              (:lsp . %type-advanced-boolean-value-p))
                                       :custom-validator '%type-advanced-validate-incremental-checking))
   (%type-advanced-contract-specs '("FR-1701" "FR-1702" "FR-1704" "FR-1705")
                                  :type-level-programming
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-1703" :type-level-programming
                                       :exact-args 1
                                       :required-properties '(:stage :transition)
                                       :property-predicates '((:stage . %type-advanced-stage-designator-p)
                                                              (:transition . %type-advanced-stage-transition-p))
                                       :custom-validator '%type-advanced-validate-staging))
   (%type-advanced-contract-specs '("FR-1802" "FR-1803" "FR-1804" "FR-1805" "FR-1806")
                                  :functional-abstractions
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-1801" :functional-abstractions
                                       :exact-args 1
                                       :required-properties '(:lawful)
                                       :property-predicates '((:lawful . %type-advanced-boolean-value-p))
                                       :custom-validator '%type-advanced-validate-optics))
   (%type-advanced-contract-specs '("FR-1901" "FR-1902" "FR-1903" "FR-1904" "FR-1905" "FR-1906")
                                  :totality
                                  :min-args 1
                                  :requires-evidence-p t
                                  :custom-validator '%type-advanced-validate-proof-like)
   (%type-advanced-contract-specs '("FR-2001" "FR-2002" "FR-2003" "FR-2004" "FR-2005")
                                  :proofs
                                  :min-args 1
                                  :requires-evidence-p t
                                  :custom-validator '%type-advanced-validate-proof-like)
   (%type-advanced-contract-specs '("FR-2102" "FR-2104" "FR-2105" "FR-2106" "FR-2107")
                                  :type-directed-runtime
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-2101" :type-directed-runtime
                                       :min-args 1
                                       :required-properties '(:generator :coverage-target)
                                       :property-predicates '((:generator . %type-advanced-generator-form-p)
                                                              (:coverage-target . %type-advanced-positive-integer-p)
                                                              (:samples . %type-advanced-positive-integer-p))
                                       :custom-validator '%type-advanced-validate-test-generation)
         (%type-advanced-contract-spec "FR-2103" :type-directed-runtime
                                       :min-args 1
                                       :property-predicates '((:abi . %type-advanced-symbolic-designator-p))
                                       :custom-validator '%type-advanced-validate-type-safe-ffi))
   (%type-advanced-contract-specs '("FR-2201" "FR-2202" "FR-2203" "FR-2204" "FR-2205" "FR-2206")
                                  :concurrency
                                  :min-args 1)
   (%type-advanced-contract-specs '("FR-2301" "FR-2303" "FR-2304" "FR-2305")
                                  :numeric
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-2302" :numeric
                                       :exact-args 1
                                       :required-properties '(:unit)
                                       :property-predicates '((:unit . unit-designator-p))))
   (%type-advanced-contract-specs '("FR-2401" "FR-2402" "FR-2403" "FR-2404")
                                  :constraints
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-2405" :constraints
                                       :min-args 1
                                       :required-properties '(:exports :fingerprint)
                                        :property-predicates '((:exports . %type-advanced-interface-export-list-p)
                                                              (:fingerprint . %type-advanced-fingerprint-p))
                                       :custom-validator '%type-advanced-validate-interface-files)
         (%type-advanced-contract-spec "FR-2406" :constraints
                                       :min-args 1
                                       :required-properties '(:solver :theory)
                                       :property-predicates '((:solver . %type-advanced-smt-solver-p)
                                                              (:theory . %type-advanced-smt-theory-p))
                                       :custom-validator '%type-advanced-validate-smt-integration))
   (%type-advanced-contract-specs '("FR-2503" "FR-2504" "FR-2505")
                                  :gradual-runtime
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-2501" :gradual-runtime :exact-args 1)
         (%type-advanced-contract-spec "FR-2502" :gradual-runtime :exact-args 1))
   (%type-advanced-contract-specs '("FR-2601" "FR-2602" "FR-2604" "FR-2605" "FR-2606")
                                  :typeclasses
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-2603" :typeclasses :exact-args 1))
   (%type-advanced-contract-specs '("FR-2701" "FR-2702" "FR-2703" "FR-2704" "FR-2705" "FR-2706")
                                  :domain-specific
                                  :min-args 1)
   (%type-advanced-contract-specs '("FR-2801" "FR-2802" "FR-2803" "FR-2805")
                                  :semantics
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-2804" :semantics
                                       :exact-args 1
                                       :required-properties '(:domain :widening)
                                       :property-predicates '((:domain . %type-advanced-symbolic-designator-p)
                                                              (:widening . %type-advanced-symbolic-designator-p)
                                                              (:narrowing . %type-advanced-symbolic-designator-p))
                                       :custom-validator '%type-advanced-validate-abstract-interpretation))
   (%type-advanced-contract-specs '("FR-2901" "FR-2903" "FR-2904" "FR-2905")
                                  :analysis
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-2902" :analysis
                                       :exact-args 2
                                       :required-properties '(:disjoint :alias-class)
                                       :property-predicates '((:disjoint . %type-advanced-boolean-value-p)
                                                              (:alias-class . %type-advanced-symbolic-designator-p))
                                       :custom-validator '%type-advanced-validate-alias-analysis))
   (%type-advanced-contract-specs '("FR-3001" "FR-3004" "FR-3005")
                                  :tooling
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-3002" :tooling
                                       :min-args 1
                                       :required-properties '(:hook :phase)
                                       :property-predicates '((:hook . %type-advanced-symbolic-designator-p)
                                                              (:phase . %type-advanced-plugin-phase-p))
                                       :custom-validator '%type-advanced-validate-plugins)
         (%type-advanced-contract-spec "FR-3003" :tooling
                                       :min-args 1
                                       :required-properties '(:search :fuel)
                                       :property-predicates '((:search . %type-advanced-synthesis-strategy-p)
                                                              (:fuel . %type-advanced-positive-integer-p))
                                       :custom-validator '%type-advanced-validate-synthesis))
   (%type-advanced-contract-specs '("FR-3102" "FR-3103" "FR-3104")
                                  :linear-logic
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-3101" :linear-logic :min-args 3))
   (%type-advanced-contract-specs '("FR-3201" "FR-3202" "FR-3203" "FR-3204")
                                  :subtyping
                                  :min-args 1)
   (list (%type-advanced-contract-spec "FR-3205" :subtyping
                                       :exact-args 2
                                       :custom-validator '%type-advanced-validate-brand))
   (list (%type-advanced-contract-spec "FR-3301" :structural-programming
                                       :exact-args 1
                                       :required-properties '(:transform)
                                       :property-predicates '((:transform . %type-advanced-mapped-transform-p))
                                       :custom-validator '%type-advanced-validate-mapped-types)
         (%type-advanced-contract-spec "FR-3302" :structural-programming
                                       :exact-args 1
                                       :required-properties '(:extends :then :else)
                                       :property-predicates '((:infer . %type-advanced-symbolic-designator-p))
                                       :custom-validator '%type-advanced-validate-conditional-types)
         (%type-advanced-contract-spec "FR-3303" :structural-programming :exact-args 1)
         (%type-advanced-contract-spec "FR-3304" :structural-programming :exact-args 1)
         (%type-advanced-contract-spec "FR-3305" :structural-programming
                                       :min-args 1
                                       :custom-validator '%type-advanced-validate-route))
   (list (%type-advanced-contract-spec "FR-3401" :dependent-foundations
                                       :exact-args 2
                                       :custom-validator '%type-advanced-validate-qtt)
         (%type-advanced-contract-spec "FR-3402" :dependent-foundations
                                       :exact-args 2
                                       :custom-validator '%type-advanced-validate-graded)
         (%type-advanced-contract-spec "FR-3403" :dependent-foundations
                                       :exact-args 1
                                       :required-properties '(:encoding)
                                       :property-predicates '((:encoding . %type-advanced-encoding-kind-p))
                                       :custom-validator '%type-advanced-validate-encodings)
         (%type-advanced-contract-spec "FR-3404" :dependent-foundations
                                       :exact-args 2
                                       :custom-validator '%type-advanced-validate-extensible-effects)
         (%type-advanced-contract-spec "FR-3405" :dependent-foundations
                                       :exact-args 2
                                       :required-properties '(:mode)
                                       :property-predicates '((:mode . %type-advanced-equality-mode-p))
                                       :custom-validator '%type-advanced-validate-type-theory-equality)
          (%type-advanced-contract-spec "FR-3406" :dependent-foundations
                                        :min-args 1
                                        :requires-evidence-p t
                                        :custom-validator '%type-advanced-validate-proof-like)))
  "Explicit per-FR semantic contracts for every tracked advanced feature id.")
