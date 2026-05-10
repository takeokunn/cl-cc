;;;; tests/type-2026-advanced-semantic-tests.lisp - 2026 Concrete Advanced Type Semantic Tests
;;;;
;;;; Covers: concurrency traits, security labels, generics, channels, actors, STM, coroutines,
;;;; SIMD, routing, utility types, regions, capabilities, units, FFI, QTT, CIC, termination,
;;;; validators, and advanced contract enforcement tests.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── Concrete advanced semantic API regression tests (merged for Nix flake source) ─

(deftest concurrency-send-sync-registry-is-concrete
  "Send/Sync registries reject explicitly non-sendable values and accept registered safe ones."
  (cl-cc/type:register-concurrency-traits 'mutex-guard :send nil :sync nil :note :host-only)
  (cl-cc/type:register-concurrency-traits 'immutable-box :send t :sync t :note :value-object)
  (assert-false (cl-cc/type:sendable-type-p 'mutex-guard))
  (assert-false (cl-cc/type:shareable-type-p 'mutex-guard))
  (assert-true (cl-cc/type:validate-spawn-argument 'immutable-box))
  (assert-true (cl-cc/type:validate-shared-reference 'immutable-box))
  (assert-true (cl-cc/type:sendable-type-p 'integer)))

(deftest security-label-lattice-and-declassification-are-enforced
  "Security labels form a lattice and declassification leaves audit evidence."
  (assert-true (cl-cc/type:security-label<= :public :secret))
  (assert-false (cl-cc/type:security-label<= :secret :public))
  (assert-eq :secret (cl-cc/type:join-security-labels :trusted :secret))
  (assert-eq :public (cl-cc/type:meet-security-labels :public :trusted))
  (let* ((secret (cl-cc/type:make-labeled-value "token" :secret :tainted-p t))
         (sanitized (cl-cc/type:sanitize-labeled-value secret #'identity :audit-entry '(:sanitize sql))))
    (assert-false (cl-cc/type:labeled-value-flow-allowed-p secret :public))
    (assert-false (cl-cc/type:labeled-value-tainted-p sanitized))
  (let ((public (cl-cc/type:declassify-labeled-value secret :public 'audit-log)))
      (assert-true (cl-cc/type:labeled-value-flow-allowed-p public :public))
      (assert-= 1 (length (cl-cc/type:labeled-value-audit-trail public))))))

(deftest concrete-generics-registry-and-structural-traversal-work
  "Datatype-generic registrations produce custom reps while structural traversal still works on lists."
  (let* ((table cl-cc/type:*generic-instance-registry*)
         (saved (cl-cc/type:lookup-generic-instance 'keyword)))
    (unwind-protect
        (progn
          (cl-cc/type:register-generic-instance
           'keyword
           (lambda (value)
             (cl-cc/type:make-generic-sum
              :tag :keyword
              :value (cl-cc/type:make-generic-k1 :value value :type 'keyword)))
           :show (lambda (value) (string-downcase (symbol-name value)))
           :traverse (lambda (fn value) (funcall fn value)))
          (let ((representation (cl-cc/type:generic-representation-of :TOKEN)))
            (assert-true (cl-cc/type:generic-sum-p representation))
            (assert-true (cl-cc/type:generic-representation-valid-p representation))
            (assert-string= "token" (cl-cc/type:generic-show :TOKEN)))
          (assert-equal '(2 3 4) (cl-cc/type:generic-transform #'1+ '(1 2 3)))
          (assert-equal '(2 4) (cl-cc/type:generic-query #'evenp '(1 2 3 4))))
      (if saved
          (setf (gethash 'keyword table) saved)
          (remhash 'keyword table)))))

(deftest concrete-channels-enforce-capacity-type-and-close-semantics
  "Typed channels enforce payload type, bounded capacity, and closed-channel rejection."
  (multiple-value-bind (sender receiver) (cl-cc/type:make-buffered-channel 'integer 1)
    (assert-eq 'integer (cl-cc/type:channel-payload-type sender))
    (assert-true (cl-cc/type:channel-send sender 7))
    (assert-signals error
        (cl-cc/type:channel-send sender 8))
    (assert-= 7 (cl-cc/type:channel-recv receiver))
    (assert-null (cl-cc/type:channel-recv receiver))
    (assert-signals error
        (cl-cc/type:channel-send sender "wrong"))
    (cl-cc/type:close-typed-channel sender)
    (assert-signals error
        (cl-cc/type:channel-send sender 9))))

(deftest concrete-actors-accept-typed-messages-and-stop-cleanly
  "Typed actors accept matching protocol messages, invoke handlers, and reject sends after stop."
  (let* ((seen nil)
         (actor (cl-cc/type:make-actor-ref '(:ping integer)
                                           :handler (lambda (message) (setf seen message)))))
    (assert-true (cl-cc/type:actor-message-accepted-p actor '(:ping 5)))
    (assert-false (cl-cc/type:actor-message-accepted-p actor '(:pong 5)))
    (assert-true (cl-cc/type:actor-send actor '(:ping 5)))
    (assert-equal '(:ping 5) seen)
    (cl-cc/type:actor-stop actor)
    (assert-false (cl-cc/type:actor-message-accepted-p actor '(:ping 6)))
    (assert-signals error
        (cl-cc/type:actor-send actor '(:ping 6)))))

(deftest concrete-stm-actions-sequence-and-reject-io-effects
  "STM actions compose through bind, mutate TVars atomically, and reject IO effects."
  (let* ((cell (cl-cc/type:make-tvar 'integer 1))
         (action (cl-cc/type:stm-bind
                  (cl-cc/type:stm-read cell)
                  (lambda (current)
                    (cl-cc/type:stm-bind
                     (cl-cc/type:stm-write cell (+ current 1))
                     (lambda (_)
                       (declare (ignore _))
                       (cl-cc/type:stm-read cell)))))))
    (assert-= 2 (cl-cc/type:atomically action))
    (assert-= 2 (cl-cc/type:atomically (cl-cc/type:stm-read cell)))
    (assert-signals error
        (cl-cc/type:atomically
         (cl-cc/type::%make-stm-action :result-type cl-cc/type:type-int
                                       :thunk (lambda () 0)
                                       :effects '(:io))))))

(deftest concrete-coroutines-generators-and-coroutines-enforce-runtime-types
  "Generators yield finite values and coroutines validate both send and receive sides."
  (let ((generator (cl-cc/type:make-generator 'integer '(1 2)
                                              :return-type 'string
                                              :final-value "done")))
    (multiple-value-bind (value done-p) (cl-cc/type:generator-next generator)
      (assert-= 1 value)
      (assert-false done-p))
    (multiple-value-bind (value done-p) (cl-cc/type:generator-next generator)
      (assert-= 2 value)
      (assert-false done-p))
    (multiple-value-bind (value done-p) (cl-cc/type:generator-next generator)
      (assert-string= "done" value)
      (assert-true done-p)))
  (let ((coroutine (cl-cc/type:make-coroutine
                    'integer 'integer 'string
                    (lambda (value)
                      (if (plusp value)
                          (values (+ value 1) nil)
                          (values "done" t))))))
    (multiple-value-bind (value done-p) (cl-cc/type:coroutine-resume coroutine 3)
      (assert-= 4 value)
      (assert-false done-p))
    (multiple-value-bind (value done-p) (cl-cc/type:coroutine-resume coroutine 0)
      (assert-string= "done" value)
      (assert-true done-p)))
  (assert-signals error
      (let ((coroutine (cl-cc/type:make-coroutine
                        'integer 'integer 'string
                        (lambda (_value)
                          (declare (ignore _value))
                          (values :wrong nil)))))
        (cl-cc/type:coroutine-resume coroutine 1))))

(deftest concrete-simd-vectors-preserve-lanes-and-element-types
  "SIMD helpers preserve lane counts on map and reject incompatible additions."
  (let* ((left (cl-cc/type:make-simd-vector 'integer '(1 2 3)))
         (right (cl-cc/type:make-simd-vector 'integer '(4 5 6)))
         (sum (cl-cc/type:simd-add left right))
         (mapped (cl-cc/type:simd-map (lambda (value) (* value 2)) left)))
    (assert-= 3 (cl-cc/type:simd-vector-lanes sum))
    (assert-equal '(5 7 9) (cl-cc/type:simd-vector-values sum))
    (assert-equal '(2 4 6) (cl-cc/type:simd-vector-values mapped))
    (assert-signals error
        (cl-cc/type:simd-add left (cl-cc/type:make-simd-vector 'integer '(1 2))))))

(deftest concrete-routing-api-lookup-and-response-type-work
  "Routing helpers build FR-3305 API types and resolve typed routes from an api-spec."
  (let* ((users (cl-cc/type:make-route :get "/users/{id}"
                                       :parameters '((id integer))
                                       :response-type 'user))
         (health (cl-cc/type:make-route :get "/health"
                                        :parameters nil
                                        :response-type 'status))
         (api-spec (cl-cc/type::make-api-spec :routes (list users health)))
         (api-type (cl-cc/type:make-api-type :get "/users/{id}" '((id integer)) 'user)))
    (assert-true (cl-cc/type:type-advanced-p api-type))
    (assert-true (cl-cc/type:api-spec-valid-p api-spec))
    (multiple-value-bind (route params) (cl-cc/type:api-route-lookup api-spec :get "/users/42")
      (assert-true route)
      (assert-equal '((:ID . 42)) params))
    (assert-eq 'user (cl-cc/type:route-response-type-for api-spec :get "/users/42"))
    (assert-null (cl-cc/type:route-response-type-for api-spec :get "/missing"))))

(deftest concrete-utility-type-helpers-operate-on-nats-strings-and-record-transforms
  "Utility helpers compute type-level naturals/strings and structural readonly/partial transforms."
  (let* ((nat-two (cl-cc/type:make-type-level-natural 2))
         (nat-five (cl-cc/type:type-plus nat-two 3))
         (template (cl-cc/type:template-literal-type "user-" (cl-cc/type:make-type-level-string "id")))
         (record (cl-cc/type:make-type-record :fields (list (cons 'name cl-cc/type:type-string)
                                                            (cons 'age cl-cc/type:type-int))
                                              :row-var nil))
         (partial (cl-cc/type:partial-type record))
         (required (cl-cc/type:required-type partial))
         (frozen (cl-cc/type:freeze "value" cl-cc/type:type-string))
         (matrix-product (cl-cc/type:matrix-mul-type
                          (cl-cc/type:make-matrix-type 2 3 cl-cc/type:type-int)
                          (cl-cc/type:make-matrix-type 3 4 cl-cc/type:type-int)))
         (format-fn (cl-cc/type:format-type "~A => ~D")))
    (assert-= 5 (cl-cc/type:type-level-natural-value nat-five))
    (assert-string= "user-id" (cl-cc/type:type-level-string-value template))
    (assert-true (cl-cc/type:type-union-p (cl-cc/type:get-field-type 'name partial)))
    (assert-true (cl-cc/type:type-equal-p cl-cc/type:type-string
                                          (cl-cc/type:get-field-type 'name required)))
    (assert-true (cl-cc/type:frozen-value-p frozen))
    (let ((args (cl-cc/type:type-constructor-args matrix-product)))
      (assert-= 2 (cl-cc/type:type-level-natural-value (first args)))
      (assert-= 4 (cl-cc/type:type-level-natural-value (second args)))
      (assert-true (cl-cc/type:type-equal-p cl-cc/type:type-int (third args))))
    (assert-true (cl-cc/type:type-arrow-p format-fn))
    (assert-= 2 (length (cl-cc/type:type-arrow-params format-fn)))))

(deftest region-tokens-enforce-lifetimes
  "Region references become invalid as soon as their owning region closes."
  (let (dangling)
    (cl-cc/type:with-region (region)
      (setf dangling (cl-cc/type:region-alloc region 42))
      (assert-true (cl-cc/type:region-ref-valid-p dangling))
      (assert-= 42 (cl-cc/type:region-deref dangling)))
    (assert-false (cl-cc/type:region-ref-valid-p dangling))
    (assert-signals cl-cc/type:region-lifetime-error
        (cl-cc/type:region-deref dangling))))

(deftest capabilities-support-implication-restriction-and-effects
  "Capabilities encode permission implication, restriction, and effect extraction."
  (let* ((writer (cl-cc/type:make-capability '(:file-write :network-read)))
         (read-only (cl-cc/type:restrict-capability writer :read-only)))
    (assert-true (cl-cc/type:capability-allows-p writer :file-read))
    (assert-true (cl-cc/type:capability-implies-p writer :file-read))
    (assert-false (cl-cc/type:capability-allows-p read-only :file-write))
    (assert-true (cl-cc/type:capability-allows-p read-only :file-read))
    (assert-true (member :READ-FILE (cl-cc/type:capability-effects read-only) :test #'eq))))

(deftest units-of-measure-perform-dimension-checking
  "Units permit compatible arithmetic and reject incompatible dimensions."
  (let* ((one-meter (cl-cc/type:make-measure 1 'meter))
         (hundred-centimeters (cl-cc/type:make-measure 100 'centimeter))
         (two-seconds (cl-cc/type:make-measure 2 'second))
         (sum (cl-cc/type:measure+ one-meter hundred-centimeters))
         (velocity (cl-cc/type:measure/ (cl-cc/type:make-measure 10 'meter) two-seconds)))
    (assert-= 2 (cl-cc/type:measure-value sum))
    (assert-true (cl-cc/type:unit-compatible-p (cl-cc/type:measure-unit sum) 'meter))
    (assert-= 1 (cl-cc/type:convert-unit 100 'centimeter 'meter))
    (assert-equal '((:LENGTH . 1) (:TIME . -1))
                  (cl-cc/type:unit-definition-dimension (cl-cc/type:measure-unit velocity)))
    (assert-signals cl-cc/type:unit-mismatch-error
        (cl-cc/type:measure+ one-meter two-seconds))))

(deftest routing-validates-path-parameters-and-roundtrips
  "Typed routes validate placeholders, render paths, and parse them back."
  (let ((route (cl-cc/type:make-route :get "/users/{id}"
                                      :parameters '((id integer))
                                      :response-type 'user)))
    (assert-true (cl-cc/type:route-valid-p route))
    (assert-string= "/users/42" (cl-cc/type:build-route-path route '((:id . 42))))
    (multiple-value-bind (matched params)
        (cl-cc/type:match-route-path route "/users/42")
      (assert-true matched)
      (assert-equal '((:ID . 42)) params))
    (assert-signals cl-cc/type:route-validation-error
        (cl-cc/type:build-route-path route '((:id . "forty-two"))))))

(deftest ffi-descriptors-validate-recursively
  "FFI descriptors validate nested pointers and callbacks instead of accepting arbitrary forms."
  (let* ((int (cl-cc/type:make-ffi-scalar-type 'int))
         (ptr (cl-cc/type:make-ffi-pointer-type int :borrowed-p t))
         (callback (cl-cc/type:make-ffi-callback-type (list int) int))
         (descriptor (cl-cc/type:make-ffi-function-descriptor 'strlen (list ptr callback) int)))
    (assert-true (cl-cc/type:ffi-type-valid-p int))
    (assert-true (cl-cc/type:ffi-type-valid-p ptr))
    (assert-true (cl-cc/type:ffi-type-valid-p callback))
    (assert-true (cl-cc/type:ffi-type-valid-p descriptor))
    (assert-true (cl-cc/type:ffi-lisp-type-compatible-p 'integer int))
    (assert-false (cl-cc/type:ffi-descriptor-form-valid-p '(c-ptr)))))

(deftest qtt-and-graded-semantics-check-semiring-behavior
  "QTT multiplicities and graded semiring composition reject invalid usage."
  (let* ((semiring (cl-cc/type:make-qtt-semiring))
         (left (cl-cc/type:make-graded-value :one 'x semiring))
         (right (cl-cc/type:make-graded-value :omega 'y semiring))
         (binding (cl-cc/type:make-qtt-binding 'n 'nat 0)))
    (assert-true (cl-cc/type:valid-multiplicity-p 1))
    (assert-false (cl-cc/type:valid-multiplicity-p 2))
    (assert-true (cl-cc/type:multiplicity<= 0 1))
    (assert-eq :omega (cl-cc/type:multiplicity+ 1 :omega))
    (assert-eq :omega (cl-cc/type:multiplicity* :omega 1))
    (assert-true (cl-cc/type:usage-satisfies-multiplicity-p 1 1))
    (assert-false (cl-cc/type:usage-satisfies-multiplicity-p 1 2))
    (assert-true (cl-cc/type:qtt-erased-p binding))
    (assert-true (cl-cc/type:finite-semiring-valid-p semiring))
    (assert-eq :omega (cl-cc/type:graded-value-grade (cl-cc/type:graded-add left right)))
    (assert-eq :omega (cl-cc/type:graded-value-grade (cl-cc/type:graded-compose left right)))))

(deftest cic-scaffolding-validates-universes-and-proof-erasure
  "CIC scaffolding distinguishes Prop from Type and forbids large elimination from Prop."
  (let* ((prop (cl-cc/type:make-universe-sort :prop))
         (type0 (cl-cc/type:make-universe-sort :type 0))
         (proposition (cl-cc/type:make-cic-proposition 'non-zero prop '(d)))
         (proof (cl-cc/type:make-cic-proof proposition 'witness)))
    (assert-true (cl-cc/type:valid-universe-sort-p prop))
    (assert-true (cl-cc/type:universe<= prop type0))
    (assert-false (cl-cc/type:cic-large-elimination-allowed-p prop type0))
    (assert-true (cl-cc/type:cic-proof-valid-p proof))
    (assert-true (cl-cc/type:proof-erasable-p proof))))

(deftest termination-and-pcc-evidence-check-real-obligations
  "Termination evidence and proof-carrying code check concrete decreasing traces and witnesses."
  (let* ((termination (cl-cc/type:make-termination-evidence :structural '(5 4 3 2 1)))
         (obligation (cl-cc/type:make-nonzero-obligation 'non-zero-denominator))
         (evidence (cl-cc/type:make-proof-evidence 'non-zero-denominator 2))
         (bundle (cl-cc/type:make-proof-carrying-code 'safe-div (list obligation) (list evidence))))
    (assert-true (cl-cc/type:termination-evidence-valid-p termination))
    (assert-true (cl-cc/type:verify-proof-obligation obligation 2))
    (assert-true (cl-cc/type:verify-proof-evidence obligation evidence))
    (assert-true (cl-cc/type:verify-proof-carrying-code bundle))
    (assert-false
     (cl-cc/type:verify-proof-carrying-code
      (cl-cc/type:make-proof-carrying-code
       'unsafe-div
       (list obligation)
       (list (cl-cc/type:make-proof-evidence 'non-zero-denominator 0)))))))

(deftest advanced-validators-now-use-concrete-semantic-modules
  "Advanced validators consult concrete route/unit/multiplicity/termination semantics."
  (assert-signals error
      (cl-cc/type:parse-type-specifier '(units-of-measure float :unit furlong)))
  (assert-signals error
      (cl-cc/type:parse-type-specifier '(advanced fr-3401 2 fixnum :evidence (proof impossible))))
  (assert-signals error
      (cl-cc/type:parse-type-specifier '(advanced fr-1901 recursive-length :evidence (mystery proof))))
  (let ((route (cl-cc/type:parse-type-specifier '(api-type (get "/users/{id}" integer user)))))
    (assert-true (cl-cc/type:type-advanced-valid-p route))))

(defun %expect-valid (form expected-id)
  "Parse FORM as a type specifier, assert it is valid and has EXPECTED-ID as its feature id.
Returns the parsed node."
  (let ((node (cl-cc/type:parse-type-specifier form)))
    (assert-true (cl-cc/type:type-advanced-valid-p node))
    (assert-string= expected-id (cl-cc/type:type-advanced-feature-id node))
    node))

(deftest advanced-contracts-enforce-incremental-staging-optics-and-test-generation
  "Representative previously shallow FRs now require explicit semantic properties or evidence."
  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(advanced fr-1606 cache-entry :dependency-graph call-graph)))
  (%expect-valid '(advanced fr-1606 cache-entry :dependency-graph call-graph :cache module-cache :lsp t)
                 "FR-1606")

  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(advanced fr-1703 (code integer) :stage 0 :transition :run)))
  (%expect-valid '(advanced fr-1703 (code integer) :stage 1 :transition :run :evidence (proof staged-eval))
                 "FR-1703")

  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(advanced fr-1801 (zoom a b) :lawful t)))
  (%expect-valid '(advanced fr-1801 (lens a b s t) :lawful t)
                 "FR-1801")

  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(advanced fr-2101 (list integer) :generator fuzz :coverage-target 0)))
  (%expect-valid '(advanced fr-2101 (list integer) :generator (arbitrary integer) :coverage-target 100 :samples 20)
                 "FR-2101"))

(deftest advanced-contracts-enforce-constraint-analysis-and-tooling-families
  "Interface files, SMT, abstract interpretation, alias analysis, plugins, and synthesis require structured metadata."
  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(advanced fr-2405 user-module :exports (lookup lookup) :fingerprint "")))
  (%expect-valid '(advanced fr-2405 user-module :exports (lookup save) :fingerprint "sha256:abc")
                 "FR-2405")

  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(advanced fr-2406 (< v n) :solver :unknown :theory :lia)))
  (%expect-valid '(advanced fr-2406 (< v n) :solver :z3 :theory :lia :evidence (proof smt-discharge))
                 "FR-2406")

  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(advanced fr-2804 integer :domain interval-lattice :widening widen :narrowing widen)))
  (%expect-valid '(advanced fr-2804 integer :domain interval-lattice :widening widen :narrowing narrow)
                 "FR-2804")

  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(advanced fr-2902 (pointer integer) (pointer integer) :disjoint t :alias-class heap)))
  (%expect-valid '(advanced fr-2902 (pointer integer) (pointer float) :disjoint t :alias-class heap)
                 "FR-2902")

  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(advanced fr-3002 nat-normalise :hook solver :phase :emit)))
  (%expect-valid '(advanced fr-3002 nat-normalise :hook solver :phase :solve)
                 "FR-3002")

  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(advanced fr-3003 (-> integer integer) :search :enumerative :fuel 0)))
  (%expect-valid '(advanced fr-3003 (-> integer integer) :search :enumerative :fuel 8)
                 "FR-3003"))

(deftest advanced-contracts-enforce-typescript-encodings-effects-and-equality
  "Mapped/conditional types and dependent-foundation representatives require meaningful properties or evidence."
  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(mapped-type (list fixnum) :transform mysterious)))
  (%expect-valid '(mapped-type (list fixnum) :transform optional)
                 "FR-3301")

  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(conditional-type (list fixnum) :extends list :then item :else item)))
  (%expect-valid '(conditional-type (list fixnum) :extends list :infer item :then item :else null)
                 "FR-3302")

  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(church-encoding integer :encoding :scott)))
  (%expect-valid '(church-encoding integer :encoding :church)
                 "FR-3403")

  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(open-union (io io) fixnum)))
  (%expect-valid '(open-union (io state) fixnum)
                 "FR-3404")

  (assert-signals error
      (cl-cc/type:parse-type-specifier
       '(type-theory-equality (-> integer integer) (-> integer integer) :mode :extensional)))
  (%expect-valid '(type-theory-equality (-> integer integer) (-> integer integer)
                                        :mode :extensional
                                        :evidence (proof functional-extensionality))
                 "FR-3405"))
