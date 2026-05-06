;;;; tests/type-2026-nodes-tests.lisp - 2026 Type System Node and Substitution API Tests
;;;;
;;;; Covers: 2026 type node extensions (rigid vars, product/variant, exists/mu, HKT app,
;;;; record, arrow-mult, linear), ANSI upgrade helpers, and hash-table substitution API.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: New Type Node Tests (direct new API)
;;; ─────────────────────────────────────────────────────────────────────────

(deftest type-rigid-var-identity-and-uniqueness
  "fresh-rigid-var: each call produces a distinct var; same var is equal to itself."
  (let ((r1 (fresh-rigid-var 'a))
        (r2 (fresh-rigid-var 'a)))
    (assert-true  (type-rigid-p r1))
    (assert-true  (type-rigid-p r2))
    (assert-false (type-rigid-equal-p r1 r2))
    (assert-true  (type-rigid-equal-p r1 r1))
    (assert-eq 'a (type-rigid-name r1))))

(deftest type-product-and-variant-creation
  "make-type-product stores ordered elem types; make-type-variant stores cases with nil row-var."
  (let ((pair (make-type-product :elems (list type-int type-string))))
    (assert-true (type-product-p pair))
    (assert-= 2 (length (type-product-elems pair)))
    (assert-true (type-equal-p type-int    (first  (type-product-elems pair))))
    (assert-true (type-equal-p type-string (second (type-product-elems pair)))))
  (let ((v (make-type-variant :cases (list (cons 'some type-int) (cons 'none type-null))
                              :row-var nil)))
    (assert-true (type-variant-p v))
    (assert-= 2 (length (type-variant-cases v)))
    (assert-null (type-variant-row-var v))))

(deftest type-forall-body-keyword-aliases-type
  "make-type-forall :body and :type slots are aliases — both return the same body."
  (let* ((a  (fresh-type-var 'a))
         (fn (make-type-arrow (list a) a))
         (fa (make-type-forall :var a :body fn)))
    (assert-true (type-forall-p fa))
    (assert-true (type-var-equal-p a (type-forall-var fa)))
    (assert-true (type-equal-p fn (type-forall-body fa)))))

(deftest type-exists-and-mu-creation
  "make-type-exists and make-type-mu store var and body correctly."
  (let* ((a    (fresh-type-var 'a))
         (pair (make-type-product :elems (list type-string a)))
         (ex   (make-type-exists :var a :knd nil :body pair)))
    (assert-true (type-exists-p ex))
    (assert-true (type-var-equal-p a (type-exists-var ex)))
    (assert-true (type-product-p (type-exists-body ex))))
  (let* ((a  (fresh-type-var 'a))
         (mu (make-type-mu :var a
                           :body (make-type-union
                                  (list type-null
                                        (make-type-product :elems (list type-int a)))))))
    (assert-true (type-mu-p mu))
    (assert-true (type-var-equal-p a (type-mu-var mu)))
    (assert-true (type-union-p (type-mu-body mu)))))

(deftest type-hkt-app-creation
  "make-type-app builds a higher-kinded application with fun and arg accessors."
  (let* ((list-con (make-type-primitive :name 'list))
         (list-int (make-type-app :fun list-con :arg type-int)))
    (assert-true (type-app-p list-int))
    (assert-true (type-primitive-p (type-app-fun list-int)))
    (assert-true (type-equal-p type-int (type-app-arg list-int)))))

(deftest-each type-record-open-closed
  "make-type-record: closed record has nil row-var; open record has a type-var row-var."
  :cases (("closed" nil 2)
          ("open"   t   1))
  (open-p expected-field-count)
  (let* ((rv  (when open-p (fresh-type-var 'rho)))
         (fields (if open-p
                     (list (cons 'name type-string))
                     (list (cons 'name type-string) (cons 'age type-int))))
         (rec (make-type-record :fields fields :row-var rv)))
    (assert-true (type-record-p rec))
    (assert-= expected-field-count (length (type-record-fields rec)))
    (if open-p
        (assert-true (type-var-p (type-record-row-var rec)))
        (assert-null (type-record-row-var rec)))))


(deftest-each type-arrow-mult-cases
  "make-type-arrow-raw supports :one (linear) and :zero (erased) multiplicities."
  :cases (("linear-one"  type-int  type-int  +pure-effect-row+ :one)
          ("erased-zero" type-bool type-null nil                :zero))
  (param-t ret-t effs mult)
  (let ((arr (make-type-arrow-raw :params (list param-t) :return ret-t :effects effs :mult mult)))
    (assert-true (type-arrow-p arr))
    (assert-eq mult (type-arrow-mult arr))))


(deftest-each type-linear-creation
  "make-type-linear creates graded modal types !_q T with the correct grade."
  :cases (("linear-one"   type-int    :one)
          ("erased-zero"  type-string :zero)
          ("unrestricted" type-bool   :omega))
  (base grade)
  (let ((lin (make-type-linear :base base :grade grade)))
    (assert-true (type-linear-p lin))
    (assert-eq grade (type-linear-grade lin))
    (assert-true (type-equal-p base (type-linear-base lin)))))

(deftest upgraded-array-and-complex-part-types
  "ANSI CL upgrade helpers return the expected core type nodes."
  (let ((bit-upgraded (upgraded-array-element-type 'bit))
        (char-upgraded (upgraded-array-element-type 'character))
        (fallback-upgraded (upgraded-array-element-type '(or fixnum string)))
        (complex-part (upgraded-complex-part-type 'complex)))
    (assert-true (type-equal-p (cl-cc/type:parse-type-specifier 'bit) bit-upgraded))
    (assert-true (type-equal-p (cl-cc/type:parse-type-specifier 'character) char-upgraded))
    (assert-true (type-equal-p type-any fallback-upgraded))
    (assert-true (type-equal-p (cl-cc/type:parse-type-specifier 'real) complex-part))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: Hash-Table Substitution API Tests
;;; ─────────────────────────────────────────────────────────────────────────

(deftest subst-empty-has-no-bindings
  "make-substitution: generation is 0 and fresh vars are not found."
  (let ((s (make-substitution)))
    (assert-true (substitution-p s))
    (assert-= 0 (substitution-generation s))
    (let ((v (fresh-type-var)))
      (multiple-value-bind (bound found) (subst-lookup v s)
        (declare (ignore bound))
        (assert-false found)))))

(deftest subst-extend-is-functional
  "subst-extend creates a new substitution; old substitution is unchanged."
  (let* ((v  (fresh-type-var))
         (s0 (make-substitution))
         (s1 (subst-extend v type-int s0)))
    (multiple-value-bind (b f) (subst-lookup v s0) (declare (ignore b)) (assert-false f))
    (multiple-value-bind (bound found) (subst-lookup v s1)
      (assert-true found)
      (assert-true (type-equal-p type-int bound)))
    (assert-true (> (substitution-generation s1) (substitution-generation s0)))))

(deftest subst-extend!-mutates-in-place
  "subst-extend! adds a binding to the existing substitution destructively."
  (let* ((v (fresh-type-var))
         (s (make-substitution)))
    (subst-extend! v type-string s)
    (multiple-value-bind (bound found) (subst-lookup v s)
      (assert-true found)
      (assert-true (type-equal-p type-string bound)))))

(deftest subst-advanced-operations
  "subst-compose chains via v2→v1→int; zonk applies through arrow; type-occurs-p detects circular refs."
  (let* ((v1  (fresh-type-var))
         (v2  (fresh-type-var))
         (s1  (subst-extend v1 type-int (make-substitution)))
         (s2  (subst-extend v2 v1 (make-substitution)))
         (s12 (subst-compose s1 s2)))
    (assert-true (type-equal-p type-int (zonk v2 s12))))
  (let* ((v  (fresh-type-var))
         (fn (make-type-arrow (list v) v))
         (s  (subst-extend v type-bool (make-substitution)))
         (r  (zonk fn s)))
    (assert-true (type-arrow-p r))
    (assert-true (type-equal-p type-bool (first (type-arrow-params r))))
    (assert-true (type-equal-p type-bool (type-arrow-return r))))
  (let* ((v  (fresh-type-var))
         (fn (make-type-arrow (list v) type-int))
         (s  (make-substitution))
         (w  (fresh-type-var)))
    (assert-true  (type-occurs-p v fn s))
    (assert-false (type-occurs-p v type-int s))
    (assert-false (type-occurs-p w fn s))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Advanced feature registry / representation tests
;;; ─────────────────────────────────────────────────────────────────────────

(deftest advanced-feature-registry-covers-doc-fr-list
  "The advanced feature registry covers every represented docs/type-advanced.md FR id."
  (let ((expected-ids (mapcar #'first cl-cc/type:+type-advanced-feature-specs+))
        (actual-ids (cl-cc/type:list-type-advanced-feature-ids))
        (contract-count (hash-table-count cl-cc/type::*type-advanced-contract-registry*)))
    (assert-= (length expected-ids) (length actual-ids))
    (assert-= (length expected-ids) contract-count)
    (assert-equal expected-ids actual-ids)
    (dolist (feature-id expected-ids)
      (assert-true (cl-cc/type:lookup-type-advanced-feature feature-id))
      (assert-true (cl-cc/type:type-advanced-semantics-implemented-p feature-id))
      (assert-true (cl-cc/type::lookup-type-advanced-contract feature-id)))))

(deftest advanced-feature-contract-registry-rejects-unknown-and-missing-contracts
  "Unknown FR ids and registry entries with no explicit contract are rejected at construction time."
  (assert-false (cl-cc/type:type-advanced-semantics-implemented-p "FR-9999"))
  (assert-signals error
      (cl-cc/type:parse-type-specifier '(advanced fr-9999 integer)))
  (let* ((feature-id "FR-1606")
         (table cl-cc/type::*type-advanced-contract-registry*)
         (saved (cl-cc/type::lookup-type-advanced-contract feature-id)))
    (assert-true saved)
    (unwind-protect
        (progn
          (remhash feature-id table)
          (assert-false (cl-cc/type:type-advanced-semantics-implemented-p feature-id))
          (assert-signals error
              (cl-cc/type:make-type-advanced
               :feature-id feature-id
               :args (list 'cache-entry)
               :properties (list (cons :dependency-graph 'call-graph)
                                 (cons :cache 'module-cache)))))
      (setf (gethash feature-id table) saved))
    (assert-true (cl-cc/type:type-advanced-semantics-implemented-p feature-id))))

(deftest-each advanced-feature-parser-roundtrips
  "Representative advanced feature forms parse, unparse, and print through type-advanced."
  :cases (("generic" '(advanced fr-1503 (secret string) :flow secret) "FR-1503" "ADVANCED")
          ("type-safe-ffi" '(type-safe-ffi (c-callback (-> fixnum fixnum)) :abi c) "FR-2103" "TYPE-SAFE-FFI")
          ("future" '(future fixnum :mode eager) "FR-2201" "FUTURE")
          ("units-of-measure" '(units-of-measure float :unit meter) "FR-2302" "UNITS-OF-MEASURE")
          ("dynamic" '(dynamic fixnum) "FR-2501" "DYNAMIC")
          ("typerep" '(typerep (list fixnum)) "FR-2502" "TYPEREP")
          ("dict" '(dict (eq fixnum)) "FR-2603" "DICT")
          ("mapped-type" '(mapped-type (list fixnum) :transform optional) "FR-3301" "MAPPED-TYPE")
          ("readonly" '(readonly (list fixnum)) "FR-3303" "READONLY")
          ("partial-type" '(partial-type (list fixnum)) "FR-3304" "PARTIAL-TYPE")
          ("api-type" '(api-type (get "/users" (list fixnum))) "FR-3305" "API-TYPE")
          ("linear-logic" '(linear-logic tensor fixnum string) "FR-3101" "LINEAR-LOGIC")
          ("algebraic-subtype" '(algebraic-subtype (-> fixnum string)) "FR-3201" "ALGEBRAIC-SUBTYPE")
          ("brand-type" '(brand-type user-id string) "FR-3205" "BRAND-TYPE")
          ("qtt" '(qtt 0 (vector fixnum)) "FR-3401" "QTT")
          ("graded" '(graded :omega (list fixnum)) "FR-3402" "GRADED")
          ("open-union" '(open-union (io state) fixnum) "FR-3404" "OPEN-UNION"))
  (form expected-id printed-head)
  (let ((ty (cl-cc/type:parse-type-specifier form)))
    (assert-true (cl-cc/type:looks-like-type-specifier-p form))
    (assert-true (cl-cc/type:type-advanced-p ty))
    (assert-string= expected-id (cl-cc/type:type-advanced-feature-id ty))
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (assert-true (cl-cc/type:looks-like-type-specifier-p roundtrip))
      (assert-true (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip))))
    (assert-true (search printed-head (cl-cc/type:type-to-string ty)))))

(deftest advanced-feature-head-registry-covers-representative-aliases
  "Representative surface heads resolve back to their registered FR ids."
  (assert-string= "FR-2501" (cl-cc/type:type-advanced-feature-id-for-head 'dynamic))
  (assert-string= "FR-2502" (cl-cc/type:type-advanced-feature-id-for-head 'typerep))
  (assert-string= "FR-3305" (cl-cc/type:type-advanced-feature-id-for-head 'api-type))
  (assert-true (cl-cc/type:type-advanced-head-p 'advanced))
  (assert-true (cl-cc/type:type-advanced-head-p 'mapped-type)))

(deftest advanced-null-safety-option-parses-nullable-union
  "FR-1501 null safety uses concrete union/null representation for option sugar."
  (let ((ty (cl-cc/type:parse-type-specifier '(option string))))
    (assert-true (cl-cc/type:type-union-p ty))
    (assert-true (some (lambda (member)
                         (cl-cc/type:type-equal-p member cl-cc/type:type-null))
                       (cl-cc/type:type-union-types ty)))
    (assert-true (some (lambda (member)
                         (cl-cc/type:type-equal-p member cl-cc/type:type-string))
                       (cl-cc/type:type-union-types ty)))))

(deftest advanced-information-flow-enforces-security-lattice
  "FR-1503 rejects secret-to-public flow unless explicit declassification evidence is present."
  (assert-true (cl-cc/type:type-advanced-security-label<= :public :secret))
  (assert-false (cl-cc/type:type-advanced-security-label<= :secret :public))
  (assert-signals error
      (cl-cc/type:parse-type-specifier '(advanced fr-1503 (secret string) :flow public)))
  (let ((ty (cl-cc/type:parse-type-specifier
             '(advanced fr-1503 (secret string) :flow public :evidence (declassify audit-log)))))
    (assert-true (cl-cc/type:type-advanced-valid-p ty))
    (assert-string= "FR-1503" (cl-cc/type:type-advanced-feature-id ty)))
  (let ((public-string (cl-cc/type:parse-type-specifier
                        '(advanced fr-1503 (public string) :flow public)))
        (secret-string (cl-cc/type:parse-type-specifier
                        '(advanced fr-1503 (secret string) :flow secret))))
    (assert-true (cl-cc/type:is-subtype-p public-string secret-string))
    (assert-false (cl-cc/type:is-subtype-p secret-string public-string))))

(deftest advanced-validation-rejects-malformed-units-routes-and-ffi
  "Advanced validators reject malformed FR-2302, FR-3305, and FR-2103 payloads."
  (assert-signals error
      (cl-cc/type:parse-type-specifier '(units-of-measure float)))
  (assert-signals error
      (cl-cc/type:parse-type-specifier '(api-type (fetch "/users" string))))
  (assert-signals error
      (cl-cc/type:parse-type-specifier '(type-safe-ffi (c-ptr))))
  (let ((route (cl-cc/type:parse-type-specifier '(api-type (get "/users/{id}" integer user)))))
    (assert-true (cl-cc/type:type-advanced-valid-p route))
    (assert-true (cl-cc/type:type-advanced-route-p (first (cl-cc/type:type-advanced-args route))))))

(deftest advanced-proof-like-features-require-evidence
  "Proof/totality families require explicit evidence instead of accepting bare metadata."
  (assert-signals error
      (cl-cc/type:parse-type-specifier '(advanced fr-2002 safe-div-proof)))
  (let ((pcc (cl-cc/type:parse-type-specifier
              '(advanced fr-2002 safe-div-proof :evidence (proof non-zero-denominator)))))
    (assert-true (cl-cc/type:type-advanced-valid-p pcc))
    (assert-string= "FR-2002" (cl-cc/type:type-advanced-feature-id pcc))))

(deftest advanced-graded-and-branded-types-have-semantic-shape
  "FR-3402 grades and FR-3205 brands are validated as semantic payload, not opaque syntax."
  (assert-signals error
      (cl-cc/type:parse-type-specifier '(graded :sometimes (list fixnum))))
  (let ((user-id (cl-cc/type:parse-type-specifier '(brand-type user-id string)))
        (post-id (cl-cc/type:parse-type-specifier '(brand-type post-id string))))
    (assert-true (cl-cc/type:type-advanced-valid-p user-id))
    (assert-false (cl-cc/type:type-equal-p user-id post-id))))

(deftest advanced-node-children-and-free-vars-follow-nested-type-payload
  "type-children and type-free-vars traverse nested advanced payload types."
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (b (cl-cc/type:fresh-type-var 'b))
         (node (cl-cc/type:make-type-advanced
                 :feature-id "FR-1601"
                 :name 'typed-hole
                 :args (list a (cl-cc/type:make-type-arrow (list b) cl-cc/type:type-int)))))
    (let ((children (cl-cc/type:type-children node))
          (free-vars (cl-cc/type:type-free-vars node)))
      (assert-= 2 (length children))
      (assert-true (cl-cc/type:type-var-p (first children)))
      (assert-true (cl-cc/type:type-arrow-p (second children)))
      (assert-= 2 (length free-vars))
      (assert-true (some (lambda (var) (cl-cc/type:type-var-equal-p var a)) free-vars))
      (assert-true (some (lambda (var) (cl-cc/type:type-var-equal-p var b)) free-vars)))))

(deftest advanced-node-zonk-updates-args-properties-and-evidence
  "zonk substitutes nested type payload across args, properties, and evidence."
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (b (cl-cc/type:fresh-type-var 'b))
         (subst (cl-cc/type:make-substitution))
         (node (cl-cc/type:make-type-advanced
                :feature-id "FR-2501"
                :name 'dynamic
                :args (list (cl-cc/type:make-type-arrow (list a) b))
                :properties (list (cons :guard a))
                :evidence (list 'proof b))))
    (cl-cc/type:subst-extend! a cl-cc/type:type-int subst)
    (cl-cc/type:subst-extend! b cl-cc/type:type-string subst)
    (assert-true
     (cl-cc/type:type-equal-p
      (cl-cc/type:make-type-advanced
       :feature-id "FR-2501"
       :name 'dynamic
       :args (list (cl-cc/type:make-type-arrow (list cl-cc/type:type-int) cl-cc/type:type-string))
       :properties (list (cons :guard cl-cc/type:type-int))
       :evidence (list 'proof cl-cc/type:type-string))
      (cl-cc/type:zonk node subst)))))

(deftest advanced-node-unification-is-structural-and-fr-scoped
  "Advanced feature nodes unify only inside the same FR family and shape."
  (let ((a (cl-cc/type:fresh-type-var 'a)))
    (multiple-value-bind (subst ok)
        (cl-cc/type:type-unify (cl-cc/type:make-type-dynamic a)
                               (cl-cc/type:parse-type-specifier '(advanced fr-2501 fixnum)))
      (assert-true ok)
      (assert-true (cl-cc/type:substitution-p subst))
      (assert-true (cl-cc/type:type-equal-p cl-cc/type:type-int (cl-cc/type:zonk a subst)))))
  (multiple-value-bind (_ ok)
      (cl-cc/type:type-unify (cl-cc/type:parse-type-specifier '(dynamic fixnum))
                             (cl-cc/type:parse-type-specifier '(typerep fixnum)))
    (declare (ignore _))
    (assert-false ok)))

(deftest advanced-node-properties-are-key-order-insensitive
  "Advanced node properties compare and unify by property key, not source order."
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (left (cl-cc/type:make-type-advanced
                :feature-id "FR-2501"
                :name 'dynamic
                :args (list a)
                :properties (list (cons :guard a)
                                  (cons :mode 'checked))))
         (right (cl-cc/type:make-type-advanced
                 :feature-id "FR-2501"
                 :name 'dynamic
                 :args (list cl-cc/type:type-int)
                 :properties (list (cons :mode 'checked)
                                   (cons :guard cl-cc/type:type-int)))))
    (multiple-value-bind (subst ok)
        (cl-cc/type:type-unify left right)
      (assert-true ok)
      (assert-true (cl-cc/type:type-equal-p cl-cc/type:type-int
                                            (cl-cc/type:zonk a subst)))
      (assert-true (cl-cc/type:type-equal-p right
                                            (cl-cc/type:zonk left subst))))))

(deftest advanced-node-subtyping-degrades-safely
  "Advanced nodes remain exact by default and do not turn Dynamic into an unsound top type."
  (let ((dynamic-fixnum (cl-cc/type:parse-type-specifier '(dynamic fixnum)))
        (dynamic-string (cl-cc/type:parse-type-specifier '(dynamic string)))
        (generic-fixnum (cl-cc/type:parse-type-specifier '(advanced fr-2501 fixnum))))
    (assert-true (cl-cc/type:is-subtype-p dynamic-fixnum generic-fixnum))
    (assert-false (cl-cc/type:is-subtype-p cl-cc/type:type-string dynamic-fixnum))
    (assert-false (cl-cc/type:is-subtype-p dynamic-fixnum dynamic-string))
    (assert-true (cl-cc/type:is-subtype-p dynamic-fixnum cl-cc/type:type-any))))


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

(deftest advanced-contracts-enforce-incremental-staging-optics-and-test-generation
  "Representative previously shallow FRs now require explicit semantic properties or evidence."
  (flet ((expect-valid (form expected-id)
           (let ((node (cl-cc/type:parse-type-specifier form)))
             (assert-true (cl-cc/type:type-advanced-valid-p node))
             (assert-string= expected-id (cl-cc/type:type-advanced-feature-id node))
             node)))
    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(advanced fr-1606 cache-entry :dependency-graph call-graph)))
    (expect-valid '(advanced fr-1606 cache-entry :dependency-graph call-graph :cache module-cache :lsp t)
                  "FR-1606")

    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(advanced fr-1703 (code integer) :stage 0 :transition :run)))
    (expect-valid '(advanced fr-1703 (code integer) :stage 1 :transition :run :evidence (proof staged-eval))
                  "FR-1703")

    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(advanced fr-1801 (zoom a b) :lawful t)))
    (expect-valid '(advanced fr-1801 (lens a b s t) :lawful t)
                  "FR-1801")

    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(advanced fr-2101 (list integer) :generator fuzz :coverage-target 0)))
    (expect-valid '(advanced fr-2101 (list integer) :generator (arbitrary integer) :coverage-target 100 :samples 20)
                  "FR-2101")))

(deftest advanced-contracts-enforce-constraint-analysis-and-tooling-families
  "Interface files, SMT, abstract interpretation, alias analysis, plugins, and synthesis require structured metadata."
  (flet ((expect-valid (form expected-id)
           (let ((node (cl-cc/type:parse-type-specifier form)))
             (assert-true (cl-cc/type:type-advanced-valid-p node))
             (assert-string= expected-id (cl-cc/type:type-advanced-feature-id node))
             node)))
    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(advanced fr-2405 user-module :exports (lookup lookup) :fingerprint "")))
    (expect-valid '(advanced fr-2405 user-module :exports (lookup save) :fingerprint "sha256:abc")
                  "FR-2405")

    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(advanced fr-2406 (< v n) :solver :unknown :theory :lia)))
    (expect-valid '(advanced fr-2406 (< v n) :solver :z3 :theory :lia :evidence (proof smt-discharge))
                  "FR-2406")

    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(advanced fr-2804 integer :domain interval-lattice :widening widen :narrowing widen)))
    (expect-valid '(advanced fr-2804 integer :domain interval-lattice :widening widen :narrowing narrow)
                  "FR-2804")

    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(advanced fr-2902 (pointer integer) (pointer integer) :disjoint t :alias-class heap)))
    (expect-valid '(advanced fr-2902 (pointer integer) (pointer float) :disjoint t :alias-class heap)
                  "FR-2902")

    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(advanced fr-3002 nat-normalise :hook solver :phase :emit)))
    (expect-valid '(advanced fr-3002 nat-normalise :hook solver :phase :solve)
                  "FR-3002")

    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(advanced fr-3003 (-> integer integer) :search :enumerative :fuel 0)))
    (expect-valid '(advanced fr-3003 (-> integer integer) :search :enumerative :fuel 8)
                  "FR-3003")))

(deftest advanced-contracts-enforce-typescript-encodings-effects-and-equality
  "Mapped/conditional types and dependent-foundation representatives require meaningful properties or evidence."
  (flet ((expect-valid (form expected-id)
           (let ((node (cl-cc/type:parse-type-specifier form)))
             (assert-true (cl-cc/type:type-advanced-valid-p node))
             (assert-string= expected-id (cl-cc/type:type-advanced-feature-id node))
             node)))
    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(mapped-type (list fixnum) :transform mysterious)))
    (expect-valid '(mapped-type (list fixnum) :transform optional)
                  "FR-3301")

    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(conditional-type (list fixnum) :extends list :then item :else item)))
    (expect-valid '(conditional-type (list fixnum) :extends list :infer item :then item :else null)
                  "FR-3302")

    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(church-encoding integer :encoding :scott)))
    (expect-valid '(church-encoding integer :encoding :church)
                  "FR-3403")

    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(open-union (io io) fixnum)))
    (expect-valid '(open-union (io state) fixnum)
                  "FR-3404")

    (assert-signals error
        (cl-cc/type:parse-type-specifier
         '(type-theory-equality (-> integer integer) (-> integer integer) :mode :extensional)))
    (expect-valid '(type-theory-equality (-> integer integer) (-> integer integer)
                                         :mode :extensional
                                         :evidence (proof functional-extensionality))
                  "FR-3405")))
