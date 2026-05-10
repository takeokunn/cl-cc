;;;; tests/type-2026-advanced-registry-tests.lisp - 2026 Advanced Feature Registry Tests
;;;;
;;;; Covers: advanced feature FR-id registry, implementation evidence, semantic completion,
;;;; contract registry, parser roundtrips, head aliases, and structural traversal tests.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(defun %advanced-doc-fr-ids ()
  "Return the ordered FR headings from docs/type-advanced.md."
  (let ((ids nil))
    (dolist (line (uiop:split-string
                   (uiop:read-file-string
                    (merge-pathnames #P"docs/type-advanced.md" (uiop:getcwd)))
                   :separator '(#\Newline))
              (nreverse ids))
      (let ((fr-pos (and (>= (length line) 7)
                         (string= "### " (subseq line 0 4))
                         (search "FR-" line))))
        (when (and fr-pos
                   (<= (+ fr-pos 7) (length line)))
          (push (subseq line fr-pos (+ fr-pos 7)) ids))))))

(defun %test-anchor-registered-p (anchor)
  "Return T when ANCHOR names a registered cl-cc/test test."
  (let ((test-symbol (find-symbol (symbol-name anchor) :cl-cc/test)))
    (or (and test-symbol
             (cl-cc/test:persist-lookup cl-cc/test::*test-registry* test-symbol))
        (let ((case-prefix (concatenate 'string "/" (symbol-name anchor) " ["))
              (found nil))
          (cl-cc/test:persist-each
           cl-cc/test::*test-registry*
           (lambda (name _plist)
             (declare (ignore _plist))
             (when (search case-prefix (symbol-name name))
               (setf found t))))
          found))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Advanced feature registry / representation tests
;;; ─────────────────────────────────────────────────────────────────────────

(deftest advanced-feature-registry-covers-doc-fr-list
  "The advanced feature registry covers every represented docs/type-advanced.md FR id."
  (let ((expected-ids (%advanced-doc-fr-ids))
        (actual-ids (cl-cc/type:list-type-advanced-feature-ids))
        (contract-count (hash-table-count cl-cc/type::*type-advanced-contract-registry*))
        (evidence-count (hash-table-count cl-cc/type::*type-advanced-implementation-evidence-registry*)))
    (assert-= (length expected-ids) (length actual-ids))
    (assert-= (length expected-ids) contract-count)
    (assert-= (length expected-ids) evidence-count)
    (assert-equal expected-ids actual-ids)
    (dolist (feature-id expected-ids)
      (assert-true (cl-cc/type:lookup-type-advanced-feature feature-id))
      (assert-true (cl-cc/type:type-advanced-semantics-implemented-p feature-id))
      (assert-true (cl-cc/type::lookup-type-advanced-contract feature-id))
      (assert-true (cl-cc/type::lookup-type-advanced-implementation-evidence feature-id)))))

(deftest advanced-feature-implementation-evidence-covers-all-fr-ids
  "Every advanced FR id has a concrete implementation evidence record with callable APIs."
  (let* ((expected-ids (mapcar #'first cl-cc/type:+type-advanced-feature-specs+))
         (table cl-cc/type::*type-advanced-implementation-evidence-registry*)
         (actual-ids nil))
    (maphash (lambda (feature-id evidence)
               (declare (ignore evidence))
               (push feature-id actual-ids))
             table)
    (setf actual-ids (sort actual-ids #'string<))
    (assert-= (length expected-ids) (hash-table-count table))
    (assert-equal expected-ids actual-ids)
    (dolist (feature-id expected-ids)
      (let ((evidence (cl-cc/type::lookup-type-advanced-implementation-evidence feature-id)))
        (assert-true evidence)
        (assert-true (consp (cl-cc/type::type-advanced-implementation-evidence-modules evidence)))
        (assert-true (consp (cl-cc/type::type-advanced-implementation-evidence-api-symbols evidence)))
        (assert-true (consp (cl-cc/type::type-advanced-implementation-evidence-test-anchors evidence)))
        (dolist (module (cl-cc/type::type-advanced-implementation-evidence-modules evidence))
          (assert-true (cl-cc/type::%type-advanced-implementation-module-present-p module)))
        (dolist (api (cl-cc/type::type-advanced-implementation-evidence-api-symbols evidence))
          (assert-true (fboundp api)))
        (dolist (anchor (cl-cc/type::type-advanced-implementation-evidence-test-anchors evidence))
          (assert-true (%test-anchor-registered-p anchor)))
        (assert-true (cl-cc/type::%type-advanced-implementation-evidence-complete-p evidence))))))

(deftest advanced-feature-semantic-completion-requires-implementation-evidence
  "Contract lookup alone is insufficient: completion requires a concrete evidence record."
  (let* ((cl-cc/type::*type-advanced-implementation-evidence-registry*
           (%copy-hash-table-shallow
            cl-cc/type::*type-advanced-implementation-evidence-registry*))
         (feature-id "FR-2501")
         (contract (cl-cc/type::lookup-type-advanced-contract feature-id))
         (table cl-cc/type::*type-advanced-implementation-evidence-registry*)
         (saved (cl-cc/type::lookup-type-advanced-implementation-evidence feature-id))
         (modules (cl-cc/type::type-advanced-implementation-evidence-modules saved))
         (api-symbols (cl-cc/type::type-advanced-implementation-evidence-api-symbols saved))
         (test-anchors (cl-cc/type::type-advanced-implementation-evidence-test-anchors saved)))
    (assert-true contract)
    (assert-true saved)
    (assert-true (cl-cc/type:type-advanced-semantics-implemented-p feature-id))
    (unwind-protect
        (progn
          (remhash feature-id table)
          (assert-true (cl-cc/type::lookup-type-advanced-contract feature-id))
          (assert-false (cl-cc/type::lookup-type-advanced-implementation-evidence feature-id))
          (assert-false (cl-cc/type:type-advanced-semantics-implemented-p feature-id))
          (assert-true (cl-cc/type:type-advanced-valid-p
                        (cl-cc/type:make-type-dynamic cl-cc/type:type-int))))
      (setf (gethash feature-id table) saved))
    (unwind-protect
        (progn
          (setf (gethash feature-id table)
                (cl-cc/type::%make-type-advanced-implementation-evidence
                 :feature-id feature-id
                 :modules modules
                 :api-symbols nil
                 :test-anchors test-anchors
                 :summary "missing API symbols should not satisfy completion"))
          (assert-false (cl-cc/type:type-advanced-semantics-implemented-p feature-id))
          (setf (gethash feature-id table)
                (cl-cc/type::%make-type-advanced-implementation-evidence
                 :feature-id feature-id
                 :modules modules
                 :api-symbols '(cl-cc/type::definitely-missing-advanced-api)
                 :test-anchors test-anchors
                 :summary "unbound API symbols should not satisfy completion"))
          (assert-false (cl-cc/type:type-advanced-semantics-implemented-p feature-id))
          (setf (gethash feature-id table)
                (cl-cc/type::%make-type-advanced-implementation-evidence
                 :feature-id feature-id
                 :modules modules
                 :api-symbols api-symbols
                 :test-anchors nil
                 :summary "missing test anchors should not satisfy completion"))
          (assert-false (cl-cc/type:type-advanced-semantics-implemented-p feature-id))
          (setf (gethash feature-id table)
                (cl-cc/type::%make-type-advanced-implementation-evidence
                 :feature-id feature-id
                 :modules modules
                 :api-symbols api-symbols
                 :test-anchors '(cl-cc/test::definitely-missing-advanced-test-anchor)
                 :summary "unregistered test anchors should not satisfy completion"))
          (assert-false (cl-cc/type:type-advanced-semantics-implemented-p feature-id)))
      (setf (gethash feature-id table) saved))
    (assert-true (cl-cc/type:type-advanced-semantics-implemented-p feature-id))))

(deftest advanced-feature-contract-registry-rejects-unknown-and-missing-contracts
  "Unknown FR ids and registry entries with no explicit contract are rejected at construction time."
  (let ((cl-cc/type::*type-advanced-contract-registry*
          (%copy-hash-table-shallow
           cl-cc/type::*type-advanced-contract-registry*)))
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
      (assert-true (cl-cc/type:type-advanced-semantics-implemented-p feature-id)))))

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
