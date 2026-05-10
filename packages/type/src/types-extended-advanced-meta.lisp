;;;; types-extended-advanced-meta.lisp — Feature metadata, specs, registries, surface tables
(in-package :cl-cc/type)

;;; ─── Advanced feature metadata / representation ───────────────────────────

(defun canonicalize-type-advanced-feature-id (feature-id)
  "Return FEATURE-ID as an uppercase FR identifier string."
  (cond
    ((stringp feature-id) (string-upcase feature-id))
    ((symbolp feature-id) (string-upcase (symbol-name feature-id)))
    (t (error "Advanced feature id must be a string or symbol: ~S" feature-id))))

(defstruct (type-advanced-feature (:constructor %make-type-advanced-feature))
  "Metadata for a tracked advanced type-system feature section."
  (fr-id "" :type string)
  (title "" :type string)
  (heads nil :type list))

(defun make-type-advanced-feature (&key id title heads)
  "Construct metadata for an advanced type feature section."
  (%make-type-advanced-feature :fr-id (canonicalize-type-advanced-feature-id id)
                               :title title
                               :heads (copy-list heads)))

(defparameter +type-advanced-feature-specs+
  '(("FR-1501" "Null Safety")
    ("FR-1502" "Concurrency Safety Types")
    ("FR-1503" "Information Flow Types")
    ("FR-1504" "Region Types")
    ("FR-1505" "Capability Types")
    ("FR-1601" "Typed Holes")
    ("FR-1602" "Datatype-Generic Programming")
    ("FR-1603" "Effect Inference")
    ("FR-1604" "Value Restriction")
    ("FR-1605" "Type Error Quality")
    ("FR-1606" "Incremental Type Checking")
    ("FR-1701" "Type-Level Natural Numbers")
    ("FR-1702" "Type-Level Strings")
    ("FR-1703" "Multi-Stage Programming")
    ("FR-1704" "Typed Metaprogramming")
    ("FR-1705" "Binding-Time Analysis")
    ("FR-1801" "Lens / Optics")
    ("FR-1802" "Free Monads")
    ("FR-1803" "HLists")
    ("FR-1804" "Type-Safe printf")
    ("FR-1805" "Profunctor / Traversal Types")
    ("FR-1806" "Continuation Types")
    ("FR-1901" "Termination Checking")
    ("FR-1902" "Totality Checking")
    ("FR-1903" "Exhaustiveness Checking")
    ("FR-1904" "Positivity Checking")
    ("FR-1905" "Sized Types")
    ("FR-1906" "Guarded Recursion")
    ("FR-2001" "Curry-Howard")
    ("FR-2002" "Proof-Carrying Code")
    ("FR-2003" "Tactic-Based Type Checking")
    ("FR-2004" "Homotopy Type Theory")
    ("FR-2005" "Normalization by Evaluation")
    ("FR-2101" "Type-Directed Test Generation")
    ("FR-2102" "Type-Directed Serialization")
    ("FR-2103" "Type-Safe FFI")
    ("FR-2104" "Type-Based Fusion")
    ("FR-2105" "Type Inference Performance")
    ("FR-2106" "Type-Safe Embedded DSLs")
    ("FR-2107" "Type-Safe Database Queries")
    ("FR-2201" "Async Types")
    ("FR-2202" "Typed Channels")
    ("FR-2203" "Typed Actors")
    ("FR-2204" "STM Types")
    ("FR-2205" "Coroutine / Generator Types")
    ("FR-2206" "Data Parallel Types")
    ("FR-2301" "Numeric Tower Types")
    ("FR-2302" "Units of Measure")
    ("FR-2303" "Interval Arithmetic Types")
    ("FR-2304" "Tensor Types")
    ("FR-2305" "Precision Types")
    ("FR-2401" "Unification")
    ("FR-2402" "Skolemization")
    ("FR-2403" "Constraint Propagation")
    ("FR-2404" "Visible Type Application")
    ("FR-2405" "Type Interface Files")
    ("FR-2406" "SMT Solver Integration")
    ("FR-2501" "Dynamic Type")
    ("FR-2502" "Runtime Type Representations")
    ("FR-2503" "Template Literal Types")
    ("FR-2504" "Type Guards")
    ("FR-2505" "Typed Regular Expressions")
    ("FR-2601" "Quantified Constraints")
    ("FR-2602" "Constraint Kinds")
    ("FR-2603" "Type Class Witnesses")
    ("FR-2604" "DerivingVia")
    ("FR-2605" "Implicit Function Types")
    ("FR-2606" "Match Types")
    ("FR-2701" "Quantum Types")
    ("FR-2702" "Probabilistic Types")
    ("FR-2703" "Differentiable Types")
    ("FR-2704" "Hardware Types")
    ("FR-2705" "Type-Safe Configuration")
    ("FR-2706" "Typed State Machines")
    ("FR-2801" "Axiomatic Semantics")
    ("FR-2802" "Categorical Semantics")
    ("FR-2803" "Game Semantics")
    ("FR-2804" "Abstract Interpretation")
    ("FR-2805" "Behavioral Subtyping")
    ("FR-2901" "Strictness Analysis")
    ("FR-2902" "Type-Based Alias Analysis")
    ("FR-2903" "Boxity Analysis")
    ("FR-2904" "Arity Analysis")
    ("FR-2905" "Usage Analysis")
    ("FR-3001" "Custom Type Errors")
    ("FR-3002" "Type Checking Plugins")
    ("FR-3003" "Type-Directed Program Synthesis")
    ("FR-3004" "Cross-Language Type Mapping")
    ("FR-3005" "Type-Based Documentation")
    ("FR-3101" "Linear Logic Types")
    ("FR-3102" "Bang Types")
    ("FR-3103" "Bounded Linear Logic")
    ("FR-3104" "Structural Rules as Types")
    ("FR-3201" "Algebraic Subtyping")
    ("FR-3202" "Polar Types")
    ("FR-3203" "Path-Dependent Types")
    ("FR-3204" "Self Type")
    ("FR-3205" "Branded Types")
    ("FR-3301" "Mapped Types")
    ("FR-3302" "Conditional Types")
    ("FR-3303" "Readonly Types")
    ("FR-3304" "Utility Types")
    ("FR-3305" "Type-Safe Routing")
    ("FR-3401" "Quantitative Type Theory")
    ("FR-3402" "Graded Types")
    ("FR-3403" "Data Encodings")
    ("FR-3404" "Extensible Effects")
    ("FR-3405" "Intensional vs Extensional Type Theory")
    ("FR-3406" "Calculus of Inductive Constructions"))
  "Exact advanced FR id list represented by the type system.")

(defparameter +type-advanced-head-specs+
  '((async-function . "FR-2201") (future . "FR-2201")
    (channel . "FR-2202") (actor . "FR-2203") (stm . "FR-2204")
    (coroutine . "FR-2205") (generator . "FR-2205")
    (data-parallel . "FR-2206") (simd . "FR-2206")
    (numeric-tower . "FR-2301") (units-of-measure . "FR-2302")
    (interval . "FR-2303") (tensor . "FR-2304") (precision-type . "FR-2305")
    (dynamic . "FR-2501") (type-rep . "FR-2502") (typerep . "FR-2502")
    (template-literal . "FR-2503") (regex-type . "FR-2505")
    (quantified-constraint . "FR-2601") (constraint-kind . "FR-2602")
    (dict . "FR-2603") (type-witness . "FR-2603")
    (implicit-function . "FR-2605") (match-type . "FR-2606") (type-match . "FR-2606")
    (quantum-type . "FR-2701") (distribution . "FR-2702")
    (differentiable . "FR-2703") (hardware-type . "FR-2704")
    (config-type . "FR-2705") (state-machine . "FR-2706")
    (linear-logic . "FR-3101") (bang-type . "FR-3102")
    (complexity-type . "FR-3103") (structural-rules . "FR-3104")
    (algebraic-subtype . "FR-3201") (path-dependent . "FR-3203")
    (self-type . "FR-3204") (this-type . "FR-3204") (brand-type . "FR-3205")
    (mapped-type . "FR-3301") (conditional-type . "FR-3302")
    (deep-readonly . "FR-3303") (readonly . "FR-3303")
    (exclude-type . "FR-3304") (extract-type . "FR-3304")
    (non-nullable-type . "FR-3304") (omit-type . "FR-3304")
    (partial-type . "FR-3304") (pick-type . "FR-3304")
    (required-type . "FR-3304") (return-type-of . "FR-3304")
    (api-type . "FR-3305") (type-safe-routing . "FR-3305")
    (qtt . "FR-3401") (graded . "FR-3402")
    (church-encoding . "FR-3403") (scott-encoding . "FR-3403")
    (parigot-encoding . "FR-3403") (eff . "FR-3404") (open-union . "FR-3404")
    (type-theory-equality . "FR-3405") (cic . "FR-3406")
    (type-safe-ffi . "FR-2103"))
  "Representative surface heads for advanced feature families.")

(defvar *type-advanced-feature-registry* (make-hash-table :test #'equal)
  "Maps canonical advanced FR ids to type-advanced-feature metadata.")

(defvar *type-advanced-head-registry* (make-hash-table :test #'equal)
  "Maps uppercase representative surface heads to canonical advanced FR ids.")

(defun register-type-advanced-feature (feature)
  "Register FEATURE metadata and return FEATURE."
  (let* ((canonical-id (canonicalize-type-advanced-feature-id
                        (type-advanced-feature-fr-id feature)))
         (canonical-feature
           (make-type-advanced-feature
            :id canonical-id
            :title (type-advanced-feature-title feature)
            :heads (type-advanced-feature-heads feature))))
    (setf (gethash canonical-id *type-advanced-feature-registry*) canonical-feature)
    canonical-feature))

(defun lookup-type-advanced-feature (feature-id)
  "Return metadata for FEATURE-ID, or NIL when it is not registered."
  (gethash (canonicalize-type-advanced-feature-id feature-id)
           *type-advanced-feature-registry*))

(defun list-type-advanced-features ()
  "Return all registered advanced feature metadata sorted by FR id."
  (let (features)
    (maphash (lambda (_id feature)
               (declare (ignore _id))
               (push feature features))
             *type-advanced-feature-registry*)
    (sort features #'string< :key #'type-advanced-feature-fr-id)))

(defun list-type-advanced-feature-ids ()
  "Return all registered advanced FR ids in ascending order."
  (mapcar #'type-advanced-feature-fr-id
          (list-type-advanced-features)))

(defun register-type-advanced-head (head feature-id)
  "Associate HEAD with FEATURE-ID in the representative surface-head table."
  (let* ((key (string-upcase (if (symbolp head) (symbol-name head) head)))
         (canonical-id (canonicalize-type-advanced-feature-id feature-id))
         (feature (or (lookup-type-advanced-feature canonical-id)
                      (error "Unknown advanced feature id for head ~S: ~A" head canonical-id))))
    (setf (gethash key *type-advanced-head-registry*) canonical-id)
    (pushnew key (type-advanced-feature-heads feature) :test #'string=)
    canonical-id))

(defun type-advanced-feature-id-for-head (head)
  "Return the registered FR id for HEAD, or NIL when HEAD is not known."
  (when (or (symbolp head) (stringp head))
    (let ((key (string-upcase (if (symbolp head) (symbol-name head) head))))
      (gethash key *type-advanced-head-registry*))))

(defun type-advanced-head-p (head)
  "Return T when HEAD names a registered advanced feature surface form."
  (and (or (symbolp head) (stringp head))
       (let ((name (string-upcase (if (symbolp head) (symbol-name head) head))))
         (or (string= name "ADVANCED")
             (not (null (gethash name *type-advanced-head-registry*)))))))

(defstruct (type-advanced (:include type-node) (:constructor %make-type-advanced))
  "Validated carrier for advanced feature families."
  (feature-id "" :type string)
  (name 'advanced :type symbol)
  (args nil :type list)
  (properties nil :type list)
  (evidence nil))

(defun %type-advanced-property-sort-key (entry)
  "Return a stable textual sort key for an advanced property ENTRY."
  (let ((key (if (consp entry) (car entry) entry)))
    (if (symbolp key)
        (symbol-name key)
        (prin1-to-string key))))

(defun %type-advanced-normalize-properties (properties)
  "Return PROPERTIES in deterministic alist-key order."
  (stable-sort (copy-list properties)
               #'string<
               :key #'%type-advanced-property-sort-key))

(defun %type-advanced-normalize-graded-keyword-arg (feature-id args properties)
  "Turn (graded :grade T) parsed as a keyword property back into positional args."
  (if (and (member feature-id '("FR-3401" "FR-3402") :test #'string=)
           (null args)
           (= (length properties) 1))
      (let ((entry (first properties)))
        (values (list (car entry) (cdr entry)) nil))
      (values args properties)))

(defun type-advanced-property (advanced property &optional default)
  "Return ADVANCED's PROPERTY value, or DEFAULT when absent."
  (let ((cell (assoc property (type-advanced-properties advanced) :test #'equal)))
    (if cell (cdr cell) default)))

(defun type-advanced-property-present-p (advanced property)
  "Return T when ADVANCED contains PROPERTY."
  (not (null (assoc property (type-advanced-properties advanced) :test #'equal))))

(defparameter +type-advanced-security-label-order+
  +security-label-order+
  "Information-flow lattice from least to most confidential.")

(defun %type-advanced-normalize-symbol-keyword (value)
  "Normalize VALUE into a keyword by symbol/string name when possible."
  (cond
    ((keywordp value) value)
    ((symbolp value) (intern (symbol-name value) :keyword))
    ((stringp value) (intern (string-upcase value) :keyword))
    (t value)))

(defun %type-advanced-label-rank (label)
  "Return LABEL's rank in the advanced information-flow lattice."
  (security-label-rank label))

(defun type-advanced-security-label<= (source target)
  "Return T when information at SOURCE may flow to TARGET."
  (security-label<= source target))

(defun %type-advanced-symbol-name-in-p (value names)
  "Return T when VALUE is a symbol whose name is in NAMES."
  (and (symbolp value)
       (member (symbol-name value) names :test #'string=)))

(defun %type-advanced-head-name (value)
  "Return VALUE's list-head symbol name when VALUE is a cons form."
  (and (consp value)
       (symbolp (first value))
       (symbol-name (first value))))

(defun %type-advanced-payload-security-label (value)
  "Return the explicit security label encoded by VALUE, if any."
  (let ((head (%type-advanced-head-name value)))
    (cond
      ((null head) nil)
      ((string= head "PUBLIC") :public)
      ((string= head "TRUSTED") :trusted)
      ((string= head "TAINTED") :tainted)
      ((string= head "SECRET") :secret)
      ((string= head "TOP-SECRET") :top-secret)
      (t nil))))

(defun %type-advanced-multiplicity-p (value)
  "Return T when VALUE is a supported multiplicity/grade designator."
  (grade-designator-p value))

(defun %type-advanced-route-method-p (value)
  "Return T when VALUE names a supported HTTP route method."
  (%type-advanced-symbol-name-in-p value
                                   '("GET" "POST" "PUT" "PATCH" "DELETE" "HEAD" "OPTIONS")))

(defun type-advanced-route-p (value)
  "Return T when VALUE is a well-formed route payload for FR-3305."
  (or (and (route-p value) (route-valid-p value))
      (route-form-valid-p value)))

(defstruct (type-advanced-contract (:constructor %make-type-advanced-contract))
  "Explicit per-FR semantic validation contract for an advanced feature."
  (feature-id "" :type string)
  (semantic-domain nil)
  (exact-args nil)
  (min-args nil)
  (required-properties nil :type list)
  (property-predicates nil :type list)
  (requires-evidence-p nil :type boolean)
  (evidence-predicate nil)
  (custom-validator nil)
  (summary "" :type string))

(defvar *type-advanced-contract-registry* (make-hash-table :test #'equal)
  "Maps canonical advanced FR ids to explicit semantic contracts.")

(defun make-type-advanced-contract (&key id semantic-domain exact-args min-args
                                      required-properties property-predicates
                                      requires-evidence-p evidence-predicate
                                      custom-validator summary)
  "Construct an explicit semantic contract for advanced FEATURE-ID."
  (let ((feature-id (canonicalize-type-advanced-feature-id id)))
    (unless semantic-domain
      (error "Advanced contract ~A requires a semantic domain" feature-id))
    (unless (or exact-args min-args required-properties property-predicates
                requires-evidence-p evidence-predicate custom-validator)
      (error "Advanced contract ~A must encode at least one validation rule" feature-id))
    (%make-type-advanced-contract :feature-id feature-id
                                  :semantic-domain semantic-domain
                                  :exact-args exact-args
                                  :min-args min-args
                                  :required-properties (copy-list required-properties)
                                  :property-predicates (copy-tree property-predicates)
                                  :requires-evidence-p requires-evidence-p
                                  :evidence-predicate evidence-predicate
                                  :custom-validator custom-validator
                                  :summary (or summary ""))))

(defun %type-advanced-contract-spec (id semantic-domain &rest options)
  "Build a plist specification for the contract covering FEATURE-ID."
  (list* :id id :semantic-domain semantic-domain options))

(defun %type-advanced-contract-specs (ids semantic-domain &rest options)
  "Build contract plist specs for each id in IDS."
  (mapcar (lambda (id)
            (apply #'%type-advanced-contract-spec id semantic-domain options))
          ids))

(defun register-type-advanced-contract (contract)
  "Register CONTRACT and return it."
  (let ((feature-id (type-advanced-contract-feature-id contract)))
    (unless (lookup-type-advanced-feature feature-id)
      (error "Cannot register a contract for unknown advanced feature id ~A" feature-id))
    (when (gethash feature-id *type-advanced-contract-registry*)
      (error "Duplicate advanced contract for ~A" feature-id))
    (setf (gethash feature-id *type-advanced-contract-registry*) contract)
    contract))

(defun lookup-type-advanced-contract (feature-id)
  "Return the explicit semantic contract registered for FEATURE-ID, or NIL."
  (gethash (canonicalize-type-advanced-feature-id feature-id)
           *type-advanced-contract-registry*))

