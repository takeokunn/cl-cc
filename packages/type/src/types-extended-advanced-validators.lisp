;;;; types-extended-advanced-validators.lisp — 22 %type-advanced-validate-* functions
(in-package :cl-cc/type)

(defun %type-advanced-validate-information-flow (advanced)
  "Validate FR-1503 information-flow labels and declassification evidence."
  (%type-advanced-require-min-args advanced 1)
  (let* ((source (%type-advanced-payload-security-label (first (type-advanced-args advanced))))
         (target (type-advanced-property advanced :flow)))
    (when (and target (null (%type-advanced-label-rank target)))
      (%type-advanced-invalid advanced "unknown information-flow target label ~S" target))
    (when (and source target
               (not (type-advanced-security-label<= source target))
               (null (type-advanced-evidence advanced)))
      (%type-advanced-invalid advanced "flow from ~S to ~S requires declassification evidence"
                              source target))))

(defun %type-advanced-validate-type-safe-ffi (advanced)
  "Validate FR-2103 typed FFI descriptors at the boundary shape level."
  (%type-advanced-require-min-args advanced 1)
  (dolist (arg (type-advanced-args advanced))
    (unless (ffi-descriptor-form-valid-p arg)
      (%type-advanced-invalid advanced "malformed FFI descriptor ~S" arg))))

(defun %type-advanced-validate-route (advanced)
  "Validate FR-3305 API route payloads."
  (%type-advanced-require-min-args advanced 1)
  (dolist (route (type-advanced-args advanced))
    (unless (type-advanced-route-p route)
      (%type-advanced-invalid advanced "malformed route payload ~S" route))))

(defun %type-advanced-validate-proof-like (advanced)
  "Validate proof/totality features that require machine-checkable evidence."
  (%type-advanced-require-min-args advanced 1)
  (let ((evidence (type-advanced-evidence advanced))
        (feature-id (type-advanced-feature-id advanced)))
    (unless evidence
      (%type-advanced-invalid advanced "requires explicit proof/termination evidence"))
    (when (member feature-id '("FR-1901" "FR-1902" "FR-1903" "FR-1904" "FR-1905" "FR-1906")
                  :test #'string=)
      (unless (or (termination-evidence-p evidence)
                  (termination-evidence-form-valid-p evidence)
                  (proof-evidence-form-valid-p evidence))
        (%type-advanced-invalid advanced "malformed totality evidence ~S" evidence)))
    (when (member feature-id '("FR-2001" "FR-2002" "FR-2003" "FR-2004" "FR-2005" "FR-3406")
                  :test #'string=)
      (unless (or (proof-evidence-form-valid-p evidence)
                  (cic-proof-valid-p evidence))
        (%type-advanced-invalid advanced "malformed proof evidence ~S" evidence)))))

(defun %type-advanced-validate-incremental-checking (advanced)
  "Validate FR-1606 incremental type-checking cache contracts."
  (let ((dependency-graph (type-advanced-property advanced :dependency-graph))
        (cache (type-advanced-property advanced :cache)))
    (when (equal dependency-graph cache)
      (%type-advanced-invalid advanced
                              "dependency graph and cache descriptors must be distinct, got ~S"
                              dependency-graph))))

(defun %type-advanced-validate-staging (advanced)
  "Validate FR-1703 staged payloads and stage transitions."
  (let* ((payload (first (type-advanced-args advanced)))
         (stage (type-advanced-property advanced :stage))
         (transition (%type-advanced-normalize-symbol-keyword
                      (type-advanced-property advanced :transition))))
    (unless (or (typep payload 'type-node)
                (%type-advanced-staged-form-p payload))
      (%type-advanced-invalid advanced
                              "staging payload must be a code/run/splice form or type node, got ~S"
                              payload))
    (when (member transition '(:run :splice) :test #'eq)
      (unless (type-advanced-evidence advanced)
        (%type-advanced-invalid advanced
                                "~S transition requires stage-safety evidence"
                                transition)))
    (when (and (eq transition :run)
               (not (or (eql stage 1)
                        (eq (%type-advanced-normalize-symbol-keyword stage) :code))))
      (%type-advanced-invalid advanced
                              ":run transition requires stage 1 / :code input, got ~S"
                              stage))))

(defun %type-advanced-validate-optics (advanced)
  "Validate FR-1801 optic descriptors."
  (let ((payload (first (type-advanced-args advanced))))
    (unless (%type-advanced-optic-form-p payload)
      (%type-advanced-invalid advanced
                              "optics payload must be a lens/prism/traversal descriptor, got ~S"
                              payload))))

(defun %type-advanced-validate-test-generation (advanced)
  "Validate FR-2101 type-directed generator configuration."
  (let ((coverage-target (type-advanced-property advanced :coverage-target))
        (samples (type-advanced-property advanced :samples)))
    (when (and samples (> samples coverage-target))
      (%type-advanced-invalid advanced
                              ":samples must not exceed :coverage-target (~S > ~S)"
                              samples
                              coverage-target))))

(defun %type-advanced-validate-interface-files (advanced)
  "Validate FR-2405 interface-file cache metadata."
  (let ((exports (type-advanced-property advanced :exports)))
    (unless (= (length exports)
               (length (remove-duplicates exports :test #'equal)))
      (%type-advanced-invalid advanced
                              "interface exports must be unique, got ~S"
                              exports))))

(defun %type-advanced-validate-smt-integration (advanced)
  "Validate FR-2406 SMT integration metadata."
  (unless (or (type-advanced-evidence advanced)
              (type-advanced-property-present-p advanced :counterexample))
    (%type-advanced-invalid advanced
                            "SMT integration requires either proof evidence or a :counterexample payload")))

(defun %type-advanced-validate-abstract-interpretation (advanced)
  "Validate FR-2804 abstract-interpretation descriptors."
  (let ((widening (type-advanced-property advanced :widening))
        (narrowing (type-advanced-property advanced :narrowing)))
    (when (and (type-advanced-property-present-p advanced :narrowing)
               (equal widening narrowing))
      (%type-advanced-invalid advanced
                              "widening and narrowing descriptors must differ, got ~S"
                              widening))))

(defun %type-advanced-validate-alias-analysis (advanced)
  "Validate FR-2902 alias-analysis descriptors."
  (let ((left (first (type-advanced-args advanced)))
        (right (second (type-advanced-args advanced)))
        (disjoint (type-advanced-property advanced :disjoint)))
    (unless (%type-advanced-pointerish-form-p left)
      (%type-advanced-invalid advanced "left alias operand must be pointer-like, got ~S" left))
    (unless (%type-advanced-pointerish-form-p right)
      (%type-advanced-invalid advanced "right alias operand must be pointer-like, got ~S" right))
    (when (and disjoint (equal left right))
      (%type-advanced-invalid advanced
                              "disjoint alias operands must not be structurally identical: ~S"
                              left))))

(defun %type-advanced-validate-plugins (advanced)
  "Validate FR-3002 plugin hook descriptors."
  (unless (%type-advanced-symbolic-designator-p (first (type-advanced-args advanced)))
    (%type-advanced-invalid advanced "plugin descriptor must start with a symbolic plugin name")))

(defun %type-advanced-validate-synthesis (advanced)
  "Validate FR-3003 synthesis strategy descriptors."
  (let ((strategy (%type-advanced-normalize-symbol-keyword
                   (type-advanced-property advanced :search))))
    (when (and (eq strategy :proof-search)
               (null (type-advanced-evidence advanced)))
      (%type-advanced-invalid advanced
                              "proof-search synthesis requires evidence describing the proof search goal"))))

(defun %type-advanced-validate-brand (advanced)
  "Validate FR-3205 branded-type payloads."
  (unless (or (symbolp (first (type-advanced-args advanced)))
              (stringp (first (type-advanced-args advanced))))
    (%type-advanced-invalid advanced "brand-type requires a symbolic brand name")))

(defun %type-advanced-validate-mapped-types (advanced)
  "Validate FR-3301 mapped-type transforms."
  (let ((base (first (type-advanced-args advanced)))
        (filter (type-advanced-property advanced :filter :absent)))
    (unless (or (typep base 'type-node)
                (consp base)
                (%type-advanced-symbolic-designator-p base))
      (%type-advanced-invalid advanced
                              "mapped-type base must be a type node or structured type form, got ~S"
                              base))
    (when (and (not (eq filter :absent))
               (not (or (%type-advanced-symbolic-designator-p filter)
                        (consp filter)
                        (typep filter 'type-node))))
      (%type-advanced-invalid advanced
                              ":filter must be a symbolic predicate or type form, got ~S"
                              filter))))

(defun %type-advanced-validate-conditional-types (advanced)
  "Validate FR-3302 conditional/infer-type descriptors."
  (let ((then-branch (type-advanced-property advanced :then))
        (else-branch (type-advanced-property advanced :else)))
    (when (type-advanced-property-present-p advanced :infer)
      (unless (%type-advanced-symbolic-designator-p (type-advanced-property advanced :infer))
        (%type-advanced-invalid advanced ":infer must name a symbolic type variable")))
    (when (equal then-branch else-branch)
      (%type-advanced-invalid advanced
                              "conditional branches must differ to encode a real type split, got ~S"
                              then-branch))))

(defun %type-advanced-validate-encodings (advanced)
  "Validate FR-3403 functional data encoding descriptors."
  (let* ((encoding (%type-advanced-normalize-symbol-keyword
                    (type-advanced-property advanced :encoding)))
         (head-name (string-upcase (symbol-name (type-advanced-name advanced))))
         (expected (cond
                     ((string= head-name "CHURCH-ENCODING") :church)
                     ((string= head-name "SCOTT-ENCODING") :scott)
                     ((string= head-name "PARIGOT-ENCODING") :parigot)
                     (t nil))))
    (when (and expected (not (eq expected encoding)))
      (%type-advanced-invalid advanced
                              "encoding property ~S must agree with surface head ~S"
                              encoding
                              (type-advanced-name advanced)))))

(defun %type-advanced-validate-extensible-effects (advanced)
  "Validate FR-3404 extensible-effect descriptors."
  (let ((effects (first (type-advanced-args advanced))))
    (unless (%type-advanced-effect-label-list-p effects)
      (%type-advanced-invalid advanced
                              "extensible effects require a non-empty list of unique effect labels, got ~S"
                              effects))))

(defun %type-advanced-validate-type-theory-equality (advanced)
  "Validate FR-3405 equality-mode descriptors."
  (let* ((mode (%type-advanced-normalize-symbol-keyword
                (type-advanced-property advanced :mode)))
         (left (first (type-advanced-args advanced)))
         (right (second (type-advanced-args advanced))))
    (cond
      ((eq mode :intensional)
       (unless (type-advanced-payload-equal-p left right)
         (%type-advanced-invalid advanced
                                 "intensional equality requires computationally identical payloads, got ~S and ~S"
                                 left
                                 right)))
      ((member mode '(:extensional :observational) :test #'eq)
       (unless (type-advanced-evidence advanced)
         (%type-advanced-invalid advanced
                                 "~S equality requires supporting evidence"
                                 mode))))))

(defun %type-advanced-validate-qtt (advanced)
  "Validate FR-3401 quantitative multiplicity payloads."
  (unless (valid-multiplicity-p (first (type-advanced-args advanced)))
    (%type-advanced-invalid advanced
                            "unsupported QTT multiplicity ~S"
                            (first (type-advanced-args advanced)))))

(defun %type-advanced-validate-graded (advanced)
  "Validate FR-3402 graded-type multiplicity payloads."
  (unless (%type-advanced-multiplicity-p (first (type-advanced-args advanced)))
    (%type-advanced-invalid advanced
                            "unsupported multiplicity/grade ~S"
                            (first (type-advanced-args advanced)))))

