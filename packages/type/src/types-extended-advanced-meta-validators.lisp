;;;; types-extended-advanced-meta-validators.lisp — Implementation evidence, validation helpers, predicate functions
(in-package :cl-cc/type)

(defstruct (type-advanced-implementation-evidence
             (:constructor %make-type-advanced-implementation-evidence))
  "Concrete completion evidence for one advanced feature id."
  (feature-id "" :type string)
  (modules nil :type list)
  (api-symbols nil :type list)
  (test-anchors nil :type list)
  (summary "" :type string))

(defvar *type-advanced-implementation-evidence-registry* (make-hash-table :test #'equal)
  "Maps canonical advanced FR ids to concrete implementation evidence records.")

(defun make-type-advanced-implementation-evidence (&key id modules api-symbols test-anchors summary)
  "Construct a concrete implementation evidence record for FEATURE-ID."
  (let ((feature-id (canonicalize-type-advanced-feature-id id)))
    (unless (and (consp modules) (every #'stringp modules))
      (error "Advanced implementation evidence ~A requires non-empty module paths" feature-id))
    (unless (and (consp api-symbols) (every #'symbolp api-symbols))
      (error "Advanced implementation evidence ~A requires concrete API symbols" feature-id))
    (unless (and (consp test-anchors) (every #'symbolp test-anchors))
      (error "Advanced implementation evidence ~A requires concrete test anchors" feature-id))
    (%make-type-advanced-implementation-evidence
     :feature-id feature-id
     :modules (copy-list modules)
     :api-symbols (copy-list api-symbols)
     :test-anchors (copy-list test-anchors)
     :summary (or summary ""))))

(defun %type-advanced-implementation-evidence-spec (id modules api-symbols test-anchors &rest options)
  "Build a plist specification for implementation evidence covering FEATURE-ID."
  (list* :id id
         :modules modules
         :api-symbols api-symbols
         :test-anchors test-anchors
         options))

(defun %type-advanced-implementation-evidence-specs (ids modules api-symbols test-anchors &rest options)
  "Build implementation evidence plist specs for each id in IDS."
  (mapcar (lambda (id)
            (apply #'%type-advanced-implementation-evidence-spec
                   id modules api-symbols test-anchors options))
          ids))

(defun register-type-advanced-implementation-evidence (evidence)
  "Register EVIDENCE and return it."
  (let ((feature-id (type-advanced-implementation-evidence-feature-id evidence)))
    (unless (lookup-type-advanced-feature feature-id)
      (error "Cannot register implementation evidence for unknown advanced feature id ~A" feature-id))
    (when (gethash feature-id *type-advanced-implementation-evidence-registry*)
      (error "Duplicate advanced implementation evidence for ~A" feature-id))
    (setf (gethash feature-id *type-advanced-implementation-evidence-registry*) evidence)
    evidence))

(defun lookup-type-advanced-implementation-evidence (feature-id)
  "Return the concrete implementation evidence registered for FEATURE-ID, or NIL."
  (gethash (canonicalize-type-advanced-feature-id feature-id)
           *type-advanced-implementation-evidence-registry*))

(defun %type-advanced-invalid (advanced format-control &rest args)
  "Signal a semantic validation error for ADVANCED."
  (error "Invalid advanced type ~A (~A): ~?"
         (type-advanced-feature-id advanced)
         (type-advanced-name advanced)
         format-control
         args))

(defun %type-advanced-require-arg-count (advanced count)
  "Require ADVANCED to have exactly COUNT positional args."
  (unless (= (length (type-advanced-args advanced)) count)
    (%type-advanced-invalid advanced "expected exactly ~D positional arg(s), got ~D"
                            count (length (type-advanced-args advanced)))))

(defun %type-advanced-require-min-args (advanced count)
  "Require ADVANCED to have at least COUNT positional args."
  (unless (>= (length (type-advanced-args advanced)) count)
    (%type-advanced-invalid advanced "expected at least ~D positional arg(s), got ~D"
                            count (length (type-advanced-args advanced)))))

(defun %type-advanced-resolve-checker (designator)
  "Return a callable predicate/validator for DESIGNATOR."
  (etypecase designator
    (null nil)
    (function designator)
    (symbol (symbol-function designator))))

(defun %type-advanced-contract-property-key (entry)
  "Return the property key encoded by contract ENTRY."
  (if (consp entry) (car entry) entry))

(defun %type-advanced-contract-property-predicate (entry)
  "Return the predicate designator encoded by contract ENTRY."
  (if (consp entry) (cdr entry) nil))

(defun %type-advanced-symbol-name-member-p (value names)
  "Return T when VALUE's textual name is a member of NAMES."
  (and (or (symbolp value) (stringp value))
       (member (string-upcase (if (stringp value) value (symbol-name value)))
               names
               :test #'string=)))

(defun %type-advanced-symbolic-designator-p (value)
  "Return T when VALUE is a symbol/string designator used by semantic metadata."
  (or (symbolp value) (stringp value)))

(defun %type-advanced-boolean-value-p (value)
  "Return T when VALUE is an explicit boolean."
  (or (null value)
      (eq value t)
      (and (type-primitive-p value)
           (member (type-primitive-name value) '(t nil) :test #'eq))))

(defun %type-advanced-positive-integer-p (value)
  "Return T when VALUE is a positive integer."
  (and (integerp value) (plusp value)))

(defun %type-advanced-non-empty-symbolic-list-p (value)
  "Return T when VALUE is a non-empty list of unique symbolic designators."
  (and (listp value)
       value
       (every #'%type-advanced-symbolic-designator-p value)
       (= (length value)
          (length (remove-duplicates value :test #'equal)))))

(defun %type-advanced-interface-export-list-p (value)
  "Return T when VALUE is a non-empty list of unique interface export specs."
  (labels ((export-name (entry)
             (if (and (consp entry) (symbolp (first entry)))
                 (first entry)
                 entry))
           (export-entry-p (entry)
             (or (%type-advanced-symbolic-designator-p entry)
                 (and (consp entry)
                      (symbolp (first entry))
                      (second entry)))))
    (and (listp value)
         value
         (every #'export-entry-p value)
         (let ((names (mapcar #'export-name value)))
           (= (length names)
              (length (remove-duplicates names :test #'equal)))))))

(defun %type-advanced-stage-designator-p (value)
  "Return T when VALUE denotes a supported staging level."
  (or (member value '(0 1) :test #'eql)
      (member (%type-advanced-normalize-symbol-keyword value)
              '(:runtime :code)
              :test #'eq)))

(defun %type-advanced-stage-transition-p (value)
  "Return T when VALUE denotes a supported stage transition."
  (member (%type-advanced-normalize-symbol-keyword value)
          '(:quote :splice :run)
          :test #'eq))

(defun %type-advanced-generator-form-p (value)
  "Return T when VALUE is a plausible type-directed generator descriptor."
  (and (consp value)
       (symbolp (first value))
       (member (string-upcase (symbol-name (first value)))
               '("ARBITRARY" "ENUM" "FUZZ" "SIZED" "GENERATOR")
               :test #'string=)
       (>= (length value) 2)))

(defun %type-advanced-fingerprint-p (value)
  "Return T when VALUE is a plausible interface fingerprint token."
  (or (and (stringp value) (> (length value) 0))
      (integerp value)
      (%type-advanced-symbolic-designator-p value)))

(defun %type-advanced-smt-solver-p (value)
  "Return T when VALUE names a supported SMT solver integration target."
  (or (%type-advanced-symbol-name-member-p value '("Z3" "CVC5"))
      (not (null (lookup-smt-solver value)))))

(defun %type-advanced-smt-theory-p (value)
  "Return T when VALUE names a supported SMT theory."
  (%type-advanced-symbol-name-member-p value '("LIA" "BITVEC" "UF" "ARRAY")))

(defun %type-advanced-plugin-phase-p (value)
  "Return T when VALUE names a supported plugin hook phase."
  (%type-advanced-symbol-name-member-p value '("SOLVE" "REWRITE")))

(defun %type-advanced-synthesis-strategy-p (value)
  "Return T when VALUE names a supported synthesis strategy."
  (or (%type-advanced-symbol-name-member-p value '("ENUMERATIVE" "REFINEMENT" "PROOF-SEARCH"))
      (not (null (lookup-type-synthesis-strategy value)))))

(defun %type-advanced-mapped-transform-p (value)
  "Return T when VALUE names a supported mapped-type transform."
  (%type-advanced-symbol-name-member-p value
                                       '("OPTIONAL" "READONLY" "REQUIRED" "PARTIAL"
                                         "PICK" "OMIT" "RECORD" "EXCLUDE"
                                         "EXTRACT" "NON-NULLABLE")))

(defun %type-advanced-encoding-kind-p (value)
  "Return T when VALUE names a supported functional data encoding."
  (%type-advanced-symbol-name-member-p value '("CHURCH" "SCOTT" "PARIGOT")))

(defun %type-advanced-equality-mode-p (value)
  "Return T when VALUE names a supported equality-checking mode."
  (%type-advanced-symbol-name-member-p value '("INTENSIONAL" "EXTENSIONAL" "OBSERVATIONAL")))

(defun %type-advanced-pointerish-form-p (value)
  "Return T when VALUE resembles a typed pointer/reference descriptor."
  (and (consp value)
       (symbolp (first value))
       (member (string-upcase (symbol-name (first value)))
               '("POINTER" "PTR" "FOREIGN-POINTER" "C-PTR" "SLOT-REF" "CAR-REF")
               :test #'string=)
       (>= (length value) 2)))

(defun %type-advanced-effect-label-list-p (value)
  "Return T when VALUE is a non-empty list of unique effect labels."
  (%type-advanced-non-empty-symbolic-list-p value))

(defun %type-advanced-optic-form-p (value)
  "Return T when VALUE is a plausible lens/prism/traversal descriptor."
  (and (consp value)
       (symbolp (first value))
       (let ((head (string-upcase (symbol-name (first value)))))
         (cond
           ((or (string= head "LENS") (string= head "PRISM"))
            (= (length value) 5))
           ((string= head "TRAVERSAL")
            (>= (length value) 3))
           (t nil)))))

(defun %type-advanced-staged-form-p (value)
  "Return T when VALUE is a plausible code/splice/run descriptor."
  (and (consp value)
       (symbolp (first value))
       (member (string-upcase (symbol-name (first value)))
               '("CODE" "QUOTE" "SPLICE" "RUN")
               :test #'string=)
       (>= (length value) 2)))

