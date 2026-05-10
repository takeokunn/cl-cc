;;;; types-extended-advanced-validate.lisp — Validation logic for advanced type nodes
;;;; Extracted from types-extended-advanced-data.lisp.
;;;; Load order: after types-extended-advanced-data, before types-extended-advanced-init.
(in-package :cl-cc/type)

(defun %make-type-advanced-contract-from-spec (spec)
  "Construct a semantic contract object from plist SPEC."
  (apply #'make-type-advanced-contract spec))

(defun %make-type-advanced-implementation-evidence-from-spec (spec)
  "Construct a concrete implementation evidence object from plist SPEC."
  (apply #'make-type-advanced-implementation-evidence spec))

(defun %type-advanced-implementation-api-available-p (symbol)
  "Return T when SYMBOL names a loaded concrete helper API."
  (and (symbolp symbol) (fboundp symbol)))

(defun %type-advanced-implementation-module-present-p (path)
  "Return T when PATH identifies a source module present in this checkout."
  (and (stringp path)
       (or (ignore-errors
             (probe-file (asdf:system-relative-pathname :cl-cc path)))
           (probe-file (merge-pathnames path *default-pathname-defaults*)))))

(defun %type-advanced-implementation-test-anchor-available-p (anchor)
  "Return T when ANCHOR has a registered test when the test package is loaded."
  (and (symbolp anchor)
       (let ((test-package (find-package :cl-cc/test)))
         (if test-package
             (let ((test-symbol (find-symbol (symbol-name anchor) test-package))
                   (registry-symbol (find-symbol "*TEST-REGISTRY*" test-package))
                   (lookup-symbol (find-symbol "PERSIST-LOOKUP" test-package))
                   (each-symbol (find-symbol "PERSIST-EACH" test-package)))
               (and registry-symbol
                    lookup-symbol
                    each-symbol
                    (boundp registry-symbol)
                    (fboundp lookup-symbol)
                    (fboundp each-symbol)
                    (or (and test-symbol
                             (funcall (symbol-function lookup-symbol)
                                      (symbol-value registry-symbol)
                                      test-symbol))
                        (let ((case-prefix (concatenate 'string "/" (symbol-name anchor) " ["))
                              (found nil))
                          (funcall (symbol-function each-symbol)
                                   (symbol-value registry-symbol)
                                   (lambda (name _plist)
                                     (declare (ignore _plist))
                                     (when (search case-prefix (symbol-name name))
                                       (setf found t))))
                          found))))
             t))))

(defun %type-advanced-implementation-evidence-complete-p (evidence)
  "Return T when EVIDENCE references concrete loaded APIs and tests."
  (and evidence
        (consp (type-advanced-implementation-evidence-modules evidence))
        (consp (type-advanced-implementation-evidence-api-symbols evidence))
        (consp (type-advanced-implementation-evidence-test-anchors evidence))
        (every #'%type-advanced-implementation-module-present-p
               (type-advanced-implementation-evidence-modules evidence))
        (every #'%type-advanced-implementation-api-available-p
               (type-advanced-implementation-evidence-api-symbols evidence))
        (every #'%type-advanced-implementation-test-anchor-available-p
               (type-advanced-implementation-evidence-test-anchors evidence))))

(defun type-advanced-semantics-implemented-p (feature-id)
  "Return T when FEATURE-ID has both a semantic contract and concrete evidence."
  (let ((contract (lookup-type-advanced-contract feature-id))
        (evidence (lookup-type-advanced-implementation-evidence feature-id)))
    (and contract
         (%type-advanced-implementation-evidence-complete-p evidence))))

(defun %type-advanced-validate-contract (advanced contract)
  "Validate ADVANCED against the explicit per-FR CONTRACT."
  (let ((exact-args (type-advanced-contract-exact-args contract))
        (min-args (type-advanced-contract-min-args contract)))
    (when exact-args
      (%type-advanced-require-arg-count advanced exact-args))
    (when (and min-args (null exact-args))
      (%type-advanced-require-min-args advanced min-args)))
  (dolist (property (type-advanced-contract-required-properties contract))
    (unless (type-advanced-property-present-p advanced property)
      (%type-advanced-invalid advanced "missing required property ~S" property)))
  (dolist (entry (type-advanced-contract-property-predicates contract))
    (let ((property (%type-advanced-contract-property-key entry))
          (predicate (%type-advanced-contract-property-predicate entry)))
      (when (type-advanced-property-present-p advanced property)
        (let ((value (type-advanced-property advanced property)))
          (unless (funcall (%type-advanced-resolve-checker predicate) value)
            (%type-advanced-invalid advanced
                                    "property ~S failed contract predicate ~S with value ~S"
                                    property
                                    predicate
                                    value))))))
  (let ((evidence (type-advanced-evidence advanced))
        (evidence-predicate (type-advanced-contract-evidence-predicate contract)))
    (when (and (type-advanced-contract-requires-evidence-p contract)
               (null evidence))
      (%type-advanced-invalid advanced "requires explicit semantic evidence"))
    (when (and evidence evidence-predicate
               (not (funcall (%type-advanced-resolve-checker evidence-predicate) evidence)))
      (%type-advanced-invalid advanced
                              "evidence failed contract predicate ~S with value ~S"
                              evidence-predicate
                              evidence)))
  (let ((custom-validator (type-advanced-contract-custom-validator contract)))
    (when custom-validator
      (funcall (%type-advanced-resolve-checker custom-validator) advanced)))
  advanced)

(defun %type-advanced-validate-by-feature (advanced)
  "Apply the explicit semantic contract registered for ADVANCED."
  (let ((contract (lookup-type-advanced-contract (type-advanced-feature-id advanced))))
    (unless contract
      (%type-advanced-invalid advanced "no semantic contract is implemented"))
    (%type-advanced-validate-contract advanced contract)))

(defun validate-type-advanced (advanced)
  "Validate ADVANCED against its registered feature and explicit semantic contract."
  (unless (type-advanced-p advanced)
    (error "Expected type-advanced node, got ~S" advanced))
  (unless (lookup-type-advanced-feature (type-advanced-feature-id advanced))
    (%type-advanced-invalid advanced "unregistered feature id"))
  (%type-advanced-validate-by-feature advanced)
  advanced)

(defun type-advanced-valid-p (advanced)
  "Return T when ADVANCED passes semantic validation."
  (handler-case
      (progn (validate-type-advanced advanced) t)
    (error () nil)))
