;;;; optimizer-pipeline-roadmap-backend.lisp — Backend roadmap evidence profile and lookup
;;;; Extracted from optimizer-pipeline-roadmap.lisp.
;;;; Load order: after optimizer-pipeline-roadmap.
(in-package :cl-cc/optimize)

(defparameter +opt-backend-roadmap-evidence-profile-ranges+
  ;; Each entry: (lo hi modules api-symbols test-anchors)
  ;; Entries checked in order; NIL hi means open-ended (>= lo).
  '((4 56
     ("packages/compile/src/codegen.lisp"
      "packages/compile/src/codegen-control.lisp"
      "packages/compile/src/codegen-core.lisp"
      "packages/optimize/src/optimizer-memory.lisp"
      "packages/optimize/src/optimizer-inline.lisp"
      "packages/optimize/tests/optimizer-memory-tests.lisp"
      "packages/optimize/tests/optimizer-inline-tests.lisp")
     (opt-compute-points-to opt-array-bounds-check-eliminable-p
      opt-known-callee-labels opt-function-summary-safe-to-inline-p)
     (optimize-backend-roadmap-analysis-evidence-is-loaded))
    (57 184
     ("packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/src/optimizer-dataflow.lisp"
      "packages/optimize/src/optimizer-purity.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp"
      "packages/optimize/tests/optimizer-dataflow-tests.lisp")
     (opt-lattice-meet opt-run-dataflow opt-profile-record-edge
      opt-profile-record-value opt-stack-map-live-root-p)
     (optimize-backend-roadmap-support-evidence-has-behavior))
    (185 256
     ("packages/optimize/src/optimizer-inline.lisp"
      "packages/optimize/src/optimizer-memory.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/vm/src/list.lisp"
      "packages/vm/tests/vm-tests.lisp")
     (opt-profile-record-call-chain opt-profile-record-allocation
      opt-bump-allocate opt-slab-allocate opt-merge-module-summaries)
     (optimize-backend-roadmap-support-evidence-has-behavior))
    (257 305
     ("packages/optimize/src/optimizer.lisp"
      "packages/optimize/src/optimizer-memory.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-strength-tests.lisp"
      "packages/optimize/tests/optimizer-memory-tests.lisp")
     (opt-compute-cfg-value-ranges opt-compute-value-ranges
      opt-guard-record opt-weaken-guard opt-adaptive-compilation-threshold)
     (optimize-backend-roadmap-support-evidence-has-behavior))
    (306 386
     ("packages/pipeline/src/pipeline.lisp"
      "packages/compile/src/codegen.lisp"
      "packages/vm/src/vm-dispatch.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (opt-ic-transition opt-record-speculation-failure
      opt-materialize-deopt-state opt-shape-slot-offset
      opt-jit-cache-select-eviction)
     (optimize-roadmap-runtime-helpers-have-concrete-behavior))
    (387 502
     ("packages/codegen/src/x86-64-codegen.lisp"
      "packages/codegen/src/aarch64-codegen.lisp"
      "packages/regalloc/src/regalloc.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (opt-sea-node-schedulable-p opt-merge-module-summaries
      opt-stack-map-live-root-p opt-shape-slot-offset
      opt-adaptive-compilation-threshold)
     (optimize-roadmap-support-helpers-have-conservative-behavior)))
  "Alist of (lo hi modules api-symbols test-anchors) range entries for
`%opt-backend-roadmap-evidence-profile'.  The default fallback is handled
by the function itself.")

(defun %opt-backend-roadmap-evidence-profile (feature-id)
  "Return audit-evidence anchors for a docs/optimize-backend.md FR.
Backend FRs span compiler analysis, runtime modelling, native code generation,
and tooling.  Status still comes from the roadmap heading: unmarked headings
are tracked as planned evidence, not completed implementation."
  (let ((n (%opt-roadmap-feature-number feature-id)))
    (let ((entry (and n
                      (find-if (lambda (e)
                                 (destructuring-bind (lo hi . _rest) e
                                   (declare (ignore _rest))
                                   (and (<= lo n)
                                        (or (null hi) (<= n hi)))))
                               +opt-backend-roadmap-evidence-profile-ranges+))))
      (if entry
          (destructuring-bind (_lo _hi modules api-symbols test-anchors) entry
            (declare (ignore _lo _hi))
            (values modules api-symbols test-anchors))
          (values '("packages/optimize/src/optimizer-pipeline.lisp"
                    "packages/optimize/tests/optimizer-pipeline-tests.lisp")
                  '(optimize-backend-roadmap-doc-features
                    optimize-backend-roadmap-register-doc-evidence)
                  '(optimize-backend-roadmap-evidence-covers-doc-fr-list))))))

(defun make-opt-roadmap-evidence-for-feature
    (feature &key (doc-module "docs/optimize-passes.md") profile-function)
  "Create subsystem-specific evidence for FEATURE."
  (let* ((feature-id (opt-roadmap-feature-id feature))
         (status (%opt-roadmap-evidence-status feature))
         (profile-function (or profile-function #'%opt-roadmap-evidence-profile)))
    (multiple-value-bind (modules api-symbols test-anchors)
        (funcall profile-function feature-id)
      (make-opt-roadmap-evidence
       :feature-id feature-id
       :status status
       :modules (remove-duplicates
                  (cons doc-module modules)
                  :test #'string=)
       :api-symbols api-symbols
       :test-anchors test-anchors
       :summary (format nil "~A [~A]: ~A"
                        feature-id status (opt-roadmap-feature-title feature))))))

(defun optimize-roadmap-register-doc-evidence (&optional (pathname (%opt-roadmap-doc-pathname)))
  "Populate `*opt-roadmap-evidence-registry*` from docs/optimize-passes.md."
  (let ((registry (make-hash-table :test #'equal)))
    (dolist (feature (optimize-roadmap-doc-features pathname))
      (let ((evidence (make-opt-roadmap-evidence-for-feature feature)))
        (setf (gethash (opt-roadmap-evidence-feature-id evidence) registry)
              evidence)))
    (setf *opt-roadmap-evidence-registry* registry)))

(defun optimize-backend-roadmap-register-doc-evidence
    (&optional (pathname (%opt-backend-roadmap-doc-pathname)))
  "Populate `*opt-backend-roadmap-evidence-registry*` from docs/optimize-backend.md."
  (let ((registry (make-hash-table :test #'equal)))
    (dolist (feature (optimize-backend-roadmap-doc-features pathname))
      (let ((evidence (make-opt-roadmap-evidence-for-feature
                       feature
                       :doc-module "docs/optimize-backend.md"
                       :profile-function #'%opt-backend-roadmap-evidence-profile)))
        (setf (gethash (opt-roadmap-evidence-feature-id evidence) registry)
              evidence)))
    (setf *opt-backend-roadmap-evidence-registry* registry)))

(defun lookup-opt-roadmap-evidence (feature-id)
  "Return implementation evidence for FEATURE-ID, populating the registry lazily."
  (let ((registry *opt-roadmap-evidence-registry*))
    (when (zerop (hash-table-count registry))
      (setf registry (optimize-roadmap-register-doc-evidence)))
    (gethash feature-id registry)))

(defun lookup-opt-backend-roadmap-evidence (feature-id)
  "Return optimize-backend implementation evidence for FEATURE-ID."
  (let ((registry *opt-backend-roadmap-evidence-registry*))
    (when (zerop (hash-table-count registry))
      (setf registry (optimize-backend-roadmap-register-doc-evidence)))
    (gethash feature-id registry)))

(defun optimize-roadmap-implementation-evidence-complete-p (evidence)
  "Return T when EVIDENCE references concrete modules, APIs, and tests."
  (and evidence
       (eq :implemented (opt-roadmap-evidence-status evidence))
       (optimize-roadmap-evidence-well-formed-p evidence)))

(defun optimize-backend-roadmap-implementation-evidence-complete-p (evidence)
  "Return T only when optimize-backend EVIDENCE is marked implemented and its anchors resolve."
  (optimize-roadmap-implementation-evidence-complete-p evidence))

(defstruct (opt-ic-site (:conc-name opt-ic-site-))
  "Polymorphic inline-cache state for one call site."
  (state :uninitialized :type keyword)
  (entries nil :type list)
  (max-polymorphic-entries 4 :type integer)
  (misses 0 :type integer)
  (megamorphic-fallback nil))
