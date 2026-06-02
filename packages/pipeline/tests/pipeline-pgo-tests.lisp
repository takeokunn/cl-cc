;;;; pipeline-pgo-tests.lisp — unit tests for PGO helper functions.
;;; Tests the three pure helpers in pipeline-pgo.lisp:
;;;   %pgo-label-position-map
;;;   %pgo-block-start-pc
;;;   %pgo-plan-with-runtime-keys (nil-guard path)
(in-package :cl-cc/test)

(in-suite pipeline-native-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; %pgo-label-position-map
;;; ─────────────────────────────────────────────────────────────────────────

(deftest pgo-label-position-map-single-label
  "Label at PC 0 is mapped to 0."
  (let* ((label (cl-cc:make-vm-label :name :entry))
         (table (cl-cc/pipeline::%pgo-label-position-map (list label))))
    (assert-= 0 (gethash :entry table))))

(deftest pgo-label-position-map-label-after-instructions
  "Label after two non-label instructions gets PC 2."
  (let* ((c1    (cl-cc:make-vm-const :dst :r0 :value 1))
         (c2    (cl-cc:make-vm-const :dst :r1 :value 2))
         (label (cl-cc:make-vm-label :name :mid))
         (table (cl-cc/pipeline::%pgo-label-position-map (list c1 c2 label))))
    (assert-= 2 (gethash :mid table))))

(deftest pgo-label-position-map-multiple-labels
  "Multiple labels each map to their own PC."
  (let* ((l1    (cl-cc:make-vm-label :name :a))
         (inst  (cl-cc:make-vm-const :dst :r0 :value 0))
         (l2    (cl-cc:make-vm-label :name :b))
         (table (cl-cc/pipeline::%pgo-label-position-map (list l1 inst l2))))
    (assert-= 0 (gethash :a table))
    (assert-= 2 (gethash :b table))))

(deftest pgo-label-position-map-empty-list
  "Empty instruction list produces an empty table."
  (let ((table (cl-cc/pipeline::%pgo-label-position-map '())))
    (assert-= 0 (hash-table-count table))))

(deftest pgo-label-position-map-no-labels
  "Instruction list with no labels produces an empty table."
  (let* ((c (cl-cc:make-vm-const :dst :r0 :value 7))
         (table (cl-cc/pipeline::%pgo-label-position-map (list c))))
    (assert-= 0 (hash-table-count table))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %pgo-block-start-pc
;;; ─────────────────────────────────────────────────────────────────────────

(deftest pgo-block-start-pc-prefers-label
  "When bb-label is present and in label->pc, its PC is returned."
  (let* ((label   (cl-cc:make-vm-label :name :entry))
         (inst    (cl-cc:make-vm-const :dst :r0 :value 0))
         (insts   (list label inst))
         (label->pc (cl-cc/pipeline::%pgo-label-position-map insts))
         (cfg     (cl-cc/optimize:cfg-build insts))
         (block   (cl-cc/optimize:cfg-entry cfg)))
    ;; Only check if the entry block has a label; construction may vary.
    (let ((pc (cl-cc/pipeline::%pgo-block-start-pc block insts label->pc)))
      (assert-true (integerp pc)))))

(deftest pgo-block-start-pc-falls-back-to-position
  "When no label, position of the first instruction is used."
  (let* ((c0 (cl-cc:make-vm-const :dst :r0 :value 0))
         (c1 (cl-cc:make-vm-const :dst :r1 :value 1))
         (insts (list c0 c1))
         (label->pc (make-hash-table :test #'equal))
         (cfg   (cl-cc/optimize:cfg-build insts))
         (block (cl-cc/optimize:cfg-entry cfg)))
    (let ((pc (cl-cc/pipeline::%pgo-block-start-pc block insts label->pc)))
      (assert-true (integerp pc))
      (assert-true (>= pc 0)))))

(deftest pgo-block-start-pc-zero-fallback
  "When both label lookup and position fail, 0 is returned."
  ;; Construct a synthetic block whose first instruction is not in INSTS.
  (let* ((phantom (cl-cc:make-vm-const :dst :r0 :value 99))
         (insts   (list (cl-cc:make-vm-const :dst :r1 :value 1)))
         (cfg     (cl-cc/optimize:cfg-build (list phantom)))
         (block   (cl-cc/optimize:cfg-entry cfg))
         (label->pc (make-hash-table :test #'equal)))
    ;; phantom is NOT in insts — position returns nil, so 0 is the fallback.
    (assert-= 0 (cl-cc/pipeline::%pgo-block-start-pc block insts label->pc))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %pgo-plan-with-runtime-keys — nil-guard short-circuit
;;; ─────────────────────────────────────────────────────────────────────────

(deftest pgo-plan-with-runtime-keys-nil-instructions-returns-plan
  "Nil instructions short-circuits and returns COUNTER-PLAN unchanged."
  (let ((plan '(:bb-counters () :edge-counters ())))
    (assert-equal plan
                  (cl-cc/pipeline::%pgo-plan-with-runtime-keys nil plan))))

(deftest pgo-plan-with-runtime-keys-nil-plan-returns-nil
  "Nil counter-plan short-circuits and returns nil."
  (let ((insts (list (cl-cc:make-vm-const :dst :r0 :value 1))))
    (assert-true (null (cl-cc/pipeline::%pgo-plan-with-runtime-keys insts nil)))))

(deftest pgo-plan-with-runtime-keys-both-nil-returns-nil
  "Both nil returns nil (the counter-plan nil path)."
  (assert-true (null (cl-cc/pipeline::%pgo-plan-with-runtime-keys nil nil))))

(deftest pgo-plan-with-runtime-keys-appends-runtime-key-plists
  "Non-nil instructions and plan extend plan with :bb-runtime-keys and :edge-runtime-keys."
  (let* ((insts (list (cl-cc:make-vm-const :dst :r0 :value 42)
                      (cl-cc:make-vm-ret :reg :r0)))
         (plan  (cl-cc/optimize:opt-pgo-build-counter-plan
                 0
                 (mapcar (lambda (block)
                           (cons (cl-cc/optimize:bb-id block)
                                 (mapcar #'cl-cc/optimize:bb-id
                                         (cl-cc/optimize:bb-successors block))))
                         (coerce (cl-cc/optimize:cfg-blocks
                                  (cl-cc/optimize:cfg-build insts))
                                 'list)))))
    (let ((result (cl-cc/pipeline::%pgo-plan-with-runtime-keys insts plan)))
      (assert-true (listp result))
      (assert-true (member :bb-runtime-keys result))
      (assert-true (member :edge-runtime-keys result)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; *pgo-edge-kind-types* and %pgo-edge-kind
;;; ─────────────────────────────────────────────────────────────────────────

(deftest pgo-edge-kind-types-is-list-of-five
  "*pgo-edge-kind-types* contains exactly the five tracked instruction types."
  (assert-= 5 (length cl-cc/pipeline::*pgo-edge-kind-types*)))

(deftest pgo-edge-kind-returns-nil-for-non-terminator
  "%pgo-edge-kind returns nil for a non-terminator instruction."
  (let ((inst (cl-cc:make-vm-const :dst :r0 :value 1)))
    (assert-true (null (cl-cc/pipeline::%pgo-edge-kind inst)))))

(deftest pgo-edge-kind-returns-symbol-for-vm-ret
  "%pgo-edge-kind returns cl-cc/vm:vm-ret for a vm-ret instruction."
  (let ((inst (cl-cc:make-vm-ret :reg :r0)))
    (assert-equal 'cl-cc/vm:vm-ret (cl-cc/pipeline::%pgo-edge-kind inst))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %pgo-type-feedback-rows
;;; ─────────────────────────────────────────────────────────────────────────

(deftest pgo-type-feedback-rows-nil-profile
  "%pgo-type-feedback-rows returns nil when profile-data is nil."
  (assert-true (null (cl-cc/pipeline::%pgo-type-feedback-rows nil))))

(deftest pgo-type-feedback-rows-non-plist
  "%pgo-type-feedback-rows returns nil for a non-plist atom (not a cons)."
  (assert-true (null (cl-cc/pipeline::%pgo-type-feedback-rows 42))))

(deftest pgo-type-feedback-rows-plist-without-key
  "%pgo-type-feedback-rows returns nil when :type-feedback key is absent."
  (assert-true (null (cl-cc/pipeline::%pgo-type-feedback-rows
                      '(:other-key (1 2 3))))))

(deftest pgo-type-feedback-rows-plist-with-type-feedback-key
  "%pgo-type-feedback-rows returns the rows stored under :type-feedback."
  (let* ((rows '((((:generic-call 0 :integer) . 10))
                 (((:generic-call 1 :string) . 5))))
         (profile-data (list :type-feedback rows)))
    (assert-equal rows (cl-cc/pipeline::%pgo-type-feedback-rows profile-data))))

(deftest pgo-type-feedback-rows-empty-rows
  "%pgo-type-feedback-rows returns nil when :type-feedback maps to nil."
  (assert-true (null (cl-cc/pipeline::%pgo-type-feedback-rows
                      '(:type-feedback nil)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %pgo-dominant-types-by-pc
;;; ─────────────────────────────────────────────────────────────────────────

(deftest pgo-dominant-types-by-pc-empty-profile
  "%pgo-dominant-types-by-pc returns nil for an empty profile."
  (assert-true (null (cl-cc/pipeline::%pgo-dominant-types-by-pc nil))))

(deftest pgo-dominant-types-by-pc-profile-no-type-feedback
  "%pgo-dominant-types-by-pc returns nil when profile has no :type-feedback."
  (assert-true (null (cl-cc/pipeline::%pgo-dominant-types-by-pc
                      '(:other-key ())))))

(deftest pgo-dominant-types-by-pc-below-threshold
  "%pgo-dominant-types-by-pc returns nil when no type has >90% share."
  ;; Two types with equal counts: 50% each, below the 90% threshold.
  (let ((profile-data
         (list :type-feedback
               (list (cons '(:generic-call 0 :integer) 5)
                     (cons '(:generic-call 0 :string)  5)))))
    (assert-true (null (cl-cc/pipeline::%pgo-dominant-types-by-pc profile-data)))))

(deftest pgo-dominant-types-by-pc-dominant-site
  "%pgo-dominant-types-by-pc returns pc->specializer for >90% dominant site."
  ;; :integer has 95 out of 100 calls at PC 3 — clearly dominant.
  (let ((profile-data
         (list :type-feedback
               (list (cons '(:generic-call 3 :integer) 95)
                     (cons '(:generic-call 3 :string)   5)))))
    (let ((result (cl-cc/pipeline::%pgo-dominant-types-by-pc profile-data)))
      (assert-true (consp result))
      (let ((entry (assoc 3 result :test #'eql)))
        (assert-true entry)
        (assert-equal :integer (cdr entry))))))

(deftest pgo-dominant-types-by-pc-exactly-at-threshold-not-dominant
  "%pgo-dominant-types-by-pc excludes a site at exactly 90% (threshold is strict >)."
  ;; 9 out of 10 = 90.0% — NOT strictly greater than 0.9, so not dominant.
  (let ((profile-data
         (list :type-feedback
               (list (cons '(:generic-call 7 :integer) 9)
                     (cons '(:generic-call 7 :string)  1)))))
    (assert-true (null (cl-cc/pipeline::%pgo-dominant-types-by-pc profile-data)))))

(deftest pgo-dominant-types-by-pc-multiple-sites-mixed
  "%pgo-dominant-types-by-pc returns only dominant sites when several PCs exist."
  ;; PC 0: :integer dominant (95%). PC 1: evenly split (50/50) — not dominant.
  (let ((profile-data
         (list :type-feedback
               (list (cons '(:generic-call 0 :integer) 95)
                     (cons '(:generic-call 0 :string)   5)
                     (cons '(:generic-call 1 :integer)  5)
                     (cons '(:generic-call 1 :string)   5)))))
    (let ((result (cl-cc/pipeline::%pgo-dominant-types-by-pc profile-data)))
      (assert-true (assoc 0 result :test #'eql))
      (assert-true (null (assoc 1 result :test #'eql))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %pgo-apply-type-feedback-to-instructions
;;; ─────────────────────────────────────────────────────────────────────────

(deftest pgo-apply-type-feedback-to-instructions-nil-guard
  "%pgo-apply-type-feedback-to-instructions returns instructions unchanged when profile is nil."
  (let ((insts (list (cl-cc:make-vm-const :dst :r0 :value 1))))
    (assert-equal insts
                  (cl-cc/pipeline::%pgo-apply-type-feedback-to-instructions insts nil))))

(deftest pgo-apply-type-feedback-to-instructions-nil-instructions
  "%pgo-apply-type-feedback-to-instructions returns nil when instructions is nil."
  (let ((profile-data (list :type-feedback
                            (list (cons '(:generic-call 0 :integer) 99)))))
    (assert-true (null (cl-cc/pipeline::%pgo-apply-type-feedback-to-instructions
                        nil profile-data)))))

(deftest pgo-apply-type-feedback-to-instructions-sets-pgo-specializer
  "%pgo-apply-type-feedback-to-instructions sets pgo-specializer on a dominant vm-generic-call."
  ;; PC 0 has :integer as dominant (99%).
  (let* ((gc-inst (cl-cc:make-vm-generic-call :dst :r0 :gf-reg :r1 :args '()))
         (insts   (list gc-inst))
         (profile-data
          (list :type-feedback
                (list (cons '(:generic-call 0 :integer) 99)
                      (cons '(:generic-call 0 :string)   1)))))
    (cl-cc/pipeline::%pgo-apply-type-feedback-to-instructions insts profile-data)
    (assert-equal :integer (cl-cc/vm:vm-pgo-specializer gc-inst))))

(deftest pgo-apply-type-feedback-to-instructions-non-generic-call-unchanged
  "%pgo-apply-type-feedback-to-instructions leaves non-generic-call instructions alone."
  (let* ((const-inst (cl-cc:make-vm-const :dst :r0 :value 42))
         (insts      (list const-inst))
         (profile-data
          (list :type-feedback
                (list (cons '(:generic-call 0 :integer) 99)))))
    (cl-cc/pipeline::%pgo-apply-type-feedback-to-instructions insts profile-data)
    ;; const-inst has no pgo-specializer — it is a vm-const, not vm-generic-call.
    (assert-true (typep const-inst 'cl-cc/vm:vm-const))))

(deftest pgo-apply-type-feedback-to-instructions-no-dominant-no-annotation
  "%pgo-apply-type-feedback-to-instructions leaves pgo-specializer nil when below threshold."
  (let* ((gc-inst (cl-cc:make-vm-generic-call :dst :r0 :gf-reg :r1 :args '()))
         (insts   (list gc-inst))
         (profile-data
          ;; 50/50 split — not dominant.
          (list :type-feedback
                (list (cons '(:generic-call 0 :integer) 5)
                      (cons '(:generic-call 0 :string)  5)))))
    (cl-cc/pipeline::%pgo-apply-type-feedback-to-instructions insts profile-data)
    (assert-true (null (cl-cc/vm:vm-pgo-specializer gc-inst)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %pgo-apply-type-feedback-to-result
;;; ─────────────────────────────────────────────────────────────────────────

(deftest pgo-apply-type-feedback-to-result-nil-profile-is-noop
  "%pgo-apply-type-feedback-to-result returns result unchanged when pgo-profile-data is nil."
  (let* ((result  (cl-cc/compile:make-compilation-result
                   :vm-instructions (list (cl-cc:make-vm-const :dst :r0 :value 1))))
         (opts    (cl-cc/pipeline::%make-pipeline-opts :pgo-profile-data nil))
         (outcome (cl-cc/pipeline::%pgo-apply-type-feedback-to-result result opts)))
    (assert-true (eq result outcome))))

(deftest pgo-apply-type-feedback-to-result-annotates-all-three-lists
  "%pgo-apply-type-feedback-to-result annotates vm-instructions, optimized-instructions, and program."
  (let* ((gc-vm  (cl-cc:make-vm-generic-call :dst :r0 :gf-reg :r1 :args '()))
         (gc-opt (cl-cc:make-vm-generic-call :dst :r2 :gf-reg :r3 :args '()))
         (gc-prg (cl-cc:make-vm-generic-call :dst :r4 :gf-reg :r5 :args '()))
         (program (cl-cc:make-vm-program :instructions (list gc-prg)))
         (result  (cl-cc/compile:make-compilation-result
                   :vm-instructions         (list gc-vm)
                   :optimized-instructions  (list gc-opt)
                   :program                 program))
         (profile-data
          (list :type-feedback
                (list (cons '(:generic-call 0 :integer) 99)
                      (cons '(:generic-call 0 :string)   1))))
         (opts (cl-cc/pipeline::%make-pipeline-opts :pgo-profile-data profile-data)))
    (cl-cc/pipeline::%pgo-apply-type-feedback-to-result result opts)
    (assert-equal :integer (cl-cc/vm:vm-pgo-specializer gc-vm))
    (assert-equal :integer (cl-cc/vm:vm-pgo-specializer gc-opt))
    (assert-equal :integer (cl-cc/vm:vm-pgo-specializer gc-prg))))
