;;;; tests/unit/optimize/optimizer-pipeline-tests.lisp
;;;; Unit tests for src/optimize/optimizer-pipeline.lisp
;;;;
;;;; Covers: opt-parse-pass-pipeline-string (tokenisation, whitespace, empty),
;;;;   opt-converged-p (eq-identity, structural-equal-not-enough, length-change),
;;;;   opt-adaptive-max-iterations (boundary regions, default clamping),
;;;;   opt-verify-instructions (success, duplicate-label, unknown-target).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── %opt-trim-whitespace ────────────────────────────────────────────────────

(deftest-each opt-trim-whitespace-cases
  "%opt-trim-whitespace strips leading/trailing spaces, tabs, and newlines."
  :cases (("spaces"      "hello"      "  hello  ")
          ("tabs"        "world"      (format nil "~Cworld~C" #\Tab #\Tab))
          ("newlines"    "foo"        (format nil "~Cfoo~C" #\Newline #\Newline))
          ("mixed"       "bar"        (format nil " ~C~C bar ~C~C " #\Tab #\Newline #\Newline #\Tab))
          ("no-trim"     "bare"       "bare")
          ("empty"       ""           ""))
  (expected input)
  (assert-equal expected (cl-cc/optimize::%opt-trim-whitespace input)))

;;; ─── opt-parse-pass-pipeline-string ─────────────────────────────────────────

(deftest parse-pass-pipeline-string-single-pass
  "opt-parse-pass-pipeline-string parses a single pass name into a one-element list."
  (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string "sccp")))
    (assert-= 1 (length result))
    (assert-eq :SCCP (first result))))

(deftest parse-pass-pipeline-string-multi-pass-comma-separated
  "opt-parse-pass-pipeline-string parses a comma-separated multi-pass string into keywords."
  (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string "sccp,cse,dce")))
    (assert-= 3 (length result))
    (assert-eq :SCCP (first  result))
    (assert-eq :CSE  (second result))
    (assert-eq :DCE  (third  result))))

(deftest parse-pass-pipeline-string-trims-whitespace
  "opt-parse-pass-pipeline-string trims whitespace around pass names."
  (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string " sccp , cse ")))
    (assert-= 2 (length result))
    (assert-eq :SCCP (first result))
    (assert-eq :CSE  (second result))))

(deftest parse-pass-pipeline-string-empty-returns-nil
  "opt-parse-pass-pipeline-string on an empty string returns nil."
  (assert-null (cl-cc/optimize::opt-parse-pass-pipeline-string "")))

;;; ─── opt-converged-p ─────────────────────────────────────────────────────────

(deftest opt-converged-p-both-nil-returns-true
  "opt-converged-p returns T when both sequences are nil (empty)."
  (assert-true (cl-cc/optimize::opt-converged-p nil nil)))

(deftest opt-converged-p-same-object-returns-true
  "opt-converged-p returns T when both sequences are the same list object."
  (let* ((i1 (make-vm-const :dst :r0 :value 1))
         (i2 (make-vm-ret  :reg :r0))
         (prog (list i1 i2)))
    (assert-true (cl-cc/optimize::opt-converged-p prog prog))))

(deftest opt-converged-p-structurally-equal-returns-true
  "opt-converged-p returns T for structurally equal (but distinct) instruction lists."
  (let* ((a (make-vm-const :dst :r0 :value 1))
         (b (make-vm-const :dst :r0 :value 1)))
    (assert-true (cl-cc/optimize::opt-converged-p (list a) (list b)))))

(deftest opt-converged-p-different-length-returns-false
  "opt-converged-p returns NIL when the two sequences have different lengths."
  (let* ((i (make-vm-const :dst :r0 :value 1)))
    (assert-false (cl-cc/optimize::opt-converged-p (list i) (list i i)))))

;;; ─── opt-adaptive-max-iterations ─────────────────────────────────────────────

(deftest-each adaptive-max-iterations-regions
  "opt-adaptive-max-iterations returns the correct budget for each size region."
  :cases (("tiny"   20  8)   ; < 50 insts: base-20 + (-12) = 8 (clamped to min 6)
          ("small"  100 14)  ; 50-149: base-20 + (-6) = 14
          ("medium" 200 20)  ; 150-399: base-20 + 0 = 20
          ("large"  500 28)  ; 400-799: base-20 + 8 = 28
          ("huge"   1000 35)) ; >= 800: base-20 + 15 = 35
  (n-insts expected)
  (let ((insts (make-list n-insts :initial-element (make-vm-const :dst :r0 :value 1))))
    (assert-= expected (cl-cc/optimize::opt-adaptive-max-iterations insts))))

(deftest adaptive-max-iterations-respects-max-cap
  "opt-adaptive-max-iterations never exceeds :max-iterations.
For n=2000 insts the natural computed value is base(20) + 15 = 35; pass a
max-iterations of 30 to actually exercise the cap clamping (35 → 30)."
  (let ((insts (make-list 2000 :initial-element (make-vm-const :dst :r0 :value 1))))
    (assert-= 30 (cl-cc/optimize::opt-adaptive-max-iterations insts :max-iterations 30))))

(deftest adaptive-max-iterations-respects-min-floor
  "opt-adaptive-max-iterations never returns less than :min-iterations."
  (let ((insts nil))  ; empty → smallest budget
    (assert-true (>= (cl-cc/optimize::opt-adaptive-max-iterations insts :min-iterations 6) 6))))

;;; ─── opt-verify-instructions ─────────────────────────────────────────────────

(deftest verify-instructions-simple-sequence-passes
  "opt-verify-instructions returns T for a simple const+ret sequence."
  (assert-true (cl-cc/optimize:opt-verify-instructions
                (list (make-vm-const :dst :r0 :value 1) (make-vm-ret :reg :r0)))))

(deftest verify-instructions-jump-with-known-label-passes
  "opt-verify-instructions returns T when a jump target label is defined in the sequence."
  (assert-true (cl-cc/optimize:opt-verify-instructions
                (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump  :label "target")
                      (make-vm-label :name "target")
                      (make-vm-ret   :reg :r0)))))

(deftest-each verify-instructions-invalid-cases
  "opt-verify-instructions signals an error for duplicate labels or unknown jump targets."
  :cases (("duplicate-label"   (list (make-vm-const :dst :r0 :value 1)
                                     (make-vm-label :name "dup")
                                     (make-vm-label :name "dup")
                                     (make-vm-ret   :reg :r0)))
          ("unknown-target"    (list (make-vm-jump :label "ghost")
                                     (make-vm-ret  :reg :r0))))
  (insts)
  (assert-signals error
    (cl-cc/optimize:opt-verify-instructions insts)))

;;; ─── opt-resolve-pass-pipeline ───────────────────────────────────────────

(deftest resolve-pass-pipeline-nil-returns-convergence-passes
  "opt-resolve-pass-pipeline on nil returns *opt-convergence-passes*."
  (assert-eq cl-cc/optimize::*opt-convergence-passes*
             (cl-cc/optimize::opt-resolve-pass-pipeline nil)))

(deftest resolve-pass-pipeline-functions-pass-through-unchanged
  "opt-resolve-pass-pipeline passes function objects through as-is."
  (let* ((fn (lambda (x) x))
         (pipeline (list fn)))
    (assert-eq fn (first (cl-cc/optimize::opt-resolve-pass-pipeline pipeline)))))

(deftest resolve-pass-pipeline-keywords-resolve-to-functions
  "opt-resolve-pass-pipeline resolves keyword pass names to function objects."
  (let ((result (cl-cc/optimize::opt-resolve-pass-pipeline (list :fold :dce))))
    (assert-= 2 (length result))
    (assert-true (every #'functionp result))))

(deftest resolve-pass-pipeline-string-parses-and-resolves
  "opt-resolve-pass-pipeline on a comma-separated string parses and resolves to functions."
  (let ((result (cl-cc/optimize::opt-resolve-pass-pipeline "fold,dce")))
    (assert-= 2 (length result))
    (assert-true (every #'functionp result))))

(deftest resolve-pass-pipeline-unknown-pass-signals-error
  "opt-resolve-pass-pipeline signals an error for an unregistered pass keyword."
  (assert-signals error
    (cl-cc/optimize::opt-resolve-pass-pipeline (list :nonexistent-pass))))

;;; ─── *opt-convergence-passes* / *opt-pass-registry* data coverage ────────

(deftest opt-pass-data-integrity
  "*opt-convergence-passes* is a non-empty function list; registry includes early Prolog/egraph passes and core cleanup passes."
  (assert-true (listp cl-cc/optimize::*opt-convergence-passes*))
  (assert-true (> (length cl-cc/optimize::*opt-convergence-passes*) 10))
  (assert-true (every #'functionp cl-cc/optimize::*opt-convergence-passes*))
  (assert-true (gethash :prolog-rewrite cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :egraph cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :fold cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :cons-slot-forward cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :pure-call-optimization cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :dce  cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :cse  cl-cc/optimize::*opt-pass-registry*))
  (assert-eq #'cl-cc/optimize::%maybe-apply-prolog-rewrite (first cl-cc/optimize::*opt-convergence-passes*))
  (assert-false (member #'cl-cc/optimize:optimize-with-egraph cl-cc/optimize::*opt-convergence-passes*))
  (assert-false (member #'cl-cc/optimize::opt-pass-fold cl-cc/optimize::*opt-convergence-passes*))
  (assert-false (member #'cl-cc/optimize::opt-pass-strength-reduce cl-cc/optimize::*opt-convergence-passes*))
  (assert-equal '(:prolog-rewrite :call-site-splitting :devirtualize :inline :sccp)
                (subseq cl-cc/optimize::*opt-default-convergence-pass-keys* 0 5))
  (assert-eq :cons-slot-forward (sixth cl-cc/optimize::*opt-default-convergence-pass-keys*))
  (assert-true (member :pure-call-optimization cl-cc/optimize::*opt-default-convergence-pass-keys*))
  (assert-true (< (position :copy-prop cl-cc/optimize::*opt-default-convergence-pass-keys*)
                  (position :pure-call-optimization cl-cc/optimize::*opt-default-convergence-pass-keys*)))
  (assert-true (< (position :pure-call-optimization cl-cc/optimize::*opt-default-convergence-pass-keys*)
                  (position :gvn cl-cc/optimize::*opt-default-convergence-pass-keys*)))
  (assert-true (< (position :pure-call-optimization cl-cc/optimize::*opt-default-convergence-pass-keys*)
                  (position :cse cl-cc/optimize::*opt-default-convergence-pass-keys*)))
  (assert-true (< (position :pure-call-optimization cl-cc/optimize::*opt-default-convergence-pass-keys*)
                  (position :dce cl-cc/optimize::*opt-default-convergence-pass-keys*))))

;;; ─── *verify-optimizer-instructions* integration ──────────────────────────

(deftest verify-optimizer-flag-runs-verifier-on-valid-input
  "*verify-optimizer-instructions* T causes opt-verify-instructions to run; valid input succeeds."
  (let ((cl-cc/optimize:*verify-optimizer-instructions* t)
        (insts (list (make-vm-const :dst :r0 :value 42) (make-vm-ret :reg :r0))))
    (assert-true (listp (cl-cc/optimize:optimize-instructions insts)))))

(deftest verify-optimizer-flag-nil-skips-verifier
  "*verify-optimizer-instructions* NIL causes optimize-instructions to skip verification."
  (let ((cl-cc/optimize:*verify-optimizer-instructions* nil)
        (insts (list (make-vm-const :dst :r0 :value 1) (make-vm-ret :reg :r0))))
    (assert-true (listp (cl-cc/optimize:optimize-instructions insts)))))

;;; ─── Prolog rewrite stage ──────────────────────────────────────────────────

(deftest prolog-rewrite-stage-disabled-is-identity
  "%maybe-apply-prolog-rewrite returns the input unchanged when the Prolog hook is disabled."
  (let ((cl-cc/optimize::*enable-prolog-peephole* nil)
        (insts (list (make-vm-const :dst :r0 :value 1)
                     (make-vm-ret :reg :r0))))
    (assert-eq insts (cl-cc/optimize::%maybe-apply-prolog-rewrite insts))))

(deftest prolog-rewrite-stage-invokes-prolog-backends
  "%maybe-apply-prolog-rewrite returns a list result when enabled."
  (let ((cl-cc/optimize::*enable-prolog-peephole* t)
        (insts (list (make-vm-const :dst :r0 :value 1)
                     (make-vm-ret :reg :r0))))
    (let ((result (cl-cc/optimize::%maybe-apply-prolog-rewrite insts)))
      (assert-true (listp result))
      (assert-= 2 (length result))
      (assert-equal (mapcar #'cl-cc/optimize::instruction->sexp insts)
                    (mapcar #'cl-cc/optimize::instruction->sexp result)))))

;;; ─── Optimize roadmap evidence and helper contracts ───────────────────────

(defun %optimizer-doc-fr-id-from-line (line)
  "Extract an FR id from an optimize roadmap heading line."
  (let ((fr-pos (and (>= (length line) 4)
                     (string= "####" (subseq line 0 4))
                     (search "FR-" line))))
    (when fr-pos
      (let ((end (+ fr-pos 3)))
        (loop while (and (< end (length line))
                         (digit-char-p (char line end)))
              do (incf end))
        (when (> end (+ fr-pos 3))
          (subseq line fr-pos end))))))

(defun %optimizer-doc-fr-ids ()
  "Return the ordered FR headings from docs/optimize-passes.md."
  (let ((ids nil))
    (dolist (line (uiop:split-string
                   (uiop:read-file-string
                    (merge-pathnames #P"docs/optimize-passes.md" (uiop:getcwd)))
                   :separator '(#\Newline))
             (nreverse ids))
      (let ((feature-id (%optimizer-doc-fr-id-from-line line)))
        (when feature-id
          (push feature-id ids))))))

(defun %optimizer-doc-content ()
  "Return the current optimize roadmap document text."
  (uiop:read-file-string
   (merge-pathnames #P"docs/optimize-passes.md" (uiop:getcwd))))

(defun %optimizer-doc-completed-heading-contradictions ()
  "Return ✅ optimize roadmap headings whose body still says implementation is absent."
  (let ((contradictions nil)
        (current-heading nil)
        (current-lines nil)
        (needles (list "未実装" "未統合" "欠落" "未接続" "未対応"
                       "未定義" "不可能" "なし")))
    (labels ((heading-line-p (line)
               (and (>= (length line) 7)
                    (string= "####" (subseq line 0 4))
                    (search "FR-" line)))
             (completed-heading-p (line)
               (search "✅" line))
             (block-text ()
               (with-output-to-string (out)
                 (dolist (line (nreverse current-lines))
                   (format out "~A~%" line))))
             (flush-block ()
               (when (and current-heading (completed-heading-p current-heading))
                 (let ((text (block-text)))
                   (when (some (lambda (needle) (search needle text)) needles)
                     (push current-heading contradictions))))
               (setf current-lines nil)))
      (dolist (line (uiop:split-string (%optimizer-doc-content)
                                       :separator (list #\Newline)))
        (if (heading-line-p line)
            (progn
              (flush-block)
              (setf current-heading line
                    current-lines (list line)))
            (when current-heading
              (push line current-lines))))
      (flush-block)
      (nreverse contradictions))))

(defun %optimizer-doc-fr-ids-from-line (line)
  "Return all FR ids mentioned in LINE."
  (let ((ids nil)
        (start 0))
    (loop for pos = (search "FR-" line :start2 start)
          while pos
          do (let ((end (+ pos 3)))
               (loop while (and (< end (length line))
                                (digit-char-p (char line end)))
                     do (incf end))
               (when (> end (+ pos 3))
                 (push (subseq line pos end) ids))
               (setf start (max (1+ pos) end))))
    (nreverse ids)))

(defun %optimizer-doc-path-token-p (token)
  "Return T when TOKEN is a repository-relative doc/code path."
  (or (and (>= (length token) 9)
           (string= "packages/" token :end2 9))
      (and (>= (length token) 5)
           (string= "docs/" token :end2 5))))

(defun %optimizer-doc-backtick-paths (line)
  "Return repository paths enclosed in backticks on LINE."
  (let ((paths nil)
        (start 0)
        (tick (code-char 96)))
    (loop for open = (position tick line :start start)
          while open
          for close = (position tick line :start (1+ open))
          while close
          do (let ((token (subseq line (1+ open) close)))
               (when (%optimizer-doc-path-token-p token)
                 (push token paths))
               (setf start (1+ close))))
    (nreverse paths)))

(defun %optimizer-doc-related-implementation-missing-paths ()
  "Return `関連実装` repository paths that do not exist in the checkout."
  (let ((missing nil))
    (loop for line in (uiop:split-string (%optimizer-doc-content)
                                         :separator (list #\Newline))
          for line-no from 1
          when (search "関連実装" line)
            do (dolist (path (%optimizer-doc-backtick-paths line))
                 (unless (probe-file (merge-pathnames path (uiop:getcwd)))
                   (push (format nil "~D:~A" line-no path) missing))))
    (nreverse missing)))

(defun %optimizer-doc-heading-status-table ()
  "Return a hash table mapping optimize roadmap FR ids to heading statuses."
  (let ((table (make-hash-table :test (function equal))))
    (dolist (feature (cl-cc/optimize::optimize-roadmap-doc-features) table)
      (setf (gethash (cl-cc/optimize::opt-roadmap-feature-id feature) table)
            (cl-cc/optimize::opt-roadmap-feature-status feature)))))

(defun %optimizer-doc-completed-sidecar-contradictions ()
  "Return `完了済みFR` entries that do not point at ✅ headings."
  (let ((status-by-id (%optimizer-doc-heading-status-table))
        (contradictions nil))
    (dolist (line (uiop:split-string (%optimizer-doc-content)
                                     :separator (list #\Newline))
                  (nreverse contradictions))
      (when (search "完了済みFR" line)
        (dolist (feature-id (%optimizer-doc-fr-ids-from-line line))
          (let ((status (gethash feature-id status-by-id)))
            (unless (eq status :implemented)
              (push (format nil "~A listed complete with status ~A"
                            feature-id status)
                    contradictions))))))))

(deftest optimize-roadmap-doc-summary-is-status-aware
  "The optimize roadmap summary must not claim every FR is fully implemented."
  (let ((doc (%optimizer-doc-content)))
    (assert-false (search "全FR実装済み" doc))
    (assert-true (search "175 FR を追跡中" doc))
    (assert-true (search "✅" doc))
    (assert-true (search "🔶" doc))))

(deftest optimize-roadmap-completed-headings-avoid-incomplete-language
  "FRs marked ✅ must not contain wording that explicitly says the implementation is absent."
  (assert-null (%optimizer-doc-completed-heading-contradictions)))

(deftest optimize-roadmap-related-implementation-paths-exist
  "Backtick paths on `関連実装` lines must point at files in the checkout."
  (assert-null (%optimizer-doc-related-implementation-missing-paths)))

(deftest optimize-roadmap-completed-sidecar-claims-match-heading-status
  "`完了済みFR` sidecar summaries must only list FR headings marked ✅."
  (assert-null (%optimizer-doc-completed-sidecar-contradictions)))

(deftest optimize-roadmap-evidence-covers-doc-fr-list
  "Every docs/optimize-passes.md FR id has status-aware optimizer roadmap evidence."
  (let* ((expected-ids (%optimizer-doc-fr-ids))
          (features (cl-cc/optimize::optimize-roadmap-doc-features))
          (actual-ids (mapcar #'cl-cc/optimize::opt-roadmap-feature-id features))
          (table (cl-cc/optimize::optimize-roadmap-register-doc-evidence))
          (implemented 0)
          (partial 0)
          (profiles nil))
     (assert-true (> (length expected-ids) 100))
     (assert-equal expected-ids actual-ids)
     (assert-= (length expected-ids) (hash-table-count table))
     (assert-true (some #'cl-cc/optimize::opt-roadmap-feature-marked-complete-p features))
     (dolist (feature features)
       (let* ((feature-id (cl-cc/optimize::opt-roadmap-feature-id feature))
              (status (cl-cc/optimize::opt-roadmap-feature-status feature))
              (evidence (cl-cc/optimize::lookup-opt-roadmap-evidence feature-id)))
         (assert-false (search ":" feature-id))
        (assert-true (member status '(:implemented :partial :planned)))
         (assert-true evidence)
        (assert-eq status (cl-cc/optimize::opt-roadmap-evidence-status evidence))
         (assert-equal feature-id
                       (cl-cc/optimize::opt-roadmap-evidence-feature-id evidence))
         (assert-true
          (cl-cc/optimize::optimize-roadmap-evidence-well-formed-p evidence))
        (push (cl-cc/optimize::opt-roadmap-evidence-modules evidence) profiles)
        (case status
          (:implemented
           (incf implemented)
           (assert-true
            (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
             evidence)))
          (:partial
           (incf partial)
           (assert-false
            (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
             evidence))))))
    (assert-true (> implemented 0))
    (assert-true (> partial 0))
    (assert-true (> (length (remove-duplicates profiles :test #'equal)) 5))))

(deftest optimizer-roadmap-core-passes-have-evidence
  "Core optimizer FRs point at concrete pass implementations and tests."
  (let ((evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-001")))
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (assert-true (member "packages/optimize/src/optimizer.lisp"
                         (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                         :test #'string=))
    (assert-true (member 'cl-cc/optimize::opt-pass-fold
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)))
    (assert-true (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                  evidence))))

(deftest optimizer-roadmap-inline-and-memory-evidence
  "Inline, IPA, and memory FRs point at their dedicated optimizer subsystems."
  (let ((evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-051")))
    (assert-eq :partial (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (assert-true (member "packages/optimize/src/optimizer-inline.lisp"
                         (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                         :test #'string=))
    (assert-true (member 'cl-cc/optimize::opt-pass-devirtualize
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)))
    (assert-false (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                    evidence))))

(deftest optimizer-roadmap-pic-evidence-is-runtime-backed
  "FR-023 must point at PIC/runtime helper evidence instead of coarse inline-only markers."
  (let ((evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-023")))
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (assert-true (member "packages/optimize/src/optimizer-pipeline.lisp"
                         (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                         :test #'string=))
    (assert-true (member 'cl-cc/optimize::opt-ic-transition
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)))
    (assert-false (member 'cl-cc/optimize::opt-pass-devirtualize
                          (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)))
    (assert-true (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                  evidence))))

(deftest optimizer-roadmap-flow-and-ssa-evidence
  "Flow and SSA FRs point at CFG/SSA/flow pass implementations."
  (let ((evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-112")))
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (assert-true (member "packages/optimize/src/ssa.lisp"
                         (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                         :test #'string=))
    (assert-true (member 'cl-cc/optimize::cfg-split-critical-edges
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)))
    (assert-true (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                  evidence))))

(deftest optimizer-roadmap-code-motion-evidence
  "Code-motion FRs distinguish conservative partial passes from implemented ones."
  (let ((partial-evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-167"))
        (implemented-evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-164")))
    (assert-eq :partial (cl-cc/optimize::opt-roadmap-evidence-status partial-evidence))
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status implemented-evidence))
    (assert-true (member 'cl-cc/optimize::opt-pass-tail-duplication
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols partial-evidence)))
    (assert-false (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                   partial-evidence))
    (assert-true (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                  implemented-evidence))))

(deftest optimizer-roadmap-callee-saved-evidence-is-native-backed
  "FR-329 must point at the native backend/regalloc implementation that computes used callee-saved registers."
  (labels ((function-present-p (package-name symbol-name)
             (let* ((pkg (find-package package-name))
                    (sym (and pkg (find-symbol symbol-name pkg))))
               (and sym (fboundp sym)))))
    (let* ((codegen-spec '("CL-CC/CODEGEN" . "X86-64-USED-CALLEE-SAVED-REGS"))
           (regalloc-spec '("CL-CC/REGALLOC" . "COMPUTE-LIVE-INTERVALS"))
           (evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-329")))
      (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
      (assert-true (member "packages/codegen/src/x86-64-codegen.lisp"
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=))
      (assert-true (member "packages/regalloc/src/regalloc.lisp"
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=))
      (assert-true (member codegen-spec
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal))
      (assert-true (member regalloc-spec
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal))
      (assert-true (function-present-p :cl-cc/codegen "X86-64-USED-CALLEE-SAVED-REGS"))
      (assert-true (function-present-p :cl-cc/regalloc "COMPUTE-LIVE-INTERVALS"))
      (assert-true (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                    evidence)))))

(deftest optimize-roadmap-runtime-helpers-have-concrete-behavior
  "Roadmap helper APIs maintain concrete PIC, profile, deopt, shape, and tiering state."
  (let ((site (cl-cc/optimize::make-opt-ic-site :max-polymorphic-entries 2)))
    (assert-eq :uninitialized (cl-cc/optimize::opt-ic-site-state site))
    (cl-cc/optimize::opt-ic-transition site :shape-a :target-a)
    (assert-eq :monomorphic (cl-cc/optimize::opt-ic-site-state site))
    (cl-cc/optimize::opt-ic-transition site :shape-b :target-b)
    (assert-eq :polymorphic (cl-cc/optimize::opt-ic-site-state site))
    (cl-cc/optimize::opt-ic-transition site :shape-c :target-c)
    (assert-eq :megamorphic (cl-cc/optimize::opt-ic-site-state site))
    (assert-= 3 (cl-cc/optimize::opt-ic-site-misses site))
    (assert-= 3 (length (cl-cc/optimize::opt-ic-site-megamorphic-fallback site))))
  (let ((spec-log (cl-cc/optimize::make-opt-speculation-log :threshold 2)))
    (assert-false (cl-cc/optimize::opt-speculation-failed-p spec-log :site :guard))
    (assert-= 1 (cl-cc/optimize::opt-record-speculation-failure spec-log :site :guard))
    (assert-false (cl-cc/optimize::opt-speculation-failed-p spec-log :site :guard))
    (assert-= 2 (cl-cc/optimize::opt-record-speculation-failure spec-log :site :guard))
    (assert-true (cl-cc/optimize::opt-speculation-failed-p spec-log :site :guard)))
  (let ((profile (cl-cc/optimize::make-opt-profile-data)))
    (assert-= 3 (cl-cc/optimize::opt-profile-record-edge profile :entry :exit 3))
    (assert-= 2 (cl-cc/optimize::opt-profile-record-value profile :ic-site 42 2))
    (assert-= 4 (cl-cc/optimize::opt-profile-record-call-chain profile '(:main :callee) 4))
    (assert-equal (cons 2 64)
                  (cl-cc/optimize::opt-profile-record-allocation profile :alloc-site 64 2)))
  (let* ((frame (cl-cc/optimize::make-opt-deopt-frame
                 :vm-pc 7
                 :register-map '((:rax . :r0) (:rbx . :r1))))
         (state (cl-cc/optimize::opt-materialize-deopt-state
                 frame
                 '((:rax . 10) (:rbx . 20)))))
    (assert-equal '((:r0 . 10) (:r1 . 20)) state))
  (let ((shape (cl-cc/optimize::make-opt-shape-descriptor-for-slots
                9
                '(:class :slots))))
    (assert-= 0 (cl-cc/optimize::opt-shape-slot-offset shape :class))
    (assert-= 1 (cl-cc/optimize::opt-shape-slot-offset shape :slots))
    (assert-null (cl-cc/optimize::opt-shape-slot-offset shape :missing)))
  (assert-= 120
            (cl-cc/optimize::opt-adaptive-compilation-threshold
             :base 90
             :warmup-p t
             :cache-pressure 0.7
             :failures 1)))

(deftest optimize-roadmap-support-helpers-have-conservative-behavior
  "Roadmap support helpers expose conservative lattice, summary, allocation, stack-map, guard, cache, module, and Sea-of-Nodes behavior."
  (let* ((bottom (cl-cc/optimize::opt-lattice-bottom))
         (seven-a (cl-cc/optimize::opt-lattice-constant 7))
         (seven-b (cl-cc/optimize::opt-lattice-constant 7))
         (eight (cl-cc/optimize::opt-lattice-constant 8))
         (overdefined (cl-cc/optimize::opt-lattice-overdefined)))
    (assert-eq :constant
               (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet bottom seven-a)))
    (assert-= 7
              (cl-cc/optimize::opt-lattice-value-value
               (cl-cc/optimize::opt-lattice-meet seven-a seven-b)))
    (assert-eq :overdefined
               (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet seven-a eight)))
    (assert-eq :overdefined
               (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet seven-a overdefined))))
  (let ((pure-summary (cl-cc/optimize::make-opt-function-summary
                       :name 'pure-helper
                       :pure-p t
                       :effects nil))
        (effectful-summary (cl-cc/optimize::make-opt-function-summary
                            :name 'effectful-helper
                            :pure-p t
                            :effects '(:heap-write))))
    (assert-true (cl-cc/optimize::opt-function-summary-safe-to-inline-p pure-summary))
    (assert-false (cl-cc/optimize::opt-function-summary-safe-to-inline-p effectful-summary)))
  (let* ((pool (cl-cc/optimize::make-opt-slab-pool :object-size 2))
         (first-object (cl-cc/optimize::opt-slab-allocate pool)))
    (assert-equal '(:slab-object 2 1) first-object)
    (cl-cc/optimize::opt-slab-free pool first-object)
    (assert-eq first-object (cl-cc/optimize::opt-slab-allocate pool)))
  (let ((region (cl-cc/optimize::make-opt-bump-region :limit 8)))
    (assert-= 0 (cl-cc/optimize::opt-bump-allocate region 3))
    (assert-= 3 (cl-cc/optimize::opt-bump-mark region))
    (assert-= 4 (cl-cc/optimize::opt-bump-allocate region 2 :alignment 4))
    (assert-null (cl-cc/optimize::opt-bump-allocate region 4))
    (cl-cc/optimize::opt-bump-reset region)
    (assert-= 3 (cl-cc/optimize::opt-bump-region-cursor region)))
  (let ((stack-map (cl-cc/optimize::make-opt-stack-map :pc 42 :roots '(:r0 :r2))))
    (assert-true (cl-cc/optimize::opt-stack-map-live-root-p stack-map :r0))
    (assert-false (cl-cc/optimize::opt-stack-map-live-root-p stack-map :r1)))
  (let ((guard (cl-cc/optimize::make-opt-guard-state :executions 9)))
    (assert-eq :tag-bit-test
               (cl-cc/optimize::opt-guard-record guard t))
    (assert-eq :full-type-check
               (cl-cc/optimize::opt-guard-record guard nil)))
  (let* ((cold (cl-cc/optimize::make-opt-jit-cache-entry :id :cold :size 8 :warmth 1))
         (warm (cl-cc/optimize::make-opt-jit-cache-entry :id :warm :size 8 :warmth 9))
         (evicted (cl-cc/optimize::opt-jit-cache-select-eviction
                   (list warm cold)
                   :current-size 90
                   :max-size 100)))
    (assert-eq cold evicted))
  (let ((summary (cl-cc/optimize::opt-merge-module-summaries
                  (list (cl-cc/optimize::make-opt-module-summary
                         :module :a
                         :exports '(foo)
                         :function-count 2)
                        (cl-cc/optimize::make-opt-module-summary
                         :module :b
                         :exports '(foo bar)
                         :function-count 3)))))
    (assert-equal '(:a :b) (getf summary :modules))
    (assert-= 5 (getf summary :function-count))
    (assert-true (member 'foo (getf summary :exports)))
    (assert-true (member 'bar (getf summary :exports))))
  (assert-true
   (cl-cc/optimize::opt-sea-node-schedulable-p
    (cl-cc/optimize::make-opt-sea-node :id :n1 :op :add :controls '(:entry))))
  (assert-false
   (cl-cc/optimize::opt-sea-node-schedulable-p
     (cl-cc/optimize::make-opt-sea-node :id :n2 :controls '(:entry)))))


(defun %optimize-backend-doc-content ()
  "Return the current optimize-backend roadmap document text."
  (uiop:read-file-string
   (merge-pathnames #P"docs/optimize-backend.md" (uiop:getcwd))))

(defun %optimize-backend-doc-completed-heading-contradictions ()
  "Return ✅ optimize-backend FR headings whose own section says implementation is absent."
  (let ((contradictions nil)
        (current-heading nil)
        (current-lines nil)
        (needles (list "未実装" "未着手" "未対応" "未完"
                       "未統合" "未接続" "欠落" "未定義" "不可能"
                       "回転命令なし" "検出なし" "エミッションなし" "分岐なし"
                       "分解なし" "ガードなし" "ABI 固定" "全 caller-saved")))
    (labels ((heading-boundary-p (line)
               (and (>= (length line) 3)
                    (string= "###" (subseq line 0 3))))
             (fr-heading-line-p (line)
               (and (>= (length line) 7)
                    (string= "####" (subseq line 0 4))
                    (search "FR-" line)))
             (completed-heading-p (line)
               (search "✅" line))
             (block-text ()
               (with-output-to-string (out)
                 (dolist (line (nreverse current-lines))
                   (format out "~A~%" line))))
             (flush-block ()
               (when (and current-heading (completed-heading-p current-heading))
                 (let ((text (block-text)))
                   (when (some (lambda (needle) (search needle text)) needles)
                     (push current-heading contradictions))))
               (setf current-lines nil)))
      (dolist (line (uiop:split-string (%optimize-backend-doc-content)
                                       :separator (list #\Newline)))
        (cond
          ((heading-boundary-p line)
           (flush-block)
           (if (fr-heading-line-p line)
               (setf current-heading line
                     current-lines (list line))
               (setf current-heading nil
                     current-lines nil)))
          (current-heading
           (push line current-lines))))
      (flush-block)
      (nreverse contradictions))))

(defun %optimize-backend-evidence-status-for-feature (feature)
  "Return expected evidence status for optimize-backend FEATURE."
  (let ((status (cl-cc/optimize::opt-roadmap-feature-status feature)))
    (case status
      (:unknown :planned)
      (otherwise status))))

(deftest optimize-backend-roadmap-completed-headings-avoid-incomplete-language
  "FRs marked ✅ in optimize-backend.md must not explicitly say they are unimplemented."
  (assert-null (%optimize-backend-doc-completed-heading-contradictions)))

(deftest optimize-backend-roadmap-evidence-covers-doc-fr-list
  "Every docs/optimize-backend.md FR id has status-aware audit evidence."
  (let* ((features (cl-cc/optimize:optimize-backend-roadmap-doc-features))
         (ids (mapcar #'cl-cc/optimize::opt-roadmap-feature-id features))
         (table (cl-cc/optimize:optimize-backend-roadmap-register-doc-evidence))
         (implemented 0)
         (not-implemented 0)
         (profiles nil))
    (assert-= 232 (length ids))
    (assert-= (length ids) (hash-table-count table))
    (dolist (feature features)
      (let* ((feature-id (cl-cc/optimize::opt-roadmap-feature-id feature))
             (doc-status (cl-cc/optimize::opt-roadmap-feature-status feature))
             (evidence-status (%optimize-backend-evidence-status-for-feature feature))
             (evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence feature-id)))
        (assert-false (search ":" feature-id))
        (assert-true (member doc-status '(:implemented :partial :planned :unknown)))
        (assert-true evidence)
        (assert-equal feature-id
                      (cl-cc/optimize::opt-roadmap-evidence-feature-id evidence))
        (assert-eq evidence-status
                   (cl-cc/optimize::opt-roadmap-evidence-status evidence))
        (assert-true
         (member "docs/optimize-backend.md"
                 (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                 :test #'string=))
        (assert-true
         (cl-cc/optimize::optimize-roadmap-evidence-well-formed-p evidence))
        (push (cl-cc/optimize::opt-roadmap-evidence-modules evidence) profiles)
        (if (eq evidence-status :implemented)
            (progn
              (incf implemented)
              (assert-true
               (cl-cc/optimize:optimize-backend-roadmap-implementation-evidence-complete-p
                evidence)))
            (progn
              (incf not-implemented)
              (assert-false
               (cl-cc/optimize:optimize-backend-roadmap-implementation-evidence-complete-p
                evidence))))))
    (assert-true (> implemented 0))
    (assert-true (> not-implemented 0))
    (assert-true (< implemented (length ids)))
    (assert-true (> (length (remove-duplicates profiles :test #'equal)) 5))))

(deftest optimize-backend-roadmap-analysis-evidence-is-loaded
  "Representative backend FR clusters expose status-aware audit anchors."
  (let ((complete 0)
        (open 0))
    (dolist (feature-id '("FR-007" "FR-116" "FR-209" "FR-282" "FR-370" "FR-502"))
      (let* ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence feature-id))
             (status (and evidence
                          (cl-cc/optimize::opt-roadmap-evidence-status evidence))))
        (assert-true evidence)
        (assert-true (member status '(:implemented :partial :planned)))
        (assert-true
         (cl-cc/optimize::optimize-roadmap-evidence-well-formed-p evidence))
        (if (eq status :implemented)
            (incf complete)
            (progn
              (incf open)
              (assert-false
               (cl-cc/optimize:optimize-backend-roadmap-implementation-evidence-complete-p
                evidence))))))
    (assert-= 6 (+ complete open))))

(deftest optimize-backend-roadmap-support-evidence-has-behavior
  "Backend support evidence exercises analysis, profile, allocation, guard, and layout helpers."
  (let* ((bottom (cl-cc/optimize::opt-lattice-bottom))
         (const-a (cl-cc/optimize::opt-lattice-constant :a))
         (const-b (cl-cc/optimize::opt-lattice-constant :b)))
    (assert-eq :constant
               (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet bottom const-a)))
    (assert-eq :overdefined
               (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet const-a const-b))))
  (let ((profile (cl-cc/optimize::make-opt-profile-data)))
    (assert-= 5 (cl-cc/optimize::opt-profile-record-edge profile :a :b 5))
    (assert-= 2 (cl-cc/optimize::opt-profile-record-call-chain profile '(:a :b) 2))
    (assert-equal (cons 3 32)
                  (cl-cc/optimize::opt-profile-record-allocation profile :site 32 3)))
  (let ((region (cl-cc/optimize::make-opt-bump-region :limit 16)))
    (assert-= 0 (cl-cc/optimize::opt-bump-allocate region 4))
    (assert-= 8 (cl-cc/optimize::opt-bump-allocate region 4 :alignment 8)))
  (let ((guard (cl-cc/optimize::make-opt-guard-state :executions 10)))
    (assert-eq :tag-bit-test (cl-cc/optimize::opt-guard-record guard t)))
  (let ((shape (cl-cc/optimize::make-opt-shape-descriptor-for-slots 1 '(:x :y :z))))
    (assert-= 2 (cl-cc/optimize::opt-shape-slot-offset shape :z))))
