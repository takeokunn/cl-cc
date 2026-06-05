;;;; tests/unit/optimize/optimizer-concurrency-tests.lisp
;;;; Unit tests for concurrency/async helpers in the optimizer:
;;;; async state machine, coroutine strategy, channel select, STM, lock-free reclamation.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Async state machine ─────────────────────────────────────────────────────

(deftest optimize-build-async-state-machine-builds-linear-transitions
  "opt-build-async-state-machine creates one transition per await point."
  (let* ((sm (cl-cc/optimize::opt-build-async-state-machine '(:await-1 :await-2)))
         (trans (cl-cc/optimize::opt-async-sm-transitions sm)))
    (assert-= 3 (length (cl-cc/optimize::opt-async-sm-states sm)))
    (assert-= 2 (length trans))
    (assert-eq :await-1 (getf (first trans) :await))
    (assert-= 2 (getf (second trans) :to))))

;;; ─── Coroutine strategy ──────────────────────────────────────────────────────

(deftest optimize-choose-coroutine-lowering-strategy-prefers-stackful-when-needed
  "Strategy chooser returns stackful for call/cc or deep yield, stackless otherwise."
  (assert-eq :stackless
             (cl-cc/optimize::opt-choose-coroutine-lowering-strategy
              :supports-call/cc nil :deep-yield-p nil))
  (assert-eq :stackful
             (cl-cc/optimize::opt-choose-coroutine-lowering-strategy
              :supports-call/cc t :deep-yield-p nil))
  (assert-eq :stackful
             (cl-cc/optimize::opt-choose-coroutine-lowering-strategy
              :supports-call/cc nil :deep-yield-p t)))

;;; ─── Channel select ──────────────────────────────────────────────────────────

(deftest optimize-channel-select-path-classifies-buffered-sync-and-contended-cases
  "Channel helper classifies fast buffered, synchronous, and contended paths."
  (let ((buffered (cl-cc/optimize::make-opt-channel-site
                   :buffer-size 8 :queue-depth 2 :contention 0 :select-arity 2))
        (sync (cl-cc/optimize::make-opt-channel-site
               :buffer-size 0 :queue-depth 0 :contention 0 :select-arity 2))
        (contended (cl-cc/optimize::make-opt-channel-site
                    :buffer-size 8 :queue-depth 2 :contention 9 :select-arity 2)))
    (assert-eq :fast-buffered (cl-cc/optimize::opt-channel-select-path buffered))
    (assert-eq :synchronous-rendezvous (cl-cc/optimize::opt-channel-select-path sync))
    (assert-eq :contended-fallback (cl-cc/optimize::opt-channel-select-path contended))))

(deftest optimize-channel-jump-table-select-threshold
  "Jump-table select is enabled when select arity meets threshold."
  (let ((small (cl-cc/optimize::make-opt-channel-site
                :buffer-size 1 :queue-depth 0 :contention 0 :select-arity 2))
        (large (cl-cc/optimize::make-opt-channel-site
                :buffer-size 1 :queue-depth 0 :contention 0 :select-arity 6)))
    (assert-false (cl-cc/optimize::opt-channel-should-jump-table-select-p small :threshold 4))
    (assert-true (cl-cc/optimize::opt-channel-should-jump-table-select-p large :threshold 4))))

;;; ─── STM plan ────────────────────────────────────────────────────────────────

(deftest optimize-stm-plan-skips-log-for-pure-block
  "STM helper marks pure block as log-free."
  (let ((plan (cl-cc/optimize::opt-stm-build-plan
               :reads '(:x) :writes nil :pure-p t)))
    (assert-false (cl-cc/optimize::opt-stm-needs-log-p plan))
    (assert-false (cl-cc/optimize::opt-stm-plan-inline-log-p plan))))

(deftest optimize-stm-plan-enables-log-for-impure-read-write
  "STM helper enables logging for impure transactional access."
  (let ((plan (cl-cc/optimize::opt-stm-build-plan
               :reads '(:x) :writes '(:y) :pure-p nil)))
    (assert-true (cl-cc/optimize::opt-stm-needs-log-p plan))
    (assert-true (cl-cc/optimize::opt-stm-plan-inline-log-p plan))))

;;; ─── Lock-free reclamation ───────────────────────────────────────────────────

(deftest-each lockfree-reclamation-cases
  "Lock-free helper selects reclamation strategy based on ABA risk/contention."
  :cases (("no-aba"    nil 10 :none)
          ("aba-low"   t   1  :hazard-pointer)
          ("aba-high"  t   6  :epoch))
  (aba-p contention expected)
  (assert-eq expected
             (cl-cc/optimize::opt-lockfree-select-reclamation
              :aba-risk-p aba-p :contention contention)))
