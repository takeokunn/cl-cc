(in-package :cl-cc/optimize)

(defun opt-adaptive-compilation-threshold (&key (base 1000) warmup-p cache-pressure failures)
  "Return an adaptive tiering/recompilation threshold.
Warmup lowers the threshold, cache pressure raises it, and failures suppress
speculative recompilation."
  (let ((threshold base))
    (when warmup-p
      (setf threshold (max 1 (floor threshold 3))))
    (when (and cache-pressure (> cache-pressure 0.6))
      (setf threshold (* threshold 2)))
    (when (and failures (plusp failures))
      (setf threshold (* threshold (1+ failures))))
    threshold))

(defun opt-tier-transition (current-tier hotness
                            &key (baseline-threshold 100)
                                 (optimized-threshold 1000))
  "Return the next tier for CURRENT-TIER and HOTNESS, or CURRENT-TIER."
  (ecase current-tier
    (:interpreter
     (if (>= hotness baseline-threshold) :baseline :interpreter))
    (:baseline
     (if (>= hotness optimized-threshold) :optimized :baseline))
    (:optimized :optimized)))

(defun opt-build-async-state-machine (await-labels)
  "Build a linear async state machine skeleton from AWAIT-LABELS.

Each await label creates one state and a transition to the next state.
Returns an OPT-ASYNC-STATE-MACHINE descriptor for planning/testing."
  (let* ((states (loop for i from 0 below (1+ (length await-labels)) collect i))
         (transitions
           (loop for i from 0 below (length await-labels)
                 for label in await-labels
                 collect (list :from i :await label :to (1+ i)))))
    (make-opt-async-state-machine
     :entry-state 0
     :states states
     :await-points (copy-list await-labels)
     :transitions transitions)))

(defun opt-choose-coroutine-lowering-strategy (&key supports-call/cc deep-yield-p)
  "Choose coroutine lowering strategy.

Returns :stackful when deep yield or call/cc compatibility is required,
otherwise returns :stackless for state-machine lowering."
  (if (or supports-call/cc deep-yield-p)
      :stackful
      :stackless))

(defun opt-channel-select-path (site)
  "Select channel fast/sync path based on SITE characteristics.

Returns one of:
  :fast-buffered      buffered channel with available queue slots/items
  :synchronous-rendezvous  unbuffered channel hand-off path
  :contended-fallback heavy contention -> conservative fallback"
  (cond
    ((> (opt-chan-site-contention site) 3) :contended-fallback)
    ((and (> (opt-chan-site-buffer-size site) 0)
          (< (opt-chan-site-queue-depth site) (opt-chan-site-buffer-size site)))
     :fast-buffered)
    (t :synchronous-rendezvous)))

(defun opt-channel-should-jump-table-select-p (site &key (threshold 4))
  "Return T when select-arity is large enough to prefer jump-table lowering."
  (>= (opt-chan-site-select-arity site) threshold))

(defun opt-stm-build-plan (&key reads writes pure-p)
  "Build a conservative STM plan from read/write sets and purity.

When PURE-P is true, transaction logging can be skipped."
  (make-opt-stm-plan
   :reads (copy-list reads)
   :writes (copy-list writes)
   :pure-p pure-p
   :inline-log-p (not pure-p)))

(defun opt-stm-needs-log-p (plan)
  "Return T when PLAN requires transactional logging."
  (and (not (opt-stm-plan-pure-p plan))
       (or (opt-stm-plan-reads plan)
           (opt-stm-plan-writes plan))))

(defun opt-lockfree-select-reclamation (&key aba-risk-p contention)
  "Choose memory reclamation strategy for lock-free lowering.

Rules:
  - no ABA risk -> :none
  - high contention (>= 4) -> :epoch
  - otherwise -> :hazard-pointer"
  (cond
    ((not aba-risk-p) :none)
    ((and contention (>= contention 4)) :epoch)
    (t :hazard-pointer)))

(defun opt-lockfree-build-plan (&key operation aba-risk-p contention)
  "Build a lock-free support plan with reclamation strategy."
  (make-opt-lockfree-plan
   :operation (or operation :cas)
   :aba-risk-p aba-risk-p
   :reclamation (opt-lockfree-select-reclamation
                 :aba-risk-p aba-risk-p
                 :contention contention)))
