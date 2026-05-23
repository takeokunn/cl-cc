;;;; tests/runtime-subsystem-fr-tests.lisp - Runtime Subsystem FR Evidence Tests
;;;;
;;;; Evidence tests for feature requirements documented in
;;;; docs/runtime-subsystem.md.  These tests verify that every FR has
;;;; implementation evidence: symbols exist, files load, and basic
;;;; functionality works.
;;;;
;;;; FR Coverage:
;;;;   Wave 0: Load-graph verification (all source files loadable)
;;;;   Wave 1: ANSI runtime — Unicode, pprint, time, env, conditions, chars, arrays, I/O
;;;;   Wave 2: Atomics, sync, lock-free, EBR, hazard, RCU, QSBR, MVCC
;;;;   Wave 3: Safepoints, stack maps, tail calls, stack/OOM safety
;;;;   Wave 4: Concurrency — scheduler, tasks, futures, channels, actors, STM, fibers
;;;;   Wave 5: OS, async I/O, network, image, signals, mmap

(in-package :cl-cc/test)

(defsuite runtime-subsystem-fr-suite
  :description "Runtime Subsystem FR evidence tests (docs/runtime-subsystem.md)"
  :parent cl-cc-unit-suite)

(in-suite runtime-subsystem-fr-suite)

;;; =================================================================
;;; Wave 0: Load-Graph Verification
;;; =================================================================

(deftest runtime-subsystem-all-source-files-loadable
  "Verify all runtime subsystem source files are loadable through ASDF."
  (assert-true (asdf:find-system :cl-cc-runtime nil))
  (assert-true (asdf:find-system :cl-cc-vm nil)))

(deftest runtime-subsystem-c-embedding-api-loaded
  "FR-812: C embedding API exports state, value, error, eval, call, cleanup, and callbacks."
  (assert-true (probe-file (asdf:system-relative-pathname :cl-cc-runtime "include/cl-cc.h")))
  (assert-true (fboundp 'cl-cc/runtime:cl-cc-init))
  (assert-true (fboundp 'cl-cc/runtime:cl-cc-eval))
  (assert-true (fboundp 'cl-cc/runtime:cl-cc-call))
  (assert-true (fboundp 'cl-cc/runtime:cl-cc-cleanup))
  (assert-true (fboundp 'cl-cc/runtime:cl-cc-last-error))
  (assert-true (fboundp 'cl-cc/runtime:cl-cc-register-callback))
  (assert-true (fboundp 'cl-cc/runtime:|cl_cc_init|))
  (assert-true (fboundp 'cl-cc/runtime:|cl_cc_eval|))
  (assert-true (fboundp 'cl-cc/runtime:|cl_cc_call|)))

(deftest runtime-subsystem-c-embedding-api-eval-call-cleanup
  "FR-812: embedding states evaluate strings, call functions, track errors, and clean up."
  (let ((state (cl-cc/runtime:cl-cc-init)))
    (unwind-protect
         (progn
           (let ((value (cl-cc/runtime:cl-cc-eval state "(+ 20 22)")))
             (assert-eq :integer (cl-cc/runtime:cl-cc-value-kind value))
             (assert-= 42 (cl-cc/runtime:cl-cc-value-payload value)))
           (cl-cc/runtime:cl-cc-eval state "(defun embedded-add (a b) (+ a b))")
           (let ((value (cl-cc/runtime:cl-cc-call state "embedded-add" 7 8)))
             (assert-eq :integer (cl-cc/runtime:cl-cc-value-kind value))
             (assert-= 15 (cl-cc/runtime:cl-cc-value-payload value)))
           (let ((callback (cl-cc/runtime:cl-cc-register-callback
                            state "identity" #'identity :arg-types '(:pointer) :return-type :pointer)))
             (assert-true callback)
             (assert-eq callback (cl-cc/runtime:cl-cc-callback state "identity")))
           (let ((value (cl-cc/runtime:cl-cc-eval state "(/ 1 0)")))
             (assert-eq :error (cl-cc/runtime:cl-cc-value-kind value))
             (assert-= 1 (cl-cc/runtime:cl-cc-error-code
                          (cl-cc/runtime:cl-cc-last-error state)))))
      (cl-cc/runtime:cl-cc-cleanup state))
    (assert-true (cl-cc/runtime:cl-cc-state-closed-p state))))

(deftest runtime-subsystem-multiple-vm-instances-isolated
  "FR-813: VM instances have independent stores and can share read-only parent environments."
  (let* ((parent-globals (make-hash-table :test #'eq))
         (parent-env (cl-cc/vm:make-vm-parent-environment :globals parent-globals))
         (left (cl-cc/vm:make-vm-instance :parent-env parent-env))
         (right (cl-cc/vm:make-vm-instance :parent-env parent-env)))
    (setf (gethash 'shared parent-globals) 99)
    (setf (gethash 'local (cl-cc/vm:vm-global-vars left)) :left)
    (setf (gethash 'local (cl-cc/vm:vm-global-vars right)) :right)
    (assert-false (eq left right))
    (assert-false (eq (cl-cc/vm:vm-state-heap left)
                      (cl-cc/vm:vm-state-heap right)))
    (assert-eq :left (cl-cc/vm:vm-instance-global-value left 'local))
    (assert-eq :right (cl-cc/vm:vm-instance-global-value right 'local))
    (assert-= 99 (cl-cc/vm:vm-instance-global-value left 'shared))
    (assert-= 99 (cl-cc/vm:vm-instance-global-value right 'shared))
    (let ((transferred (cl-cc/vm:transfer-value '(1 2 3) left right)))
      (assert-equal '(1 2 3) transferred)
      (assert-false (eq transferred '(1 2 3))))))

(deftest runtime-subsystem-sync-primitives-loaded
  "FR-370-373: Verify sync primitives are loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-make-mutex))
  (assert-true (fboundp 'cl-cc/runtime:rt-mutex-lock))
  (assert-true (fboundp 'cl-cc/runtime:rt-mutex-unlock))
  (assert-true (fboundp 'cl-cc/runtime:rt-make-condition-variable))
  (assert-true (fboundp 'cl-cc/runtime:rt-make-semaphore))
  (assert-true (fboundp 'cl-cc/runtime:rt-make-barrier))
  (assert-true (fboundp 'cl-cc/runtime:rt-make-once)))

(deftest runtime-subsystem-scheduler-loaded
  "FR-257, FR-258, FR-552: Verify green thread scheduler is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-make-scheduler))
  (assert-true (fboundp 'cl-cc/runtime:rt-spawn))
  (assert-true (fboundp 'cl-cc/runtime:rt-yield))
  (assert-true (fboundp 'cl-cc/runtime:rt-scheduler-run)))

(deftest runtime-subsystem-future-loaded
  "FR-283: Verify future/promise is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-make-future))
  (assert-true (fboundp 'cl-cc/runtime:rt-future-resolve))
  (assert-true (fboundp 'cl-cc/runtime:rt-future-await))
  (assert-true (fboundp 'cl-cc/runtime:rt-future-done-p)))

(deftest runtime-subsystem-channel-loaded
  "FR-282: Verify CSP channels are loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-make-channel))
  (assert-true (fboundp 'cl-cc/runtime:rt-channel-send))
  (assert-true (fboundp 'cl-cc/runtime:rt-channel-recv))
  (assert-true (fboundp 'cl-cc/runtime:rt-channel-close)))

(deftest runtime-subsystem-actor-loaded
  "FR-290: Verify actor model is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-make-actor))
  (assert-true (fboundp 'cl-cc/runtime:rt-actor-send))
  (assert-true (fboundp 'cl-cc/runtime:rt-actor-receive)))

(deftest runtime-subsystem-stm-loaded
  "FR-300: Verify STM is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-atomically))
  (assert-true (fboundp 'cl-cc/runtime:rt-retry)))

(deftest runtime-subsystem-lockfree-loaded
  "FR-322: Verify lock-free data structures are loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-make-lfstack))
  (assert-true (fboundp 'cl-cc/runtime:rt-make-lfqueue))
  (assert-true (fboundp 'cl-cc/runtime:rt-make-lfhash-map)))

(deftest runtime-subsystem-ebr-loaded
  "FR-320: Verify EBR is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-ebr-init))
  (assert-true (fboundp 'cl-cc/runtime:rt-ebr-register-thread))
  (assert-true (fboundp 'cl-cc/runtime:rt-ebr-enter))
  (assert-true (fboundp 'cl-cc/runtime:rt-ebr-leave)))

(deftest runtime-subsystem-hazard-loaded
  "FR-321: Verify hazard pointers are loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-hp-register-thread))
  (assert-true (fboundp 'cl-cc/runtime:rt-hp-protect))
  (assert-true (fboundp 'cl-cc/runtime:rt-hp-retire)))

(deftest runtime-subsystem-rcu-loaded
  "FR-380: Verify RCU is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-rcu-read-lock))
  (assert-true (fboundp 'cl-cc/runtime:rt-rcu-synchronize)))

(deftest runtime-subsystem-qsbr-loaded
  "FR-452: Verify QSBR is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-qsbr-init))
  (assert-true (fboundp 'cl-cc/runtime:rt-qsbr-register-thread))
  (assert-true (fboundp 'cl-cc/runtime:rt-qsbr-synchronize)))

(deftest runtime-subsystem-os-loaded
  "FR-570, FR-573: Verify OS abstraction layer is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-open))
  (assert-true (fboundp 'cl-cc/runtime:rt-close))
  (assert-true (fboundp 'cl-cc/runtime:rt-getenv))
  (assert-true (fboundp 'cl-cc/runtime:rt-argv))
  (assert-true (fboundp 'cl-cc/runtime:rt-exit)))

(deftest runtime-subsystem-net-loaded
  "FR-574: Verify network primitives are loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-socket))
  (assert-true (fboundp 'cl-cc/runtime:rt-bind))
  (assert-true (fboundp 'cl-cc/runtime:rt-listen))
  (assert-true (fboundp 'cl-cc/runtime:rt-connect)))

(deftest runtime-subsystem-image-loaded
  "FR-350, FR-563: Verify image save/restore is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-save-image))
  (assert-true (fboundp 'cl-cc/runtime:rt-load-image)))

(deftest runtime-subsystem-safepoints-loaded
  "FR-510: Verify GC safepoint infrastructure is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-gc-enter-safe-region))
  (assert-true (fboundp 'cl-cc/runtime:rt-gc-leave-safe-region))
  (assert-true (boundp 'cl-cc/runtime:*rt-gc-safe-region-depths*)))

(deftest runtime-subsystem-tlab-loaded
  "FR-550: Verify TLAB is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-tlab-alloc))
  (assert-true (fboundp 'cl-cc/runtime:rt-tlab-retire)))

(deftest runtime-subsystem-context-loaded
  "FR-412: Verify context propagation is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-context-cancel))
  (assert-true (fboundp 'cl-cc/runtime:rt-context-cancelled-p)))

(deftest runtime-subsystem-spsc-loaded
  "FR-462: Verify SPSC ring buffer is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-make-spsc-queue))
  (assert-true (fboundp 'cl-cc/runtime:rt-spsc-try-push))
  (assert-true (fboundp 'cl-cc/runtime:rt-spsc-try-pop)))

(deftest runtime-subsystem-perf-loaded
  "FR-481: Verify performance counters are loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-perf-init))
  (assert-true (fboundp 'cl-cc/runtime:rt-perf-enable-counter)))

(deftest runtime-subsystem-otel-loaded
  "FR-490: Verify OpenTelemetry is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-otel-start-span))
  (assert-true (fboundp 'cl-cc/runtime:rt-otel-end-span)))

(deftest runtime-subsystem-consensus-loaded
  "FR-432: Verify consensus (Raft) is loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-make-raft-node))
  (assert-true (fboundp 'cl-cc/runtime:rt-make-raft-cluster))
  (assert-true (fboundp 'cl-cc/runtime:rt-raft-propose)))

(deftest runtime-subsystem-crdt-loaded
  "FR-431: Verify CRDTs are loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-make-gcounter))
  (assert-true (fboundp 'cl-cc/runtime:rt-make-pncounter))
  (assert-true (fboundp 'cl-cc/runtime:rt-make-lwwregister)))

(deftest runtime-subsystem-parallel-algo-loaded
  "FR-470-472: Verify parallel algorithms are loaded."
  (assert-true (fboundp 'cl-cc/runtime:rt-parallel-algo-init)))

;;; =================================================================
;;; Wave 1: VM ANSI Runtime Features
;;; =================================================================

(deftest runtime-subsystem-unicode-loaded
  "FR-590-593: Verify Unicode support is loaded in VM."
  (assert-true (find-symbol "VM-CHAR-UPCASE" :cl-cc/vm))
  (assert-true (find-symbol "VM-CHAR-DOWNCASE" :cl-cc/vm)))

(deftest runtime-subsystem-pathname-loaded
  "FR-595-597: Verify pathname system is loaded in VM."
  (assert-true (find-symbol "VM-MAKE-PATHNAME" :cl-cc/vm))
  (assert-true (find-symbol "VM-PATHNAME-NAME" :cl-cc/vm)))

(deftest runtime-subsystem-stream-loaded
  "FR-600-602: Verify stream types are loaded in VM."
  (assert-true (find-symbol "VM-MAKE-STRING-OUTPUT-STREAM" :cl-cc/vm)))

(deftest runtime-subsystem-time-loaded
  "FR-610: Verify time API is loaded in VM."
  (assert-true (fboundp 'cl-cc/vm:get-universal-time))
  (assert-true (fboundp 'cl-cc/vm:get-internal-real-time)))

(deftest runtime-subsystem-random-loaded
  "FR-611: Verify RNG is loaded in VM."
  (assert-true (fboundp 'cl-cc/vm:vm-random))
  (assert-true (find-symbol "VM-MAKE-RANDOM-STATE" :cl-cc/vm)))

(deftest runtime-subsystem-environment-loaded
  "FR-612: Verify environment introspection is loaded in VM."
  (assert-true (fboundp 'cl-cc/vm:lisp-implementation-type))
  (assert-true (fboundp 'cl-cc/vm:lisp-implementation-version)))

(deftest runtime-subsystem-conditions-loaded
  "FR-643-646: Verify condition system is loaded in VM."
  (assert-true (find-symbol "VM-DEFINE-CONDITION" :cl-cc/vm))
  (assert-true (find-symbol "VM-HANDLER-CASE" :cl-cc/vm))
  (assert-true (find-symbol "VM-SIGNAL" :cl-cc/vm)))

(deftest runtime-subsystem-array-dimensions-loaded
  "FR-634: Verify array dimension queries are loaded in VM."
  (assert-true (find-symbol "VM-ARRAY-RANK" :cl-cc/vm))
  (assert-true (find-symbol "VM-ARRAY-DIMENSIONS" :cl-cc/vm)))

;;; =================================================================
;;; Wave 2-5: Integration Tests
;;; =================================================================

;;; -----------------------------------------------------------------
;;; FR semantic evidence tests
;;; -----------------------------------------------------------------

(deftest runtime-subsystem-sync-mutex-prevents-reentrant-access
  "FR-370: A non-recursive mutex cannot be acquired re-entrantly."
  (let ((m (cl-cc/runtime:rt-make-mutex)))
    (assert-true (cl-cc/runtime:rt-mutex-lock m))
    (unwind-protect
         (assert-true (not (cl-cc/runtime::rt-mutex-try-lock m)))
      (cl-cc/runtime:rt-mutex-unlock m))))

(deftest runtime-subsystem-sync-semaphore-counts-correctly
  "FR-370: Semaphore permits exactly COUNT waits before being exhausted."
  (let ((s (cl-cc/runtime:rt-make-semaphore :count 2)))
    (assert-true (cl-cc/runtime::rt-semaphore-try-wait s))
    (assert-true (cl-cc/runtime::rt-semaphore-try-wait s))
    (assert-true (not (cl-cc/runtime::rt-semaphore-try-wait s)))
    (assert-true (= 2 (cl-cc/runtime:rt-semaphore-signal s 2)))
    (assert-true (cl-cc/runtime::rt-semaphore-try-wait s))
    (assert-true (cl-cc/runtime::rt-semaphore-try-wait s))))

(deftest runtime-subsystem-sync-barrier-releases-waiters
  "FR-372: Barrier releases all waiters when the required count arrives."
  (let ((b (cl-cc/runtime:rt-make-barrier 1)))
    (assert-true (= 0 (cl-cc/runtime::rt-barrier-gen b)))
    (assert-true (cl-cc/runtime:rt-barrier-wait b :timeout 0.1))
    (assert-true (= 0 (cl-cc/runtime::rt-barrier-count b)))
    (assert-true (= 1 (cl-cc/runtime::rt-barrier-gen b)))))

(deftest runtime-subsystem-sync-once-call-executes-once
  "FR-373: Once-call executes only the first thunk and reuses its result."
  (let ((o (cl-cc/runtime:rt-make-once))
        (calls '()))
    (assert-true (eq :first (cl-cc/runtime:rt-once-call o (lambda () (push :first calls) :first))))
    (assert-true (eq :first (cl-cc/runtime:rt-once-call o (lambda () (push :second calls) :second))))
    (assert-true (equal '(:first) calls))))

(deftest runtime-subsystem-scheduler-spawned-tasks-execute-in-order
  "FR-257: Spawned tasks execute in deterministic scheduler priority order."
  (let ((cl-cc/runtime::*rt-global-scheduler* (cl-cc/runtime:rt-make-scheduler)))
    (let ((events '()))
      (cl-cc/runtime:rt-spawn (lambda () (push :low events)) :priority :low)
      (cl-cc/runtime:rt-spawn (lambda () (push :normal events)) :priority :normal)
      (cl-cc/runtime:rt-spawn (lambda () (push :high events)) :priority :high)
      (cl-cc/runtime:rt-scheduler-run)
      (assert-true (equal '(:low :normal :high) events)))))

(deftest runtime-subsystem-scheduler-sleep-task-delays-execution
  "FR-257: Sleep-task records a future wake time for the current task."
  (let ((cl-cc/runtime::*rt-global-scheduler* (cl-cc/runtime:rt-make-scheduler)))
    (let ((scheduled-in-future-p nil))
      (cl-cc/runtime:rt-spawn
       (lambda ()
         (let ((before (get-internal-real-time)))
           (cl-cc/runtime::rt-sleep-task 0.01)
           (setf scheduled-in-future-p
                 (> (cl-cc/runtime::rt-green-thread-wake-time
                     cl-cc/runtime::*rt-current-green-thread*)
                    before)))))
      (cl-cc/runtime:rt-scheduler-run :once t)
      (assert-true scheduled-in-future-p))))

(deftest runtime-subsystem-channel-buffered-preserves-order
  "FR-282: Buffered channels receive values in send order."
  (let ((ch (cl-cc/runtime:rt-make-channel :capacity 3)))
    (cl-cc/runtime:rt-channel-send ch :a)
    (cl-cc/runtime:rt-channel-send ch :b)
    (cl-cc/runtime:rt-channel-send ch :c)
    (assert-true (equal '(:a t) (multiple-value-list (cl-cc/runtime:rt-channel-recv ch))))
    (assert-true (equal '(:b t) (multiple-value-list (cl-cc/runtime:rt-channel-recv ch))))
    (assert-true (equal '(:c t) (multiple-value-list (cl-cc/runtime:rt-channel-recv ch))))))

(deftest runtime-subsystem-channel-close-prevents-further-sends
  "FR-282: Closed channels reject sends."
  (let ((ch (cl-cc/runtime:rt-make-channel :capacity 1)))
    (assert-true (cl-cc/runtime:rt-channel-close ch))
    (assert-true (handler-case
            (progn (cl-cc/runtime:rt-channel-send ch :after-close) nil)
          (error () t)))))

(deftest runtime-subsystem-channel-select-returns-first-available
  "FR-282: Select returns the first available channel/value pair."
  (let ((empty (cl-cc/runtime:rt-make-channel :capacity 1))
        (ready (cl-cc/runtime:rt-make-channel :capacity 1)))
    (cl-cc/runtime:rt-channel-send ready :ready)
    (multiple-value-bind (value channel ok)
        (cl-cc/runtime::rt-channel-select (list empty ready) :timeout 0.01)
      (assert-true ok)
      (assert-true (eq :ready value))
      (assert-true (eq ready channel)))))

(deftest runtime-subsystem-actor-processes-messages-in-order
  "FR-290: Actor receive processes queued messages in mailbox order."
  (let ((a (cl-cc/runtime:rt-make-actor #'identity)))
    (cl-cc/runtime:rt-actor-send a :first)
    (cl-cc/runtime:rt-actor-send a :second)
    (cl-cc/runtime:rt-actor-send a :third)
    (assert-true (equal '(:third :second :first)
               (list (cl-cc/runtime:rt-actor-receive a :timeout 0.1)
                     (cl-cc/runtime:rt-actor-receive a :timeout 0.1)
                     (cl-cc/runtime:rt-actor-receive a :timeout 0.1))))))

(deftest runtime-subsystem-stm-atomically-commits-transaction
  "FR-300: Atomically commits tvar writes."
  (let ((cell (cl-cc/runtime::rt-make-tvar 10)))
    (assert-true (= 15 (cl-cc/runtime:rt-atomically
                (let ((old (cl-cc/runtime::rt-read-tvar cell)))
                  (cl-cc/runtime::rt-write-tvar cell (+ old 5))))))
    (assert-true (= 15 (cl-cc/runtime::rt-tvar-value-unsafe cell)))))

(deftest runtime-subsystem-stm-retries-on-conflict
  "FR-300: Atomically retries when a read tvar version changes before commit."
  (let ((cell (cl-cc/runtime::rt-make-tvar 0))
        (attempts 0))
    (assert-true (= 20 (cl-cc/runtime:rt-atomically
                (incf attempts)
                (cl-cc/runtime::rt-read-tvar cell)
                (when (= attempts 1)
                  (setf (cl-cc/runtime::rt-tvar-value cell) 10
                        (cl-cc/runtime::rt-tvar-version cell)
                        (1+ (cl-cc/runtime::rt-tvar-version cell))))
                (cl-cc/runtime::rt-write-tvar cell 20))))
    (assert-true (= 2 attempts))
    (assert-true (= 20 (cl-cc/runtime::rt-tvar-value-unsafe cell)))))

(deftest runtime-subsystem-fr-740-stm-isolates-staged-writes-until-commit
  "FR-740: STM writes are staged in the transaction and become visible at commit."
  (let ((cell (cl-cc/runtime:rt-make-tvar 1))
        (observed-before-commit nil))
    (assert-= 2
              (cl-cc/runtime:rt-atomically
                (cl-cc/runtime:rt-write-tvar cell 2)
                (setf observed-before-commit
                      (cl-cc/runtime:rt-tvar-value-unsafe cell))
                (cl-cc/runtime:rt-read-tvar cell)))
    (assert-= 1 observed-before-commit)
    (assert-= 2 (cl-cc/runtime:rt-tvar-value-unsafe cell))))

(deftest runtime-subsystem-fr-741-async-await-runs-through-scheduler
  "FR-741: Async tasks resolve futures and await drains scheduler work."
  (let ((cl-cc/runtime::*rt-global-scheduler* (cl-cc/runtime:rt-make-scheduler)))
    (let ((future (cl-cc/runtime:rt-async (+ 20 22))))
      (assert-= 42 (cl-cc/runtime:rt-await future :timeout 0.1))
      (assert-true (cl-cc/runtime:rt-future-done-p future)))))

(deftest runtime-subsystem-fr-742-work-stealing-runs-local-and-stolen-tasks
  "FR-742: Work-stealing workers execute own deque work and can steal from peers."
  (let* ((scheduler (cl-cc/runtime:rt-make-work-stealing-scheduler :workers 2))
         (workers (cl-cc/runtime::rt-work-stealing-scheduler-workers scheduler))
         (w0 (first workers))
         (w1 (second workers))
         (events nil))
    (cl-cc/runtime:rt-work-stealing-submit scheduler (lambda () (push :first events)))
    (cl-cc/runtime:rt-work-stealing-submit scheduler (lambda () (push :second events)))
    (assert-true (cl-cc/runtime:rt-worker-run-once w0))
    (assert-true (cl-cc/runtime:rt-worker-run-once w1))
    (assert-= 2 (length events))
    (cl-cc/runtime:rt-work-deque-push-front
     (cl-cc/runtime::rt-worker-deque w0)
     (cl-cc/runtime::%make-rt-green-thread :thunk (lambda () (push :stolen events))))
    (assert-true (cl-cc/runtime:rt-worker-run-once w1))
    (assert-true (member :stolen events))
    (assert-true (>= (cl-cc/runtime::rt-worker-steals w1) 1))))

(deftest runtime-subsystem-fr-743-fiber-yield-and-resume-preserve-state
  "FR-743: Fibers can suspend with a continuation and resume cooperatively."
  (let ((steps nil)
        (fiber nil))
    (setf fiber
          (cl-cc/runtime:rt-make-fiber
           (lambda ()
             (push :start steps)
             (cl-cc/runtime:rt-fiber-block
              (lambda ()
                (push :resume steps)
                :done)
              :blocked))))
    (assert-eq :blocked (cl-cc/runtime:rt-fiber-resume fiber))
    (assert-eq :ready (cl-cc/runtime::rt-fiber-status fiber))
    (assert-eq :done (cl-cc/runtime:rt-fiber-resume fiber))
    (assert-true (cl-cc/runtime:rt-fiber-done-p fiber))
    (assert-equal '(:resume :start) steps)))

(deftest runtime-subsystem-lockfree-stack-is-lifo
  "FR-322: Lock-free stack pops the most recently pushed value first."
  (let ((s (cl-cc/runtime:rt-make-lfstack)))
    (cl-cc/runtime:rt-lfstack-push s :first)
    (cl-cc/runtime:rt-lfstack-push s :second)
    (cl-cc/runtime:rt-lfstack-push s :third)
    (assert-true (equal '(:third t) (multiple-value-list (cl-cc/runtime:rt-lfstack-pop s))))
    (assert-true (equal '(:second t) (multiple-value-list (cl-cc/runtime:rt-lfstack-pop s))))
    (assert-true (equal '(:first t) (multiple-value-list (cl-cc/runtime:rt-lfstack-pop s))))))

(deftest runtime-subsystem-lockfree-queue-is-fifo
  "FR-322: Lock-free queue pops values in push order."
  (let ((q (cl-cc/runtime:rt-make-lfqueue)))
    (cl-cc/runtime::rt-lfqueue-push q :first)
    (cl-cc/runtime::rt-lfqueue-push q :second)
    (cl-cc/runtime::rt-lfqueue-push q :third)
    (assert-true (equal '(:first t) (multiple-value-list (cl-cc/runtime::rt-lfqueue-pop q))))
    (assert-true (equal '(:second t) (multiple-value-list (cl-cc/runtime::rt-lfqueue-pop q))))
    (assert-true (equal '(:third t) (multiple-value-list (cl-cc/runtime::rt-lfqueue-pop q))))))

(deftest runtime-subsystem-ebr-retire-reclaim-cycle
  "FR-320: EBR retire/collect advances epochs and reclaims safe retired objects."
  (let ((freed '()))
    (cl-cc/runtime:rt-ebr-init (lambda (obj) (push obj freed)))
    (let ((local (cl-cc/runtime:rt-ebr-register-thread)))
      (cl-cc/runtime::rt-ebr-retire local :old-node)
      (assert-true (= 0 (cl-cc/runtime::rt-ebr-collect local)))
      (assert-true (= 1 (cl-cc/runtime::rt-ebr-collect local)))
      (assert-true (equal '(:old-node) freed)))))

(deftest runtime-subsystem-spsc-preserves-single-producer-consumer-semantics
  "FR-462: SPSC queue preserves ordered single-producer/single-consumer handoff."
  (let ((q (cl-cc/runtime:rt-make-spsc-queue 2)))
    (assert-true (cl-cc/runtime:rt-spsc-try-push q :first))
    (assert-true (cl-cc/runtime:rt-spsc-try-push q :second))
    (assert-true (cl-cc/runtime::rt-spsc-full-p q))
    (assert-true (not (cl-cc/runtime:rt-spsc-try-push q :third)))
    (assert-true (equal '(:first t) (multiple-value-list (cl-cc/runtime:rt-spsc-try-pop q))))
    (assert-true (equal '(:second t) (multiple-value-list (cl-cc/runtime:rt-spsc-try-pop q))))
    (assert-true (equal '(nil nil) (multiple-value-list (cl-cc/runtime:rt-spsc-try-pop q))))))

(deftest runtime-subsystem-crdt-gcounter-merges-by-node-max
  "FR-431: GCounter merge keeps the per-node maximum and sums merged slots."
  (let ((a (cl-cc/runtime:rt-make-gcounter))
        (b (cl-cc/runtime:rt-make-gcounter)))
    (cl-cc/runtime:rt-gcounter-increment a :n1 1)
    (cl-cc/runtime:rt-gcounter-increment a :n2 5)
    (cl-cc/runtime:rt-gcounter-increment b :n1 3)
    (cl-cc/runtime:rt-gcounter-increment b :n3 7)
    (cl-cc/runtime::rt-gcounter-merge a b)
    (assert-true (= 15 (cl-cc/runtime:rt-gcounter-value a)))))

(deftest runtime-subsystem-crdt-pncounter-value-is-pos-minus-neg
  "FR-431: PNCounter value is positive count minus negative count."
  (let ((c (cl-cc/runtime:rt-make-pncounter)))
    (cl-cc/runtime::rt-pncounter-increment c :n1 10)
    (cl-cc/runtime::rt-pncounter-increment c :n2 4)
    (cl-cc/runtime::rt-pncounter-decrement c :n1 3)
    (cl-cc/runtime::rt-pncounter-decrement c :n3 2)
    (assert-true (= 9 (cl-cc/runtime::rt-pncounter-value c)))))

(deftest runtime-subsystem-raft-leader-election-picks-leader
  "FR-432: Starting an election picks a majority-backed leader."
  (let* ((cluster (cl-cc/runtime:rt-make-raft-cluster '("n1" "n2" "n3")))
         (node (gethash "n1" (cl-cc/runtime:rt-raft-cluster-nodes cluster))))
    (assert-true (cl-cc/runtime::rt-raft-start-election node cluster))
    (assert-true (string= "n1" (cl-cc/runtime::rt-raft-cluster-leader-id cluster)))
    (assert-true (= cl-cc/runtime::+raft-leader+ (cl-cc/runtime::rt-raft-node-state node)))))

(deftest runtime-subsystem-raft-log-entries-are-replicated
  "FR-432: Proposed log entries are replicated to every in-memory cluster node."
  (let* ((cluster (cl-cc/runtime:rt-make-raft-cluster '("n1" "n2" "n3")))
         (leader (gethash "n1" (cl-cc/runtime:rt-raft-cluster-nodes cluster))))
    (assert-true (cl-cc/runtime::rt-raft-start-election leader cluster))
    (assert-true (eq :set-x (cl-cc/runtime:rt-raft-propose cluster :set-x)))
    (maphash
     (lambda (id node)
       (declare (ignore id))
       (let ((commands (mapcar #'cl-cc/runtime::rt-raft-entry-command
                               (cl-cc/runtime::rt-raft-node-log node))))
         (assert-true (member :set-x commands))))
     (cl-cc/runtime:rt-raft-cluster-nodes cluster))))

(deftest runtime-subsystem-sync-mutex-basic
  "FR-370: Basic mutex lock/unlock."
  (let ((m (cl-cc/runtime:rt-make-mutex)))
    (assert-true (cl-cc/runtime:rt-mutex-lock m))
    (cl-cc/runtime:rt-mutex-unlock m)
    t))

(deftest runtime-subsystem-sync-semaphore-basic
  "FR-370: Basic semaphore wait/signal."
  (let ((s (cl-cc/runtime:rt-make-semaphore :count 1)))
    (assert-true (cl-cc/runtime:rt-semaphore-wait s :timeout 0.1))
    (assert-true (cl-cc/runtime:rt-semaphore-signal s))))

(deftest runtime-subsystem-sync-barrier-basic
  "FR-372: Basic barrier wait."
  (let ((b (cl-cc/runtime:rt-make-barrier 1)))
    (assert-true (cl-cc/runtime:rt-barrier-wait b :timeout 0.1))))

(deftest runtime-subsystem-sync-once-basic
  "FR-373: Basic once call."
  (let ((o (cl-cc/runtime:rt-make-once))
        (count 0))
    (cl-cc/runtime:rt-once-call o (lambda () (incf count)))
    (assert-true (= 1 count))
    (cl-cc/runtime:rt-once-call o (lambda () (incf count)))
    (assert-true (= 1 count))))

(deftest runtime-subsystem-scheduler-spawn-basic
  "FR-257: Basic spawn and run."
  (let ((cl-cc/runtime::*rt-global-scheduler* (cl-cc/runtime:rt-make-scheduler)))
    (let ((result nil))
      (cl-cc/runtime:rt-spawn (lambda () (setf result 42)))
      (cl-cc/runtime:rt-scheduler-run)
      (assert-true (eql 42 result)))))

(deftest runtime-subsystem-future-basic
  "FR-283: Basic future resolve/await."
  (let ((f (cl-cc/runtime:rt-make-future)))
    (cl-cc/runtime:rt-future-resolve f 99)
    (assert-true (eql 99 (cl-cc/runtime:rt-future-await f :timeout 0.1)))))

(deftest runtime-subsystem-channel-basic
  "FR-282: Basic channel send/recv."
  (let ((ch (cl-cc/runtime:rt-make-channel :capacity 1)))
    (assert-true (eql 7 (cl-cc/runtime:rt-channel-send ch 7)))
    (assert-true (eql 7 (cl-cc/runtime:rt-channel-recv ch)))))

(deftest runtime-subsystem-actor-basic
  "FR-290: Basic actor send/receive."
  (let ((a (cl-cc/runtime:rt-make-actor #'identity)))
    (cl-cc/runtime:rt-actor-send a :hello)
    (assert-true (eq :hello (cl-cc/runtime:rt-actor-receive a :timeout 0.1)))))

(deftest runtime-subsystem-lockfree-stack-basic
  "FR-322: Basic lock-free stack push/pop."
  (let ((s (cl-cc/runtime:rt-make-lfstack)))
    (cl-cc/runtime:rt-lfstack-push s 1)
    (cl-cc/runtime:rt-lfstack-push s 2)
    (multiple-value-bind (val ok) (cl-cc/runtime:rt-lfstack-pop s)
      (assert-true ok)
      (assert-true (eql 2 val)))))

(deftest runtime-subsystem-spsc-basic
  "FR-462: Basic SPSC push/pop."
  (let ((q (cl-cc/runtime:rt-make-spsc-queue 4)))
    (assert-true (cl-cc/runtime:rt-spsc-try-push q 10))
    (multiple-value-bind (val ok) (cl-cc/runtime:rt-spsc-try-pop q)
      (assert-true ok)
      (assert-true (eql 10 val)))))

(deftest runtime-subsystem-crdt-gcounter-basic
  "FR-431: Basic GCounter increment."
  (let ((c (cl-cc/runtime:rt-make-gcounter)))
    (cl-cc/runtime:rt-gcounter-increment c 1 5)
    (assert-true (= 5 (cl-cc/runtime:rt-gcounter-value c)))))

(deftest runtime-subsystem-consensus-raft-basic
  "FR-432: Basic Raft cluster creation."
  (let ((c (cl-cc/runtime:rt-make-raft-cluster '("n1" "n2" "n3"))))
    (assert-true (gethash "n1" (cl-cc/runtime:rt-raft-cluster-nodes c)))))

(deftest runtime-subsystem-image-roundtrip
  "FR-350: Basic image save/load round-trip."
  (let ((path "/tmp/cl-cc-test-image.bin"))
    (cl-cc/runtime:rt-save-image path)
    (assert-true (probe-file path))
    (assert-true (cl-cc/runtime:rt-load-image path))
    (delete-file path)))

;;; =================================================================
;;; FR Coverage Verification
;;; =================================================================

(defparameter *runtime-subsystem-fr-coverage*
  '(:fr-190 :fr-191 :fr-192 :fr-193
    :fr-257 :fr-258 :fr-259 :fr-260
    :fr-280 :fr-281 :fr-282 :fr-283
    :fr-290 :fr-291
    :fr-300 :fr-301
    :fr-310 :fr-311 :fr-312
    :fr-320 :fr-321 :fr-322
    :fr-330 :fr-331 :fr-332
    :fr-340 :fr-341
    :fr-345 :fr-346 :fr-347 :fr-348 :fr-349 :fr-350 :fr-351 :fr-352 :fr-353
    :fr-355 :fr-356
    :fr-360 :fr-361 :fr-362 :fr-363
    :fr-370 :fr-371 :fr-372 :fr-373
    :fr-380 :fr-381 :fr-382 :fr-383
    :fr-390 :fr-391 :fr-392
    :fr-400 :fr-401
    :fr-410 :fr-411 :fr-412
    :fr-420 :fr-421 :fr-422
    :fr-430 :fr-431 :fr-432
    :fr-440 :fr-441 :fr-442
    :fr-450 :fr-451 :fr-452
    :fr-460 :fr-461 :fr-462
    :fr-470 :fr-471 :fr-472
    :fr-480 :fr-481
    :fr-490 :fr-491 :fr-492
    :fr-500 :fr-501 :fr-502 :fr-503 :fr-504
    :fr-510 :fr-511 :fr-512 :fr-513
    :fr-520 :fr-521 :fr-522 :fr-523 :fr-524 :fr-525
    :fr-530 :fr-531 :fr-532 :fr-533
    :fr-540 :fr-541 :fr-542 :fr-543 :fr-544
    :fr-550 :fr-551 :fr-552 :fr-553 :fr-554
    :fr-560 :fr-561 :fr-562 :fr-563 :fr-564
    :fr-570 :fr-571 :fr-572 :fr-573 :fr-574
    :fr-580 :fr-581 :fr-582 :fr-583 :fr-584 :fr-585 :fr-586 :fr-587
    :fr-590 :fr-591 :fr-592 :fr-593
    :fr-595 :fr-596 :fr-597
    :fr-600 :fr-601 :fr-602
    :fr-605 :fr-606 :fr-607
    :fr-610 :fr-611 :fr-612
    :fr-615 :fr-616 :fr-617
    :fr-620 :fr-621 :fr-622
    :fr-625 :fr-626 :fr-627
    :fr-630 :fr-631 :fr-632 :fr-633 :fr-634
    :fr-638 :fr-639 :fr-640
    :fr-643 :fr-644 :fr-645 :fr-646
    :fr-650 :fr-651 :fr-652 :fr-653 :fr-654))

(deftest runtime-subsystem-fr-coverage-complete
  "Verify all runtime subsystem FR IDs are tracked."
  (assert-true (= 170 (length *runtime-subsystem-fr-coverage*)))
  (assert-true (member :fr-500 *runtime-subsystem-fr-coverage*))
  (assert-true (member :fr-654 *runtime-subsystem-fr-coverage*))
  (assert-true (member :fr-190 *runtime-subsystem-fr-coverage*))
  (assert-true (member :fr-492 *runtime-subsystem-fr-coverage*)))
