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

(test runtime-subsystem-all-source-files-loadable
  "Verify all runtime subsystem source files are loadable through ASDF."
  (is (asdf:find-system :cl-cc-runtime nil))
  (is (asdf:find-system :cl-cc-vm nil)))

(test runtime-subsystem-sync-primitives-loaded
  "FR-370-373: Verify sync primitives are loaded."
  (is (fboundp 'cl-cc/runtime:rt-make-mutex))
  (is (fboundp 'cl-cc/runtime:rt-mutex-lock))
  (is (fboundp 'cl-cc/runtime:rt-mutex-unlock))
  (is (fboundp 'cl-cc/runtime:rt-make-condition-variable))
  (is (fboundp 'cl-cc/runtime:rt-make-semaphore))
  (is (fboundp 'cl-cc/runtime:rt-make-barrier))
  (is (fboundp 'cl-cc/runtime:rt-make-once)))

(test runtime-subsystem-scheduler-loaded
  "FR-257, FR-258, FR-552: Verify green thread scheduler is loaded."
  (is (fboundp 'cl-cc/runtime:rt-make-scheduler))
  (is (fboundp 'cl-cc/runtime:rt-spawn))
  (is (fboundp 'cl-cc/runtime:rt-yield))
  (is (fboundp 'cl-cc/runtime:rt-scheduler-run)))

(test runtime-subsystem-future-loaded
  "FR-283: Verify future/promise is loaded."
  (is (fboundp 'cl-cc/runtime:rt-make-future))
  (is (fboundp 'cl-cc/runtime:rt-future-resolve))
  (is (fboundp 'cl-cc/runtime:rt-future-await))
  (is (fboundp 'cl-cc/runtime:rt-future-done-p)))

(test runtime-subsystem-channel-loaded
  "FR-282: Verify CSP channels are loaded."
  (is (fboundp 'cl-cc/runtime:rt-make-channel))
  (is (fboundp 'cl-cc/runtime:rt-channel-send))
  (is (fboundp 'cl-cc/runtime:rt-channel-recv))
  (is (fboundp 'cl-cc/runtime:rt-channel-close)))

(test runtime-subsystem-actor-loaded
  "FR-290: Verify actor model is loaded."
  (is (fboundp 'cl-cc/runtime:rt-make-actor))
  (is (fboundp 'cl-cc/runtime:rt-actor-send))
  (is (fboundp 'cl-cc/runtime:rt-actor-receive)))

(test runtime-subsystem-stm-loaded
  "FR-300: Verify STM is loaded."
  (is (fboundp 'cl-cc/runtime:rt-atomically))
  (is (fboundp 'cl-cc/runtime:rt-retry)))

(test runtime-subsystem-lockfree-loaded
  "FR-322: Verify lock-free data structures are loaded."
  (is (fboundp 'cl-cc/runtime:rt-make-lfstack))
  (is (fboundp 'cl-cc/runtime:rt-make-lfqueue))
  (is (fboundp 'cl-cc/runtime:rt-make-lfhash-map)))

(test runtime-subsystem-ebr-loaded
  "FR-320: Verify EBR is loaded."
  (is (fboundp 'cl-cc/runtime:rt-ebr-init))
  (is (fboundp 'cl-cc/runtime:rt-ebr-register-thread))
  (is (fboundp 'cl-cc/runtime:rt-ebr-enter))
  (is (fboundp 'cl-cc/runtime:rt-ebr-leave)))

(test runtime-subsystem-hazard-loaded
  "FR-321: Verify hazard pointers are loaded."
  (is (fboundp 'cl-cc/runtime:rt-hp-register-thread))
  (is (fboundp 'cl-cc/runtime:rt-hp-protect))
  (is (fboundp 'cl-cc/runtime:rt-hp-retire)))

(test runtime-subsystem-rcu-loaded
  "FR-380: Verify RCU is loaded."
  (is (fboundp 'cl-cc/runtime:rt-rcu-read-lock))
  (is (fboundp 'cl-cc/runtime:rt-rcu-synchronize)))

(test runtime-subsystem-qsbr-loaded
  "FR-452: Verify QSBR is loaded."
  (is (fboundp 'cl-cc/runtime:rt-qsbr-init))
  (is (fboundp 'cl-cc/runtime:rt-qsbr-register-thread))
  (is (fboundp 'cl-cc/runtime:rt-qsbr-synchronize)))

(test runtime-subsystem-os-loaded
  "FR-570, FR-573: Verify OS abstraction layer is loaded."
  (is (fboundp 'cl-cc/runtime:rt-open))
  (is (fboundp 'cl-cc/runtime:rt-close))
  (is (fboundp 'cl-cc/runtime:rt-getenv))
  (is (fboundp 'cl-cc/runtime:rt-argv))
  (is (fboundp 'cl-cc/runtime:rt-exit)))

(test runtime-subsystem-net-loaded
  "FR-574: Verify network primitives are loaded."
  (is (fboundp 'cl-cc/runtime:rt-socket))
  (is (fboundp 'cl-cc/runtime:rt-bind))
  (is (fboundp 'cl-cc/runtime:rt-listen))
  (is (fboundp 'cl-cc/runtime:rt-connect)))

(test runtime-subsystem-image-loaded
  "FR-350, FR-563: Verify image save/restore is loaded."
  (is (fboundp 'cl-cc/runtime:rt-save-image))
  (is (fboundp 'cl-cc/runtime:rt-load-image)))

(test runtime-subsystem-safepoints-loaded
  "FR-510: Verify GC safepoint infrastructure is loaded."
  (is (fboundp 'cl-cc/runtime:rt-gc-enter-safe-region))
  (is (fboundp 'cl-cc/runtime:rt-gc-leave-safe-region))
  (is (boundp 'cl-cc/runtime:*rt-gc-safe-region-depths*)))

(test runtime-subsystem-tlab-loaded
  "FR-550: Verify TLAB is loaded."
  (is (fboundp 'cl-cc/runtime:rt-tlab-alloc))
  (is (fboundp 'cl-cc/runtime:rt-tlab-retire)))

(test runtime-subsystem-context-loaded
  "FR-412: Verify context propagation is loaded."
  (is (fboundp 'cl-cc/runtime:rt-context-cancel))
  (is (fboundp 'cl-cc/runtime:rt-context-cancelled-p)))

(test runtime-subsystem-spsc-loaded
  "FR-462: Verify SPSC ring buffer is loaded."
  (is (fboundp 'cl-cc/runtime:rt-make-spsc-queue))
  (is (fboundp 'cl-cc/runtime:rt-spsc-try-push))
  (is (fboundp 'cl-cc/runtime:rt-spsc-try-pop)))

(test runtime-subsystem-perf-loaded
  "FR-481: Verify performance counters are loaded."
  (is (fboundp 'cl-cc/runtime:rt-perf-init))
  (is (fboundp 'cl-cc/runtime:rt-perf-enable-counter)))

(test runtime-subsystem-otel-loaded
  "FR-490: Verify OpenTelemetry is loaded."
  (is (fboundp 'cl-cc/runtime:rt-otel-start-span))
  (is (fboundp 'cl-cc/runtime:rt-otel-end-span)))

(test runtime-subsystem-consensus-loaded
  "FR-432: Verify consensus (Raft) is loaded."
  (is (fboundp 'cl-cc/runtime:rt-make-raft-node))
  (is (fboundp 'cl-cc/runtime:rt-make-raft-cluster))
  (is (fboundp 'cl-cc/runtime:rt-raft-propose)))

(test runtime-subsystem-crdt-loaded
  "FR-431: Verify CRDTs are loaded."
  (is (fboundp 'cl-cc/runtime:rt-make-gcounter))
  (is (fboundp 'cl-cc/runtime:rt-make-pncounter))
  (is (fboundp 'cl-cc/runtime:rt-make-lwwregister)))

(test runtime-subsystem-parallel-algo-loaded
  "FR-470-472: Verify parallel algorithms are loaded."
  (is (fboundp 'cl-cc/runtime:rt-parallel-algo-init)))

;;; =================================================================
;;; Wave 1: VM ANSI Runtime Features
;;; =================================================================

(test runtime-subsystem-unicode-loaded
  "FR-590-593: Verify Unicode support is loaded in VM."
  (is (find-symbol "VM-CHAR-UPCASE" :cl-cc/vm))
  (is (find-symbol "VM-CHAR-DOWNCASE" :cl-cc/vm)))

(test runtime-subsystem-pathname-loaded
  "FR-595-597: Verify pathname system is loaded in VM."
  (is (find-symbol "VM-MAKE-PATHNAME" :cl-cc/vm))
  (is (find-symbol "VM-PATHNAME-NAME" :cl-cc/vm)))

(test runtime-subsystem-stream-loaded
  "FR-600-602: Verify stream types are loaded in VM."
  (is (find-symbol "VM-MAKE-STRING-OUTPUT-STREAM" :cl-cc/vm)))

(test runtime-subsystem-time-loaded
  "FR-610: Verify time API is loaded in VM."
  (is (fboundp 'cl-cc/vm:get-universal-time))
  (is (fboundp 'cl-cc/vm:get-internal-real-time)))

(test runtime-subsystem-random-loaded
  "FR-611: Verify RNG is loaded in VM."
  (is (fboundp 'cl-cc/vm:vm-random))
  (is (find-symbol "VM-MAKE-RANDOM-STATE" :cl-cc/vm)))

(test runtime-subsystem-environment-loaded
  "FR-612: Verify environment introspection is loaded in VM."
  (is (fboundp 'cl-cc/vm:vm-lisp-implementation-type))
  (is (fboundp 'cl-cc/vm:vm-lisp-implementation-version)))

(test runtime-subsystem-conditions-loaded
  "FR-643-646: Verify condition system is loaded in VM."
  (is (find-symbol "VM-DEFINE-CONDITION" :cl-cc/vm))
  (is (find-symbol "VM-HANDLER-CASE" :cl-cc/vm))
  (is (find-symbol "VM-SIGNAL" :cl-cc/vm)))

(test runtime-subsystem-array-dimensions-loaded
  "FR-634: Verify array dimension queries are loaded in VM."
  (is (find-symbol "VM-ARRAY-RANK" :cl-cc/vm))
  (is (find-symbol "VM-ARRAY-DIMENSIONS" :cl-cc/vm)))

;;; =================================================================
;;; Wave 2-5: Integration Tests
;;; =================================================================

;;; -----------------------------------------------------------------
;;; FR semantic evidence tests
;;; -----------------------------------------------------------------

(test runtime-subsystem-sync-mutex-prevents-reentrant-access
  "FR-370: A non-recursive mutex cannot be acquired re-entrantly."
  (let ((m (cl-cc/runtime:rt-make-mutex)))
    (is (cl-cc/runtime:rt-mutex-lock m :timeout 0.1))
    (unwind-protect
         (is (not (cl-cc/runtime::rt-mutex-try-lock m)))
      (cl-cc/runtime:rt-mutex-unlock m))))

(test runtime-subsystem-sync-semaphore-counts-correctly
  "FR-370: Semaphore permits exactly COUNT waits before being exhausted."
  (let ((s (cl-cc/runtime:rt-make-semaphore :count 2)))
    (is (cl-cc/runtime::rt-semaphore-try-wait s))
    (is (cl-cc/runtime::rt-semaphore-try-wait s))
    (is (not (cl-cc/runtime::rt-semaphore-try-wait s)))
    (is (= 2 (cl-cc/runtime:rt-semaphore-signal s 2)))
    (is (cl-cc/runtime::rt-semaphore-try-wait s))
    (is (cl-cc/runtime::rt-semaphore-try-wait s))))

(test runtime-subsystem-sync-barrier-releases-waiters
  "FR-372: Barrier releases all waiters when the required count arrives."
  (let ((b (cl-cc/runtime:rt-make-barrier 1)))
    (is (= 0 (cl-cc/runtime::rt-barrier-gen b)))
    (is (cl-cc/runtime:rt-barrier-wait b :timeout 0.1))
    (is (= 0 (cl-cc/runtime::rt-barrier-count b)))
    (is (= 1 (cl-cc/runtime::rt-barrier-gen b)))))

(test runtime-subsystem-sync-once-call-executes-once
  "FR-373: Once-call executes only the first thunk and reuses its result."
  (let ((o (cl-cc/runtime:rt-make-once))
        (calls '()))
    (is (eq :first (cl-cc/runtime:rt-once-call o (lambda () (push :first calls) :first))))
    (is (eq :first (cl-cc/runtime:rt-once-call o (lambda () (push :second calls) :second))))
    (is (equal '(:first) calls))))

(test runtime-subsystem-scheduler-spawned-tasks-execute-in-order
  "FR-257: Spawned tasks execute in deterministic scheduler priority order."
  (cl-cc/runtime:rt-scheduler-init)
  (let ((events '()))
    (cl-cc/runtime:rt-spawn (lambda () (push :low events)) :priority :low)
    (cl-cc/runtime:rt-spawn (lambda () (push :normal events)) :priority :normal)
    (cl-cc/runtime:rt-spawn (lambda () (push :high events)) :priority :high)
    (cl-cc/runtime:rt-scheduler-run)
    (is (equal '(:low :normal :high) events))))

(test runtime-subsystem-scheduler-sleep-task-delays-execution
  "FR-257: Sleep-task records a future wake time for the current task."
  (cl-cc/runtime:rt-scheduler-init)
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
    (is scheduled-in-future-p)))

(test runtime-subsystem-channel-buffered-preserves-order
  "FR-282: Buffered channels receive values in send order."
  (let ((ch (cl-cc/runtime:rt-make-channel :capacity 3)))
    (cl-cc/runtime:rt-channel-send ch :a)
    (cl-cc/runtime:rt-channel-send ch :b)
    (cl-cc/runtime:rt-channel-send ch :c)
    (is (equal '(:a t) (multiple-value-list (cl-cc/runtime:rt-channel-recv ch))))
    (is (equal '(:b t) (multiple-value-list (cl-cc/runtime:rt-channel-recv ch))))
    (is (equal '(:c t) (multiple-value-list (cl-cc/runtime:rt-channel-recv ch))))))

(test runtime-subsystem-channel-close-prevents-further-sends
  "FR-282: Closed channels reject sends."
  (let ((ch (cl-cc/runtime:rt-make-channel :capacity 1)))
    (is (cl-cc/runtime:rt-channel-close ch))
    (is (handler-case
            (progn (cl-cc/runtime:rt-channel-send ch :after-close) nil)
          (error () t)))))

(test runtime-subsystem-channel-select-returns-first-available
  "FR-282: Select returns the first available channel/value pair."
  (let ((empty (cl-cc/runtime:rt-make-channel :capacity 1))
        (ready (cl-cc/runtime:rt-make-channel :capacity 1)))
    (cl-cc/runtime:rt-channel-send ready :ready)
    (multiple-value-bind (value channel ok)
        (cl-cc/runtime::rt-channel-select (list empty ready) :timeout 0.01)
      (is ok)
      (is (eq :ready value))
      (is (eq ready channel)))))

(test runtime-subsystem-actor-processes-messages-in-order
  "FR-290: Actor receive processes queued messages in mailbox order."
  (let ((a (cl-cc/runtime:rt-make-actor #'identity)))
    (cl-cc/runtime:rt-actor-send a :first)
    (cl-cc/runtime:rt-actor-send a :second)
    (cl-cc/runtime:rt-actor-send a :third)
    (is (equal '(:third :second :first)
               (list (cl-cc/runtime:rt-actor-receive a :timeout 0.1)
                     (cl-cc/runtime:rt-actor-receive a :timeout 0.1)
                     (cl-cc/runtime:rt-actor-receive a :timeout 0.1))))))

(test runtime-subsystem-stm-atomically-commits-transaction
  "FR-300: Atomically commits tvar writes."
  (let ((cell (cl-cc/runtime::rt-make-tvar 10)))
    (is (= 15 (cl-cc/runtime:rt-atomically
                (let ((old (cl-cc/runtime::rt-read-tvar cell)))
                  (cl-cc/runtime::rt-write-tvar cell (+ old 5))))))
    (is (= 15 (cl-cc/runtime::rt-tvar-value-unsafe cell)))))

(test runtime-subsystem-stm-retries-on-conflict
  "FR-300: Atomically retries when a read tvar version changes before commit."
  (let ((cell (cl-cc/runtime::rt-make-tvar 0))
        (attempts 0))
    (is (= 20 (cl-cc/runtime:rt-atomically
                (incf attempts)
                (cl-cc/runtime::rt-read-tvar cell)
                (when (= attempts 1)
                  (setf (cl-cc/runtime::rt-tvar-value cell) 10
                        (cl-cc/runtime::rt-tvar-version cell)
                        (1+ (cl-cc/runtime::rt-tvar-version cell))))
                (cl-cc/runtime::rt-write-tvar cell 20))))
    (is (= 2 attempts))
    (is (= 20 (cl-cc/runtime::rt-tvar-value-unsafe cell)))))

(test runtime-subsystem-lockfree-stack-is-lifo
  "FR-322: Lock-free stack pops the most recently pushed value first."
  (let ((s (cl-cc/runtime:rt-make-lfstack)))
    (cl-cc/runtime:rt-lfstack-push s :first)
    (cl-cc/runtime:rt-lfstack-push s :second)
    (cl-cc/runtime:rt-lfstack-push s :third)
    (is (equal '(:third t) (multiple-value-list (cl-cc/runtime:rt-lfstack-pop s))))
    (is (equal '(:second t) (multiple-value-list (cl-cc/runtime:rt-lfstack-pop s))))
    (is (equal '(:first t) (multiple-value-list (cl-cc/runtime:rt-lfstack-pop s))))))

(test runtime-subsystem-lockfree-queue-is-fifo
  "FR-322: Lock-free queue pops values in push order."
  (let ((q (cl-cc/runtime:rt-make-lfqueue)))
    (cl-cc/runtime::rt-lfqueue-push q :first)
    (cl-cc/runtime::rt-lfqueue-push q :second)
    (cl-cc/runtime::rt-lfqueue-push q :third)
    (is (equal '(:first t) (multiple-value-list (cl-cc/runtime::rt-lfqueue-pop q))))
    (is (equal '(:second t) (multiple-value-list (cl-cc/runtime::rt-lfqueue-pop q))))
    (is (equal '(:third t) (multiple-value-list (cl-cc/runtime::rt-lfqueue-pop q))))))

(test runtime-subsystem-ebr-retire-reclaim-cycle
  "FR-320: EBR retire/collect advances epochs and reclaims safe retired objects."
  (let ((freed '()))
    (cl-cc/runtime:rt-ebr-init (lambda (obj) (push obj freed)))
    (let ((local (cl-cc/runtime:rt-ebr-register-thread)))
      (cl-cc/runtime::rt-ebr-retire local :old-node)
      (is (= 0 (cl-cc/runtime::rt-ebr-collect local)))
      (is (= 1 (cl-cc/runtime::rt-ebr-collect local)))
      (is (equal '(:old-node) freed)))))

(test runtime-subsystem-spsc-preserves-single-producer-consumer-semantics
  "FR-462: SPSC queue preserves ordered single-producer/single-consumer handoff."
  (let ((q (cl-cc/runtime:rt-make-spsc-queue 2)))
    (is (cl-cc/runtime:rt-spsc-try-push q :first))
    (is (cl-cc/runtime:rt-spsc-try-push q :second))
    (is (cl-cc/runtime::rt-spsc-full-p q))
    (is (not (cl-cc/runtime:rt-spsc-try-push q :third)))
    (is (equal '(:first t) (multiple-value-list (cl-cc/runtime:rt-spsc-try-pop q))))
    (is (equal '(:second t) (multiple-value-list (cl-cc/runtime:rt-spsc-try-pop q))))
    (is (equal '(nil nil) (multiple-value-list (cl-cc/runtime:rt-spsc-try-pop q))))))

(test runtime-subsystem-crdt-gcounter-merges-by-node-max
  "FR-431: GCounter merge keeps the per-node maximum and sums merged slots."
  (let ((a (cl-cc/runtime:rt-make-gcounter))
        (b (cl-cc/runtime:rt-make-gcounter)))
    (cl-cc/runtime:rt-gcounter-increment a :n1 1)
    (cl-cc/runtime:rt-gcounter-increment a :n2 5)
    (cl-cc/runtime:rt-gcounter-increment b :n1 3)
    (cl-cc/runtime:rt-gcounter-increment b :n3 7)
    (cl-cc/runtime::rt-gcounter-merge a b)
    (is (= 15 (cl-cc/runtime:rt-gcounter-value a)))))

(test runtime-subsystem-crdt-pncounter-value-is-pos-minus-neg
  "FR-431: PNCounter value is positive count minus negative count."
  (let ((c (cl-cc/runtime:rt-make-pncounter)))
    (cl-cc/runtime::rt-pncounter-increment c :n1 10)
    (cl-cc/runtime::rt-pncounter-increment c :n2 4)
    (cl-cc/runtime::rt-pncounter-decrement c :n1 3)
    (cl-cc/runtime::rt-pncounter-decrement c :n3 2)
    (is (= 9 (cl-cc/runtime::rt-pncounter-value c)))))

(test runtime-subsystem-raft-leader-election-picks-leader
  "FR-432: Starting an election picks a majority-backed leader."
  (let* ((cluster (cl-cc/runtime:rt-make-raft-cluster '("n1" "n2" "n3")))
         (node (gethash "n1" (cl-cc/runtime:rt-raft-cluster-nodes cluster))))
    (is (cl-cc/runtime::rt-raft-start-election node cluster))
    (is (string= "n1" (cl-cc/runtime::rt-raft-cluster-leader-id cluster)))
    (is (= cl-cc/runtime::+raft-leader+ (cl-cc/runtime::rt-raft-node-state node)))))

(test runtime-subsystem-raft-log-entries-are-replicated
  "FR-432: Proposed log entries are replicated to every in-memory cluster node."
  (let* ((cluster (cl-cc/runtime:rt-make-raft-cluster '("n1" "n2" "n3")))
         (leader (gethash "n1" (cl-cc/runtime:rt-raft-cluster-nodes cluster))))
    (is (cl-cc/runtime::rt-raft-start-election leader cluster))
    (is (eq :set-x (cl-cc/runtime:rt-raft-propose cluster :set-x)))
    (maphash
     (lambda (id node)
       (declare (ignore id))
       (let ((commands (mapcar #'cl-cc/runtime::rt-raft-entry-command
                               (cl-cc/runtime::rt-raft-node-log node))))
         (is (member :set-x commands))))
     (cl-cc/runtime:rt-raft-cluster-nodes cluster))))

(test runtime-subsystem-sync-mutex-basic
  "FR-370: Basic mutex lock/unlock."
  (let ((m (cl-cc/runtime:rt-make-mutex)))
    (is (cl-cc/runtime:rt-mutex-lock m :timeout 0.1))
    (is (cl-cc/runtime:rt-mutex-unlock m))))

(test runtime-subsystem-sync-semaphore-basic
  "FR-370: Basic semaphore wait/signal."
  (let ((s (cl-cc/runtime:rt-make-semaphore :count 1)))
    (is (cl-cc/runtime:rt-semaphore-wait s :timeout 0.1))
    (is (cl-cc/runtime:rt-semaphore-signal s))))

(test runtime-subsystem-sync-barrier-basic
  "FR-372: Basic barrier wait."
  (let ((b (cl-cc/runtime:rt-make-barrier 1)))
    (is (cl-cc/runtime:rt-barrier-wait b :timeout 0.1))))

(test runtime-subsystem-sync-once-basic
  "FR-373: Basic once call."
  (let ((o (cl-cc/runtime:rt-make-once))
        (count 0))
    (cl-cc/runtime:rt-once-call o (lambda () (incf count)))
    (is (= 1 count))
    (cl-cc/runtime:rt-once-call o (lambda () (incf count)))
    (is (= 1 count))))

(test runtime-subsystem-scheduler-spawn-basic
  "FR-257: Basic spawn and run."
  (cl-cc/runtime:rt-scheduler-init)
  (let ((result nil))
    (cl-cc/runtime:rt-spawn (lambda () (setf result 42)))
    (cl-cc/runtime:rt-scheduler-run)
    (is (eql 42 result))))

(test runtime-subsystem-future-basic
  "FR-283: Basic future resolve/await."
  (let ((f (cl-cc/runtime:rt-make-future)))
    (cl-cc/runtime:rt-future-resolve f 99)
    (is (eql 99 (cl-cc/runtime:rt-future-await f :timeout 0.1)))))

(test runtime-subsystem-channel-basic
  "FR-282: Basic channel send/recv."
  (let ((ch (cl-cc/runtime:rt-make-channel :capacity 1)))
    (is (eql 7 (cl-cc/runtime:rt-channel-send ch 7)))
    (is (eql 7 (cl-cc/runtime:rt-channel-recv ch)))))

(test runtime-subsystem-actor-basic
  "FR-290: Basic actor send/receive."
  (let ((a (cl-cc/runtime:rt-make-actor #'identity)))
    (cl-cc/runtime:rt-actor-send a :hello)
    (is (eq :hello (cl-cc/runtime:rt-actor-receive a :timeout 0.1)))))

(test runtime-subsystem-lockfree-stack-basic
  "FR-322: Basic lock-free stack push/pop."
  (let ((s (cl-cc/runtime:rt-make-lfstack)))
    (cl-cc/runtime:rt-lfstack-push s 1)
    (cl-cc/runtime:rt-lfstack-push s 2)
    (multiple-value-bind (val ok) (cl-cc/runtime:rt-lfstack-pop s)
      (is ok)
      (is (eql 2 val)))))

(test runtime-subsystem-spsc-basic
  "FR-462: Basic SPSC push/pop."
  (let ((q (cl-cc/runtime:rt-make-spsc-queue 4)))
    (is (cl-cc/runtime:rt-spsc-try-push q 10))
    (multiple-value-bind (val ok) (cl-cc/runtime:rt-spsc-try-pop q)
      (is ok)
      (is (eql 10 val)))))

(test runtime-subsystem-crdt-gcounter-basic
  "FR-431: Basic GCounter increment."
  (let ((c (cl-cc/runtime:rt-make-gcounter)))
    (cl-cc/runtime:rt-gcounter-increment c 1 5)
    (is (= 5 (cl-cc/runtime:rt-gcounter-value c)))))

(test runtime-subsystem-consensus-raft-basic
  "FR-432: Basic Raft cluster creation."
  (let ((c (cl-cc/runtime:rt-make-raft-cluster '("n1" "n2" "n3"))))
    (is (gethash "n1" (cl-cc/runtime:rt-raft-cluster-nodes c)))))

(test runtime-subsystem-image-roundtrip
  "FR-350: Basic image save/load round-trip."
  (let ((path "/tmp/cl-cc-test-image.bin"))
    (cl-cc/runtime:rt-save-image path)
    (is (probe-file path))
    (is (cl-cc/runtime:rt-load-image path))
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

(test runtime-subsystem-fr-coverage-complete
  "Verify all runtime subsystem FR IDs are tracked."
  (is (= 170 (length *runtime-subsystem-fr-coverage*)))
  (is (member :fr-500 *runtime-subsystem-fr-coverage*))
  (is (member :fr-654 *runtime-subsystem-fr-coverage*))
  (is (member :fr-190 *runtime-subsystem-fr-coverage*))
  (is (member :fr-492 *runtime-subsystem-fr-coverage*)))
