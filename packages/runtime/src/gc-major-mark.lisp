;;;; packages/runtime/src/gc-major.lisp — Major GC (tri-color mark-sweep) + GC Statistics
;;;
;;; Contains:
;;;   - %gc-sweep-old-space — sweep dead objects from old generation
;;;   - rt-gc-major-collect — tri-color mark-and-sweep full GC
;;;   - rt-gc-stats — GC statistics plist
;;;
;;; Minor GC (Cheney copying), write barrier, allocation, and root registration
;;; are in gc.lisp (loads before).
;;;
;;; Load order: after gc.lisp.
(in-package :cl-cc/runtime)

(defparameter *rt-concurrent-gc-enabled-p* nil
  "When true, major collection enters :major-gc-concurrent state.")

(defparameter *rt-concurrent-gc-write-barrier-mode* :satb
  "Concurrent-GC barrier mode (:satb or :incremental-update).")

(defparameter *rt-concurrent-gc-stw-phases* '(:initial-mark :final-remark)
  "STW phase model used when concurrent GC mode is enabled.")

(defparameter *rt-concurrent-gc-mutator-assist-p* t
  "When true, mutators are expected to assist concurrent marking progress.")

(defparameter *rt-concurrent-mark-thread* nil
  "Host thread object for the active concurrent mark worker, or NIL.

FR-339 foundation: SBCL hosts may run phase-2 marking on SB-THREAD while mutators
continue through SATB pre-write barriers. Implementations without threads drain
the same grey queue synchronously while preserving the four-phase protocol.")

;;; FR-088: Incremental GC Marking — bounded mark steps interleaved with mutator work; 4-phase protocol (initial-mark, incremental-mark, final-remark, sweep)
(defparameter *gc-incremental-mark-enabled* nil
  "When true, major GC exposes its mark phase as bounded incremental steps.

RT-GC-MAJOR-COLLECT is organized as:
  Phase 1: initial mark (STW root snapshot)
  Phase 2: incremental mark (RT-GC-INCREMENTAL-MARK-STEP drains grey work)
  Phase 3: final remark (STW SATB drain)
   Phase 4: sweep (STW in this foundation implementation).")

;;; FR-468: GC Copy Order as Cache Warm-Up — biases free-list toward blocks adjacent to hot objects for cache-friendly placement
(defparameter *gc-profile-guided-placement* nil
  "When true, bias old-space free-list order toward blocks adjacent to hot objects.

This is the FR-468 foundation hook.  The current mark-sweep collector does not
move live objects; it reorders reclaimed blocks so future old-space allocation
prefers holes near hot survivors, preserving a cache-friendly placement path for
the future compacting collector.")

;;; FR-340: Concurrent Sweeping — sweeps old space on-demand during allocation; lazy sweep with page-level granularity
(defparameter *gc-lazy-sweep-enabled* nil
  "When true, major GC defers old-space sweeping and allocation sweeps pages on demand.")

(defparameter *gc-max-pause-ms* 200
  "Maximum target stop-the-world GC pause in milliseconds before SLO accounting fires.")

(defvar *rt-gc-incremental-mark-queues* (make-hash-table :test #'eq)
  "Heap -> grey queue for incremental major-GC marking.")

(defun %rt-gc-mark-sb-thread-function (name)
  "Return the SB-THREAD function named NAME, or NIL when unavailable."
  (ignore-errors
    (let ((package (find-package "SB-THREAD")))
      (when package
        (multiple-value-bind (symbol status) (find-symbol name package)
          (when (and status (fboundp symbol))
            (symbol-function symbol)))))))

(defun %rt-gc-mark-make-mutex (name)
  "Create an optional host mutex without reader-time SB-THREAD dependency."
  (let ((make-mutex (%rt-gc-mark-sb-thread-function "MAKE-MUTEX")))
    (and make-mutex (ignore-errors (funcall make-mutex :name name)))))

(defvar *rt-gc-incremental-mark-queues-lock*
  (%rt-gc-mark-make-mutex "gc-incremental-mark-queues")
  "Mutex protecting *rt-gc-incremental-mark-queues* from concurrent access.")

(defmacro with-gc-mark-queue-locked (() &body body)
  "Execute BODY while holding the incremental mark-queue hash-table mutex."
  (let ((fn (gensym "CALL-WITH-MUTEX"))
        (lock (gensym "LOCK")))
    `(let ((,lock *rt-gc-incremental-mark-queues-lock*))
       (if ,lock
           (let ((,fn (%rt-gc-mark-sb-thread-function "CALL-WITH-MUTEX")))
             (if ,fn
                 (funcall ,fn (lambda () ,@body) ,lock)
                 (progn ,@body)))
           (progn ,@body)))))

(defun rt-gc-configure-concurrent-mode (&key enabled-p write-barrier stw-phases mutator-assist-p)
  "Configure concurrent-GC runtime mode and return current settings plist."
  (when (not (null enabled-p))
    (setf *rt-concurrent-gc-enabled-p* enabled-p))
  (when write-barrier
    (setf *rt-concurrent-gc-write-barrier-mode* write-barrier))
  (when stw-phases
    (setf *rt-concurrent-gc-stw-phases* (copy-list stw-phases)))
  (when (not (null mutator-assist-p))
    (setf *rt-concurrent-gc-mutator-assist-p* mutator-assist-p))
  (list :enabled-p *rt-concurrent-gc-enabled-p*
        :write-barrier *rt-concurrent-gc-write-barrier-mode*
        :stw-phases (copy-list *rt-concurrent-gc-stw-phases*)
        :mutator-assist-p *rt-concurrent-gc-mutator-assist-p*))

(defun rt-gc-concurrent-assist (heap &key (budget 16))
  "Perform a bounded mutator-assist step during concurrent major GC.

Drains current SATB entries into the incremental grey queue, marks up to BUDGET
grey objects, and returns the number of grey objects processed."
  (if (or (not *rt-concurrent-gc-enabled-p*)
          (not *rt-concurrent-gc-mutator-assist-p*)
          (not (eq (rt-heap-gc-state heap) :major-gc-concurrent)))
      0
      (let ((queue-cell (cons (with-gc-mark-queue-locked ()
                                (gethash heap *rt-gc-incremental-mark-queues*))
                              nil)))
        (%rt-gc-drain-satb-to-grey heap queue-cell)
        (with-gc-mark-queue-locked ()
          (setf (gethash heap *rt-gc-incremental-mark-queues*) (car queue-cell)))
        (destructuring-bind (status &optional processed)
            (rt-gc-incremental-mark-step heap budget)
          (declare (ignore status))
          (or processed 0)))))

;;; Section 5: Major GC — Tri-color Mark-Sweep

(defun %rt-gc-grey-object (heap queue-cell addr)
  "Push old-space ADDR into QUEUE-CELL when it has not yet been marked/greyed."
  (when (and (integerp addr) (rt-old-addr-p heap addr))
    (let ((h (rt-heap-object-header heap addr)))
      (when (and (integerp h)
                 (not (header-marked-p h))
                 (not (header-gray-p h)))
        (rt-heap-set-header heap addr (header-set-gray h))
        (push addr (car queue-cell))
        addr))))

(defun %rt-gc-package-symbol-address (heap symbol)
  "Return SYMBOL's heap address when it is represented in HEAP."
  (%rt-gc-pointer-address heap symbol))

(defun rt-gc-mark-package-symbols (heap package)
  "Mark/grey interned symbols reachable from PACKAGE's runtime symbol table.

PACKAGE may be a runtime package descriptor or any designator accepted by the
runtime registry.  Only symbols present in the package's intern table are
considered package roots.  Uninterned host symbols are removed from the runtime
symbol table during this pass so subsequent sweeps do not retain them through
stale package metadata.  Returns the list of heap symbol addresses submitted to
the major-GC mark queue."
  (check-type heap rt-heap)
  (let* ((pkg (and package
                   (if (hash-table-p package)
                       package
                       (and (fboundp '%rt-package-metadata)
                            (%rt-package-metadata package)))))
         (table (and pkg (gethash :symbols pkg)))
         (host-package (and pkg (gethash :host-package pkg)))
         (queue-cell (cons nil nil))
         (marked nil))
    (when table
      (let (stale-names)
        (maphash
         (lambda (name symbol)
           (cond
             ((and (symbolp symbol)
                   (null (symbol-package symbol)))
              (push name stale-names))
             ((and (symbolp symbol) host-package
                   (symbol-package symbol)
                   (not (eq (symbol-package symbol) host-package)))
              ;; The symbol is interned elsewhere; it is not a root of this package.
              nil)
             (t
              (let ((addr (%rt-gc-package-symbol-address heap symbol)))
                (when (and addr (%rt-gc-grey-object heap queue-cell addr))
                  (push addr marked))))))
         table)
        (dolist (name stale-names)
          (remhash name table))))
    (when pkg
      (setf (gethash :gc-marked-symbols pkg) (nreverse marked)))
    (car queue-cell)))

(defun %rt-gc-mark-package-registry (heap queue-cell)
  "Grey symbols reachable from all runtime package descriptors."
  (when (boundp '*rt-package-registry*)
    (maphash (lambda (name package)
               (declare (ignore name))
               (dolist (addr (rt-gc-mark-package-symbols heap package))
                 (pushnew addr (car queue-cell) :test #'eql)))
             *rt-package-registry*))
  queue-cell)

(defun %rt-gc-seed-major-roots (heap queue-cell)
  "Phase 1 initial mark: grey old-space objects reachable from roots."
  (labels ((maybe-gray (addr)
             (%rt-gc-grey-object heap queue-cell addr))
           (scan-young-object (addr)
             (let ((h (rt-heap-object-header heap addr)))
               (when (and (integerp h) (> (header-size h) 0))
                 (dolist (offset (rt-object-pointer-slots heap addr))
                    (let ((child (%rt-gc-pointer-address
                                  heap (rt-heap-ref heap (+ addr offset)))))
                     (when child (maybe-gray child))))))))
    (dolist (root-cell (rt-heap-roots heap))
      (let ((val (%rt-gc-root-pointer-address heap root-cell)))
        (cond
          ((and val (rt-old-addr-p heap val)) (maybe-gray val))
          ((and val (rt-young-addr-p heap val)) (scan-young-object val)))))
    (dolist (addr (%rt-gc-scan-binding-stacks heap))
      (cond
        ((rt-old-addr-p heap addr) (maybe-gray addr))
        ((rt-young-addr-p heap addr) (scan-young-object addr))))
    (dolist (addr (%rt-gc-scan-global-variables heap))
      (cond
        ((rt-old-addr-p heap addr) (maybe-gray addr))
        ((rt-young-addr-p heap addr) (scan-young-object addr))))
    (%rt-gc-mark-package-registry heap queue-cell)
    (dolist (addr (%rt-gc-conservative-root-addresses heap))
      (cond
        ((rt-old-addr-p heap addr) (maybe-gray addr))
        ((rt-young-addr-p heap addr) (scan-young-object addr))))
    queue-cell))

(defun %rt-gc-drain-satb-to-grey (heap queue-cell)
  "Phase 3 final remark helper: drain SATB entries into the major-GC grey queue."
  (dolist (ptr (nconc (rt-heap-satb-queue heap)
                      (when (fboundp 'rt-gc-drain-satb-thread-queues)
                        (rt-gc-drain-satb-thread-queues heap))))
    (let ((addr (%rt-gc-pointer-address heap ptr)))
      (when addr
        (%rt-gc-grey-object heap queue-cell addr))))
  (setf (rt-heap-satb-queue heap) nil)
  queue-cell)

(defun %rt-gc-mark-one-grey (heap queue-cell addr)
  "Blacken ADDR and grey each unvisited old-space child."
  (let ((header (rt-heap-object-header heap addr)))
    (when (integerp header)
      (rt-heap-set-header heap addr
                          (header-clear-gray (header-set-mark header)))
      (dolist (offset (rt-object-pointer-slots heap addr))
        (let ((child-addr (%rt-gc-pointer-address
                            heap (rt-heap-ref heap (+ addr offset)))))
          (when child-addr
            (%rt-gc-grey-object heap queue-cell child-addr)))))))

;;; FR-339: Tri-color invariant verification
(defun rt-gc-verify-tri-color-invariant (heap)
  "Verify the tri-color invariant: no black object points to a white object.
Returns T if invariant holds, or (values nil violating-addr) if violated."
  (check-type heap rt-heap)
  (labels ((white-object-p (addr)
             (and (integerp addr)
                  (rt-old-addr-p heap addr)
                  (let ((h (rt-heap-object-header heap addr)))
                    (and (integerp h)
                         (plusp (header-size h))
                         (not (header-marked-p h))
                         (not (header-gray-p h)))))))
    (loop with addr = (rt-heap-old-base heap)
          while (< addr (rt-heap-old-free heap)) do
            (let ((h (rt-heap-object-header heap addr)))
              (cond
                ((header-forwarding-p h)
                 (incf addr 1))
                ((or (not (integerp h)) (zerop (header-size h)))
                 (return t))
                ((header-marked-p h)
                 (dolist (offset (rt-object-pointer-slots heap addr))
                   (let ((child (%rt-gc-pointer-address
                                  heap (rt-heap-ref heap (+ addr offset)))))
                     (when (white-object-p child)
                       (return-from rt-gc-verify-tri-color-invariant
                         (values nil addr)))))
                 (incf addr (header-size h)))
                (t
                 (incf addr (header-size h))))))
    t))

(defun %rt-gc-drain-major-mark-work (heap queue-cell)
  "Drain major-GC mark work, using FR-338 workers when configured."
  (if (plusp *gc-worker-count*)
      (progn
        (with-gc-mark-queue-locked ()
          (setf (gethash heap *rt-gc-incremental-mark-queues*) (car queue-cell)))
        (rt-gc-parallel-mark heap *gc-worker-count*)
        (setf (car queue-cell) nil))
      (loop while (car queue-cell) do
        (%rt-gc-mark-one-grey heap queue-cell (pop (car queue-cell))))))

;;; FR-088: Incremental GC Marking — bounded mark steps interleaved with mutator work; 4-phase protocol (initial-mark, incremental-mark, final-remark, sweep)
(defun rt-gc-incremental-mark-step (heap budget)
  "Process up to BUDGET grey old-space objects for an incremental major GC.

Returns (:MORE-WORK N) when grey work remains after processing N objects, or
(:DONE) when the queue is empty.  The queue is initialized by
RT-GC-MAJOR-COLLECT phase 1 and stored in *RT-GC-INCREMENTAL-MARK-QUEUES* so
mutator assist and idle work can make bounded progress between STW phases."
  (check-type heap rt-heap)
  (check-type budget (integer 0 *))
  (let ((queue-cell (cons (with-gc-mark-queue-locked ()
                            (gethash heap *rt-gc-incremental-mark-queues*))
                          nil))
        (processed 0))
    (loop while (and (< processed budget) (car queue-cell)) do
      (%rt-gc-mark-one-grey heap queue-cell (pop (car queue-cell)))
      (incf processed))
    (with-gc-mark-queue-locked ()
      (setf (gethash heap *rt-gc-incremental-mark-queues*) (car queue-cell)))
    (if (car queue-cell)
        (list :more-work processed)
        (progn
          (with-gc-mark-queue-locked ()
            (remhash heap *rt-gc-incremental-mark-queues*))
          (list :done)))))

(defun %rt-gc-drain-incremental-mark (heap queue-cell)
  "Drain QUEUE-CELL to completion using the same primitive as incremental steps."
  (with-gc-mark-queue-locked ()
    (setf (gethash heap *rt-gc-incremental-mark-queues*) (car queue-cell)))
  (loop for result = (rt-gc-incremental-mark-step heap 64)
        until (eq (first result) :done))
  (setf (car queue-cell)
        (with-gc-mark-queue-locked ()
          (gethash heap *rt-gc-incremental-mark-queues*)))
  queue-cell)

(defun %rt-gc-run-concurrent-mark (heap queue-cell)
  "Phase 2 concurrent mark worker for FR-339.

The worker owns the grey queue between initial mark and final remark. On SBCL it
is launched with SB-THREAD; otherwise the Pure CL fallback executes the same work
synchronously. SATB queues are drained only by final remark to close the snapshot
created by the initial STW root scan."
  (labels ((mark-work ()
             (if *gc-incremental-mark-enabled*
                 (%rt-gc-drain-incremental-mark heap queue-cell)
                 (loop while (car queue-cell) do
                   (%rt-gc-mark-one-grey heap queue-cell (pop (car queue-cell)))))))
    (if (and (find-package :sb-thread)
             (fboundp (find-symbol "MAKE-THREAD" :sb-thread))
             (fboundp (find-symbol "JOIN-THREAD" :sb-thread)))
        (let* ((make-thread (symbol-function (find-symbol "MAKE-THREAD" :sb-thread)))
               (join-thread (symbol-function (find-symbol "JOIN-THREAD" :sb-thread)))
               (thread (ignore-errors
                         (funcall make-thread #'mark-work :name "cl-cc concurrent mark"))))
          (if thread
              (unwind-protect
                   (progn
                     (setf *rt-concurrent-mark-thread* thread)
                     (ignore-errors (funcall join-thread thread)))
                (setf *rt-concurrent-mark-thread* nil))
              (mark-work)))
        (mark-work)))
  queue-cell)
