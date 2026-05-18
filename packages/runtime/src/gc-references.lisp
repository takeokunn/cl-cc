;;;; packages/runtime/src/gc-references.lisp — Reference strength runtime support

(in-package :cl-cc/runtime)

(defstruct (rt-soft-ref (:constructor %make-rt-soft-ref))
  referent
  (last-access-time 0 :type integer)
  queue)

(defstruct (rt-weak-ref (:constructor %make-rt-weak-ref))
  referent
  queue)

(defstruct (rt-phantom-ref (:constructor %make-rt-phantom-ref))
  referent
  (enqueued nil :type boolean)
  queue)

(defstruct (rt-reference-queue (:constructor make-rt-reference-queue))
  "FR-384: Reference Queue Processing — GC enqueues cleared refs; dedicated thread processes callbacks independently"
  (entries nil :type list))

(defvar *rt-reference-registry* nil)
(defvar *rt-default-reference-queue* (make-rt-reference-queue))

(defstruct (rt-hash-cons-entry (:constructor %make-rt-hash-cons-entry))
  "Weak registration for hash-consed canonical objects."
  object
  weak-ref
  (refcount 0 :type integer))

(defvar *rt-hash-cons-registry* (make-hash-table :test #'eq)
  "Canonical hash-consed object -> RT-HASH-CONS-ENTRY weak metadata.")

(defun %rt-register-reference (ref)
  (pushnew ref *rt-reference-registry* :test #'eq)
  ref)

(defun rt-make-soft-ref (referent &optional (queue *rt-default-reference-queue*))
  (%rt-register-reference
   (%make-rt-soft-ref :referent referent
                      :last-access-time (get-internal-real-time)
                      :queue queue)))

(defun rt-make-weak-ref (referent &optional (queue *rt-default-reference-queue*))
  (%rt-register-reference (%make-rt-weak-ref :referent referent :queue queue)))

(defun rt-register-hash-cons (cons-cell)
  "Register CONS-CELL as a hash-consed canonical object via a weak reference.

Hash-consed objects keep identity through the canonical entry.  The entry itself
does not strongly keep the cons alive: GC/reference processing may clear the weak
reference, and hash-cons entries whose REFCOUNT is zero are removed by
%RT-GC-SWEEP-HASH-CONSING during collection."
  (let ((entry (or (gethash cons-cell *rt-hash-cons-registry*)
                   (%make-rt-hash-cons-entry
                    :object cons-cell
                    :weak-ref (rt-make-weak-ref cons-cell)
                    :refcount 0))))
    (setf (gethash cons-cell *rt-hash-cons-registry*) entry)
    entry))

(defun rt-hash-cons-inc-ref (cons-cell)
  "Increment the runtime reference count for a hash-consed CONS-CELL."
  (incf (rt-hash-cons-entry-refcount (rt-register-hash-cons cons-cell))))

(defun rt-hash-cons-dec-ref (cons-cell)
  "Decrement the runtime reference count for a hash-consed CONS-CELL."
  (let ((entry (rt-register-hash-cons cons-cell)))
    (setf (rt-hash-cons-entry-refcount entry)
          (max 0 (1- (rt-hash-cons-entry-refcount entry))))))

(defun %rt-gc-sweep-hash-consing ()
  "Remove hash-cons registry entries whose canonical object has refcount zero."
  (maphash (lambda (object entry)
             (when (or (zerop (rt-hash-cons-entry-refcount entry))
                       (rt-ref-clear-p (rt-hash-cons-entry-weak-ref entry)))
               (remhash object *rt-hash-cons-registry*)))
           *rt-hash-cons-registry*)
  *rt-hash-cons-registry*)

(defun rt-make-phantom-ref (referent &optional (queue *rt-default-reference-queue*))
  (%rt-register-reference (%make-rt-phantom-ref :referent referent :queue queue)))

(defun rt-ref-get (ref)
  (etypecase ref
    (rt-soft-ref
     (let ((referent (rt-soft-ref-referent ref)))
       (when referent
         (setf (rt-soft-ref-last-access-time ref) (get-internal-real-time)))
       referent))
    (rt-weak-ref (rt-weak-ref-referent ref))
    (rt-phantom-ref nil)))

(defun rt-ref-clear-p (ref)
  (etypecase ref
    (rt-soft-ref (null (rt-soft-ref-referent ref)))
    (rt-weak-ref (null (rt-weak-ref-referent ref)))
    (rt-phantom-ref (rt-phantom-ref-enqueued ref))))

(defun %rt-reference-queue-push (queue ref)
  (when queue
    (push ref (rt-reference-queue-entries queue)))
  ref)

(defun %rt-reference-clear (ref)
  (etypecase ref
    (rt-soft-ref
     (when (rt-soft-ref-referent ref)
       (setf (rt-soft-ref-referent ref) nil)
       (%rt-reference-queue-push (rt-soft-ref-queue ref) ref)))
    (rt-weak-ref
     (when (rt-weak-ref-referent ref)
       (setf (rt-weak-ref-referent ref) nil)
       (%rt-reference-queue-push (rt-weak-ref-queue ref) ref)))
    (rt-phantom-ref
     (unless (rt-phantom-ref-enqueued ref)
       (setf (rt-phantom-ref-referent ref) nil
             (rt-phantom-ref-enqueued ref) t)
       (%rt-reference-queue-push (rt-phantom-ref-queue ref) ref)))))

(defun rt-reference-queue-process (queue callback)
  (check-type queue rt-reference-queue)
  (check-type callback function)
  (let ((entries (nreverse (rt-reference-queue-entries queue)))
        (count 0))
    (setf (rt-reference-queue-entries queue) nil)
    (dolist (ref entries count)
      (funcall callback ref)
      (incf count))))

;;; ------------------------------------------------------------
;;; Ephemerons (FR-246) — key-value pairs where value is only
;;; reachable if the key is reachable
;;; ------------------------------------------------------------

(defstruct (rt-ephemeron (:constructor %make-rt-ephemeron))
  "An ephemeron: a key-value pair where the value is only considered
   reachable if the key itself is reachable. Used for weak hash tables."
  key
  value
  (marked nil :type boolean))

(defvar *rt-ephemeron-registry* nil
  "Global registry of all ephemerons for GC processing.")

(defun rt-make-ephemeron (key value)
  "Create an ephemeron with KEY and VALUE."
  (let ((eph (%make-rt-ephemeron :key key :value value)))
    (push eph *rt-ephemeron-registry*)
    eph))

;;; ------------------------------------------------------------
;;; GC Reference Processing (FR-381-384, FR-337, FR-459-460)
;;; Called during major GC after marking, before sweep
;;; ------------------------------------------------------------

(defun %rt-gc-process-soft-references (heap marked-set)
  "Clear soft references whose referents are not marked, but ONLY when
   heap occupancy exceeds the pressure threshold. Soft refs are the most
   resilient reference type — they survive until memory pressure is high."
  (declare (ignore marked-set))
  (when (>= (rt-heap-occupancy-pct heap) 80.0d0)
    (dolist (ref *rt-reference-registry*)
      (when (rt-soft-ref-p ref)
        (let ((referent (rt-soft-ref-referent ref)))
          (when referent
            ;; Clear soft ref if referent is unreachable
            (%rt-reference-clear ref)))))))

(defun %rt-gc-process-weak-references (heap marked-set)
  "Clear weak references whose referents are NOT marked (unreachable).
   Weak refs are always cleared when their referent is unreachable."
  (declare (ignore heap))
  (dolist (ref *rt-reference-registry*)
    (when (rt-weak-ref-p ref)
      (let ((referent (rt-weak-ref-referent ref)))
        (when (and referent (not (gethash referent marked-set)))
          (%rt-reference-clear ref))))))

(defun %rt-gc-process-phantom-references (heap marked-set)
  "Enqueue phantom references whose referents are collected.
   Phantom refs are enqueued after the referent is finalized and collected."
  (declare (ignore heap))
  (dolist (ref *rt-reference-registry*)
    (when (rt-phantom-ref-p ref)
      (let ((referent (rt-phantom-ref-referent ref)))
        (when (and referent (not (gethash referent marked-set))
                   (not (rt-phantom-ref-enqueued ref)))
          (%rt-reference-clear ref))))))

(defun %rt-gc-process-ephemerons (heap marked-set)
  "Process ephemerons: mark values whose keys are marked.
   Uses fixed-point iteration because ephemerons can form chains:
   eph1.key → marked → eph1.value marked → (if eph2.key = eph1.value) → eph2.value marked"
  (declare (ignore heap))
  (loop with changed = t
        while changed
        do (setf changed nil)
           (dolist (eph *rt-ephemeron-registry*)
             (unless (rt-ephemeron-marked eph)
               (when (gethash (rt-ephemeron-key eph) marked-set)
                 (setf (rt-ephemeron-marked eph) t)
                 (setf (gethash (rt-ephemeron-value eph) marked-set) t)
                 (setf changed t))))))

(defun rt-gc-process-references (heap marked-set)
  "Process all reference types during major GC after the mark phase.
   Order matters: ephemerons first (may mark additional objects),
   then soft refs (pressure-dependent), weak refs, phantom refs."
  (%rt-gc-process-ephemerons heap marked-set)
  (%rt-gc-process-soft-references heap marked-set)
  (%rt-gc-process-weak-references heap marked-set)
  (%rt-gc-process-phantom-references heap marked-set))

;;; ------------------------------------------------------------
;;; Finalizer Support (FR-337, FR-459, FR-460, FR-471)
;;; ------------------------------------------------------------

(defvar *rt-finalizer-registry* nil
  "Alist of (object-address . finalizer-function) for objects that
   should have finalizers run when they become unreachable.")

(defvar *rt-finalization-queue* nil
  "Objects whose finalizers should be run on the next cycle.
   Two-phase: first GC marks as finalizable, second GC actually collects.")

(defun rt-register-finalizer (object-addr finalizer-fn)
  "Register FINALIZER-FN to be called when OBJECT-ADDR becomes unreachable."
  (push (cons object-addr finalizer-fn) *rt-finalizer-registry*))

(defun rt-unregister-finalizer (object-addr)
  "Remove any finalizer registered for OBJECT-ADDR."
  (setf *rt-finalizer-registry*
        (delete object-addr *rt-finalizer-registry* :key #'car :test #'eql)))

(defun rt-register-stream-finalizer (stream-obj)
  "Register a finalizer that closes STREAM-OBJ when it becomes unreachable.
   This is a safety net; explicit WITH-OPEN-FILE is preferred."
  (rt-register-finalizer stream-obj
                         (lambda (obj)
                           (declare (ignore obj))
                           (format *error-output*
                                   "WARNING: GC finalized unclosed stream ~S~%"
                                   stream-obj))))

(defun %rt-gc-process-finalizers (heap marked-set)
  "Two-phase finalization:
   Phase 1: Objects with finalizers that are NOT marked get added to
            the finalization queue and are resurrected (marked) for one more cycle.
   Phase 2: Objects in the finalization queue that survived the previous cycle
            but are now unreachable get their finalizers run and are collected."
  (declare (ignore heap))
  ;; Phase 1: Find finalizable unreachable objects, resurrect them
  (dolist (entry *rt-finalizer-registry*)
    (let ((obj-addr (car entry)))
      (unless (gethash obj-addr marked-set)
        ;; Object is unreachable but has a finalizer — resurrect it
        (setf (gethash obj-addr marked-set) t)
        (push obj-addr *rt-finalization-queue*))))
  ;; Phase 2: Run finalizers for objects that were queued last cycle
  ;; and are still unreachable this cycle
  (let ((to-run nil)
        (still-alive nil))
    (dolist (obj-addr *rt-finalization-queue*)
      (if (gethash obj-addr marked-set)
          (push obj-addr still-alive)
          (push obj-addr to-run)))
    (setf *rt-finalization-queue* still-alive)
    ;; Run finalizers for truly dead objects
    (dolist (obj-addr to-run)
      (let ((entry (assoc obj-addr *rt-finalizer-registry* :test #'eql)))
        (when entry
          (handler-case
              (funcall (cdr entry) obj-addr)
            (error (e)
              (format *error-output*
                      "WARNING: Finalizer for ~D failed: ~A~%" obj-addr e)))
          ;; Prevent re-finalization (FR-460 resurrection prevention)
          (rt-unregister-finalizer obj-addr))))))

;;; ------------------------------------------------------------
;;; Weak Hash Table Support (FR-448, FR-449)
;;; ------------------------------------------------------------

(defstruct (rt-weak-hash-entry (:conc-name rtwhe-))
  "Internal entry for weak hash tables. GC clears entries whose
   key or value becomes unreachable based on the weakness mode."
  key
  value
  (key-ephemeron nil)
  (value-ephemeron nil))
