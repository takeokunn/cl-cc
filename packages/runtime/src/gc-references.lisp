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

(defvar *rt-weak-hash-table-registry* nil
  "All runtime weak hash tables.  The collector uses this metadata to remove
entries whose weak key/value has died without treating the metadata itself as a
strong reference.")

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

(defun rt-make-weak-pointer (referent &optional (queue *rt-default-reference-queue*))
  "Create a weak pointer to REFERENT.  The referent is not treated as a GC root."
  (rt-make-weak-ref referent queue))

(defun rt-weak-pointer-p (object)
  "Return true when OBJECT is a runtime weak pointer."
  (rt-weak-ref-p object))

(defun rt-weak-pointer-value (weak-pointer)
  "Return WEAK-POINTER's referent, or NIL after the GC clears it."
  (rt-ref-get weak-pointer))

(defun (setf rt-weak-pointer-value) (value weak-pointer)
  "Retarget WEAK-POINTER to VALUE without creating a strong GC root."
  (check-type weak-pointer rt-weak-ref)
  (setf (rt-weak-ref-referent weak-pointer) value))

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

(defun %rt-gc-reference-value-address (heap value)
  "Return VALUE's heap address, or NIL for non-heap host values."
  (%rt-gc-pointer-address heap value))

(defun %rt-gc-reference-live-p (heap marked-set value)
  "True when VALUE is not managed by HEAP or its managed referent is live.

Host values are outside the cl-cc heap and are therefore not cleared by the
runtime GC.  Heap values are live when they are young/current large objects or
when their old-space header was marked by the major collector."
  (let ((addr (%rt-gc-reference-value-address heap value)))
    (or (null addr)
        (gethash addr marked-set))))

(defun %rt-gc-add-range-to-marked-set (heap marked-set start end predicate)
  "Add object starts in [START, END) satisfying PREDICATE to MARKED-SET."
  (loop with addr = start
        while (< addr end) do
          (let ((h (rt-heap-object-header heap addr)))
            (cond
              ((header-forwarding-p h) (incf addr 1))
              ((or (not (integerp h)) (zerop (rt-header-size h))) (return))
              (t
               (when (funcall predicate h)
                 (setf (gethash addr marked-set) t))
               (incf addr (rt-header-size h))))))
  marked-set)

(defun %rt-gc-build-marked-set (heap)
  "Build an address set from the completed major-GC mark state.

All allocated young objects are live for an old-generation collection.  Old
objects are live only when their mark bit is set; this function must run before
sweep clears those bits."
  (let ((marked-set (make-hash-table :test #'eql)))
    (%rt-gc-add-range-to-marked-set
     heap marked-set
     (rt-heap-young-from-base heap) (rt-heap-young-free heap)
     (lambda (h) (declare (ignore h)) t))
    (%rt-gc-add-range-to-marked-set
     heap marked-set
     (rt-heap-old-base heap) (rt-heap-old-free heap)
     #'header-marked-p)
    (%rt-gc-add-range-to-marked-set
     heap marked-set
     (rt-heap-large-obj-base heap) (rt-heap-large-obj-free heap)
     (lambda (h) (declare (ignore h)) t))
    marked-set))

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
  (when (>= (rt-heap-occupancy-pct heap) 80.0d0)
    (dolist (ref *rt-reference-registry*)
      (when (rt-soft-ref-p ref)
        (let ((referent (rt-soft-ref-referent ref)))
          (when (and referent
                     (not (%rt-gc-reference-live-p heap marked-set referent)))
            (%rt-reference-clear ref)))))))

(defun %rt-gc-process-weak-references (heap marked-set)
  "Clear weak references whose referents are NOT marked (unreachable).
   Weak refs are always cleared when their referent is unreachable."
  (dolist (ref *rt-reference-registry*)
    (when (rt-weak-ref-p ref)
      (let ((referent (rt-weak-ref-referent ref)))
        (when (and referent
                   (not (%rt-gc-reference-live-p heap marked-set referent)))
          (%rt-reference-clear ref))))))

(defun %rt-gc-process-phantom-references (heap marked-set)
  "Enqueue phantom references whose referents are collected.
   Phantom refs are enqueued after the referent is finalized and collected."
  (dolist (ref *rt-reference-registry*)
    (when (rt-phantom-ref-p ref)
      (let ((referent (rt-phantom-ref-referent ref)))
        (when (and referent
                   (not (%rt-gc-reference-live-p heap marked-set referent))
                   (not (rt-phantom-ref-enqueued ref)))
          (%rt-reference-clear ref))))))

(defun %rt-gc-process-ephemerons (heap marked-set)
  "Process ephemerons: mark values whose keys are marked.
   Uses fixed-point iteration because ephemerons can form chains:
   eph1.key → marked → eph1.value marked → (if eph2.key = eph1.value) → eph2.value marked"
  (dolist (eph *rt-ephemeron-registry*)
    (setf (rt-ephemeron-marked eph) nil))
  (loop with changed = t
        while changed
        do (setf changed nil)
           (dolist (eph *rt-ephemeron-registry*)
             (unless (rt-ephemeron-marked eph)
                (when (%rt-gc-reference-live-p heap marked-set (rt-ephemeron-key eph))
                  (setf (rt-ephemeron-marked eph) t)
                  (%rt-gc-mark-reference-value heap marked-set (rt-ephemeron-value eph))
                  (setf changed t))))))

(defun rt-gc-process-references (heap marked-set)
  "Process all reference types during major GC after the mark phase.
   Order matters: ephemerons first (may mark additional objects),
   then soft refs (pressure-dependent), weak refs, phantom refs."
  (%rt-gc-process-ephemerons heap marked-set)
  (%rt-gc-process-soft-references heap marked-set)
  (%rt-gc-process-weak-references heap marked-set)
  (%rt-gc-process-weak-hash-tables heap marked-set))

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

(defun %rt-gc-mark-reference-value (heap marked-set value)
  "Mark VALUE and its strong outgoing graph during ephemeron processing."
  (let ((addr (%rt-gc-reference-value-address heap value)))
    (when (and addr (not (gethash addr marked-set)))
      (setf (gethash addr marked-set) t)
      (when (and (rt-old-addr-p heap addr)
                 (fboundp '%rt-gc-grey-object)
                 (fboundp '%rt-gc-drain-major-mark-work))
        (let ((queue-cell (cons nil nil)))
          (%rt-gc-grey-object heap queue-cell addr)
          (%rt-gc-drain-major-mark-work heap queue-cell)
          ;; The drain marks transitively; refresh the visible set for weak
          ;; processing without consuming mark bits.
          (maphash (lambda (k v) (declare (ignore v))
                     (setf (gethash k marked-set) t))
                   (%rt-gc-build-marked-set heap))))))
  marked-set)

(defun %rt-gc-process-weak-hash-tables (heap marked-set)
  "Remove weak hash entries whose key/value liveness violates their weakness."
  (dolist (ht *rt-weak-hash-table-registry*)
    (when (rt-weak-hash-table-p ht)
      (let ((backing (rt-weak-hash-table-table ht))
            (entries (rt-weak-hash-table-entries ht))
            (weakness (rt-weak-hash-table-weakness ht))
            (dead-keys nil))
        (maphash
         (lambda (metadata-key entry)
           (let* ((key (rtwhe-key entry))
                  (value (rtwhe-value entry))
                  (key-live-p (%rt-gc-reference-live-p heap marked-set key))
                  (value-live-p (%rt-gc-reference-live-p heap marked-set value))
                  (remove-p
                    (case weakness
                      (:key (not key-live-p))
                      (:value (not value-live-p))
                      (:key-and-value (and (not key-live-p) (not value-live-p)))
                      (:key-or-value (or (not key-live-p) (not value-live-p)))
                      (otherwise nil))))
             (when remove-p
               (remhash key backing)
               (push metadata-key dead-keys))))
         entries)
        (dolist (key dead-keys)
          (remhash key entries)))))
  *rt-weak-hash-table-registry*)

(defun %rt-gc-forwarded-value-after-minor (heap value in-source-p)
  "Return (values NEW-VALUE LIVE-IN-SOURCE-P) for a weak referent after minor GC."
  (let ((addr (%rt-gc-value-address-for-predicate value in-source-p)))
    (if (null addr)
        (values value t)
        (let ((h (rt-heap-object-header heap addr)))
          (if (header-forwarding-p h)
              (values (%rt-gc-rebox-pointer-like value (header-forwarding-ptr h)) t)
              (values nil nil))))))

(defun rt-gc-process-weak-after-minor (heap in-source-p)
  "Update or clear weak references after young-generation evacuation."
  (dolist (ref *rt-reference-registry*)
      (when (rt-weak-ref-p ref)
        (let ((referent (rt-weak-ref-referent ref)))
          (when referent
            (multiple-value-bind (new live-p)
                (%rt-gc-forwarded-value-after-minor heap referent in-source-p)
              (if live-p
                  (setf (rt-weak-ref-referent ref) new)
                  (%rt-reference-clear ref)))))))
  (dolist (ht *rt-weak-hash-table-registry*)
      (when (rt-weak-hash-table-p ht)
        (let ((backing (rt-weak-hash-table-table ht))
              (entries (rt-weak-hash-table-entries ht))
              (dead-keys nil)
              (updates nil))
          (maphash
           (lambda (metadata-key entry)
             (multiple-value-bind (new-key key-live-p)
                 (%rt-gc-forwarded-value-after-minor heap (rtwhe-key entry) in-source-p)
               (multiple-value-bind (new-value value-live-p)
                   (%rt-gc-forwarded-value-after-minor heap (rtwhe-value entry) in-source-p)
                 (let* ((weakness (rt-weak-hash-table-weakness ht))
                        (remove-p
                          (case weakness
                            (:key (not key-live-p))
                            (:value (not value-live-p))
                            (:key-and-value (and (not key-live-p) (not value-live-p)))
                            (:key-or-value (or (not key-live-p) (not value-live-p)))
                            (otherwise nil))))
                   (if remove-p
                       (progn
                         (remhash (rtwhe-key entry) backing)
                         (push metadata-key dead-keys))
                       (progn
                         (setf (rtwhe-key entry) new-key
                               (rtwhe-value entry) new-value)
                         (push (list metadata-key new-key new-value entry) updates)))))))
           entries)
          (dolist (key dead-keys)
            (remhash key entries))
          (dolist (update updates)
            (destructuring-bind (old-key new-key new-value entry) update
              (unless (eql old-key new-key)
                (remhash old-key entries)
                (remhash old-key backing))
              (setf (gethash new-key entries) entry
                    (gethash new-key backing) new-value))))))
  heap)
