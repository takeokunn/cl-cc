(in-package :cl-cc/runtime)

(defun %rt-gc-valid-header-p (header)
  (and (integerp header)
       (> (rt-header-size header) 0)
       (<= 0 (rt-header-type-tag header) 7)))

(defun %rt-gc-object-start-p (heap addr)
  "Return true when ADDR appears to designate a live heap object header."
  (and (integerp addr)
       (rt-heap-addr-p heap addr)
       (let ((header (rt-heap-object-header heap addr)))
         (or (header-forwarding-p header)
             (%rt-gc-valid-header-p header)))))

;;; FR-336: GC-NaN-Boxing Integration — uses val-pointer-p/decode-pointer for precise pointer identification during GC scanning
(defun %rt-gc-pointer-address (heap value)
  "Return the heap address encoded by VALUE, or NIL when VALUE is not a pointer.

NaN-boxed pointer values are recognized with VAL-POINTER-P and decoded with
DECODE-POINTER.  Runtime heap words that already contain object-start addresses
are accepted as canonical internal addresses."
  (cond
    ((and (typep value '(unsigned-byte 64)) (val-pointer-p value))
     (let ((addr (decode-pointer value)))
       (when (%rt-gc-object-start-p heap addr)
         addr)))
    ((%rt-gc-object-start-p heap value)
     value)
    (t nil)))

(defun %rt-gc-value-address-for-predicate (value predicate)
  "Return VALUE's decoded address when PREDICATE accepts it.

This is used during minor GC after from/to-space flipping, where the evacuation
source is no longer considered a live heap range by RT-HEAP-ADDR-P."
  (cond
    ((and (typep value '(unsigned-byte 64)) (val-pointer-p value))
     (let ((addr (decode-pointer value)))
       (when (funcall predicate addr) addr)))
    ((and (integerp value) (funcall predicate value))
     value)
    (t nil)))

(defun %rt-gc-rebox-pointer-like (old-value new-addr)
  "Preserve OLD-VALUE's pointer representation while replacing its address."
  (if (and (integerp old-value) (val-pointer-p old-value))
      (encode-pointer new-addr (pointer-tag old-value))
      new-addr))

(defun %rt-gc-root-type (heap root-cell)
  (or (cdr (assoc root-cell (gethash heap *rt-gc-root-types*) :test #'eq))
      :any))

(defun %rt-gc-root-pointer-address (heap root-cell)
  "Return ROOT-CELL's heap address according to its precise root metadata."
  (let ((value (cdr root-cell)))
    (case (%rt-gc-root-type heap root-cell)
      ((:pointer :any) (%rt-gc-pointer-address heap value))
      ((:fixnum :double :char) nil)
      (otherwise nil))))

(defun %rt-gc-thread-words (thread-state)
  "Return a list of conservative stack words described by THREAD-STATE."
  (cond
    ((null thread-state) nil)
    ((vm-frame-p thread-state)
     (loop for value across (vm-frame-registers thread-state) collect value))
    ((vectorp thread-state)
     (loop for value across thread-state collect value))
    ((and (consp thread-state)
          (or (getf thread-state :stack) (getf thread-state :frames)))
     (append (copy-list (getf thread-state :stack))
             (loop for frame in (getf thread-state :frames)
                   append (%rt-gc-thread-words frame))))
    ((listp thread-state) thread-state)
    (t nil)))

(defun rt-gc-register-stackmap (frame-id slots &key (source :compiler-stub))
  "Register precise stack map SLOTS for FRAME-ID.

SLOTS has the compiler-facing shape ((FRAME-OFFSET . :OBJECT) ...).  Non-object
slot kinds are accepted but ignored by root scanning."
  (let ((stackmap (make-rt-stackmap :frame-id frame-id
                                    :slots (copy-list slots)
                                    :source source)))
    (setf (gethash frame-id *rt-gc-stackmap-table*) stackmap)
    stackmap))

(defun rt-gc-stackmap-for-frame (frame)
  "Return stack-map metadata for FRAME, if any."
  (let ((frame-id (cond
                    ((and (consp frame) (getf frame :stackmap-id)) (getf frame :stackmap-id))
                    ((and (consp frame) (getf frame :frame-id)) (getf frame :frame-id))
                    ((vm-frame-p frame) (vm-frame-closure frame))
                    (t nil))))
    (and frame-id (gethash frame-id *rt-gc-stackmap-table*))))

(defun rt-gc-generate-stackmap (frame-id live-object-offsets)
  "FR-541: Generate and register a precise GC stack map for FRAME-ID.
LIVE-OBJECT-OFFSETS is a list of frame offsets containing live object pointers.
The stack map is registered into *rt-gc-stackmap-table* for use by the
precise garbage collector. Source tag distinguishes compiler-generated
maps from manually registered ones."
  (rt-gc-register-stackmap
   frame-id
   (mapcar (lambda (offset) (cons offset :object)) live-object-offsets)
   :source :compiler))

(defun %rt-gc-frame-slot-value (frame offset)
  "Read OFFSET from FRAME for precise stack-map scanning."
  (cond
    ((vm-frame-p frame) (aref (vm-frame-registers frame) offset))
    ((vectorp frame) (aref frame offset))
    ((and (consp frame) (getf frame :slots)) (cdr (assoc offset (getf frame :slots))))
    ((and (consp frame) (getf frame :registers)) (cdr (assoc offset (getf frame :registers))))
    ((listp frame) (nth offset frame))
    (t nil)))

(defun (setf %rt-gc-frame-slot-value) (value frame offset)
  "Write VALUE to OFFSET in FRAME when FRAME supports precise updates."
  (cond
    ((vm-frame-p frame) (setf (aref (vm-frame-registers frame) offset) value))
    ((vectorp frame) (setf (aref frame offset) value))
    ((and (consp frame) (getf frame :slots))
     (let ((cell (assoc offset (getf frame :slots))))
       (if cell (setf (cdr cell) value) (push (cons offset value) (getf frame :slots)))))
    ((and (consp frame) (getf frame :registers))
     (let ((cell (assoc offset (getf frame :registers))))
       (if cell (setf (cdr cell) value) (push (cons offset value) (getf frame :registers))))))
  value)

(defun rt-gc-scan-stackmap-frame (heap frame)
  "Return heap object addresses in FRAME described by its precise stack map."
  (let ((stackmap (rt-gc-stackmap-for-frame frame)))
    (remove-duplicates
     (loop for (offset . kind) in (and stackmap (rt-stackmap-slots stackmap))
           when (eq kind :object)
             append (let ((addr (%rt-gc-pointer-address
                                 heap (%rt-gc-frame-slot-value frame offset))))
                      (and addr (list addr))))
     :test #'eql)))

(defun rt-gc-scan-stackmaps (heap)
  "Return precise stack-map roots for all registered thread frames."
  (remove-duplicates
   (loop for thread-state in *gc-threads*
         for frames = (and (consp thread-state) (getf thread-state :frames))
         append (loop for frame in frames append (rt-gc-scan-stackmap-frame heap frame)))
   :test #'eql))

(defun rt-gc-update-stackmap-frame (heap frame address-mapper)
  "Update object slots in FRAME using ADDRESS-MAPPER for moving GC integration." 
  (declare (ignore heap))
  (let ((stackmap (rt-gc-stackmap-for-frame frame)))
    (dolist (slot (and stackmap (rt-stackmap-slots stackmap)) frame)
      (destructuring-bind (offset . kind) slot
        (when (eq kind :object)
          (let* ((old (%rt-gc-frame-slot-value frame offset))
                 (new (funcall address-mapper old)))
            (when new
              (setf (%rt-gc-frame-slot-value frame offset) new))))))))

(defun %rt-gc-thread-binding-stack (thread)
  "Return THREAD's dynamic binding stack, if any."
  (cond
    ((and (consp thread) (getf thread :bindings)) (getf thread :bindings))
    ((boundp '*rt-dynamic-binding-stacks*)
     (gethash (%rt-gc-thread-id thread) *rt-dynamic-binding-stacks*))
    (t nil)))

(defun %rt-gc-binding-symbol (binding)
  (cond
    ((and (consp binding) (getf binding :symbol)) (getf binding :symbol))
    ((consp binding) (car binding))
    (t nil)))

(defun %rt-gc-binding-value (binding)
  (cond
    ((and (consp binding) (getf binding :value)) (getf binding :value))
    ((consp binding) (cdr binding))
    (t nil)))

(defun %rt-gc-set-binding-value (binding value)
  (cond
    ((and (consp binding) (getf binding :value))
     (setf (getf binding :value) value))
    ((consp binding)
     (setf (cdr binding) value)))
  binding)

(defun rt-gc-scan-binding-stack (heap thread)
  "Return heap pointer addresses found in THREAD's dynamic binding stack.

Bindings are treated as additional GC roots.  Bindings for global-only special
variables are skipped because those symbols never use thread-local dynamic
storage and are traced via the global/runtime registries instead."
  (check-type heap rt-heap)
  (remove-duplicates
   (loop for binding in (%rt-gc-thread-binding-stack thread)
         for sym = (%rt-gc-binding-symbol binding)
         for skip = (and sym
                         (fboundp 'rt-special-variable-global-only-p)
                         (rt-special-variable-global-only-p sym))
         for addr = (and (not skip)
                         (%rt-gc-pointer-address heap (%rt-gc-binding-value binding)))
         when addr collect addr)
   :test #'eql))

(defun %rt-gc-scan-binding-stacks (heap)
  "Return heap pointer addresses from all registered dynamic binding stacks."
  (remove-duplicates
   (loop for thread-state in *gc-threads*
         append (rt-gc-scan-binding-stack heap thread-state))
   :test #'eql))

(defun %rt-gc-global-variable-bindings ()
  "Return (SYMBOL . VALUE) bindings from the runtime global special registry."
  (when (boundp '*rt-global-var-registry*)
    (let (bindings)
      (maphash (lambda (sym value) (push (cons sym value) bindings))
               *rt-global-var-registry*)
      bindings)))

(defun %rt-gc-scan-global-variables (heap)
  "Return heap pointer addresses held by global-only special variables."
  (remove-duplicates
   (loop for binding in (%rt-gc-global-variable-bindings)
          for addr = (%rt-gc-pointer-address heap (cdr binding))
         when addr collect addr)
   :test #'eql))

(defun %rt-gc-conservative-root-addresses (heap)
  "Return stack words that conservatively look like valid heap object pointers."
  (when *gc-conservative-roots*
    (remove-duplicates
     (loop for thread-state in *gc-threads*
           append (loop for word in (%rt-gc-thread-words thread-state)
                         for addr = (%rt-gc-pointer-address heap word)
                        when addr collect addr))
     :test #'eql)))

(defun rt-gc-verify-heap (heap)
  "Verify basic heap invariants and signal an error on corruption."
  (labels ((verify-range (start end evacuated-p)
             (loop with addr = start
                   while (< addr end) do
                     (let ((h (rt-heap-object-header heap addr)))
                       (when (and evacuated-p (header-forwarding-p h))
                         (error "GC verify: forwarding pointer remains at ~D" addr))
                       (cond
                         ((header-forwarding-p h)
                          (if evacuated-p
                              (error "GC verify: forwarding pointer remains at ~D" addr)
                              (incf addr 1)))
                         ((and (integerp h) (zerop h)) (return))
                         ((not (%rt-gc-valid-header-p h))
                          (error "GC verify: invalid header at ~D: ~S" addr h))
                          (t
                           (let ((size (rt-header-size h)))
                             (when (> (+ addr size) end)
                               (error "GC verify: object at ~D exceeds range" addr))
                             (when (or (header-marked-p h) (header-gray-p h))
                               (error "GC verify: mark/gray bits not cleared at ~D" addr))
                             (dolist (offset (rt-object-pointer-slots heap addr))
                               (let ((value (rt-heap-ref heap (+ addr offset))))
                                 (when (and (integerp value) (val-pointer-p value))
                                   (let ((target (decode-pointer value)))
                                     (unless (%rt-gc-object-start-p heap target)
                                       (error "GC verify: invalid boxed pointer ~S at ~D+~D"
                                              value addr offset))))
                                 (when (and (integerp value)
                                            (not (val-pointer-p value))
                                            (rt-heap-addr-p heap value)
                                            (not (%rt-gc-object-start-p heap value)))
                                   (error "GC verify: invalid raw pointer ~S at ~D+~D"
                                          value addr offset))))
                             (incf addr size))))))))
    (verify-range (rt-heap-young-from-base heap) (rt-heap-young-free heap) nil)
    (verify-range (rt-heap-old-base heap) (rt-heap-old-free heap) t)
    (verify-range (rt-heap-large-obj-base heap) (rt-heap-large-obj-free heap) t)
    t))

;; Ensure the card-size constant is present even if this file is compiled or
;; loaded in isolation during incremental builds.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '*gc-tenuring-threshold*)
    (defparameter *gc-tenuring-threshold* 3
      "Minor GC survival cycles before promotion to old generation."))
  (unless (boundp '+gc-card-size-words+)
    (defconstant +gc-card-size-words+ 64
      "Card size in words (512 bytes with 8-byte words).")))

;;; ------------------------------------------------------------
;;; Section 1: Allocation
;;; ------------------------------------------------------------

(defun %rt-gc-old-space-allocation-p (type-tag)
  "Return true when TYPE-TAG explicitly requests old-generation allocation."
  (member type-tag '(:old :old-space :old-generation :tenured) :test #'eq))

(defun %rt-gc-alloc-old-bump (heap size-words)
  "Allocate SIZE-WORDS words from old-space using the bump pointer."
  (let* ((addr (rt-heap-old-free heap))
         (limit (+ (rt-heap-old-base heap) (rt-heap-old-size heap))))
    (when (> (+ addr size-words) limit)
      (if (fboundp 'rt-signal-oom)
          (rt-signal-oom size-words :heap heap :limit-words limit :used-words addr)
          (error "cl-cc/runtime: old space exhausted — ~D words requested" size-words)))
    (%rt-ensure-compressed-pointer-range addr size-words)
    (setf (rt-heap-old-free heap) (+ addr size-words))
    addr))

(defun rt-gc-alloc (heap type-tag size-words)
  "Allocate SIZE-WORDS words in young from-space and return the word address.
   TYPE-TAG selects allocation policy/slab class.  Object headers are FR-266
   compressed one-word headers built with MAKE-RT-HEADER, but the
   header is NOT written here; the caller is responsible for writing it
   immediately after allocation, optionally embedding an FR-214 shape id.
   Automatically triggers a minor GC if from-space is exhausted.
   Signals an error if the heap is exhausted even after GC."
  (rt-gc-safepoint-check heap :kind :allocation)
  (rt-gc-pacer-maybe-throttle heap size-words)
  (%rt-gc-enforce-heap-limit heap size-words)
  (incf (rt-heap-total-alloc-words heap) size-words)
  (%rt-gc-note-allocation-rate heap)
  (dolist (hook *rt-gc-alloc-hooks*)
    (funcall hook heap size-words))
  ;; The allocator returns an uninitialized object whose compressed header is
  ;; written by the caller.  Stress mode therefore collects immediately before
  ;; each allocation, exercising GC on every allocation boundary without
  ;; evacuating a headerless object that has not yet been installed by the caller.
  (when (and *gc-stress-mode*
             (not (rt-heap-gc-inhibit heap))
             (eq (rt-heap-gc-state heap) :normal)
             (> (rt-heap-young-free heap) (rt-heap-young-from-base heap)))
    (rt-gc-minor-collect heap))
  (let ((slab-class (and *rt-use-slab-allocator*
                          (rt-slab-size-class type-tag size-words))))
    (cond
     (slab-class
      ;; Try slab allocation first; fall back to bump-pointer if exhausted
       (handler-case
           (let ((addr (rt-slab-alloc heap slab-class)))
             (%rt-ensure-compressed-pointer-range addr size-words)
             (rt-gc-profile-sample (* size-words 8))
             addr)
        (error ()
          ;; Slab exhausted — fall through to bump-pointer allocator
          nil)))
    ;;; FR-086: Large Object Space (LOS) — objects exceeding threshold bypass nursery; allocated directly in large-object space
    ((> size-words (rt-heap-large-obj-threshold heap))
      (let* ((addr (rt-heap-large-obj-free heap))
             (limit (+ (rt-heap-large-obj-base heap)
                       (rt-heap-large-obj-size heap))))
        (when (> (+ addr size-words) limit)
          (if (fboundp 'rt-signal-oom)
              (rt-signal-oom size-words :heap heap :limit-words limit :used-words addr)
              (error "cl-cc/runtime: large object space exhausted — ~D words requested" size-words)))
        (%rt-ensure-compressed-pointer-range addr size-words)
        (setf (rt-heap-large-obj-free heap) (+ addr size-words))
        (rt-gc-profile-sample (* size-words 8))
        addr))
    ((%rt-gc-old-space-allocation-p type-tag)
      (multiple-value-bind (class-size free-list-addr)
          (rt-free-list-alloc-from-bin heap size-words)
        (let* ((alloc-size (or class-size size-words))
               (addr (or free-list-addr
                         (%rt-gc-alloc-old-bump heap alloc-size))))
          (rt-gc-profile-sample (* alloc-size 8))
          addr)))
    (t
      (let* ((from-base (rt-heap-young-from-base heap))
             (semi-size (rt-heap-young-semi-size heap))
             (limit     (+ from-base semi-size))
             (addr      (rt-heap-young-free heap)))
        (when (> (+ addr size-words) limit)
          (if (rt-heap-gc-inhibit heap)
              (setf (rt-heap-gc-pending heap) t)
              (rt-gc-minor-collect heap))
          ;; Recompute after GC (bases may have flipped)
          (setf from-base (rt-heap-young-from-base heap)
                limit     (+ from-base semi-size)
                addr      (rt-heap-young-free heap))
          (when (> (+ addr size-words) limit)
            (if (fboundp 'rt-signal-oom)
                (rt-signal-oom size-words :heap heap :limit-words limit :used-words addr)
                (error "cl-cc/runtime: heap exhausted — ~D words requested, ~D words available in young space"
                       size-words (- limit addr)))))
        (%rt-ensure-compressed-pointer-range addr size-words)
        (setf (rt-heap-young-free heap) (+ addr size-words))
        (rt-gc-profile-sample (* size-words 8))
        addr)))))

;;; ------------------------------------------------------------
;;; Section 2: Root Registration
;;; ------------------------------------------------------------

(defun rt-gc-add-root (heap root-cell)
  "Register ROOT-CELL as a GC root.
   ROOT-CELL must be a cons whose cdr holds the heap address to keep live.
   The GC updates (cdr root-cell) in place when the object is moved."
  (rt-gc-add-root-typed heap root-cell :any))

(defun rt-gc-add-root-typed (heap root-cell type)
  "Register ROOT-CELL as a GC root with precise TYPE metadata.

  TYPE is one of :POINTER, :FIXNUM, :DOUBLE, :CHAR, or :ANY.  :ANY accepts either
  a boxed pointer value or a canonical internal heap address; typed non-pointer
  roots are skipped entirely by the collectors."
  (check-type root-cell cons)
  (unless (member type '(:pointer :fixnum :double :char :any) :test #'eq)
    (error "cl-cc/runtime: invalid GC root type ~S" type))
  (pushnew root-cell (rt-heap-roots heap) :test #'eq)
  (let ((alist (gethash heap *rt-gc-root-types*)))
    (setf (gethash heap *rt-gc-root-types*)
          (acons root-cell type (delete root-cell alist :key #'car :test #'eq))))
  root-cell)

(defun rt-gc-remove-root (heap root-cell)
  "Unregister ROOT-CELL from the GC root set."
  (setf (rt-heap-roots heap)
        (delete root-cell (rt-heap-roots heap) :test #'eq))
  (let ((alist (gethash heap *rt-gc-root-types*)))
    (setf (gethash heap *rt-gc-root-types*)
          (delete root-cell alist :key #'car :test #'eq)))
  root-cell)

;;; ------------------------------------------------------------
;;; Section 3: Minor GC — Cheney Copying
;;; ------------------------------------------------------------
