;;;; packages/vm/src/vm-ic.lisp - Inline Caching for Generic Functions (FR-009)
;;;;
;;;; Monomorphic inline cache: each generic-call site caches the last
;;;; (specializer-key method applicable-methods generation) tuple.  For
;;;; single-dispatch calls, SPECIALIZER-KEY is the single argument class name;
;;;; for multi-dispatch calls, it is the full argument class-name tuple.  Cache
;;;; hits bypass full method resolution.  Cache invalidation via per-GF
;;;; generation counter.

(in-package :cl-cc/vm)

;;; ---------------------------------------------------------------------------
;;; IC Infrastructure
;;; ---------------------------------------------------------------------------

(defun %ic-specializer-key (arg-regs state)
  "Compute a stable IC key from ARG-REGS.
Returns the single class name for one dispatch argument, or the class-name tuple
for multi-dispatch generic functions."
  (let ((classes (mapcar (lambda (reg) (vm-classify-arg (vm-reg-get state reg) state))
                         arg-regs)))
    (if (and classes (null (cdr classes)))
        (car classes)
        classes)))

(defun %ic-cacheable-gf-p (gf-ht)
  "T when GF-HT can use the simple primary-method inline cache.
Qualified methods and custom method combinations must run through full dispatch
so method ordering and combination semantics are preserved."
  (let ((has-qualified (or (gethash :__BEFORE__ gf-ht)
                           (gethash :__AFTER__ gf-ht)
                           (gethash :__AROUND__ gf-ht)))
        (combination (gethash :__method-combination__ gf-ht)))
    (and (not has-qualified)
         (or (null combination) (eq combination 'standard)))))

(defun %ic-cache-hit-p (cache key gen &optional satiated-p)
  "T if CACHE matches KEY and GEN.  CACHE = (key method methods gen).
When SATIATED-P is true, skip the generation comparison and treat the cache as
permanent for this GF."
  (and cache (listp cache) (>= (length cache) 4)
       (equal (nth 0 cache) key)
       (or satiated-p
           (eql (nth 3 cache) gen))))

(defun %ic-direct-call (method methods state pc labels dst-reg arg-regs gf-ht)
  "Execute a direct cached call to METHOD, pushing method-call-stack for
   call-next-method support."
  (let ((method-fn (%vm-method-function method))
        (all-arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs)))
    (vm-push-call-frame state (1+ pc) dst-reg)
    (push (list gf-ht methods all-arg-values)
          (vm-method-call-stack state))
    (vm-profile-enter-call state (vm-closure-entry-label method-fn))
    (vm-bind-closure-args method-fn state all-arg-values)
    (values (vm-label-table-lookup labels (vm-closure-entry-label method-fn))
            nil nil)))

;;; ---------------------------------------------------------------------------
;;; Execute-Instruction integration (called from vm-dispatch-gf-call)
;;; ---------------------------------------------------------------------------

(defun %ic-lookup-or-dispatch (gf-ht state pc arg-regs dst-reg labels &optional inst)
  "FR-009: Check monomorphic inline cache for GF-HT.  On hit, bypass full
   dispatch.  On miss, perform dispatch and update cache.  INST, when supplied,
   must be the vm-generic-call instruction that owns the cache slot."
  (let* ((gf-gen (gethash '__ic-gen__ gf-ht 0))
          (satiated-p (gethash :__satiated__ gf-ht))
          (cache (when (typep inst 'vm-generic-call)
                   (vm-ic-cache inst)))
         (key (%ic-specializer-key arg-regs state)))
    ;; Check cache
    (when (and (%ic-cacheable-gf-p gf-ht)
               (%ic-cache-hit-p cache key gf-gen satiated-p))
      (return-from %ic-lookup-or-dispatch
        (%ic-direct-call (nth 1 cache) (nth 2 cache) state pc labels dst-reg arg-regs gf-ht)))
    ;; Full dispatch
    (multiple-value-bind (next-pc halt-p result)
        (vm-dispatch-generic-call gf-ht state pc arg-regs dst-reg labels)
      ;; Update cache (only primary methods, no qualifiers)
      (when (and (typep inst 'vm-generic-call)
                 (%ic-cacheable-gf-p gf-ht))
        (let* ((vals (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
                (methods (vm-get-all-applicable-methods gf-ht state vals))
                (primary (car methods)))
          (when primary
            (setf (vm-ic-cache inst) (list key primary methods gf-gen)))))
      (values next-pc halt-p result))))
