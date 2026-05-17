;;;; packages/vm/src/vm-ic.lisp - Inline Caching for Generic Functions (FR-009)
;;;;
;;;; Monomorphic inline cache: each generic-call site caches the last
;;;; (specializer-key . method-closure) pair.  Cache hits bypass full
;;;; method resolution.  Cache invalidation via per-GF generation counter.

(in-package :cl-cc/vm)

;;; ---------------------------------------------------------------------------
;;; IC Infrastructure
;;; ---------------------------------------------------------------------------

(defun %ic-specializer-key (arg-regs state)
  "Compute a stable monomorphic cache key from the types of ARG-REGS."
  (mapcar (lambda (reg) (vm-classify-arg (vm-reg-get state reg) state))
          arg-regs))

(defun %ic-cache-hit-p (cache key gen)
  "T if CACHE matches KEY and GEN.  CACHE = (key method gen)."
  (and cache (listp cache) (>= (length cache) 3)
       (equal (nth 0 cache) key)
       (eql (nth 2 cache) gen)))

(defun %ic-direct-call (method state pc labels dst-reg arg-regs gf-ht)
  "Execute a direct cached call to METHOD, pushing method-call-stack for
   call-next-method support."
  (let ((all-arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs)))
    (vm-push-call-frame state (1+ pc) dst-reg)
    (push (list gf-ht (list method) all-arg-values)
          (vm-method-call-stack state))
    (vm-profile-enter-call state (vm-closure-entry-label method))
    (vm-bind-closure-args method state all-arg-values)
    (values (vm-label-table-lookup labels (vm-closure-entry-label method))
            nil nil)))

;;; ---------------------------------------------------------------------------
;;; Execute-Instruction integration (called from vm-dispatch-gf-call)
;;; ---------------------------------------------------------------------------

(defun %ic-lookup-or-dispatch (gf-ht state pc arg-regs dst-reg labels)
  "FR-009: Check monomorphic inline cache for GF-HT.  On hit, bypass full
   dispatch.  On miss, perform dispatch and update cache."
  (let* ((gf-gen (gethash '__ic-gen__ gf-ht 0))
         (inst (vm-current-instruction state))  ; get the vm-generic-call inst
         (cache (when (typep inst 'vm-generic-call)
                  (vm-ic-cache inst)))
         (key (%ic-specializer-key arg-regs state)))
    ;; Check cache
    (when (%ic-cache-hit-p cache key gf-gen)
      (return-from %ic-lookup-or-dispatch
        (%ic-direct-call (nth 1 cache) state pc labels dst-reg arg-regs gf-ht)))
    ;; Full dispatch
    (multiple-value-bind (next-pc halt-p result)
        (vm-dispatch-generic-call gf-ht state pc arg-regs dst-reg labels)
      (declare (ignore halt-p result))
      ;; Update cache (only primary methods, no qualifiers)
      (when (typep inst 'vm-generic-call)
        (let* ((vals (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
               (methods (vm-get-all-applicable-methods gf-ht state vals))
               (primary (car methods)))
          (when primary
            (setf (vm-ic-cache inst) (list key primary gf-gen)))))
      (values next-pc halt-p result))))