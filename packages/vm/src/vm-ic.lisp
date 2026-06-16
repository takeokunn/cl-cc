;;;; packages/vm/src/vm-ic.lisp - Inline Caching for Generic Functions (FR-009)
;;;;
;;;; Monomorphic inline cache: each generic-call site caches the last
;;;; (specializer-key generation . method-closure) entry.  SPECIALIZER-KEY is
;;;; the list of argument class names, and GENERATION mirrors GF method cache
;;;; versions.  Cache hits bypass full method resolution.  Cache invalidation
;;;; clears registered generic-call site caches when methods are registered.

(in-package :cl-cc/vm)

;;; ---------------------------------------------------------------------------
;;; IC Infrastructure
;;; ---------------------------------------------------------------------------

(defvar *collect-profile* nil
  "T enables FR-058 type-feedback collection for inline-cache dispatch sites.")

(defvar *profile-output* nil
  "Optional stream or pathname used by RUN-COMPILED to dump FR-058 type profiles.")

(defparameter +ic-pgo-dominance-threshold+ 0.9
  "Minimum single-type ratio required before PGO specializes a generic-call site.")

(defun %ic-specializer-key (arg-regs state)
  "Compute a stable IC key from ARG-REGS.
Returns a list of argument class names for single- and multi-dispatch generic
functions."
  (mapcar (lambda (reg) (vm-classify-arg (vm-reg-get state reg) state))
          arg-regs))

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

(defun %ic-gf-generation (gf-ht)
  "Return the generation tuple used to invalidate GF-HT inline-cache entries."
  (list (or (gethash '__ic-gen__ gf-ht) 0)
        (or (gethash :__cache-version__ gf-ht) 0)))

(defun %ic-cache-entry (key generation method)
  "Build a monomorphic inline-cache entry for KEY at GENERATION."
  (cons key (cons generation method)))

(defun %ic-cache-hit-method (cache key generation)
  "Return the cached method when CACHE matches KEY and GENERATION.
CACHE = (specializer-key generation . method-closure)."
  (and cache
       (consp cache)
       (equal (car cache) key)
       (let ((payload (cdr cache)))
         (and (consp payload)
              (equal (car payload) generation)
              (cdr payload)))))

(defun %ic-type-counter-table (inst)
  "Return INST's mutable type counter table, creating it on demand."
  (or (vm-ic-type-counters inst)
      (setf (vm-ic-type-counters inst) (make-hash-table :test #'equal))))

(defun %ic-record-type-feedback (inst state pc key)
  "Record one FR-058 type-feedback hit for generic-call INST at PC and KEY."
  (when (and (typep inst 'vm-generic-call)
             (or *collect-profile* (%vm-profile-enabled-p state)))
    (incf (gethash key (%ic-type-counter-table inst) 0))
    (incf (gethash (list :generic-call pc key)
                    (vm-get-profile-type-feedback state)
                    0))))

(defun %ic-record-cache-hit (inst state pc key &optional (kind :ic-hit))
  "Record one FR-058 inline-cache fast-path hit for INST at PC and KEY."
  (when (typep inst 'vm-generic-call)
    (incf (vm-ic-hit-count inst))
    (when (or *collect-profile* (%vm-profile-enabled-p state))
      (incf (gethash (list kind pc key)
                     (vm-get-profile-type-feedback state)
                     0)))))

(defun %ic-record-cache-miss (inst state pc key)
  "Record one FR-058 inline-cache miss for INST at PC and KEY."
  (when (typep inst 'vm-generic-call)
    (incf (vm-ic-miss-count inst))
    (when (or *collect-profile* (%vm-profile-enabled-p state))
      (incf (gethash (list :ic-miss pc key)
                     (vm-get-profile-type-feedback state)
                     0)))))

(defun %ic-pgo-specializer-hit-p (inst key)
  "T when INST has a PGO-selected specializer matching KEY."
  (and (typep inst 'vm-generic-call)
       (let ((specializer (vm-pgo-specializer inst)))
         (and specializer (equal specializer key)))))

(defun %ic-primary-method-for-key (gf-ht state arg-regs)
  "Resolve the primary method for the current ARG-REGS without changing dispatch semantics."
  (let* ((vals (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
         (methods (vm-get-all-applicable-methods gf-ht state vals)))
    (car methods)))

(defun %ic-register-site (gf-ht inst)
  "Register INST as an inline-cache site owned by GF-HT."
  (when (and (hash-table-p gf-ht) (typep inst 'vm-generic-call))
    (pushnew inst (gethash :__ic-sites__ gf-ht) :test #'eq)))

(defun %ic-clear-gf-caches (gf-ht)
  "Clear all registered inline-cache sites for GF-HT."
  (when (hash-table-p gf-ht)
    (dolist (site (gethash :__ic-sites__ gf-ht))
      (when (typep site 'vm-generic-call)
        (setf (vm-ic-cache site) nil)
        (setf (vm-pgo-specializer site) nil)))))

(defun %ic-clear-all-generic-caches (state)
  "Clear inline caches for all generic functions reachable from STATE."
  (let ((seen (make-hash-table :test #'eq)))
    (flet ((clear-value (value)
             (when (and (vm-generic-function-p value)
                        (not (gethash value seen)))
               (setf (gethash value seen) t)
               (%ic-clear-gf-caches value))))
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (clear-value v))
               (vm-function-registry state))
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (clear-value v))
               (vm-global-vars state)))))

(defun %ic-direct-call (method state pc labels dst-reg arg-regs gf-ht)
  "Execute a direct cached call to METHOD, pushing method-call-stack for
call-next-method context."
  (let ((method-fn (%vm-method-function method))
        (all-arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs)))
    (vm-push-call-frame state (1+ pc) dst-reg)
    (push (list gf-ht (list method) all-arg-values)
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
  (let* ((generation (%ic-gf-generation gf-ht))
         (cache (when (typep inst 'vm-generic-call)
                   (%ic-register-site gf-ht inst)
                   (vm-ic-cache inst)))
          (key (%ic-specializer-key arg-regs state)))
    (%ic-record-type-feedback inst state pc key)
    ;; PGO-specialized type check + direct-call path.  The type check is the KEY
    ;; comparison; uncommon types fall back to the normal IC/full-dispatch path.
    (when (and (%ic-cacheable-gf-p gf-ht)
                (%ic-pgo-specializer-hit-p inst key))
      (let ((primary (%ic-primary-method-for-key gf-ht state arg-regs)))
        (when primary
          (%ic-record-cache-hit inst state pc key :pgo-hit)
          (setf (vm-ic-cache inst) (%ic-cache-entry key generation primary))
          (return-from %ic-lookup-or-dispatch
            (%ic-direct-call primary state pc labels dst-reg arg-regs gf-ht)))))
    ;; Check cache
    (let ((cached-method (and (%ic-cacheable-gf-p gf-ht)
                              (%ic-cache-hit-method cache key generation))))
      (when cached-method
        (%ic-record-cache-hit inst state pc key)
        (return-from %ic-lookup-or-dispatch
          (%ic-direct-call cached-method state pc labels dst-reg arg-regs gf-ht))))
    (%ic-record-cache-miss inst state pc key)
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
             (setf (vm-ic-cache inst) (%ic-cache-entry key generation primary)))))
      (values next-pc halt-p result))))

(defun vm-type-profile-alist (state)
  "Return collected FR-058 type feedback as an alist of ((:generic-call pc key) . count)."
  (let ((rows nil))
    (maphash (lambda (key count) (push (cons key count) rows))
             (vm-get-profile-type-feedback state))
    (sort rows #'< :key (lambda (row) (or (second (car row)) 0)))))

(defun vm-dump-type-profile (state &optional (stream *standard-output*))
  "Dump collected FR-058 type feedback for STATE as a readable PGO fragment."
  (format stream "(:format :cl-cc-pgo-v1~% :type-feedback (~%")
  (dolist (row (vm-type-profile-alist state))
    (format stream "   (~S . ~D)~%" (car row) (cdr row)))
  (format stream " ))~%"))

(defun vm-maybe-dump-type-profile (state)
  "Dump type feedback after execution when *COLLECT-PROFILE* requests it."
  (when (and *collect-profile* *profile-output*)
    (etypecase *profile-output*
      (stream (vm-dump-type-profile state *profile-output*))
      ((or string pathname)
       (ensure-directories-exist *profile-output*)
       (with-open-file (out *profile-output*
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
         (vm-dump-type-profile state out))))))
