;;;; optimizer-pipeline-speculative.lisp — IC/speculation/profiling/lattice helpers
(in-package :cl-cc/optimize)

(defun opt-ic-transition (site receiver-key target)
  "Record RECEIVER-KEY → TARGET in SITE and return SITE.
State transitions follow uninitialized → monomorphic → polymorphic → megamorphic."
  (incf (opt-ic-site-misses site))
  (let ((existing (assoc receiver-key (opt-ic-site-entries site) :test #'equal)))
    (if existing
        (setf (cdr existing) target)
        (push (cons receiver-key target) (opt-ic-site-entries site))))
  (let ((n (length (opt-ic-site-entries site))))
    (setf (opt-ic-site-state site)
          (cond ((= n 0) :uninitialized)
                ((= n 1) :monomorphic)
                ((<= n (opt-ic-site-max-polymorphic-entries site)) :polymorphic)
                (t :megamorphic)))
    (when (eq (opt-ic-site-state site) :megamorphic)
      (setf (opt-ic-site-megamorphic-fallback site)
            (copy-list (opt-ic-site-entries site)))))
  site)

(defun %opt-mega-cache-touch (cache key)
  (setf (opt-mega-cache-order cache)
        (cons key (remove key (opt-mega-cache-order cache) :test #'equal))))

(defun %opt-mega-cache-evict-if-needed (cache)
  (let ((limit (opt-mega-cache-max-size cache)))
    (when (and (plusp limit)
               (> (length (opt-mega-cache-order cache)) limit))
      (let ((victim (car (last (opt-mega-cache-order cache)))))
        (setf (opt-mega-cache-order cache)
              (butlast (opt-mega-cache-order cache) 1))
        (remhash victim (opt-mega-cache-entries cache))))))

(defun opt-mega-cache-put (cache receiver-key target)
  "Insert RECEIVER-KEY -> TARGET into CACHE with simple LRU eviction."
  (setf (gethash receiver-key (opt-mega-cache-entries cache)) target)
  (%opt-mega-cache-touch cache receiver-key)
  (%opt-mega-cache-evict-if-needed cache)
  target)

(defun opt-mega-cache-get (cache receiver-key)
  "Lookup RECEIVER-KEY in CACHE. Returns (values target found-p)."
  (multiple-value-bind (target found-p)
      (gethash receiver-key (opt-mega-cache-entries cache))
    (when found-p
      (%opt-mega-cache-touch cache receiver-key))
    (values target found-p)))

(defun opt-ic-resolve-target (site receiver-key &optional megamorphic-cache)
  "Resolve dispatch target for RECEIVER-KEY from SITE and optional shared cache.

Lookup order:
  1) site-local IC entries
  2) shared megamorphic cache (only when SITE is :megamorphic)

Returns (values target source-keyword), where source is one of
  :site-local, :megamorphic-shared, or :miss."
  (let ((local (assoc receiver-key (opt-ic-site-entries site) :test #'equal)))
    (when local
      (return-from opt-ic-resolve-target (values (cdr local) :site-local))))
  (when (and megamorphic-cache
             (eq (opt-ic-site-state site) :megamorphic))
    (multiple-value-bind (target found-p)
        (opt-mega-cache-get megamorphic-cache receiver-key)
      (when found-p
        (return-from opt-ic-resolve-target (values target :megamorphic-shared)))))
  (values nil :miss))

(defun opt-ic-make-patch-plan (site-id old-state new-state target)
  "Build a conservative IC patch plan from OLD-STATE to NEW-STATE.

PATCH-KIND is one of:
  :install-monomorphic
  :promote-polymorphic
  :promote-megamorphic
  :no-op"
  (make-opt-ic-patch-plan
   :site-id site-id
   :old-state old-state
   :new-state new-state
   :patch-kind (cond
                 ((and (eq old-state :uninitialized)
                       (eq new-state :monomorphic)) :install-monomorphic)
                 ((and (member old-state '(:uninitialized :monomorphic))
                       (eq new-state :polymorphic)) :promote-polymorphic)
                 ((eq new-state :megamorphic) :promote-megamorphic)
                 (t :no-op))
   :target target))

(defun opt-build-inline-polymorphic-dispatch (entries receiver-reg)
  "Build a simple PIC-style dispatch chain from ENTRIES.

ENTRIES is an alist of (shape-key . target). Returns a list of plists with
fields :shape, :receiver, :target representing sequential guards."
  (loop for (shape . target) in entries
        collect (list :shape shape :receiver receiver-reg :target target)))

(defun %opt-stable-print-string (object)
  (with-standard-io-syntax
    (prin1-to-string object)))

(defun %opt-effective-speculation-log (log)
  (or log *opt-speculation-log*))

(defun opt-clear-speculation-log (&optional (log *opt-speculation-log*))
  "Clear LOG's recorded failures and return LOG."
  (let ((target (%opt-effective-speculation-log log)))
    (clrhash (opt-spec-log-failures target))
    target))

(defun opt-record-speculation-failure (log site-id reason)
  "Record a failed speculation for SITE-ID and REASON."
  (let* ((target (%opt-effective-speculation-log log))
         (key (list site-id reason))
         (count (1+ (gethash key (opt-spec-log-failures target) 0))))
    (setf (gethash key (opt-spec-log-failures target)) count)
    count))

(defun opt-speculation-failed-p (log site-id reason)
  "Return T when SITE-ID/REASON has crossed LOG's failure threshold."
  (let ((target (%opt-effective-speculation-log log)))
    (>= (gethash (list site-id reason) (opt-spec-log-failures target) 0)
        (opt-spec-log-threshold target))))

(defun opt-speculation-allowed-p (site-id reason &optional (log *opt-speculation-log*))
  "Return T when SITE-ID/REASON is still allowed under LOG."
  (not (opt-speculation-failed-p log site-id reason)))

(defun %opt-speculation-log-snapshot (log)
  (let ((entries nil))
    (maphash (lambda (key count)
               (push (cons key count) entries))
             (opt-spec-log-failures log))
    (sort entries
          (lambda (left right)
            (string< (%opt-stable-print-string (car left))
                     (%opt-stable-print-string (car right)))))))

(defun opt-save-speculation-log (pathname &optional (log *opt-speculation-log*))
  "Persist LOG to PATHNAME as a simple S-expression and return PATHNAME."
  (let ((target (%opt-effective-speculation-log log)))
    (with-open-file (stream pathname
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (with-standard-io-syntax
        (prin1 (list :threshold (opt-spec-log-threshold target)
                     :failures (%opt-speculation-log-snapshot target))
               stream)))
    pathname))

(defun opt-load-speculation-log (pathname &optional (log *opt-speculation-log*))
  "Load a persisted speculation log from PATHNAME into LOG and return LOG."
  (let ((target (%opt-effective-speculation-log log)))
    (with-open-file (stream pathname :direction :input)
      (with-standard-io-syntax
        (let* ((payload (read stream nil nil))
               (threshold (or (getf payload :threshold) 1))
               (failures (getf payload :failures)))
          (setf (opt-spec-log-threshold target) threshold)
          (opt-clear-speculation-log target)
          (dolist (entry failures)
            (when (and (consp entry) (integerp (cdr entry)))
              (setf (gethash (car entry) (opt-spec-log-failures target))
                    (cdr entry)))))))
    target))
