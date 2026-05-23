;;; FR-856: DELAY / FORCE lazy evaluation
;;; FR-857: MEMOIZE with hit/miss statistics

(in-package :cl-cc/expand)

;; ── FR-856 Promises ─────────────────────────────────────────────────────

(defstruct (promise (:constructor %make-promise (thunk forced-p value))
                    (:conc-name promise-))
  "Lazy value: stores a thunk and caches its result after the first FORCE."
  thunk
  forced-p
  value)

(defun force (promise)
  "Force PROMISE, calling its thunk exactly once and caching the result."
  (unless (promise-forced-p promise)
    (let ((result (funcall (promise-thunk promise))))
      (setf (promise-value promise) result
            (promise-forced-p promise) t)))
  (promise-value promise))

(defmacro delay (form)
  "Create a promise that evaluates FORM lazily on first FORCE."
  `(%make-promise (lambda () ,form) nil nil))

;; ── FR-857 Memoize ──────────────────────────────────────────────────────

(defstruct memoize-state
  fn
  (cache  (make-hash-table :test #'equal))
  (hits   0 :type fixnum)
  (misses 0 :type fixnum))

(defvar *memoize-registry* (make-hash-table :test #'eq)
  "Maps each memoized closure to its MEMOIZE-STATE.")

(defun memoize (fn)
  "Return a memoized wrapper around FN. Supports MEMOIZE-STATS / MEMOIZE-CLEAR."
  (let ((state (make-memoize-state :fn fn)))
    (let ((memo-fn
           (lambda (&rest args)
             (multiple-value-bind (val found)
                 (gethash args (memoize-state-cache state))
               (if found
                   (progn (incf (memoize-state-hits state)) val)
                   (let ((result (apply fn args)))
                     (incf (memoize-state-misses state))
                     (setf (gethash args (memoize-state-cache state)) result)
                     result))))))
      (setf (gethash memo-fn *memoize-registry*) state)
      memo-fn)))

(defun memoize-stats (memo-fn)
  "Return a plist with :HITS, :MISSES, and :SIZE for MEMO-FN."
  (let ((state (gethash memo-fn *memoize-registry*)))
    (when state
      (list :hits   (memoize-state-hits state)
            :misses (memoize-state-misses state)
            :size   (hash-table-count (memoize-state-cache state))))))

(defun memoize-clear (memo-fn)
  "Clear MEMO-FN's cache and reset counters. Returns T on success."
  (let ((state (gethash memo-fn *memoize-registry*)))
    (when state
      (clrhash (memoize-state-cache state))
      (setf (memoize-state-hits   state) 0
            (memoize-state-misses state) 0)
      t)))

(export '(promise promisep %make-promise force delay
          memoize memoize-stats memoize-clear *memoize-registry*)
        :cl-cc/expand)
