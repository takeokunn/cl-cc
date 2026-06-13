;;;; packages/javascript/src/runtime-map.lisp — JS Map, WeakMap, WeakSet

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Map (ES2015+)
;;; -----------------------------------------------------------------------
;;;
;;; JS Map preserves insertion order and accepts any value as a key.
;;; We represent it as a struct holding an alist (preserves order) and
;;; a hash-table (O(1) lookups). The alist stores (key . value) pairs
;;; in insertion order; the hash-table maps keys → tail-of-alist for O(1)
;;; mutation. For correctness we use #'equal on primitive keys and #'eq
;;; for object keys — CL's #'equal handles this correctly for the common
;;; string/number/symbol case.

(defstruct (js-map (:conc-name js-map-))
  (ht (make-hash-table :test #'equal))   ; key → value
  (order nil))                           ; insertion-order list of keys

(defun %js-map-p (x) (js-map-p x))

(defun %js-make-map (&optional pairs)
  "Create a JS Map, optionally seeded from an iterable of [key,val] pairs."
  (let ((m (make-js-map)))
    (when (and pairs (not (eq pairs +js-undefined+)) (not (eq pairs +js-null+)))
      (%js-for-of pairs
                  (lambda (pair)
                    (let ((k (%js-get-prop pair 0))
                          (v (%js-get-prop pair 1)))
                      (%js-map-set m k v)))))
    m))

(defun %js-map-set (m key value)
  "Set KEY → VALUE in Map M, preserving insertion order."
  (let ((ht (js-map-ht m)))
    (unless (nth-value 1 (gethash key ht))
      (setf (js-map-order m) (nconc (js-map-order m) (list key))))
    (setf (gethash key ht) value))
  m)

(defun %js-map-get (m key)
  "Return value at KEY in Map M, or undefined."
  (multiple-value-bind (v f) (gethash key (js-map-ht m))
    (if f v +js-undefined+)))

(defun %js-map-has (m key)
  "True if Map M has KEY."
  (nth-value 1 (gethash key (js-map-ht m))))

(defun %js-map-delete (m key)
  "Remove KEY from Map M; return true if it existed."
  (multiple-value-bind (v f) (gethash key (js-map-ht m))
    (declare (ignore v))
    (when f
      (remhash key (js-map-ht m))
      (setf (js-map-order m) (delete key (js-map-order m) :test #'equal)))
    f))

(defun %js-map-clear (m)
  "Remove all entries from Map M."
  (clrhash (js-map-ht m))
  (setf (js-map-order m) nil)
  +js-undefined+)

(defun %js-map-size (m)
  "Return the number of entries in Map M."
  (hash-table-count (js-map-ht m)))

(defmacro %define-js-map-iterator (name docstring &body value-expr)
  "Define a Map iterator that yields VALUE-EXPR (with K and V bound to key/value)."
  `(defun ,name (m)
     ,docstring
     (let ((keys (copy-list (js-map-order m)))
           (ht   (js-map-ht m))
           (i    0))
       (%js-make-cl-iterator
        (lambda ()
          (if (>= i (length keys))
              :done
              (let* ((k (nth i keys))
                     (v (gethash k ht +js-undefined+)))
                (declare (ignorable k v))
                (incf i)
                (cons (progn ,@value-expr) nil))))))))

(%define-js-map-iterator %js-map-keys
  "Return an iterator over Map M's keys in insertion order."
  k)

(%define-js-map-iterator %js-map-values
  "Return an iterator over Map M's values in insertion order."
  v)

(%define-js-map-iterator %js-map-entries
  "Return an iterator over Map M's [key,value] pairs in insertion order."
  (%js-make-array k v))

(defun %js-map-for-each (m fn)
  "Call FN(value, key, map) for each entry in insertion order."
  (dolist (k (js-map-order m))
    (%js-funcall fn (gethash k (js-map-ht m) +js-undefined+) k m))
  +js-undefined+)

;;; -----------------------------------------------------------------------
;;;  WeakMap (ES2015+)
;;; -----------------------------------------------------------------------
;;;
;;; CL has no native weak references portable across implementations, so
;;; WeakMap is backed by a normal hash table using #'eq (identity equality).
;;; This gives correct JS semantics for object keys; primitive keys would be
;;; invalid per spec but we accept them silently for robustness.

(defstruct (js-weak-map (:conc-name js-weak-map-))
  (ht (make-hash-table :test #'eq)))

(defun %js-weak-map-p (x) (js-weak-map-p x))

(defun %js-make-weak-map () (make-js-weak-map))

(defun %js-weak-map-set (m key value)
  (setf (gethash key (js-weak-map-ht m)) value)
  m)

(defun %js-weak-map-get (m key)
  (multiple-value-bind (v f) (gethash key (js-weak-map-ht m))
    (if f v +js-undefined+)))

(defun %js-weak-map-has (m key)
  (nth-value 1 (gethash key (js-weak-map-ht m))))

(defun %js-weak-map-delete (m key)
  (multiple-value-bind (v f) (gethash key (js-weak-map-ht m))
    (declare (ignore v))
    (when f (remhash key (js-weak-map-ht m)))
    f))

;;; -----------------------------------------------------------------------
;;;  WeakSet (ES2015+)
;;; -----------------------------------------------------------------------

(defstruct (js-weak-set (:conc-name js-weak-set-))
  (ht (make-hash-table :test #'eq)))

(defun %js-weak-set-p (x) (js-weak-set-p x))

(defun %js-make-weak-set () (make-js-weak-set))

(defun %js-weak-set-add (s value)
  (setf (gethash value (js-weak-set-ht s)) t)
  s)

(defun %js-weak-set-has (s value)
  (nth-value 1 (gethash value (js-weak-set-ht s))))

(defun %js-weak-set-delete (s value)
  (multiple-value-bind (v f) (gethash value (js-weak-set-ht s))
    (declare (ignore v))
    (when f (remhash value (js-weak-set-ht s)))
    f))

;;; -----------------------------------------------------------------------
;;;  WeakRef (ES2021) — stub
;;; -----------------------------------------------------------------------

(defstruct (js-weak-ref (:conc-name js-weak-ref-))
  target)

(defun %js-make-weak-ref (target) (make-js-weak-ref :target target))
(defun %js-weak-ref-deref (wr) (js-weak-ref-target wr))

;;; -----------------------------------------------------------------------
;;;  FinalizationRegistry (ES2021) — stub (no-op without GC hooks)
;;; -----------------------------------------------------------------------

(defstruct (js-finalization-registry (:conc-name js-finreg-))
  callback)

(defun %js-make-finalization-registry (callback)
  (make-js-finalization-registry :callback callback))

(defun %js-finreg-register (reg target held-value &optional unregister-token)
  "Register TARGET with held-value HELD-VALUE (no-op in this implementation)."
  (declare (ignore reg target held-value unregister-token))
  +js-undefined+)

(defun %js-finreg-unregister (reg token)
  "Unregister entries associated with TOKEN (no-op)."
  (declare (ignore reg token))
  nil)
