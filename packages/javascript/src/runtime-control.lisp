;;;; packages/javascript/src/runtime-control.lisp — JS control-flow & iteration
;;;;
;;;; Extracted from runtime.lisp: exception infrastructure, try/catch/finally,
;;;; and the full iteration-protocol family (for-in, for-of, iter-values/keys).
;;;; Load order: directly after runtime.lisp.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Control flow / exceptions
;;; -----------------------------------------------------------------------

(define-condition js-exception ()
  ((value :initarg :value :reader js-exception-value)))

(defun %js-throw (value)
  (error 'js-exception :value value))

(defun %js-try-catch-finally (try-thunk catch-thunk finally-thunk)
  "Execute TRY-THUNK; on JS exception call CATCH-THUNK with the value.
   FINALLY-THUNK is always called. Returns value of try or catch.
   The thunks are compiled-JS lambdas (vm-closure-objects), so they must be
   invoked through %js-funcall (-> *js-apply-fn* -> %vm-call-closure-sync), not a
   raw CL:FUNCALL which cannot call a vm-closure."
  (let ((result +js-undefined+))
    (unwind-protect
         (handler-case
             (setf result (%js-funcall try-thunk))
           (js-exception (c)
             (when catch-thunk
               (setf result (%js-funcall catch-thunk (js-exception-value c))))))
      (when finally-thunk
        (%js-funcall finally-thunk)))
    result))

;;; -----------------------------------------------------------------------
;;;  Iteration protocol
;;; -----------------------------------------------------------------------

(defun %js-for-in (obj body-fn)
  "Execute BODY-FN for each enumerable string key in OBJ.
Skips internal double-underscore keys (__proto__, __class__, etc.)."
  (when (%js-ht-p obj)
    (maphash (lambda (k v)
               (declare (ignore v))
               ;; Skip internal/prototype-chain keys (double-underscore prefix+suffix)
               ;; e.g. __proto__, __class__, __constructor__, __super__, __new__
               (unless (let ((n (length k)))
                         (and (> n 4)
                              (string= k "__" :end1 2)
                              (string= k "__" :start1 (- n 2))))
                 (%js-funcall body-fn k)))
             obj))
  +js-undefined+)

(defun %js-advance-iterator (iter body-fn)
  "Drain a JS iterator object (with a \"next\" method), calling BODY-FN per value."
  (let ((next-fn (gethash "next" iter)))
    (when next-fn
      (loop
        (let ((result (%js-funcall next-fn)))
          (when (or (not (%js-ht-p result))
                    (%js-truthy (gethash "done" result)))
            (return))
          (%js-funcall body-fn (gethash "value" result)))))))

(defun %js-advance-cl-iterator (iter body-fn)
  "Drain a CL-iterator (closure returning (:value v :done p) or :done)."
  (loop
    (multiple-value-bind (val done) (%js-iter-next iter)
      (when done (return))
      (%js-funcall body-fn val))))

(defun %js-for-of (iterable body-fn)
  "Execute BODY-FN for each element of ITERABLE.
Supports: arrays, strings, Map (entries), Set (values),
CL-iterator closures, JS iterator objects, generator objects."
  (cond
    ;; Plain CL vector (JS Array)
    ((%js-vec-p iterable)
     (loop for i below (length iterable)
           do (%js-funcall body-fn (aref iterable i))))
    ;; String — iterate characters
    ((stringp iterable)
     (loop for ch across iterable
           do (%js-funcall body-fn (string ch))))
    ;; JS Map → iterate [key, value] pairs in insertion order
    ((typep iterable 'js-map)
     (dolist (k (js-map-order iterable))
       (let ((v (gethash k (js-map-ht iterable) +js-undefined+)))
         (%js-funcall body-fn (%js-make-array k v)))))
    ;; JS Set → iterate values in insertion order
    ((typep iterable 'js-set)
     (dolist (k (js-set-order iterable))
       (%js-funcall body-fn k)))
    ;; CL closure — treat as an iterator (used by %js-make-cl-iterator)
    ((functionp iterable)
     (%js-advance-cl-iterator iterable body-fn))
    ;; JS object: check for iterator protocol (@@iterator or "next" method)
    ((%js-ht-p iterable)
     (let ((next-fn (gethash "next" iterable))
           (iter-fn (gethash "@@iterator" iterable)))
       (cond
         ;; Already an iterator (has .next)
         (next-fn (%js-advance-iterator iterable body-fn))
         ;; Iterable (has @@iterator factory)
         (iter-fn
          (let ((iter (%js-funcall iter-fn)))
            (%js-for-of iter body-fn)))
         ;; Set-like hash-table (all values are t — Set representation)
         ;; For now: if hash-table is not an Object (no string-key convention),
         ;; iterate keys. We use the presence of +php-null+ sentinel to detect
         ;; PHP arrays and skip those.
         (t
          ;; Iterate hash-table keys as a Set (best effort)
          (maphash (lambda (k v)
                     (declare (ignore v))
                     (unless (and (stringp k) (%js-internal-key-p k))
                       (%js-funcall body-fn k)))
                   iterable)))))
    (t nil))
  +js-undefined+)

(defun %js-iter-values (iterable)
  "Collect ITERABLE's values into a fresh CL list, in order. This is the 1-arg
companion to the 2-arg %js-for-of (which calls a body-fn): the for-of loop
LOWERING walks the resulting list with car/cdr so that break/continue and the
loop body's own scope work as ordinary statements. Reuses %js-for-of so every
iterable kind (array, string, iterator-protocol object) is handled in one place."
  (let ((acc nil))
    (%js-for-of iterable (lambda (el) (push el acc)))
    (nreverse acc)))

(defun %js-iter-keys (obj)
  "Collect OBJ's enumerable string keys into a fresh CL list — the 1-arg
companion to %js-for-in, used by the for-in loop lowering (see %js-iter-values)."
  (let ((acc nil))
    (%js-for-in obj (lambda (k) (push k acc)))
    (nreverse acc)))
