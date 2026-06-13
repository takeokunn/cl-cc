;;;; packages/javascript/src/runtime-collections.lisp — JS Set and Iterator built-ins
;;;;
;;;; Set is represented as a js-set struct with a hash-table (for O(1) membership
;;;; tests) and an insertion-order list (for deterministic for-of iteration).
;;;; Iterators are represented as closures or JS objects with a "next" method.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Set built-ins (ES2025)
;;; -----------------------------------------------------------------------

;;; JS Set is represented as a struct with:
;;;   HT    — CL hash-table (key → t) for O(1) has/delete
;;;   ORDER — list of keys in insertion order (used by for-of / keys / values)
(defstruct (js-set (:conc-name js-set-))
  (ht    (make-hash-table :test #'equal :size 8) :type hash-table)
  (order nil))

(defun %js-make-set ()
  "Create a new empty ordered JS Set."
  (make-js-set))

;;; Internal helper: copy every key from SRC (a js-set) into TARGET (a js-set).
(defun %js-set-copy-all (target src)
  (dolist (k (js-set-order src))
    (%js-set-add target k)))

(defun %js-set-add (s val)
  (unless (nth-value 1 (gethash val (js-set-ht s)))
    (setf (gethash val (js-set-ht s)) t)
    (setf (js-set-order s) (append (js-set-order s) (list val))))
  s)

(defun %js-set-delete (s val)
  (multiple-value-bind (v found) (gethash val (js-set-ht s))
    (declare (ignore v))
    (when found
      (remhash val (js-set-ht s))
      (setf (js-set-order s) (delete val (js-set-order s) :test #'equal :count 1)))
    found))

(defun %js-set-has (s val)
  (nth-value 1 (gethash val (js-set-ht s))))

(defun %js-set-clear (s)
  (clrhash (js-set-ht s))
  (setf (js-set-order s) nil)
  +js-undefined+)

(defun %js-set-size (s)
  (hash-table-count (js-set-ht s)))

(defun %js-set-keys (s)
  "Return a vector of the set's values in insertion order."
  (let ((order (js-set-order s)))
    (make-array (length order) :element-type t :adjustable t :fill-pointer (length order)
                :initial-contents order)))

(defun %js-set-entries (s)
  "Return array of [key, key] pairs (Set semantics)."
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (dolist (k (js-set-order s))
      (vector-push-extend (%js-make-array k k) result))
    result))

(defun %js-set-for-each (s fn)
  (dolist (k (js-set-order s))
    (%js-funcall fn k k s))
  +js-undefined+)

;;; Data-driven set operations
;;; (define-js-set-predicate name stop-condition result-if-stopped default-result)
(defmacro define-js-set-predicate (name stop-condition result-if-stopped default-result)
  `(defun ,name (a b)
     (block check
       (dolist (k (js-set-order a))
         (when ,stop-condition
           (return-from check ,result-if-stopped)))
       ,default-result)))

;;; (define-js-set-filter-op name docstring condition)  — CONDITION may reference A, B, K.
(defmacro define-js-set-filter-op (name docstring condition)
  `(defun ,name (a b)
     ,docstring
     (let ((result (%js-make-set)))
       (dolist (k (js-set-order a))
         (when ,condition (%js-set-add result k)))
       result)))

(defun %js-set-union (a b)
  "New set: union of A and B (insertion order: A first, then new elements from B)."
  (let ((result (%js-make-set)))
    (%js-set-copy-all result a)
    (%js-set-copy-all result b)
    result))

(define-js-set-filter-op %js-set-intersection
  "New set: elements in both A and B (order from A)."
  (%js-set-has b k))

(define-js-set-filter-op %js-set-difference
  "New set: elements in A but not in B."
  (not (%js-set-has b k)))

(defun %js-set-symmetric-difference (a b)
  "New set: elements in A or B but not both."
  (let ((result (%js-make-set)))
    (dolist (k (js-set-order a))
      (unless (%js-set-has b k) (%js-set-add result k)))
    (dolist (k (js-set-order b))
      (unless (%js-set-has a k) (%js-set-add result k)))
    result))

(define-js-set-predicate %js-set-is-subset-of
  (not (nth-value 1 (gethash k (js-set-ht b)))) nil t)

(define-js-set-predicate %js-set-is-disjoint-from
  (nth-value 1 (gethash k (js-set-ht b))) nil t)

;;; isSupersetOf(A, B) means every element of B is in A — must iterate B, not A.
;;; The macro always iterates A, so this is a plain defun instead.
(defun %js-set-is-superset-of (a b)
  (dolist (k (js-set-order b) t)
    (unless (nth-value 1 (gethash k (js-set-ht a)))
      (return nil))))

;;; -----------------------------------------------------------------------
;;;  Iterator helpers (ES2025)
;;; -----------------------------------------------------------------------

;;; Iterators are represented as closures that return (:value v :done nil/t)
;;; or as JS objects with a "next" method.

(defun %js-iter-next (iter)
  "Advance iter; return (values value done-p)."
  (let* ((result (if (functionp iter)
                     (funcall iter)
                     (funcall (gethash "next" iter))))
         (ht-p  (%js-ht-p result)))
    (values (if ht-p (gethash "value" result) result)
            (if ht-p (%js-truthy (gethash "done" result)) nil))))

(defun %js-make-cl-iterator (get-next-fn)
  "Create a JS iterator object from a CL thunk that returns (value . done)."
  (let ((ht (%js-make-ht)))
    (setf (gethash "next" ht)
          (lambda ()
            (let ((pair (funcall get-next-fn)))
              (if (eq pair :done)
                  (%js-make-object "value" +js-undefined+ "done" t)
                  (%js-make-object "value" (car pair) "done" nil)))))
    ;; Attach ES2025 Iterator.prototype helpers and @@iterator
    (%js-add-iterator-helpers! ht)))

(defun %js-vec-to-iter (vec)
  "Create iterator over a vector."
  (let ((i 0))
    (%js-make-cl-iterator
     (lambda ()
       (if (>= i (length vec))
           :done
           (let ((v (aref vec i)))
             (incf i)
             (cons v nil)))))))

;;; ─── Stateless transformers (no extra mutable state needed) ──────────────────

(defun %js-iterator-map (iter fn)
  (%js-make-cl-iterator
   (lambda ()
     (multiple-value-bind (val done) (%js-iter-next iter)
       (if done :done (cons (%js-funcall fn val) nil))))))

(defun %js-iterator-filter (iter fn)
  (%js-make-cl-iterator
   (lambda ()
     (loop
       (multiple-value-bind (val done) (%js-iter-next iter)
         (when done (return :done))
         (when (%js-truthy (%js-funcall fn val))
           (return (cons val nil))))))))

;;; ─── Stateful transformers (carry extra mutable state in closure) ─────────────

(defun %js-iterator-take (iter n)
  (let ((count 0))
    (%js-make-cl-iterator
     (lambda ()
       (if (>= count n)
           :done
           (multiple-value-bind (val done) (%js-iter-next iter)
             (if done :done (progn (incf count) (cons val nil)))))))))

(defun %js-iterator-drop (iter n)
  ;; init-done guards the one-time skip phase so it never re-runs on later calls.
  ;; Using return-from here was unsafe: the outer function's block is already gone
  ;; by the time the stored lambda is invoked, which is undefined behavior in CL.
  (let ((init-done nil))
    (%js-make-cl-iterator
     (lambda ()
       (unless init-done
         (setf init-done t)
         (dotimes (_ n)
           (multiple-value-bind (v d) (%js-iter-next iter)
             (declare (ignore v))
             (when d (return)))))
       (multiple-value-bind (val done) (%js-iter-next iter)
         (if done :done (cons val nil)))))))

(defun %js-iterator-flat-map (iter fn)
  (let ((inner nil))
    (%js-make-cl-iterator
     (lambda ()
       (loop
         (when inner
           (multiple-value-bind (val done) (%js-iter-next inner)
             (unless done (return (cons val nil)))
             (setf inner nil)))
         (multiple-value-bind (val done) (%js-iter-next iter)
           (when done (return :done))
           (let ((mapped (%js-funcall fn val)))
             (setf inner (if (%js-vec-p mapped)
                             (%js-vec-to-iter mapped)
                             mapped)))))))))

;;; ─── Terminal consumers (return a single value, not an iterator) ──────────────

(defmacro %js-doiter ((var iter &optional (done-result '+js-undefined+)) &body body)
  "Iterate JS iterator ITER, binding VAR to each successive value.
BODY runs for each element; DONE-RESULT is returned when the iterator exhausts."
  (let ((done (gensym "done")))
    `(loop
       (multiple-value-bind (,var ,done) (%js-iter-next ,iter)
         (when ,done (return ,done-result))
         ,@body))))

(defun %js-iterator-reduce (iter fn &optional (init +js-undefined+))
  (let ((acc init) (first-p (eq init +js-undefined+)))
    (%js-doiter (val iter acc)
      (if first-p
          (setf acc val first-p nil)
          (setf acc (%js-funcall fn acc val))))))

(defun %js-iterator-to-array (iter)
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (%js-doiter (val iter result)
      (vector-push-extend val result))))

(defun %js-iterator-for-each (iter fn)
  (%js-doiter (val iter +js-undefined+)
    (%js-funcall fn val)))

(defun %js-iterator-some (iter fn)
  (%js-doiter (val iter nil)
    (when (%js-truthy (%js-funcall fn val)) (return t))))

(defun %js-iterator-every (iter fn)
  (%js-doiter (val iter t)
    (unless (%js-truthy (%js-funcall fn val)) (return nil))))

(defun %js-iterator-find (iter fn)
  (%js-doiter (val iter +js-undefined+)
    (when (%js-truthy (%js-funcall fn val)) (return val))))

;;; ─── ES2025 Iterator.prototype helpers ────────────────────────────────────────

;;; Data table: ES2025 Iterator.prototype method names → CL implementation symbols.
;;; Each function takes the iterator itself as its first argument, followed by any
;;; additional parameters — so the binding loop can use (apply impl self args)
;;; uniformly across all methods.
(defparameter *%js-iterator-method-names*
  '(("map"     . %js-iterator-map)
    ("filter"  . %js-iterator-filter)
    ("take"    . %js-iterator-take)
    ("drop"    . %js-iterator-drop)
    ("flatMap" . %js-iterator-flat-map)
    ("reduce"  . %js-iterator-reduce)
    ("toArray" . %js-iterator-to-array)
    ("forEach" . %js-iterator-for-each)
    ("some"    . %js-iterator-some)
    ("every"   . %js-iterator-every)
    ("find"    . %js-iterator-find))
  "ES2025 Iterator.prototype methods: JS name -> CL function (iter &rest args).")

(defun %js-add-iterator-helpers! (it)
  "Attach ES2025 Iterator.prototype methods and @@iterator to IT.
Binding logic is uniform: each method dispatches through *%js-iterator-method-names*."
  (setf (gethash "@@iterator" it) (lambda () it))
  (dolist (entry *%js-iterator-method-names*)
    (let ((key (car entry))
          (fn  (symbol-function (cdr entry))))
      (setf (gethash key it)
            (let ((impl fn) (self it))
              (lambda (&rest args) (apply impl self args))))))
  it)
