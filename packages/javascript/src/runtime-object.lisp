;;;; packages/javascript/src/runtime-object.lisp — JS Object built-ins + destructuring
;;;;
;;;; Object representation: CL hash tables (:test #'equal), key is string.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Objects
;;; -----------------------------------------------------------------------

(defun %js-make-object (&rest key-value-pairs)
  "Create a JS object from alternating key/value args.  A get/set accessor
descriptor value is routed to __get_K/__set_K (see %js-object-put-entry)."
  (let ((ht (%js-make-ht (max 8 (length key-value-pairs)))))
    (loop for (k v) on key-value-pairs by #'cddr
          do (%js-object-put-entry ht (string k) v))
    ht))

(defun %js-internal-key-p (k)
  "True when K is an internal runtime key that must not appear in Object.keys() /
for-in enumeration: a double-underscore-wrapped key (__proto__, __class__, ...)
or an accessor slot (__get_NAME / __set_NAME)."
  (let ((n (length k)))
    (or (and (> n 4)
             (string= k "__" :end1 2)
             (string= k "__" :start1 (- n 2)))
        (and (> n 6) (string= k "__get_" :end1 6))
        (and (> n 6) (string= k "__set_" :end1 6)))))

;;; Internal: iterate non-internal properties of OBJ, collecting (select-fn k v)
;;; into a fresh adjustable vector. Returns an empty array when OBJ is not a HT.
(defun %js-object-collect (obj select-fn)
  (if (%js-ht-p obj)
      (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
        (maphash (lambda (k v)
                   (unless (%js-internal-key-p k)
                     (vector-push-extend (funcall select-fn k v) result)))
                 obj)
        result)
      (%js-make-array)))

(defun %js-object-keys    (obj) (%js-object-collect obj (lambda (k v) (declare (ignore v)) k)))
(defun %js-object-values  (obj) (%js-object-collect obj (lambda (k v) (declare (ignore k)) v)))
(defun %js-object-entries (obj) (%js-object-collect obj (lambda (k v) (%js-make-array k v))))

(defun %js-object-assign (target &rest sources)
  "Copy all enumerable own properties from SOURCES to TARGET."
  (dolist (src sources)
    (when (%js-ht-p src)
      (maphash (lambda (k v) (setf (gethash k target) v)) src)))
  target)

(defun %js-object-spread-set (obj key value)
  "Set OBJ[KEY] = VALUE and return OBJ. Used to fold object-literal entries that
follow a spread (the fold threads the object, and %js-set-prop returns the value)."
  (%js-set-prop obj key value)
  obj)

(defun %js-object-create (proto)
  "Create object with PROTO as prototype."
  (let ((ht (%js-make-ht)))
    (unless (or (eq proto +js-null+) (null proto))
      (setf (gethash "__proto__" ht) proto))
    ht))

(defun %js-object-get-prototype-of (obj)
  "Return prototype of OBJ."
  (if (%js-ht-p obj)
      (multiple-value-bind (v f) (gethash "__proto__" obj)
        (if f v +js-null+))
      +js-null+))

(defun %js-object-set-prototype-of (obj proto)
  "Set prototype of OBJ."
  (when (%js-ht-p obj)
    (if (or (eq proto +js-null+) (null proto))
        (remhash "__proto__" obj)
        (setf (gethash "__proto__" obj) proto)))
  obj)


(defun %js-object-has-own (obj key)
  "True if OBJ has own property KEY."
  (when (%js-ht-p obj)
    (nth-value 1 (gethash (%js-to-string key) obj))))

(defun %js-object-from-entries (iterable)
  "Create object from [key, value] iterable."
  (let ((ht (%js-make-ht)))
    (%js-for-of iterable
                (lambda (entry)
                  (let ((k (%js-get-prop entry 0))
                        (v (%js-get-prop entry 1)))
                    (setf (gethash (%js-to-string k) ht) v))))
    ht))

(defun %js-object-is (a b)
  "Object.is — like === but handles NaN and -0."
  (cond
    ((and (%js-nan-p a) (%js-nan-p b)) t)
    ((and (eql a 0.0d0) (eql b -0.0d0)) nil)
    ((and (eql a -0.0d0) (eql b 0.0d0)) nil)
    (t (%js-strict-eq a b))))

(defun %js-object-without-keys (obj keys)
  "Return a copy of OBJ without the given KEYS (vector of strings)."
  (let ((ht (%js-make-ht)))
    (when (%js-ht-p obj)
      (let ((exclude (make-hash-table :test #'equal)))
        (when (%js-vec-p keys)
          (loop for i below (length keys)
                do (setf (gethash (%js-to-string (aref keys i)) exclude) t)))
        (maphash (lambda (k v)
                   (unless (gethash k exclude)
                     (setf (gethash k ht) v)))
                 obj)))
    ht))

(defun %js-object-group-by (iterable key-fn)
  "Group iterable elements by key."
  (let ((ht (%js-make-ht)))
    (%js-for-of iterable
                (lambda (item)
                  (let* ((k (%js-to-string (%js-funcall key-fn item)))
                         (bucket (multiple-value-bind (v f) (gethash k ht)
                                   (if f v
                                       (let ((arr (make-array 0 :element-type t
                                                                :adjustable t
                                                                :fill-pointer 0)))
                                         (setf (gethash k ht) arr)
                                         arr)))))
                    (vector-push-extend item bucket))))
    ht))

;;; -----------------------------------------------------------------------
;;;  structuredClone — deep copy of a JS value
;;; -----------------------------------------------------------------------

(defun %js-deep-clone (val &optional (seen (make-hash-table)))
  "Deep clone VAL (structuredClone semantics). Handles objects, arrays, Maps, Sets."
  (cond
    ((eq val +js-undefined+) +js-undefined+)
    ((eq val +js-null+)      +js-null+)
    ((eq val +js-nan+)       +js-nan+)
    ((numberp val)           val)
    ((stringp val)           (copy-seq val))
    ((or (eq val t) (eq val nil)) val)  ; boolean values
    ;; Cycle detection
    ((gethash val seen)      (gethash val seen))
    ;; Array
    ((%js-vec-p val)
     (let ((clone (make-array (length val) :element-type t :adjustable t :fill-pointer (length val))))
       (setf (gethash val seen) clone)
       (loop for i below (length val)
             do (setf (aref clone i) (%js-deep-clone (aref val i) seen)))
       clone))
    ;; Map
    ((js-map-p val)
     (let ((clone (%js-make-map)))
       (setf (gethash val seen) clone)
       (dolist (k (js-map-order val))
         (%js-map-set clone (%js-deep-clone k seen)
                           (%js-deep-clone (gethash k (js-map-ht val) +js-undefined+) seen)))
       clone))
    ;; Plain object
    ((%js-ht-p val)
     (let ((clone (%js-make-ht)))
       (setf (gethash val seen) clone)
       (maphash (lambda (k v)
                  (setf (gethash k clone) (%js-deep-clone v seen)))
                val)
       clone))
    ;; Date
    ((typep val 'js-date)
     (make-js-date :ms (js-date-ms val)))
    ;; RegExp
    ((js-regexp-p val)
     (%js-make-regex (js-regexp-source val) (js-regexp-flags val)))
    ;; Everything else — return as-is (functions, symbols, etc.)
    (t val)))


;;; -----------------------------------------------------------------------
;;;  Destructuring helpers
;;; -----------------------------------------------------------------------

(defun %js-destructure-array (arr &rest indices-and-defaults)
  "Array-destructuring rest helper.  The lowering calls this only for a rest
element: (%js-destructure-array arr idx :rest) collects arr[idx..end-1] into a
fresh JS array (a real vector, so rest.map/.join/Array.isArray all work).
Regression: the :rest sentinel was mistaken for a default value, so `rest' was a
one-element CL list — `typeof rest' was \"object\" and rest.join threw.  A
value-mode fallback (pairs of index/default) is kept for safety."
  (if (and (= (length indices-and-defaults) 2)
           (eq (second indices-and-defaults) :rest))
      (let ((idx (first indices-and-defaults))
            (len (truncate (%js-to-number (%js-get-prop arr "length")))))
        (%js-list-to-array
         (loop for i from idx below len collect (%js-get-prop arr i))))
      (loop for (idx default) on indices-and-defaults by #'cddr
            collect (let ((v (%js-get-prop arr idx)))
                      (if (eq v +js-undefined+) default v)))))

(defun %js-destructure-object (obj &rest keys-and-defaults)
  "Object-destructuring rest helper.  Rest form (emitted by the lowering):
   (%js-destructure-object obj :rest k1 k2 …) → a fresh JS object holding obj's
own enumerable keys EXCEPT the already-destructured k1,k2,… .  Regression: the
rest call passed no excluded keys and returned a one-element list, so `others'
was null/garbage.  A value-mode fallback (pairs of key/default) is kept."
  (if (and keys-and-defaults (eq (first keys-and-defaults) :rest))
      (let ((excluded (rest keys-and-defaults))
            (out (%js-make-object)))
        (dolist (k (coerce (%js-object-keys obj) 'list) out)
          (unless (member k excluded :test #'equal)
            (%js-set-prop out k (%js-get-prop obj k)))))
      (loop for (k default) on keys-and-defaults by #'cddr
            collect (let ((v (%js-get-prop obj k)))
                      (if (eq v +js-undefined+) default v)))))
