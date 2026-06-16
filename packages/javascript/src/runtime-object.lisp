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

(defun %js-string-prefix-p (prefix string)
  (and (stringp string)
       (<= (length prefix) (length string))
       (string= prefix (subseq string 0 (length prefix)))))

(defun %js-object-accessor-property-name (slot-key)
  (cond
    ((%js-string-prefix-p "__get_" slot-key) (subseq slot-key 6))
    ((%js-string-prefix-p "__set_" slot-key) (subseq slot-key 6))
    (t nil)))

(defun %js-object-own-string-property-keys (obj)
  "Return public own string property names for OBJ, translating accessor slots
back to their JS property names and filtering runtime-only keys."
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0))
        (seen (%js-make-ht)))
    (labels ((add-key (key)
               (unless (%js-symbol-p key)
                 (let ((k (%js-to-string key)))
                   (unless (or (%js-symbol-storage-key-p k)
                               (%js-internal-key-p k)
                               (nth-value 1 (gethash k seen)))
                     (setf (gethash k seen) t)
                     (vector-push-extend k result)))))
             (add-accessor-key (slot-key)
               (let ((property-name (%js-object-accessor-property-name slot-key)))
                 (when property-name
                   (unless (nth-value 1 (gethash property-name seen))
                     (setf (gethash property-name seen) t)
                     (vector-push-extend property-name result))))))
      (cond
        ((%js-proxy-object-p obj)
         (dolist (key (%js-proxy-key-list (%js-proxy-own-keys obj)))
           (add-key key)))
        ((%js-ht-p obj)
         (maphash (lambda (k v)
                    (declare (ignore v))
                    (add-key k))
                  obj)
         (maphash (lambda (k v)
                    (declare (ignore v))
                    (add-accessor-key k))
                  obj))))
    result))

(defun %js-object-own-symbol-property-keys (obj)
  "Return public own Symbol property keys for OBJ."
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0))
        (seen (%js-make-ht)))
    (labels ((add-symbol-key (key)
               (let ((sym (cond
                            ((%js-symbol-p key) key)
                            ((%js-symbol-storage-key-p key)
                             (%js-symbol-from-storage-key key))
                            (t +js-undefined+))))
                 (when (and (%js-symbol-p sym)
                            (not (nth-value 1 (gethash (%js-symbol-as-key sym) seen))))
                   (setf (gethash (%js-symbol-as-key sym) seen) t)
                   (vector-push-extend sym result)))))
      (cond
        ((%js-proxy-object-p obj)
         (dolist (key (%js-proxy-key-list (%js-proxy-own-keys obj)))
           (add-symbol-key key)))
        ((%js-ht-p obj)
         (maphash (lambda (k v)
                    (declare (ignore v))
                    (add-symbol-key k))
                  obj))))
    result))

(defun %js-object-own-property-keys (obj)
  "Return public own string and Symbol property keys for Reflect.ownKeys."
  (let ((result (%js-object-own-string-property-keys obj)))
    (map nil
         (lambda (key) (vector-push-extend key result))
         (%js-object-own-symbol-property-keys obj))
    result))

;;; Internal: iterate non-internal properties of OBJ, collecting (select-fn k v)
;;; into a fresh adjustable vector. Returns an empty array when OBJ is not a HT.
(defun %js-object-collect (obj select-fn)
  (if (or (%js-proxy-object-p obj) (%js-ht-p obj))
      (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
        (map nil
             (lambda (k)
               (vector-push-extend (funcall select-fn k (%js-get-prop obj k)) result))
             (%js-object-own-string-property-keys obj))
        result)
      (%js-make-array)))

(defun %js-object-keys    (obj) (%js-object-collect obj (lambda (k v) (declare (ignore v)) k)))
(defun %js-object-values  (obj) (%js-object-collect obj (lambda (k v) (declare (ignore k)) v)))
(defun %js-object-entries (obj) (%js-object-collect obj (lambda (k v) (%js-make-array k v))))
(defun %js-object-own-keys (obj) (%js-object-own-property-keys obj))
(defun %js-object-get-own-property-names (obj) (%js-object-own-string-property-keys obj))
(defun %js-object-get-own-property-symbols (obj) (%js-object-own-symbol-property-keys obj))

(defun %js-object-assign (target &rest sources)
  "Copy all enumerable own properties from SOURCES to TARGET."
  (dolist (src sources)
    (when (or (%js-proxy-object-p src) (%js-ht-p src))
      (map nil
           (lambda (key) (%js-set-prop target key (%js-get-prop src key)))
           (%js-object-own-string-property-keys src))
      (map nil
           (lambda (key) (%js-set-prop target key (%js-get-prop src key)))
           (%js-object-own-symbol-property-keys src))))
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
  (when (and (%js-ht-p obj)
             (or (%js-object-extensible-p obj)
                 (eq (%js-object-get-prototype-of obj) proto)))
    (if (or (eq proto +js-null+) (null proto))
        (remhash "__proto__" obj)
        (setf (gethash "__proto__" obj) proto)))
  obj)

(defun %js-object-internal-flag (obj key default)
  (if (%js-ht-p obj)
      (multiple-value-bind (value present-p) (gethash key obj)
        (if present-p value default))
      default))

(defun %js-object-extensible-p (obj)
  (and (%js-ht-p obj)
       (not (eq (%js-object-internal-flag obj "__extensible__" t) nil))))

(defun %js-object-sealed-p (obj)
  (and (%js-ht-p obj)
       (not (eq (%js-object-internal-flag obj "__sealed__" nil) nil))))

(defun %js-object-frozen-p (obj)
  (and (%js-ht-p obj)
       (not (eq (%js-object-internal-flag obj "__frozen__" nil) nil))))

(defun %js-object-prevent-extensions (obj)
  (when (%js-ht-p obj)
    (setf (gethash "__extensible__" obj) nil))
  obj)

(defun %js-object-seal (obj)
  (%js-object-prevent-extensions obj)
  (when (%js-ht-p obj)
    (setf (gethash "__sealed__" obj) t))
  obj)

(defun %js-object-freeze (obj)
  (%js-object-seal obj)
  (when (%js-ht-p obj)
    (setf (gethash "__frozen__" obj) t))
  obj)

(defun %js-object-has-own (obj key)
  "True if OBJ has own property KEY."
  (cond
    ((%js-proxy-object-p obj)
     (not (eq (%js-proxy-get-own-property-descriptor obj key) +js-undefined+)))
    ((%js-ht-p obj)
     (%js-object-own-property-or-accessor-present-p obj key))
    (t nil)))

(defun %js-object-from-entries (iterable)
  "Create object from [key, value] iterable."
  (let ((ht (%js-make-ht)))
    (%js-for-of iterable
                (lambda (entry)
                  (let ((k (%js-get-prop entry 0))
                        (v (%js-get-prop entry 1)))
                    (%js-set-prop ht k v))))
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
  "Object.groupBy(iterable, keyFn): group values into a null-prototype object."
  (let ((ht (%js-object-create +js-null+))
        (index 0))
    (%js-for-of iterable
                (lambda (item)
                  (let* ((k (%js-to-string (%js-funcall key-fn item index)))
                         (bucket (multiple-value-bind (v f) (gethash k ht)
                                   (if f v
                                       (let ((arr (%js-make-array)))
                                         (setf (gethash k ht) arr)
                                         arr)))))
                    (vector-push-extend item bucket)
                    (incf index))))
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
