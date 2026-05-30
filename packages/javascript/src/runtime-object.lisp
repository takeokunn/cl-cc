;;;; packages/javascript/src/runtime-object.lisp — JS Object built-ins + destructuring
;;;;
;;;; Object representation: CL hash tables (:test #'equal), key is string.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Objects
;;; -----------------------------------------------------------------------

(defun %js-make-object (&rest key-value-pairs)
  "Create a JS object from alternating key/value args."
  (let ((ht (%js-make-ht (max 8 (length key-value-pairs)))))
    (loop for (k v) on key-value-pairs by #'cddr
          do (setf (gethash (string k) ht) v))
    ht))

(defun %js-object-keys (obj)
  "Return array of own enumerable string keys."
  (if (%js-ht-p obj)
      (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (unless (and (> (length k) 2)
                                (string= (subseq k 0 2) "__"))
                     (vector-push-extend k result)))
                 obj)
        result)
      (%js-make-array)))

(defun %js-object-values (obj)
  "Return array of own enumerable values."
  (if (%js-ht-p obj)
      (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
        (maphash (lambda (k v)
                   (unless (and (> (length k) 2)
                                (string= (subseq k 0 2) "__"))
                     (vector-push-extend v result)))
                 obj)
        result)
      (%js-make-array)))

(defun %js-object-entries (obj)
  "Return array of [key, value] pairs."
  (if (%js-ht-p obj)
      (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
        (maphash (lambda (k v)
                   (unless (and (> (length k) 2)
                                (string= (subseq k 0 2) "__"))
                     (vector-push-extend (%js-make-array k v) result)))
                 obj)
        result)
      (%js-make-array)))

(defun %js-object-assign (target &rest sources)
  "Copy all enumerable own properties from SOURCES to TARGET."
  (dolist (src sources)
    (when (%js-ht-p src)
      (maphash (lambda (k v) (setf (gethash k target) v)) src)))
  target)

(defun %js-object-create (proto)
  "Create object with PROTO as prototype."
  (let ((ht (%js-make-ht)))
    (unless (or (eq proto +js-null+) (null proto))
      (setf (gethash "__proto__" ht) proto))
    ht))

(defun %js-object-define-property (obj key descriptor)
  "Define property with descriptor."
  (let ((k (%js-to-string key)))
    (when (%js-ht-p obj)
      (let ((val (%js-get-prop descriptor "value")))
        (unless (eq val +js-undefined+)
          (setf (gethash k obj) val)))))
  obj)

(defun %js-object-define-properties (obj props)
  "Define multiple properties."
  (when (%js-ht-p props)
    (maphash (lambda (k descriptor)
               (%js-object-define-property obj k descriptor))
             props))
  obj)

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


(defun %js-object-get-own-property-descriptor (obj key)
  "Return property descriptor."
  (let ((k (%js-to-string key)))
    (multiple-value-bind (val found) (gethash k obj)
      (if found
          (%js-make-object "value" val "writable" t "enumerable" t "configurable" t)
          +js-undefined+))))

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
                  (let* ((k (%js-to-string (funcall key-fn item)))
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
;;;  Destructuring helpers
;;; -----------------------------------------------------------------------

(defun %js-destructure-array (arr &rest indices-and-defaults)
  "Return a list of values from ARR at given indices (with defaults)."
  (loop for (idx default) on indices-and-defaults by #'cddr
        collect (let ((v (%js-get-prop arr idx)))
                  (if (eq v +js-undefined+) default v))))

(defun %js-destructure-object (obj &rest keys-and-defaults)
  "Return a list of values from OBJ for given keys (with defaults)."
  (loop for (k default) on keys-and-defaults by #'cddr
        collect (let ((v (%js-get-prop obj k)))
                  (if (eq v +js-undefined+) default v))))
