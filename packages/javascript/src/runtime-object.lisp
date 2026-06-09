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
;;;  JSON serialization / deserialization
;;; -----------------------------------------------------------------------

(defun %js-json-stringify-value (val depth)
  "Recursively convert JS value VAL to a JSON string (depth limited to 50)."
  (when (> depth 50) (return-from %js-json-stringify-value "null"))
  (cond
    ((or (eq val +js-null+) (eq val +js-undefined+) (null val)) "null")
    ((eq val t)  "true")
    ((eq val nil) "false")
    ((%js-float-nan-p val) "null")
    ((%js-float-infinity-p val) "null")
    ((numberp val)
     (let ((n (coerce val 'double-float)))
       (if (= n (floor n))
           (format nil "~D" (floor n))
           (format nil "~F" n))))
    ((stringp val)
     (with-output-to-string (s)
       (write-char #\" s)
       (loop for ch across val do
         (cond ((char= ch #\") (write-string "\\\"" s))
               ((char= ch #\\) (write-string "\\\\" s))
               ((char= ch #\Newline) (write-string "\\n" s))
               ((char= ch #\Return) (write-string "\\r" s))
               ((char= ch #\Tab) (write-string "\\t" s))
               (t (write-char ch s))))
       (write-char #\" s)))
    ((%js-vec-p val)
     (format nil "[~{~A~^,~}]"
             (loop for i below (length val)
                   collect (%js-json-stringify-value (aref val i) (1+ depth)))))
    ((%js-ht-p val)
     (let ((pairs nil))
       (maphash (lambda (k v)
                  (unless (%js-internal-key-p k)
                    (let ((vs (%js-json-stringify-value v (1+ depth))))
                      (unless (string= vs "undefined")
                        (push (format nil "~A:~A" (%js-json-stringify-value k 0) vs) pairs)))))
                val)
       (format nil "{~{~A~^,~}}" (nreverse pairs))))
    (t "null")))

(defun %js-json-stringify (val)
  (%js-json-stringify-value val 0))

(defun %js-json-parse (str)
  "Minimal JSON parser: handles null, true, false, numbers, strings, arrays, objects.
For production use, a full JSON parser would be needed."
  (handler-case
      (%js-json-parse-value (string-trim '(#\Space #\Tab #\Newline #\Return) str) 0)
    (error () +js-undefined+)))

(defun %js-json-parse-value (str pos)
  "Parse JSON value at POS in STR, returning (values parsed-value new-pos)."
  (let ((c (and (< pos (length str)) (char str pos))))
    (cond
      ((null c) (values +js-undefined+ pos))
      ((char= c #\") (%js-json-parse-string str (1+ pos)))
      ((char= c #\{) (%js-json-parse-object str (1+ pos)))
      ((char= c #\[) (%js-json-parse-array str (1+ pos)))
      ((and (>= (length str) (+ pos 4)) (string= str "null" :start1 pos :end1 (+ pos 4)))
       (values +js-null+ (+ pos 4)))
      ((and (>= (length str) (+ pos 4)) (string= str "true" :start1 pos :end1 (+ pos 4)))
       (values t (+ pos 4)))
      ((and (>= (length str) (+ pos 5)) (string= str "false" :start1 pos :end1 (+ pos 5)))
       (values nil (+ pos 5)))
      ((or (digit-char-p c) (char= c #\-))
       (%js-json-parse-number str pos))
      (t (values +js-undefined+ pos)))))

(defun %js-json-skip-ws (str pos)
  (loop while (and (< pos (length str))
                   (member (char str pos) '(#\Space #\Tab #\Newline #\Return)))
        do (incf pos))
  pos)

(defun %js-json-parse-string (str pos)
  ;; NB: use an explicit stream, not WITH-OUTPUT-TO-STRING — the latter returns
  ;; the buffer string and discards the body's (values …), so the new position was
  ;; lost (and the inner get-output-stream-string drained the buffer, yielding "").
  (let ((buf (make-string-output-stream)))
    (loop
      (when (>= pos (length str)) (return))
      (let ((c (char str pos)))
        (when (char= c #\") (return))
        (incf pos)
        (if (char= c #\\)
            (let ((esc (char str pos)))
              (incf pos)
              (write-char (case esc (#\" #\") (#\\ #\\) (#\/ #\/) (#\n #\Newline)
                                    (#\r #\Return) (#\t #\Tab) (t esc)) buf))
            (write-char c buf))))
    (values (get-output-stream-string buf) (1+ pos))))

(defun %js-json-parse-number (str pos)
  (let ((end pos))
    (when (char= (char str end) #\-) (incf end))
    (loop while (and (< end (length str)) (or (digit-char-p (char str end)) (char= (char str end) #\.)
                                              (member (char str end) '(#\e #\E #\+ #\-))))
          do (incf end))
    (values (handler-case (coerce (read-from-string (subseq str pos end)) 'double-float)
              (error () *js-nan-float*))
            end)))

(defun %js-json-parse-array (str pos)
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (setf pos (%js-json-skip-ws str pos))
    (when (and (< pos (length str)) (char= (char str pos) #\]))
      (return-from %js-json-parse-array (values result (1+ pos))))
    (loop
      (multiple-value-bind (val new-pos) (%js-json-parse-value str (%js-json-skip-ws str pos))
        (vector-push-extend val result)
        (setf pos (%js-json-skip-ws str new-pos))
        (cond ((and (< pos (length str)) (char= (char str pos) #\,)) (incf pos))
              ((and (< pos (length str)) (char= (char str pos) #\])) (return (values result (1+ pos))))
              (t (return (values result pos))))))))

(defun %js-json-parse-object (str pos)
  (let ((ht (%js-make-ht)))
    (setf pos (%js-json-skip-ws str pos))
    (when (and (< pos (length str)) (char= (char str pos) #\}))
      (return-from %js-json-parse-object (values ht (1+ pos))))
    (loop
      (setf pos (%js-json-skip-ws str pos))
      (unless (and (< pos (length str)) (char= (char str pos) #\")) (return (values ht pos)))
      (multiple-value-bind (key new-pos) (%js-json-parse-string str (1+ pos))
        (setf pos (%js-json-skip-ws str new-pos))
        (when (and (< pos (length str)) (char= (char str pos) #\:)) (incf pos))
        (multiple-value-bind (val new-pos2) (%js-json-parse-value str (%js-json-skip-ws str pos))
          (setf (gethash key ht) val
                pos (%js-json-skip-ws str new-pos2))
          (cond ((and (< pos (length str)) (char= (char str pos) #\,)) (incf pos))
                ((and (< pos (length str)) (char= (char str pos) #\})) (return (values ht (1+ pos))))
                (t (return (values ht pos)))))))))

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
