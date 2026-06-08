;;;; packages/javascript/src/runtime-typed-arrays.lisp — JS TypedArray (ES2015+)
;;;;
;;;; TypedArrays are backed by CL arrays of the appropriate element type.
;;;; Supported: Int8Array, Uint8Array, Uint8ClampedArray,
;;;;            Int16Array, Uint16Array, Int32Array, Uint32Array,
;;;;            Float32Array, Float64Array, BigInt64Array, BigUint64Array.
;;;;
;;;; Each TypedArray is a struct wrapping a CL array + a type tag.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  TypedArray struct
;;; -----------------------------------------------------------------------

(defstruct (js-typed-array (:conc-name js-ta-))
  type-name        ; string: "Uint8Array" etc.
  element-size     ; bytes per element
  buffer           ; CL array (element-type determined by type-name)
  byte-offset      ; offset into buffer (usually 0)
  length)          ; number of elements

(defun %js-typed-array-p (x) (js-typed-array-p x))

;;; -----------------------------------------------------------------------
;;;  Constructor helpers
;;; -----------------------------------------------------------------------

(defparameter *js-typed-array-specs*
  '(("Int8Array"          :int8    1 (signed-byte 8)    -128                  127)
    ("Uint8Array"         :uint8   1 (unsigned-byte 8)   0                    255)
    ("Uint8ClampedArray"  :uint8c  1 (unsigned-byte 8)   0                    255)
    ("Int16Array"         :int16   2 (signed-byte 16)   -32768                32767)
    ("Uint16Array"        :uint16  2 (unsigned-byte 16)  0                    65535)
    ("Int32Array"         :int32   4 (signed-byte 32)   -2147483648           2147483647)
    ("Uint32Array"        :uint32  4 (unsigned-byte 32)  0                    4294967295)
    ("Float32Array"       :float32 4 single-float        nil                  nil)
    ("Float64Array"       :float64 8 double-float         nil                  nil)
    ;; ES2020 BigInt typed arrays (backed by signed-byte 64 / unsigned-byte 64)
    ("BigInt64Array"      :bigint64  8 (signed-byte 64)  -9223372036854775808  9223372036854775807)
    ("BigUint64Array"     :biguint64 8 (unsigned-byte 64) 0                   18446744073709551615))
  "TypedArray type specs: (name tag bytes cl-type min max).")

(defun %js-ta-clamp (val min max)
  "Clamp VAL to [MIN, MAX] for Uint8ClampedArray."
  (if (or (null min) (null max)) val
      (max min (min max (truncate val)))))

(defun %js-ta-coerce-element (type-tag val)
  "Coerce VAL to the appropriate element type for TYPE-TAG."
  (let ((n (truncate (%js-to-number val))))
    (case type-tag
      (:int8    (let ((x (logand n #xFF)))
                  (if (>= x 128) (- x 256) x)))
      (:uint8   (logand n #xFF))
      (:uint8c  (%js-ta-clamp n 0 255))
      (:int16   (let ((x (logand n #xFFFF)))
                  (if (>= x 32768) (- x 65536) x)))
      (:uint16  (logand n #xFFFF))
      (:int32   (let ((x (logand n #xFFFFFFFF)))
                  (if (>= x 2147483648) (- x 4294967296) x)))
      (:uint32  (logand n #xFFFFFFFF))
      (:float32 (coerce (%js-to-number val) 'single-float))
      (:float64 (coerce (%js-to-number val) 'double-float))
      (:bigint64
       (let ((v (if (js-bigint-p val) (js-bigint-value val) (truncate (%js-to-number val)))))
         (let ((x (logand v #xFFFFFFFFFFFFFFFF)))
           (if (>= x 9223372036854775808) (- x 18446744073709551616) x))))
      (:biguint64
       (let ((v (if (js-bigint-p val) (js-bigint-value val) (truncate (%js-to-number val)))))
         (logand v #xFFFFFFFFFFFFFFFF)))
      (t n))))

(defun %js-make-typed-array (type-name &optional (arg +js-undefined+))
  "Construct a TypedArray of TYPE-NAME.
ARG can be: length (integer), another TypedArray, an array, or an iterable."
  (let* ((spec (find type-name *js-typed-array-specs* :test #'string= :key #'car))
         (tag  (if spec (second spec) :uint8))
         (esz  (if spec (third spec)  1))
         (n (cond
              ((or (eq arg +js-undefined+) (eq arg +js-null+)) 0)
              ((numberp arg) (truncate arg))
              ((js-typed-array-p arg) (js-ta-length arg))
              ((%js-vec-p arg) (length arg))
              (t 0)))
         (buf (make-array n :element-type t :initial-element 0)))
    ;; Copy elements if source is a TypedArray or plain array
    (cond
      ((js-typed-array-p arg)
       (let ((src-buf (js-ta-buffer arg))
             (src-tag (js-ta-type-name arg)))
         (dotimes (i (min n (js-ta-length arg)))
           (setf (aref buf i)
                 (%js-ta-coerce-element tag (aref src-buf i))))))
      ((%js-vec-p arg)
       (dotimes (i (min n (length arg)))
         (setf (aref buf i) (%js-ta-coerce-element tag (aref arg i))))))
    (make-js-typed-array :type-name type-name :element-size esz
                          :buffer buf :byte-offset 0 :length n)))

;;; -----------------------------------------------------------------------
;;;  Element access
;;; -----------------------------------------------------------------------

(defun %js-ta-get (ta index)
  "Get element at INDEX from TypedArray TA."
  (let ((i (truncate (%js-to-number index))))
    (if (and (>= i 0) (< i (js-ta-length ta)))
        (aref (js-ta-buffer ta) i)
        +js-undefined+)))

(defun %js-ta-set (ta index value)
  "Set element at INDEX in TypedArray TA to VALUE."
  (let ((i (truncate (%js-to-number index))))
    (when (and (>= i 0) (< i (js-ta-length ta)))
      (setf (aref (js-ta-buffer ta) i)
            (%js-ta-coerce-element (js-ta-type-name ta) value))))
  value)

;;; -----------------------------------------------------------------------
;;;  TypedArray prototype methods
;;; -----------------------------------------------------------------------

(defun %js-ta-set-from (ta source &optional (offset 0))
  "TypedArray.prototype.set(array, offset)."
  (let ((off (truncate (%js-to-number offset))))
    (cond
      ((js-typed-array-p source)
       (dotimes (i (js-ta-length source))
         (when (< (+ off i) (js-ta-length ta))
           (setf (aref (js-ta-buffer ta) (+ off i))
                 (%js-ta-coerce-element (js-ta-type-name ta) (aref (js-ta-buffer source) i))))))
      ((%js-vec-p source)
       (dotimes (i (length source))
         (when (< (+ off i) (js-ta-length ta))
           (setf (aref (js-ta-buffer ta) (+ off i))
                 (%js-ta-coerce-element (js-ta-type-name ta) (aref source i)))))))
    ta))

(defun %js-ta-subarray (ta begin &optional end)
  "TypedArray.prototype.subarray(begin, end)."
  (let* ((n (js-ta-length ta))
         (b (if (< begin 0) (max 0 (+ n begin)) (min begin n)))
         (e (if (null end) n (if (< end 0) (max 0 (+ n end)) (min end n))))
         (new-len (max 0 (- e b)))
         (new-buf (subseq (js-ta-buffer ta) b (+ b new-len))))
    (make-js-typed-array :type-name (js-ta-type-name ta)
                          :element-size (js-ta-element-size ta)
                          :buffer new-buf
                          :byte-offset (* b (js-ta-element-size ta))
                          :length new-len)))

(defun %js-ta-slice (ta &optional (begin 0) end)
  "TypedArray.prototype.slice — returns a copy."
  (%js-ta-subarray ta begin end))

(defun %js-ta-fill (ta value &optional (begin 0) end)
  "TypedArray.prototype.fill(value, begin, end)."
  (let* ((n (js-ta-length ta))
         (b (if (< begin 0) (max 0 (+ n begin)) (min begin n)))
         (e (if (null end) n (if (< end 0) (max 0 (+ n end)) (min end n))))
         (coerced (%js-ta-coerce-element (js-ta-type-name ta) value)))
    (loop for i from b below e
          do (setf (aref (js-ta-buffer ta) i) coerced)))
  ta)

(defun %js-ta-to-array (ta)
  "Convert TypedArray to plain JS array."
  (let* ((n (js-ta-length ta))
         (result (make-array n :element-type t :adjustable t :fill-pointer n)))
    (dotimes (i n)
      (setf (aref result i) (coerce (aref (js-ta-buffer ta) i) 'double-float)))
    result))

(defun %js-ta-index-of (ta search-element)
  "TypedArray.prototype.indexOf."
  (let ((target (%js-ta-coerce-element (js-ta-type-name ta) search-element)))
    (loop for i below (js-ta-length ta)
          when (= (aref (js-ta-buffer ta) i) target) return i
          finally (return -1))))

(defun %js-ta-includes (ta search-element)
  "TypedArray.prototype.includes."
  (not (= -1 (%js-ta-index-of ta search-element))))

(defun %js-ta-join (ta &optional (sep ","))
  "TypedArray.prototype.join."
  (let ((sep-str (%js-to-string sep)))
    (with-output-to-string (out)
      (dotimes (i (js-ta-length ta))
        (when (> i 0) (write-string sep-str out))
        (write-string (format nil "~A" (aref (js-ta-buffer ta) i)) out)))))

(defun %js-ta-for-each (ta fn)
  "TypedArray.prototype.forEach."
  (dotimes (i (js-ta-length ta))
    (%js-funcall fn (coerce (aref (js-ta-buffer ta) i) 'double-float) i ta))
  +js-undefined+)

(defun %js-ta-map (ta fn)
  "TypedArray.prototype.map."
  (let* ((n (js-ta-length ta))
         (result (%js-make-typed-array (js-ta-type-name ta) n)))
    (dotimes (i n)
      (%js-ta-set result i (%js-funcall fn (coerce (aref (js-ta-buffer ta) i) 'double-float) i ta)))
    result))

(defun %js-ta-filter (ta fn)
  "TypedArray.prototype.filter."
  (let ((results nil))
    (dotimes (i (js-ta-length ta))
      (let ((v (coerce (aref (js-ta-buffer ta) i) 'double-float)))
        (when (%js-truthy (%js-funcall fn v i ta))
          (push v results))))
    (let* ((filtered (nreverse results))
           (result (%js-make-typed-array (js-ta-type-name ta) (length filtered))))
      (loop for v in filtered for i from 0
            do (%js-ta-set result i v))
      result)))

(defun %js-ta-reduce (ta fn &optional (init +js-undefined+))
  "TypedArray.prototype.reduce."
  (let ((acc init) (first-p (eq init +js-undefined+)))
    (dotimes (i (js-ta-length ta))
      (let ((v (coerce (aref (js-ta-buffer ta) i) 'double-float)))
        (if first-p
            (setf acc v first-p nil)
            (setf acc (%js-funcall fn acc v i ta)))))
    acc))

;;; -----------------------------------------------------------------------
;;;  Method dispatch table for TypedArrays
;;; -----------------------------------------------------------------------

;;; ─── ES2023 non-mutating TypedArray methods ──────────────────────────────────

(defun %js-ta-to-reversed (ta)
  "TypedArray.prototype.toReversed() — reversed copy."
  (let* ((n (js-ta-length ta))
         (new-buf (make-array n :element-type t :initial-element 0)))
    (dotimes (i n)
      (setf (aref new-buf i) (aref (js-ta-buffer ta) (- n 1 i))))
    (make-js-typed-array :type-name (js-ta-type-name ta)
                          :element-size (js-ta-element-size ta)
                          :buffer new-buf :byte-offset 0 :length n)))

(defun %js-ta-to-sorted (ta &optional compare-fn)
  "TypedArray.prototype.toSorted() — sorted copy."
  (let* ((n (js-ta-length ta))
         (arr (copy-seq (js-ta-buffer ta)))
         (new-buf (make-array n :element-type t :initial-element 0)))
    (let ((sorted (if compare-fn
                      (sort (coerce arr 'list)
                            (lambda (a b) (< (%js-to-number (%js-funcall compare-fn a b)) 0)))
                      (sort (coerce arr 'list) #'<))))
      (loop for v in sorted for i from 0 do (setf (aref new-buf i) v)))
    (make-js-typed-array :type-name (js-ta-type-name ta)
                          :element-size (js-ta-element-size ta)
                          :buffer new-buf :byte-offset 0 :length n)))

(defun %js-ta-with (ta index value)
  "TypedArray.prototype.with() — copy with one element replaced."
  (let* ((n (js-ta-length ta))
         (i (if (< index 0) (+ n index) index))
         (new-buf (copy-seq (js-ta-buffer ta))))
    (setf (aref new-buf i) (%js-ta-coerce-element (js-ta-type-name ta) value))
    (make-js-typed-array :type-name (js-ta-type-name ta)
                          :element-size (js-ta-element-size ta)
                          :buffer new-buf :byte-offset 0 :length n)))

(defun %js-ta-at (ta index)
  "TypedArray.prototype.at() — negative indexing."
  (let* ((n (js-ta-length ta))
         (i (if (< index 0) (+ n index) index)))
    (if (or (< i 0) (>= i n))
        +js-undefined+
        (coerce (aref (js-ta-buffer ta) i) 'double-float))))

(defun %js-ta-find (ta fn)
  "TypedArray.prototype.find()."
  (dotimes (i (js-ta-length ta) +js-undefined+)
    (let ((v (coerce (aref (js-ta-buffer ta) i) 'double-float)))
      (when (%js-truthy (%js-funcall fn v i ta))
        (return v)))))

(defun %js-ta-find-index (ta fn)
  "TypedArray.prototype.findIndex()."
  (dotimes (i (js-ta-length ta) -1.0d0)
    (let ((v (coerce (aref (js-ta-buffer ta) i) 'double-float)))
      (when (%js-truthy (%js-funcall fn v i ta))
        (return (coerce i 'double-float))))))

(defun %js-ta-find-last (ta fn)
  "TypedArray.prototype.findLast()."
  (loop for i from (1- (js-ta-length ta)) downto 0
        do (let ((v (coerce (aref (js-ta-buffer ta) i) 'double-float)))
             (when (%js-truthy (%js-funcall fn v i ta))
               (return v)))
        finally (return +js-undefined+)))

(defun %js-ta-find-last-index (ta fn)
  "TypedArray.prototype.findLastIndex()."
  (loop for i from (1- (js-ta-length ta)) downto 0
        do (let ((v (coerce (aref (js-ta-buffer ta) i) 'double-float)))
             (when (%js-truthy (%js-funcall fn v i ta))
               (return (coerce i 'double-float))))
        finally (return -1.0d0)))

(defun %js-ta-every (ta fn)
  "TypedArray.prototype.every()."
  (dotimes (i (js-ta-length ta) t)
    (unless (%js-truthy (%js-funcall fn (coerce (aref (js-ta-buffer ta) i) 'double-float) i ta))
      (return nil))))

(defun %js-ta-some (ta fn)
  "TypedArray.prototype.some()."
  (dotimes (i (js-ta-length ta) nil)
    (when (%js-truthy (%js-funcall fn (coerce (aref (js-ta-buffer ta) i) 'double-float) i ta))
      (return t))))

(defun %js-ta-reverse (ta)
  "TypedArray.prototype.reverse() — mutating."
  (let ((n (js-ta-length ta)) (buf (js-ta-buffer ta)))
    (loop for i below (floor n 2)
          do (rotatef (aref buf i) (aref buf (- n 1 i)))))
  ta)

(defun %js-ta-sort (ta &optional compare-fn)
  "TypedArray.prototype.sort() — mutating in-place sort."
  (let* ((n (js-ta-length ta))
         (sorted (if compare-fn
                     (sort (coerce (js-ta-buffer ta) 'list)
                           (lambda (a b) (< (%js-to-number (%js-funcall compare-fn a b)) 0)))
                     (sort (coerce (js-ta-buffer ta) 'list) #'<))))
    (loop for v in sorted for i from 0 do (setf (aref (js-ta-buffer ta) i) v)))
  ta)

(defun %js-ta-copy-within (ta target &optional (start 0) end)
  "TypedArray.prototype.copyWithin()."
  (let* ((n (js-ta-length ta))
         (t1 (if (< target 0) (max 0 (+ n target)) (min target n)))
         (s  (if (< start  0) (max 0 (+ n start))  (min start  n)))
         (e  (if (null end) n (if (< end 0) (max 0 (+ n end)) (min end n)))))
    (let ((src (subseq (js-ta-buffer ta) s e)))
      (loop for v across src for i from t1 while (< i n)
            do (setf (aref (js-ta-buffer ta) i) v))))
  ta)

(defun %js-ta-last-index-of (ta val)
  "TypedArray.prototype.lastIndexOf()."
  (let ((target (%js-ta-coerce-element (js-ta-type-name ta) val)))
    (loop for i from (1- (js-ta-length ta)) downto 0
          when (= (aref (js-ta-buffer ta) i) target) return (coerce i 'double-float)
          finally (return -1.0d0))))

(defun %js-ta-values (ta)
  "TypedArray.prototype.values() — returns an iterator."
  (let ((i (list 0)) (n (js-ta-length ta)) (buf (js-ta-buffer ta)))
    (%js-make-object
     "next" (lambda ()
               (if (< (car i) n)
                   (prog1 (%js-make-object "value" (coerce (aref buf (car i)) 'double-float) "done" nil)
                     (incf (car i)))
                   (%js-make-object "value" +js-undefined+ "done" t)))
     "@@iterator" (lambda () (gethash "@@iterator" (%js-ta-values ta))))))

(defun %js-ta-keys (ta)
  "TypedArray.prototype.keys()."
  (let ((i (list 0)) (n (js-ta-length ta)))
    (%js-make-object
     "next" (lambda ()
               (if (< (car i) n)
                   (prog1 (%js-make-object "value" (coerce (car i) 'double-float) "done" nil)
                     (incf (car i)))
                   (%js-make-object "value" +js-undefined+ "done" t))))))

(defun %js-ta-entries (ta)
  "TypedArray.prototype.entries()."
  (let ((i (list 0)) (n (js-ta-length ta)) (buf (js-ta-buffer ta)))
    (%js-make-object
     "next" (lambda ()
               (if (< (car i) n)
                   (let ((pair (%js-make-array (coerce (car i) 'double-float)
                                               (coerce (aref buf (car i)) 'double-float))))
                     (prog1 (%js-make-object "value" pair "done" nil)
                       (incf (car i))))
                   (%js-make-object "value" +js-undefined+ "done" t))))))

;;; ─── ES2025: Uint8Array hex/base64 encoding ──────────────────────────────────

(defun %js-uint8-to-hex (ta)
  "Uint8Array.prototype.toHex() — ES2025."
  (with-output-to-string (out)
    (dotimes (i (js-ta-length ta))
      (format out "~2,'0x" (aref (js-ta-buffer ta) i)))))

(defun %js-uint8-from-hex (hex-str)
  "Uint8Array.fromHex(string) — ES2025."
  (let* ((s (%js-to-string hex-str))
         (n (floor (length s) 2))
         (ta (%js-make-typed-array "Uint8Array" n)))
    (dotimes (i n ta)
      (setf (aref (js-ta-buffer ta) i)
            (parse-integer s :start (* 2 i) :end (* 2 (1+ i)) :radix 16)))))

(defun %js-uint8-to-base64 (ta &optional opts)
  "Uint8Array.prototype.toBase64() — ES2025."
  (declare (ignore opts))
  (let* ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (n (js-ta-length ta))
         (buf (js-ta-buffer ta)))
    (with-output-to-string (out)
      (loop for i from 0 below n by 3
            do (let* ((b0 (aref buf i))
                      (b1 (if (< (1+ i) n) (aref buf (1+ i)) 0))
                      (b2 (if (< (+ i 2) n) (aref buf (+ i 2)) 0)))
                 (write-char (char alphabet (ash b0 -2)) out)
                 (write-char (char alphabet (logior (ash (logand b0 3) 4) (ash b1 -4))) out)
                 (write-char (if (< (1+ i) n) (char alphabet (logior (ash (logand b1 15) 2) (ash b2 -6))) #\=) out)
                 (write-char (if (< (+ i 2) n) (char alphabet (logand b2 63)) #\=) out))))))

(defun %js-uint8-from-base64 (b64-str &optional opts)
  "Uint8Array.fromBase64(string) — ES2025."
  (declare (ignore opts))
  (let* ((s (string-trim '(#\Space #\Tab #\Newline #\Return) (%js-to-string b64-str)))
         (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((dc (ch) (or (position ch alphabet) 0)))
      (loop for i from 0 below (length s) by 4
            when (< (+ i 3) (length s))
            do (let* ((g0 (dc (char s i)))
                      (g1 (dc (char s (1+ i))))
                      (g2 (if (char= (char s (+ i 2)) #\=) 0 (dc (char s (+ i 2)))))
                      (g3 (if (char= (char s (+ i 3)) #\=) 0 (dc (char s (+ i 3))))))
                 (vector-push-extend (logior (ash g0 2) (ash g1 -4)) bytes)
                 (unless (char= (char s (+ i 2)) #\=)
                   (vector-push-extend (logior (ash (logand g1 15) 4) (ash g2 -2)) bytes))
                 (unless (char= (char s (+ i 3)) #\=)
                   (vector-push-extend (logior (ash (logand g2 3) 6) g3) bytes)))))
    (let ((ta (%js-make-typed-array "Uint8Array" (length bytes))))
      (loop for b across bytes for i from 0 do (setf (aref (js-ta-buffer ta) i) b))
      ta)))

(defparameter *js-typed-array-method-table*
  (list (cons "set"           #'%js-ta-set-from)
        (cons "subarray"      #'%js-ta-subarray)
        (cons "slice"         #'%js-ta-slice)
        (cons "fill"          #'%js-ta-fill)
        (cons "indexOf"       #'%js-ta-index-of)
        (cons "lastIndexOf"   #'%js-ta-last-index-of)
        (cons "includes"      #'%js-ta-includes)
        (cons "join"          #'%js-ta-join)
        (cons "forEach"       #'%js-ta-for-each)
        (cons "map"           #'%js-ta-map)
        (cons "filter"        #'%js-ta-filter)
        (cons "reduce"        #'%js-ta-reduce)
        (cons "find"          #'%js-ta-find)
        (cons "findIndex"     #'%js-ta-find-index)
        (cons "findLast"      #'%js-ta-find-last)
        (cons "findLastIndex" #'%js-ta-find-last-index)
        (cons "every"         #'%js-ta-every)
        (cons "some"          #'%js-ta-some)
        (cons "reverse"       #'%js-ta-reverse)
        (cons "sort"          #'%js-ta-sort)
        (cons "copyWithin"    #'%js-ta-copy-within)
        (cons "at"            #'%js-ta-at)
        (cons "toReversed"    #'%js-ta-to-reversed)
        (cons "toSorted"      #'%js-ta-to-sorted)
        (cons "with"          #'%js-ta-with)
        (cons "values"        #'%js-ta-values)
        (cons "keys"          #'%js-ta-keys)
        (cons "entries"       #'%js-ta-entries)
        (cons "toArray"       #'%js-ta-to-array)
        (cons "@@iterator"    #'%js-ta-values)
        ;; ES2025 Uint8Array hex/base64
        (cons "toHex"         #'%js-uint8-to-hex)
        (cons "toBase64"      #'%js-uint8-to-base64))
  "TypedArray.prototype method dispatch.")
