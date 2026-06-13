;;;; packages/javascript/src/runtime-typed-arrays-methods.lisp — ES2023+ TypedArray methods
;;;;
;;;; ES2023 non-mutating methods (toReversed, toSorted, with, at, findLast, etc.)
;;;; ES2025 Uint8Array hex/base64 encoding (toHex, toBase64, fromHex, fromBase64)
;;;; *js-typed-array-methods* dispatch table (references fns from both files)
;;;;
;;;; Load order: after runtime-typed-arrays.lisp (needs struct, coerce helpers,
;;;; %js-ta-* bulk methods defined there).

(in-package :cl-cc/javascript)

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
