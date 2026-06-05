;;;; packages/javascript/src/runtime-string.lisp — JS String, Number, Regex, Math built-ins
;;;;
;;;; String methods, numeric helpers, regex stubs, and Math functions.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  String methods
;;; -----------------------------------------------------------------------

(defun %js-string-length (s)
  (length s))

(defun %js-string-slice (s &optional (start 0) (end nil))
  "JS String.prototype.slice."
  (let* ((n (length s))
         (st (if (< start 0) (max 0 (+ n start)) (min start n)))
         (en (if (null end) n (if (< end 0) (max 0 (+ n end)) (min end n)))))
    (if (>= st en) "" (subseq s st en))))

(defun %js-string-index-of (s sub &optional (from 0))
  "JS String.prototype.indexOf."
  (let* ((n (length s))
         (st (if (< from 0) 0 (min from n)))
         (found (search sub s :start2 st)))
    (if found found -1)))

(defun %js-string-last-index-of (s sub &optional (from nil))
  "JS String.prototype.lastIndexOf."
  (let* ((n (length s))
         (end (if (null from) n (min n (+ (or from n) (length sub)))))
         (found (search sub s :from-end t :end2 end)))
    (if found found -1)))

(defun %js-string-includes (s sub &optional (from 0))
  "JS String.prototype.includes."
  (not (eql -1 (%js-string-index-of s sub from))))

(defun %js-string-starts-with (s prefix &optional (pos 0))
  (let ((plen (length prefix))
        (slen (length s)))
    (and (<= (+ pos plen) slen)
         (string= s prefix :start1 pos :end1 (+ pos plen)))))

(defun %js-string-ends-with (s suffix &optional (end-pos nil))
  (let* ((slen (length s))
         (ep (if (null end-pos) slen (min end-pos slen)))
         (suflen (length suffix)))
    (and (>= ep suflen)
         (string= s suffix :start1 (- ep suflen) :end1 ep))))

(defun %js-string-split (s &optional (sep nil) (limit nil))
  "JS String.prototype.split."
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (cond
      ((or (eq sep +js-undefined+) (null sep))
       (vector-push-extend s result))
      ((string= sep "")
       (loop for ch across s
             do (vector-push-extend (string ch) result)))
      (t
       (let ((seplen (length sep))
             (pos 0)
             (slen (length s)))
         (loop
           (let ((found (search sep s :start2 pos)))
             (unless found (return))
             (vector-push-extend (subseq s pos found) result)
             (setf pos (+ found seplen))
             (when (and limit (>= (length result) limit)) (return))))
         (unless (and limit (>= (length result) limit))
           (vector-push-extend (subseq s pos) result)))))
    (if (and limit (%js-truthy limit))
        (%js-array-slice result 0 limit)
        result)))

(defun %js-string-replace (s pattern replacement)
  "JS String.prototype.replace (string pattern only)."
  (let* ((pat (%js-to-string pattern))
         (patlen (length pat))
         (found (search pat s)))
    (if found
        (concatenate 'string
                     (subseq s 0 found)
                     (%js-to-string replacement)
                     (subseq s (+ found patlen)))
        s)))

(defun %js-string-replace-all (s pattern replacement)
  "JS String.prototype.replaceAll."
  (let* ((pat (%js-to-string pattern))
         (rep (%js-to-string replacement))
         (patlen (length pat)))
    (with-output-to-string (out)
      (let ((pos 0))
        (loop
          (let ((found (search pat s :start2 pos)))
            (unless found
              (write-string (subseq s pos) out)
              (return))
            (write-string (subseq s pos found) out)
            (write-string rep out)
            (setf pos (+ found (max 1 patlen)))))))))

;;; Macro for JS string methods that are simple CL built-in wrappers.
;;; Usage: (define-js-string-passthrough %js-string-X cl-fn &rest fixed-args)
;;; generates (defun %js-string-X (s) (cl-fn fixed-args... s))
(defmacro define-js-string-passthrough (name cl-fn &rest fixed-args)
  `(defun ,name (s)
     (,cl-fn ,@fixed-args s)))

(define-js-string-passthrough %js-string-to-lower-case string-downcase)
(define-js-string-passthrough %js-string-to-upper-case string-upcase)

(defparameter +js-whitespace-chars+ '(#\Space #\Tab #\Newline #\Return #\Page)
  "Characters treated as whitespace by JS String.prototype.trim methods.")

(define-js-string-passthrough %js-string-trim       string-trim      +js-whitespace-chars+)
(define-js-string-passthrough %js-string-trim-start string-left-trim +js-whitespace-chars+)
(define-js-string-passthrough %js-string-trim-end   string-right-trim +js-whitespace-chars+)

(defun %js-string-pad-start (s len &optional (fill " "))
  (let* ((fl (if (eq fill +js-undefined+) " " fill))
         (need (- len (length s))))
    (if (<= need 0)
        s
        (let ((pad (make-string need :initial-element #\Space)))
          (loop for i below need
                do (setf (char pad i) (char fl (mod i (max 1 (length fl))))))
          (concatenate 'string pad s)))))

(defun %js-string-pad-end (s len &optional (fill " "))
  (let* ((fl (if (eq fill +js-undefined+) " " fill))
         (need (- len (length s))))
    (if (<= need 0)
        s
        (let ((pad (make-string need :initial-element #\Space)))
          (loop for i below need
                do (setf (char pad i) (char fl (mod i (max 1 (length fl))))))
          (concatenate 'string s pad)))))

(defun %js-string-at (s index)
  "JS String.prototype.at (negative indexing)."
  (let* ((n (length s))
         (i (if (< index 0) (+ n index) index)))
    (if (or (< i 0) (>= i n))
        +js-undefined+
        (string (char s i)))))

(defun %js-string-repeat (s n)
  "Repeat S n times."
  (if (<= n 0)
      ""
      (with-output-to-string (out)
        (loop repeat n do (write-string s out)))))

(defun %js-string-char-at (s i)
  (if (or (< i 0) (>= i (length s)))
      ""
      (string (char s i))))

(defun %js-string-char-code-at (s i)
  (if (or (< i 0) (>= i (length s)))
      :js-nan
      (char-code (char s i))))


(defun %js-string-concat (s &rest others)
  (apply #'concatenate 'string s (mapcar #'%js-to-string others)))

(defun %js-string-match (s pattern)
  "Simplified string match (pattern is a string)."
  (let ((found (search pattern s)))
    (if found
        (%js-make-array (subseq s found (+ found (length pattern))))
        +js-null+)))

(defun %js-string-match-all (s pattern)
  "Simplified matchAll — returns array of match arrays."
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0))
        (pos 0)
        (patlen (max 1 (length pattern))))
    (loop
      (let ((found (search pattern s :start2 pos)))
        (unless found (return))
        (vector-push-extend (%js-make-array (subseq s found (+ found (length pattern)))) result)
        (setf pos (+ found patlen))))
    result))

(defun %js-string-search (s pattern)
  "Return index of first match or -1."
  (let ((found (search pattern s)))
    (if found found -1)))

(defun %js-string-from-char-code (&rest codes)
  "String.fromCharCode / String.fromCodePoint — both map code-char over their args."
  (coerce (mapcar #'code-char codes) 'string))

(defun %js-string-from-code-point (&rest codes)
  (apply #'%js-string-from-char-code codes))

(defun %js-string-raw (template &rest substitutions)
  "String.raw tag function."
  (let ((raw (if (%js-ht-p template)
                 (gethash "raw" template)
                 template)))
    (with-output-to-string (out)
      (loop for i below (length raw)
            do (write-string (%js-to-string (aref raw i)) out)
               (when (< i (length substitutions))
                 (write-string (%js-to-string (nth i substitutions)) out))))))

;;; -----------------------------------------------------------------------
;;;  Math
;;; -----------------------------------------------------------------------

(defun %js-math-abs (x)
  (let ((n (%js-to-number x)))
    (if (%js-float-nan-p n) n (abs n))))

(defun %js-math-floor (x)
  (let ((n (%js-to-number x)))
    (if (or (%js-float-nan-p n) (%js-float-infinity-p n))
        n
        (coerce (floor n) 'double-float))))

(defun %js-math-ceil (x)
  (let ((n (%js-to-number x)))
    (if (or (%js-float-nan-p n) (%js-float-infinity-p n))
        n
        (coerce (ceiling n) 'double-float))))

(defun %js-math-round (x)
  "JS Math.round — rounds half away from negative infinity."
  (let ((n (%js-to-number x)))
    (if (or (%js-float-nan-p n) (%js-float-infinity-p n))
        n
        (coerce (floor (+ n 0.5d0)) 'double-float))))

(defun %js-math-trunc (x)
  (let ((n (%js-to-number x)))
    (if (or (%js-float-nan-p n) (%js-float-infinity-p n))
        n
        (coerce (truncate n) 'double-float))))

(defun %js-math-sign (x)
  (let ((n (%js-to-number x)))
    (cond
      ((%js-float-nan-p n) n)
      ((> n 0)  1.0d0)
      ((< n 0) -1.0d0)
      (t         0.0d0))))

(defun %js-math-max (&rest args)
  (if (null args)
      *js-neg-inf-float*
      (reduce (lambda (a b)
                (let ((na (%js-to-number a))
                        (nb (%js-to-number b)))
                  (if (or (%js-float-nan-p na)
                          (%js-float-nan-p nb))
                      *js-nan-float*
                      (max na nb))))
              args)))

(defun %js-math-min (&rest args)
  (if (null args)
      *js-inf-float*
      (reduce (lambda (a b)
                (let ((na (%js-to-number a))
                        (nb (%js-to-number b)))
                  (if (or (%js-float-nan-p na)
                          (%js-float-nan-p nb))
                      *js-nan-float*
                      (min na nb))))
              args)))

(defun %js-math-pow (base exp)
  (expt (%js-to-number base) (%js-to-number exp)))

(defun %js-math-sqrt (x)
  (let ((n (%js-to-number x)))
    (if (< n 0)
        *js-nan-float*
        (sqrt n))))

(defun %js-math-random ()
  (random 1.0d0))

(defun %js-math-log (x)
  (let ((n (%js-to-number x)))
    (if (< n 0) *js-nan-float*
        (log n))))

(defun %js-math-log2 (x)
  (let ((n (%js-to-number x)))
    (if (< n 0) *js-nan-float*
        (log n 2.0d0))))

(defun %js-math-log10 (x)
  (let ((n (%js-to-number x)))
    (if (< n 0) *js-nan-float*
        (log n 10.0d0))))

(defun %js-math-exp (x)
  (exp (%js-to-number x)))

(defun %js-math-sin (x)  (sin  (%js-to-number x)))
(defun %js-math-cos (x)  (cos  (%js-to-number x)))
(defun %js-math-tan (x)  (tan  (%js-to-number x)))
(defun %js-math-asin (x) (asin (%js-to-number x)))
(defun %js-math-acos (x) (acos (%js-to-number x)))
(defun %js-math-atan (x) (atan (%js-to-number x)))

(defun %js-math-atan2 (y x)
  (atan (%js-to-number y) (%js-to-number x)))

(defun %js-math-hypot (&rest args)
  (sqrt (reduce #'+ (mapcar (lambda (x)
                               (let ((n (%js-to-number x)))
                                 (* n n)))
                             args)
                :initial-value 0.0d0)))

(defun %js-math-clz32 (x)
  "Count leading zeros in 32-bit integer representation."
  (let* ((n (logand (truncate (%js-to-number x)) #xFFFFFFFF))
         (count 0))
    (if (zerop n)
        32
        (progn
          (loop for bit from 31 downto 0
                while (zerop (logand n (ash 1 bit)))
                do (incf count))
          count))))

(defun %js-math-fround (x)
  "Round to nearest single-precision float."
  (coerce (coerce (%js-to-number x) 'single-float) 'double-float))

(defun %js-math-imul (a b)
  "32-bit integer multiply."
  (let* ((ia (logand (truncate (%js-to-number a)) #xFFFFFFFF))
         (ib (logand (truncate (%js-to-number b)) #xFFFFFFFF))
         (result (logand (* ia ib) #xFFFFFFFF)))
    ;; sign-extend
    (if (logbitp 31 result)
        (- result #x100000000)
        result)))
