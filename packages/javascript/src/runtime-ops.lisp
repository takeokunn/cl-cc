;;;; packages/javascript/src/runtime-ops.lisp — JS operator runtime helpers

(in-package :cl-cc/javascript)

;;; ─── 32-bit integer coercion helpers ─────────────────────────────────────────

(defun %js-to-int32 (x)
  "Coerce X to an unsigned 32-bit integer (JS ToInt32 step 1)."
  (logand (truncate (%js-to-number x)) #xFFFFFFFF))

(defun %js-sign-extend32 (n)
  "Sign-extend a 32-bit value: if bit 31 is set, fold into the negative range."
  (if (logbitp 31 n) (- n #x100000000) n))

;;; ─── Bitwise binary operators ─────────────────────────────────────────────────

(defmacro define-js-bitwise-binop (name cl-op docstring)
  "Define a JS bitwise binary operator that coerces operands to 32-bit integers."
  `(defun ,name (a b)
     ,docstring
     (%js-sign-extend32 (,cl-op (%js-to-int32 a) (%js-to-int32 b)))))

(defun %js-bitwise-not (x)
  "JS ~x: flip all 32 bits then sign-extend."
  (%js-sign-extend32 (logxor (%js-to-int32 x) #xFFFFFFFF)))

(define-js-bitwise-binop %js-bitwise-or  logior "JS a | b.")
(define-js-bitwise-binop %js-bitwise-and logand "JS a & b.")
(define-js-bitwise-binop %js-bitwise-xor logxor "JS a ^ b.")

;;; ─── Shift operators ──────────────────────────────────────────────────────────

(defun %js-shift-left (a b)
  "JS a << b (b taken mod 32)."
  (let ((ia    (%js-to-int32 a))
        (shift (mod (logand (%js-to-int32 b) #x1F) 32)))
    (%js-sign-extend32 (logand (ash ia shift) #xFFFFFFFF))))

(defun %js-shift-right (a b)
  "JS a >> b, arithmetic (signed). b taken mod 32."
  (let ((ia    (%js-sign-extend32 (%js-to-int32 a)))
        (shift (mod (logand (%js-to-int32 b) #x1F) 32)))
    (ash ia (- shift))))

(defun %js-unsigned-shift-right (a b)
  "JS a >>> b, logical (unsigned, always non-negative). b taken mod 32."
  (ash (%js-to-int32 a)
       (- (mod (logand (%js-to-int32 b) #x1F) 32))))

;;; ─── Unary operators ──────────────────────────────────────────────────────────

(defun %js-unary-plus (x)
  "JS unary +: numeric coercion."
  (%js-to-number x))

;;; ─── Increment / decrement on non-variable targets ───────────────────────────
;;; These are fallbacks for ++/-- on non-assignable expressions.

(defun %js-postfix-inc (val) val)
(defun %js-postfix-dec (val) val)
(defun %js-prefix-inc  (val) (+ (%js-to-number val) 1))
(defun %js-prefix-dec  (val) (- (%js-to-number val) 1))

;;; ─── Regex literals ───────────────────────────────────────────────────────────
;;; Full RegExp implementation is in runtime-regex.lisp (loaded before this file).
;;; %js-make-regex is defined there as a native NFA-based engine.
;;; This file previously had a stub — removed to avoid shadowing the real impl.

;;; ─── Getter / setter accessor marker ─────────────────────────────────────────

(defun %js-accessor (kind fn)
  "Tag FN as a get/set accessor descriptor. KIND is \"get\" or \"set\"."
  (let ((ht (%js-make-ht)))
    (setf (gethash "__accessor__" ht) t
          (gethash "kind"         ht) kind
          (gethash "fn"           ht) fn)
    ht))

;;; Async wrappers and yield-from live in runtime-async.lisp (separated by concern).

;;; ─── Pattern destructuring assignment (fallback) ──────────────────────────────

(defun %js-assign-pattern (lhs rhs)
  "Fallback for destructuring assignment to a non-simple LHS: returns RHS."
  (declare (ignore lhs))
  rhs)

;;; ─── ES2025 explicit resource management ─────────────────────────────────────

(defun %js-using-register (resource)
  "Register RESOURCE for disposal at scope exit (simplified: identity)."
  resource)

;;; ─── new.target ──────────────────────────────────────────────────────────────

(defun %js-new-target ()
  "Return new.target — undefined outside a constructor."
  +js-undefined+)

;;; ─── BigInt (ES2020) ─────────────────────────────────────────────────────────
;;; js-bigint struct is declared in runtime.lisp (loads before this file).

(defun %js-bigint (x)
  "JS BigInt() constructor: coerce X to a BigInt."
  (cond ((js-bigint-p x) x)
        ((integerp x) (%make-js-bigint x))
        ((floatp x) (%make-js-bigint (truncate x)))
        ((stringp x) (handler-case (%make-js-bigint (parse-integer x :junk-allowed t))
                       (error () (error "Cannot convert ~A to BigInt" x))))
        (t (%make-js-bigint 0))))

(defun %js-bigint-to-string (bi &optional (radix 10))
  "Display a BigInt as string (without the n suffix at runtime).
JS always returns lowercase hex digits, so we normalise via string-downcase."
  (string-downcase (format nil "~vR" radix (js-bigint-value bi))))

;;; Extract integer value from a BigInt or plain number.
(defun %js-bigint-val (x)
  (if (js-bigint-p x) (js-bigint-value x) (truncate x)))

(defmacro define-js-bigint-binop (name cl-op)
  "Define a BigInt binary op (handles mixed BigInt/number operands)."
  `(defun ,name (a b)
     (%make-js-bigint (,cl-op (%js-bigint-val a) (%js-bigint-val b)))))

(define-js-bigint-binop %js-bigint-add +)
(define-js-bigint-binop %js-bigint-sub -)
(define-js-bigint-binop %js-bigint-mul *)
(define-js-bigint-binop %js-bigint-bitwise-and logand)
(define-js-bigint-binop %js-bigint-bitwise-or  logior)
(define-js-bigint-binop %js-bigint-bitwise-xor logxor)

(defmacro define-js-bigint-dividing-op (name cl-op)
  "Define a BigInt division/modulo op that throws on zero divisor."
  `(defun ,name (a b)
     (let ((av (%js-bigint-val a)) (bv (%js-bigint-val b)))
       (when (zerop bv) (error "Division by zero"))
       (%make-js-bigint (,cl-op av bv)))))

(define-js-bigint-dividing-op %js-bigint-div truncate)
(define-js-bigint-dividing-op %js-bigint-mod rem)

(defun %js-bigint-pow (a b)
  (%make-js-bigint (expt (%js-bigint-val a) (%js-bigint-val b))))

(defun %js-bigint-compare (a b)
  "Return -1/0/1 for BigInt comparison (also works if one side is a regular number)."
  (let ((av (%js-bigint-val a)) (bv (%js-bigint-val b)))
    (cond ((< av bv) -1) ((> av bv) 1) (t 0))))

(defun %js-bigint-lshift (a n)
  (%make-js-bigint (ash (%js-bigint-val a) (%js-bigint-val n))))

(defun %js-bigint-rshift (a n)
  (%make-js-bigint (ash (%js-bigint-val a) (- (%js-bigint-val n)))))

(defun %js-bigint-negate (a)
  (%make-js-bigint (- (%js-bigint-val a))))

;;; ─── URI encoding/decoding ───────────────────────────────────────────────────
;;;
;;; Both encode functions share the same percent-encoding loop — they differ
;;; only in the set of "safe" characters that are passed through unchanged.
;;; +uri-component-safe-chars+ omits the URI-structure chars that encodeURI
;;; must preserve.

(defparameter +uri-component-safe-chars+
  '(#\- #\_ #\. #\! #\~ #\* #\' #\( #\))
  "Characters that encodeURIComponent leaves unencoded (RFC 3986 unreserved).")

(defparameter +uri-safe-chars+
  (append +uri-component-safe-chars+
          '(#\; #\/ #\? #\: #\@ #\& #\= #\+ #\$ #\, #\#))
  "Characters that encodeURI leaves unencoded (unreserved + URI structure chars).")

(defun %js-percent-encode (str safe-chars)
  "Percent-encode STR, leaving alphanumerics and SAFE-CHARS unchanged."
  (with-output-to-string (out)
    (loop for ch across (%js-to-string str)
          do (if (or (alphanumericp ch) (member ch safe-chars))
                 (write-char ch out)
                 (loop for byte across (sb-ext:string-to-octets (string ch) :external-format :utf-8)
                       do (format out "%~2,'0X" byte))))))

(defun %js-encode-uri-component (str)
  "JS encodeURIComponent: percent-encode all chars except unreserved."
  (%js-percent-encode str +uri-component-safe-chars+))

(defun %js-decode-uri-component (str)
  "JS decodeURIComponent: decode percent-encoded string."
  (let ((s     (%js-to-string str))
        (bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop with i = 0 and n = (length s)
          while (< i n)
          do (let ((ch (char s i)))
               (if (and (char= ch #\%) (< (+ i 2) n))
                   (progn (vector-push-extend
                           (parse-integer s :start (1+ i) :end (+ i 3) :radix 16) bytes)
                          (incf i 3))
                   (progn (loop for b across (sb-ext:string-to-octets (string ch) :external-format :utf-8)
                                do (vector-push-extend b bytes))
                          (incf i)))))
    (sb-ext:octets-to-string bytes :external-format :utf-8)))

(defun %js-encode-uri (str)
  "JS encodeURI: encode URI, preserving scheme/path/query chars."
  (%js-percent-encode str +uri-safe-chars+))

(defun %js-decode-uri (str)
  "JS decodeURI: decode URI but leave reserved chars encoded."
  (%js-decode-uri-component str))

;;; ─── atob / btoa (base64 in browsers) ───────────────────────────────────────

(defun %js-btoa (str)
  "JS btoa: base64-encode a binary string."
  (let* ((s (%js-to-string str))
         (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (with-output-to-string (out)
      (let ((bytes (map 'vector #'char-code s))
            (n (length s)))
        (loop for i from 0 below n by 3
              do (let* ((b0 (aref bytes i))
                        (b1 (if (< (1+ i) n) (aref bytes (1+ i)) 0))
                        (b2 (if (< (+ i 2) n) (aref bytes (+ i 2)) 0)))
                   (write-char (char alphabet (ash b0 -2)) out)
                   (write-char (char alphabet (logior (ash (logand b0 3) 4) (ash b1 -4))) out)
                   (write-char (if (< (1+ i) n) (char alphabet (logior (ash (logand b1 15) 2) (ash b2 -6))) #\=) out)
                   (write-char (if (< (+ i 2) n) (char alphabet (logand b2 63)) #\=) out)))))))

(defun %js-atob (str)
  "JS atob: decode base64 string."
  (let* ((s (string-trim '(#\Space #\Tab #\Newline #\Return) (%js-to-string str)))
         (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (flet ((dc (ch) (or (position ch alphabet) 0)))
      (with-output-to-string (out)
        (loop for i from 0 below (length s) by 4
              while (< (+ i 3) (length s))
              do (let* ((g0 (dc (char s i)))       (g1 (dc (char s (1+ i))))
                        (g2 (if (char= (char s (+ i 2)) #\=) 0 (dc (char s (+ i 2)))))
                        (g3 (if (char= (char s (+ i 3)) #\=) 0 (dc (char s (+ i 3))))))
                   (write-char (code-char (logior (ash g0 2) (ash g1 -4))) out)
                   (unless (char= (char s (+ i 2)) #\=)
                     (write-char (code-char (logior (ash (logand g1 15) 4) (ash g2 -2))) out))
                   (unless (char= (char s (+ i 3)) #\=)
                     (write-char (code-char (logior (ash (logand g2 3) 6) g3)) out))))))))

;;; ─── TextEncoder / TextDecoder ───────────────────────────────────────────────

(defun %js-utf8-octets (value)
  (sb-ext:string-to-octets (%js-to-string value) :external-format :utf-8))

(defun %js-text-decoder-encoding (encoding)
  (let ((name (string-downcase
               (%js-to-string
                (if (or (null encoding)
                        (eq encoding +js-undefined+)
                        (eq encoding +js-null+))
                    "utf-8"
                    encoding)))))
    (cond
      ((or (string= name "utf8") (string= name "unicode-1-1-utf-8"))
       "utf-8")
      (t name))))

(defun %js-text-decode-octets (bytes)
  (handler-case
      (sb-ext:octets-to-string bytes :external-format :utf-8)
    (error () "")))

(defun %js-text-buffer-octets (buf)
  (cond
    ((js-typed-array-p buf)
     (let* ((len (js-ta-length buf))
            (bytes (make-array len :element-type '(unsigned-byte 8))))
       (dotimes (i len bytes)
         (setf (aref bytes i) (logand #xFF (truncate (%js-ta-get buf i)))))))
    ((vectorp buf)
     (let* ((len (length buf))
            (bytes (make-array len :element-type '(unsigned-byte 8))))
       (dotimes (i len bytes)
         (setf (aref bytes i) (logand #xFF (truncate (aref buf i)))))))
    (t nil)))

(defun %js-text-encode-into (str dest)
  (let* ((s (%js-to-string str))
         (dest-len (if (js-typed-array-p dest) (js-ta-length dest) 0))
         (read 0)
         (written 0))
    (when (js-typed-array-p dest)
      (loop for ch across s
            for ch-str = (string ch)
            for bytes = (sb-ext:string-to-octets ch-str :external-format :utf-8)
            for byte-count = (length bytes)
            while (<= (+ written byte-count) dest-len)
            do (progn
                 (dotimes (i byte-count)
                   (%js-ta-set dest (+ written i) (aref bytes i)))
                 (incf written byte-count)
                 (incf read (if (> (char-code ch) #xFFFF) 2 1)))))
    (%js-make-object "read" (coerce read 'double-float)
                     "written" (coerce written 'double-float))))

(defun %js-make-text-encoder ()
  "JS TextEncoder (UTF-8 encoding)."
  (%js-make-object
   "encoding" "utf-8"
   "encode"   (lambda (str)
                (let* ((bytes (%js-utf8-octets str))
                       (vec (%js-make-typed-array "Uint8Array" (length bytes))))
                  (loop for i below (length bytes)
                        do (%js-ta-set vec i (aref bytes i)))
                  vec))
   "encodeInto" #'%js-text-encode-into))

(defun %js-make-text-decoder (&optional (encoding "utf-8"))
  "JS TextDecoder for UTF-8 byte inputs."
  (let ((normalized (%js-text-decoder-encoding encoding)))
    (%js-make-object
     "encoding" normalized
     "fatal" nil
     "ignoreBOM" nil
     "decode"   (lambda (&optional buf _options)
                  (declare (ignore _options))
                  (cond
                    ((or (null buf) (eq buf +js-undefined+)) "")
                    (t
                     (let ((bytes (%js-text-buffer-octets buf)))
                       (if bytes
                           (%js-text-decode-octets bytes)
                           (%js-to-string buf)))))))))
