;;;; packages/vm/src/unicode.lisp — Unicode Character Database (FR-590..FR-593)
(in-package :cl-cc/vm)

;;; General Category names.  Keep these as the public category spellings used by
;;; lookup helpers; the compact BMP table stores indexes into this vector.

(defparameter +ucd-categories+
  #("Lu" "Ll" "Lt" "Lm" "Lo" "Mn" "Mc" "Me" "Nd" "Nl" "No"
    "Pc" "Pd" "Ps" "Pe" "Pi" "Pf" "Po" "Sm" "Sc" "Sk" "So"
    "Zs" "Zl" "Zp" "Cc" "Cf" "Cs" "Co" "Cn"))

(defvar +ucd-category-lu+ "Lu")  (defvar +ucd-category-ll+ "Ll")
(defvar +ucd-category-lt+ "Lt")  (defvar +ucd-category-lm+ "Lm")
(defvar +ucd-category-lo+ "Lo")  (defvar +ucd-category-mn+ "Mn")
(defvar +ucd-category-mc+ "Mc")  (defvar +ucd-category-me+ "Me")
(defvar +ucd-category-nd+ "Nd")  (defvar +ucd-category-nl+ "Nl")
(defvar +ucd-category-no+ "No")  (defvar +ucd-category-pc+ "Pc")
(defvar +ucd-category-pd+ "Pd")  (defvar +ucd-category-ps+ "Ps")
(defvar +ucd-category-pe+ "Pe")  (defvar +ucd-category-pi+ "Pi")
(defvar +ucd-category-pf+ "Pf")  (defvar +ucd-category-po+ "Po")
(defvar +ucd-category-sm+ "Sm")  (defvar +ucd-category-sc+ "Sc")
(defvar +ucd-category-sk+ "Sk")  (defvar +ucd-category-so+ "So")
(defvar +ucd-category-zs+ "Zs")  (defvar +ucd-category-zl+ "Zl")
(defvar +ucd-category-zp+ "Zp")  (defvar +ucd-category-cc+ "Cc")
(defvar +ucd-category-cf+ "Cf")  (defvar +ucd-category-cs+ "Cs")
(defvar +ucd-category-co+ "Co")  (defvar +ucd-category-cn+ "Cn")

(defconstant +unicode-max-code-point+ #x10ffff)
(defconstant +unicode-bmp-limit+ #x10000)
(defconstant +unicode-replacement-character+ #xfffd)

(defvar *ucd-category-index* nil)
(defvar *ucd-bmp-category-ranges* #()
  "Compact General Category table for U+0000..U+FFFF.
Each entry is #(START END CATEGORY-INDEX), inclusive bounds.")

(defun ucd-init-category-index ()
  (let ((table (make-hash-table :test #'equal)))
    (loop for category across +ucd-categories+
          for i from 0
          do (setf (gethash category table) i))
    (setf *ucd-category-index* table)))

(declaim (inline %unicode-code-point-p %unicode-char %unicode-code
                 %unicode-category-index %unicode-letter-category-p))

(defun %unicode-code-point-p (code)
  (and (integerp code) (<= 0 code +unicode-max-code-point+)))

(defun %unicode-char (value)
  "Return VALUE as a host character when possible, otherwise NIL."
  (cond ((characterp value) value)
        ((%unicode-code-point-p value) (code-char value))
        (t nil)))

(defun %unicode-code (value)
  "Return VALUE as a Unicode code point."
  (cond ((characterp value) (char-code value))
        ((%unicode-code-point-p value) value)
        (t (error "Not a Unicode character or code point: ~S" value))))

(defun %unicode-category-index (category)
  (or (gethash category *ucd-category-index*)
      (error "Unknown Unicode General Category: ~S" category)))

(defun %ascii-control-p (code)
  (or (<= 0 code #x1f) (= code #x7f)))

(defun %unicode-zs-code-p (code)
  (member code '(#x20 #xa0 #x1680 #x2000 #x2001 #x2002 #x2003 #x2004 #x2005
                 #x2006 #x2007 #x2008 #x2009 #x200a #x202f #x205f #x3000)
          :test #'=))

(defun %sb-unicode-call (name ch)
  "Call an SB-UNICODE helper named NAME when available."
  (let* ((package (find-package :sb-unicode))
         (symbol (and package (find-symbol name package))))
    (and symbol (fboundp symbol) (funcall symbol ch))))

(defun %unicode-host-category (ch)
  (let ((category (%sb-unicode-call "GENERAL-CATEGORY" ch)))
    (and category
         (let ((name (symbol-name category)))
           (concatenate 'string
                        (string (char-upcase (char name 0)))
                        (string (char-downcase (char name 1))))))))

(defun %unicode-host-digit-value (ch)
  (%sb-unicode-call "DIGIT-VALUE" ch))

(defun %host-general-category (code)
  "Return an approximate Unicode General Category for CODE.
SBCL already carries the full Unicode database for predicates/case conversion;
this mapper keeps our VM table compact while preserving the standard groups
needed by character predicates and syntax classification."
  (cond
    ((not (%unicode-code-point-p code)) "Cn")
    ((<= #xd800 code #xdfff) "Cs")
    ((or (<= #xe000 code #xf8ff)
         (<= #xf0000 code #xffffd)
         (<= #x100000 code #x10fffd)) "Co")
    ((%ascii-control-p code) "Cc")
    ((= code #x2028) "Zl")
    ((= code #x2029) "Zp")
    ((%unicode-zs-code-p code) "Zs")
    ((and (<= #x30 code #x39)) "Nd")
    (t (let ((ch (code-char code)))
         (cond
           ((null ch) "Cn")
           ((%unicode-host-category ch))
           ((upper-case-p ch) "Lu")
           ((lower-case-p ch) "Ll")
           ((alpha-char-p ch) "Lo")
           ((%unicode-host-digit-value ch) "Nd")
           ((alphanumericp ch) "Nl")
           ((find ch "-_" :test #'char=) "Pc")
           ((find ch "()[]{}" :test #'char=) (if (find ch "([{" :test #'char=) "Ps" "Pe"))
           ((find ch "+<=>|~^*/%&" :test #'char=) "Sm")
           ((find ch "$¢£¤¥€" :test #'char=) "Sc")
           ((graphic-char-p ch) "Po")
           (t "Cn"))))))

(defun %make-ucd-bmp-category-ranges ()
  "Build a compact run-length encoded General Category table for the BMP."
  (let ((ranges nil)
        (start 0)
        (previous nil))
    (loop for code below +unicode-bmp-limit+
          for category = (%host-general-category code)
          do (cond
               ((null previous) (setf previous category))
               ((not (string= category previous))
                (push (vector start (1- code) (%unicode-category-index previous)) ranges)
                (setf start code previous category)))
          finally (push (vector start (1- +unicode-bmp-limit+)
                                (%unicode-category-index previous))
                        ranges))
    (coerce (nreverse ranges) 'vector)))

(defun unicode-general-category (value)
  "Return the Unicode General Category string for VALUE (character or code point)."
  (let ((code (%unicode-code value)))
    (if (< code +unicode-bmp-limit+)
        (loop for range across *ucd-bmp-category-ranges*
              when (<= (aref range 0) code (aref range 1))
                return (svref +ucd-categories+ (aref range 2)))
        (%host-general-category code))))

(defun %unicode-letter-category-p (category)
  (not (null (member category '("Lu" "Ll" "Lt" "Lm" "Lo") :test #'string=))))

(defun unicode-alpha-char-p (value)
  "True when VALUE is a Unicode alphabetic character/code point."
  (%unicode-letter-category-p (unicode-general-category value)))

(defun unicode-upper-case-p (value)
  (string= (unicode-general-category value) "Lu"))

(defun unicode-lower-case-p (value)
  (string= (unicode-general-category value) "Ll"))

(defun unicode-digit-char-p (value &optional (radix 10))
  "Return VALUE's digit value when it is a Unicode decimal digit below RADIX."
  (let* ((code (%unicode-code value))
         (digit (cond ((<= #x30 code #x39) (- code #x30))
                      ((let ((ch (%unicode-char value)))
                         (and ch (or (%unicode-host-digit-value ch)
                                     (digit-char-p ch))))))))
    (and digit (< digit radix) digit)))

(defun unicode-alphanumericp (value)
  (or (unicode-alpha-char-p value) (unicode-digit-char-p value)))

(defun unicode-char-code (value)
  "Return VALUE's Unicode code point.  Integer code points are passed through."
  (%unicode-code value))

(defun unicode-code-char (code)
  "Return the host character for CODE in U+0000..U+10FFFF, or NIL if unsupported."
  (and (%unicode-code-point-p code) (code-char code)))

(defun %offset-case-map (code direction)
  "Small offset-based case map for common scripts; falls back to host Unicode."
  (ecase direction
    (:down (cond ((<= #x41 code #x5a) (+ code #x20))
                 ((or (<= #xc0 code #xd6) (<= #xd8 code #xde)) (+ code #x20))
                 ((<= #x410 code #x42f) (+ code #x20))
                 ((<= #x400 code #x40f) (+ code #x50))
                 ((<= #x531 code #x556) (+ code #x30))
                 ((<= #xff21 code #xff3a) (+ code #x20))
                 ((and (<= #x100 code #x137) (evenp code)) (1+ code))
                 (t nil)))
    (:up (cond ((<= #x61 code #x7a) (- code #x20))
               ((or (<= #xe0 code #xf6) (<= #xf8 code #xfe)) (- code #x20))
               ((<= #x430 code #x44f) (- code #x20))
               ((<= #x450 code #x45f) (- code #x50))
               ((<= #x561 code #x586) (- code #x30))
               ((<= #xff41 code #xff5a) (- code #x20))
               ((and (<= #x101 code #x137) (oddp code)) (1- code))
               (t nil)))))

(defun unicode-char-upcase (value)
  "Return VALUE uppercased using Unicode simple case mapping."
  (let* ((code (%unicode-code value))
         (mapped (or (%offset-case-map code :up)
                     (let ((ch (unicode-code-char code)))
                       (and ch (char-code (char-upcase ch)))))))
    (or (unicode-code-char (or mapped code)) value)))

(defun unicode-char-downcase (value)
  "Return VALUE downcased using Unicode simple case mapping."
  (let* ((code (%unicode-code value))
         (mapped (or (%offset-case-map code :down)
                     (let ((ch (unicode-code-char code)))
                       (and ch (char-code (char-downcase ch)))))))
    (or (unicode-code-char (or mapped code)) value)))

(defun %unicode-latin-1-decomposition (code)
  "Return CODE's canonical Latin-1 decomposition as code points, or NIL.
This covers the Latin-script precomposed vowels and cedilla required by the VM
without embedding a full Unicode decomposition table."
  (labels ((mark (marks index) (aref marks index))
           (pair (base combining) (vector base combining)))
    (cond
      ((<= #xc0 code #xc5)
       (pair #x41 (mark #(#x300 #x301 #x302 #x303 #x308 #x30a) (- code #xc0))))
      ((= code #xc7) (pair #x43 #x327))
      ((<= #xc8 code #xcb)
       (pair #x45 (mark #(#x300 #x301 #x302 #x308) (- code #xc8))))
      ((<= #xcc code #xcf)
       (pair #x49 (mark #(#x300 #x301 #x302 #x308) (- code #xcc))))
      ((= code #xd1) (pair #x4e #x303))
      ((<= #xd2 code #xd6)
       (pair #x4f (mark #(#x300 #x301 #x302 #x303 #x308) (- code #xd2))))
      ((<= #xd9 code #xdc)
       (pair #x55 (mark #(#x300 #x301 #x302 #x308) (- code #xd9))))
      ((= code #xdd) (pair #x59 #x301))
      ((<= #xe0 code #xe5)
       (pair #x61 (mark #(#x300 #x301 #x302 #x303 #x308 #x30a) (- code #xe0))))
      ((= code #xe7) (pair #x63 #x327))
      ((<= #xe8 code #xeb)
       (pair #x65 (mark #(#x300 #x301 #x302 #x308) (- code #xe8))))
      ((<= #xec code #xef)
       (pair #x69 (mark #(#x300 #x301 #x302 #x308) (- code #xec))))
      ((= code #xf1) (pair #x6e #x303))
      ((<= #xf2 code #xf6)
       (pair #x6f (mark #(#x300 #x301 #x302 #x303 #x308) (- code #xf2))))
      ((<= #xf9 code #xfc)
       (pair #x75 (mark #(#x300 #x301 #x302 #x308) (- code #xf9))))
      ((= code #xfd) (pair #x79 #x301))
      ((= code #xff) (pair #x79 #x308)))))

(defun %unicode-combining-class (code)
  "Return the canonical combining class for the combining marks we normalize."
  (case code
    (#x327 202)                         ; COMBINING CEDILLA
    ((#x300 #x301 #x302 #x303 #x308 #x30a) 230)
    (otherwise 0)))

(defun %unicode-push-code-char (code chars)
  (vector-push-extend (or (unicode-code-char code)
                          (code-char +unicode-replacement-character+))
                      chars))

(defun %unicode-nfd-codes (string)
  "Return a vector of BMP code points in canonical decomposition order."
  (let ((codes (make-array 0 :element-type 'integer :adjustable t :fill-pointer 0))
        (marks nil))
    (labels ((flush-marks ()
             (dolist (code (stable-sort marks #'< :key #'%unicode-combining-class))
                 (vector-push-extend code codes))
               (setf marks nil))
             (push-code (code)
               (let ((decomposition (and (< code +unicode-bmp-limit+)
                                         (%unicode-latin-1-decomposition code))))
                 (cond
                   (decomposition
                    (push-code (aref decomposition 0))
                    (push-code (aref decomposition 1)))
                   ((zerop (%unicode-combining-class code))
                    (flush-marks)
                    (vector-push-extend code codes))
                   (t (push code marks))))))
      (loop for ch across string
            do (push-code (char-code ch)))
      (flush-marks)
      codes)))

(defun %unicode-compose-latin-1 (base combining)
  "Compose BASE and COMBINING into a supported Latin-1 precomposed code point."
  (labels ((from-mark (marks start)
             (let ((index (position combining marks :test #'=)))
               (and index (+ start index)))))
    (case base
      (#x41 (from-mark #(#x300 #x301 #x302 #x303 #x308 #x30a) #xc0))
      (#x43 (and (= combining #x327) #xc7))
      (#x45 (from-mark #(#x300 #x301 #x302 #x308) #xc8))
      (#x49 (from-mark #(#x300 #x301 #x302 #x308) #xcc))
      (#x4e (and (= combining #x303) #xd1))
      (#x4f (from-mark #(#x300 #x301 #x302 #x303 #x308) #xd2))
      (#x55 (from-mark #(#x300 #x301 #x302 #x308) #xd9))
      (#x59 (and (= combining #x301) #xdd))
      (#x61 (from-mark #(#x300 #x301 #x302 #x303 #x308 #x30a) #xe0))
      (#x63 (and (= combining #x327) #xe7))
      (#x65 (from-mark #(#x300 #x301 #x302 #x308) #xe8))
      (#x69 (from-mark #(#x300 #x301 #x302 #x308) #xec))
      (#x6e (and (= combining #x303) #xf1))
      (#x6f (from-mark #(#x300 #x301 #x302 #x303 #x308) #xf2))
      (#x75 (from-mark #(#x300 #x301 #x302 #x308) #xf9))
      (#x79 (cond ((= combining #x301) #xfd)
                  ((= combining #x308) #xff))))))

(defun %unicode-codes-to-string (codes)
  (let ((chars (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for code across codes do (%unicode-push-code-char code chars))
    (coerce chars 'string)))

(defun %unicode-normalize-nfd (string)
  (%unicode-codes-to-string (%unicode-nfd-codes string)))

(defun %unicode-normalize-nfc (string)
  (let* ((codes (%unicode-nfd-codes string))
         (composed (make-array 0 :element-type 'integer :adjustable t :fill-pointer 0)))
    (loop with i = 0 while (< i (length codes)) do
      (let ((code (aref codes i)))
        (if (and (< (1+ i) (length codes))
                 (not (zerop (%unicode-combining-class (aref codes (1+ i))))))
            (let ((combined (%unicode-compose-latin-1 code (aref codes (1+ i)))))
              (if combined
                  (progn
                    (vector-push-extend combined composed)
                    (incf i 2))
                  (progn
                    (vector-push-extend code composed)
                    (incf i))))
            (progn
              (vector-push-extend code composed)
              (incf i)))))
    (%unicode-codes-to-string composed)))

(defun unicode-normalize-string (string &key (form :nfc))
  "Normalize STRING as FORM (:NFC, :NFD, :NFKC, or :NFKD).
Implements canonical decomposition/composition for BMP Latin-script characters
covered by the VM's compact Unicode layer; compatibility forms currently reuse
the corresponding canonical form."
  (check-type string string)
  (ecase form
    (:nfc (%unicode-normalize-nfc string))
    (:nfd (%unicode-normalize-nfd string))
    (:nfkc (%unicode-normalize-nfc string))
    (:nfkd (%unicode-normalize-nfd string))))

(defun unicode-normalize-nfc (string) (unicode-normalize-string string :form :nfc))
(defun unicode-normalize-nfd (string) (unicode-normalize-string string :form :nfd))
(defun unicode-normalize-nfkc (string) (unicode-normalize-string string :form :nfkc))
(defun unicode-normalize-nfkd (string) (unicode-normalize-string string :form :nfkd))

(defun %utf8-push-code (code bytes)
  (cond
    ((<= code #x7f) (vector-push-extend code bytes))
    ((<= code #x7ff)
     (vector-push-extend (logior #xc0 (ash code -6)) bytes)
     (vector-push-extend (logior #x80 (logand code #x3f)) bytes))
    ((<= code #xffff)
     (vector-push-extend (logior #xe0 (ash code -12)) bytes)
     (vector-push-extend (logior #x80 (logand (ash code -6) #x3f)) bytes)
     (vector-push-extend (logior #x80 (logand code #x3f)) bytes))
    (t
     (vector-push-extend (logior #xf0 (ash code -18)) bytes)
     (vector-push-extend (logior #x80 (logand (ash code -12) #x3f)) bytes)
     (vector-push-extend (logior #x80 (logand (ash code -6) #x3f)) bytes)
     (vector-push-extend (logior #x80 (logand code #x3f)) bytes))))

(defun string-to-utf8-bytes (string)
  "Encode STRING to a vector of (UNSIGNED-BYTE 8) UTF-8 octets."
  (check-type string string)
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0)))
    (loop for ch across string
          for code = (char-code ch)
          do (%utf8-push-code code bytes))
    bytes))

(defun %utf8-continuation (bytes index end)
  (when (>= index end) (error "Truncated UTF-8 sequence"))
  (let ((byte (aref bytes index)))
    (unless (= (logand byte #xc0) #x80)
      (error "Invalid UTF-8 continuation byte: ~S" byte))
    (logand byte #x3f)))

(defun utf8-bytes-to-string (bytes)
  "Decode a sequence of UTF-8 octets to a string.  Signals ERROR on malformed UTF-8."
  (let* ((end (length bytes))
         (chars (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop with i = 0 while (< i end) do
      (let* ((b0 (aref bytes i))
             (code (cond
                     ((<= b0 #x7f) (incf i) b0)
                     ((<= #xc2 b0 #xdf)
                      (prog1 (logior (ash (logand b0 #x1f) 6)
                                     (%utf8-continuation bytes (1+ i) end))
                        (incf i 2)))
                     ((<= #xe0 b0 #xef)
                      (prog1 (logior (ash (logand b0 #x0f) 12)
                                     (ash (%utf8-continuation bytes (1+ i) end) 6)
                                     (%utf8-continuation bytes (+ i 2) end))
                        (incf i 3)))
                     ((<= #xf0 b0 #xf4)
                      (prog1 (logior (ash (logand b0 #x07) 18)
                                     (ash (%utf8-continuation bytes (1+ i) end) 12)
                                     (ash (%utf8-continuation bytes (+ i 2) end) 6)
                                     (%utf8-continuation bytes (+ i 3) end))
                        (incf i 4)))
                     (t (error "Invalid UTF-8 leading byte: ~S" b0)))))
        (unless (%unicode-code-point-p code)
          (error "Invalid Unicode code point decoded from UTF-8: ~S" code))
        (vector-push-extend (or (unicode-code-char code)
                                (code-char +unicode-replacement-character+))
                            chars)))
    (coerce chars 'string)))

(defun unicode-syntax-class (value)
  "Return the reader syntax class keyword for VALUE.
Classes include :CONSTITUENT, :WHITESPACE, :TERMINATING-MACRO,
:NON-TERMINATING-MACRO, :SINGLE-ESCAPE, :MULTIPLE-ESCAPE, and :INVALID."
  (let ((code (%unicode-code value)))
    (cond ((or (%ascii-control-p code) (<= #xd800 code #xdfff)) :invalid)
          ((or (member code '(#x09 #x0a #x0c #x0d #x20) :test #'=)
               (member (unicode-general-category code) '("Zs" "Zl" "Zp") :test #'string=))
           :whitespace)
          ((= code (char-code #\\)) :single-escape)
          ((= code (char-code #\|)) :multiple-escape)
          ((find (or (unicode-code-char code) #\Null) "()'`,;\"" :test #'char=)
           :terminating-macro)
          ((= code (char-code #\#)) :non-terminating-macro)
          ((unicode-code-char code) :constituent)
          (t :invalid))))

(defun unicode-init ()
  (ucd-init-category-index)
  (setf *ucd-bmp-category-ranges* (%make-ucd-bmp-category-ranges))
  t)

(eval-when (:load-toplevel :execute) (unicode-init))
