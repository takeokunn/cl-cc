(in-package :cl-cc/vm)

;;; VM String Instructions
;;;
;;; This file extends the VM with string operations including comparisons,
;;; manipulation, and character access.
;;; Character comparison/predicate instructions: strings-char-instrs.lisp (loads after).

;;; String Comparison Instructions

(defvar *runtime-string-table* nil
  "Weak runtime string interning table mapping string contents to canonical strings.")

(defvar *rt-string-dedup-table* nil
  "Compatibility alias for the runtime string interning table.")

(defun %rt-ensure-string-dedup-table ()
  "Return the runtime string interning table, creating it lazily."
  (or *runtime-string-table*
      (let ((table (make-hash-table :test 'equal
                                    #+sbcl :weakness
                                    #+sbcl :key-and-value)))
        (setf *runtime-string-table* table
              *rt-string-dedup-table* table))))

(defun rt-string-dedup (string)
  "Return the canonical VM string with the same contents as STRING."
  (check-type string string)
  (let ((table (%rt-ensure-string-dedup-table)))
    (multiple-value-bind (canonical found-p) (gethash string table)
      (if (and found-p canonical (string= canonical string))
          canonical
          (setf (gethash string table) string)))))

(defun rt-string-intern (string)
  "Return the canonical weakly interned VM string for STRING's contents."
  (rt-string-dedup string))

(defun %vm-sso-function (name)
  "Return runtime SSO function NAME when the runtime package is loaded."
  (%vm-runtime-function name))

(defun %vm-sso-string-p (value)
  "True when VALUE is a runtime small-string immediate."
  (and (integerp value)
       (let ((predicate (%vm-sso-function "VAL-SSO-STRING-P")))
         (and predicate (funcall predicate value)))))

(defun %vm-decode-sso-string (value)
  "Decode runtime SSO VALUE to a host string."
  (funcall (%vm-sso-function "DECODE-SSO-STRING") value))

(defun %vm-byte-string-p (string)
  "True when STRING can be represented as inline SSO bytes."
  (and (stringp string)
       (<= (length string) 7)
       (loop for char across string
             always (<= (char-code char) #xFF))))

(defun %vm-host-string (value)
  "Return VALUE as a host string, decoding SSO immediates as needed."
  (if (%vm-sso-string-p value)
      (%vm-decode-sso-string value)
      value))

(defun %vm-maybe-sso-string (string)
  "In the VM interpreter, return the host string directly.
SSO encoding is reserved for native codegen backends; the interpreter
stores host CL values in registers so tests receive proper strings."
  string)

(defun %vm-string-length (value)
  "Return the length of host or SSO string VALUE."
  (if (%vm-sso-string-p value)
      (logand value #x7)
      (length value)))

(defun %vm-string-char (value index)
  "Return character INDEX from host or SSO string VALUE."
  (if (%vm-sso-string-p value)
      (code-char (ldb (byte 8 (+ 3 (* 8 index))) value))
      (char value index)))

(defmacro define-vm-sso-string-comparison-executor (vm-class cl-fn)
  `(defmethod execute-instruction ((inst ,vm-class) state pc labels)
     (declare (ignore labels))
     (vm-reg-set state (vm-dst inst)
                 (,cl-fn (%vm-host-string (vm-reg-get state (vm-str1 inst)))
                         (%vm-host-string (vm-reg-get state (vm-str2 inst)))))
     (values (1+ pc) nil nil)))

(defmacro define-vm-sso-string-unary-executor (vm-class cl-fn)
  `(defmethod execute-instruction ((inst ,vm-class) state pc labels)
     (declare (ignore labels))
     (vm-reg-set state (vm-dst inst)
                 (%vm-maybe-sso-string
                  (,cl-fn (%vm-host-string (vm-reg-get state (vm-src inst))))))
     (values (1+ pc) nil nil)))

(defmacro define-vm-sso-string-destructive-unary-executor (vm-class cl-fn)
  `(defmethod execute-instruction ((inst ,vm-class) state pc labels)
     (declare (ignore labels))
     (vm-reg-set state (vm-dst inst)
                 (,cl-fn (copy-seq (%vm-host-string (vm-reg-get state (vm-src inst))))))
     (values (1+ pc) nil nil)))

;; All binary string comparisons share the same (dst str1 str2) slot structure.
(defmacro define-vm-string-comparison (name tag docstring)
  `(define-vm-instruction ,name (vm-instruction)
     ,docstring
     (dst nil :reader vm-dst)
     (str1 nil :reader vm-str1)
     (str2 nil :reader vm-str2)
     (:sexp-tag ,tag)
     (:sexp-slots dst str1 str2)))

(define-vm-string-comparison vm-string=           :string=          "Case-sensitive string equality. Returns 1 if equal, 0 otherwise.")
(define-vm-string-comparison vm-string<           :string<          "Case-sensitive string less than. Returns 1 if STR1 < STR2, 0 otherwise.")
(define-vm-string-comparison vm-string>           :string>          "Case-sensitive string greater than. Returns 1 if STR1 > STR2, 0 otherwise.")
(define-vm-string-comparison vm-string<=          :string<=         "Case-sensitive string <=. Returns 1 if STR1 <= STR2, 0 otherwise.")
(define-vm-string-comparison vm-string>=          :string>=         "Case-sensitive string >=. Returns 1 if STR1 >= STR2, 0 otherwise.")
(define-vm-string-comparison vm-string-equal      :string-equal     "Case-insensitive string equality. Returns 1 if equal (ignoring case), 0 otherwise.")
(define-vm-string-comparison vm-string-lessp      :string-lessp     "Case-insensitive string <. Returns 1 if STR1 < STR2 (ignoring case), 0 otherwise.")
(define-vm-string-comparison vm-string-greaterp   :string-greaterp  "Case-insensitive string >. Returns 1 if STR1 > STR2 (ignoring case), 0 otherwise.")
(define-vm-string-comparison vm-string-not-equal  :string-not-equal "Case-sensitive string inequality. Returns 1 if STR1 /= STR2, 0 otherwise.")
(define-vm-string-comparison vm-string-not-greaterp :string-not-greaterp "Case-insensitive string <=. Returns 1 if STR1 <= STR2 (ignoring case), 0 otherwise.")
(define-vm-string-comparison vm-string-not-lessp  :string-not-lessp "Case-insensitive string >=. Returns 1 if STR1 >= STR2 (ignoring case), 0 otherwise.")

;;; String Access and Query Instructions

;; define-vm-unary-instruction and define-vm-char-comparison are defined in vm.lisp.

;;; FR-136: ASCII character class lookup table

(defconstant +char-class-alpha+ #x01)
(defconstant +char-class-digit+ #x02)
(defconstant +char-class-upper+ #x04)
(defconstant +char-class-lower+ #x08)
(defconstant +char-class-alphanumeric+ #x10)
(defconstant +char-class-graphic+ #x20)
(defconstant +char-class-whitespace+ #x40)
(defconstant +char-class-standard+ #x80)

(defun %make-char-class-table ()
  "Build the immutable-by-convention ASCII character class table."
  (let ((table (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)))
    (labels ((add-flag (code flag)
               (setf (aref table code) (logior (aref table code) flag)))
             (add-range (start end flag)
               (loop for code from start to end do (add-flag code flag))))
      ;; Standard graphic ASCII characters are space through tilde. Newline is
      ;; the only non-graphic standard character.
      (add-range 32 126 +char-class-graphic+)
      (add-range 32 126 +char-class-standard+)
      (add-flag (char-code #\Newline) +char-class-standard+)
      (dolist (code (mapcar #'char-code '(#\Space #\Tab #\Newline #\Return #\Page)))
        (add-flag code +char-class-whitespace+))
      (add-range (char-code #\0) (char-code #\9)
                 (logior +char-class-digit+ +char-class-alphanumeric+))
      (add-range (char-code #\A) (char-code #\Z)
                 (logior +char-class-alpha+ +char-class-upper+ +char-class-alphanumeric+))
      (add-range (char-code #\a) (char-code #\z)
                 (logior +char-class-alpha+ +char-class-lower+ +char-class-alphanumeric+)))
    table))

(defparameter *char-class-table* (%make-char-class-table)
  "256-byte ASCII character class table for O(1) character predicates.
Each byte encodes +CHAR-CLASS-* flags. Treat as read-only after load.")

(declaim (inline %char-code<=255-p %ascii-char-class-logtest
                 vm-alpha-char-p-value vm-digit-char-p-value
                 vm-upper-case-p-value vm-lower-case-p-value
                 vm-alphanumericp-value vm-graphic-char-p-value
                 vm-standard-char-p-value vm-both-case-p-value))

(defun %char-code<=255-p (code)
  (and (integerp code) (<= 0 code 255)))

(defun %ascii-char-class-logtest (ch flag)
  (let ((code (char-code ch)))
    (and (%char-code<=255-p code)
         (logtest (aref *char-class-table* code) flag))))

(defun vm-alpha-char-p-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (logtest (aref *char-class-table* code) +char-class-alpha+)
        (alpha-char-p ch))))

(defun vm-digit-char-p-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (and (logtest (aref *char-class-table* code) +char-class-digit+)
             (- code (char-code #\0)))
        (digit-char-p ch))))

(defun vm-upper-case-p-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (logtest (aref *char-class-table* code) +char-class-upper+)
        (upper-case-p ch))))

(defun vm-lower-case-p-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (logtest (aref *char-class-table* code) +char-class-lower+)
        (lower-case-p ch))))

(defun vm-alphanumericp-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (logtest (aref *char-class-table* code) +char-class-alphanumeric+)
        (alphanumericp ch))))

(defun vm-graphic-char-p-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (logtest (aref *char-class-table* code) +char-class-graphic+)
        (graphic-char-p ch))))

(defun vm-standard-char-p-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (logtest (aref *char-class-table* code) +char-class-standard+)
        (standard-char-p ch))))

(defun vm-both-case-p-value (ch)
  (let ((code (char-code ch)))
    (if (%char-code<=255-p code)
        (logtest (aref *char-class-table* code) +char-class-alpha+)
        (both-case-p ch))))

(define-vm-unary-instruction vm-string-length :string-length "String length. DST = length of SRC string.")

(define-vm-instruction vm-char (vm-instruction)
  "Character at position. DST = STRING[INDEX]."
  (dst nil :reader vm-dst)
  (string nil :reader vm-string-reg)
  (index nil :reader vm-index)
  (:sexp-tag :char)
  (:sexp-slots dst string index))

(define-vm-unary-instruction vm-char-code :char-code "Character code. DST = ASCII/Unicode code of SRC character.")
(define-vm-unary-instruction vm-code-char :code-char "Character from code. DST = character with code SRC.")

(define-vm-char-comparison vm-char= :char= "Character equality. Returns 1 if CHAR1 equals CHAR2, 0 otherwise.")
(define-vm-char-comparison vm-char< :char< "Character less than. Returns 1 if CHAR1 < CHAR2, 0 otherwise.")

;;; String Manipulation Instructions

(define-vm-instruction vm-subseq (vm-instruction)
  "Substring extraction. DST = STRING[START:END]."
  (dst nil :reader vm-dst)
  (string nil :reader vm-string-reg)
  (start nil :reader vm-start)
  (end nil :reader vm-end)
  (:sexp-tag :subseq)
  (:sexp-slots dst string start end))

(define-vm-instruction vm-concatenate (vm-instruction)
  "String concatenation. DST = STR1 + STR2."
  (dst nil :reader vm-dst)
  (str1 nil :reader vm-str1)
  (str2 nil :reader vm-str2)
  (parts nil :reader vm-parts)
  (:sexp-tag :concatenate)
  (:sexp-slots dst str1 str2 parts))

(define-vm-unary-instruction vm-string-upcase    :string-upcase    "Uppercase conversion. DST = uppercase of SRC.")
(define-vm-unary-instruction vm-string-downcase  :string-downcase  "Lowercase conversion. DST = lowercase of SRC.")
(define-vm-unary-instruction vm-string-capitalize :string-capitalize "Capitalize string. DST = capitalized form of SRC.")
;;; FR-475: Destructive string case operations
(define-vm-unary-instruction vm-nstring-upcase    :nstring-upcase    "Destructive uppercase. Modifies and returns SRC.")
(define-vm-unary-instruction vm-nstring-downcase  :nstring-downcase  "Destructive lowercase. Modifies and returns SRC.")
(define-vm-unary-instruction vm-nstring-capitalize :nstring-capitalize "Destructive capitalize. Modifies and returns SRC.")

(defmacro define-vm-string-trim-instruction (name tag docstring)
  `(define-vm-instruction ,name (vm-instruction)
     ,docstring
     (dst nil :reader vm-dst)
     (char-bag nil :reader vm-char-bag)
     (string nil :reader vm-string-reg)
     (:sexp-tag ,tag)
     (:sexp-slots dst char-bag string)))

(define-vm-string-trim-instruction vm-string-trim       :string-trim       "Trim characters from both ends. DST = STRING with CHAR-BAG chars trimmed from both ends.")
(define-vm-string-trim-instruction vm-string-left-trim  :string-left-trim  "Trim characters from left. DST = STRING with CHAR-BAG chars trimmed from left.")
(define-vm-string-trim-instruction vm-string-right-trim :string-right-trim "Trim characters from right. DST = STRING with CHAR-BAG chars trimmed from right.")

;;; String Search Instructions

(define-vm-instruction vm-search-string (vm-instruction)
  "Search for pattern in string. DST = index of PATTERN in STRING from START, or -1 if not found."
  (dst nil :reader vm-dst)
  (pattern nil :reader vm-pattern)
  (string nil :reader vm-string-reg)
  (start nil :reader vm-start)
  (:sexp-tag :search-string)
  (:sexp-slots dst pattern string start))

;;; Instruction Execution - String Comparisons
;;; Use :binary (pass-through) so ANSI return values are preserved:
;;;   string=, string-equal     → T or NIL
;;;   string<, string>, etc.    → mismatch index (integer) or NIL

(define-vm-sso-string-comparison-executor vm-string= string=)
(define-vm-sso-string-comparison-executor vm-string< string<)
(define-vm-sso-string-comparison-executor vm-string> string>)
(define-vm-sso-string-comparison-executor vm-string<= string<=)
(define-vm-sso-string-comparison-executor vm-string>= string>=)
(define-vm-sso-string-comparison-executor vm-string-equal string-equal)
(define-vm-sso-string-comparison-executor vm-string-lessp string-lessp)
(define-vm-sso-string-comparison-executor vm-string-greaterp string-greaterp)
(define-vm-sso-string-comparison-executor vm-string-not-equal string-not-equal)
(define-vm-sso-string-comparison-executor vm-string-not-greaterp string-not-greaterp)
(define-vm-sso-string-comparison-executor vm-string-not-lessp string-not-lessp)

;;; Instruction Execution - String Access and Query

(defmethod execute-instruction ((inst vm-string-length) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-string-length (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-char) state pc labels)
  (declare (ignore labels))
  (let* ((string (vm-reg-get state (vm-string-reg inst)))
          (index (vm-reg-get state (vm-index inst)))
          (result (%vm-string-char string index)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-char-code :unary char-code)
(define-simple-instruction vm-code-char :unary code-char)

(define-simple-instruction vm-char= :pred2 char= :lhs vm-char1 :rhs vm-char2)
(define-simple-instruction vm-char< :pred2 char< :lhs vm-char1 :rhs vm-char2)

;;; Instruction Execution - String Manipulation

(defmethod execute-instruction ((inst vm-subseq) state pc labels)
  (declare (ignore labels))
  (let* ((string (vm-reg-get state (vm-string-reg inst)))
         (start  (vm-reg-get state (vm-start inst)))
         ;; nil end-slot means no upper bound — pass nil to subseq (= end of sequence)
         (end    (if (vm-end inst) (vm-reg-get state (vm-end inst)) nil))
         (result (subseq (%vm-host-string string) start end)))
    (vm-reg-set state (vm-dst inst) (%vm-maybe-sso-string result))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-concatenate) state pc labels)
  (declare (ignore labels))
  (let* ((parts (or (vm-parts inst)
                     (list (vm-str1 inst) (vm-str2 inst))))
          (result (%vm-maybe-sso-string
                   (apply #'concatenate 'string
                          (mapcar (lambda (reg)
                                    (%vm-host-string (vm-reg-get state reg)))
                                  parts)))))
     (vm-reg-set state (vm-dst inst) result)
     (values (1+ pc) nil nil)))

(define-vm-sso-string-unary-executor vm-string-upcase string-upcase)
(define-vm-sso-string-unary-executor vm-string-downcase string-downcase)
(define-vm-sso-string-unary-executor vm-string-capitalize string-capitalize)
(define-vm-sso-string-destructive-unary-executor vm-nstring-upcase nstring-upcase)
(define-vm-sso-string-destructive-unary-executor vm-nstring-downcase nstring-downcase)
(define-vm-sso-string-destructive-unary-executor vm-nstring-capitalize nstring-capitalize)

(defmacro define-vm-string-trim-executor (vm-class cl-fn)
  `(defmethod execute-instruction ((inst ,vm-class) state pc labels)
     (declare (ignore labels))
     (vm-reg-set state (vm-dst inst)
                 (%vm-maybe-sso-string
                  (,cl-fn (%vm-host-string (vm-reg-get state (vm-char-bag inst)))
                          (%vm-host-string (vm-reg-get state (vm-string-reg inst))))))
     (values (1+ pc) nil nil)))

(define-vm-string-trim-executor vm-string-trim       string-trim)
(define-vm-string-trim-executor vm-string-left-trim  string-left-trim)
(define-vm-string-trim-executor vm-string-right-trim string-right-trim)

;;; Instruction Execution - String Search

(defmethod execute-instruction ((inst vm-search-string) state pc labels)
  (declare (ignore labels))
  (let* ((pattern (%vm-host-string (vm-reg-get state (vm-pattern inst))))
         (string (%vm-host-string (vm-reg-get state (vm-string-reg inst))))
         (start (vm-reg-get state (vm-start inst)))
         (result (or (search pattern string :start2 start) -1)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; String character mutation (FR-614) — (setf (char s i) v) → (rt-string-set s i v)

(define-vm-instruction vm-string-set (vm-instruction)
  "Set character in string at index. Returns the new character."
  (dst nil :reader vm-dst)
  (str nil :reader vm-str-reg)
  (idx nil :reader vm-idx)
  (val nil :reader vm-val-reg)
  (:sexp-tag :string-set)
  (:sexp-slots dst str idx val))

(defmethod execute-instruction ((inst vm-string-set) state pc labels)
  (declare (ignore labels))
  (let ((v (vm-reg-get state (vm-val-reg inst))))
    (setf (char (vm-reg-get state (vm-str-reg inst))
                (vm-reg-get state (vm-idx inst))) v)
    (vm-reg-set state (vm-dst inst) v)
    (values (1+ pc) nil nil)))
