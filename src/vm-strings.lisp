(in-package :cl-cc)

;;; VM String Instructions
;;;
;;; This file extends the VM with string operations including comparisons,
;;; manipulation, and character access.

;;; String Comparison Instructions

(define-vm-instruction vm-string= (vm-instruction)
  "Case-sensitive string equality. Returns 1 if STR1 equals STR2, 0 otherwise."
  (dst nil :reader vm-dst)
  (str1 nil :reader vm-str1)
  (str2 nil :reader vm-str2)
  (:sexp-tag :string=)
  (:sexp-slots dst str1 str2))

(define-vm-instruction vm-string< (vm-instruction)
  "Case-sensitive string less than. Returns 1 if STR1 < STR2 lexicographically, 0 otherwise."
  (dst nil :reader vm-dst)
  (str1 nil :reader vm-str1)
  (str2 nil :reader vm-str2)
  (:sexp-tag :string<)
  (:sexp-slots dst str1 str2))

(define-vm-instruction vm-string> (vm-instruction)
  "Case-sensitive string greater than. Returns 1 if STR1 > STR2 lexicographically, 0 otherwise."
  (dst nil :reader vm-dst)
  (str1 nil :reader vm-str1)
  (str2 nil :reader vm-str2)
  (:sexp-tag :string>)
  (:sexp-slots dst str1 str2))

(define-vm-instruction vm-string<= (vm-instruction)
  "Case-sensitive string less than or equal. Returns 1 if STR1 <= STR2, 0 otherwise."
  (dst nil :reader vm-dst)
  (str1 nil :reader vm-str1)
  (str2 nil :reader vm-str2)
  (:sexp-tag :string<=)
  (:sexp-slots dst str1 str2))

(define-vm-instruction vm-string>= (vm-instruction)
  "Case-sensitive string greater than or equal. Returns 1 if STR1 >= STR2, 0 otherwise."
  (dst nil :reader vm-dst)
  (str1 nil :reader vm-str1)
  (str2 nil :reader vm-str2)
  (:sexp-tag :string>=)
  (:sexp-slots dst str1 str2))

(define-vm-instruction vm-string-equal (vm-instruction)
  "Case-insensitive string equality. Returns 1 if STR1 equals STR2 (ignoring case), 0 otherwise."
  (dst nil :reader vm-dst)
  (str1 nil :reader vm-str1)
  (str2 nil :reader vm-str2)
  (:sexp-tag :string-equal)
  (:sexp-slots dst str1 str2))

(define-vm-instruction vm-string-lessp (vm-instruction)
  "Case-insensitive string less than. Returns 1 if STR1 < STR2 (ignoring case), 0 otherwise."
  (dst nil :reader vm-dst)
  (str1 nil :reader vm-str1)
  (str2 nil :reader vm-str2)
  (:sexp-tag :string-lessp)
  (:sexp-slots dst str1 str2))

(define-vm-instruction vm-string-greaterp (vm-instruction)
  "Case-insensitive string greater than. Returns 1 if STR1 > STR2 (ignoring case), 0 otherwise."
  (dst nil :reader vm-dst)
  (str1 nil :reader vm-str1)
  (str2 nil :reader vm-str2)
  (:sexp-tag :string-greaterp)
  (:sexp-slots dst str1 str2))

(define-vm-instruction vm-string-not-equal (vm-instruction)
  "Case-sensitive string inequality. Returns 1 if STR1 not equal to STR2, 0 otherwise."
  (dst nil :reader vm-dst)
  (str1 nil :reader vm-str1)
  (str2 nil :reader vm-str2)
  (:sexp-tag :string-not-equal)
  (:sexp-slots dst str1 str2))

;;; String Access and Query Instructions

(define-vm-instruction vm-string-length (vm-instruction)
  "String length. DST = length of SRC string."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :string-length)
  (:sexp-slots dst src))

(define-vm-instruction vm-char (vm-instruction)
  "Character at position. DST = STRING[INDEX]."
  (dst nil :reader vm-dst)
  (string nil :reader vm-string-reg)
  (index nil :reader vm-index)
  (:sexp-tag :char)
  (:sexp-slots dst string index))

(define-vm-instruction vm-char-code (vm-instruction)
  "Character code. DST = ASCII/Unicode code of SRC character."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :char-code)
  (:sexp-slots dst src))

(define-vm-instruction vm-code-char (vm-instruction)
  "Character from code. DST = character with code SRC."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :code-char)
  (:sexp-slots dst src))

(define-vm-instruction vm-char= (vm-instruction)
  "Character equality. Returns 1 if CHAR1 equals CHAR2, 0 otherwise."
  (dst nil :reader vm-dst)
  (char1 nil :reader vm-char1)
  (char2 nil :reader vm-char2)
  (:sexp-tag :char=)
  (:sexp-slots dst char1 char2))

(define-vm-instruction vm-char< (vm-instruction)
  "Character less than. Returns 1 if CHAR1 < CHAR2, 0 otherwise."
  (dst nil :reader vm-dst)
  (char1 nil :reader vm-char1)
  (char2 nil :reader vm-char2)
  (:sexp-tag :char<)
  (:sexp-slots dst char1 char2))

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
  (:sexp-tag :concatenate)
  (:sexp-slots dst str1 str2))

(define-vm-instruction vm-string-upcase (vm-instruction)
  "Uppercase conversion. DST = uppercase of SRC."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :string-upcase)
  (:sexp-slots dst src))

(define-vm-instruction vm-string-downcase (vm-instruction)
  "Lowercase conversion. DST = lowercase of SRC."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :string-downcase)
  (:sexp-slots dst src))

(define-vm-instruction vm-string-capitalize (vm-instruction)
  "Capitalize string. DST = capitalized form of SRC."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :string-capitalize)
  (:sexp-slots dst src))

(define-vm-instruction vm-string-trim (vm-instruction)
  "Trim characters from both ends. DST = STRING with CHAR-BAG chars trimmed from both ends."
  (dst nil :reader vm-dst)
  (char-bag nil :reader vm-char-bag)
  (string nil :reader vm-string-reg)
  (:sexp-tag :string-trim)
  (:sexp-slots dst char-bag string))

(define-vm-instruction vm-string-left-trim (vm-instruction)
  "Trim characters from left. DST = STRING with CHAR-BAG chars trimmed from left."
  (dst nil :reader vm-dst)
  (char-bag nil :reader vm-char-bag)
  (string nil :reader vm-string-reg)
  (:sexp-tag :string-left-trim)
  (:sexp-slots dst char-bag string))

(define-vm-instruction vm-string-right-trim (vm-instruction)
  "Trim characters from right. DST = STRING with CHAR-BAG chars trimmed from right."
  (dst nil :reader vm-dst)
  (char-bag nil :reader vm-char-bag)
  (string nil :reader vm-string-reg)
  (:sexp-tag :string-right-trim)
  (:sexp-slots dst char-bag string))

;;; String Search Instructions

(define-vm-instruction vm-search-string (vm-instruction)
  "Search for pattern in string. DST = index of PATTERN in STRING from START, or -1 if not found."
  (dst nil :reader vm-dst)
  (pattern nil :reader vm-pattern)
  (string nil :reader vm-string-reg)
  (start nil :reader vm-start)
  (:sexp-tag :search-string)
  (:sexp-slots dst pattern string start))

;;; S-expression -> Instruction Conversion (Extended)

;;; Instruction Execution - String Comparisons

(define-simple-instruction vm-string= :pred2 string= :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string< :pred2 string< :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string> :pred2 string> :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string<= :pred2 string<= :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string>= :pred2 string>= :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string-equal :pred2 string-equal :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string-lessp :pred2 string-lessp :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string-greaterp :pred2 string-greaterp :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string-not-equal :pred2 string/= :lhs vm-str1 :rhs vm-str2)

;;; Instruction Execution - String Access and Query

(define-simple-instruction vm-string-length :unary length)

(defmethod execute-instruction ((inst vm-char) state pc labels)
  (declare (ignore labels))
  (let* ((string (vm-reg-get state (vm-string-reg inst)))
         (index (vm-reg-get state (vm-index inst)))
         (result (char string index)))
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
         (start (vm-reg-get state (vm-start inst)))
         (end (vm-reg-get state (vm-end inst)))
         (result (subseq string start end)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-concatenate) state pc labels)
  (declare (ignore labels))
  (let ((result (concatenate 'string
                              (vm-reg-get state (vm-str1 inst))
                              (vm-reg-get state (vm-str2 inst)))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-string-upcase :unary string-upcase)
(define-simple-instruction vm-string-downcase :unary string-downcase)
(define-simple-instruction vm-string-capitalize :unary string-capitalize)

(defmethod execute-instruction ((inst vm-string-trim) state pc labels)
  (declare (ignore labels))
  (let ((result (string-trim (vm-reg-get state (vm-char-bag inst))
                             (vm-reg-get state (vm-string-reg inst)))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-string-left-trim) state pc labels)
  (declare (ignore labels))
  (let ((result (string-left-trim (vm-reg-get state (vm-char-bag inst))
                                  (vm-reg-get state (vm-string-reg inst)))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-string-right-trim) state pc labels)
  (declare (ignore labels))
  (let ((result (string-right-trim (vm-reg-get state (vm-char-bag inst))
                                   (vm-reg-get state (vm-string-reg inst)))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Instruction Execution - String Search

(defmethod execute-instruction ((inst vm-search-string) state pc labels)
  (declare (ignore labels))
  (let* ((pattern (vm-reg-get state (vm-pattern inst)))
         (string (vm-reg-get state (vm-string-reg inst)))
         (start (vm-reg-get state (vm-start inst)))
         (result (or (search pattern string :start2 start) -1)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Symbol Manipulation Instructions

(define-vm-instruction vm-symbol-name (vm-instruction)
  "Get the name string of a symbol."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :symbol-name)
  (:sexp-slots dst src))

(define-vm-instruction vm-make-symbol (vm-instruction)
  "Create an uninterned symbol from a string."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :make-symbol)
  (:sexp-slots dst src))

(define-vm-instruction vm-intern-symbol (vm-instruction)
  "Intern a string as a symbol. Optional package designator."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (pkg nil :reader vm-intern-pkg)
  (:sexp-tag :intern)
  (:sexp-slots dst src))

(define-vm-instruction vm-gensym-inst (vm-instruction)
  "Generate a unique uninterned symbol."
  (dst nil :reader vm-dst)
  (:sexp-tag :gensym)
  (:sexp-slots dst))

(define-vm-instruction vm-keywordp (vm-instruction)
  "Test if value is a keyword symbol. Returns 1 if true, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :keywordp)
  (:sexp-slots dst src))

;; Symbol execute-instruction
(define-simple-instruction vm-symbol-name :unary symbol-name)
(define-simple-instruction vm-make-symbol :unary make-symbol)

(defmethod execute-instruction ((inst vm-intern-symbol) state pc labels)
  (declare (ignore labels))
  (let* ((name (vm-reg-get state (vm-src inst)))
         (pkg-designator (when (vm-intern-pkg inst)
                           (vm-reg-get state (vm-intern-pkg inst))))
         (result (if pkg-designator
                     (intern name (find-package pkg-designator))
                     (intern name))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-gensym-inst) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (gensym))
  (values (1+ pc) nil nil))

(define-simple-instruction vm-keywordp :pred1 keywordp)

;;; Character Predicate Instructions

(define-vm-instruction vm-digit-char-p (vm-instruction)
  "Test if character is a digit. Returns weight or nil."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :digit-char-p)
  (:sexp-slots dst src))

(define-vm-instruction vm-alpha-char-p (vm-instruction)
  "Test if character is alphabetic. Returns 1/0."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :alpha-char-p)
  (:sexp-slots dst src))

(define-vm-instruction vm-upper-case-p (vm-instruction)
  "Test if character is upper case. Returns 1/0."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :upper-case-p)
  (:sexp-slots dst src))

(define-vm-instruction vm-lower-case-p (vm-instruction)
  "Test if character is lower case. Returns 1/0."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :lower-case-p)
  (:sexp-slots dst src))

(define-vm-instruction vm-char-upcase (vm-instruction)
  "Upcase a character. DST = uppercase of SRC."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :char-upcase)
  (:sexp-slots dst src))

(define-vm-instruction vm-char-downcase (vm-instruction)
  "Downcase a character. DST = lowercase of SRC."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :char-downcase)
  (:sexp-slots dst src))

(define-vm-instruction vm-stringp (vm-instruction)
  "Test if value is a string. Returns 1/0."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :stringp)
  (:sexp-slots dst src))

(define-vm-instruction vm-characterp (vm-instruction)
  "Test if value is a character. Returns 1/0."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :characterp)
  (:sexp-slots dst src))

(define-vm-instruction vm-parse-integer (vm-instruction)
  "Parse an integer from a string. DST = integer value."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :parse-integer)
  (:sexp-slots dst src))

;; Character predicate execute-instruction
(defmethod execute-instruction ((inst vm-digit-char-p) state pc labels)
  (declare (ignore labels))
  (let* ((ch (vm-reg-get state (vm-src inst)))
         (result (digit-char-p ch)))
    (vm-reg-set state (vm-dst inst) (or result nil))
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-alpha-char-p :pred1 alpha-char-p)
(define-simple-instruction vm-upper-case-p :pred1 upper-case-p)
(define-simple-instruction vm-lower-case-p :pred1 lower-case-p)
(define-simple-instruction vm-char-upcase :unary char-upcase)
(define-simple-instruction vm-char-downcase :unary char-downcase)
(define-simple-instruction vm-stringp :pred1 stringp)
(define-simple-instruction vm-characterp :pred1 characterp)
(define-simple-instruction vm-parse-integer :unary parse-integer)

(define-vm-instruction vm-alphanumericp (vm-instruction)
  "Test if character is alphanumeric. Returns 1/0."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :alphanumericp)
  (:sexp-slots dst src))

(define-simple-instruction vm-alphanumericp :pred1 alphanumericp)
