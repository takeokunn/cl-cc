(in-package :cl-cc)

;;; ----------------------------------------------------------------------------
;;; VM String Instructions
;;; ----------------------------------------------------------------------------
;;;
;;; This file extends the VM with string operations including comparisons,
;;; manipulation, and character access.

;;; ----------------------------------------------------------------------------
;;; String Comparison Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-string= (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (str1 :initarg :str1 :reader vm-str1)
   (str2 :initarg :str2 :reader vm-str2))
  (:documentation "Case-sensitive string equality. Returns 1 if STR1 equals STR2, 0 otherwise."))

(defclass vm-string< (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (str1 :initarg :str1 :reader vm-str1)
   (str2 :initarg :str2 :reader vm-str2))
  (:documentation "Case-sensitive string less than. Returns 1 if STR1 < STR2 lexicographically, 0 otherwise."))

(defclass vm-string> (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (str1 :initarg :str1 :reader vm-str1)
   (str2 :initarg :str2 :reader vm-str2))
  (:documentation "Case-sensitive string greater than. Returns 1 if STR1 > STR2 lexicographically, 0 otherwise."))

(defclass vm-string<= (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (str1 :initarg :str1 :reader vm-str1)
   (str2 :initarg :str2 :reader vm-str2))
  (:documentation "Case-sensitive string less than or equal. Returns 1 if STR1 <= STR2, 0 otherwise."))

(defclass vm-string>= (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (str1 :initarg :str1 :reader vm-str1)
   (str2 :initarg :str2 :reader vm-str2))
  (:documentation "Case-sensitive string greater than or equal. Returns 1 if STR1 >= STR2, 0 otherwise."))

(defclass vm-string-equal (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (str1 :initarg :str1 :reader vm-str1)
   (str2 :initarg :str2 :reader vm-str2))
  (:documentation "Case-insensitive string equality. Returns 1 if STR1 equals STR2 (ignoring case), 0 otherwise."))

(defclass vm-string-lessp (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (str1 :initarg :str1 :reader vm-str1)
   (str2 :initarg :str2 :reader vm-str2))
  (:documentation "Case-insensitive string less than. Returns 1 if STR1 < STR2 (ignoring case), 0 otherwise."))

(defclass vm-string-greaterp (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (str1 :initarg :str1 :reader vm-str1)
   (str2 :initarg :str2 :reader vm-str2))
  (:documentation "Case-insensitive string greater than. Returns 1 if STR1 > STR2 (ignoring case), 0 otherwise."))

(defclass vm-string-not-equal (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (str1 :initarg :str1 :reader vm-str1)
   (str2 :initarg :str2 :reader vm-str2))
  (:documentation "Case-sensitive string inequality. Returns 1 if STR1 not equal to STR2, 0 otherwise."))

;;; ----------------------------------------------------------------------------
;;; String Access and Query Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-string-length (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "String length. DST = length of SRC string."))

(defclass vm-char (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (string :initarg :string :reader vm-string-reg)
   (index :initarg :index :reader vm-index))
  (:documentation "Character at position. DST = STRING[INDEX]."))

(defclass vm-char-code (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Character code. DST = ASCII/Unicode code of SRC character."))

(defclass vm-code-char (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Character from code. DST = character with code SRC."))

(defclass vm-char= (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (char1 :initarg :char1 :reader vm-char1)
   (char2 :initarg :char2 :reader vm-char2))
  (:documentation "Character equality. Returns 1 if CHAR1 equals CHAR2, 0 otherwise."))

(defclass vm-char< (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (char1 :initarg :char1 :reader vm-char1)
   (char2 :initarg :char2 :reader vm-char2))
  (:documentation "Character less than. Returns 1 if CHAR1 < CHAR2, 0 otherwise."))

;;; ----------------------------------------------------------------------------
;;; String Manipulation Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-subseq (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (string :initarg :string :reader vm-string-reg)
   (start :initarg :start :reader vm-start)
   (end :initarg :end :reader vm-end))
  (:documentation "Substring extraction. DST = STRING[START:END]."))

(defclass vm-concatenate (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (str1 :initarg :str1 :reader vm-str1)
   (str2 :initarg :str2 :reader vm-str2))
  (:documentation "String concatenation. DST = STR1 + STR2."))

(defclass vm-string-upcase (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Uppercase conversion. DST = uppercase of SRC."))

(defclass vm-string-downcase (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Lowercase conversion. DST = lowercase of SRC."))

(defclass vm-string-capitalize (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Capitalize string. DST = capitalized form of SRC."))

(defclass vm-string-trim (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (char-bag :initarg :char-bag :reader vm-char-bag)
   (string :initarg :string :reader vm-string-reg))
  (:documentation "Trim characters from both ends. DST = STRING with CHAR-BAG chars trimmed from both ends."))

(defclass vm-string-left-trim (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (char-bag :initarg :char-bag :reader vm-char-bag)
   (string :initarg :string :reader vm-string-reg))
  (:documentation "Trim characters from left. DST = STRING with CHAR-BAG chars trimmed from left."))

(defclass vm-string-right-trim (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (char-bag :initarg :char-bag :reader vm-char-bag)
   (string :initarg :string :reader vm-string-reg))
  (:documentation "Trim characters from right. DST = STRING with CHAR-BAG chars trimmed from right."))

;;; ----------------------------------------------------------------------------
;;; String Search Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-search-string (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (pattern :initarg :pattern :reader vm-pattern)
   (string :initarg :string :reader vm-string-reg)
   (start :initarg :start :reader vm-start))
  (:documentation "Search for pattern in string. DST = index of PATTERN in STRING from START, or -1 if not found."))

;;; ----------------------------------------------------------------------------
;;; Instruction -> S-expression Conversion
;;; ----------------------------------------------------------------------------

;; String comparisons
(defmethod instruction->sexp ((inst vm-string=))
  (list :string= (vm-dst inst) (vm-str1 inst) (vm-str2 inst)))

(defmethod instruction->sexp ((inst vm-string<))
  (list :string< (vm-dst inst) (vm-str1 inst) (vm-str2 inst)))

(defmethod instruction->sexp ((inst vm-string>))
  (list :string> (vm-dst inst) (vm-str1 inst) (vm-str2 inst)))

(defmethod instruction->sexp ((inst vm-string<=))
  (list :string<= (vm-dst inst) (vm-str1 inst) (vm-str2 inst)))

(defmethod instruction->sexp ((inst vm-string>=))
  (list :string>= (vm-dst inst) (vm-str1 inst) (vm-str2 inst)))

(defmethod instruction->sexp ((inst vm-string-equal))
  (list :string-equal (vm-dst inst) (vm-str1 inst) (vm-str2 inst)))

(defmethod instruction->sexp ((inst vm-string-lessp))
  (list :string-lessp (vm-dst inst) (vm-str1 inst) (vm-str2 inst)))

(defmethod instruction->sexp ((inst vm-string-greaterp))
  (list :string-greaterp (vm-dst inst) (vm-str1 inst) (vm-str2 inst)))

(defmethod instruction->sexp ((inst vm-string-not-equal))
  (list :string-not-equal (vm-dst inst) (vm-str1 inst) (vm-str2 inst)))

;; String access and query
(defmethod instruction->sexp ((inst vm-string-length))
  (list :string-length (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-char))
  (list :char (vm-dst inst) (vm-string-reg inst) (vm-index inst)))

(defmethod instruction->sexp ((inst vm-char-code))
  (list :char-code (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-code-char))
  (list :code-char (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-char=))
  (list :char= (vm-dst inst) (vm-char1 inst) (vm-char2 inst)))

(defmethod instruction->sexp ((inst vm-char<))
  (list :char< (vm-dst inst) (vm-char1 inst) (vm-char2 inst)))

;; String manipulation
(defmethod instruction->sexp ((inst vm-subseq))
  (list :subseq (vm-dst inst) (vm-string-reg inst) (vm-start inst) (vm-end inst)))

(defmethod instruction->sexp ((inst vm-concatenate))
  (list :concatenate (vm-dst inst) (vm-str1 inst) (vm-str2 inst)))

(defmethod instruction->sexp ((inst vm-string-upcase))
  (list :string-upcase (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-string-downcase))
  (list :string-downcase (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-string-capitalize))
  (list :string-capitalize (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-string-trim))
  (list :string-trim (vm-dst inst) (vm-char-bag inst) (vm-string-reg inst)))

(defmethod instruction->sexp ((inst vm-string-left-trim))
  (list :string-left-trim (vm-dst inst) (vm-char-bag inst) (vm-string-reg inst)))

(defmethod instruction->sexp ((inst vm-string-right-trim))
  (list :string-right-trim (vm-dst inst) (vm-char-bag inst) (vm-string-reg inst)))

;; String search
(defmethod instruction->sexp ((inst vm-search-string))
  (list :search-string (vm-dst inst) (vm-pattern inst) (vm-string-reg inst) (vm-start inst)))

;;; ----------------------------------------------------------------------------
;;; S-expression -> Instruction Conversion (Extended)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - String Comparisons
;;; ----------------------------------------------------------------------------

(defmethod execute-instruction ((inst vm-string=) state pc labels)
  (declare (ignore labels))
  (let ((result (if (string= (vm-reg-get state (vm-str1 inst))
                             (vm-reg-get state (vm-str2 inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-string<) state pc labels)
  (declare (ignore labels))
  (let ((result (if (string< (vm-reg-get state (vm-str1 inst))
                             (vm-reg-get state (vm-str2 inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-string>) state pc labels)
  (declare (ignore labels))
  (let ((result (if (string> (vm-reg-get state (vm-str1 inst))
                             (vm-reg-get state (vm-str2 inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-string<=) state pc labels)
  (declare (ignore labels))
  (let ((result (if (string<= (vm-reg-get state (vm-str1 inst))
                              (vm-reg-get state (vm-str2 inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-string>=) state pc labels)
  (declare (ignore labels))
  (let ((result (if (string>= (vm-reg-get state (vm-str1 inst))
                              (vm-reg-get state (vm-str2 inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-string-equal) state pc labels)
  (declare (ignore labels))
  (let ((result (if (string-equal (vm-reg-get state (vm-str1 inst))
                                  (vm-reg-get state (vm-str2 inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-string-lessp) state pc labels)
  (declare (ignore labels))
  (let ((result (if (string-lessp (vm-reg-get state (vm-str1 inst))
                                  (vm-reg-get state (vm-str2 inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-string-greaterp) state pc labels)
  (declare (ignore labels))
  (let ((result (if (string-greaterp (vm-reg-get state (vm-str1 inst))
                                     (vm-reg-get state (vm-str2 inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-string-not-equal) state pc labels)
  (declare (ignore labels))
  (let ((result (if (string/= (vm-reg-get state (vm-str1 inst))
                              (vm-reg-get state (vm-str2 inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - String Access and Query
;;; ----------------------------------------------------------------------------

(defmethod execute-instruction ((inst vm-string-length) state pc labels)
  (declare (ignore labels))
  (let ((result (length (vm-reg-get state (vm-src inst)))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-char) state pc labels)
  (declare (ignore labels))
  (let* ((string (vm-reg-get state (vm-string-reg inst)))
         (index (vm-reg-get state (vm-index inst)))
         (result (char string index)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-char-code) state pc labels)
  (declare (ignore labels))
  (let ((result (char-code (vm-reg-get state (vm-src inst)))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-code-char) state pc labels)
  (declare (ignore labels))
  (let ((result (code-char (vm-reg-get state (vm-src inst)))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-char=) state pc labels)
  (declare (ignore labels))
  (let ((result (if (char= (vm-reg-get state (vm-char1 inst))
                          (vm-reg-get state (vm-char2 inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-char<) state pc labels)
  (declare (ignore labels))
  (let ((result (if (char< (vm-reg-get state (vm-char1 inst))
                          (vm-reg-get state (vm-char2 inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - String Manipulation
;;; ----------------------------------------------------------------------------

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

(defmethod execute-instruction ((inst vm-string-upcase) state pc labels)
  (declare (ignore labels))
  (let ((result (string-upcase (vm-reg-get state (vm-src inst)))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-string-downcase) state pc labels)
  (declare (ignore labels))
  (let ((result (string-downcase (vm-reg-get state (vm-src inst)))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-string-capitalize) state pc labels)
  (declare (ignore labels))
  (let ((result (string-capitalize (vm-reg-get state (vm-src inst)))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

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

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - String Search
;;; ----------------------------------------------------------------------------

(defmethod execute-instruction ((inst vm-search-string) state pc labels)
  (declare (ignore labels))
  (let* ((pattern (vm-reg-get state (vm-pattern inst)))
         (string (vm-reg-get state (vm-string-reg inst)))
         (start (vm-reg-get state (vm-start inst)))
         (result (or (search pattern string :start2 start) -1)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; ----------------------------------------------------------------------------
;;; Symbol Manipulation Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-symbol-name (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Get the name string of a symbol."))

(defclass vm-make-symbol (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Create an uninterned symbol from a string."))

(defclass vm-intern-symbol (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src)
   (pkg :initarg :pkg :reader vm-intern-pkg :initform nil
        :documentation "Optional register containing package designator"))
  (:documentation "Intern a string as a symbol. Optional package designator."))

(defclass vm-gensym-inst (vm-instruction)
  ((dst :initarg :dst :reader vm-dst))
  (:documentation "Generate a unique uninterned symbol."))

(defclass vm-keywordp (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Test if value is a keyword symbol. Returns 1 if true, 0 otherwise."))

;; Symbol instruction->sexp
(defmethod instruction->sexp ((inst vm-symbol-name))
  (list :symbol-name (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-make-symbol))
  (list :make-symbol (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-intern-symbol))
  (list :intern (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-gensym-inst))
  (list :gensym (vm-dst inst)))

(defmethod instruction->sexp ((inst vm-keywordp))
  (list :keywordp (vm-dst inst) (vm-src inst)))

;; Symbol execute-instruction
(defmethod execute-instruction ((inst vm-symbol-name) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (symbol-name (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-make-symbol) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (make-symbol (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

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

(defmethod execute-instruction ((inst vm-keywordp) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (if (keywordp (vm-reg-get state (vm-src inst))) 1 0))
  (values (1+ pc) nil nil))

;;; ----------------------------------------------------------------------------
;;; Character Predicate Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-digit-char-p (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Test if character is a digit. Returns weight or nil."))

(defclass vm-alpha-char-p (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Test if character is alphabetic. Returns 1/0."))

(defclass vm-upper-case-p (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Test if character is upper case. Returns 1/0."))

(defclass vm-lower-case-p (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Test if character is lower case. Returns 1/0."))

(defclass vm-char-upcase (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Upcase a character. DST = uppercase of SRC."))

(defclass vm-char-downcase (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Downcase a character. DST = lowercase of SRC."))

(defclass vm-stringp (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Test if value is a string. Returns 1/0."))

(defclass vm-characterp (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Test if value is a character. Returns 1/0."))

(defclass vm-parse-integer (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Parse an integer from a string. DST = integer value."))

;; Character predicate instruction->sexp
(defmethod instruction->sexp ((inst vm-digit-char-p))
  (list :digit-char-p (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-alpha-char-p))
  (list :alpha-char-p (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-upper-case-p))
  (list :upper-case-p (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-lower-case-p))
  (list :lower-case-p (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-char-upcase))
  (list :char-upcase (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-char-downcase))
  (list :char-downcase (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-stringp))
  (list :stringp (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-characterp))
  (list :characterp (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-parse-integer))
  (list :parse-integer (vm-dst inst) (vm-src inst)))

;; Character predicate execute-instruction
(defmethod execute-instruction ((inst vm-digit-char-p) state pc labels)
  (declare (ignore labels))
  (let* ((ch (vm-reg-get state (vm-src inst)))
         (result (digit-char-p ch)))
    (vm-reg-set state (vm-dst inst) (or result nil))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-alpha-char-p) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (alpha-char-p (vm-reg-get state (vm-src inst))) 1 0))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-upper-case-p) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (upper-case-p (vm-reg-get state (vm-src inst))) 1 0))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-lower-case-p) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (lower-case-p (vm-reg-get state (vm-src inst))) 1 0))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-char-upcase) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (char-upcase (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-char-downcase) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (char-downcase (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-stringp) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (stringp (vm-reg-get state (vm-src inst))) 1 0))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-characterp) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (characterp (vm-reg-get state (vm-src inst))) 1 0))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-parse-integer) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (parse-integer (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(defclass vm-alphanumericp (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Test if character is alphanumeric. Returns 1/0."))

(defmethod instruction->sexp ((inst vm-alphanumericp))
  (list :alphanumericp (vm-dst inst) (vm-src inst)))

(defmethod execute-instruction ((inst vm-alphanumericp) state pc labels)
  (declare (ignore labels))
  (let* ((ch (vm-reg-get state (vm-src inst)))
         (result (if (alphanumericp ch) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))
