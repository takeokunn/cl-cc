;;;; tests/unit/vm/strings-tests.lisp — VM String & Character Operations Unit Tests
;;;;
;;;; Tests for string comparison, manipulation, character access, and symbol
;;;; instructions executed via the VM.

(in-package :cl-cc/test)

(defsuite strings-suite :description "VM string and character operations unit tests")

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun str-vm ()
  "Create a minimal vm-state for string tests."
  (make-instance 'cl-cc::vm-state))

(defun str-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc::execute-instruction inst state 0 (make-hash-table :test #'equal)))

;;; ─── String Comparisons (case-sensitive) ──────────────────────────────────

(deftest str-equal-true
  "vm-string= returns 1 for equal strings."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "hello")
    (cl-cc::vm-reg-set s :R2 "hello")
    (str-exec (cl-cc::make-vm-string= :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest str-equal-false
  "vm-string= returns 0 for different strings."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "hello")
    (cl-cc::vm-reg-set s :R2 "world")
    (str-exec (cl-cc::make-vm-string= :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-equal 0 (cl-cc::vm-reg-get s :R0))))

(deftest str-less-than
  "vm-string< returns 1 when str1 < str2."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "abc")
    (cl-cc::vm-reg-set s :R2 "abd")
    (str-exec (cl-cc::make-vm-string< :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest str-greater-than
  "vm-string> returns 1 when str1 > str2."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "xyz")
    (cl-cc::vm-reg-set s :R2 "abc")
    (str-exec (cl-cc::make-vm-string> :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest str-less-equal
  "vm-string<= returns 1 for equal strings."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "abc")
    (cl-cc::vm-reg-set s :R2 "abc")
    (str-exec (cl-cc::make-vm-string<= :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest str-greater-equal
  "vm-string>= returns 1 when str1 > str2."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "xyz")
    (cl-cc::vm-reg-set s :R2 "abc")
    (str-exec (cl-cc::make-vm-string>= :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

;;; ─── String Comparisons (case-insensitive) ────────────────────────────────

(deftest str-equal-insensitive
  "vm-string-equal ignores case."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "Hello")
    (cl-cc::vm-reg-set s :R2 "hello")
    (str-exec (cl-cc::make-vm-string-equal :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest str-lessp-insensitive
  "vm-string-lessp does case-insensitive less-than."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "ABC")
    (cl-cc::vm-reg-set s :R2 "abd")
    (str-exec (cl-cc::make-vm-string-lessp :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest str-greaterp-insensitive
  "vm-string-greaterp does case-insensitive greater-than."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "XYZ")
    (cl-cc::vm-reg-set s :R2 "abc")
    (str-exec (cl-cc::make-vm-string-greaterp :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest str-not-equal-insensitive
  "vm-string-not-equal returns 1 for different strings."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "abc")
    (cl-cc::vm-reg-set s :R2 "xyz")
    (str-exec (cl-cc::make-vm-string-not-equal :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

;;; ─── String Length ────────────────────────────────────────────────────────

(deftest str-length-basic
  "vm-string-length returns correct length."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "hello")
    (str-exec (cl-cc::make-vm-string-length :dst :R0 :src :R1) s)
    (assert-equal 5 (cl-cc::vm-reg-get s :R0))))

(deftest str-length-empty
  "vm-string-length returns 0 for empty string."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "")
    (str-exec (cl-cc::make-vm-string-length :dst :R0 :src :R1) s)
    (assert-equal 0 (cl-cc::vm-reg-get s :R0))))

;;; ─── Character Access ─────────────────────────────────────────────────────

(deftest str-char-at
  "vm-char extracts character at index."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "hello")
    (cl-cc::vm-reg-set s :R2 0)
    (str-exec (cl-cc::make-vm-char :dst :R0 :string :R1 :index :R2) s)
    (assert-equal #\h (cl-cc::vm-reg-get s :R0))))

(deftest str-char-code-basic
  "vm-char-code returns ASCII code."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\A)
    (str-exec (cl-cc::make-vm-char-code :dst :R0 :src :R1) s)
    (assert-equal 65 (cl-cc::vm-reg-get s :R0))))

(deftest str-code-char-basic
  "vm-code-char returns character from code."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 65)
    (str-exec (cl-cc::make-vm-code-char :dst :R0 :src :R1) s)
    (assert-equal #\A (cl-cc::vm-reg-get s :R0))))

;;; ─── Character Comparisons ────────────────────────────────────────────────

(deftest char-equal-true
  "vm-char= returns 1 for equal characters."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\a)
    (cl-cc::vm-reg-set s :R2 #\a)
    (str-exec (cl-cc::make-vm-char= :dst :R0 :char1 :R1 :char2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest char-less-than
  "vm-char< returns 1 when char1 < char2."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\a)
    (cl-cc::vm-reg-set s :R2 #\b)
    (str-exec (cl-cc::make-vm-char< :dst :R0 :char1 :R1 :char2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest char-greater-than
  "vm-char> returns 1 when char1 > char2."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\z)
    (cl-cc::vm-reg-set s :R2 #\a)
    (str-exec (cl-cc::make-vm-char> :dst :R0 :char1 :R1 :char2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest char-less-equal
  "vm-char<= returns 1 for equal characters."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\a)
    (cl-cc::vm-reg-set s :R2 #\a)
    (str-exec (cl-cc::make-vm-char<= :dst :R0 :char1 :R1 :char2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest char-greater-equal
  "vm-char>= returns 1 when char1 >= char2."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\z)
    (cl-cc::vm-reg-set s :R2 #\a)
    (str-exec (cl-cc::make-vm-char>= :dst :R0 :char1 :R1 :char2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest char-not-equal
  "vm-char/= returns 1 for different characters."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\a)
    (cl-cc::vm-reg-set s :R2 #\b)
    (str-exec (cl-cc::make-vm-char/= :dst :R0 :char1 :R1 :char2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest char-equal-insensitive
  "vm-char-equal ignores case."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\A)
    (cl-cc::vm-reg-set s :R2 #\a)
    (str-exec (cl-cc::make-vm-char-equal :dst :R0 :char1 :R1 :char2 :R2) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

;;; ─── String Manipulation ──────────────────────────────────────────────────

(deftest str-upcase
  "vm-string-upcase converts to uppercase."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "hello")
    (str-exec (cl-cc::make-vm-string-upcase :dst :R0 :src :R1) s)
    (assert-equal "HELLO" (cl-cc::vm-reg-get s :R0))))

(deftest str-downcase
  "vm-string-downcase converts to lowercase."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "HELLO")
    (str-exec (cl-cc::make-vm-string-downcase :dst :R0 :src :R1) s)
    (assert-equal "hello" (cl-cc::vm-reg-get s :R0))))

(deftest str-capitalize
  "vm-string-capitalize capitalizes first letter of each word."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "hello world")
    (str-exec (cl-cc::make-vm-string-capitalize :dst :R0 :src :R1) s)
    (assert-equal "Hello World" (cl-cc::vm-reg-get s :R0))))

(deftest str-concatenate
  "vm-concatenate joins two strings."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "hello")
    (cl-cc::vm-reg-set s :R2 " world")
    (str-exec (cl-cc::make-vm-concatenate :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-equal "hello world" (cl-cc::vm-reg-get s :R0))))

(deftest str-subseq
  "vm-subseq extracts substring."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "hello world")
    (cl-cc::vm-reg-set s :R2 6)
    (cl-cc::vm-reg-set s :R3 11)
    (str-exec (cl-cc::make-vm-subseq :dst :R0 :string :R1 :start :R2 :end :R3) s)
    (assert-equal "world" (cl-cc::vm-reg-get s :R0))))

;;; ─── String Trim ──────────────────────────────────────────────────────────

(deftest str-trim-both
  "vm-string-trim removes chars from both ends."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 " ")
    (cl-cc::vm-reg-set s :R2 "  hello  ")
    (str-exec (cl-cc::make-vm-string-trim :dst :R0 :char-bag :R1 :string :R2) s)
    (assert-equal "hello" (cl-cc::vm-reg-get s :R0))))

(deftest str-trim-left
  "vm-string-left-trim removes chars from left only."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 " ")
    (cl-cc::vm-reg-set s :R2 "  hello  ")
    (str-exec (cl-cc::make-vm-string-left-trim :dst :R0 :char-bag :R1 :string :R2) s)
    (assert-equal "hello  " (cl-cc::vm-reg-get s :R0))))

(deftest str-trim-right
  "vm-string-right-trim removes chars from right only."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 " ")
    (cl-cc::vm-reg-set s :R2 "  hello  ")
    (str-exec (cl-cc::make-vm-string-right-trim :dst :R0 :char-bag :R1 :string :R2) s)
    (assert-equal "  hello" (cl-cc::vm-reg-get s :R0))))

;;; ─── String Search ────────────────────────────────────────────────────────

(deftest str-search-found
  "vm-search-string returns index when pattern found."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "world")
    (cl-cc::vm-reg-set s :R2 "hello world")
    (cl-cc::vm-reg-set s :R3 0)
    (str-exec (cl-cc::make-vm-search-string :dst :R0 :pattern :R1 :string :R2 :start :R3) s)
    (assert-equal 6 (cl-cc::vm-reg-get s :R0))))

(deftest str-search-not-found
  "vm-search-string returns -1 when pattern not found."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "xyz")
    (cl-cc::vm-reg-set s :R2 "hello world")
    (cl-cc::vm-reg-set s :R3 0)
    (str-exec (cl-cc::make-vm-search-string :dst :R0 :pattern :R1 :string :R2 :start :R3) s)
    (assert-equal -1 (cl-cc::vm-reg-get s :R0))))

;;; ─── Make String ──────────────────────────────────────────────────────────

(deftest str-make-string-default
  "vm-make-string creates string of spaces by default."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 3)
    (str-exec (cl-cc::make-vm-make-string :dst :R0 :src :R1 :char nil) s)
    (assert-equal "   " (cl-cc::vm-reg-get s :R0))))

(deftest str-make-string-with-char
  "vm-make-string uses provided initial character."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 4)
    (cl-cc::vm-reg-set s :R2 #\x)
    (str-exec (cl-cc::make-vm-make-string :dst :R0 :src :R1 :char :R2) s)
    (assert-equal "xxxx" (cl-cc::vm-reg-get s :R0))))

;;; ─── Symbol Operations ───────────────────────────────────────────────────

(deftest sym-symbol-name
  "vm-symbol-name returns symbol's name string."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 'hello)
    (str-exec (cl-cc::make-vm-symbol-name :dst :R0 :src :R1) s)
    (assert-equal "HELLO" (cl-cc::vm-reg-get s :R0))))

(deftest sym-make-symbol
  "vm-make-symbol creates uninterned symbol from string."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "FOO")
    (str-exec (cl-cc::make-vm-make-symbol :dst :R0 :src :R1) s)
    (let ((result (cl-cc::vm-reg-get s :R0)))
      (assert-true (symbolp result))
      (assert-equal "FOO" (symbol-name result)))))

(deftest sym-intern-symbol
  "vm-intern-symbol interns string as symbol."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "INTERN-TEST-SYM-12345")
    (str-exec (cl-cc::make-vm-intern-symbol :dst :R0 :src :R1 :pkg nil) s)
    (let ((result (cl-cc::vm-reg-get s :R0)))
      (assert-true (symbolp result))
      (assert-equal "INTERN-TEST-SYM-12345" (symbol-name result)))))

(deftest sym-gensym
  "vm-gensym-inst creates unique uninterned symbol."
  (let ((s (str-vm)))
    (str-exec (cl-cc::make-vm-gensym-inst :dst :R0) s)
    (let ((result (cl-cc::vm-reg-get s :R0)))
      (assert-true (symbolp result))
      (assert-equal nil (symbol-package result)))))

(deftest sym-keywordp-true
  "vm-keywordp returns 1 for keywords."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 :test)
    (str-exec (cl-cc::make-vm-keywordp :dst :R0 :src :R1) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest sym-keywordp-false
  "vm-keywordp returns 0 for non-keywords."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 'hello)
    (str-exec (cl-cc::make-vm-keywordp :dst :R0 :src :R1) s)
    (assert-equal 0 (cl-cc::vm-reg-get s :R0))))

;;; ─── Character Predicates ─────────────────────────────────────────────────

(deftest char-digit-char-p-true
  "vm-digit-char-p returns digit weight for digit characters."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\5)
    (str-exec (cl-cc::make-vm-digit-char-p :dst :R0 :src :R1) s)
    (assert-equal 5 (cl-cc::vm-reg-get s :R0))))

(deftest char-alpha-char-p-true
  "vm-alpha-char-p returns 1 for alphabetic characters."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\a)
    (str-exec (cl-cc::make-vm-alpha-char-p :dst :R0 :src :R1) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest char-upper-case-p-true
  "vm-upper-case-p returns 1 for uppercase characters."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\A)
    (str-exec (cl-cc::make-vm-upper-case-p :dst :R0 :src :R1) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest char-lower-case-p-true
  "vm-lower-case-p returns 1 for lowercase characters."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\a)
    (str-exec (cl-cc::make-vm-lower-case-p :dst :R0 :src :R1) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest char-upcase-convert
  "vm-char-upcase returns uppercase character."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\a)
    (str-exec (cl-cc::make-vm-char-upcase :dst :R0 :src :R1) s)
    (assert-equal #\A (cl-cc::vm-reg-get s :R0))))

(deftest char-downcase-convert
  "vm-char-downcase returns lowercase character."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\A)
    (str-exec (cl-cc::make-vm-char-downcase :dst :R0 :src :R1) s)
    (assert-equal #\a (cl-cc::vm-reg-get s :R0))))

(deftest char-alphanumericp-true
  "vm-alphanumericp returns 1 for alphanumeric characters."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\a)
    (str-exec (cl-cc::make-vm-alphanumericp :dst :R0 :src :R1) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest char-alphanumericp-false
  "vm-alphanumericp returns 0 for non-alphanumeric characters."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\!)
    (str-exec (cl-cc::make-vm-alphanumericp :dst :R0 :src :R1) s)
    (assert-equal 0 (cl-cc::vm-reg-get s :R0))))

;;; ─── Stringp / Characterp Predicates ──────────────────────────────────────

(deftest stringp-true
  "vm-stringp returns 1 for strings."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "hello")
    (str-exec (cl-cc::make-vm-stringp :dst :R0 :src :R1) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest stringp-false
  "vm-stringp returns 0 for non-strings."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 42)
    (str-exec (cl-cc::make-vm-stringp :dst :R0 :src :R1) s)
    (assert-equal 0 (cl-cc::vm-reg-get s :R0))))

(deftest characterp-true
  "vm-characterp returns 1 for characters."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 #\a)
    (str-exec (cl-cc::make-vm-characterp :dst :R0 :src :R1) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest characterp-false
  "vm-characterp returns 0 for non-characters."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 42)
    (str-exec (cl-cc::make-vm-characterp :dst :R0 :src :R1) s)
    (assert-equal 0 (cl-cc::vm-reg-get s :R0))))

;;; ─── Parse Integer ────────────────────────────────────────────────────────

(deftest parse-integer-basic
  "vm-parse-integer parses decimal string to integer."
  (let ((s (str-vm)))
    (cl-cc::vm-reg-set s :R1 "42")
    (str-exec (cl-cc::make-vm-parse-integer :dst :R0 :src :R1) s)
    (assert-equal 42 (cl-cc::vm-reg-get s :R0))))
