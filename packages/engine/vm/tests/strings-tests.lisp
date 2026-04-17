;;;; tests/unit/vm/strings-tests.lisp — VM String & Character Operations Unit Tests
;;;;
;;;; Tests for string comparison, manipulation, character access, and symbol
;;;; instructions executed via the VM.

(in-package :cl-cc/test)

(defsuite strings-suite
  :description "VM string and character operations unit tests"
  :parent cl-cc-unit-suite)

(in-suite strings-suite)

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun str-vm ()
  "Create a minimal vm-state for string tests."
  (make-instance 'cl-cc/vm::vm-state))

(defun str-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc/vm::execute-instruction inst state 0 (make-hash-table :test #'equal)))

;;; ─── String Comparisons (case-sensitive) ──────────────────────────────────

(deftest-each str-comparison-truthy
  "Binary string comparison instructions return truthy (T or mismatch index) on their respective true condition."
  :cases (("equal"         #'cl-cc::make-vm-string=  "hello" "hello")
          ("less-than"     #'cl-cc::make-vm-string<  "abc"   "abd")
          ("greater-than"  #'cl-cc::make-vm-string>  "xyz"   "abc")
          ("less-equal"    #'cl-cc::make-vm-string<= "abc"   "abc")
          ("greater-equal" #'cl-cc::make-vm-string>= "xyz"   "abc"))
  (ctor str1 str2)
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 str1)
    (cl-cc/vm::vm-reg-set s :R2 str2)
    (str-exec (funcall ctor :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-true (cl-cc/vm::vm-reg-get s :R0))))

(deftest str-equal-false
  "vm-string= returns NIL for different strings."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (cl-cc/vm::vm-reg-set s :R2 "world")
    (str-exec (cl-cc::make-vm-string= :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-true (null (cl-cc/vm::vm-reg-get s :R0)))))

;;; ─── String Comparisons (case-insensitive) ────────────────────────────────

(deftest-each str-insensitive-comparison-truthy
  "Case-insensitive string comparison instructions return truthy on their respective true condition."
  :cases (("equal"     #'cl-cc::make-vm-string-equal     "Hello" "hello")
          ("lessp"     #'cl-cc::make-vm-string-lessp     "ABC"   "abd")
          ("greaterp"  #'cl-cc::make-vm-string-greaterp  "XYZ"   "abc")
          ("not-equal" #'cl-cc::make-vm-string-not-equal "abc"   "xyz"))
  (ctor str1 str2)
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 str1)
    (cl-cc/vm::vm-reg-set s :R2 str2)
    (str-exec (funcall ctor :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-true (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── String Length ────────────────────────────────────────────────────────

(deftest str-length
  "vm-string-length returns correct length for non-empty and empty strings."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (str-exec (cl-cc::make-vm-string-length :dst :R0 :src :R1) s)
    (assert-equal 5 (cl-cc/vm::vm-reg-get s :R0)))
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "")
    (str-exec (cl-cc::make-vm-string-length :dst :R0 :src :R1) s)
    (assert-equal 0 (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── Character Access ─────────────────────────────────────────────────────

(deftest str-char-at
  "vm-char extracts character at index."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (cl-cc/vm::vm-reg-set s :R2 0)
    (str-exec (cl-cc::make-vm-char :dst :R0 :string :R1 :index :R2) s)
    (assert-equal #\h (cl-cc/vm::vm-reg-get s :R0))))

(deftest str-char-code-basic
  "vm-char-code returns ASCII code."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 #\A)
    (str-exec (cl-cc::make-vm-char-code :dst :R0 :src :R1) s)
    (assert-equal 65 (cl-cc/vm::vm-reg-get s :R0))))

(deftest str-code-char-basic
  "vm-code-char returns character from code."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 65)
    (str-exec (cl-cc::make-vm-code-char :dst :R0 :src :R1) s)
    (assert-equal #\A (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── Character Comparisons ────────────────────────────────────────────────

(deftest-each char-comparison-returns-1
  "Binary character comparison instructions return 1 on their respective true condition."
  :cases (("equal"     #'cl-cc::make-vm-char=    #\a #\a)
          ("less-than" #'cl-cc::make-vm-char<    #\a #\b)
          ("greater"   #'cl-cc::make-vm-char>    #\z #\a)
          ("le-equal"  #'cl-cc::make-vm-char<=   #\a #\a)
          ("ge-greater" #'cl-cc::make-vm-char>=  #\z #\a)
          ("not-equal" #'cl-cc::make-vm-char/=   #\a #\b)
          ("ci-equal"  #'cl-cc::make-vm-char-equal #\A #\a))
  (ctor char1 char2)
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 char1)
    (cl-cc/vm::vm-reg-set s :R2 char2)
    (str-exec (funcall ctor :dst :R0 :char1 :R1 :char2 :R2) s)
    (assert-equal 1 (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── String Manipulation ──────────────────────────────────────────────────

(deftest-each str-case-conversion
  "vm-string-upcase/downcase convert string case."
  :cases (("upcase"   #'cl-cc::make-vm-string-upcase   "hello" "HELLO")
          ("downcase" #'cl-cc::make-vm-string-downcase "HELLO" "hello"))
  (ctor input expected)
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 input)
    (str-exec (funcall ctor :dst :R0 :src :R1) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

(deftest str-capitalize
  "vm-string-capitalize capitalizes first letter of each word."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello world")
    (str-exec (cl-cc::make-vm-string-capitalize :dst :R0 :src :R1) s)
    (assert-equal "Hello World" (cl-cc/vm::vm-reg-get s :R0))))

(deftest str-concatenate
  "vm-concatenate joins two strings."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (cl-cc/vm::vm-reg-set s :R2 " world")
    (str-exec (cl-cc::make-vm-concatenate :dst :R0 :str1 :R1 :str2 :R2) s)
    (assert-equal "hello world" (cl-cc/vm::vm-reg-get s :R0))))

(deftest str-subseq
  "vm-subseq extracts substring."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello world")
    (cl-cc/vm::vm-reg-set s :R2 6)
    (cl-cc/vm::vm-reg-set s :R3 11)
    (str-exec (cl-cc::make-vm-subseq :dst :R0 :string :R1 :start :R2 :end :R3) s)
    (assert-equal "world" (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── String Trim ──────────────────────────────────────────────────────────

(deftest-each str-trim-directions
  "vm-string-trim/left-trim/right-trim remove chars from the specified end(s)."
  :cases (("both"  #'cl-cc::make-vm-string-trim       "  hello  " "hello")
          ("left"  #'cl-cc::make-vm-string-left-trim  "  hello  " "hello  ")
          ("right" #'cl-cc::make-vm-string-right-trim "  hello  " "  hello"))
  (ctor input expected)
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 " ")
    (cl-cc/vm::vm-reg-set s :R2 input)
    (str-exec (funcall ctor :dst :R0 :char-bag :R1 :string :R2) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── String Search ────────────────────────────────────────────────────────

(deftest str-search-behavior
  "vm-search-string returns index on hit; -1 on miss."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "world")
    (cl-cc/vm::vm-reg-set s :R2 "hello world")
    (cl-cc/vm::vm-reg-set s :R3 0)
    (str-exec (cl-cc::make-vm-search-string :dst :R0 :pattern :R1 :string :R2 :start :R3) s)
    (assert-equal 6 (cl-cc/vm::vm-reg-get s :R0)))
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "xyz")
    (cl-cc/vm::vm-reg-set s :R2 "hello world")
    (cl-cc/vm::vm-reg-set s :R3 0)
    (str-exec (cl-cc::make-vm-search-string :dst :R0 :pattern :R1 :string :R2 :start :R3) s)
    (assert-equal -1 (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── Make String ──────────────────────────────────────────────────────────

(deftest str-make-string-default
  "vm-make-string creates string of spaces by default."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 3)
    (str-exec (cl-cc::make-vm-make-string :dst :R0 :src :R1 :char nil) s)
    (assert-equal "   " (cl-cc/vm::vm-reg-get s :R0))))

(deftest str-make-string-with-char
  "vm-make-string uses provided initial character."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 4)
    (cl-cc/vm::vm-reg-set s :R2 #\x)
    (str-exec (cl-cc::make-vm-make-string :dst :R0 :src :R1 :char :R2) s)
    (assert-equal "xxxx" (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── Character Predicates ─────────────────────────────────────────────────

(deftest char-digit-char-p-true
  "vm-digit-char-p returns digit weight for digit characters."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 #\5)
    (str-exec (cl-cc::make-vm-digit-char-p :dst :R0 :src :R1) s)
    (assert-equal 5 (cl-cc/vm::vm-reg-get s :R0))))

(deftest char-alpha-char-p-true
  "vm-alpha-char-p returns 1 for alphabetic characters."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 #\a)
    (str-exec (cl-cc::make-vm-alpha-char-p :dst :R0 :src :R1) s)
    (assert-equal 1 (cl-cc/vm::vm-reg-get s :R0))))

(deftest char-upper-case-p-true
  "vm-upper-case-p returns 1 for uppercase characters."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 #\A)
    (str-exec (cl-cc::make-vm-upper-case-p :dst :R0 :src :R1) s)
    (assert-equal 1 (cl-cc/vm::vm-reg-get s :R0))))

(deftest char-lower-case-p-true
  "vm-lower-case-p returns 1 for lowercase characters."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 #\a)
    (str-exec (cl-cc::make-vm-lower-case-p :dst :R0 :src :R1) s)
    (assert-equal 1 (cl-cc/vm::vm-reg-get s :R0))))

(deftest-each char-case-conversion
  "vm-char-upcase/downcase convert character case."
  :cases (("upcase"   #'cl-cc::make-vm-char-upcase   #\a #\A)
          ("downcase" #'cl-cc::make-vm-char-downcase #\A #\a))
  (ctor input expected)
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 input)
    (str-exec (funcall ctor :dst :R0 :src :R1) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

(deftest-each char-alphanumericp
  "vm-alphanumericp returns 1 for alphanumeric characters, 0 for others."
  :cases (("alpha"       #\a 1)
          ("non-alnum"   #\! 0))
  (ch expected)
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 ch)
    (str-exec (cl-cc::make-vm-alphanumericp :dst :R0 :src :R1) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── Stringp / Characterp Predicates ──────────────────────────────────────

(deftest-each stringp
  "vm-stringp returns 1 for strings, 0 for non-strings."
  :cases (("string"     "hello" 1)
          ("non-string" 42      0))
  (value expected)
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (str-exec (cl-cc::make-vm-stringp :dst :R0 :src :R1) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

(deftest-each characterp
  "vm-characterp returns 1 for characters, 0 for non-characters."
  :cases (("char"     #\a 1)
          ("non-char" 42  0))
  (value expected)
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (str-exec (cl-cc::make-vm-characterp :dst :R0 :src :R1) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── Parse Integer ────────────────────────────────────────────────────────

(deftest parse-integer-basic
  "vm-parse-integer parses decimal string to integer."
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "42")
    (str-exec (cl-cc::make-vm-parse-integer :dst :R0 :src :R1) s)
    (assert-equal 42 (cl-cc/vm::vm-reg-get s :R0))))
