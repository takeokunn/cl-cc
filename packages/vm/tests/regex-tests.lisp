;;;; packages/vm/tests/regex-tests.lisp — FR-672 regex engine tests

(in-package :cl-cc/test)

(defsuite regex-suite
  :description "Pure Common Lisp regex engine unit tests"
  :parent cl-cc-unit-suite)

(in-suite regex-suite)

(deftest regex-scan-literals-and-quantifiers
  "regex:scan handles literals, dot, and *, +, ?, {n,m} quantifiers."
  (let ((match (regex:scan "ab+c.{2,3}" "xx abbbcXYZ yy")))
    (assert-true match)
    (assert-equal 3 (regex:match-start match))
    (assert-equal "abbbcXYZ" (regex:match-string match)))
  (assert-equal "color" (regex:match-string (regex:scan "colou?r" "color")))
  (assert-equal "colour" (regex:match-string (regex:scan "colou?r" "colour"))))

(deftest regex-scan-classes-escapes-and-anchors
  "regex:scan supports character classes, negated classes, escapes, ^, and $."
  (assert-equal "abc123" (regex:match-string (regex:scan "^[a-z]+\\d+$" "abc123")))
  (assert-equal "foo_bar" (regex:match-string (regex:scan "\\w+" "!foo_bar!")))
  (assert-equal (concatenate 'string " " (string #\Tab) " ")
                (regex:match-string
                 (regex:scan "\\s+"
                             (concatenate 'string "x " (string #\Tab) " y"))))
  (assert-equal "xy" (regex:match-string (regex:scan "[^0-9]+" "123xy9"))))

(deftest regex-all-matches
  "regex:all-matches returns every non-overlapping match."
  (assert-equal '("a1" "b22" "c333")
                (mapcar #'regex:match-string
                        (regex:all-matches "[a-z]\\d+" "a1 b22 c333"))))

(deftest regex-capture-groups
  "Capture groups are stored in the groups vector and readable by index."
  (let ((match (regex:scan "([a-z]+)-(\\d+)" "id abc-123 done")))
    (assert-true match)
    (assert-equal "abc-123" (regex:match-group match 0))
    (assert-equal "abc" (regex:match-group match 1))
    (assert-equal "123" (regex:match-group match 2))
    (assert-equal 3 (length (regex:match-groups match)))))

(deftest regex-replace-first-and-all
  "regex-replace and regex-replace-all support literal and captured replacements."
  (assert-equal "x <abc> y def"
                (regex:regex-replace "([a-z]+)" "<\\1>" "x abc y def" :start 2))
  (assert-equal "# # #"
                (regex:regex-replace-all "\\d+" "#" "1 22 333")))

(deftest regex-compiler-builds-nfa-and-dfa
  "Compiled regex objects expose parser, NFA, and DFA artifacts."
  (let ((compiled (regex:compile "a+b")))
    (assert-equal "a+b" (regex:compiled-regex-pattern compiled))
    (assert-true (regex:compiled-regex-ast compiled))
    (assert-true (regex:compiled-regex-nfa compiled))
    (assert-true (regex:compiled-regex-dfa compiled))))

(deftest regex-unicode-decimal-and-word-classes
  "FR-674: \\d and \\w use Unicode Decimal_Number and word categories."
  (let ((arabic-three (string (code-char #x0663)))
        (connector (string (code-char #x203F))))
    (assert-equal arabic-three
                  (regex:match-string (regex:scan "\\d+" (concatenate 'string "x" arabic-three "y"))))
    (assert-equal (concatenate 'string "λ" connector "9")
                  (regex:match-string
                   (regex:scan "\\w+" (concatenate 'string "!λ" connector "9!"))))))

(deftest regex-unicode-property-escapes
  "FR-674: \\p{Category} supports general categories and aliases."
  (assert-equal "λ" (regex:match-string (regex:scan "\\p{Letter}+" "1λ!")))
  (assert-equal "_" (regex:match-string (regex:scan "\\p{Connector_Punctuation}" "a_b")))
  (assert-equal "3" (regex:match-string (regex:scan "\\p{Nd}+" "x3y"))))

(deftest regex-unicode-case-insensitive
  "FR-674: (?i) enables Unicode-aware simple case-insensitive matching."
  (assert-equal "É" (regex:match-string (regex:scan "(?i)é" "xxÉyy")))
  (assert-equal "λ" (regex:match-string (regex:scan "(?i)Λ" "αλω"))))
