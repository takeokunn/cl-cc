(in-package :cl-cc/test)

;;; ─── FR-502/507: fill/replace/copy-seq vector support ───────────────────────

(deftest compile-fill-vector
  "fill works on a vector; with :start/:end it modifies only the specified range."
  (let ((r (run-string "(let ((v (make-array 3 :initial-contents '(1 2 3))))
                           (fill v 0)
                           v)" :stdlib t)))
    (assert-true (vectorp r))
    (assert-= 0 (aref r 0))
    (assert-= 0 (aref r 1))
    (assert-= 0 (aref r 2)))
  (let ((r (run-string "(let ((v (make-array 5 :initial-contents '(0 1 2 3 4))))
                           (fill v 9 :start 1 :end 4)
                           v)" :stdlib t)))
    (assert-true (vectorp r))
    (assert-= 0 (aref r 0))
    (assert-= 9 (aref r 1))
    (assert-= 9 (aref r 2))
    (assert-= 9 (aref r 3))
    (assert-= 4 (aref r 4))))

(deftest compile-replace-and-copy-seq-vector
  "replace copies elements into dest; copy-seq returns a fresh copy."
  :timeout 180
  (let ((r (run-string "(let ((d (make-array 3 :initial-contents '(0 0 0)))
                              (s (make-array 3 :initial-contents '(1 2 3))))
                           (replace d s)
                           d)" :stdlib t)))
    (assert-true (vectorp r))
    (assert-= 1 (aref r 0))
    (assert-= 2 (aref r 1))
    (assert-= 3 (aref r 2)))
  (let ((r (run-string "(let ((v (make-array 3 :initial-contents '(1 2 3))))
                           (copy-seq v))" :stdlib t)))
    (assert-true (vectorp r))
    (assert-= 3 (length r))
    (assert-= 1 (aref r 0))))

;;; ─── FR-697: assoc/member with :test/:key keyword args ───────────────────────

(deftest-each compile-sequence-test-keyword
  "member and assoc accept :test/#'equal for string equality."
  :cases (("member-test"   '("b" "c") "(member \"b\" '(\"a\" \"b\" \"c\") :test #'equal)")
          ("assoc-test"    '("b" . 2) "(assoc \"b\" '((\"a\" . 1) (\"b\" . 2)) :test #'equal)"))
  (expected form)
  (assert-equal expected (run-string form :stdlib t)))

(deftest compile-member-assoc-key-keyword
  "member and assoc with :key extract or transform the correct field."
  (let ((r (run-string "(member 2 '((1 . a) (2 . b) (3 . c)) :key #'car)" :stdlib t)))
    (assert-true (consp r))
    (assert-= 2 (caar r)))
  (let ((r (run-string "(assoc 4 '((1 . a) (2 . b) (3 . c)) :key (lambda (x) (* x x)))" :stdlib t)))
    (assert-true (consp r))
    (assert-= 2 (car r))))

;;; ─── position/count/find-if with keyword args ────────────────────────────────

(deftest-each compile-sequence-position-count-keywords
  "position and count accept :test and :key keyword args."
  :cases (("position-test" 1 "(position \"b\" '(\"a\" \"b\" \"c\") :test #'equal)")
          ("position-key"  1 "(position 2 '((1 . a) (2 . b) (3 . c)) :key #'car)")
          ("count-test"    2 "(count \"a\" '(\"a\" \"b\" \"a\") :test #'equal)"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

(deftest compile-find-if-key-keyword
  "find-if with :key applies the key function."
  (let ((r (run-string "(find-if #'evenp '((1 . a) (2 . b) (3 . c)) :key #'car)" :stdlib t)))
    (assert-true (consp r))
    (assert-= 2 (car r))))

;;; ─── remove-duplicates and remove with :test keyword ────────────────────────

(deftest compile-remove-test-keywords
  "remove-duplicates and remove with :test #'equal handle string equality."
  :timeout 10
  (assert-= 2 (length (run-string "(remove-duplicates '(\"a\" \"b\" \"a\") :test #'equal)" :stdlib t)))
  (assert-equal '("b") (run-string "(remove \"a\" '(\"a\" \"b\" \"a\") :test #'equal)" :stdlib t)))

;;; ─── string-upcase/downcase with :start/:end ─────────────────────────────────

(deftest-each compile-string-case-bounds
  "string-upcase and string-downcase accept :start/:end keyword args."
  :cases (("upcase"   "hELlo" "(string-upcase \"hello\" :start 1 :end 3)")
          ("downcase" "HellO" "(string-downcase \"HELLO\" :start 1 :end 4)"))
  (expected form)
  (assert-string= expected (run-string form)))

;;; ─── FR-599: #n= / #n# label and reference reader macros ────────────────────

(deftest compile-hash-n-eq
  "#n= labels a data object and #n# references it; works with strings too."
  (let ((r (run-string "(list #0=(1 2 3) #0#)")))
    (assert-equal '(1 2 3) (first r))
    (assert-equal '(1 2 3) (second r)))
  (assert-string= "hello" (run-string "#0=\"hello\"")))

;;; ─── FR-641: union/intersection/set-difference with :test ────────────────────

(deftest-each compile-set-ops-test-keyword
  "union and intersection accept :test #'equal for string equality."
  :cases (("union"        3 "(length (union '(\"a\" \"b\") '(\"b\" \"c\") :test #'equal))")
          ("intersection" 2 "(length (intersection '(\"a\" \"b\" \"c\") '(\"b\" \"c\" \"d\") :test #'equal))")
          ("set-diff"     2 "(length (set-difference '(\"a\" \"b\" \"c\") '(\"b\") :test #'equal))"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

;;; ─── FR-688: delete/substitute with :test keyword ────────────────────────────

(deftest compile-delete-test-keyword
  "delete with :test #'equal removes matching strings."
  (let ((r (run-string "(delete \"a\" '(\"a\" \"b\" \"a\") :test #'equal)" :stdlib t)))
    (assert-equal '("b") r)))

(deftest compile-substitute-test-keyword
  "substitute with :test #'equal replaces matching strings."
  (let ((r (run-string "(substitute \"x\" \"a\" '(\"a\" \"b\" \"a\") :test #'equal)" :stdlib t)))
    (assert-= 3 (length r))
    (assert-string= "x" (first r))
    (assert-string= "x" (third r))))

;;; ─── compile-file-pathname host bridge ───────────────────────────────────────

(deftest compile-compile-file-pathname
  "compile-file-pathname returns a pathname with .fasl type."
  (let ((r (run-string "(compile-file-pathname \"/tmp/foo.lisp\")")))
    (assert-true (pathnamep r))))

;;; ─── FR-604: float 2-arg prototype form ──────────────────────────────────────

(deftest compile-float-2arg
  "float with prototype argument converts to float ignoring prototype."
  (assert-true (floatp (run-string "(float 3 1.0d0)")))
  (assert-= (float 3) (run-string "(float 3 1.0d0)")))

;;; ─── FR-396: declaim macro stub ─────────────────────────────────────────────

(deftest-each compile-declaim-forms
  "declaim at top level is silently ignored; inline-declared functions still work."
  :cases (("optimize-nil" nil "(declaim (optimize (speed 3))) nil")
          ("inline-works" 25  "(declaim (inline square)) (defun square (x) (* x x)) (square 5)"))
  (expected form)
  (assert-equal expected (run-string form :stdlib t)))

;;; ─── FR-598: stream typep ────────────────────────────────────────────────────

(deftest-each compile-typep-stream-truthy
  "typep returns truthy for various stream types."
  :cases (("standard-output-stream" "(typep *standard-output* 'stream)"                              nil)
          ("output-stream-type"     "(output-stream-p *standard-output*)"                              t)
          ("string-output-stream"   "(let ((s (make-string-output-stream))) (typep s 'string-stream))" t))
  (form stdlib-p)
  (assert-true (run-string form :stdlib stdlib-p)))

(deftest compile-typep-non-stream
  "typep returns 0 for a non-stream value."
  (assert-= 0 (run-string "(typep 42 'stream)")))

;;; ─── FR-603: (setf (values ...)) ────────────────────────────────────────────

(deftest compile-setf-values-floor
  "(setf (values a b) (floor 7 3)) destructures multiple values."
  (let ((r (run-string "(let (a b) (setf (values a b) (floor 7 3)) (list a b))" :stdlib t)))
    (assert-= 2 (first r))
    (assert-= 1 (second r))))

;;; FR-562: Unicode character names via lexer
(deftest-each compile-unicode-char-code
  "Lexer resolves Unicode character names and code-char supports full Unicode range."
  :cases (("greek-alpha" 945    "(char-code #\\Greek_Small_Letter_Alpha)")
          ("snowman"     9731   "(char-code #\\Snowman)")
          ("emoji"       128512 "(char-code (code-char 128512))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; FR-687: make-string :element-type with both keywords
(deftest-each compile-make-string-element-type
  "make-string accepts :element-type; with both keywords it fills correctly."
  :cases (("length"
           "(length (make-string 5 :element-type 'character))"
           (lambda (result)
             (assert-= 5 result)))
          ("fill"
           "(make-string 3 :initial-element #\\x :element-type 'character)"
           (lambda (result)
             (assert-string= "xxx" result))))
  (form verify)
  (funcall verify (run-string form)))

;;; (run-tests is defined in framework.lisp)
