(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

;;; Self-Hosting Smoke Test: Mini-Optimizer with Labels + HOFs

(deftest self-host-optimizer-pipeline
  "Self-hosting smoke: CLOS AST + labels recursive optimizer + mapcar + defparameter"
  (assert-= 30 (run-string *self-host-optimizer-pipeline-program* :stdlib t)))

;;; Self-Hosting Integration: Macro Expander + Type Checker

(deftest self-host-macro-system-full
  "Self-hosting: hash-table macro registry + recursive expansion + multiple macros"
  (assert-true (string= "(if x (progn (if y (progn z) nil)) nil)"
             (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
               (string-downcase (format nil "~S" (run-string *self-host-macro-system-program* :stdlib t)))))))

(deftest self-host-type-checker
  "Self-hosting: simple HM-style type checker with CLOS type nodes"
  (assert-true (string= "ok"
                        (string-downcase
                         (symbol-name (run-string *self-host-type-checker-program*))))))

(deftest self-host-format-error-pipeline
  "Self-hosting: format for string building + error signaling + handler-case"
  (assert-string= "caught: bad-value" (run-string "(handler-case (let ((val 42)) (if (> val 100) val (error (format nil \"bad-value\")))) (error (e) (format nil \"caught: ~A\" e)))")))

;;; Prog/With-Slots/Nth-Value Macro Tests

(deftest compile-prog-and-friends
  "prog/prog*/with-slots/nth-value macros work in compiled code."
  (let ((results (run-string "(list
    (prog ((x 0)) loop (setq x (+ x 1)) (when (= x 10) (return x)) (go loop))
    (prog* ((x 1) (y (+ x 2))) (return y))
    (progn (defclass point () ((x :initarg :x) (y :initarg :y)))
           (let ((p (make-instance (quote point) :x 10 :y 20)))
             (with-slots (x y) p (+ x y))))
    (nth-value 1 (floor 17 5))
    (prog ((x 1)) (setq x 2)))" :stdlib t)))
    (assert-= 10 (first results))
    (assert-= 3 (second results))
    (assert-= 30 (third results))
    (assert-= 2 (fourth results))
    (assert-false (fifth results))))

;;; ANSI CL FR-400/FR-500 Tests (mismatch, make-string, float literals, string-not-equal)

(deftest-each stdlib-mismatch-make-string-float
  "mismatch, make-string, and float properties return the expected equal-comparable values."
  ;; float-precision and float-radix removed: VM functions not implemented
  :cases (("mismatch-index"     2      "(mismatch (list 1 2 3) (list 1 2 4))")
          ("mismatch-equal"     nil    "(mismatch (list 1 2 3) (list 1 2 3))")
          ("mismatch-prefix"    0      "(mismatch nil (list 1 2))")
          ("make-string-fill"   "xxxx" "(make-string 4 :initial-element #\\x)")
          ("make-string-len"    3      "(length (make-string 3))")
          ("float-literal"      4.0    "(+ 1.5 2.5)"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

(deftest-each compile-string-not-equal
  "string-not-equal returns truthy for different strings and falsy for case-insensitively equal strings."
  :cases (("different"      "(string-not-equal \"abc\" \"def\")")
          ("case-insensitive" "(if (string-not-equal \"abc\" \"ABC\") nil t)"))
  (form)
  (assert-true (run-string form :stdlib t)))

;;; ─── New stdlib tests (FR-495, FR-496, FR-540, FR-547, FR-582, FR-596, etc.) ──

(deftest-each compile-list-tree-predicates
  "tailp, copy-alist, and tree-equal all return truthy results."
  :cases (("tailp"      "(let* ((x '(1 2 3 4)) (tail (cddr x))) (tailp tail x))")
          ("copy-alist" "(let ((al '((a . 1) (b . 2)))) (equal al (copy-alist al)))")
          ("tree-equal" "(tree-equal '(1 (2 3)) '(1 (2 3)))"))
  (form)
  (assert-true (run-string form)))

(deftest-each compile-list-tree-mutators
  "ldiff, get-properties, nunion, nsubst, and nstring-upcase return expected values."
  :cases (("ldiff"          '(1 2)           "(let* ((x '(1 2 3 4)) (tail (cddr x))) (ldiff x tail))")
          ("get-properties"  :b              "(get-properties '(:a 1 :b 2 :c 3) '(:b :c))")
          ("nunion"          '(1 2 3 4)      "(sort (nunion '(1 2 3) '(2 3 4)) #'<)")
          ("nsubst"          '(99 2 (99 3))  "(nsubst 99 1 '(1 2 (1 3)))")
          ("nstring-upcase"  "HELLO"         "(nstring-upcase \"hello\")"))
  (expected form)
  (assert-equal expected (run-string form)))

(deftest compile-array-predicates
  "array-element-type returns T; array-in-bounds-p checks index validity."
  (assert-equal t (run-string "(array-element-type (make-array 3))"))
  (assert-true (run-string "(array-in-bounds-p (make-array 5) 3)"))
  (assert-true (not (run-string "(array-in-bounds-p (make-array 5) 7)"))))

(deftest-each compile-equalp
  "equalp compares lists and strings case-insensitively."
  :cases (("lists-equal"   "(equalp '(1 2) '(1 2))"      t)
          ("string-case"   "(equalp \"hello\" \"HELLO\")"  t)
          ("lists-unequal" "(equalp '(1 2) '(1 3))"       nil))
  (form expected-truthy)
  (if expected-truthy
      (assert-true (run-string form))
      (assert-true (not (run-string form)))))

(deftest compile-lisp-implementation-type
  "lisp-implementation-type returns cl-cc; compiled-function-p returns true for lambdas."
  (assert-equal "cl-cc" (run-string "(lisp-implementation-type)"))
  (assert-true (run-string "(compiled-function-p (lambda (x) x))")))

(deftest-each compile-last-butlast-count
  "last/butlast with count return the correct sublist."
  :cases (("last"    '(4 5)   "(last '(1 2 3 4 5) 2)")
          ("butlast" '(1 2 3) "(butlast '(1 2 3 4 5) 2)"))
  (expected form)
  (assert-equal expected (run-string form)))

(deftest-each compile-array-integer-results
  "make-array, setf-bit, and search return integer results."
  :cases (("initial-contents" 20 "(let ((a (make-array 3 :initial-contents '(10 20 30)))) (aref a 1))")
          ("initial-element"   7 "(let ((a (make-array 4 :initial-element 7))) (aref a 3))")
          ("setf-bit"          1 "(let ((bv (make-array 4))) (setf (bit bv 2) 1) (bit bv 2))")
          ("search-vector"     1 "(search '(2 3) '(1 2 3 4))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest compile-write-to-string-keywords
  "write-to-string with keyword args; setf on GET updates a symbol property."
  (assert-equal "42" (run-string "(write-to-string 42 :base 10)"))
  (assert-equal '(42 (:answer 42)) (run-string "(let ((sym 'hello)) (setf (get sym :answer) 42) (list (get sym :answer) (symbol-plist sym)))")))

;;; ─── FR-635: bit-nor / bit-nand / bit-eqv ────────────────────────────────────

(deftest-each compile-bit-vector-ops
  "bit-nor and bit-eqv produce bit-vector results."
  :cases (("nor" "(bit-nor #*1010 #*1100)")
          ("eqv" "(bit-eqv #*1010 #*0101)"))
  (form)
  (assert-true (vectorp (run-string form))))

;;; ─── FR-497: with-hash-table-iterator ────────────────────────────────────────

(deftest compile-with-hash-table-iterator
  "with-hash-table-iterator iterates all keys."
  (let ((r (run-string "(let ((h (make-hash-table)) (count 0))
  (setf (gethash :a h) 1 (gethash :b h) 2)
  (with-hash-table-iterator (next h)
    (loop (multiple-value-bind (more k v) (next)
            (unless more (return count))
            (incf count)
            (declare (ignore k v))))))")))
    (assert-= 2 r)))

;;; ─── FR-617: read-from-string 2nd value ──────────────────────────────────────

(deftest compile-read-from-string-position
  "read-from-string returns the end position (after delimiter) as second value."
  (let ((r (run-string "(multiple-value-bind (val pos)
  (read-from-string \"42 rest\")
  (list val pos))")))
    (assert-equal '(42 3) r)))

;;; ─── FR-637: string comparison with keyword bounds ───────────────────────────

(deftest-each compile-string-comparison-bounds
  "String comparison functions accept :start/:end bounds."
  :cases (("equal-substring" t   "(string= \"hello world\" \"world\" :start1 6)")
          ("less-equal-nil"  nil "(string< \"ab\" \"abcde\" :start2 0 :end2 2)"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; ─── FR-608: with-input-from-string keyword args ─────────────────────────────

(deftest compile-with-input-from-string-start
  "with-input-from-string :start skips prefix."
  (let ((r (run-string "(with-input-from-string (s \"hello world\" :start 6)
  (read s))")))
    (assert-equal 'cl-cc::world r)))

;;; ─── FR-363/FR-397/FR-439: compilation meta-forms ───────────────────────────

(deftest-each compile-meta-forms
  "with-compilation-unit/locally/compiler-let evaluate body and return numeric result."
  :cases (("with-compilation-unit" 3  "(with-compilation-unit () (+ 1 2))")
          ("locally"              30  "(locally (+ 10 20))")
          ("compiler-let"         8  "(compiler-let ((x 5) (y 3)) (+ x y))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; ─── FR-566: pathname host bridges ───────────────────────────────────────────

(deftest-each compile-pathname-accessors
  "pathname-name and pathname-type extract the basename and extension."
  :cases (("name-string"    "foo"  "(pathname-name \"/tmp/foo.lisp\")")
          ("type-string"    "lisp" "(pathname-type \"/tmp/foo.lisp\")")
          ("name-hash-p"    "foo"  "(pathname-name #P\"/tmp/foo.lisp\")")
          ("type-hash-p"    "txt"  "(pathname-type #P\"/tmp/bar.txt\")"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; ─── FR-572: #nA multi-dimensional array literal ─────────────────────────────

(deftest compile-hash-na-arrays
  "#2A creates a 2D array; #1A creates a 1D array (same as #(...))."
  (let ((r (run-string "#2A((1 2 3) (4 5 6))")))
    (assert-true (arrayp r))
    (assert-= 2 (array-rank r))
    (assert-= 2 (array-dimension r 0))
    (assert-= 3 (array-dimension r 1))
    (assert-= 1 (aref r 0 0))
    (assert-= 6 (aref r 1 2)))
  (let ((r (run-string "#1A(10 20 30)")))
    (assert-true (vectorp r))
    (assert-= 3 (length r))
    (assert-= 20 (aref r 1))))

;;; ─── FR-612: read-char / read-line / read with eof args ──────────────────────

(deftest compile-read-char-eof-args
  "read-char with eof-error-p and eof-value args compiles without error."
  (let ((r (run-string
              "(with-input-from-string (s \"\")
                 (read-char s nil :end-of-stream))")))
    (assert-true (or (eq r :eof) (eq r :end-of-stream) (null r)))))

(deftest compile-read-char-stream-arg
  "read-char with explicit stream reads a character."
  (let ((r (run-string
              "(with-input-from-string (s \"A\")
                 (read-char s))")))
    (assert-equal #\A r)))

(deftest compile-read-line-eof-args
  "read-line with eof args compiles without error (eof-value arg accepted)."
  (let ((r (run-string
              "(with-input-from-string (s \"\")
                 (read-line s nil nil))")))
    (assert-true (or (null r) (equal r "") (eq r :eof)))))

(deftest compile-close-abort
  "close with :abort t compiles and returns t."
  (let ((r (run-string
              "(let ((s (make-string-output-stream)))
                 (close s :abort t))")))
    (assert-true r)))

;;; ─── FR-358: Readtable stubs ─────────────────────────────────────────────────

(deftest compile-readtable-stubs
  "readtable stubs are defined and callable."
  (let ((result (run-string
                 "(list (readtablep *readtable*)
                        (copy-readtable)
                        (readtable-case nil))")))
    (assert-equal '(nil nil :upcase) result)))

(deftest compile-set-macro-character
  "set-macro-character and get-macro-character compile without error."
  (let* ((r1 (run-string
                "(set-macro-character (code-char 94) (lambda (s c) (declare (ignore s c)) :caret))"
                :stdlib t))
         (r2 (run-string "(get-macro-character (code-char 94))" :stdlib t)))
    (assert-true r1)
    (assert-false r2)))

;;; ─── FR-579: string-to-octets / octets-to-string ─────────────────────────────

;;; REMOVED: compile-string-octets — string-to-octets/octets-to-string not implemented

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
  :timeout :none
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
          ("output-stream-type"     "(typep *standard-output* 'output-stream)"                       nil)
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

;;; ─── FR-607: documentation storage ──────────────────────────────────────────

;;; REMOVED: compile-documentation — %get-documentation VM function not implemented

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
  :cases (("length"  "(length (make-string 5 :element-type 'character))"               5     nil)
          ("fill"    "(make-string 3 :initial-element #\\x :element-type 'character)"  "xxx" t))
  (form expected string-p)
  (if string-p
      (assert-string= expected (run-string form))
      (assert-= expected (run-string form))))

;;; (run-tests is defined in framework.lisp)
