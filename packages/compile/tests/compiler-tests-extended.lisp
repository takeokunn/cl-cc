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

(deftest-compile compile-prog-and-friends
  "prog/prog*/with-slots/nth-value macros work in compiled code."
  :cases (("prog-loop"        10  "(prog ((x 0)) loop (setq x (+ x 1)) (when (= x 10) (return x)) (go loop))")
          ("prog*-sequential"  3  "(prog* ((x 1) (y (+ x 2))) (return y))")
          ("with-slots"       30  "(progn (defclass point () ((x :initarg :x) (y :initarg :y)))
                                          (let ((p (make-instance (quote point) :x 10 :y 20)))
                                            (with-slots (x y) p (+ x y))))")
          ("nth-value"         2  "(nth-value 1 (floor 17 5))")
          ("prog-no-return"   nil "(prog ((x 1)) (setq x 2))"))
  :stdlib t)

;;; ANSI CL FR-400/FR-500 Tests (mismatch, make-string, float literals, string-not-equal)

(deftest-compile stdlib-mismatch-make-string-float
  "mismatch, make-string, and float properties return the expected equal-comparable values."
  :cases (("mismatch-index"     2      "(mismatch (list 1 2 3) (list 1 2 4))")
          ("mismatch-equal"     nil    "(mismatch (list 1 2 3) (list 1 2 3))")
          ("mismatch-prefix"    0      "(mismatch nil (list 1 2))")
          ("make-string-fill"   "xxxx" "(make-string 4 :initial-element #\\x)")
          ("make-string-len"    3      "(length (make-string 3))")
          ("float-literal"      4.0    "(+ 1.5 2.5)"))
  :stdlib t)

(deftest compile-find-package-builtin
  "find-package compiles through the registry and returns a non-NIL package descriptor."
  (assert-true (run-string "(find-package :cl-user)" :stdlib t)))

(deftest compile-find-symbol-builtin
  "find-symbol compiles through the registry and preserves symbol status as multiple values."
  (assert-equal '("CAR" :external)
                (run-string "(multiple-value-bind (sym status) (find-symbol \"CAR\" :cl) (list (symbol-name sym) status))" :stdlib t)))

(deftest-each compile-float-inspection-builtins
  "Unary float inspection builtins compile through the registry and return the host results."
  :cases (("float-precision" "(float-precision 1.0)" (float-precision 1.0))
          ("float-radix" "(float-radix 1.0)" (float-radix 1.0))
          ("float-sign" "(float-sign -2.5)" (float-sign -2.5))
          ("float-digits" "(float-digits 1.0)" (float-digits 1.0)))
  (form expected)
  (assert-equal expected (run-string form :stdlib t)))

(deftest-each compile-float-decode-builtins
  "Float decode builtins compile through the registry and preserve all multiple values."
  :cases (("decode-float"
           "(multiple-value-list (decode-float 1.0))"
           (multiple-value-list (decode-float 1.0)))
          ("integer-decode-float"
           "(multiple-value-list (integer-decode-float 1.0))"
           (multiple-value-list (integer-decode-float 1.0))))
  (form expected)
  (assert-equal expected (run-string form :stdlib t)))

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

(deftest-compile compile-list-tree-mutators
  "ldiff, get-properties, nunion, nsubst, and nstring-upcase return expected values."
  :cases (("ldiff"          '(1 2)           "(let* ((x '(1 2 3 4)) (tail (cddr x))) (ldiff x tail))")
          ("get-properties"  :b              "(get-properties '(:a 1 :b 2 :c 3) '(:b :c))")
          ("nunion"          '(1 2 3 4)      "(sort (nunion '(1 2 3) '(2 3 4)) #'<)")
          ("nsubst"          '(99 2 (99 3))  "(nsubst 99 1 '(1 2 (1 3)))")
          ("nstring-upcase"  "HELLO"         "(nstring-upcase \"hello\")"))
  )

(deftest-compile compile-array-predicates
  "array-element-type returns T; array-in-bounds-p checks index validity."
  :cases (("element-type"      t   "(array-element-type (make-array 3))")
          ("in-bounds-valid"   t   "(array-in-bounds-p (make-array 5) 3)")
          ("in-bounds-invalid" nil "(array-in-bounds-p (make-array 5) 7)"))
  )

(deftest-each compile-equalp
  "equalp compares lists and strings case-insensitively."
  :cases (("lists-equal"   "(equalp '(1 2) '(1 2))"      t)
          ("string-case"   "(equalp \"hello\" \"HELLO\")"  t)
          ("lists-unequal" "(equalp '(1 2) '(1 3))"       nil))
  (form expected-truthy)
  (assert-equal expected-truthy (not (null (run-string form)))))

(deftest compile-lisp-implementation-type
  "lisp-implementation-type returns cl-cc."
  (assert-equal "cl-cc" (run-string "(lisp-implementation-type)")))

(deftest-compile compile-last-butlast-count
  "last/butlast with count return the correct sublist."
  :cases (("last"    '(4 5)   "(last '(1 2 3 4 5) 2)")
          ("butlast" '(1 2 3) "(butlast '(1 2 3 4 5) 2)"))
  )

(deftest-compile compile-array-integer-results
  "make-array, setf-bit, and search return integer results."
  :cases (("initial-contents" 20 "(let ((a (make-array 3 :initial-contents '(10 20 30)))) (aref a 1))")
          ("initial-element"   7 "(let ((a (make-array 4 :initial-element 7))) (aref a 3))")
          ("setf-bit"          1 "(let ((bv (make-array 4))) (setf (bit bv 2) 1) (bit bv 2))")
          ("search-vector"     1 "(search '(2 3) '(1 2 3 4))"))
  )

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

;;; ─── FR-617/FR-608: stream read forms ────────────────────────────────────────

(deftest-compile compile-stream-read-forms
  "read-from-string returns (val pos); with-input-from-string :start skips prefix."
  :cases (("read-from-string-pos"
           '(42 3)
           "(multiple-value-bind (val pos)
              (read-from-string \"42 rest\")
              (list val pos))")
          ("with-input-from-string-start"
           'cl-cc::world
           "(with-input-from-string (s \"hello world\" :start 6)
              (read s))"))
  )

;;; ─── FR-637: string comparison with keyword bounds ───────────────────────────

(deftest-compile compile-string-comparison-bounds
  "String comparison functions accept :start/:end bounds."
  :cases (("equal-substring" t   "(string= \"hello world\" \"world\" :start1 6)")
          ("less-equal-nil"  nil "(string< \"ab\" \"abcde\" :start2 0 :end2 2)"))
  )

;;; ─── FR-397: compilation local scope forms ───────────────────────────────────

(deftest-compile compile-meta-forms
  "locally evaluates its body and returns the numeric result."
  :cases (("locally" 30 "(locally (+ 10 20))")))

;;; ─── FR-566: pathname host bridges ───────────────────────────────────────────

(deftest-compile compile-pathname-accessors
  "pathname-name and pathname-type extract the basename and extension."
  :cases (("name-string"    "foo"  "(pathname-name \"/tmp/foo.lisp\")")
          ("type-string"    "lisp" "(pathname-type \"/tmp/foo.lisp\")")
          ("name-hash-p"    "foo"  "(pathname-name #P\"/tmp/foo.lisp\")")
          ("type-hash-p"    "txt"  "(pathname-type #P\"/tmp/bar.txt\")"))
  )

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

(deftest-each compile-stream-read-eof-args
  "read-char and read-line with explicit eof args compile; result is nil-like or an eof marker."
  :cases (("read-char"  "(with-input-from-string (s \"\") (read-char s nil :end-of-stream))")
          ("read-line"  "(with-input-from-string (s \"\") (read-line s nil nil))"))
  (form)
  (let ((r (run-string form)))
    (assert-true (or (null r) (equal r "") (eq r :eof) (eq r :end-of-stream)))))

(deftest compile-read-char-stream-arg
  "read-char with explicit stream reads a character."
  (let ((r (run-string
              "(with-input-from-string (s \"A\")
                 (read-char s))")))
    (assert-equal #\A r)))

(deftest compile-close-abort
  "close with :abort t compiles and returns t."
  (let ((r (run-string
              "(let ((s (make-string-output-stream)))
                 (close s :abort t))")))
    (assert-true r)))

;;; Extended stdlib/keyword integration tests are in compiler-tests-extended-stdlib.lisp.
