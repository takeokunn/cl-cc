(in-package :cl-cc/test)

(defun %fr-361-large-body-form ()
  (let ((expr 'x))
    (dotimes (_ 16 expr)
      (declare (ignore _))
      (setf expr `(+ ,expr 1)))))

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

(deftest compile-empty-let-and-flet
  "let/flet accept empty binding lists."
  (assert-= 42 (run-string "(let () 42)"))
  (assert-= 7 (run-string "(flet () 7)")))

(deftest compile-top-level-defvar-visible-to-following-defun
  "Top-level defvars stay on the direct path so later definitions are not skipped by CPS labels."
  (assert-= 43
            (run-string
             "(progn
                (defvar *defvar-cps-visibility-probe-a* 41)
                (defvar *defvar-cps-visibility-probe-b* 2)
                (defun defvar-cps-visibility-probe ()
                  (+ *defvar-cps-visibility-probe-a* *defvar-cps-visibility-probe-b*))
                (defvar-cps-visibility-probe))")))

(deftest compile-type-of-float-and-function
  "type-of returns float and function type names for runtime values."
  (assert-eq 'single-float (run-string "(type-of 1.0)"))
  (assert-eq 'function (run-string "(type-of (lambda (x) x))")))

(deftest compile-error-format-control
  "error accepts string format control plus arguments."
  (assert-true
   (run-string
    "(handler-case
         (progn (error \"bad ~A\" 42) nil)
       (error (e)
         (not (null (search \"42\" (format nil \"~A\" e))))))")))

(deftest compile-warn-format-control
  "warn accepts string format control plus arguments and returns nil."
  (assert-= 42 (run-string "(progn (warn \"warn ~A\" 7) 42)")))

;;; ─── FR-599: #n= / #n# label and reference reader macros ────────────────────

(deftest compile-hash-n-eq
  "#n= labels a data object and #n# references it; works with strings too."
  (let ((r (run-string "(list #0=(1 2 3) #0#)")))
    (assert-equal '(1 2 3) (first r))
    (assert-equal '(1 2 3) (second r)))
  (assert-string= "hello" (run-string "#0=\"hello\"")))

;;; ─── FR-592: readtable API compatibility ────────────────────────────────────

(deftest compile-readtable-compatibility-api
  "Readtable API calls are available and preserve registered macro metadata."
  (assert-true (run-string "(readtablep *readtable*)" :stdlib t))
  (assert-eq :downcase
             (run-string "(let ((rt (copy-readtable)))
                             (setf (readtable-case rt) :downcase)
                             (readtable-case rt))"
                          :stdlib t))
  (assert-string= "abc"
                 (run-string "(progn
                                (setf (readtable-case *readtable*) :downcase)
                                (symbol-name (read-from-string \"ABC\")))"
                             :stdlib t))
  (assert-equal '(t t)
                (run-string "(let ((rt (copy-readtable)))
                               (set-macro-character #\! (lambda (stream char)
                                                          (declare (ignore stream char))
                                                          :ok)
                                                    t rt)
                               (set-syntax-from-char #\? #\! rt rt)
                               (multiple-value-bind (fn non-terminating-p)
                                   (get-macro-character #\? rt)
                                 (list (functionp fn) non-terminating-p)))"
                            :stdlib t))
  (assert-true
   (run-string "(let ((rt (copy-readtable)))
                  (make-dispatch-macro-character #\# nil rt)
                  (set-dispatch-macro-character #\# #\q
                                                (lambda (stream char arg)
                                                  (declare (ignore stream char arg))
                                                  :q)
                                                rt)
                  (functionp (get-dispatch-macro-character #\# #\q rt)))"
               :stdlib t)))

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

(deftest compile-compile-file-is-fbound
  "compile-file is available as a stdlib function."
  (assert-true (run-string "(fboundp 'compile-file)" :stdlib t)))

(deftest compile-safe-debug-bridges
  "disassemble is non-blocking and inspect uses describe while returning the object."
  (assert-null (run-string "(disassemble 42)"))
  (assert-= 42 (run-string "(inspect 42)")))

(deftest compile-foreign-funcall-strlen
  "foreign-funcall provides a minimal CFFI-compatible host-backed FFI path."
  (assert-= 4 (run-string "(foreign-funcall \"strlen\" :string \"abcd\" :int)" :stdlib t))
  (assert-= 5 (run-string "(cffi:foreign-funcall \"strlen\" :string \"abcde\" :int)" :stdlib t)))

(deftest compile-documentation-defun-docstring
  "documentation returns docstrings captured by the defun expander."
  (assert-string=
   "doc text"
   (run-string
    "(progn (defun documented-probe () \"doc text\" 42)
       (documentation 'documented-probe 'function))"
    :stdlib t))
  (assert-null (run-string "(documentation 'missing-doc-probe 'function)" :stdlib t)))

(deftest compile-subtypep-basic-two-values
  "subtypep is available at runtime and preserves ANSI two-value results."
  (assert-equal '(t t)
                (run-string "(multiple-value-list (subtypep 'integer 'number))" :stdlib t))
  (assert-equal '(nil t)
                (run-string "(multiple-value-list (subtypep 'string 'integer))" :stdlib t)))

(deftest compile-string-octets-bridges
  "string-to-octets / octets-to-string round-trip UTF-8 data and honor encoding keywords."
  (let ((octets (run-string "(string-to-octets \"hé\" :encoding :utf-8)")))
    (assert-true (vectorp octets))
    (assert-= 3 (length octets))
    (assert-= 104 (aref octets 0))
    (assert-= 195 (aref octets 1))
    (assert-= 169 (aref octets 2)))
  (let ((octets (run-string "(string-to-octets \"é\" :external-format :latin-1)")))
    (assert-= 1 (length octets))
    (assert-= 233 (aref octets 0)))
  (assert-string=
   "hé"
   (run-string "(octets-to-string (string-to-octets \"hé\" :external-format :utf-8) :encoding :utf-8)")))

;;; ─── FR-604: float 2-arg prototype form ──────────────────────────────────────

(deftest compile-float-2arg
  "float with prototype argument converts to float ignoring prototype."
  (assert-true (floatp (run-string "(float 3 1.0d0)")))
  (assert-= (float 3) (run-string "(float 3 1.0d0)")))

;;; ─── FR-605: bignum predicate helper ────────────────────────────────────────

(deftest compile-bignump
  "bignump recognizes integers outside the fixnum range via the VM numeric tower."
  (assert-true (run-string "(bignump (expt 2 100))" :stdlib t))
  (assert-false (run-string "(bignump 42)" :stdlib t)))

;;; ─── FR-361/363/396: declaim inline policy + optimize quality handling ─────

(deftest compile-declaim-optimize-form-records-global-policy
  "DECLAIM optimize still evaluates to NIL while recording global quality levels."
  (let ((cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
    (assert-equal nil
                  (run-string "(declaim (optimize speed (safety 0) (debug 3))) nil" :stdlib t))
    (assert-= 3 (gethash 'speed cl-cc/expand:*declaim-optimize-registry*))
    (assert-= 0 (gethash 'safety cl-cc/expand:*declaim-optimize-registry*))
    (assert-= 3 (gethash 'debug cl-cc/expand:*declaim-optimize-registry*))))

(deftest compile-declaim-safety-zero-suppresses-later-defun-type-assertion
  "A global `(declaim (optimize (safety 0)))` suppresses vm-typep in later defuns."
  (let ((cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((declaim (optimize (safety 0)))
                      (defun later-safe (x) (the integer x)))
                    :target :vm))
           (insts (cl-cc/compile:compilation-result-vm-instructions result)))
      (assert-true (find-if #'cl-cc:vm-closure-p insts))
      (assert-null (find-if (lambda (inst) (typep inst 'cl-cc/vm::vm-typep)) insts)))))

(deftest compile-declaim-safety-zero-suppresses-top-level-the-type-assertion
  "Global safety 0 is honored by the VM CPS top-level path for THE forms."
  (let ((cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((declaim (optimize (safety 0)))
                      (the integer 42))
                    :target :vm))
           (insts (cl-cc/compile:compilation-result-vm-instructions result)))
      (assert-null (find-if (lambda (inst) (typep inst 'cl-cc/vm::vm-typep)) insts)))))

(deftest-each compile-declaim-optimize-inline-policy-applies-globally-across-compilations
  "Global optimize qualities annotate later defun closures through the existing inline policy path."
  :cases (("speed-three" '(declaim (optimize (speed 3))) :inline)
          ("debug-three" '(declaim (optimize (debug 3))) :notinline)
          ("space-two" '(declaim (optimize (space 2))) :notinline))
  (declaim-form expected-policy)
  (let ((cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
    (assert-equal nil (our-macroexpand-1 declaim-form))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((defun mapped-inline-policy (x) (+ x 1)))
                    :target :vm
                    :pass-pipeline '(:inline)))
           (closure (find-if #'cl-cc:vm-closure-p
                             (cl-cc/compile:compilation-result-vm-instructions result))))
      (assert-true closure)
      (assert-eq expected-policy (cl-cc/vm:vm-closure-inline-policy closure)))))

(deftest compile-declaim-inline-applies-globally-across-compilations
  "A prior `(declaim (inline f))` annotates later defuns compiled in another unit."
  (let ((cl-cc/expand:*declaim-inline-registry* (make-hash-table :test #'eq)))
    (assert-equal nil (our-macroexpand-1 '(declaim (inline big))))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    (list `(defun big (x) ,(%fr-361-large-body-form)))
                    :target :vm
                    :pass-pipeline '(:inline)))
           (closure (find-if #'cl-cc:vm-closure-p
                             (cl-cc/compile:compilation-result-vm-instructions result))))
      (assert-true closure)
      (assert-eq :inline (cl-cc/vm:vm-closure-inline-policy closure)))))

(deftest compile-declaim-notinline-applies-globally-across-compilations
  "Explicit NOTINLINE still wins even when optimize speed 3 would prefer inlining."
  (let ((cl-cc/expand:*declaim-inline-registry* (make-hash-table :test #'eq))
        (cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
    (assert-equal nil (our-macroexpand-1 '(declaim (optimize (speed 3)))))
    (assert-equal nil (our-macroexpand-1 '(declaim (notinline inc))))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((defun inc (x) (+ x 1)))
                    :target :vm
                    :pass-pipeline '(:inline)))
           (closure (find-if #'cl-cc:vm-closure-p
                             (cl-cc/compile:compilation-result-vm-instructions result))))
      (assert-true closure)
      (assert-eq :notinline (cl-cc/vm:vm-closure-inline-policy closure)))))

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
