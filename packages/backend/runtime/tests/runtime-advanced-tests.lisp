;;;; tests/unit/runtime/runtime-advanced-tests.lisp — Runtime Advanced Unit Tests
;;;;
;;;; Tests for runtime.lisp (bitwise), runtime-math-io.lisp (symbols/hash/conditions/misc),
;;;; and runtime-io.lisp (I/O wrappers).
;;;;
;;;; String/char ops → runtime-strings-chars-tests.lisp
;;;; CLOS/generic ops → runtime-clos-tests.lisp

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── Bitwise ───────────────────────────────────────────────────────────────

(deftest-each rt-bitwise-ops
  "rt-logand/logior/logxor/ash: binary bitwise and shift operations."
  :cases (("and"   #'cl-cc/runtime:rt-logand #b1110  #b1011 #b1010)
          ("or"    #'cl-cc/runtime:rt-logior #b1010  #b0101 #b1111)
          ("xor"   #'cl-cc/runtime:rt-logxor #b1010  #b0101 #b1111)
          ("ash-l" #'cl-cc/runtime:rt-ash    2       2      8)
          ("ash-r" #'cl-cc/runtime:rt-ash    8      -2      2))
  (fn a b expected)
  (assert-= expected (funcall fn a b)))

(deftest-each rt-bitwise-predicate-conventions
  "rt-logtest and rt-logbitp return 1/0 (bit overlap and bit position tests)."
  :cases (("logtest-overlap"  #'cl-cc/runtime:rt-logtest  #b1010 #b1000 1)
          ("logtest-disjoint" #'cl-cc/runtime:rt-logtest  #b1010 #b0101 0)
          ("logbitp-set"      #'cl-cc/runtime:rt-logbitp  0      1      1)
          ("logbitp-clear"    #'cl-cc/runtime:rt-logbitp  1      1      0))
  (pred-fn a b expected)
  (assert-= expected (funcall pred-fn a b)))

;;; ─── Symbol Operations ─────────────────────────────────────────────────────

(deftest rt-symbol-operations
  "rt-symbol-name, rt-make-symbol (uninterned), and rt-gensym (unique)."
  (assert-equal "FOO" (cl-cc/runtime:rt-symbol-name 'foo))
  (let ((s (cl-cc/runtime:rt-make-symbol "TEST")))
    (assert-equal "TEST" (symbol-name s))
    (assert-false (symbol-package s)))
  (assert-false (eq (cl-cc/runtime:rt-gensym) (cl-cc/runtime:rt-gensym))))

(deftest rt-symbol-plist-roundtrip
  "rt-put-prop / rt-get-prop / rt-remprop roundtrip."
  (let ((sym (cl-cc/runtime:rt-make-symbol "PLIST-TEST")))
    (cl-cc/runtime:rt-put-prop sym :color 'red)
    (assert-eq 'red (cl-cc/runtime:rt-get-prop sym :color))
    (cl-cc/runtime:rt-remprop sym :color)
    (assert-false (cl-cc/runtime:rt-get-prop sym :color))))

(deftest rt-intern-in-package
  "rt-intern interns through the runtime package registry and preserves identity per package/name."
  (let* ((pkg (find-package :cl-cc/test))
         (sym (cl-cc/runtime:rt-intern "RT-INTERN-TEST-SYM" pkg))
         (sym2 (cl-cc/runtime:rt-intern "RT-INTERN-TEST-SYM" pkg)))
    (assert-true (symbolp sym))
    (assert-equal "RT-INTERN-TEST-SYM" (symbol-name sym))
    (assert-eq sym sym2)))

;;; ─── Hash Table Operations ─────────────────────────────────────────────────

(deftest rt-hash-table-ops
  "rt-make-hash-table: set/get/rem/count/keys/values/clrhash all work correctly."
  (let ((ht (cl-cc/runtime:rt-make-hash-table)))
    (cl-cc/runtime:rt-sethash :a ht 1)
    (cl-cc/runtime:rt-sethash :b ht 2)
    (assert-= 1 (cl-cc/runtime:rt-gethash :a ht))
    (assert-= 2 (cl-cc/runtime:rt-hash-count ht))
    (cl-cc/runtime:rt-remhash :a ht)
    (assert-= 1 (cl-cc/runtime:rt-hash-count ht)))
  (let ((ht (cl-cc/runtime:rt-make-hash-table)))
    (cl-cc/runtime:rt-sethash :x ht 10)
    (cl-cc/runtime:rt-sethash :y ht 20)
    (assert-= 2 (length (cl-cc/runtime:rt-hash-keys ht)))
    (assert-= 2 (length (cl-cc/runtime:rt-hash-values ht)))
    (assert-true (member :x (cl-cc/runtime:rt-hash-keys ht)))
    (assert-true (member 10 (cl-cc/runtime:rt-hash-values ht)))
    (cl-cc/runtime:rt-clrhash ht)
    (assert-= 0 (cl-cc/runtime:rt-hash-count ht))))

(deftest rt-maphash-and-hash-test
  "rt-maphash iterates all entries; rt-hash-test returns the test function."
  (let ((ht (cl-cc/runtime:rt-make-hash-table :test #'equal)))
    (cl-cc/runtime:rt-sethash "a" ht 1)
    (cl-cc/runtime:rt-sethash "b" ht 2)
    (let ((collected nil))
      (cl-cc/runtime:rt-maphash (lambda (k v) (push (cons k v) collected)) ht)
      (assert-= 2 (length collected)))
    (assert-equal 'equal (cl-cc/runtime:rt-hash-test ht))))

;;; ─── Conditions ────────────────────────────────────────────────────────────

(deftest rt-signal-error-signals
  "rt-signal-error signals the given condition."
  (assert-signals error (cl-cc/runtime:rt-signal-error "test error")))

(deftest rt-signal-conditions
  "rt-signal signals a condition (not an error); rt-warn-fn issues a warning."
  (let ((got nil))
    (handler-bind ((simple-condition (lambda (c) (setf got c))))
      (cl-cc/runtime:rt-signal
        (make-condition 'simple-condition :format-control "test")))
    (assert-true got))
  (assert-signals warning (cl-cc/runtime:rt-warn-fn "a warning")))

;;; ─── Misc ──────────────────────────────────────────────────────────────────

(deftest rt-fboundp-uses-runtime-function-registry
  "rt-fboundp reflects the explicit runtime function registry rather than scanning the host namespace on demand." 
  (let ((sym (gensym "RT-FBOUNDP-TEST-")))
    (setf (gethash sym cl-cc/runtime::*rt-function-registry*) t)
    (assert-= 1 (cl-cc/runtime:rt-fboundp sym))
    (remhash sym cl-cc/runtime::*rt-function-registry*)
    (assert-= 0 (cl-cc/runtime:rt-fboundp sym))))

(deftest rt-bootstrap-function-registry-uses-explicit-seed-list
  "%rt-bootstrap-function-registry registers the explicit seed list without consulting host function cells." 
  (let ((cl-cc/runtime::*rt-function-registry* (make-hash-table :test #'eq)))
    (cl-cc/runtime::%rt-bootstrap-function-registry)
    (dolist (sym cl-cc/runtime::*rt-bootstrap-function-symbols*)
      (assert-true (gethash sym cl-cc/runtime::*rt-function-registry*)))))

(deftest rt-package-registry-is-seeded-conservatively
  "The runtime package registry is seeded from an explicit package list, not the full host universe." 
  (assert-true (find :cl-cc/runtime cl-cc/runtime::*rt-bootstrap-package-names*))
  (assert-true (find :cl cl-cc/runtime::*rt-bootstrap-package-names*))
  (assert-true (gethash "CL-CC/RUNTIME" cl-cc/runtime::*rt-package-registry*)))

(deftest rt-boundp-and-makunbound
  "rt-boundp detects runtime-registry bindings; rt-makunbound removes them."
  (let ((sym (gensym "RT-BOUND-TEST-")))
    (assert-= 0 (cl-cc/runtime:rt-boundp sym))
    (cl-cc/runtime:rt-set-symbol-value sym 42)
    (assert-= 1 (cl-cc/runtime:rt-boundp sym))
    (cl-cc/runtime:rt-makunbound sym)
    (assert-= 0 (cl-cc/runtime:rt-boundp sym))))

(deftest rt-coerce-works
  "rt-coerce delegates to CL coerce."
  (assert-equal '(1 2 3) (cl-cc/runtime:rt-coerce #(1 2 3) 'list)))

(deftest rt-read-write-to-string
  "rt-read-from-string and rt-write-to-string roundtrip."
  (assert-= 42 (cl-cc/runtime:rt-read-from-string "42"))
  (assert-equal "(1 2 3)" (cl-cc/runtime:rt-write-to-string '(1 2 3))))

(deftest rt-random-and-time
  "rt-random returns integer in range; rt-get-universal-time returns an integer."
  (let ((r (cl-cc/runtime:rt-random 100)))
    (assert-true (integerp r))
    (assert-true (and (>= r 0) (< r 100))))
  (assert-true (integerp (cl-cc/runtime:rt-get-universal-time))))

;;; ─── I/O Wrappers ──────────────────────────────────────────────────────────

(deftest rt-string-stream-creation-and-io
  "String stream creation: input-stream read-char, output-stream write+get, output-stream roundtrip."
  (let ((s (cl-cc/runtime:rt-make-string-stream "hello")))
    (assert-equal #\h (cl-cc/runtime:rt-read-char s)))
  (let ((s (cl-cc/runtime:rt-make-string-stream "" :direction :output)))
    (cl-cc/runtime:rt-write-string "world" s)
    (assert-equal "world" (cl-cc/runtime:rt-get-string-from-stream s)))
  (let ((s (cl-cc/runtime:rt-make-string-output-stream)))
    (cl-cc/runtime:rt-stream-write-string s "test")
    (assert-equal "test" (cl-cc/runtime:rt-get-output-stream-string s))))

(deftest-each rt-stream-predicates
  "rt-input-stream-p / rt-output-stream-p / rt-open-stream-p return 1/0."
  :cases (("input-true"   #'cl-cc/runtime:rt-input-stream-p  :input
           (lambda (pred-fn stream) (assert-= 1 (funcall pred-fn stream))))
          ("input-false"  #'cl-cc/runtime:rt-input-stream-p  :output
           (lambda (pred-fn stream) (assert-= 0 (funcall pred-fn stream))))
          ("output-true"  #'cl-cc/runtime:rt-output-stream-p :output
           (lambda (pred-fn stream) (assert-= 1 (funcall pred-fn stream))))
          ("output-false" #'cl-cc/runtime:rt-output-stream-p :input
           (lambda (pred-fn stream) (assert-= 0 (funcall pred-fn stream))))
          ("open-true"    #'cl-cc/runtime:rt-open-stream-p   :input
           (lambda (pred-fn stream) (assert-= 1 (funcall pred-fn stream)))))
  (pred-fn direction verify)
  (let ((stream (ecase direction
                  (:input  (make-string-input-stream "x"))
                  (:output (make-string-output-stream)))))
    (funcall verify pred-fn stream)))

(deftest rt-read-write-char-roundtrip
  "rt-write-char / rt-read-char via string streams."
  (let ((out (make-string-output-stream)))
    (cl-cc/runtime:rt-write-char #\Z out)
    (let ((in (make-string-input-stream (get-output-stream-string out))))
      (assert-equal #\Z (cl-cc/runtime:rt-read-char in)))))
