(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

;;; Nested Destructuring-Bind Tests

(deftest-each destructuring-bind-nested
  "DESTRUCTURING-BIND with nested and deeply nested patterns."
  :cases (("nested"      10 "(destructuring-bind (a (b c) d) (list 1 (list 2 3) 4) (+ a b c d))")
          ("deep-nested" 15 "(destructuring-bind (a (b (c d)) e) (list 1 (list 2 (list 3 4)) 5) (+ a b c d e))"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

;;; Variadic Append/Nconc Tests

(deftest-each append-variadic
  "append and nconc work with 0, 1, or 3 arguments."
  :cases (("three-args" '(1 2 3 4 5)   "(append (list 1 2) (list 3 4) (list 5))")
          ("zero-args"  nil             "(append)")
          ("one-arg"    '(1 2 3)        "(append (list 1 2 3))")
          ("nconc"      '(1 2 3 4 5 6)  "(nconc (list 1 2) (list 3 4) (list 5 6))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

(deftest self-host-stack-compiler
  "Mini stack-machine compiler: parse -> compile -> run through VM"
  (assert-= 17 (run-string *self-host-stack-compiler-program* :stdlib t)))

;;; Consp Fix / Type Predicates Tests

(deftest-each consp-and
  "consp returns CL booleans and composes correctly with and."
  :cases (("consp-list"
           "(consp (list 1 2))"
           (lambda (result)
             (assert-true result)))
          ("consp-int"
           "(consp 42)"
           (lambda (result)
             (assert-equal nil result)))
          ("and-consp"
           "(and (consp (list 1)) 42)"
           (lambda (result)
             (assert-equal 42 result))))
  (form verify)
  (funcall verify (run-string form)))

(deftest mini-compiler-self-host
  "Mini compiler can compile expression with pattern matching"
  (assert-true (equal '(:ADD (:CONST 1) (:CONST 2))
             (run-string "(defun my-compile (expr) (cond ((integerp expr) (list :const expr)) ((and (consp expr) (eq (car expr) (quote +))) (list :add (my-compile (second expr)) (my-compile (third expr)))) (t (list :unknown expr)))) (my-compile (quote (+ 1 2)))" :stdlib t))))

;;; Funcall/Apply with Quoted Symbols Tests

(deftest-each funcall-apply-quoted
  "funcall and apply work with quoted symbols, lambdas, and #' references."
  :cases (("funcall-builtin"  7 "(funcall (quote +) 3 4)")
          ("funcall-user"     7 "(defun my-add2 (a b) (+ a b)) (funcall (quote my-add2) 3 4)")
          ("apply-builtin"    6 "(apply (quote +) (list 1 2 3))")
          ("apply-user"       7 "(defun my-add3 (a b) (+ a b)) (apply (quote my-add3) (list 3 4))")
          ("apply-lambda"     6 "(apply (lambda (a b c) (+ a b c)) (list 1 2 3))")
          ("funcall-hash-ref" 7 "(funcall #'+ 3 4)"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

;;; Maphash Tests

(deftest maphash-collect-values
  "MAPHASH iterates over hash table entries with closure mutation"
  (let ((result (run-string "(let ((result nil))
  (let ((ht (make-hash-table)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (maphash (lambda (k v) (setq result (cons v result))) ht)
    result))")))
    (assert-true (listp result))
    (assert-= 2 (length result))
    (assert-true (null (set-difference result (list 1 2))))))

(deftest maphash-behavior
  "maphash iterates and always returns nil; closure mutation captures counts."
  (assert-true (null (run-string "(let ((ht (make-hash-table)))
  (setf (gethash :x ht) 10)
  (maphash (lambda (k v) v) ht))")))
  (assert-true (null (run-string "(let ((ht (make-hash-table)))
  (maphash (lambda (k v) k) ht))")))
  (assert-= 3 (run-string "(let ((count 0))
  (let ((ht (make-hash-table)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (setf (gethash :c ht) 3)
    (maphash (lambda (k v) (setq count (+ count 1))) ht)
    count))")))

;;; Capture-by-Reference Tests

(deftest-each capture-by-ref
  "Closures capture variables by reference; mutations are visible to all accessing closures."
  :cases (("counter" 3
           "(let ((count 0))
  (let ((inc (lambda () (setq count (+ count 1)) count)))
    (funcall inc) (funcall inc) (funcall inc)))")
          ("shared-state" 42
           "(let ((x 0))
  (defun get-x4 () x)
  (defun set-x4 (v) (setq x v))
  (set-x4 42)
  (get-x4))")
          ("accumulator" 10
           "(let ((sum 0))
  (let ((add (lambda (n) (setq sum (+ sum n)) sum)))
    (funcall add 1) (funcall add 2) (funcall add 3) (funcall add 4)))"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

;;; File I/O Tests

(deftest file-io
  "File I/O: write-char/read-char, with-open-file, read-from-string, and read."
  (let* ((tmp-dir (namestring (uiop:temporary-directory)))
         (wr-path  (concatenate 'string tmp-dir "cl-cc-test-wr.txt"))
         (wof-path (concatenate 'string tmp-dir "cl-cc-test-wof2.txt"))
         (rd-path  (concatenate 'string tmp-dir "cl-cc-test-rd.txt")))
    ;; write-char + read-char via open/close
    (let ((result (run-string
                   (format nil "(let ((h (open ~S :direction :output)))
  (write-char #\\H h)
  (write-char #\\i h)
  (close h)
  (let ((h2 (open ~S :direction :input)))
    (let ((c1 (read-char h2)))
      (let ((c2 (read-char h2)))
        (close h2)
        (list c1 c2)))))" wr-path wr-path))))
      (assert-true (equal result '(#\H #\i))))
    ;; with-open-file
    (let ((result (run-string
                   (format nil "(with-open-file (out ~S :direction :output)
  (write-char #\\X out))
(with-open-file (in ~S :direction :input)
  (read-char in))" wof-path wof-path))))
      (assert-eql result #\X))
    ;; read-from-string
    (let ((result (run-string "(read-from-string \"(+ 1 2)\")")))
      (assert-true (listp result))
      (assert-= 3 (length result)))
    ;; read from file
    (let ((result (run-string
                   (format nil "(with-open-file (out ~S :direction :output)
  (write-char #\\( out) (write-char #\\a out) (write-char #\\) out))
(with-open-file (in ~S :direction :input)
  (read in))" rd-path rd-path))))
      (assert-true (listp result)))))

;;; Setf Accessor Tests

(deftest-each setf-defstruct
  "setf on defstruct accessors modifies slots correctly."
  :cases (("accessor" 42
           "(defstruct my-cell (value 0))
(let ((c (make-my-cell)))
  (setf (my-cell-value c) 42)
  (my-cell-value c))")
          ("counter"  3
           "(defstruct my-counter2 (n 0))
(let ((c (make-my-counter2)))
  (setf (my-counter2-n c) (+ (my-counter2-n c) 1))
  (setf (my-counter2-n c) (+ (my-counter2-n c) 1))
  (setf (my-counter2-n c) (+ (my-counter2-n c) 1))
  (my-counter2-n c))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Self-Hosting Pattern Tests

(deftest self-host-compiler-context
  "Self-hosting: compile a mini compiler context with defstruct"
  (assert-equal '(3 3 0 1 2) (run-string "
(defstruct compiler-ctx2 (counter 0) (instructions nil))
(defun make-reg2 (ctx) (let ((n (compiler-ctx2-counter ctx))) (setf (compiler-ctx2-counter ctx) (+ n 1)) n))
(defun emit-inst2 (ctx inst) (setf (compiler-ctx2-instructions ctx) (cons inst (compiler-ctx2-instructions ctx))))
(let ((ctx (make-compiler-ctx2))) (let ((r1 (make-reg2 ctx)) (r2 (make-reg2 ctx)) (r3 (make-reg2 ctx))) (emit-inst2 ctx (list :const r1 42)) (emit-inst2 ctx (list :const r2 10)) (emit-inst2 ctx (list :add r3 r1 r2)) (list (compiler-ctx2-counter ctx) (length (compiler-ctx2-instructions ctx)) r1 r2 r3)))" :stdlib t)))

(deftest self-host-clos-ast-eval
  "Self-hosting: CLOS-based AST evaluator"
  (assert-= 42 (run-string "
(defclass eval-int () ((value :initarg :value :reader eval-value)))
(defclass eval-binop () ((op :initarg :op :reader eval-op) (lhs :initarg :lhs :reader eval-lhs) (rhs :initarg :rhs :reader eval-rhs)))
(defgeneric eval-node (node))
(defmethod eval-node ((node eval-int)) (eval-value node))
(defmethod eval-node ((node eval-binop)) (let ((l (eval-node (eval-lhs node))) (r (eval-node (eval-rhs node)))) (if (eq (eval-op node) :add) (+ l r) (* l r))))
(let ((tree (make-instance 'eval-binop :op :add :lhs (make-instance 'eval-int :value 30) :rhs (make-instance 'eval-binop :op :mul :lhs (make-instance 'eval-int :value 4) :rhs (make-instance 'eval-int :value 3))))) (eval-node tree))")))

;;; Labels Mutual Recursion Tests

(deftest-each labels-mutual-recursion
  "Labels with mutually recursive functions behave correctly."
  :cases (("odd-3" nil
           "(labels ((even-p (n) (if (= n 0) t (odd-p (- n 1)))) (odd-p (n) (if (= n 0) nil (even-p (- n 1))))) (even-p 3))")
          ("three-fns" 6
           "(labels ((a (n) (if (= n 0) 0 (+ 1 (b (- n 1))))) (b (n) (if (= n 0) 0 (+ 1 (c (- n 1))))) (c (n) (if (= n 0) 0 (+ 1 (a (- n 1)))))) (a 6))"))
  (expected form)
  (assert-true (equal expected (run-string form))))

;;; Hash Table :test Parameter Tests

(deftest-each ht-test-parameter
  "Hash tables with various :test parameters work correctly."
  :cases (("equal-quote"       42 "(let ((ht (make-hash-table :test 'equal))) (setf (gethash \"key\" ht) 42) (gethash \"key\" ht))")
          ("equal-sharp-quote" 42 "(let ((ht (make-hash-table :test #'equal))) (setf (gethash \"key\" ht) 42) (gethash \"key\" ht))")
          ("eql-int-keys"      99 "(let ((ht (make-hash-table))) (setf (gethash 1 ht) 99) (gethash 1 ht))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; New Builtin Tests (type-of, make-list, alphanumericp, prin1-to-string)

(deftest-each builtin-type-of
  "type-of returns the correct type symbol for common types."
  :cases (("integer" 'fixnum "(type-of 42)")
          ("string"  'string  "(type-of \"hello\")")
          ("cons"    'cons    "(type-of '(1 2))"))
  (expected form)
  (assert-true (eq expected (run-string form))))

(deftest-each builtin-make-list
  "make-list creates a list of the specified length filled with nil."
  :cases (("three" '(nil nil nil) "(make-list 3)")
          ("zero"  nil            "(make-list 0)"))
  (expected form)
  (assert-true (equal expected (run-string form))))

(deftest-each builtin-alphanumericp
  "alphanumericp returns truthy for alphanumeric chars and 0 for punctuation."
  :cases (("alpha" "(alphanumericp #\\a)" t)
          ("digit" "(alphanumericp #\\5)" t)
          ("punct" "(alphanumericp #\\!)" nil))
  (form expected)
  (assert-equal expected (not (zerop (run-string form)))))

(deftest-each builtin-print-to-string
  "prin1-to-string and princ-to-string both return strings."
  :cases (("prin1" "(prin1-to-string 42)")
          ("princ" "(princ-to-string 42)"))
  (form)
  (assert-true (stringp (run-string form))))

;;; String/Symbol and later tests moved to compiler-tests-runtime-string-tests.lisp.
