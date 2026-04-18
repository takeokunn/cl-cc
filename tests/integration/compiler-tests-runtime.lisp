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

;;; String/Symbol Builtin Compilation Tests

(deftest-each compile-string-comparison
  "String comparison operators return truthy for true condition and NIL for false."
  :cases (("string=-match"    t   "(string= \"hello\" \"hello\")")
          ("string=-mismatch" nil "(string= \"hello\" \"world\")")
          ("string<-true"     t   "(string< \"abc\" \"def\")")
          ("string<-false"    nil "(string< \"def\" \"abc\")")
          ("string/=-true"    t   "(string/= \"hello\" \"world\")")
          ("string/=-false"   nil "(string/= \"hello\" \"hello\")")
          ("string-equal-ci"  t   "(string-equal \"Hello\" \"hello\")")
          ("string-equal-no"  nil "(string-equal \"hello\" \"world\")"))
  (expected form)
  (assert-equal expected (not (null (run-string form)))))

(deftest-each compile-string-ops-and-type-predicates
  "String ops return numeric results and type predicates return CL booleans."
  :cases (("length-hello"  5 "(string-length \"hello\")")
          ("length-empty"  0 "(string-length \"\")")
          ("upcase-len"    5 "(string-length (string-upcase \"hello\"))")
          ("downcase-len"  3 "(string-length (string-downcase \"ABC\"))")
          ("concat-len"   10 "(string-length (concatenate 'string \"hello\" \"world\"))")
          ("symbolp-sym"   t "(symbolp 'foo)")
          ("symbolp-num"   nil "(symbolp 42)")
          ("numberp-num"   t "(numberp 42)")
          ("numberp-sym"   nil "(numberp 'foo)"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; Macro Expansion in Compiler Tests

(deftest-each compile-control-macros-valued
  "cond/when/unless/and/or macros return the expected values."
  :cases (("cond-match"    42  "(cond ((= 1 2) 10) ((= 1 1) 42) (t 0))")
          ("cond-default"   0  "(cond ((= 1 2) 10) ((= 2 3) 20) (t 0))")
          ("when-true"     42  "(when (= 1 1) 42)")
          ("unless-false"  99  "(unless (= 1 2) 99)")
          ("and-all"        3  "(and 1 2 3)")
          ("or-find"        5  "(or nil nil 5)")
          ("or-first"       1  "(or 1 2 3)"))
  (expected form)
  (assert-run= expected form))

(deftest-each compile-control-macros-nil
  "cond/when/unless/and/or return nil in the false cases."
  :cases (("when-false"   "(when (= 1 2) 42)")
          ("unless-true"  "(unless (= 1 1) 99)")
          ("and-short"    "(and 1 nil 3)")
          ("or-all-nil"   "(or nil nil nil)"))
  (form)
  (assert-run-false form))

(deftest-each compile-t-nil-constants
  "t and nil are recognized as constants; not inverts truthiness."
  :cases (("t-is-true"    t   "t")
          ("not-nil"      t   "(not nil)")
          ("nil-is-nil"   nil "nil")
          ("not-1"        nil "(not 1)")
          ("not-t"        nil "(not t)"))
  (expected form)
  (assert-run= expected form))

;;; List Operation Builtin Tests

(deftest-each compile-list-ops
  "cons/car/cdr, list/length, first/rest, eq/eql return the expected numeric values."
  :cases (("car"         1 "(car (cons 1 2))")
          ("cdr"         2 "(cdr (cons 1 2))")
          ("length-3"    3 "(length (list 1 2 3))")
          ("length-0"    0 "(length (list))")
          ("first"      10 "(first (list 10 20 30))")
          ("rest-len"    2 "(length (rest (list 10 20 30)))")
          ("eq-true"     1 "(eq 1 1)")
          ("eq-false"    0 "(eq 1 2)")
          ("eql-true"    1 "(eql 42 42)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-list-builtins
  "append/reverse builtins work on lists."
  :cases (("append-len" 4 "(length (append (list 1 2) (list 3 4)))")
          ("reverse-first" 3 "(first (reverse (list 1 2 3)))"))
  (expected form)
  (assert-run= expected form))

;;; Handler-Case Tests

(deftest-each compile-handler-case-return-value
  "handler-case returns the correct numeric value in various scenarios."
  :cases (("no-error"      42 "(handler-case 42 (error (e) 0))")
          ("catches-error" 99 "(handler-case (error \"boom\") (error (e) 99))")
          ("arithmetic"    10 "(handler-case (+ 3 7) (error (e) 0))")
          ("handler-body" 100 "(handler-case (error \"x\") (error (e) (* 10 10)))")
          ("nested"         1 "(handler-case (handler-case (error \"inner\") (error (e) 1)) (error (e) 2))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest compile-handler-case-error-variable
  "handler-case binds the error value to the variable"
  (assert-string= "boom" (run-string "(handler-case (error \"boom\") (error (e) e))")))

(deftest compile-error-builtin-without-handler
  "error without handler signals a CL error"
  (assert-signals error
    (run-string "(error \"unhandled\")")))

;;; Hash Table Operation Tests

(deftest-each compile-hash-table-numeric
  "Hash table operations return the expected numeric values; gethash returns nil when missing with no default."
  :cases (("hash-table-p"   1  "(let ((ht (make-hash-table))) (hash-table-p ht))")
          ("gethash-get"    42 "(let ((ht (make-hash-table))) (setf (gethash 'x ht) 42) (gethash 'x ht))")
          ("gethash-default" 99 "(let ((ht (make-hash-table))) (gethash 'missing ht 99))")
          ("count"           2 "(let ((ht (make-hash-table))) (setf (gethash 'a ht) 1) (setf (gethash 'b ht) 2) (hash-table-count ht))")
          ("remhash"         1 "(let ((ht (make-hash-table))) (setf (gethash 'a ht) 1) (setf (gethash 'b ht) 2) (remhash 'a ht) (hash-table-count ht))")
          ("hash-table-p-no" 0 "(hash-table-p 42)")
          ("setf-returns"  100 "(let ((ht (make-hash-table))) (setf (gethash 'k ht) 100))")
          ("overwrite"      20 "(let ((ht (make-hash-table))) (setf (gethash 'k ht) 10) (setf (gethash 'k ht) 20) (gethash 'k ht))"))
  (expected form)
  (assert-= expected (run-string form))
  (assert-true (null (run-string "(let ((ht (make-hash-table))) (gethash 'missing ht))"))))

;;; Defmacro Compilation Tests

(deftest-each compile-defmacro-numeric
  "defmacro defines macros that expand and evaluate to the correct numeric result."
  :cases (("basic"      42  "(defmacro my-const () 42) (my-const)")
          ("with-args"  10  "(defmacro my-dbl (x) (list '+ x x)) (my-dbl 5)")
          ("quasiquote" 15  "(defmacro my-add3 (a b c) `(+ ,a (+ ,b ,c))) (my-add3 3 5 7)")
          ("in-let"     100 "(defmacro my-square (x) `(* ,x ,x)) (let ((n 10)) (my-square n))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest compile-defmacro-returns-name
  "defmacro returns the macro name; symbol-name returns the name string."
  (assert-true (string= "my-mac" (string-downcase (symbol-name (run-string "(defmacro my-mac (x) x)")))))
  (assert-string= "FOO" (run-string "(symbol-name 'foo)")))

;;; Symbol Manipulation Tests

(deftest-each compile-symbol-creation
  "intern/gensym/make-symbol all create symbols (symbolp returns truthy)."
  :cases (("intern"      "(symbolp (intern \"TEST-SYM\"))")
          ("gensym"      "(symbolp (gensym))")
          ("make-symbol" "(symbolp (make-symbol \"TEMP\"))"))
  (form)
  (assert-true (not (null (run-string form)))))

;;; Typep and Destructuring Tests

(deftest-each compile-typep-destructuring-iteration
  "typep/destructuring-bind/iteration macros return the expected numeric result."
  :cases (("typep-integer"   1 "(typep 42 'integer)")
          ("typep-string"    1 "(typep \"hello\" 'string)")
          ("typep-symbol"    1 "(typep 'foo 'symbol)")
          ("typep-cons"      1 "(typep (cons 1 2) 'cons)")
          ("typep-null"      1 "(typep nil 'null)")
          ("typep-negative"  0 "(typep 42 'string)")
          ("db-basic"        6 "(destructuring-bind (a b c) (list 1 2 3) (+ a (+ b c)))")
          ("db-rest"         2 "(destructuring-bind (a &rest b) (list 1 2 3) (length b))")
          ("dolist"          6 "(let ((sum 0)) (dolist (x (list 1 2 3)) (setq sum (+ sum x))) sum)")
          ("dotimes"        10 "(let ((sum 0)) (dotimes (i 5) (setq sum (+ sum i))) sum)")
          ("do"             10 "(do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((= i 5) sum))")
          ("loop"           10 "(let ((sum 0) (i 0)) (loop (if (= i 5) (return sum)) (setq sum (+ sum i)) (setq i (+ i 1))))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Runtime Eval and Setf Variable Tests

(deftest-each compile-eval-and-setf-variable
  "eval and setf on plain variables return the expected numeric results."
  :cases (("eval-constant"    42 "(eval 42)")
          ("eval-quoted"       3 "(eval '(+ 1 2))")
          ("eval-nested"      21 "(eval '(* (+ 1 2) (+ 3 4)))")
          ("eval-let-form"    15 "(eval '(let ((x 10)) (+ x 5)))")
          ("eval-constructed" 30 "(let ((op '+) (a 10) (b 20)) (eval (list op a b)))")
          ("setf-plain"       10 "(let ((x 0)) (setf x 10) x)")
          ("setf-increment"    3 "(let ((counter 0)) (setf counter (+ counter 1)) (setf counter (+ counter 1)) (setf counter (+ counter 1)) counter)"))
  (expected form)
  (assert-= expected (run-string form)))

;;; FR-603: (setf (values ...)) assigns to multiple places

(deftest-each compile-setf-values
  "(setf (values ...)) assigns multiple values to individual places."
  :cases (("reads-a"    10 "(let ((a 0) (b 0)) (setf (values a b) (values 10 20)) a)")
          ("reads-b"    20 "(let ((a 0) (b 0)) (setf (values a b) (values 10 20)) b)")
          ("reads-both" 30 "(let ((x 0) (y 0)) (setf (values x y) (values 10 20)) (+ x y))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-obsolete-set
  "SET is available as a builtin and assigns through SYMBOL-VALUE without stdlib loading."
  :cases (("returns-value" 42 "(progn (defparameter fr586-set-var 0) (set 'fr586-set-var 42))")
          ("reads-binding" 42 "(progn (defparameter fr586-set-var 0) (set 'fr586-set-var 42) (symbol-value 'fr586-set-var))")
          ("updates-value" 99 "(progn (defparameter fr586-set-var 0) (set 'fr586-set-var 42) (set 'fr586-set-var 99) (symbol-value 'fr586-set-var))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Stdlib HOF Tests (with stdlib)

(deftest-each stdlib-hof-basic
  "mapcar, reduce, and remove-if with stdlib loaded return correct values."
  :cases (("mapcar"    '(2 4 6) "(mapcar (lambda (x) (* x 2)) '(1 2 3))")
          ("reduce"    15       "(reduce #'+ '(1 2 3 4 5) :initial-value 0)")
          ("remove-if" '(2 4)   "(remove-if #'oddp '(1 2 3 4 5))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

;;; Let Alias Fix and Prog1/Prog2 Tests

(deftest-each let-no-alias-and-prog1-prog2
  "LET bindings are independent copies; prog1 returns first value; prog2 returns second."
  :cases (("let-simple"      0  "(let ((x 0)) (let ((y x)) (setq x 10) y))")
          ("let-nested"      5  "(let ((a 5)) (let ((b a)) (let ((c b)) (setq a 99) c)))")
          ("prog1-basic"     42 "(prog1 42 (+ 1 2))")
          ("prog1-side-eff"   0 "(let ((x 0)) (prog1 x (setq x 10)))")
          ("prog2"           42 "(prog2 1 42 3)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-ignore-errors
  "ignore-errors returns the value on success and nil on error."
  :cases (("success" 3   "(ignore-errors (+ 1 2))")
          ("failure" nil "(ignore-errors (error \"boom\"))"))
  (expected form)
  (assert-true (equal expected (run-string form))))

;;; Unwind-Protect Integration Tests

(deftest unwind-protect-cleanup-visible
  "unwind-protect cleanup side effects visible in handler-case"
  (assert-eq t (run-string "
(let ((cleaned nil)) (handler-case (unwind-protect (error \"boom\") (setf cleaned t)) (error (e) cleaned)))")))

;;; Self-Hosting CLOS Compiler Test

(deftest self-host-clos-compiler
  "Self-hosting: CLOS-based AST compiler with defgeneric/defmethod dispatch"
  (assert-equal '(:R2 3) (run-string *self-host-clos-compiler-program* :stdlib t)))

;;; Hash Table Extended Builtins Tests

(deftest compile-hash-table-values
  "hash-table-values returns list of values"
  (let ((result (run-string "(let ((ht (make-hash-table)))
  (setf (gethash :a ht) 1)
  (setf (gethash :b ht) 2)
  (hash-table-values ht))")))
    (assert-= 2 (length result))
    (assert-true (null (set-difference result '(1 2))))))

(deftest-each compile-hash-table-test
  "hash-table-test returns the test function name the table was created with."
  :cases (("default-eql"    'eql   "(hash-table-test (make-hash-table))")
          ("explicit-equal" 'equal "(hash-table-test (make-hash-table :test 'equal))"))
  (expected form)
  (assert-true (eq expected (run-string form))))

(deftest compile-copy-hash-table
  "copy-hash-table creates independent copy"
  (assert-= 42 (run-string "
(let ((ht (make-hash-table))) (setf (gethash :a ht) 42) (let ((ht2 (copy-hash-table ht))) (setf (gethash :a ht) 99) (gethash :a ht2)))")))

;;; Car/Cdr Composition and List Accessor Tests

(deftest-each cxr-compositions
  "c*r composition functions access deeply nested list elements correctly."
  :cases (("cadddr" "d" "(string-downcase (symbol-name (cadddr '(a b c d e))))")
          ("caadr"  "x" "(string-downcase (symbol-name (caadr '(a (x y) c))))")
          ("caddar" 3   "(caddar '((1 2 3) b c))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

;;; Self-Hosting Integration Tests

(deftest-each self-host-patterns
  "Self-hosting patterns: eval dispatch, defstruct, registry, tree walk, closure counter, code gen."
  :cases (("eval-loop"       7  " (defun mini-eval (form env) (cond ((integerp form) form) ((symbolp form) (cdr (assoc form env))) ((and (consp form) (eq (car form) 'quote)) (cadr form)) ((and (consp form) (eq (car form) 'if)) (if (not (= 0 (mini-eval (cadr form) env))) (mini-eval (caddr form) env) (mini-eval (cadddr form) env))) ((and (consp form) (eq (car form) '+)) (+ (mini-eval (cadr form) env) (mini-eval (caddr form) env))) (t 0))) (mini-eval '(if 1 (+ 3 4) 0) nil) ")
          ("defstruct-pipe"  42 " (defstruct node type value children) (let ((n (make-node :type 'add :value nil :children (list (make-node :type 'lit :value 42 :children nil))))) (node-value (car (node-children n)))) ")
          ("ht-registry"     30 " (let ((registry (make-hash-table))) (setf (gethash 'add registry) (lambda (a b) (+ a b))) (setf (gethash 'mul registry) (lambda (a b) (* a b))) (let ((op (gethash 'add registry))) (funcall op 10 20))) ")
          ("tree-walk"       10 " (defun tree-sum (tree) (if (consp tree) (+ (tree-sum (car tree)) (tree-sum (cdr tree))) (if (integerp tree) tree 0))) (tree-sum '((1 . 2) . (3 . (4 . nil)))) ")
          ("closure-counter"  3 " (let ((counter 0)) (defun next-id () (setq counter (+ counter 1)) counter)) (next-id) (next-id) (next-id) ")
          ("macro-code-gen"  15 " (defun make-add-expr (a b) (list '+ a b)) (defun make-let-expr (var val body) (list 'let (list (list var val)) body)) (eval (make-let-expr 'x 10 (make-add-expr 'x 5))) "))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

;;; Non-Constant Default Parameter Tests

(deftest-each non-constant-default-params
  "&key and &optional parameters use non-constant default expressions correctly."
  :cases (("key-default"    '(1 2 3) "(progn (defun test-fn (&key (data (list 1 2 3))) data) (test-fn))")
          ("key-supplied"   '(4 5 6) "(progn (defun test-fn (&key (data (list 1 2 3))) data) (test-fn :data (list 4 5 6)))")
          ("optional-default" '(10 20) "(progn (defun test-fn (&optional (data (list 10 20))) data) (test-fn))"))
  (expected form)
  (assert-equal expected (run-string form)))

(deftest defstruct-non-constant-default
  "Test defstruct with non-constant slot default (hash table)."
  (assert-eq :bar (run-string "(progn (defstruct registry (entries (make-hash-table :test 'eq))) (let ((r (make-registry))) (setf (gethash 'foo (registry-entries r)) :bar) (gethash 'foo (registry-entries r))))" :stdlib t)))

;;; Multiple Dispatch Tests

(deftest-each multi-dispatch-numeric
  "Multiple dispatch: double/mixed specialization and CLOS second arg."
  :cases (("dog+bone"     1  "(progn (defclass animal () ()) (defclass dog (animal) ()) (defclass cat (animal) ()) (defclass food () ()) (defclass bone (food) ()) (defclass fish (food) ()) (defgeneric feed (a f)) (defmethod feed ((a dog) (f bone)) 1) (defmethod feed ((a cat) (f fish)) 2) (feed (make-instance 'dog) (make-instance 'bone)))")
           ("cat+fish"     2  "(progn (defclass animal () ()) (defclass dog (animal) ()) (defclass cat (animal) ()) (defclass food () ()) (defclass bone (food) ()) (defclass fish (food) ()) (defgeneric feed (a f)) (defmethod feed ((a dog) (f bone)) 1) (defmethod feed ((a cat) (f fish)) 2) (feed (make-instance 'cat) (make-instance 'fish)))")
           ("circle-mixed" 10 "(progn (defclass shape () ()) (defclass circle (shape) ()) (defclass rect (shape) ()) (defgeneric area (s ctx)) (defmethod area ((s circle) ctx) 10) (defmethod area ((s rect) ctx) 20) (area (make-instance 'circle) 99))")
           ("rect-mixed"   20 "(progn (defclass shape () ()) (defclass circle (shape) ()) (defclass rect (shape) ()) (defgeneric area (s ctx)) (defmethod area ((s circle) ctx) 10) (defmethod area ((s rect) ctx) 20) (area (make-instance 'rect) 99))")
           ("clos-2nd-arg" 42 "(progn (defclass ctx () ()) (defclass nd () ()) (defclass nd-int (nd) ((v :initarg :v :reader nd-v))) (defgeneric cmp (n c)) (defmethod cmp ((n nd-int) c) 42) (cmp (make-instance 'nd-int :v 1) (make-instance 'ctx)))"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

(deftest multi-dispatch-inheritance-fallback
  "Multiple dispatch: dispatch falls back via class inheritance"
  (assert-true (string= "base"
                        (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                          (string-downcase (format nil "~S" (run-string "(progn (defclass a () ()) (defclass b (a) ()) (defclass x () ()) (defclass y (x) ()) (defgeneric op (p q)) (defmethod op ((p a) (q x)) 'base) (op (make-instance 'b) (make-instance 'y)))" :stdlib t)))))))

(deftest-each multi-dispatch-type-equality
  "Multiple dispatch: double dispatch for type equality (self-hosting pattern)"
  :cases (("same-type" t   "(progn (defclass ty () ()) (defclass ty-int (ty) ()) (defclass ty-str (ty) ()) (defgeneric ty-eq (a b)) (defmethod ty-eq ((a ty-int) (b ty-int)) t) (defmethod ty-eq ((a ty-str) (b ty-str)) t) (defmethod ty-eq ((a ty) (b ty)) nil) (ty-eq (make-instance 'ty-int) (make-instance 'ty-int)))")
           ("diff-type" nil "(progn (defclass ty () ()) (defclass ty-int (ty) ()) (defclass ty-str (ty) ()) (defgeneric ty-eq (a b)) (defmethod ty-eq ((a ty-int) (b ty-int)) t) (defmethod ty-eq ((a ty-str) (b ty-str)) t) (defmethod ty-eq ((a ty) (b ty)) nil) (ty-eq (make-instance 'ty-int) (make-instance 'ty-str)))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

;;; CLOS Initform and Accessor Setf Tests

(deftest-each clos-initform
  "CLOS :initform provides default values and :accessor setf works for mutation."
  :cases (("integer"    0 "(progn (defclass counter () ((n :initform 0 :accessor counter-n))) (counter-n (make-instance 'counter)))")
          ("setf"      99 "(progn (defclass box () ((val :initarg :val :initform 0 :accessor box-val))) (let ((b (make-instance 'box))) (setf (box-val b) 99) (box-val b)))")
          ("increment"  3 "(progn (defclass counter () ((n :initform 0 :accessor counter-n))) (let ((c (make-instance 'counter))) (setf (counter-n c) (+ (counter-n c) 1)) (setf (counter-n c) (+ (counter-n c) 1)) (setf (counter-n c) (+ (counter-n c) 1)) (counter-n c)))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Self-Hosting Bootstrap Tests

(deftest self-host-make-register
  "Test self-hosting: register allocation utility."
  (assert-equal '(:R0 :R1 :R2) (run-string *self-host-make-register-program* :stdlib t)))

(deftest self-host-mini-compiler
  "Test self-hosting: complete mini-compiler pipeline (parse -> compile -> VM -> run)."
  (assert-= 35 (run-string *self-host-mini-compiler-program* :stdlib t)))

(deftest self-host-clos-compiler-full
  "Test self-hosting: CLOS-based compiler with generic dispatch (full pipeline)."
  (assert-true (equal '((:CONST :R0 3) (:CONST :R1 4) (:MUL :R2 :R0 :R1) (:CONST :R3 5) (:ADD :R4 :R2 :R3))
    (run-string *self-host-clos-compiler-full-program* :stdlib t))))


;;; Self-hosting and extended compiler integration tests moved to compiler-tests-selfhost.lisp.
;;; Additional integration coverage moved to compiler-tests-extended.lisp.
