(in-package :cl-cc/test)

;;; Standard Library Set Operations Tests

(in-suite cl-cc-integration-serial-suite)

(deftest-compile stdlib-list-ops
  "set-difference, union, append-lists, and last-cons work on lists."
  :cases (("set-diff"       '(1 3 5)     "(set-difference (list 1 2 3 4 5) (list 2 4))")
          ("set-diff-empty" '(1 2 3)     "(set-difference (list 1 2 3) (list))")
          ("union"          '(1 2 3 4 5) "(sort (union (list 1 2 3) (list 3 4 5)) #'<)")
          ("append-lists"   '(1 2 3 4)   "(append (list 1 2) (list 3 4))")
          ("last-cons"      3            "(car (last (list 1 2 3)))"))
  :stdlib t)

(deftest-compile stdlib-reduce
  "reduce folds a list with a function and optional initial value."
  :cases (("basic"       10 "(reduce (lambda (a b) (+ a b)) (list 1 2 3 4))")
          ("single"      42 "(reduce (lambda (a b) (+ a b)) (list 42))")
          ("with-init"   10 "(reduce (lambda (a b) (+ a b)) (list 1 2 3 4) :initial-value 0)")
          ("empty-init"   0 "(reduce (lambda (a b) (+ a b)) nil :initial-value 0)"))
  :stdlib t)

(deftest-compile stdlib-reduce-edge
  "reduce edge cases: nil initial value, reduce-init accumulation."
  :cases (("init-nil"     nil      "(reduce (lambda (a b) (cons b a)) nil :initial-value nil)")
          ("init-accum"   '(3 2 1) "(reduce (lambda (acc x) (cons x acc)) (list 1 2 3) :initial-value nil)"))
  :stdlib t)

(in-suite cl-cc-integration-suite)

(deftest compile-hash-table-keys
  "hash-table-keys returns list of keys"
  (assert-= 2 (run-string " (let ((ht (make-hash-table))) (setf (gethash :x ht) 10) (setf (gethash :y ht) 20) (length (hash-table-keys ht)))")))

(deftest-each compile-clos-mop-introspection
  "CLOS MOP helpers expose direct/effective slots, slot metadata, metaclass, and redefinition migration."
  :tags '(:ansi-gap :mop)
  :cases (("direct-vs-effective-slots"
           '((b) (a b))
           "(progn
              (defclass mop-base () ((a :initarg :a)))
              (defclass mop-child (mop-base) ((b :initarg :b)))
              (list (mapcar #'slot-definition-name (class-direct-slots (find-class 'mop-child)))
                    (mapcar #'slot-definition-name (class-slots (find-class 'mop-child)))))")
          ("slot-type-initfunction-metaclass"
           '(x integer 7 standard-class)
           "(progn
              (defclass typed-mop () ((x :initarg :x :initform 7 :type integer)) (:metaclass standard-class))
              (let ((slot (car (class-slots (find-class 'typed-mop)))))
                (list (slot-definition-name slot)
                      (slot-definition-type slot)
                      (funcall (slot-definition-initfunction slot))
                      (class-metaclass (find-class 'typed-mop)))))")
          ("compute-effective-slot-definition"
           'integer
           "(progn
              (defclass effective-mop () ((x :initarg :x :type integer)))
              (slot-definition-type
               (compute-effective-slot-definition
                (find-class 'effective-mop)
                'x
                (class-direct-slots (find-class 'effective-mop)))))")
          ("redefined-class-lazy-migration"
           nil
           "(let ((obj nil))
              (defclass redef-mop () ((x :initarg :x)))
              (setq obj (make-instance 'redef-mop :x 1))
              (defclass redef-mop () ((y :initarg :y)))
              (slot-value obj 'y))")
          ("redefined-class-slot-boundp-migration"
           t
           "(let ((obj nil))
              (defclass boundp-redef-mop () ((x :initarg :x)))
              (setq obj (make-instance 'boundp-redef-mop :x 1))
              (defclass boundp-redef-mop () ((y :initarg :y)))
              (slot-boundp obj 'y))")
          ("redefined-class-slot-makunbound-migration"
           nil
           "(let ((obj nil))
              (defclass makun-redef-mop () ((x :initarg :x)))
              (setq obj (make-instance 'makun-redef-mop :x 1))
              (defclass makun-redef-mop () ((y :initarg :y)))
              (slot-makunbound obj 'y)
              (slot-boundp obj 'y))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

;;; Defstruct Tests

(deftest-each compile-defstruct
  "defstruct creates constructors, accessors, predicates, and custom variants."
  :cases (("basic"       10 " (progn (defstruct point x y) (let ((p (make-point :x 10 :y 20))) (point-x p)))")
          ("default"      0 " (progn (defstruct counter (count 0)) (let ((c (make-counter))) (counter-count c)))")
          ("predicate"    1 " (progn (defstruct my-box value) (let ((b (make-my-box :value 42))) (if (my-box-p b) 1 0)))")
          ("typep"        1 " (progn (defstruct my-pair first second) (let ((p (make-my-pair :first 1 :second 2))) (if (typep p 'my-pair) 1 0)))")
          ("boa"          3 " (progn (defstruct (my-vec (:constructor make-my-vec (x y))) x y) (let ((v (make-my-vec 1 3))) (my-vec-y v)))")
          ("conc-name"   42 " (progn (defstruct (my-item (:conc-name item-)) value) (let ((i (make-my-item :value 42))) (item-value i)))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Car/Cdr Composition Tests

(deftest-each compile-cxr-basic
  "c*r compositions extract elements from list structures."
  :cases (("caar"  1    "(caar (list (list 1 2) (list 3 4)))")
          ("cadr"  2    "(cadr (list 1 2 3))")
          ("cddr"  '(3) "(cddr (list 1 2 3))")
          ("caddr" 3    "(caddr (list 1 2 3 4))"))
  (expected form)
  (assert-true (equal expected (run-string form))))

;;; Stdlib Find/Position Tests

(deftest-compile stdlib-find-position
  "find and position return element/index, or nil when not found."
  :cases (("find"          3   "(find 3 (list 1 2 3 4 5))")
          ("find-miss"     nil "(find 9 (list 1 2 3))")
          ("position"      2   "(position 3 (list 1 2 3 4 5))")
          ("position-miss" nil "(position 9 (list 1 2 3))"))
  :stdlib t)

(deftest-each stdlib-cons-printing-forms
  "Stdlib forms producing cons/alist structures render correctly as lowercase strings."
  :cases (("find-with-key"   "(2 . b)"           "(find 2 (list (cons 1 'a) (cons 2 'b) (cons 3 'c)) :key (lambda (x) (car x)))")
          ("pairlis"         "((b . 2) (a . 1))"  "(pairlis (list 'a 'b) (list 1 2))")
          ("assoc-if"        "(2 . b)"            "(assoc-if (lambda (k) (= k 2)) (list (cons 1 'a) (cons 2 'b)))")
          ("rassoc"          "(2 . b)"            "(rassoc 'b (list (cons 1 'a) (cons 2 'b) (cons 3 'c)))")
          ("find-sharpsign-key" "(2 . b)"         "(find 2 (list (cons 1 'a) (cons 2 'b) (cons 3 'c)) :key #'car)"))
  (expected form)
  (assert-true (string= expected
                         (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                           (string-downcase (format nil "~S" (run-string form :stdlib t)))))))

(deftest stdlib-identity
  "identity returns its argument"
  (assert-run= 42 "(identity 42)"))

;;; Setf Places Tests

(deftest-each compile-setf-places
  "setf on car/cdr/first/nth returns and mutates the correct value."
  :cases (("car"         99 "(let ((pair (cons 1 2))) (setf (car pair) 99) (car pair))")
          ("cdr"         99 "(let ((pair (cons 1 2))) (setf (cdr pair) 99) (cdr pair))")
          ("first"       42 "(let ((lst (list 1 2 3))) (setf (first lst) 42) (first lst))")
          ("nth"         99 "(let ((lst (list 10 20 30))) (setf (nth 1 lst) 99) (nth 1 lst))")
          ("returns-val" 42 "(let ((pair (cons 1 2))) (setf (car pair) 42))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Package System Tests

(deftest-each compile-package-forms
  "in-package and defpackage return their package keywords; subsequent forms evaluate normally."
  :cases (("in-package"        :cl-cc   "(in-package :cl-cc)")
          ("defpackage"        :test-pkg "(defpackage :test-pkg (:use :cl))")
          ("in-package-then-code" 42   "(progn (in-package :cl-cc) 42)"))
  (expected form)
  (assert-true (equal expected (run-string form))))

;;; Macrolet Tests

;;; Macrolet and Function Reference Tests (#'builtin)

(deftest-each compile-macrolet-and-funcall
  "macrolet scoped macros and #'builtin funcall return the expected numeric results."
  :cases (("macrolet-basic"    6  "(macrolet ((double (x) `(+ ,x ,x))) (double 3))")
          ("macrolet-multiple" 10 "(macrolet ((add1 (x) `(+ ,x 1)) (add2 (x) `(+ ,x 2))) (+ (add1 3) (add2 4)))")
          ("macrolet-scoped"   42 "(let ((x 42)) (macrolet ((get-x () 'x)) (get-x)))")
          ("macrolet-nested"    8 "(macrolet ((square (x) `(* ,x ,x))) (macrolet ((sq-plus-sq (a b) `(+ (square ,a) (square ,b)))) (sq-plus-sq 2 2)))")
          ("funcall-car"        1 "(funcall #'car (cons 1 2))")
          ("funcall-plus"       7 "(funcall #'+ 3 4)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-function-sharpsign
  "#'builtin creates callables usable with funcall and higher-order functions."
  :cases (("cons-pair"  '(1 . 2) "(funcall #'cons 1 2)")
          ("car-mapcar" '(1 2 3) "(mapcar #'car (list (cons 1 'a) (cons 2 'b) (cons 3 'c)))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

;;; Warn Test

;;; String Concatenation Tests

(deftest-each compile-string-concat
  "string-concat and concatenate 'string join strings correctly."
  :cases (("two-strings" "hello world" "(string-concat \"hello \" \"world\")")
          ("concat-abc"  "abc"         "(concatenate 'string \"a\" \"b\" \"c\")")
          ("concat-two"  "foobar"      "(concatenate 'string \"foo\" \"bar\")")
          ("concat-one"  "hello"       "(concatenate 'string \"hello\")"))
  (expected form)
  (assert-string= expected (run-string form)))

;;; Check-Type Tests

(deftest-each compile-check-type
  "check-type passes silently for correct type and signals error for wrong type."
  :cases (("passes"
           "(let ((x 42)) (check-type x integer))"
           (lambda (form)
             (assert-true (eq nil (run-string form)))))
          ("errors"
           "(let ((x \"hello\")) (check-type x integer))"
           (lambda (form)
             (assert-signals error (run-string form)))))
  (form verify)
  (funcall verify form))

(deftest-each compile-correctable-type-restarts
  "check-type, ccase, and ctypecase honor STORE-VALUE restarts."
  :cases (("check-type-store-value"
           7
           "(let ((x \"bad\")) (handler-bind ((type-error (lambda (c) (declare (ignore c)) (store-value 7)))) (check-type x integer) x))")
          ("ccase-store-value"
           11
           "(let ((x 'bad)) (handler-bind ((type-error (lambda (c) (declare (ignore c)) (store-value 'ok)))) (ccase x (ok 11))))")
          ("ctypecase-store-value"
           42
           "(let ((x \"bad\")) (handler-bind ((type-error (lambda (c) (declare (ignore c)) (store-value 42)))) (ctypecase x (integer x))))"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

;;; Eval-When Tests

(deftest-each compile-eval-when-numeric
  "eval-when :execute/:load-toplevel includes body and returns numeric value."
  :cases (("execute"       42 "(eval-when (:execute) 42)")
          ("load-toplevel" 10 "(eval-when (:load-toplevel :execute) (+ 3 7))")
          ("all"            5 "(eval-when (:compile-toplevel :load-toplevel :execute) 5)"))
  (expected form)
  (handler-bind ((warning #'muffle-warning))
    (assert-= expected (run-string form))))

(deftest compile-eval-when-skip
  "eval-when without :execute skips body"
  (handler-bind ((warning #'muffle-warning))
    (assert-true (eq nil (run-string "(eval-when (:compile-toplevel) 42)")))))

;;; Property List and Set Operations Tests

(deftest-compile stdlib-getf-and-set-ops
  "getf returns the correct value; intersection and remove filter list elements."
  :cases (("getf-found"          2         "(getf (list :a 1 :b 2 :c 3) :b)")
          ("getf-default"        99        "(getf (list :a 1) :z 99)")
          ("getf-first"          1         "(getf (list :a 1 :b 2) :a)")
          ("getf-not-found"      nil       "(getf (list :a 1 :b 2) :z)")
          ("set-intersection"    '(2 3)    "(intersection (list 1 2 3) (list 2 3 4))")
          ("set-intersection-empty" nil    "(intersection (list 1 2) (list 3 4))")
          ("set-remove"          '(1 3 5)  "(remove 2 (list 1 2 3 2 5))"))
  :stdlib t)

;;; Eval Tests

(deftest-each our-eval-numeric
  "our-eval compiles and runs arithmetic, lambda, and let forms."
  :cases (("basic"  42 '(+ 20 22))
          ("lambda" 10 '(funcall (lambda (x) (+ x 3)) 7))
          ("let"    15 '(let ((a 5) (b 10)) (+ a b))))
  (expected form)
  (assert-= expected (our-eval form)))
