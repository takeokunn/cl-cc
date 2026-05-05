;;;; tests/e2e/selfhost-tests.lisp — Self-Hosting End-to-End Tests
;;;; Demonstrates that cl-cc can compile and run significant pieces of its own
;;;; compiler infrastructure: CPS transformer, optimizer, macro expander, etc.

(in-package :cl-cc/test)

(defmacro with-selfhost-warning-tolerance (&body body)
  `(handler-bind ((warning #'muffle-warning))
     ,@body))

(defsuite selfhost-suite
  :description "Self-hosting end-to-end tests"
  :parent cl-cc-e2e-suite
  :parallel nil)

(defbefore :each (selfhost-suite)
  (setf cl-cc:*macro-eval-fn* #'cl-cc::our-eval))

(in-suite selfhost-suite)

;;; ─── REPL State Tests ──────────────────────────────────────────────────────

(deftest selfhost-defvar-persists
  "defvar values persist across REPL calls."
  (with-selfhost-warning-tolerance
    (assert-eql 200
      (run-repl-forms
       "(defvar *sh-counter* 100)"
       "(setq *sh-counter* 200)"
       "*sh-counter*"))))

(deftest selfhost-label-isolation
  "Labels from different REPL compilations don't collide."
  (with-selfhost-warning-tolerance
    (assert-eq :yes
      (run-repl-forms
       "(defun sh-pred (x) (or (numberp x) (symbolp x)))"
       "(defun sh-check (x y) (if (sh-pred x) :yes :no))"
       "(sh-check 42 'ignored)"))))

;;; ─── Self-Hosting: CPS Transformer ─────────────────────────────────────────

(in-suite selfhost-suite)

(deftest-each selfhost-quasiquote
  "quasiquote works in self-hosted helper definitions and produces the expected result."
  :cases (("defun-builds-form" 9
           "(defun make-mul (a b) `(* ,a ,b))"
           "(let ((form (make-mul 3 3))) (eval form))")
          ("defun-builds-binding-form" t
           "(defun wrap-in-let (var val body) `(let ((,var ,val)) ,body))"
           "(equal (wrap-in-let 'x 5 '(+ x 1)) '(let ((x 5)) (+ x 1)))"))
  (expected setup-form eval-form)
  (with-selfhost-warning-tolerance
    (assert-true
      (equal expected
             (run-repl-forms setup-form eval-form)))))

(deftest selfhost-cps-transformer
  "cl-cc compiles a CPS transformer using quasiquotes and recursion."
  (with-selfhost-warning-tolerance
    (let ((r (run-repl-forms
              "(defun sh-cps-atom-p (x)
              (or (numberp x) (symbolp x) (stringp x)))"
              "(defun sh-cps (expr k)
               (cond
                 ((sh-cps-atom-p expr) `(funcall ,k ,expr))
                  ((eq (car expr) 'if)
                  (let ((tv (gensym \"T\"))
                        (then-r (sh-cps (caddr expr) k))
                        (else-r (sh-cps (cadddr expr) k)))
                    (sh-cps (cadr expr)
                             `(lambda (,tv) (if ,tv ,then-r ,else-r)))))
                 (t `(funcall ,k ,expr))))"
              "(sh-cps '(if x 1 2) '(lambda (v) v))")))
      (assert-true (and (consp r) (eq (car r) 'funcall))))))

(deftest-each selfhost-cps-transformer-arithmetic
  "cl-cc compiles and runs its own CPS transformer for arithmetic expressions."
  :cases (("add-1-2" 3 '(+ 1 2))
          ("mul-6-7" 42 '(* 6 7)))
  (expected expr)
  (with-selfhost-warning-tolerance
    (assert-= expected
              (run-repl-forms
               "(defun sh-cps-run (expr)
                 (cond
                   ((integerp expr) expr)
                   ((symbolp expr) expr)
                  ((consp expr)
                   (case (car expr)
                     (+ (+ (sh-cps-run (second expr)) (sh-cps-run (third expr))))
                     (- (- (sh-cps-run (second expr)) (sh-cps-run (third expr))))
                     (* (* (sh-cps-run (second expr)) (sh-cps-run (third expr))))
                     (otherwise (error \"Unsupported\"))))
                  (t (error \"Unsupported\"))))"
               (format nil "(sh-cps-run '~S)" expr)))))

;;; ─── Self-Hosting: Optimizer Pattern Matcher ───────────────────────────────

(deftest selfhost-optimizer-fold
  "cl-cc compiles an optimizer-style constant folder."
  (with-selfhost-warning-tolerance
    (assert-eql 7
      (run-repl-forms
        "(defun sh-fold (op a b)
         (cond
           ((and (eq op '+) (numberp a) (numberp b)) (+ a b))
           ((and (eq op '+) (eql a 0)) b)
           ((and (eq op '+) (eql b 0)) a)
           (t (list op a b))))"
        "(sh-fold '+ 3 4)"))))

;;; ─── Self-Hosting: Macro Code Generation ───────────────────────────────────

(deftest selfhost-macro-codegen
  "cl-cc compiles a macro that generates constructor and accessor functions."
  (with-selfhost-warning-tolerance
    (assert-eql 30
      (run-repl-forms
        "(defmacro sh-def-record (name &rest fields)
         `(progn
            (defun ,(intern (format nil \"MAKE-~A\" name)) (&rest args)
              args)
           (defun ,(intern (format nil \"~A-REF\" name)) (obj field)
             (getf obj field))))"
      "(sh-def-record sh-person :name :age)"
        "(let ((p (make-sh-person :name \"Alice\" :age 30)))
           (sh-person-ref p :age))"))))

;;; ─── Self-Hosting: Recursive Data Processing ──────────────────────────────

(in-suite selfhost-suite)

(deftest selfhost-tree-walk
  "cl-cc compiles a recursive tree walker."
  (with-selfhost-warning-tolerance
    (assert-eql 10
      (run-repl-forms
       "(defun sh-tree-sum (tree)
         (if (numberp tree)
             tree
             (+ (sh-tree-sum (car tree))
                (sh-tree-sum (cdr tree)))))"
       "(sh-tree-sum '(1 . (2 . (3 . 4))))"))))

;;; ─── Self-Hosting: Load File ───────────────────────────────────────────────

(deftest selfhost-load-multi-form
  "cl-cc can load a file with multiple top-level forms."
  (let ((tmpfile (format nil "/tmp/cl-cc-selfhost-~A.lisp" (get-universal-time))))
    (unwind-protect
         (progn
           (with-open-file (s tmpfile :direction :output :if-exists :supersede)
             (write-string "(defvar *sh-base* 100)
(defun sh-offset (n) (+ *sh-base* n))" s))
            (with-selfhost-warning-tolerance
              (assert-eql 142
                (run-repl-forms
                 (format nil "(load ~S)" tmpfile)
                 "(sh-offset 42)"))))
      (ignore-errors (delete-file tmpfile)))))

;;; ─── Self-Hosting: Higher-Order Functions ──────────────────────────────────

(deftest selfhost-hof-pipeline
  "cl-cc compiles a pipeline of higher-order functions via lambda wrappers."
  (with-selfhost-warning-tolerance
    (assert-eql 21
      (run-repl-forms
       "(defun sh-compose (f g) (lambda (x) (funcall f (funcall g x))))"
       "(defun sh-add1 (x) (+ x 1))"
       "(defun sh-double (x) (* x 2))"
       "(funcall (sh-compose (lambda (x) (sh-add1 x)) (lambda (x) (sh-double x))) 10)"))))

;;; ─── Self-Hosting: Handler-Case with Recovery ─────────────────────────────

(deftest selfhost-error-recovery
  "cl-cc compiles handler-case for error recovery."
  (assert-eql 42
    (run-string "(handler-case
                   (progn (error \"oops\") 0)
                   (error (e) 42))")))

;;; ─── Self-Hosting: defstruct roundtrip ─────────────────────────────────────

(deftest selfhost-defstruct-roundtrip
  "cl-cc compiles defstruct with constructors and accessors."
  (with-selfhost-warning-tolerance
    (assert-eql 4
      (run-repl-forms
        "(defstruct sh-point x y)"
        "(let ((p (make-sh-point :x 3 :y 4)))
           (sh-point-y p))"))))

;;; ─── Self-Hosting: Mutual Recursion via labels ─────────────────────────────

(deftest selfhost-mutual-recursion
  "cl-cc compiles mutually recursive local functions via labels using the
selfhost REPL path (run-string-repl) to cover the REPL-specific code path."
  (with-selfhost-warning-tolerance
    (assert-true
      (run-repl-forms
       "(labels ((is-even (n) (if (= n 0) t (is-odd (- n 1))))
                (is-odd (n) (if (= n 0) nil (is-even (- n 1)))))
         (is-even 10))"))))

;;; ─── Self-Hosting: Reader Macros ─────────────────────────────────────────

(deftest selfhost-reader-macros
  "Reader macros #:, #+, #-, and #. compile and evaluate correctly."
  (assert-true
    (run-string "(symbolp (quote #:foo))"))
  (assert-eq :yes
    (run-string "#+sbcl :yes"))
  (assert-eq :yes
    (run-string "#-nonexistent-feature :yes"))
  (assert-eq :fallback
    (run-string "(progn #+nonexistent-feature :no :fallback)"))
  (assert-eql 6
    (run-string "(+ 1 #.(+ 2 3))")))

(deftest selfhost-read-eval-respects-special
  "#.-reader evaluation is rejected when *read-eval* is nil."
  (assert-true
   (null
    (ignore-errors
      (run-string "(let ((*read-eval* nil)) (read-from-string \"#.(+ 2 3)\"))")))))
