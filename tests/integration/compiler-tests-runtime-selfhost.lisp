(in-package :cl-cc/test)

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
  :timeout 180
  (assert-true (equal '((:CONST :R0 3) (:CONST :R1 4) (:MUL :R2 :R0 :R1) (:CONST :R3 5) (:ADD :R4 :R2 :R3))
    (run-string *self-host-clos-compiler-full-program* :stdlib t))))
