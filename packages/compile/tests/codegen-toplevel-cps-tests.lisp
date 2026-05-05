(in-package :cl-cc/test)

(in-suite cl-cc-codegen-unit-serial-suite)

(defun %unwrap-captured-cps-entry (captured-expr)
  "Normalize top-level CPS capture shape.
Accept either a raw (lambda (k) ...) form or a singleton list containing it."
  (if (and (consp captured-expr)
           (consp (car captured-expr))
           (eq 'lambda (caar captured-expr)))
      (car captured-expr)
      captured-expr))

;;; ─── %make-compile-opts ──────────────────────────────────────────────────

(deftest codegen-make-compile-opts-defaults
  "%make-compile-opts: opt-remarks-mode defaults to :all; all other slots nil."
  (let ((opts (cl-cc/compile::%make-compile-opts)))
    (assert-true (listp opts))
    (assert-null (getf opts :pass-pipeline))
    (assert-null (getf opts :print-pass-timings))
    (assert-null (getf opts :timing-stream))
    (assert-eq :all (getf opts :opt-remarks-mode))
    (assert-null (getf opts :trace-json-stream))))

(deftest codegen-make-compile-opts-explicit-values
  "%make-compile-opts captures explicit values into the plist."
  (let ((opts (cl-cc/compile::%make-compile-opts :pass-pipeline '(:fold :dce)
                                                 :opt-remarks-mode :pass
                                                 :print-pass-stats t)))
    (assert-equal '(:fold :dce) (getf opts :pass-pipeline))
    (assert-eq :pass (getf opts :opt-remarks-mode))
    (assert-true (getf opts :print-pass-stats))))

;;; ─── %result-vm-instructions-without-halt ────────────────────────────────

(deftest codegen-result-vm-instructions-without-halt-strips-terminal-halt-toplevel
  "%result-vm-instructions-without-halt removes only the final vm-halt instruction."
  (let* ((move (cl-cc:make-vm-move :dst :R1 :src :R0))
         (halt (cl-cc:make-vm-halt :reg :R1))
         (result (cl-cc/compile:make-compilation-result
                  :program (cl-cc:make-vm-program :instructions (list move halt) :result-register :R1)
                  :vm-instructions (list move halt))))
    (assert-equal (list move)
                  (cl-cc/compile::%result-vm-instructions-without-halt result))))

(deftest codegen-toplevel-safe-form-prefers-cps-primary-path
  "compile-toplevel-forms routes VM-safe literal top-level forms through the CPS entry path."
  (let ((captured-expr nil)
         (compile-ast-called nil))
    (with-replaced-function (cl-cc/compile:compile-expression
                             (lambda (expr &rest args)
                               (declare (ignore args))
                               (setf captured-expr expr)
                               (cl-cc/compile:make-compilation-result
                                :program (cl-cc:make-vm-program :instructions nil :result-register :R-CPS)
                                :vm-instructions (list (cl-cc:make-vm-halt :reg :R-CPS))
                                :cps '(lambda (k) (funcall k 3)))))
    (with-replaced-function (cl-cc/compile:compile-ast
                               (lambda (&rest args)
                                 (declare (ignore args))
                                 (setf compile-ast-called t)
                                 :R-DIRECT))
        (cl-cc/compile:compile-toplevel-forms '(42) :target :vm)))
    (let ((normalized (%unwrap-captured-cps-entry captured-expr)))
      (assert-true normalized)
      (assert-eq 'lambda (car normalized)))
    (assert-false compile-ast-called)))

(deftest-each codegen-toplevel-direct-path-safe-subset-exclusions
  "The VM CPS-safe subset excludes definition/control forms that still require the direct path."
  :cases (("defun" '(defun cps-safe-fn (x) x))
           ("defvar" '(defvar *cps-safe-var* 1))
           ("defun-rest" '(defun cps-safe-rest-fn (x &rest rest) x))
           ("defun-optional" '(defun cps-safe-opt-fn (x &optional y) x))
           ("defun-key" '(defun cps-safe-key-fn (&key x) x))
           ("defclass" '(defclass cps-safe-class () ((slot :initarg :slot))))
           ("defmethod" '(defmethod cps-safe-generic ((x integer)) x)))
  (form)
  (let* ((expanded (cl-cc/expand:compiler-macroexpand-all form))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded)))
    (assert-false (cl-cc/compile:%cps-vm-compile-safe-ast-p ast))))

(deftest-each codegen-toplevel-safe-subset-still-allows-simple-definition-and-clos-forms
  "Simple top-level defvar/CLOS helper forms remain inside the current VM CPS-safe subset."
  :cases (("defgeneric" '(defgeneric cps-safe-generic (x)))
           ("make-instance" '(make-instance 'cps-safe-class))
           ("slot-value" '(slot-value obj 'slot))
           ("set-slot-value" '(setf (slot-value obj 'slot) 1)))
  (form)
  (let* ((expanded (cl-cc/expand:compiler-macroexpand-all form))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded)))
    (assert-true (cl-cc/compile:%cps-vm-compile-safe-ast-p ast))))

(deftest codegen-toplevel-variadic-lambda-stays-unsafe
  "A variadic AST-LAMBDA stays on the direct path until the CPS lambda path preserves optional/rest/key metadata."
  (let ((lambda-ast (cl-cc/ast:make-ast-lambda
                     :params '(x)
                     :optional-params (list (list 'y nil nil))
                     :body (list (cl-cc:make-ast-var :name 'x)))))
    (assert-false (cl-cc::%cps-vm-compile-safe-ast-p lambda-ast))))

(deftest codegen-toplevel-unsafe-form-stays-on-direct-path
  "Current compile-toplevel-forms still routes unsafe single forms through compile-expression."
  (let ((compile-expression-called nil)
        (compile-ast-called nil))
    (with-replaced-function (cl-cc/compile:compile-expression
                             (lambda (&rest args)
                               (declare (ignore args))
                               (setf compile-expression-called t)
                               (cl-cc/compile:make-compilation-result
                                :program (cl-cc:make-vm-program :instructions nil :result-register :R-CPS)
                                :vm-instructions (list (cl-cc:make-vm-halt :reg :R-CPS)))))
      (with-replaced-function (cl-cc/compile:compile-ast
                               (lambda (&rest args)
                                 (declare (ignore args))
                                 (setf compile-ast-called t)
                                 :R-DIRECT))
        (let ((*enable-cps-vm-primary-path* nil))
          (cl-cc/compile:compile-toplevel-forms '((+ 1 2)) :target :vm))))
    (assert-true compile-expression-called)
    (assert-false compile-ast-called)))

(deftest-compile codegen-toplevel-cps-semantic-preservation
  "Multi-form Lisp sources still evaluate to the final value after the top-level CPS routing change."
  :cases (("two-safe-forms" 7 "(+ 1 2) (+ 3 4)")
          ("defvar-then-use" 3 "(defvar *ulw-cps* 1) (+ *ulw-cps* 2)")
          ("call-bearing-form" 6 "(defun add1 (x) (+ x 1)) (add1 5)"))
  :stdlib nil)

(deftest-compile codegen-toplevel-cps-multi-value-semantics
  "Top-level CPS routing preserves apply/values/multiple-value-bind behavior for supported forms."
  :cases (("apply" 6 "(apply #'+ (list 1 2 3))")
          ("values-primary" 1 "(values 1 2 3)")
          ("multiple-value-bind" 3 "(multiple-value-bind (a b) (values 1 2) (+ a b))"))
  :stdlib nil)
