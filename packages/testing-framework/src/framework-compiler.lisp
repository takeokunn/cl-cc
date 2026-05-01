;;;; tests/framework-compiler.lisp — CL-CC Test Framework (Compiler-Specific)
;;;; DSL helpers, differential testing, pattern matching, performance, cross-backend.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; High-Level Test Macros (requirement #7)
;;; ------------------------------------------------------------
;;;
;;; These macros reduce boilerplate in the most common test patterns:
;;;   1. deftest-compile  — compile-and-run tests (~200+ tests)
;;;   2. deftest-codegen                         — AST codegen instruction checks
;;;   3. deftest-vm                              — VM instruction execute-and-check

;;; --- Compile-and-Run ---

(defmacro deftest-compile (name docstring &key cases stdlib)
  "Define parameterized compile-and-run tests.
Each case compiles a CL source string and asserts the result equals EXPECTED.

Syntax:
  (deftest-compile name \"docstring\"
    :cases ((\"label\" expected form-string) ...)
    :stdlib t/nil)

When :STDLIB is T, uses (run-string form :stdlib t) instead of assert-run=."
  (let ((expansions
          (loop for (label expected form) in cases
                for source-id = (pathname-name
                                 (or *compile-file-pathname*
                                     *load-pathname*
                                     *default-pathname-defaults*))
                for test-name = (intern
                                 (format nil "~A/~A [~A]"
                                         (string-upcase source-id)
                                         (symbol-name name)
                                         label))
                collect
                  `(deftest ,test-name
                     ,docstring
                    ,(if stdlib
                         `(assert-evaluates-to ,form ,expected :stdlib t)
                         `(assert-run= ,expected ,form))))))
    `(progn ,@expansions)))

;;; --- Codegen Instruction Check ---

(defun %make-codegen-ctx ()
  "Create a fresh codegen context for testing."
  (let ((ctx-class (find-symbol "CODEGEN-CTX" :cl-cc/compile)))
    (if ctx-class
        (make-instance ctx-class)
        (error "Cannot find cl-cc/compile::codegen-ctx"))))

(defun %codegen-find-inst (ctx inst-type)
  "Find the first instruction of INST-TYPE in the codegen context's output."
  (let ((instructions-sym (find-symbol "CODEGEN-INSTRUCTIONS" :cl-cc/compile)))
    (when instructions-sym
      (find-if (lambda (i) (typep i (find-symbol (symbol-name inst-type) :cl-cc)))
               (funcall instructions-sym ctx)))))

(defmacro deftest-codegen (name docstring ast-form &body checks)
  "Define a codegen test that compiles AST-FORM and runs CHECKS against the context.

Syntax:
  (deftest-codegen name \"doc\"
    ast-form
    (:emits inst-type)           ; assert instruction type is present
    (:not-emits inst-type)       ; assert instruction type is absent
    (:returns-register-p)        ; assert compile-ast returns a keyword
    (:check (ctx reg) body...))  ; arbitrary check with ctx and result-reg bound"
  (let ((ctx-var (gensym "CTX"))
        (reg-var (gensym "REG")))
    (flet ((expand-check (check)
             (ecase (first check)
               (:emits
                `(assert-true (%codegen-find-inst ,ctx-var ',(second check))
                   :at ,(format nil "expected ~A" (second check))))
               (:not-emits
                `(assert-null (%codegen-find-inst ,ctx-var ',(second check))
                   :at ,(format nil "expected absence of ~A" (second check))))
               (:returns-register-p
                `(assert-true (keywordp ,reg-var)))
               (:check
                `(symbol-macrolet ((ctx ,ctx-var) (reg ,reg-var))
                   ,@(rest check))))))
      `(deftest ,name
         ,docstring
         (let* ((,ctx-var (%make-codegen-ctx))
                (,reg-var (compile-ast ,ast-form ,ctx-var)))
           (declare (ignorable ,reg-var))
           ,@(mapcar #'expand-check checks))))))

;;; --- VM Instruction Execute-and-Check ---

(defmacro deftest-vm (name docstring instruction-form &body checks)
  "Define a VM instruction test.
Creates a fresh VM, executes INSTRUCTION-FORM via exec1, then runs CHECKS.

Syntax:
  (deftest-vm name \"doc\"
    (make-vm-const :dst :R0 :value 42)
    (:reg :R0 = 42)               ; register equals value
    (:reg :R0 equal \"x\")         ; register equal to value
    (:halted)                      ; halt-p was true
    (:pc = 1)                      ; next pc value
    (:check (s pc halted result) body...))  ; arbitrary check"
  (let ((s-var     (gensym "S"))
        (pc-var    (gensym "PC"))
        (halt-var  (gensym "HALT"))
        (res-var   (gensym "RES")))
    (flet ((expand-check (check)
             (destructuring-bind (kind &rest args) check
               (ecase kind
                 (:reg
                  (destructuring-bind (reg op val) args
                    (let ((actual `(cl-cc:vm-reg-get ,s-var ,reg)))
                      (if (eq op '=)
                          `(assert-= ,val ,actual)
                          `(assert-equal ,val ,actual)))))
                 (:halted
                  `(assert-true ,halt-var))
                 (:pc
                  (destructuring-bind (op val) args
                    (declare (ignore op))
                    `(assert-= ,val ,pc-var)))
                 (:check
                  `(let ((,s-var ,s-var) (,pc-var ,pc-var)
                         (,halt-var ,halt-var) (,res-var ,res-var))
                     ,@args))))))
      `(deftest ,name
         ,docstring
         (let ((,s-var (make-test-vm)))
           (multiple-value-bind (,pc-var ,halt-var ,res-var)
               (exec1 ,instruction-form ,s-var)
             (declare (ignorable ,halt-var ,res-var))
             ,@(mapcar #'expand-check checks)))))))
