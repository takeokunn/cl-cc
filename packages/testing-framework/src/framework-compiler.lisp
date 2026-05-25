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

;;; ------------------------------------------------------------
;;; Benchmark support (FR-316)
;;; ------------------------------------------------------------

(defun %benchmark-now-ns ()
  "Return the current internal real time converted to nanoseconds."
  (floor (* (get-internal-real-time) 1000000000)
         internal-time-units-per-second))

(defun %benchmark-duration-ns (thunk)
  "Run THUNK once and return its elapsed duration in nanoseconds."
  (let ((start (%benchmark-now-ns)))
    (funcall thunk)
    (- (%benchmark-now-ns) start)))

(defun %benchmark-percentile (sorted-values percentile)
  "Return the nearest-rank PERCENTILE from SORTED-VALUES."
  (let ((count (length sorted-values)))
    (if (zerop count)
        0
        (let ((index (max 0 (min (1- count)
                                 (1- (ceiling (* percentile count)))))))
          (nth index sorted-values)))))

(defun benchmark-statistics (durations-ns &key (warmup-count 0))
  "Return a plist of benchmark statistics for DURATIONS-NS.

The result contains stable machine-readable keys: :ITERATION-COUNT,
:WARMUP-COUNT, :DURATIONS-NS, :MIN-NS, :MAX-NS, :MEAN-NS, :MEDIAN-NS,
:P99-NS, and :STDDEV-NS."
  (let* ((durations (copy-list durations-ns))
         (count (length durations))
         (sorted (sort (copy-list durations) #'<))
         (sum (reduce #'+ durations :initial-value 0))
         (mean (if (zerop count) 0 (/ sum count)))
         (median (cond
                   ((zerop count) 0)
                   ((oddp count) (nth (floor count 2) sorted))
                   (t (/ (+ (nth (1- (/ count 2)) sorted)
                           (nth (/ count 2) sorted))
                         2))))
         (variance (if (zerop count)
                       0
                       (/ (reduce #'+ durations
                                  :key (lambda (duration)
                                         (let ((delta (- duration mean)))
                                           (* delta delta)))
                                  :initial-value 0)
                          count))))
    (list :iteration-count count
          :warmup-count warmup-count
          :durations-ns durations
          :min-ns (if sorted (first sorted) 0)
          :max-ns (if sorted (car (last sorted)) 0)
          :mean-ns mean
          :median-ns median
          :p99-ns (%benchmark-percentile sorted 0.99)
          :stddev-ns (sqrt variance))))

(defun run-benchmark (name thunk &key (warmup 1) (iterations 10) output-stream)
  "Run THUNK as benchmark NAME and return a benchmark result plist.

WARMUP controls unmeasured iterations. ITERATIONS controls measured
iterations. When OUTPUT-STREAM is non-NIL, a JSON representation is written to
that stream before the result plist is returned."
  (check-type warmup (integer 0 *))
  (check-type iterations (integer 1 *))
  (dotimes (_ warmup)
    (declare (ignore _))
    (funcall thunk))
  (let* ((durations (loop repeat iterations collect (%benchmark-duration-ns thunk)))
         (stats (benchmark-statistics durations :warmup-count warmup))
         (result (list* :name name stats)))
    (when output-stream
      (write-benchmark-result-json result output-stream)
      (terpri output-stream))
    result))

(defun %parse-defbenchmark-body (forms)
  "Parse DEFBENCHMARK body into docstring, options, and benchmark forms."
  (let ((docstring nil)
        (warmup 1)
        (iterations 10)
        (output-stream nil)
        (rest forms))
    (when (and rest (stringp (first rest)))
      (setf docstring (pop rest)))
    (loop while (and rest (keywordp (first rest)))
          do (let ((key (pop rest))
                   (value (pop rest)))
               (case key
                 (:warmup (setf warmup value))
                 (:iterations (setf iterations value))
                 (:output-stream (setf output-stream value))
                 (otherwise (error "Unknown DEFBENCHMARK option: ~S" key)))))
    (values docstring warmup iterations output-stream rest)))

(defmacro defbenchmark (name &body body)
  "Define a benchmark function.

Syntax:
  (defbenchmark name
    \"optional docstring\"
    :warmup 3
    :iterations 100
    body...)

The generated function accepts :WARMUP, :ITERATIONS, and :OUTPUT-STREAM keyword
overrides and returns the plist produced by RUN-BENCHMARK.
The benchmark is automatically registered in *BENCHMARK-REGISTRY*."
  (multiple-value-bind (docstring warmup iterations output-stream body-forms)
      (%parse-defbenchmark-body body)
    `(progn
       (defun ,name (&key (warmup ,warmup)
                           (iterations ,iterations)
                           (output-stream ,output-stream))
         ,(or docstring (format nil "Run benchmark ~A." name))
         (run-benchmark ',name
                        (lambda () ,@body-forms)
                        :warmup warmup
                        :iterations iterations
                        :output-stream output-stream))
       (pushnew (list :name ',name
                      :docstring ,docstring
                      :warmup ,warmup
                      :iterations ,iterations)
                *benchmark-registry*
                :key (lambda (entry) (getf entry :name))
                :test #'eq)
       ',name)))


(defun run-all-benchmarks (&key (output-directory nil)
                                (warmup-override nil)
                                (iteration-override nil))
  "Run every benchmark in *BENCHMARK-REGISTRY*.
When OUTPUT-DIRECTORY is non-NIL, each benchmark's JSON result is written to
<name>.json in that directory.  Returns a list of result plists."
  (let ((results '())
        (dir (when output-directory
               (ensure-directories-exist output-directory)
               output-directory)))
    (dolist (entry (reverse *benchmark-registry*))
      (let* ((name (getf entry :name))
             (warmup (or warmup-override (getf entry :warmup) 1))
             (iterations (or iteration-override (getf entry :iterations) 10))
             (result-path (and dir
                               (merge-pathnames
                                (format nil "~(~A~).json" name)
                                dir)))
             (output-stream (and result-path
                                 (open result-path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create))))
        (format t "# benchmark ~(~A~)...~%" name)
        (force-output)
        (handler-case
            (let* ((result (funcall name :warmup warmup
                                    :iterations iterations
                                    :output-stream output-stream))
                   (mean-ns (getf result :mean-ns))
                   (min-ns  (getf result :min-ns))
                   (max-ns  (getf result :max-ns)))
              (when output-stream
                (close output-stream))
              (push result results)
              (format t "#   mean=~,2Fms min=~,2Fms max=~,2Fms (~D iters)~%"
                      (/ mean-ns 1000000.0) (/ min-ns 1000000.0)
                      (/ max-ns 1000000.0) (getf result :iteration-count))
              (force-output))
          (error (e)
            (when output-stream
              (ignore-errors (close output-stream)))
            (format *error-output* "#   ERROR: ~A~%" e)
            (force-output)))))
    results))

(defun %json-escape-string (value)
  "Return VALUE escaped for JSON string output."
  (with-output-to-string (stream)
    (loop for char across value
          do (case char
               (#\" (write-string "\\\"" stream))
               (#\\ (write-string "\\\\" stream))
               (#\Newline (write-string "\\n" stream))
               (#\Return (write-string "\\r" stream))
               (#\Tab (write-string "\\t" stream))
               (otherwise (write-char char stream))))))

(defun %benchmark-json-key (key)
  "Convert plist KEY to the stable lower-case JSON field name."
  (string-downcase (substitute #\_ #\- (string key))))

(defun %benchmark-json-value (value stream)
  "Write VALUE to STREAM as a dependency-free JSON value."
  (cond
    ((null value) (write-string "null" stream))
    ((stringp value) (format stream "\"~A\"" (%json-escape-string value)))
    ((symbolp value) (format stream "\"~A\"" (%json-escape-string (string value))))
    ((integerp value) (princ value stream))
    ((realp value) (format stream "~,12F" value))
    ((listp value)
     (write-char #\[ stream)
     (loop for item in value
           for first-p = t then nil
           do (progn
                (unless first-p (write-char #\, stream))
                (%benchmark-json-value item stream)))
     (write-char #\] stream))
    (t (format stream "\"~A\"" (%json-escape-string (prin1-to-string value))))))

(defun write-benchmark-result-json (result stream)
  "Write benchmark RESULT plist as a compact JSON object to STREAM."
  (write-char #\{ stream)
  (loop for (key value) on result by #'cddr
        for first-p = t then nil
        do (progn
             (unless first-p (write-char #\, stream))
             (format stream "\"~A\":" (%benchmark-json-key key))
             (%benchmark-json-value value stream)))
  (write-char #\} stream)
  result)

(defun benchmark-result-json (result)
  "Return benchmark RESULT plist as a compact JSON object string."
  (with-output-to-string (stream)
    (write-benchmark-result-json result stream)))

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
