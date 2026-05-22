(in-package :cl-cc/test)

(in-suite pipeline-native-suite)

(deftest fr-500-lto-ir-roundtrip
  "FR-500: LTO serializes VM IR portably and deserializes it at link time."
  (let* ((module (cl-cc/pipeline:make-lto-module
                  :name "unit" :target :x86-64 :language :lisp
                  :instructions (list (make-vm-const :dst :r0 :value 42)
                                      (make-vm-ret :reg :r0))))
         (bytes (cl-cc/pipeline:serialize-lto-ir (list module)))
         (roundtrip (cl-cc/pipeline:deserialize-lto-ir bytes)))
    (assert-= 1 (length roundtrip))
    (assert-true (typep (first (cl-cc/pipeline::lto-module-instructions (first roundtrip)))
                        'vm-const))))

(deftest fr-501-thin-lto-summary-generation
  "FR-501: ThinLTO builds per-function summaries without importing full IR eagerly."
  (let* ((module (cl-cc/pipeline:make-lto-module
                  :name "thin" :target :x86-64 :language :lisp
                  :instructions (list (make-vm-closure :dst :r0 :label "f" :params '(:r1) :captured nil)
                                      (make-vm-label :name "f")
                                      (make-vm-ret :reg :r1))))
         (summary (cl-cc/pipeline:generate-thin-lto-summaries module))
         (functions (cl-cc/pipeline::thin-lto-summary-functions summary)))
    (assert-= 1 (length functions))
    (assert-equal '(:r1) (cl-cc/pipeline::thin-lto-function-summary-signature (first functions)))))

(defun %write-test-source (path text)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string text out))
  path)

(deftest pipeline-incremental-skips-unchanged-source
  "FR-640 records source hashes and skips unchanged recompilation."
  (uiop:with-temporary-file (:pathname source :type "lisp" :keep t)
    (let ((output (make-pathname :type "bin" :defaults source)))
      (%write-test-source source "(defun inc (x) (+ x 1))")
      (%write-test-source output "binary")
      (let ((state (cl-cc/pipeline:prepare-incremental-compilation source output :language :lisp)))
        (assert-true (cl-cc/pipeline:incremental-state-dirty-p state))
        (cl-cc/pipeline:commit-incremental-compilation state))
      (let ((state (cl-cc/pipeline:prepare-incremental-compilation source output :language :lisp)))
        (assert-false (cl-cc/pipeline:incremental-state-dirty-p state))
        (assert-eq :unchanged (cl-cc/pipeline:incremental-state-reason state)))
      (ignore-errors (delete-file source))
      (ignore-errors (delete-file output)))))

(deftest pipeline-incremental-macro-dependency-dirties-user
  "FR-640 marks macro users dirty when the defining file hash changes."
  (uiop:with-temporary-file (:pathname macro-source :type "lisp" :keep t)
    (uiop:with-temporary-file (:pathname user-source :type "lisp" :keep t)
      (let ((macro-output (make-pathname :type "bin" :defaults macro-source))
            (user-output (make-pathname :type "bin" :defaults user-source)))
        (%write-test-source macro-source "(defmacro twice (x) `(+ ,x ,x))")
        (%write-test-source user-source "(twice 21)")
        (%write-test-source macro-output "binary")
        (%write-test-source user-output "binary")
        (cl-cc/pipeline:commit-incremental-compilation
         (cl-cc/pipeline:prepare-incremental-compilation macro-source macro-output :language :lisp))
        (let ((state (cl-cc/pipeline:prepare-incremental-compilation user-source user-output :language :lisp)))
          (assert-true (cl-cc/pipeline:incremental-state-dirty-p state))
          (cl-cc/pipeline:commit-incremental-compilation state))
        (let ((state (cl-cc/pipeline:prepare-incremental-compilation user-source user-output :language :lisp)))
          (assert-false (cl-cc/pipeline:incremental-state-dirty-p state)))
        (%write-test-source macro-source "(defmacro twice (x) `(* 2 ,x))")
        (let ((state (cl-cc/pipeline:prepare-incremental-compilation user-source user-output :language :lisp)))
          (assert-true (cl-cc/pipeline:incremental-state-dirty-p state))
          (assert-eq :dependency-changed (cl-cc/pipeline:incremental-state-reason state)))
        (mapc (lambda (p) (ignore-errors (delete-file p)))
              (list macro-source user-source macro-output user-output))))))

(deftest pipeline-hot-reload-entry-swaps-after-quiescence
  "FR-641 swaps hot-reload entries only after active executions drain."
  (let* ((entry (cl-cc/pipeline:make-hot-reload-entry 'f (lambda () :old)))
         (done nil))
    (sb-thread:with-mutex ((cl-cc/pipeline::hot-reload-entry-lock entry))
      (setf (cl-cc/pipeline::hot-reload-entry-active-count entry) 1))
    (let ((thread (sb-thread:make-thread
                   (lambda ()
                     (cl-cc/pipeline:hot-reload-swap entry (lambda () :new))
                     (setf done t)))))
      (cl:sleep 0.02)
      (assert-false done)
      (sb-thread:with-mutex ((cl-cc/pipeline::hot-reload-entry-lock entry))
        (setf (cl-cc/pipeline::hot-reload-entry-active-count entry) 0))
      (sb-thread:join-thread thread)
      (assert-true done)
      (assert-eq :new (cl-cc/pipeline:hot-reload-call entry)))))

(deftest pipeline-parallel-respects-dependency-waves
  "FR-632 compiles dependency-free files in worker waves before dependents."
  (uiop:with-temporary-file (:pathname a :type "lisp" :keep t)
    (uiop:with-temporary-file (:pathname b :type "lisp" :keep t)
      (%write-test-source a "(defmacro m () 1)")
      (%write-test-source b "(m)")
      (cl-cc/pipeline::%write-deps-file b b (list (namestring (truename a))))
      (let ((order nil))
        (cl-cc/pipeline:compile-files-parallel
         (list a b)
         :workers 2
         :compile-function (lambda (file &rest args)
                             (declare (ignore args))
                             (push (pathname-name file) order)
                             file))
        (assert-equal (list a b) (reverse order)))
      (ignore-errors (delete-file a))
      (ignore-errors (delete-file b)))))
