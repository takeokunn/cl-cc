(in-package :cl-cc/test)

(in-suite pipeline-native-suite)

(deftest pipeline-native-cps-safe-ast-p-allowlist
  "%cps-native-compile-safe-ast-p accepts call-bearing CPS-backed forms and rejects unsupported object forms."
  (let ((safe-ast (cl-cc:make-ast-let
                   :bindings (list (cons 'x (cl-cc:make-ast-int :value 1)))
                   :body (list (cl-cc:make-ast-binop
                                :op '+
                                :lhs (cl-cc:make-ast-var :name 'x)
                                :rhs (cl-cc:make-ast-int :value 2)))))
        (call-ast (cl-cc:make-ast-call
                   :func 'f
                   :args (list (cl-cc:make-ast-int :value 1))))
        (unsafe-ast (cl-cc/ast::make-ast-make-instance
                     :class (cl-cc:make-ast-quote :value 'point)
                     :initargs nil)))
    (assert-true (cl-cc::%cps-native-compile-safe-ast-p safe-ast))
    (assert-true (cl-cc::%cps-native-compile-safe-ast-p call-ast))
    (assert-false (cl-cc::%cps-native-compile-safe-ast-p unsafe-ast))))

(deftest pipeline-native-maybe-compile-via-cps-wraps-identity-continuation
  "%maybe-compile-native-via-cps compiles the CPS wrapper form for safe expressions."
  (let ((compiled-form nil))
    (with-replaced-function (cl-cc:compile-expression
                             (lambda (form &rest args)
                               (declare (ignore args))
                               (setf compiled-form form)
                               (cl-cc/compile::make-compilation-result :program :dummy)))
      (multiple-value-bind (result used-cps)
          (cl-cc::%maybe-compile-native-via-cps '(+ 1 2) :target :x86_64)
        (assert-true used-cps)
        (assert-eq :dummy (cl-cc/compile:compilation-result-program result))
        (assert-true (consp compiled-form))
        (assert-true (consp (first compiled-form)))
        (assert-eq 'lambda (car (first compiled-form)))
        (assert-eq 'lambda (car (second compiled-form)))
        (assert-true (= 1 (length (second (second compiled-form)))))
        (assert-eq (first (second (second compiled-form)))
                   (third (second compiled-form)))))))

(deftest pipeline-native-compile-to-native-string-single-form-prefers-cps-path
  "compile-to-native routes single safe Lisp strings through the CPS-native helper before compile-string fallback."
  (let ((helper-called nil)
        (compile-string-called nil))
    (with-replaced-function (cl-cc::%maybe-compile-native-via-cps
                             (lambda (form &rest args)
                               (declare (ignore form args))
                               (setf helper-called t)
                               (values (cl-cc/compile::make-compilation-result :program :dummy) t)))
      (with-replaced-function (cl-cc:compile-string
                               (lambda (&rest args)
                                 (declare (ignore args))
                                 (setf compile-string-called t)
                                 (cl-cc/compile::make-compilation-result :program :fallback)))
        (with-replaced-function (cl-cc:compile-to-x86-64-bytes
                                 (lambda (program)
                                   (declare (ignore program))
                                   #(1 2 3)))
          (with-replaced-function (cl-cc/binary:make-mach-o-builder
                                   (lambda (&rest args)
                                     (declare (ignore args))
                                     :builder))
            (with-replaced-function (cl-cc::%write-native-binary
                                     (lambda (builder code-bytes output-path)
                                       (declare (ignore builder code-bytes))
                                       output-path))
              (assert-equal #P"out.bin"
                            (cl-cc::compile-to-native "(+ 1 2)"
                                                      :output-file #P"out.bin"
                                                      :language :lisp))
              (assert-true helper-called)
              (assert-false compile-string-called))))))))

(deftest pipeline-native-compile-file-single-safe-form-prefers-cps-path
  "compile-file-to-native routes a single safe Lisp top-level form through the CPS-native helper, ignoring in-package forms."
  (uiop:with-temporary-file (:pathname input :type "lisp" :keep t)
    (let ((helper-form nil)
          (compile-toplevel-called nil))
      (with-open-file (stream input :direction :output :if-exists :supersede)
        (write-line "(in-package :cl-user)" stream)
        (write-line "(+ 1 2)" stream))
      (with-replaced-function (cl-cc::%maybe-compile-native-via-cps
                               (lambda (form &rest args)
                                 (declare (ignore args))
                                 (setf helper-form form)
                                 (values (cl-cc/compile::make-compilation-result :program :dummy) t)))
        (with-replaced-function (cl-cc/compile:compile-toplevel-forms
                                 (lambda (&rest args)
                                   (declare (ignore args))
                                   (setf compile-toplevel-called t)
                                   (cl-cc/compile::make-compilation-result :program :fallback)))
          (with-replaced-function (cl-cc:compile-to-x86-64-bytes
                                   (lambda (program)
                                     (declare (ignore program))
                                     #(1 2 3)))
            (with-replaced-function (cl-cc/binary:make-mach-o-builder
                                     (lambda (&rest args)
                                       (declare (ignore args))
                                       :builder))
              (with-replaced-function (cl-cc::%compile-cache-path
                                       (lambda (&rest args)
                                         (declare (ignore args))
                                         #P"./tmp-native-cache.bin"))
                (with-replaced-function (cl-cc::%copy-file-bytes
                                         (lambda (from to)
                                           (declare (ignore from))
                                           to))
                  (with-replaced-function (cl-cc::%write-native-binary
                                           (lambda (builder code-bytes output-path)
                                             (declare (ignore builder code-bytes))
                                             output-path))
                    (assert-equal #P"out.bin"
                                  (cl-cc::compile-file-to-native input :output-file #P"out.bin" :language :lisp))
                    (assert-equal '(+ 1 2) helper-form)
                    (assert-false compile-toplevel-called))))))))
      (ignore-errors (delete-file input)))))

(deftest pipeline-native-compile-file-multi-form-falls-back-to-toplevel
  "compile-file-to-native keeps multi-form Lisp files on the existing compile-toplevel-forms path."
  (uiop:with-temporary-file (:pathname input :type "lisp" :keep t)
    (let ((helper-called nil)
          (compile-toplevel-called nil))
      (with-open-file (stream input :direction :output :if-exists :supersede)
        (write-line "(+ 1 2)" stream)
        (write-line "(+ 3 4)" stream))
      (with-replaced-function (cl-cc::%maybe-compile-native-via-cps
                               (lambda (&rest args)
                                 (declare (ignore args))
                                 (setf helper-called t)
                                 (values (cl-cc/compile::make-compilation-result :program :dummy) t)))
        (with-replaced-function (cl-cc/compile:compile-toplevel-forms
                                 (lambda (&rest args)
                                   (declare (ignore args))
                                   (setf compile-toplevel-called t)
                                   (cl-cc/compile::make-compilation-result :program :fallback)))
          (with-replaced-function (cl-cc:compile-to-x86-64-bytes
                                   (lambda (program)
                                     (declare (ignore program))
                                     #(1 2 3)))
            (with-replaced-function (cl-cc/binary:make-mach-o-builder
                                     (lambda (&rest args)
                                       (declare (ignore args))
                                       :builder))
              (with-replaced-function (cl-cc::%compile-cache-path
                                       (lambda (&rest args)
                                         (declare (ignore args))
                                         #P"./tmp-native-cache.bin"))
                (with-replaced-function (cl-cc::%copy-file-bytes
                                         (lambda (from to)
                                           (declare (ignore from))
                                           to))
                  (with-replaced-function (cl-cc::%write-native-binary
                                           (lambda (builder code-bytes output-path)
                                             (declare (ignore builder code-bytes))
                                             output-path))
                    (assert-equal #P"out.bin"
                                  (cl-cc::compile-file-to-native input :output-file #P"out.bin" :language :lisp))
                    (assert-false helper-called)
                    (assert-true compile-toplevel-called))))))))
      (ignore-errors (delete-file input)))))
