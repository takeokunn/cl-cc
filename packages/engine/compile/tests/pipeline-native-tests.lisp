;;;; tests/unit/compile/pipeline-native-tests.lisp — Pipeline Native tests
;;;;
;;;; Tests for pipeline-native.lisp: *compile-cache-root*, %compile-cache-key,
;;;; %compile-cache-path, %copy-file-bytes, and typeclass macro registration.

(in-package :cl-cc/test)

(defsuite pipeline-native-suite
  :description "Serial tests for native pipeline helpers that temporarily replace global functions."
  :parent cl-cc-unit-suite
  :parallel nil)

(in-suite pipeline-native-suite)

(defmacro with-replaced-function ((name replacement) &body body)
  (let ((old (gensym "OLD-FN"))
        (had (gensym "HAD-FN")))
    `(let ((,had (fboundp ',name))
           (,old (ignore-errors (symbol-function ',name))))
       (unwind-protect
            (progn
              (setf (symbol-function ',name) ,replacement)
              ,@body)
         (if ,had
             (setf (symbol-function ',name) ,old)
             (fmakunbound ',name))))))

;;; ─── *compile-cache-root* ───────────────────────────────────────────────────

(deftest pipeline-native-cache-root
  "*compile-cache-root* is a pathname whose namestring contains .cache/cl-cc/native/."
  (let ((str (namestring cl-cc::*compile-cache-root*)))
    (assert-true (pathnamep cl-cc::*compile-cache-root*))
    (assert-true (stringp str))
    (assert-true (search ".cache/cl-cc/native/" str))))

;;; ─── %compile-cache-key ─────────────────────────────────────────────────────

(deftest-each pipeline-native-cache-key-contains-components
  "%compile-cache-key result contains the arch and language substrings (uppercased by ~A)."
  :cases (("x86-64-lisp"  :x86-64 :lisp  "X86-64" "LISP")
          ("arm64-lisp"   :arm64  :lisp  "ARM64"  "LISP")
          ("x86-64-php"   :x86-64 :php   "X86-64" "PHP"))
  (arch lang arch-str lang-str)
  (let ((key (cl-cc::%compile-cache-key "(+ 1 2)" arch lang)))
    (assert-true (search arch-str key))
    (assert-true (search lang-str key))))

(deftest pipeline-native-cache-key-string-properties
  "%compile-cache-key returns a string; same args are deterministic; result has >= 3 dash-separated parts."
  (assert-true (stringp (cl-cc::%compile-cache-key "hello" :x86-64 :lisp)))
  (let ((k1 (cl-cc::%compile-cache-key "(defun f (x) x)" :x86-64 :lisp))
        (k2 (cl-cc::%compile-cache-key "(defun f (x) x)" :x86-64 :lisp)))
    (assert-equal k1 k2))
  (let* ((key (cl-cc::%compile-cache-key "test" :x86-64 :lisp))
         (parts (cl:loop for start = 0 then (1+ pos)
                         for pos = (position #\- key :start start)
                         collect (subseq key start (or pos (length key)))
                         while pos)))
    (assert-true (>= (length parts) 3))))

(deftest-each pipeline-native-cache-key-differs-by-dimension
  "%compile-cache-key: varying arch, language, or source content each produces a distinct key."
  :cases (("arch"     (lambda () (cl-cc::%compile-cache-key "source" :arm64  :lisp)))
          ("language" (lambda () (cl-cc::%compile-cache-key "source" :x86-64 :php)))
          ("content"  (lambda () (cl-cc::%compile-cache-key "bbb"    :x86-64 :lisp))))
  (make-variant)
  (let ((baseline (cl-cc::%compile-cache-key "source" :x86-64 :lisp)))
    (assert-false (equal baseline (funcall make-variant)))))

;;; ─── %compile-cache-path ────────────────────────────────────────────────────

(deftest pipeline-native-cache-path-cases
  "%compile-cache-path: rooted under *compile-cache-root*; embeds key; different keys differ; preserves name."
  (let* ((path     (cl-cc::%compile-cache-path "mykey" #P"a.out"))
         (root-str (namestring cl-cc::*compile-cache-root*))
         (path-str (namestring path)))
    (assert-true (pathnamep path))
    (assert-true (and (> (length path-str) (length root-str))
                      (string= root-str (subseq path-str 0 (length root-str))))))
  (let* ((key "unique-cache-key-42")
         (path (cl-cc::%compile-cache-path key #P"a.out")))
    (assert-true (search key (namestring path))))
  (let ((p1 (cl-cc::%compile-cache-path "key-one" #P"a.out"))
        (p2 (cl-cc::%compile-cache-path "key-two" #P"a.out")))
    (assert-false (equal (namestring p1) (namestring p2))))
  (let ((path (cl-cc::%compile-cache-path "k" #P"a.out")))
    (assert-true (pathnamep path))
    (assert-equal "a" (pathname-name path))))

(deftest-each pipeline-native-cache-path-filename-components
  "%compile-cache-path preserves the filename and extension from the output-file argument."
  :cases (("filename"  "my-program" (lambda (p) (pathname-name p)))
          ("extension" "out"        (lambda (p) (pathname-type p))))
  (expected accessor)
  (let ((path (cl-cc::%compile-cache-path "somekey" #P"my-program.out")))
    (assert-equal expected (funcall accessor path))))

;;; ─── %copy-file-bytes ───────────────────────────────────────────────────────

(deftest pipeline-native-copy-file-bytes-basic-cases
  "%copy-file-bytes returns destination pathname and creates destination file."
  (uiop:with-temporary-file (:pathname src :type "bin")
    (uiop:with-temporary-file (:pathname dst :type "bin" :keep t)
      (let* ((data (make-array 4 :element-type '(unsigned-byte 8)
                                :initial-contents '(1 2 3 4)))
             (_ (with-open-file (out src :direction :output
                                        :if-exists :supersede
                                        :element-type '(unsigned-byte 8))
                  (write-sequence data out)))
             (result (cl-cc::%copy-file-bytes src dst)))
        (declare (ignore _))
        (assert-true (pathnamep result))
        (assert-equal (namestring dst) (namestring result))
        (assert-true (probe-file dst))
        (ignore-errors (delete-file dst))))))

(deftest pipeline-native-copy-file-bytes-same-contents
  "%copy-file-bytes produces a destination file with identical contents."
  (uiop:with-temporary-file (:pathname src :type "bin")
    (uiop:with-temporary-file (:pathname dst :type "bin" :keep t)
      (let ((data (make-array 8 :element-type '(unsigned-byte 8)
                               :initial-contents '(0 1 2 3 255 128 64 32))))
        (with-open-file (out src :direction :output
                                 :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
          (write-sequence data out))
        (cl-cc::%copy-file-bytes src dst)
        (let ((read-back (make-array 8 :element-type '(unsigned-byte 8))))
          (with-open-file (in dst :direction :input
                                  :element-type '(unsigned-byte 8))
            (read-sequence read-back in))
          (assert-equal (coerce data 'list)
                        (coerce read-back 'list)))
        (ignore-errors (delete-file dst))))))

(deftest pipeline-native-copy-file-bytes-empty-file
  "%copy-file-bytes handles empty source files."
  (uiop:with-temporary-file (:pathname src :type "bin")
    (uiop:with-temporary-file (:pathname dst :type "bin" :keep t)
      ;; Write empty file
      (with-open-file (out src :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8)))
      (cl-cc::%copy-file-bytes src dst)
      (assert-true (probe-file dst))
      (assert-= 0 (with-open-file (in dst :direction :input
                                          :element-type '(unsigned-byte 8))
                    (file-length in)))
      (ignore-errors (delete-file dst)))))

(deftest pipeline-native-copy-file-bytes-large-buffer
  "%copy-file-bytes handles files larger than the 4096-byte internal buffer."
  ;; Write a large file by repeating a small chunk to cross the 4096-byte
  ;; buffer boundary inside %copy-file-bytes.  We use a text stream for
  ;; building the source, then copy it as bytes.
  (uiop:with-temporary-file (:pathname src :type "bin")
    (uiop:with-temporary-file (:pathname dst :type "bin" :keep t)
      ;; Write the source: fill with printable ASCII repeated until >4096 bytes.
      (let ((chunk "ABCDEFGHIJ"))
        (with-open-file (out src :direction :output
                                 :if-exists :supersede
                                 :element-type 'character)
          (loop repeat 450 do (write-string chunk out))))
      (let ((src-size (with-open-file (s src :element-type '(unsigned-byte 8))
                        (file-length s))))
        (assert-true (> src-size 4096))
        (cl-cc::%copy-file-bytes src dst)
        (let ((dst-size (with-open-file (s dst :element-type '(unsigned-byte 8))
                          (file-length s))))
          (assert-= src-size dst-size)))
      (ignore-errors (delete-file dst)))))

;;; ─── Typeclass macro registration ───────────────────────────────────────────

(deftest-each pipeline-native-typeclass-macros-registered-as-functions
  "deftype-class and deftype-instance are each registered as a function-valued macro."
  :cases (("deftype-class"    'cl-cc::deftype-class)
          ("deftype-instance" 'cl-cc::deftype-instance))
  (macro-name)
  (let ((expander (gethash macro-name
                            (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*))))
    (assert-true expander)
    (assert-true (functionp expander))))

(deftest pipeline-native-compile-file-cache-hit-copies-artifact
  "compile-file-to-native reuses a cached native artifact when present."
  (uiop:with-temporary-file (:pathname input :type "php" :keep t)
    (uiop:with-temporary-file (:pathname output :type "bin" :keep t)
      (uiop:with-temporary-file (:pathname cache :type "bin" :keep t)
        (let ((copied nil)
              (chmod-called nil))
          (with-open-file (stream input :direction :output :if-exists :supersede)
            (write-line "<?php echo 1;" stream))
          (with-replaced-function (cl-cc::%compile-cache-key (lambda (&rest args)
                                                               (declare (ignore args))
                                                               "cache-key"))
            (with-replaced-function (cl-cc::%compile-cache-path (lambda (key out)
                                                                  (declare (ignore key out))
                                                                  cache))
              (with-replaced-function (cl-cc::%copy-file-bytes
                                       (lambda (from to)
                                         (setf copied (list from to))
                                         to))
                (with-replaced-function (uiop:run-program
                                         (lambda (&rest args)
                                           (declare (ignore args))
                                           (setf chmod-called t)
                                           nil))
                  (assert-equal output
                                (cl-cc::compile-file-to-native input :output-file output))
                  (assert-equal (list cache output) copied)
                  (assert-true chmod-called))))))
        (ignore-errors (delete-file cache)))
      (ignore-errors (delete-file output)))
    (ignore-errors (delete-file input))))

(deftest pipeline-native-cps-safe-ast-p-allowlist
  "%cps-native-compile-safe-ast-p accepts the narrow semantic-test-backed subset and rejects calls."
  (let ((safe-ast (cl-cc:make-ast-let
                   :bindings (list (cons 'x (cl-cc:make-ast-int :value 1)))
                   :body (list (cl-cc:make-ast-binop
                                :op '+
                                :lhs (cl-cc:make-ast-var :name 'x)
                                :rhs (cl-cc:make-ast-int :value 2)))))
        (unsafe-ast (cl-cc:make-ast-call
                     :func 'f
                     :args (list (cl-cc:make-ast-int :value 1)))))
    (assert-true (cl-cc::%cps-native-compile-safe-ast-p safe-ast))
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
          (cl-cc::%maybe-compile-native-via-cps '(+ 1 2) :target :vm)
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
