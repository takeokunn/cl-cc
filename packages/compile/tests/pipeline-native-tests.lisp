;;;; tests/unit/compile/pipeline-native-tests.lisp — Pipeline Native tests
;;;;
;;;; Tests for pipeline-native.lisp: *compile-cache-root*, %compile-cache-key,
;;;; %compile-cache-path, and %make-native-opts.
;;;; File I/O + typeclass + integration → pipeline-native-io-tests.lisp.

(in-package :cl-cc/test)

(defsuite pipeline-native-suite
  :description "Serial tests for native pipeline helpers that temporarily replace global functions."
  :parent cl-cc-unit-suite
  :parallel nil)

(in-suite pipeline-native-suite)

;;; ─── %make-native-opts ──────────────────────────────────────────────────────

(deftest pipeline-native-make-opts-defaults
  "%make-native-opts returns a plist with :opt-remarks-mode defaulting to :all; other slots nil."
  (let ((opts (cl-cc::%make-native-opts)))
    (assert-true (listp opts))
    (assert-null (getf opts :pass-pipeline))
    (assert-null (getf opts :print-pass-timings))
    (assert-null (getf opts :timing-stream))
    (assert-eq :all (getf opts :opt-remarks-mode))
    (assert-null (getf opts :trace-json-stream))))

(deftest pipeline-native-make-opts-explicit-values
  "%make-native-opts captures explicit keyword values into the plist."
  (let ((opts (cl-cc::%make-native-opts :pass-pipeline '(:fold :dce)
                                        :print-pass-timings t
                                        :opt-remarks-mode :pass)))
    (assert-equal '(:fold :dce) (getf opts :pass-pipeline))
    (assert-true (getf opts :print-pass-timings))
    (assert-eq :pass (getf opts :opt-remarks-mode))))

(deftest pipeline-native-make-opts-applyable-to-compile-expression
  "%make-native-opts plist can be APPLYed directly as compile-expression keyword args."
  (let ((captured-args nil))
    (with-replaced-function (cl-cc:compile-expression
                             (lambda (form &rest args)
                               (declare (ignore form))
                               (setf captured-args args)
                               (cl-cc/compile:make-compilation-result :program nil)))
      (let ((opts (cl-cc::%make-native-opts :pass-pipeline '(:fold))))
        (apply #'cl-cc:compile-expression '(+ 1 2) :target :x86_64 opts)
        (assert-equal '(:fold) (getf captured-args :pass-pipeline))))))

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

(deftest pipeline-native-cache-path-rooted-under-cache-root
  "%compile-cache-path returns a pathname rooted under *compile-cache-root*."
  (let* ((path     (cl-cc::%compile-cache-path "mykey" #P"a.out"))
         (root-str (namestring cl-cc::*compile-cache-root*))
         (path-str (namestring path)))
    (assert-true (pathnamep path))
    (assert-true (and (> (length path-str) (length root-str))
                      (string= root-str (subseq path-str 0 (length root-str)))))))

(deftest pipeline-native-cache-path-embeds-key-in-path
  "%compile-cache-path embeds the cache key string inside the returned path."
  (let* ((key "unique-cache-key-42")
         (path (cl-cc::%compile-cache-path key #P"a.out")))
    (assert-true (search key (namestring path)))))

(deftest pipeline-native-cache-path-different-keys-differ
  "%compile-cache-path returns distinct paths for distinct keys."
  (let ((p1 (cl-cc::%compile-cache-path "key-one" #P"a.out"))
        (p2 (cl-cc::%compile-cache-path "key-two" #P"a.out")))
    (assert-false (equal (namestring p1) (namestring p2)))))

(deftest pipeline-native-cache-path-preserves-output-filename
  "%compile-cache-path preserves the filename from the output-file argument."
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
