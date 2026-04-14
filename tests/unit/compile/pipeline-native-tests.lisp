;;;; tests/unit/compile/pipeline-native-tests.lisp — Pipeline Native tests
;;;;
;;;; Tests for pipeline-native.lisp: *compile-cache-root*, %compile-cache-key,
;;;; %compile-cache-path, %copy-file-bytes, and typeclass macro registration.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── *compile-cache-root* ───────────────────────────────────────────────────

(deftest pipeline-native-cache-root-is-pathname
  "*compile-cache-root* is a pathname."
  (assert-true (pathnamep cl-cc::*compile-cache-root*)))

(deftest pipeline-native-cache-root-contains-expected-path
  "*compile-cache-root* namestring contains .cache/cl-cc/native/."
  (let ((str (namestring cl-cc::*compile-cache-root*)))
    (assert-true (stringp str))
    (assert-true (search ".cache/cl-cc/native/" str))))

;;; ─── %compile-cache-key ─────────────────────────────────────────────────────

(deftest pipeline-native-cache-key-returns-string
  "%compile-cache-key returns a string."
  (assert-true (stringp (cl-cc::%compile-cache-key "hello" :x86-64 :lisp))))

(deftest-each pipeline-native-cache-key-contains-components
  "%compile-cache-key result contains the arch and language substrings (uppercased by ~A)."
  :cases (("x86-64-lisp"  :x86-64 :lisp  "X86-64" "LISP")
          ("arm64-lisp"   :arm64  :lisp  "ARM64"  "LISP")
          ("x86-64-php"   :x86-64 :php   "X86-64" "PHP"))
  (arch lang arch-str lang-str)
  (let ((key (cl-cc::%compile-cache-key "(+ 1 2)" arch lang)))
    (assert-true (search arch-str key))
    (assert-true (search lang-str key))))

(deftest pipeline-native-cache-key-deterministic
  "%compile-cache-key: same source + arch + language always yields the same key."
  (let ((k1 (cl-cc::%compile-cache-key "(defun f (x) x)" :x86-64 :lisp))
        (k2 (cl-cc::%compile-cache-key "(defun f (x) x)" :x86-64 :lisp)))
    (assert-equal k1 k2)))

(deftest pipeline-native-cache-key-differs-by-arch
  "%compile-cache-key: different arch yields different keys."
  (let ((k1 (cl-cc::%compile-cache-key "source" :x86-64 :lisp))
        (k2 (cl-cc::%compile-cache-key "source" :arm64  :lisp)))
    (assert-false (equal k1 k2))))

(deftest pipeline-native-cache-key-differs-by-language
  "%compile-cache-key: different language yields different keys."
  (let ((k1 (cl-cc::%compile-cache-key "source" :x86-64 :lisp))
        (k2 (cl-cc::%compile-cache-key "source" :x86-64 :php)))
    (assert-false (equal k1 k2))))

(deftest pipeline-native-cache-key-differs-by-source
  "%compile-cache-key: different source content yields different keys."
  (let ((k1 (cl-cc::%compile-cache-key "aaa" :x86-64 :lisp))
        (k2 (cl-cc::%compile-cache-key "bbb" :x86-64 :lisp)))
    (assert-false (equal k1 k2))))

(deftest pipeline-native-cache-key-dash-separated
  "%compile-cache-key contains dashes separating hash, arch, and language."
  (let* ((key (cl-cc::%compile-cache-key "test" :x86-64 :lisp))
         (parts (cl:loop for start = 0 then (1+ pos)
                         for pos = (position #\- key :start start)
                         collect (subseq key start (or pos (length key)))
                         while pos)))
    ;; At minimum 3 parts: hash, arch, language
    (assert-true (>= (length parts) 3))))

;;; ─── %compile-cache-path ────────────────────────────────────────────────────

(deftest pipeline-native-cache-path-returns-pathname
  "%compile-cache-path returns a pathname."
  (let ((path (cl-cc::%compile-cache-path "abc123" #P"a.out")))
    (assert-true (pathnamep path))))

(deftest pipeline-native-cache-path-is-below-cache-root
  "%compile-cache-path result is rooted under *compile-cache-root*."
  (let* ((key "mykey")
         (path (cl-cc::%compile-cache-path key #P"a.out"))
         (root-str (namestring cl-cc::*compile-cache-root*))
         (path-str (namestring path)))
    (assert-true (and (> (length path-str) (length root-str))
                      (string= root-str
                                (subseq path-str 0 (length root-str)))))))

(deftest pipeline-native-cache-path-preserves-filename
  "%compile-cache-path preserves the filename from the output-file argument."
  (let ((path (cl-cc::%compile-cache-path "somekey" #P"my-program.out")))
    (assert-equal "my-program" (pathname-name path))
    (assert-equal "out"        (pathname-type path))))

(deftest pipeline-native-cache-path-no-extension
  "%compile-cache-path works with output files that have no extension."
  (let ((path (cl-cc::%compile-cache-path "k" #P"a.out")))
    (assert-true (pathnamep path))
    (assert-equal "a" (pathname-name path))))

(deftest pipeline-native-cache-path-includes-key
  "%compile-cache-path embeds the key in the directory structure."
  (let* ((key "unique-cache-key-42")
         (path (cl-cc::%compile-cache-path key #P"a.out"))
         (path-str (namestring path)))
    (assert-true (search key path-str))))

(deftest pipeline-native-cache-path-different-keys-differ
  "%compile-cache-path: different keys produce different pathnames."
  (let ((p1 (cl-cc::%compile-cache-path "key-one" #P"a.out"))
        (p2 (cl-cc::%compile-cache-path "key-two" #P"a.out")))
    (assert-false (equal (namestring p1) (namestring p2)))))

;;; ─── %copy-file-bytes ───────────────────────────────────────────────────────

(deftest pipeline-native-copy-file-bytes-returns-to-path
  "%copy-file-bytes returns the destination pathname."
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
        (ignore-errors (delete-file dst))))))

(deftest pipeline-native-copy-file-bytes-destination-exists
  "%copy-file-bytes creates the destination file."
  (uiop:with-temporary-file (:pathname src :type "bin")
    (uiop:with-temporary-file (:pathname dst :type "bin" :keep t)
      (let ((data (make-array 3 :element-type '(unsigned-byte 8)
                               :initial-contents '(10 20 30))))
        (with-open-file (out src :direction :output
                                 :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
          (write-sequence data out))
        (cl-cc::%copy-file-bytes src dst)
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

(deftest pipeline-native-deftype-class-registered
  "deftype-class is registered in the global macro environment."
  (assert-true (gethash 'cl-cc::deftype-class
                         (cl-cc::macro-env-table cl-cc::*macro-environment*))))

(deftest pipeline-native-deftype-instance-registered
  "deftype-instance is registered in the global macro environment."
  (assert-true (gethash 'cl-cc::deftype-instance
                         (cl-cc::macro-env-table cl-cc::*macro-environment*))))

(deftest pipeline-native-deftype-class-is-function
  "The deftype-class macro expander is a function."
  (assert-true (functionp
                (gethash 'cl-cc::deftype-class
                          (cl-cc::macro-env-table cl-cc::*macro-environment*)))))

(deftest pipeline-native-deftype-instance-is-function
  "The deftype-instance macro expander is a function."
  (assert-true (functionp
                (gethash 'cl-cc::deftype-instance
                          (cl-cc::macro-env-table cl-cc::*macro-environment*)))))
