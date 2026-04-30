;;;; tests/unit/compile/pipeline-native-io-tests.lisp — Pipeline Native I/O + Integration
;;;;
;;;; Tests for %copy-file-bytes, compile-file-to-native cache hits, typeclass
;;;; macro registration, and %cps-native-compile-safe-ast-p.
;;;; Suite: pipeline-native-suite (defined in pipeline-native-tests.lisp).

(in-package :cl-cc/test)

(in-suite pipeline-native-suite)

(defmacro with-native-cache-stubs ((&key cache-path) &body body)
  "Stub cache-related native helper calls for routing/cache tests."
  `(with-replaced-function (cl-cc::%compile-cache-key
                            (lambda (&rest args)
                              (declare (ignore args))
                              "cache-key"))
     (with-replaced-function (cl-cc::%compile-cache-path
                              (lambda (&rest args)
                                (declare (ignore args))
                                ,cache-path))
       (with-replaced-function (cl-cc::%copy-file-bytes
                                (lambda (from to)
                                  (declare (ignore from))
                                  to))
         ,@body))))

;;; ─── %copy-file-bytes ───────────────────────────────────────────────────────

(deftest pipeline-native-copy-file-bytes-returns-dst-and-creates-file
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
  (uiop:with-temporary-file (:pathname src :type "bin")
    (uiop:with-temporary-file (:pathname dst :type "bin" :keep t)
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

(deftest-each pipeline-native-typeclass-macros-registered-as-expanders
  "deftype-class and deftype-instance are each registered as invokable macro expanders."
  :cases (("deftype-class"    'cl-cc::deftype-class)
          ("deftype-instance" 'cl-cc::deftype-instance))
  (macro-name)
  (let ((expander (gethash macro-name
                            (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*))))
    (assert-true expander)
    (assert-true (or (functionp expander)
                     (eq (getf expander :kind) :macro-expander)
                     (eq (getf expander :kind) :register-macro-expander)))))

(deftest pipeline-native-typeclass-class-expander-builds-register-form
  "deftype-class expander produces a register-typeclass form backed by make-typeclass-def data."
  (let* ((expander (gethash 'cl-cc::deftype-class
                            (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
         (expanded (cl-cc/expand::invoke-registered-expander
                    expander
                    '(deftype-class eq-like (a)
                       (equals (-> a a bool)))
                    nil)))
    (assert-eq 'progn (car expanded))
    (assert-true (search "REGISTER-TYPECLASS" (prin1-to-string expanded)))
    (assert-true (search "MAKE-TYPECLASS-DEF" (prin1-to-string expanded)))))

(deftest pipeline-native-typeclass-instance-expander-builds-register-form
  "deftype-instance expander produces register-typeclass-instance plus a dictionary defvar."
  (let* ((expander (gethash 'cl-cc::deftype-instance
                            (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
         (expanded (cl-cc/expand::invoke-registered-expander
                    expander
                    '(deftype-instance eq-like integer
                       (equals (lambda (x y) (= x y))))
                    nil)))
    (assert-eq 'progn (car expanded))
    (assert-true (search "REGISTER-TYPECLASS-INSTANCE" (prin1-to-string expanded)))
    (assert-true (search "DEFVAR" (prin1-to-string expanded)))))

;;; ─── compile-file-to-native cache hit ──────────────────────────────────────

(deftest pipeline-native-compile-file-cache-hit-copies-artifact
  "compile-file-to-native reuses a cached native artifact when present."
  (uiop:with-temporary-file (:pathname input :type "php" :keep t)
    (uiop:with-temporary-file (:pathname output :type "bin" :keep t)
      (uiop:with-temporary-file (:pathname cache :type "bin" :keep t)
        (let ((copied nil)
              (chmod-called nil))
          (with-open-file (stream input :direction :output :if-exists :supersede)
            (write-line "<?php echo 1;" stream))
          (with-native-cache-stubs (:cache-path cache)
            (with-replaced-function (cl-cc::%copy-file-bytes
                                     (lambda (from to)
                                       (setf copied (list from to))
                                       to))
              (with-replaced-function (cl-cc::%run-short-native-command
                                       (lambda (&rest args)
                                         (declare (ignore args))
                                         (setf chmod-called t)
                                         nil))
                (assert-equal output
                              (cl-cc::compile-file-to-native input :output-file output))
                (assert-equal (list cache output) copied)
                (assert-true chmod-called))))
        (ignore-errors (delete-file cache)))
      (ignore-errors (delete-file output)))
    (ignore-errors (delete-file input)))))

;;; ─── CPS-safe AST allowlist ─────────────────────────────────────────────────

(deftest pipeline-native-cps-safe-ast-p-rejects-io-and-mv-forms-until-native-cps-lowering-exists
  "%cps-native-compile-safe-ast-p rejects call and multiple-value forms while native CPS lowering is disabled."
  (let ((safe-ast (cl-cc:make-ast-call :func 'f :args (list (cl-cc:make-ast-int :value 1))))
        (mv-ast (cl-cc::make-ast-multiple-value-prog1
                 :first (cl-cc:make-ast-int :value 1)
                 :forms (list (cl-cc:make-ast-int :value 2))))
        (unsafe-ast (cl-cc/ast::make-ast-make-instance
                     :class (cl-cc:make-ast-quote :value 'point)
                     :initargs nil)))
    (assert-false (cl-cc::%cps-native-compile-safe-ast-p safe-ast))
    (assert-false (cl-cc::%cps-native-compile-safe-ast-p mv-ast))
    (assert-false (cl-cc::%cps-native-compile-safe-ast-p unsafe-ast))))
