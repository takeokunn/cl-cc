;;;; packages/cli/tests/plugin-tests.lisp — FR-720 plugin architecture tests

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(defmacro with-clean-plugin-registries (&body body)
  `(let ((cl-cc/cli:*plugin-directory* cl-cc/cli:*plugin-directory*))
     (clrhash cl-cc/cli:*loaded-plugins*)
     (clrhash cl-cc/cli:*repl-command-extensions*)
     (clrhash cl-cc/cli:*compiler-pass-extensions*)
     (clrhash cl-cc/cli:*vm-instruction-extensions*)
     (clrhash cl-cc/cli::*loaded-plugin-files*)
     ,@body))

(defun %plugin-test-handler (&rest args)
  args)

(defvar *plugin-test-command-line* nil)

(defun %plugin-test-command-handler (line)
  (setf *plugin-test-command-line* line))

(deftest define-cl-cc-plugin-registers-extension-points
  "define-cl-cc-plugin registers REPL, compiler pass, and VM instruction extension points."
  (with-clean-plugin-registries
    (cl-cc/cli:define-cl-cc-plugin fr-720-test
      :description "test plugin"
      :version "1.0"
      :repl-commands ((":hello" #'%plugin-test-command-handler :documentation "hello command"))
      :compiler-passes ((:after-parse #'%plugin-test-handler :phase :after-parse))
      :vm-instructions ((:plugin-op 'plugin-instruction :documentation "plugin op")))
    (assert-equal "test plugin"
                  (getf (gethash "fr-720-test" cl-cc/cli:*loaded-plugins*) :description))
    (assert-equal 'fr-720-test
                  (getf (cl-cc/cli:registered-repl-command ":hello") :plugin))
    (setf *plugin-test-command-line* nil)
    (assert-true (cl-cc/cli:run-repl-command-extension ":hello world"))
    (assert-equal ":hello world" *plugin-test-command-line*)
    (assert-equal :after-parse
                  (getf (first (cl-cc/cli:registered-compiler-passes :after-parse)) :name))
    (assert-equal 'plugin-instruction
                  (getf (cl-cc/cli:registered-vm-instruction :plugin-op) :definition))))

(deftest load-cl-cc-plugins-autoloads-lisp-files
  "load-cl-cc-plugins loads .lisp plugin files from the plugin directory."
  (with-clean-plugin-registries
    (let* ((dir (merge-pathnames
                 (format nil "cl-cc-plugin-test-~D/" (random 1000000000))
                 (uiop:temporary-directory)))
           (file (merge-pathnames "autoload-plugin.lisp" dir)))
      (ensure-directories-exist file)
      (unwind-protect
           (progn
             (with-open-file (stream file :direction :output :if-exists :supersede)
               (format stream "(cl-cc/cli:define-cl-cc-plugin autoloaded-plugin~%")
               (format stream "  :repl-commands ((\"auto\" #'cl:list)))~%"))
             (let ((cl-cc/cli:*plugin-directory* dir))
               (assert-equal (list file) (cl-cc/cli:load-cl-cc-plugins :force t))
               (assert-true (gethash "autoloaded-plugin" cl-cc/cli:*loaded-plugins*))
               (assert-equal 'autoloaded-plugin
                             (getf (cl-cc/cli:registered-repl-command "auto") :plugin))))
        (ignore-errors (delete-file file))
        (ignore-errors (uiop:delete-directory-tree dir :validate t))))))
