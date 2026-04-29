;;;; tests/unit/cli/cli-tests.lisp — Edge-case tests for main.lisp utilities
;;;;
;;;; Covers:
;;;;   %detect-language — language auto-detection from extension / --lang flag
;;;;   %read-file       — file reading correctness incl. multibyte UTF-8 (bug fix)

(in-package :cl-cc/test)

(in-suite cl-cc-cli-serial-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Temp-file helper
;;; ─────────────────────────────────────────────────────────────────────────

(defmacro %with-temp-file ((path-var content) &body body)
  "Write CONTENT to a uniquely-named temp file, bind its namestring to PATH-VAR,
execute BODY, then delete the file.  The file is written as UTF-8 text."
  (let ((gpath (gensym "PATH")))
    `(let* ((,gpath (uiop:native-namestring
                     (make-pathname
                      :name (format nil "cl-cc-cli-test-~A-~A"
                                    (get-universal-time) (random 999999))
                      :type "tmp"
                      :defaults uiop:*temporary-directory*)))
            (,path-var ,gpath))
       (with-open-file (out ,gpath :direction :output
                                   :if-exists :supersede
                                   :element-type 'character
                                   :external-format :utf-8)
         (write-string ,content out))
       (unwind-protect
           (progn ,@body)
         (ignore-errors (delete-file ,gpath))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %detect-language — extension auto-detection
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cli-detect-language-by-extension
  "detect-language: auto-detects language from the file extension"
  :cases (("php ext"   "foo.php"  "" :php)
          ("lisp ext"  "foo.lisp" "" :lisp)
          ("cl ext"    "foo.cl"   "" :lisp)
          ("txt ext"   "foo.txt"  "" :lisp))
  (file lang-flag expected)
(assert-eq expected (cl-cc/cli::%detect-language file lang-flag)))

(deftest-each cli-detect-language-no-extension
  ;; Regression test: (pathname-type \"Makefile\") returns NIL;
  ;; the old code called (string= nil \"php\") which was a type error.
  "detect-language: file with no extension defaults to :lisp (not a type error)"
  :cases (("makefile" "Makefile")
          ("no-ext"   "foo")
          ("readme"   "README"))
  (file)
(assert-eq :lisp (cl-cc/cli::%detect-language file "")))

(deftest cli-detect-language-nil-file
  "detect-language: nil file path with no lang flag defaults to :lisp"
(assert-eq :lisp (cl-cc/cli::%detect-language nil "")))

(deftest-each cli-detect-language-case-sensitive
  "detect-language: extension matching is case-sensitive (.PHP ≠ .php), non-lowercase defaults to :lisp."
  :cases (("uppercase" "foo.PHP")
          ("mixed"     "foo.Php"))
  (file)
(assert-eq :lisp (cl-cc/cli::%detect-language file "")))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %detect-language — --lang flag overrides
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cli-detect-language-flag-overrides-extension
  "detect-language: explicit --lang flag wins over file extension"
  :cases (("php flag beats lisp ext"  "foo.lisp" "php"  :php)
          ("lisp flag beats php ext"  "foo.php"  "lisp" :lisp)
          ("php flag with no ext"     "Makefile" "php"  :php))
  (file lang-flag expected)
(assert-eq expected (cl-cc/cli::%detect-language file lang-flag)))

(deftest-each cli-detect-language-unknown-flag-falls-through
  "detect-language: unknown lang flag falls through to extension detection."
  :cases (("php-ext"  "foo.php"  :php)
          ("lisp-ext" "foo.lisp" :lisp)
          ("no-ext"   "foo"      :lisp))
  (file expected)
  ;; 'ruby' is not a known lang, so extension check runs next
(assert-eq expected (cl-cc/cli::%detect-language file "ruby")))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %read-file — content correctness
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cli-read-file-content-types
  "read-file: various content types are returned correctly"
  :cases (("ascii roundtrip" "(+ 1 2)")
          ("empty string"    "")
          ("with newlines"   nil))
  (content-or-nil)
  (let ((content (or content-or-nil
                     (format nil "(defun f (x)~%  (+ x 1))~%"))))
    (%with-temp-file (path content)
  (assert-string= content (cl-cc/cli::%read-file path)))))

(deftest-each cli-read-file-multibyte-correctness
  ;; Regression: file-length returns byte count (≥ char count for UTF-8).
  ;; Old code: (make-string (file-length in)) over-allocated → trailing #\Nul.
  ;; Fix: (subseq buf 0 (read-sequence buf in)).
  "read-file: returns exact content and character count for multibyte UTF-8 files."
  :cases (("cjk-3chars"    "(+ 1 2) ;; テスト")  ; 3 CJK chars = 9 bytes
          ("greek-3chars"  "αβγ"))                ; 3 Greek chars = 6 bytes
  (content)
  (%with-temp-file (path content)
    (let ((result (cl-cc/cli::%read-file path)))
      (assert-false (find #\Nul result))
      (assert-string= content result)
      (assert-= (length content) (length result)))))

(deftest cli-read-file-not-found
  "read-file: non-existent file signals a plain error"
  (assert-signals error
  (cl-cc/cli::%read-file "/tmp/cl-cc-nonexistent-99999.lisp")))

;;; ─────────────────────────────────────────────────────────────────────────
;;; handlers.lisp — stable helper-level coverage
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cli-count-parens-ignores-parens-inside-strings
  "count-parens: only structural parentheses are counted (strings excluded)."
  :cases (("open-count"  1 0)
          ("close-count" 1 1))
  (expected value-index)
  (assert-= expected (nth-value value-index (cl-cc/cli::%count-parens "(print \"(()\")"))))

(deftest cli-command-dispatch-covers-all-public-subcommands
  "The CLI dispatch table still exposes the expected public commands."
  (assert-equal '("run" "compile" "eval" "repl" "check")
                (mapcar #'car cl-cc/cli::*cli-command-dispatch*)))

(deftest cli-maybe-make-profiled-vm-state-disabled
  "No profiled VM state is created when flamegraph output is not requested."
  (let ((opts (cl-cc/cli::make-compile-opts)))
    (assert-null (cl-cc/cli::%maybe-make-profiled-vm-state opts))))

(deftest cli-maybe-make-profiled-vm-state-enabled
  "Flamegraph-enabled options create and prime a profiled VM state."
  (let* ((opts (cl-cc/cli::make-compile-opts :flamegraph-path "/tmp/cl-cc-test.svg"))
         (vm-state (cl-cc/cli::%maybe-make-profiled-vm-state opts)))
    (assert-true vm-state)
    (assert-true (cl-cc/vm::vm-profile-enabled-p vm-state))
    (assert-equal '("<toplevel>") (cl-cc/vm::vm-profile-call-stack vm-state))))

(deftest cli-read-command-source-roundtrip
  "%read-command-source reuses the CLI read path for ordinary source files."
  (let ((content "(print :ok)"))
    (%with-temp-file (path content)
      (assert-string= content (cl-cc/cli::%read-command-source path)))))

(defun %capture-fake-quit-code (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (fake-quit (e) (fake-quit-code e))))

(deftest cli-run-compiled-result-executes-program
  "%run-compiled-result compiles a simple expression and runs it without error."
  (let* ((result (cl-cc::compile-string "(+ 1 1)" :target :vm))
         (vm-state (cl-cc/vm::make-vm-state :output-stream *standard-output*))
         (opts (cl-cc/cli::make-compile-opts))
         (val (cl-cc/cli::%run-compiled-result result vm-state opts)))
    (assert-= 2 val)))

(deftest-each cli-do-command-missing-arg-exits-2
  "Each command handler exits 2 and prints command-specific help when the required arg is absent."
  :cases (("run"     "run"     'cl-cc/cli::%do-run)
          ("compile" "compile" 'cl-cc/cli::%do-compile)
          ("eval"    "eval"    'cl-cc/cli::%do-eval)
          ("check"   "check"   'cl-cc/cli::%do-check))
  (command fn-sym)
  (let ((help-command nil))
    (with-fake-quit
      (with-replaced-function (cl-cc/cli::%print-help
                               (lambda (cmd) (setf help-command cmd)))
        (assert-= 2 (%capture-fake-quit-code
                      (lambda ()
                        (funcall (symbol-function fn-sym)
                                 (make-cli-parsed :command command)))))))
    (assert-string= command help-command)))
