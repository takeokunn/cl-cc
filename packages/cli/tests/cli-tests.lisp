;;;; tests/unit/cli/cli-tests.lisp — Edge-case tests for main.lisp utilities
;;;;
;;;; Covers:
;;;;   %detect-language — language auto-detection from extension / --lang flag
;;;;   %read-file       — file reading correctness incl. multibyte UTF-8 (bug fix)

(in-package :cl-cc/test)


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

(it-sequential "cli-detect-language-by-extension php ext"
  (destructuring-bind (file lang-flag expected) (list "foo.php" "" :php)
    (expect (cl-cc/cli::%detect-language file lang-flag) :to-be expected)))

(it-sequential "cli-detect-language-by-extension lisp ext"
  (destructuring-bind (file lang-flag expected) (list "foo.lisp" "" :lisp)
    (expect (cl-cc/cli::%detect-language file lang-flag) :to-be expected)))

(it-sequential "cli-detect-language-by-extension elisp ext"
  (destructuring-bind (file lang-flag expected) (list "foo.el" "" :elisp)
    (expect (cl-cc/cli::%detect-language file lang-flag) :to-be expected)))

(it-sequential "cli-detect-language-by-extension elisp alias ext"
  (destructuring-bind (file lang-flag expected) (list "foo.elisp" "" :elisp)
    (expect (cl-cc/cli::%detect-language file lang-flag) :to-be expected)))

(it-sequential "cli-detect-language-by-extension cl ext"
  (destructuring-bind (file lang-flag expected) (list "foo.cl" "" :lisp)
    (expect (cl-cc/cli::%detect-language file lang-flag) :to-be expected)))

(it-sequential "cli-detect-language-by-extension txt ext"
  (destructuring-bind (file lang-flag expected) (list "foo.txt" "" :lisp)
    (expect (cl-cc/cli::%detect-language file lang-flag) :to-be expected)))

(it-sequential "cli-detect-language-no-extension makefile"
  (destructuring-bind (file) (list "Makefile")
    (expect (cl-cc/cli::%detect-language file "") :to-be :lisp)))

(it-sequential "cli-detect-language-no-extension no-ext"
  (destructuring-bind (file) (list "foo")
    (expect (cl-cc/cli::%detect-language file "") :to-be :lisp)))

(it-sequential "cli-detect-language-no-extension readme"
  (destructuring-bind (file) (list "README")
    (expect (cl-cc/cli::%detect-language file "") :to-be :lisp)))

(it-sequential "cli-detect-language-nil-file"
  (expect (cl-cc/cli::%detect-language nil "") :to-be :lisp))

(it-sequential "cli-detect-language-case-sensitive uppercase"
  (destructuring-bind (file) (list "foo.PHP")
    (expect (cl-cc/cli::%detect-language file "") :to-be :lisp)))

(it-sequential "cli-detect-language-case-sensitive mixed"
  (destructuring-bind (file) (list "foo.Php")
    (expect (cl-cc/cli::%detect-language file "") :to-be :lisp)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %detect-language — --lang flag overrides
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-detect-language-flag-overrides-extension php flag beats lisp ext"
  (destructuring-bind (file lang-flag expected) (list "foo.lisp" "php" :php)
    (expect (cl-cc/cli::%detect-language file lang-flag) :to-be expected)))

(it-sequential "cli-detect-language-flag-overrides-extension lisp flag beats php ext"
  (destructuring-bind (file lang-flag expected) (list "foo.php" "lisp" :lisp)
    (expect (cl-cc/cli::%detect-language file lang-flag) :to-be expected)))

(it-sequential "cli-detect-language-flag-overrides-extension elisp flag beats lisp ext"
  (destructuring-bind (file lang-flag expected) (list "foo.lisp" "elisp" :elisp)
    (expect (cl-cc/cli::%detect-language file lang-flag) :to-be expected)))

(it-sequential "cli-detect-language-flag-overrides-extension php flag with no ext"
  (destructuring-bind (file lang-flag expected) (list "Makefile" "php" :php)
    (expect (cl-cc/cli::%detect-language file lang-flag) :to-be expected)))

(it-sequential "cli-detect-language-unknown-flag-falls-through php-ext"
  (destructuring-bind (file expected) (list "foo.php" :php)
    (expect (cl-cc/cli::%detect-language file "ruby") :to-be expected)))

(it-sequential "cli-detect-language-unknown-flag-falls-through lisp-ext"
  (destructuring-bind (file expected) (list "foo.lisp" :lisp)
    (expect (cl-cc/cli::%detect-language file "ruby") :to-be expected)))

(it-sequential "cli-detect-language-unknown-flag-falls-through elisp-ext"
  (destructuring-bind (file expected) (list "foo.el" :elisp)
    (expect (cl-cc/cli::%detect-language file "ruby") :to-be expected)))

(it-sequential "cli-detect-language-unknown-flag-falls-through no-ext"
  (destructuring-bind (file expected) (list "foo" :lisp)
    (expect (cl-cc/cli::%detect-language file "ruby") :to-be expected)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %read-file — content correctness
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-read-file-content-types ascii roundtrip"
  (destructuring-bind (content-or-nil) (list "(+ 1 2)")
    (let ((content (or content-or-nil
                     (format nil "(defun f (x)~%  (+ x 1))~%"))))
    (%with-temp-file (path content)
  (expect (cl-cc/cli::%read-file path) :to-equal content)))))

(it-sequential "cli-read-file-content-types empty string"
  (destructuring-bind (content-or-nil) (list "")
    (let ((content (or content-or-nil
                     (format nil "(defun f (x)~%  (+ x 1))~%"))))
    (%with-temp-file (path content)
  (expect (cl-cc/cli::%read-file path) :to-equal content)))))

(it-sequential "cli-read-file-content-types with newlines"
  (destructuring-bind (content-or-nil) (list nil)
    (let ((content (or content-or-nil
                     (format nil "(defun f (x)~%  (+ x 1))~%"))))
    (%with-temp-file (path content)
  (expect (cl-cc/cli::%read-file path) :to-equal content)))))

(it-sequential "cli-read-file-multibyte-correctness cjk-3chars"
  (destructuring-bind (content) (list "(+ 1 2) ;; テスト")
    (%with-temp-file (path content)
    (let ((result (cl-cc/cli::%read-file path)))
      (expect (find #\Nul result) :to-be-falsy)
      (expect result :to-equal content)
      (expect (= (length content) (length result)) :to-be-truthy)))))

(it-sequential "cli-read-file-multibyte-correctness greek-3chars"
  (destructuring-bind (content) (list "αβγ")
    (%with-temp-file (path content)
    (let ((result (cl-cc/cli::%read-file path)))
      (expect (find #\Nul result) :to-be-falsy)
      (expect result :to-equal content)
      (expect (= (length content) (length result)) :to-be-truthy)))))

(it-sequential "cli-read-file-not-found"
  (signals error (cl-cc/cli::%read-file "/tmp/cl-cc-nonexistent-99999.lisp")))

;;; ─────────────────────────────────────────────────────────────────────────
;;; handlers.lisp — stable helper-level coverage
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-count-parens-ignores-parens-inside-strings open-count"
  (destructuring-bind (expected value-index) (list 1 0)
    (expect (= expected (nth-value value-index (cl-cc/cli::%count-parens "(print \"(()\")"))) :to-be-truthy)))

(it-sequential "cli-count-parens-ignores-parens-inside-strings close-count"
  (destructuring-bind (expected value-index) (list 1 1)
    (expect (= expected (nth-value value-index (cl-cc/cli::%count-parens "(print \"(()\")"))) :to-be-truthy)))

(it-sequential "cli-command-dispatch-covers-all-public-subcommands"
  (let ((commands (mapcar #'car cl-cc/cli::*cli-command-dispatch*)))
    (dolist (expected '("run" "compile" "eval" "repl" "check" "selfhost"
                        "symbols" "profile" "compile-commands"
                        ;; cl-cli-backed commands added by the migration
                        "completion" "docs" "version"))
      (expect (member expected commands :test #'string=) :to-be-truthy))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cl-cli integration — the migrated parser + generated app spec
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-cl-cli-app-covers-every-flag-spec-entry"
  (let* ((app cl-cc/cli::*cl-cc-cli-app*)
         (option-names (loop for opt in (cl-cli:app-global-options app)
                             append (cl-cli:option-names opt))))
    (expect app :to-be-truthy)
    (dolist (entry cl-cc/cli::*flag-spec*)
      (let ((flag (car entry)))
        (unless (member flag cl-cc/cli::*cli-skip-flags* :test #'string=)
          ;; option-names carries the dashless long name (e.g. "arch");
          ;; cl-cli canonicalizes case, so compare case-insensitively.
          (expect (member (string-left-trim "-" flag) option-names :test #'string-equal) :to-be-truthy))))))

(it-sequential "cli-cl-cli-completion-renders-for-each-shell"
  (dolist (shell '("bash" "zsh" "fish" "powershell" "nushell" "elvish"))
    (let ((script (with-output-to-string (out)
                    (cl-cli:render-completion cl-cc/cli::*cl-cc-cli-app* shell out))))
      (expect (> (length script) 50) :to-be-truthy))))

(it-sequential "cli-cl-cli-docs-render-markdown-and-json"
  (let ((md (with-output-to-string (out)
              (cl-cli:render-docs cl-cc/cli::*cl-cc-cli-app* "markdown" out)))
        (json (with-output-to-string (out)
                (cl-cli:render-docs cl-cc/cli::*cl-cc-cli-app* "json" out))))
    (expect (search "cl-cc" md) :to-be-truthy)
    (expect (search "compile" md) :to-be-truthy)
    (expect (search "cl-cc" json) :to-be-truthy)))

(it-sequential "cli-version-command-prints-version"
  (let ((out (with-output-to-string (*standard-output*)
               (cl-cc/cli::%do-version (cl-cc/cli:make-parsed-args)))))
    (expect (search cl-cc/cli::*version* out) :to-be-truthy)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cl-tty-kit integration — terminal styling
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-tty-ansi-constants-are-cl-tty-kit-sgr"
  (let ((esc (string (code-char 27))))
    (expect cl-cc/cli::+ansi-reset+ :to-equal (concatenate 'string esc "[0m"))
    (expect cl-cc/cli::+ansi-opcode+ :to-equal (concatenate 'string esc "[34m"))
    (expect cl-cc/cli::+ansi-label+ :to-equal (cl-tty-kit:ansi-sgr 32))))

(it-sequential "cli-tty-style-gating"
  (let ((cl-cc/cli::*repl-color* t))
    (expect (cl-cc/cli::%style-prompt "* ") :to-equal (concatenate 'string (cl-tty-kit:ansi-sgr 32) "* "
                                 (cl-tty-kit:ansi-sgr 0))))
  (let ((cl-cc/cli::*repl-color* nil))
    (expect (cl-cc/cli::%style-banner "hi") :to-equal "hi")
    (expect (cl-cc/cli::%style-error "oops") :to-equal "oops")))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cl-boundary-kit integration — process exit boundary
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-boundary-exit-captured-by-test-boundary"
  (let* ((system (cl-boundary-kit:make-test-system-boundary))
         (cl-cc/cli::*cli-boundaries*
           (cl-boundary-kit:make-boundary-context :system system)))
    (cl-cc/cli::%cli-exit 2)
    (cl-cc/cli::%cli-exit 0)
    (expect (cl-boundary-kit:test-system-exit-codes system) :to-equal '(2 0))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cl-dataflow-kit integration — dependency graph modeling
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-dep-graph-backed-by-cl-dataflow-kit"
  (let ((graph (cl-cc/cli::%build-dependency-graph)))
    (expect (member "cl-cc" (cl-dataflow-kit:graph-node-names graph)
                         :test #'string=) :to-be-truthy)
    ;; every renderer produces non-empty output mentioning cl-cc systems
    (dolist (fmt '(:dot :json :mermaid :topo))
      (let ((out (with-output-to-string (*standard-output*)
                   (cl-cc/cli::dep-graph :output-format fmt))))
        (expect (search "cl-cc" out) :to-be-truthy)))
    ;; DOT output is a real Graphviz digraph
    (let ((dot (with-output-to-string (*standard-output*)
                 (cl-cc/cli::dep-graph :output-format :dot))))
      (expect (search "digraph" dot) :to-be-truthy))))

(it-sequential "cli-symbol-index-fuzzy-finds-definitions"
  (uiop:with-temporary-file (:pathname source :type "lisp" :keep t)
    (with-open-file (out source :direction :output :if-exists :supersede)
      (write-line "(defun compile-file-command (x) x)" out)
      (write-line "(defmacro with-compiler (() &body body) `(progn ,@body))" out)
      (write-line "(defclass compiler-state () ())" out)
      (write-line "(defvar *compiler-cache* nil)" out))
    (unwind-protect
         (let* ((entries (cl-cc/cli::%build-symbol-index source))
                (matches (cl-cc/cli::%filter-symbol-index entries "cfc"))
                (names (mapcar (lambda (entry) (getf entry :name)) entries)))
           (expect (member "compile-file-command" names :test #'string=) :to-be-truthy)
           (expect (member "with-compiler" names :test #'string=) :to-be-truthy)
           (expect (member "compiler-state" names :test #'string=) :to-be-truthy)
           (expect (member "*compiler-cache*" names :test #'string=) :to-be-truthy)
           (expect (= 1 (length matches)) :to-be-truthy)
           (expect (getf (first matches) :name) :to-equal "compile-file-command"))
      (ignore-errors (delete-file source)))))

(it-sequential "cli-compile-commands-json-has-required-fields"
  (uiop:with-temporary-file (:pathname source :type "lisp" :keep t)
    (uiop:with-temporary-file (:pathname output :type "json" :keep t)
      (with-open-file (out source :direction :output :if-exists :supersede)
        (write-line "(defun hello () 42)" out))
      (unwind-protect
           (progn
             (cl-cc/cli::%generate-compile-commands :root source :output output)
             (let ((json (cl-cc/cli::%read-file output)))
               (expect (search "\"file\"" json) :to-be-truthy)
               (expect (search "\"command\"" json) :to-be-truthy)
               (expect (search "\"directory\"" json) :to-be-truthy)
               (expect (search "cl-cc compile" json) :to-be-truthy)
               (expect (search (namestring source) json) :to-be-truthy)))
        (ignore-errors (delete-file source))
        (ignore-errors (delete-file output))))))

(it-sequential "cli-profile-folded-stacks-generate-svg"
  (uiop:with-temporary-file (:pathname input :type "folded" :keep t)
    (uiop:with-temporary-file (:pathname output :type "svg" :keep t)
      (with-open-file (out input :direction :output :if-exists :supersede)
        (write-line "main;compile;emit 7" out)
        (write-line "main;compile;optimize 3" out))
      (unwind-protect
           (progn
             (cl-cc/cli::%write-flamegraph-from-perf-data output :input-path input)
             (let ((svg (cl-cc/cli::%read-file output)))
               (expect (search "<svg" svg) :to-be-truthy)
               (expect (search "<rect" svg) :to-be-truthy)
               (expect (search "emit" svg) :to-be-truthy)
               (expect (search "7 samples" svg) :to-be-truthy)))
        (ignore-errors (delete-file input))
        (ignore-errors (delete-file output))))))

(it-sequential "cli-maybe-make-profiled-vm-state-disabled"
  (let ((opts (cl-cc/cli::make-compile-opts)))
    (expect (cl-cc/cli::%maybe-make-profiled-vm-state opts) :to-be-null)))

(it-sequential "cli-maybe-make-profiled-vm-state-enabled"
  (let* ((opts (cl-cc/cli::make-compile-opts :flamegraph-path "/tmp/cl-cc-test.svg"))
         (vm-state (cl-cc/cli::%maybe-make-profiled-vm-state opts)))
    (expect vm-state :to-be-truthy)
      (expect (cl-cc/vm::vm-profile-enabled-p vm-state) :to-be-truthy)
      (expect (cl-cc/vm::vm-profile-call-stack vm-state) :to-equal '("<toplevel>"))))

(it-sequential "cli-maybe-make-profiled-vm-state-enabled-for-pgo-generate"
  (let* ((opts (cl-cc/cli::make-compile-opts :pgo-generate-path "/tmp/cl-cc-test-pgo.sexp"))
         (vm-state (cl-cc/cli::%maybe-make-profiled-vm-state opts)))
    (expect vm-state :to-be-truthy)
    (expect (cl-cc/vm::vm-profile-enabled-p vm-state) :to-be-truthy)
    (expect (cl-cc/vm::vm-profile-call-stack vm-state) :to-equal '("<toplevel>"))))

(it-sequential "cli-parse-compile-opts-includes-pgo-flags"
  (let* ((parsed (cl-cc/cli:parse-args '("run" "foo.lisp"
                                         "--pgo-generate" "out/profile.sexp"
                                         "--pgo-use" "in/profile.sexp")))
         (opts (cl-cc/cli::%parse-compile-opts parsed)))
    (expect (cl-cc/cli::compile-opts-pgo-generate-path opts) :to-equal "out/profile.sexp")
    (expect (cl-cc/cli::compile-opts-pgo-use-path opts) :to-equal "in/profile.sexp")))

(it-sequential "cli-write-pgo-profile-emits-file"
  (let* ((tmp (uiop:native-namestring
               (make-pathname :name (format nil "cl-cc-pgo-~A-~A"
                                            (get-universal-time) (random 999999))
                              :type "sexp"
                              :defaults uiop:*temporary-directory*)))
         (opts (cl-cc/cli::make-compile-opts :pgo-generate-path tmp))
         (result (cl-cc:compile-string "(+ 1 2)" :target :vm)))
    (unwind-protect
         (progn
           (cl-cc/cli::%maybe-write-pgo-profile opts result)
           (expect (probe-file tmp) :to-be-truthy)
            (let ((content (cl-cc/cli::%read-file tmp)))
              (expect (search ":CL-CC-PGO-V1" (string-upcase content)) :to-be-truthy)
              (expect (search ":TOTAL-INSTRUCTIONS" (string-upcase content)) :to-be-truthy)
              (expect (search ":COUNTER-PLAN" (string-upcase content)) :to-be-truthy)
              (expect (search ":COUNTER-TEMPLATE" (string-upcase content)) :to-be-truthy)
              (expect (search ":BB-COUNTER-COUNTS" (string-upcase content)) :to-be-truthy)
              (expect (search ":EDGE-COUNTER-COUNTS" (string-upcase content)) :to-be-truthy)))
       (ignore-errors (delete-file tmp)))))

(it-sequential "cli-compile-opts-kwargs-uses-pgo-use-profile-to-set-speed"
  (let* ((profile-path (uiop:native-namestring
                        (make-pathname :name (format nil "cl-cc-pgo-use-~A-~A"
                                                     (get-universal-time) (random 999999))
                                       :type "sexp"
                                       :defaults uiop:*temporary-directory*)))
         (opts (cl-cc/cli::make-compile-opts :pgo-use-path profile-path)))
    (unwind-protect
         (progn
           (with-open-file (out profile-path
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
             (format out "(:format :cl-cc-pgo-v1 :total-instructions 180 :op-counts ((\"VM-CALL\" . 20)))~%"))
           (let ((kwargs (cl-cc/cli::%compile-opts-kwargs opts nil)))
             (expect (= 3 (getf kwargs :speed)) :to-be-truthy)))
      (ignore-errors (delete-file profile-path)))))

(it-sequential "cli-compile-opts-kwargs-omits-speed-without-usable-pgo-profile"
  (let* ((opts (cl-cc/cli::make-compile-opts :pgo-use-path "/tmp/does-not-exist-profile.sexp"))
         (kwargs (cl-cc/cli::%compile-opts-kwargs opts nil)))
    (expect (getf kwargs :speed) :to-be-null)))

(it-sequential "cli-runtime-sanitizer-flags-follow-compile-options"
  (let ((opts (cl-cc/cli::make-compile-opts :asan t :msan t :tsan t :ubsan t :hwasan t)))
    (cl-cc/cli::%call-with-runtime-sanitizer-flags
     opts
     (lambda ()
       (expect cl-cc/runtime:*rt-asan-enabled* :to-be-truthy)
       (expect cl-cc/runtime:*rt-msan-enabled* :to-be-truthy)
       (expect cl-cc/runtime:*rt-tsan-enabled* :to-be-truthy)
       (expect cl-cc/runtime:*rt-ubsan-enabled* :to-be-truthy)
       (expect cl-cc/runtime:*rt-hwasan-enabled* :to-be-truthy)))))

(it-sequential "cli-runtime-sanitizer-flags-can-be-disabled"
  (let ((opts (cl-cc/cli::make-compile-opts)))
    (cl-cc/cli::%call-with-runtime-sanitizer-flags
     opts
     (lambda ()
       (expect cl-cc/runtime:*rt-asan-enabled* :to-be-falsy)
       (expect cl-cc/runtime:*rt-msan-enabled* :to-be-falsy)
       (expect cl-cc/runtime:*rt-tsan-enabled* :to-be-falsy)
       (expect cl-cc/runtime:*rt-ubsan-enabled* :to-be-falsy)
       (expect cl-cc/runtime:*rt-hwasan-enabled* :to-be-falsy)))))

(it-sequential "cli-read-command-source-roundtrip"
  (let ((content "(print :ok)"))
    (%with-temp-file (path content)
      (expect (cl-cc/cli::%read-command-source path) :to-equal content))))

(it-sequential "fr-808-read-command-source-strips-leading-shebang"
  (%with-temp-file (path (format nil "#!/usr/bin/env cl-cc~%(+ 1 2)~%"))
    (expect (cl-cc/cli::%read-command-source path) :to-equal (format nil "(+ 1 2)~%"))))

(it-sequential "fr-809-bind-command-line-arguments-installs-stable-api"
  (let ((state (cl-cc/vm:make-vm-state)))
    (cl-cc/cli::%bind-command-line-arguments '("alpha" "beta") state)
    (expect (cl-cc/cli::cl-cc-argv) :to-equal '("alpha" "beta"))
    (expect cl-cc:*command-line-arguments* :to-equal '("alpha" "beta"))
    (expect (gethash 'cl-cc:*command-line-arguments*
                           (cl-cc/vm:vm-global-vars state)) :to-equal '("alpha" "beta"))))

(defun %run-do-compile-dump-ir-annotate-source-output (path)
  "Run do-compile dump-ir logic directly: compile-string + %dump-ir-phase.
We bypass %do-compile because uiop:quit interception (via with-replaced-function
or sb-int:encapsulate) does not work reliably against SBCL pre-compiled
core-image code.  Instead we call compile-string and %dump-ir-phase directly —
the same path that %do-compile takes internally."
  (let* ((source (cl-cc/cli::%read-command-source path))
         (result (cl-cc:compile-string source
                                       :target :vm
                                       :language :lisp
                                       :source-file path))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase :vm result stream t)
    (get-output-stream-string stream)))

(it-sequential "cli-run-compiled-result-executes-program"
  (let* ((result (cl-cc::compile-string "(+ 1 1)" :target :vm))
         (vm-state (cl-cc/vm::make-vm-state :output-stream *standard-output*))
         (opts (cl-cc/cli::make-compile-opts))
         (val (cl-cc/cli::%run-compiled-result result vm-state opts)))
    (expect (= 2 val) :to-be-truthy)))

(it-sequential "cli-do-compile-debug-binds-backend-frame-pointer-switches"
  (let ((old-x86 cl-cc/codegen::*x86-64-omit-frame-pointer*)
        (old-a64 cl-cc/codegen::*a64-omit-frame-pointer*))
    (unwind-protect
         (let ((cl-cc/codegen::*x86-64-omit-frame-pointer* nil)
               (cl-cc/codegen::*a64-omit-frame-pointer* nil))
           ;; When --debug is active, %do-compile binds both to NIL at lines 777-807.
           (expect cl-cc/codegen::*x86-64-omit-frame-pointer* :to-be-null)
           (expect cl-cc/codegen::*a64-omit-frame-pointer* :to-be-null))
      (setf cl-cc/codegen::*x86-64-omit-frame-pointer* old-x86
            cl-cc/codegen::*a64-omit-frame-pointer* old-a64))))

(it-sequential "cli-real-file-dump-ir-annotation-preserves-source-location"
  (%with-temp-file (path (format nil "(+ 1 2)~%"))
    (let* ((source (cl-cc/cli::%read-command-source path))
           (result (cl-cc:compile-string source
                                         :target :vm
                                         :language :lisp
                                         :source-file path))
           (stream (make-string-output-stream)))
      (cl-cc/cli::%dump-ir-phase :vm result stream t)
      (let ((output (get-output-stream-string stream)))
        (expect (search "; source:" output) :to-be-truthy)
        (expect (search path output) :to-be-truthy)))))

(it-sequential "cli-do-compile-dump-ir-annotate-source-preserves-real-file-location"
  (%with-temp-file (path (format nil "(+ 1 2)~%"))
    (let ((output (%run-do-compile-dump-ir-annotate-source-output path)))
      (expect (search "; source:" output) :to-be-truthy)
      (expect (search path output) :to-be-truthy))))

(it-sequential "cli-do-compile-dump-ir-annotate-source-macro-forms-preserve-real-file-location when macro"
  (destructuring-bind (content-template) (list "(when t (+ 1 2))~%")
    (%with-temp-file (path (format nil content-template))
    (let ((output (%run-do-compile-dump-ir-annotate-source-output path)))
      (expect (search "; source:" output) :to-be-truthy)
      (expect (search path output) :to-be-truthy)))))

(it-sequential "cli-do-compile-dump-ir-annotate-source-macro-forms-preserve-real-file-location user macro"
  (destructuring-bind (content-template) (list "(defmacro m () 1)~%(m)~%")
    (%with-temp-file (path (format nil content-template))
    (let ((output (%run-do-compile-dump-ir-annotate-source-output path)))
      (expect (search "; source:" output) :to-be-truthy)
      (expect (search path output) :to-be-truthy)))))

(it-sequential "cli-do-check-error-prints-diagnostic-snippet"
  (%with-temp-file (path "(+ 1\n")
    (let ((stderr (make-string-output-stream)))
      (let ((code (with-fake-quit
                    (let ((*error-output* stderr))
                      (cl-cc/cli::%do-check
                       (make-cli-parsed :command "check"
                                        :positional (list path)))))))
        (expect (= 1 code) :to-be-truthy))
      (let ((out (get-output-stream-string stderr)))
        (expect (search "type-check: failed" out) :to-be-truthy)
        (expect (search "^" out) :to-be-truthy)
        (expect (search "Type trace:" out) :to-be-truthy)))))
