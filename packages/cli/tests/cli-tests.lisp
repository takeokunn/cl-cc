;;;; tests/unit/cli/cli-tests.lisp — Edge-case tests for main.lisp utilities
;;;;
;;;; Covers:
;;;;   %detect-language — language auto-detection from extension / --lang flag
;;;;   %read-file       — file reading correctness incl. multibyte UTF-8 (bug fix)

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

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

(deftest cli-read-file-multibyte-no-nul
  ;; Regression test: file-length returns byte count (≥ char count for UTF-8).
  ;; The old code did (make-string (file-length in)) which over-allocated,
  ;; leaving trailing #\Nul characters after read-sequence.
  ;; Fixed by: (subseq buf 0 (read-sequence buf in)).
  "read-file: multibyte UTF-8 file has no trailing nul characters"
  ;; "テスト" = 3 chars but 9 bytes → old code appended 6 #\Nul chars
  (let ((content "(+ 1 2) ;; テスト"))
    (%with-temp-file (path content)
  (let ((result (cl-cc/cli::%read-file path)))
        ;; No trailing nul character anywhere
        (assert-false (find #\Nul result))
        ;; Exact content match
        (assert-string= content result)
        ;; Length matches character count, not byte count
        (assert-= (length content) (length result))))))

(deftest cli-read-file-multibyte-length-correct
  "read-file: returned string length equals character count, not byte count"
  ;; Greek letters: each 2 bytes UTF-8.  3 chars = 6 bytes.
  ;; file-length returns 6; correct char count is 3.
  (let ((content "αβγ"))
    (%with-temp-file (path content)
  (let ((result (cl-cc/cli::%read-file path)))
        (assert-= 3 (length result))
        (assert-string= "αβγ" result)))))

(deftest cli-read-file-not-found
  "read-file: non-existent file signals a plain error"
  (assert-signals error
  (cl-cc/cli::%read-file "/tmp/cl-cc-nonexistent-99999.lisp")))

;;; ─────────────────────────────────────────────────────────────────────────
;;; handlers.lisp — stable helper-level coverage
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cli-count-parens-ignores-parens-inside-strings
  "count-parens: only structural parentheses are counted."
  (multiple-value-bind (open close)
      (cl-cc/cli::%count-parens "(print \"(()\")")
    (assert-= 1 open)
    (assert-= 1 close)))

(deftest cli-command-dispatch-covers-all-public-subcommands
  "The CLI dispatch table still exposes the expected public commands."
  (assert-equal '("run" "compile" "eval" "repl" "check")
                (mapcar #'car cl-cc/cli::*cli-command-dispatch*)))
