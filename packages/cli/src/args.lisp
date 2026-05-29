;;;; cli/src/args.lisp — Zero-dependency command-line argument parser
;;;;
;;;; Supports:
;;;;   --flag              boolean flag → T
;;;;   --key value         string flag via separate token
;;;;   --key=value         string flag via inline = separator
;;;;   -o value            short flag via separate token
;;;;   positional args     first becomes :command, rest become :positional

(in-package :cl-cc/cli)

;;; --- Condition ---

(define-condition arg-parse-error (error)
  ((message :initarg :message :reader arg-parse-error-message))
  (:report (lambda (c s)
             (format s "Argument error: ~A" (arg-parse-error-message c)))))

;;; --- Flag specification table ---
;;; Associates flag name (string) → type (:bool, :string, or :optional-string)

(defvar *flag-spec*
  '(("--output"  . :string)
    ("-o"        . :string)
    ("--arch"    . :string)
    ("--target"  . :string)    ; FR-219: --target wasm32 | wasm64 | wasm32-wasi
    ("--lang"    . :string)
    ("--dump-ir" . :string)
    ("--stdlib"  . :bool)
    ("--no-stdlib" . :bool)
     ("--aot"     . :bool)      ; FR-219: enable AOT static .wasm compilation
     ("--streaming" . :bool)   ; FR-232: generate instantiateStreaming JS glue
     ("--validate" . :bool)    ; FR-305: validate output Wasm
     ("--sri" . :bool)         ; FR-307: generate SRI metadata
     ("--wat" . :bool)         ; FR-322: disassemble wasm to WAT
     ("--decompile" . :bool)   ; FR-322: use wasm-decompile pseudo-code
    ("--memory64" . :bool)     ; FR-213: enable 64-bit linear memory
    ("--bigint"  . :bool)      ; FR-236: enable JS BigInt i64 conversion
    ("--source-map" . :bool)   ; FR-223: generate .wasm.map
    ("--emit-names" . :bool)   ; FR-242: emit Wasm name section metadata
    ("--emit-debug-info" . :bool) ; FR-222 alias for --debug-info
    ("--type-reflection" . :bool) ; FR-263 devtools JS type metadata
    ("--stack-inspection" . :bool) ; FR-269 stack inspection helpers
    ("--memory-profiler" . :bool) ; FR-318 heap profiler helpers
    ("--hot-reload" . :bool) ; FR-317 table.set reload helpers
    ("--incremental-repl" . :bool) ; FR-288 browser Wasm REPL helpers
    ("--system" . :string)
    ("--annotate-source" . :bool)
    ("--debug"  . :bool)
     ("--opt-remarks" . :string)
     ("--optimization-report" . :bool)
     ("--verbose" . :bool)
    ("--strict"  . :bool)
     ("--strict-no-alloc" . :bool)
     ("--pass-pipeline" . :string)
      ("--opt-bisect-limit" . :string)
       ("--debug-info" . :bool)
      ("--sanitize" . :string)
      ("--lto" . :string)
      ("--eh-model" . :string)
      ("--incremental" . :bool)
       ("--perf-map" . :bool)
       ("--bolt" . :bool)
        ("--verify-transforms" . :bool)
        ("--parallel" . :string)
      ("--tier" . :string)
     ("--block-compile" . :bool)
     ("--print-pass-timings" . :bool)
    ("--time-passes" . :bool)
    ("--trace-json" . :string)
    ("--coverage" . :optional-string)
    ("--pgo-generate" . :string)
    ("--pgo-use" . :string)
    ("--spectre-mitigations" . :bool)
    ("--jit-cache-stats" . :bool)
    ("--profile" . :bool)
     ("--flamegraph" . :string)
     ("--fuzzy" . :string)
     ("--stats" . :bool)
    ("--trace-emit" . :bool)
    ("--retpoline" . :bool)
    ("--stack-protector" . :bool)
     ("--shadow-stack" . :bool)
     ("--compress" . :bool)
      ("--no-compress" . :bool)
      ("--deterministic" . :bool)
      ("--reproducible" . :bool)
      ("--script" . :bool)
      ("--build-id" . :string)
      ("--asan" . :bool)
    ("--msan" . :bool)
    ("--tsan" . :bool)
    ("--ubsan" . :bool)
    ("--hwasan" . :bool)
       ("--timeout" . :string)
       ("--no-timeout" . :bool)
      ("--gc-min-heap" . :string)
      ("--gc-max-heap" . :string)
       ;; FR-276: optimization level
      ("-O" . :string)
      ("--opt-level" . :string)
      ;; FR-241: macro expansion tracing
      ("--trace-macros" . :bool)
      ;; FR-153: macro expansion memoization
      ("--memoize-macros" . :bool)
      ("--dump-image" . :string)
      ("--Werror" . :bool)
      ("--Werror-category" . :string)
      ("--help"    . :bool)
    ("-h"        . :bool))
  "Alist of (flag-string . type) where type is :bool, :string, or :optional-string.")

(defun %flag-type (name)
  "Return the declared flag type for NAME, or NIL if unrecognised."
  (let ((entry (assoc name *flag-spec* :test #'string=)))
    (when entry (cdr entry))))

;;; --- Struct ---

(defstruct parsed-args
  "Result of parsing a command-line argument list."
  (command    nil                              :type (or string null))
  (positional '()                              :type list)
  (flags      (make-hash-table :test #'equal) :type hash-table))

;;; --- Core parser ---

(defun %split-equals (str)
  "Split STR at the first '=' character.
Returns (values name value) where value is nil when no '=' is present."
  (let ((pos (position #\= str)))
    (if pos
        (values (subseq str 0 pos) (subseq str (1+ pos)))
        (values str nil))))

(defun %long-flag-p (str)
  "Return T when STR starts with '--'."
  (and (>= (length str) 2)
       (char= (char str 0) #\-)
       (char= (char str 1) #\-)))

(defun %short-flag-p (str)
  "Return T when STR is a two-character short flag like '-o'."
  (and (= (length str) 2)
       (char= (char str 0) #\-)
       (alpha-char-p (char str 1))))

(defun parse-args (argv)
  "Parse ARGV (list of strings) into a PARSED-ARGS struct.

The first non-flag token becomes the command; subsequent non-flag tokens
are collected as positional arguments.  Flags are stored in a hash-table
keyed by their canonical string (e.g. \"--output\")."
  (let* ((result (make-parsed-args))
         (ht     (parsed-args-flags result))
         (rest   argv))
    (loop while rest do
      (let ((arg (pop rest)))
        (cond
          ;; Script mode: cl-cc --script tool.lisp arg -- --literal
          ;; Treat the script path as the command and preserve the rest as
          ;; script argv, dropping a single explicit positional separator.
          ((string= arg "--script")
           (setf (gethash "--script" ht) t)
           (unless rest
             (error 'arg-parse-error
                    :message "Flag --script requires a script file"))
           (setf (parsed-args-command result) (pop rest)
                 (parsed-args-positional result)
                 (loop for token in rest
                       unless (string= token "--") collect token))
           (setf rest nil))

          ;; Long flag: --name or --name=value
          ((%long-flag-p arg)
           (multiple-value-bind (name inline-val) (%split-equals arg)
             (let ((ftype (%flag-type name)))
               (unless ftype
                 (error 'arg-parse-error
                        :message (format nil "Unknown flag: ~A" name)))
               (ecase ftype
                 (:bool
                  (when inline-val
                    (error 'arg-parse-error
                           :message (format nil "Flag ~A takes no value" name)))
                  (setf (gethash name ht) t))
                 (:string
                  (cond
                    (inline-val
                     (setf (gethash name ht) inline-val))
                    (rest
                     (setf (gethash name ht) (pop rest)))
                    (t
                     (error 'arg-parse-error
                            :message (format nil "Flag ~A requires a value" name)))))
                 (:optional-string
                  (setf (gethash name ht) (or inline-val t)))))))
          ((%short-flag-p arg)
           (let ((ftype (%flag-type arg)))
             (unless ftype
               (error 'arg-parse-error
                      :message (format nil "Unknown flag: ~A" arg)))
             (ecase ftype
               (:bool
                (setf (gethash arg ht) t))
               (:string
                (if rest
                    (setf (gethash arg ht) (pop rest))
                    (error 'arg-parse-error
                           :message (format nil "Flag ~A requires a value" arg))))
               (:optional-string
                (setf (gethash arg ht) t)))))
          (t
           (if (null (parsed-args-command result))
               (setf (parsed-args-command result) arg)
               (setf (parsed-args-positional result)
                     (append (parsed-args-positional result) (list arg))))))))
    result))

;;; --- Flag accessor helpers ---

(defun flag (parsed key)
  "Return the value of flag KEY (long form, e.g. \"--output\") from PARSED,
or NIL when the flag was not supplied."
  (gethash key (parsed-args-flags parsed)))

(defun flag-or (parsed long short)
  "Return the value of flag LONG; if absent, try SHORT.
Useful for flags that have both a long and short form (e.g. --output / -o)."
  (or (flag parsed long)
      (when short (flag parsed short))))
