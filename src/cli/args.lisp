;;;; src/cli/args.lisp — Zero-dependency command-line argument parser
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
;;; Associates flag name (string) → type (:bool or :string)

(defvar *flag-spec*
  '(("--output"  . :string)
    ("-o"        . :string)
    ("--arch"    . :string)
    ("--lang"    . :string)
    ("--dump-ir" . :string)
    ("--stdlib"  . :bool)
    ("--annotate-source" . :bool)
    ("--opt-remarks" . :string)
    ("--verbose" . :bool)
    ("--strict"  . :bool)
    ("--pass-pipeline" . :string)
    ("--print-pass-timings" . :bool)
    ("--help"    . :bool)
    ("-h"        . :bool))
  "Alist of (flag-string . type) where type is :bool or :string.")

(defun %flag-type (name)
  "Return :bool or :string for the given flag NAME, or nil if unrecognised."
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
                            :message (format nil "Flag ~A requires a value" name)))))))))

          ;; Short flag: -o value
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
                           :message (format nil "Flag ~A requires a value" arg)))))))

          ;; Positional argument
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
