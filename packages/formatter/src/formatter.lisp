;;;; formatter.lisp — FR-320: Minimal Lisp Code Formatter
;;;;
;;;; A simple pretty-printer that reads Lisp source and re-emits it with
;;;; consistent indentation and spacing. Does NOT evaluate code.

(in-package :cl-user)

(defpackage :cl-cc/formatter
  (:use :cl)
  (:export #:format-string
           #:format-file))

(in-package :cl-cc/formatter)

(defparameter *indent-size* 2
  "Number of spaces per indentation level.")

(defun %format-form (stream form depth)
  "Pretty-print FORM to STREAM at indentation depth DEPTH."
  (cond
    ((null form)
     (format stream "()"))
    ((atom form)
     (format stream "~S" form))
    ((consp form)
     (let ((head (car form)))
       (format stream "(")
       (cond
         ;; Special forms with aligned bodies
         ((member head '(defun defmacro defvar defparameter defstruct defclass
                         defmethod define-condition let let* flet labels
                         macrolet symbol-macrolet multiple-value-bind
                         handler-case handler-bind restart-case))
          (format stream "~S~%" head)
          (let ((inner-depth (+ depth *indent-size*)))
            ;; Name/lambda-list on same line as head
            (when (cdr form)
              (format stream "~v@T~S~%" inner-depth (second form))
              ;; Body indented further
              (dolist (body-form (cddr form))
                (%format-form stream body-form inner-depth)
                (terpri stream))))
          (format stream "~v@T)" depth))
         ;; COND and CASE clauses
         ((member head '(cond case ecase ccase typecase etypecase ctypecase))
          (format stream "~S" head)
          (dolist (clause (cdr form))
            (format stream "~%")
            (%format-form stream clause (+ depth *indent-size*)))
          (format stream ")"))
         ;; Simple forms: all on one line if short
         (t
          (let ((printed (with-output-to-string (s)
                           (format s "~S" (cdr form)))))
            (if (< (length printed) 60)
                (format stream "~S ~A)" head printed)
                (progn
                  (format stream "~S" head)
                  (dolist (arg (cdr form))
                    (format stream "~%")
                    (%format-form stream arg (+ depth *indent-size*)))
                  (format stream ")"))))))
        (t (format stream "~S" form)))))

(defun format-string (source-string &key (indent-size 2))
  "FR-320: Format SOURCE-STRING as Lisp code and return the formatted string.
INDENT-SIZE controls spaces per nesting level."
  (let ((*indent-size* indent-size)
        (forms nil)
        (output (make-string-output-stream)))
    ;; Read all top-level forms
    (with-input-from-string (in source-string)
      (loop for form = (read in nil nil)
            while form
            do (push form forms)))
    ;; Pretty-print each form
    (dolist (form (nreverse forms))
      (%format-form output form 0)
      (format output "~%~%"))
    (get-output-stream-string output)))

(defun format-file (source-path &key (output-path nil) (indent-size 2))
  "FR-320: Format the Lisp source file at SOURCE-PATH.
If OUTPUT-PATH is NIL, return the formatted string.
Otherwise write to OUTPUT-PATH."
  (let* ((source (with-open-file (in source-path :direction :input)
                   (let* ((len (file-length in))
                          (buf (make-string len)))
                     (read-sequence buf in)
                     buf)))
         (formatted (format-string source :indent-size indent-size)))
    (if output-path
        (with-open-file (out output-path :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
          (write-string formatted out)
          (namestring output-path))
        formatted)))
