;;;; docgen.lisp --- Markdown API documentation generator

(defpackage :cl-cc/docgen
  (:use :cl)
  (:export :generate-api-docs))

(in-package :cl-cc/docgen)

(defstruct api-entry
  "A documented top-level definition discovered in a source file."
  kind
  name
  docstring)

(defun %definition-kind (operator)
  "Return the canonical API documentation kind for OPERATOR, or NIL."
  (and (symbolp operator)
       (let ((name (string-upcase (symbol-name operator))))
         (cond
           ((string= name "DEFUN") "defun")
           ((string= name "DEFMACRO") "defmacro")
           ((string= name "DEFSTRUCT") "defstruct")
           ((string= name "DEFCLASS") "defclass")
           ((string= name "DEFPARAMETER") "defparameter")
           ((string= name "DEFVAR") "defvar")
           (t nil)))))

(defun %definition-name-string (name-form)
  "Return a deterministic printable definition name for NAME-FORM."
  (let ((name (if (consp name-form) (car name-form) name-form)))
    (cond
      ((symbolp name) (string-downcase (symbol-name name)))
      ((stringp name) name)
      (t (string-downcase (prin1-to-string name))))))

(defun %function-like-docstring (form)
  "Return the docstring from a DEFUN or DEFMACRO FORM, if present."
  (let ((candidate (fourth form)))
    (and (stringp candidate) candidate)))

(defun %variable-docstring (form)
  "Return the docstring from a DEFVAR or DEFPARAMETER FORM, if present."
  (let ((candidates (cddr form)))
    (find-if #'stringp candidates)))

(defun %defstruct-docstring (form)
  "Return the docstring from a DEFSTRUCT FORM, if present."
  (let ((candidate (third form)))
    (and (stringp candidate) candidate)))

(defun %defclass-docstring (form)
  "Return the :DOCUMENTATION option docstring from a DEFCLASS FORM, if present."
  (loop for option in (cddddr form)
        when (and (consp option)
                  (symbolp (first option))
                  (string= (string-upcase (symbol-name (first option))) "DOCUMENTATION")
                  (stringp (second option)))
          return (second option)))

(defun %extract-docstring (kind form)
  "Return KIND-specific docstring from FORM, if present."
  (cond
    ((or (string= kind "defun")
         (string= kind "defmacro"))
     (%function-like-docstring form))
    ((string= kind "defstruct")
     (%defstruct-docstring form))
    ((string= kind "defclass")
     (%defclass-docstring form))
    ((or (string= kind "defparameter")
         (string= kind "defvar"))
     (%variable-docstring form))
    (t nil)))

(defun %entry-from-form (form)
  "Return an API-ENTRY for documented top-level FORM, or NIL."
  (when (and (consp form) (symbolp (first form)))
    (let* ((kind (%definition-kind (first form)))
           (docstring (and kind (%extract-docstring kind form))))
      (when (and kind docstring)
        (make-api-entry :kind kind
                        :name (%definition-name-string (second form))
                        :docstring docstring)))))

(defun %read-source-forms (source-path)
  "Read all top-level forms from SOURCE-PATH without evaluating them."
  (with-open-file (input source-path :direction :input)
    (let ((*read-eval* nil)
          (*package* (find-package :cl-user))
          (eof (gensym "EOF")))
      (loop for form = (read input nil eof)
            until (eq form eof)
            collect form))))

(defun %extract-api-entries (source-path)
  "Extract documented top-level API entries from SOURCE-PATH."
  (loop for form in (%read-source-forms source-path)
        for entry = (%entry-from-form form)
        when entry collect entry))

(defun %write-docstring-markdown (docstring stream)
  "Write DOCSTRING to STREAM as Markdown text, preserving line breaks."
  (with-input-from-string (input docstring)
    (loop for line = (read-line input nil nil)
          while line do (format stream "~A~%" line))))

(defun %emit-markdown (source-path entries stream)
  "Emit Markdown documentation for ENTRIES from SOURCE-PATH to STREAM."
  (format stream "# File: ~A~%~%" (namestring (pathname source-path)))
  (dolist (entry entries)
    (format stream "## ~A ~A~%~%"
            (api-entry-kind entry)
            (api-entry-name entry))
    (%write-docstring-markdown (api-entry-docstring entry) stream)
    (terpri stream)))

(defun generate-api-docs (source-path &key output-path)
  "Generate deterministic Markdown API docs for documented top-level definitions in SOURCE-PATH.

The generator reads source forms with READ and *READ-EVAL* bound to NIL. It
does not evaluate source forms. When OUTPUT-PATH is NIL, return the Markdown as
a string. Otherwise write the Markdown to OUTPUT-PATH and return OUTPUT-PATH."
  (let ((entries (%extract-api-entries source-path)))
    (if output-path
        (progn
          (ensure-directories-exist output-path)
          (with-open-file (output output-path
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
            (%emit-markdown source-path entries output))
          output-path)
        (with-output-to-string (output)
          (%emit-markdown source-path entries output)))))
