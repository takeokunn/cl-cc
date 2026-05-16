(defpackage :cl-cc/tools/lsp
  (:use :cl)
  (:export
   #:make-lsp-server
   #:lsp-open-document
   #:lsp-update-document
   #:lsp-handle-request
   #:lsp-supported-method-p))

(in-package :cl-cc/tools/lsp)

(defstruct (lsp-server (:constructor make-lsp-server))
  (documents (make-hash-table :test #'equal))
  (capabilities
   '(("definitionProvider" . t)
     ("hoverProvider" . t)
     ("inlayHintProvider" . t)
     ("diagnosticProvider" . t)
     ("completionProvider" . (("triggerCharacters" . ("." ":")))))))

(defparameter *lsp-completion-keywords*
  '("defun" "defmacro" "defclass" "defvar" "defparameter" "lambda" "let" "if"
    "progn" "setf" "slot-value" "make-instance")
  "Minimal keyword completion list for CL-CC LSP prototype.")

(defun %lsp-get (alist key)
  (cdr (assoc key alist :test #'string=)))

(defun %lsp-lines (text)
  (let ((lines '())
        (start 0)
        (n (length text)))
    (loop for i from 0 below n
          do (when (char= (char text i) #\Newline)
               (push (subseq text start i) lines)
               (setq start (1+ i))))
    (push (subseq text start n) lines)
    (nreverse lines)))

(defun %lsp-symbol-at-position (text line character)
  (let* ((lines (%lsp-lines text))
         (line-text (if (and (>= line 0) (< line (length lines)))
                        (nth line lines)
                        ""))
         (len (length line-text))
         (idx (max 0 (min character len))))
    (labels ((word-char-p (c)
               (or (alphanumericp c)
                   (member c '(#\- #\* #\+ #\? #\! #\< #\> #\= #\/ #\_) :test #'char=))))
      (let ((left idx)
            (right idx))
        (loop while (and (> left 0)
                         (word-char-p (char line-text (1- left))))
              do (decf left))
        (loop while (and (< right len)
                         (word-char-p (char line-text right)))
              do (incf right))
        (if (> right left)
            (subseq line-text left right)
            "")))))

(defun %lsp-find-definition (text symbol-name)
  (let ((lines (%lsp-lines text))
        (needle (string-downcase symbol-name)))
    (loop for line-text in lines
          for line-no from 0
          for lower = (string-downcase line-text)
          do (dolist (prefix '("(defun " "(defmacro " "(defclass " "(defvar " "(defparameter "))
               (let ((pos (search prefix lower :test #'char=)))
                 (when pos
                   (let* ((start (+ pos (length prefix)))
                          (end (or (position-if (lambda (c)
                                                  (member c '(#\Space #\Tab #\Newline #\) #\()) )
                                                line-text
                                                :start start)
                                   (length line-text)))
                          (name (subseq line-text start end)))
                     (when (string= (string-downcase name) needle)
                       (return-from %lsp-find-definition (values line-no start))))))))
    (values nil nil)))

(defun %lsp-count-char (text ch)
  (loop for c across text count (char= c ch)))

(defun %lsp-build-diagnostics (text)
  (let ((open (%lsp-count-char text #\())
        (close (%lsp-count-char text #\))))
    (if (= open close)
        '()
        `((("range" . (("start" . (("line" . 0) ("character" . 0)))
                        ("end" . (("line" . 0) ("character" . 1)))))
           ("severity" . 1)
           ("message" . ,(format nil "Unbalanced parentheses: open=~D close=~D" open close)))))))

(defun lsp-open-document (server uri text)
  (setf (gethash uri (lsp-server-documents server)) text)
  server)

(defun lsp-update-document (server uri text)
  (setf (gethash uri (lsp-server-documents server)) text)
  server)

(defun lsp-supported-method-p (method)
  (member method
          '("initialize"
            "textDocument/definition"
            "textDocument/hover"
            "textDocument/inlayHint"
            "textDocument/diagnostic"
            "textDocument/completion")
          :test #'string=))

(defun lsp-handle-request (server request)
  "Minimal LSP request dispatcher for FR-461 surface area.
REQUEST is expected to be an alist with string keys: method, id, params." 
  (let* ((method (cdr (assoc "method" request :test #'string=)))
         (id (cdr (assoc "id" request :test #'string=)))
         (params (%lsp-get request "params")))
    (cond
      ((not (lsp-supported-method-p method))
       `(("jsonrpc" . "2.0")
         ("id" . ,id)
         ("error" . (("code" . -32601)
                      ("message" . "Method not found")))))
      ((string= method "initialize")
       `(("jsonrpc" . "2.0")
         ("id" . ,id)
         ("result" . (("capabilities"
                        . (("definitionProvider" . t)
                           ("hoverProvider" . t)
                           ("inlayHintProvider" . t)
                           ("diagnosticProvider" . t)
                           ("completionProvider" . (("triggerCharacters" . ("." ":"))))))))))
      ((string= method "textDocument/definition")
       (let* ((doc (%lsp-get params "textDocument"))
              (uri (%lsp-get doc "uri"))
              (pos (%lsp-get params "position"))
              (line (or (%lsp-get pos "line") 0))
              (character (or (%lsp-get pos "character") 0))
              (text (or (gethash uri (lsp-server-documents server)) ""))
              (sym (%lsp-symbol-at-position text line character)))
         (multiple-value-bind (def-line def-char) (%lsp-find-definition text sym)
           `(("jsonrpc" . "2.0")
             ("id" . ,id)
             ("result" . ,(if def-line
                               `(("uri" . ,uri)
                                 ("range" . (("start" . (("line" . ,def-line)
                                                            ("character" . ,def-char)))
                                              ("end" . (("line" . ,def-line)
                                                          ("character" . ,(+ def-char (length sym))))))))
                               nil))))))
      ((string= method "textDocument/hover")
       (let* ((doc (%lsp-get params "textDocument"))
              (uri (%lsp-get doc "uri"))
              (pos (%lsp-get params "position"))
              (line (or (%lsp-get pos "line") 0))
              (character (or (%lsp-get pos "character") 0))
              (text (or (gethash uri (lsp-server-documents server)) ""))
              (sym (%lsp-symbol-at-position text line character)))
         `(("jsonrpc" . "2.0")
           ("id" . ,id)
           ("result" . (("contents" . (("kind" . "markdown")
                                         ("value" . ,(if (string= sym "")
                                                          "No symbol at cursor."
                                                          (format nil "`~A` (symbol)" sym))))))))))
      ((string= method "textDocument/inlayHint")
       `(("jsonrpc" . "2.0")
         ("id" . ,id)
         ("result" . ())))
      ((string= method "textDocument/diagnostic")
       (let* ((doc (%lsp-get params "textDocument"))
              (uri (%lsp-get doc "uri"))
              (text (or (gethash uri (lsp-server-documents server)) "")))
         `(("jsonrpc" . "2.0")
           ("id" . ,id)
           ("result" . (("kind" . "full")
                         ("items" . ,(%lsp-build-diagnostics text)))))))
      ((string= method "textDocument/completion")
       (let* ((doc (%lsp-get params "textDocument"))
              (uri (%lsp-get doc "uri"))
              (pos (%lsp-get params "position"))
              (line (or (%lsp-get pos "line") 0))
              (character (or (%lsp-get pos "character") 0))
              (text (or (gethash uri (lsp-server-documents server)) ""))
              (prefix (%lsp-symbol-at-position text line character))
              (items (loop for kw in *lsp-completion-keywords*
                           when (or (string= prefix "")
                                    (search (string-downcase prefix) (string-downcase kw)
                                            :start1 0 :end1 (length prefix)
                                            :test #'char=))
                           collect `(("label" . ,kw)
                                     ("kind" . 14)))))
         `(("jsonrpc" . "2.0")
           ("id" . ,id)
           ("result" . (("isIncomplete" . :false)
                         ("items" . ,items))))))
      (t
       `(("jsonrpc" . "2.0")
         ("id" . ,id)
         ("result" . nil))))))
