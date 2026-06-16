;;;; dcg-rules.lisp — DCG rule transformation and rule generation

(in-package :cl-cc/prolog)

(defvar *dcg-counter* 0
  "Counter for generating fresh DCG state variables.")

(defun dcg-fresh-var ()
  (intern (format nil "?S~D" (incf *dcg-counter*))))

(defun dcg-reset-counter ()
  "Reset the DCG variable counter (for testing)."
  (setf *dcg-counter* 0))

(defun %dcg-tagged-form-p (form name)
  (and (consp form)
       (symbolp (car form))
       (string= (symbol-name (car form)) name)))

(defun %dcg-terminal-goals (terminals s-in s-out)
  "Return the goal sequence that matches TERMINALS against a difference list."
  (if (null terminals)
      (list `(= ,s-in ,s-out))
      (let ((goals nil)
            (current s-in))
        (dolist (term terminals)
          (let ((next (dcg-fresh-var)))
            (push `(dcg-token-match ,term ,current ,next) goals)
            (setf current next)))
        (push `(= ,current ,s-out) goals)
        (nreverse goals))))

(defun %dcg-brace-goal (element s-in s-out)
  (let ((goal (cadr element)))
    (list `(:when ,goal) `(= ,s-in ,s-out))))

(defun dcg-transform-body-element (element s-in s-out)
  "Transform a single DCG body element into a Prolog goal."
  (cond
    ((%dcg-tagged-form-p element "TERMINAL")
     (%dcg-terminal-goals (cdr element) s-in s-out))
    ((symbolp element)
     (list (list element s-in s-out)))
    ((%dcg-tagged-form-p element "BRACE")
     (%dcg-brace-goal element s-in s-out))
    ((and (consp element) (symbolp (car element)))
     (list (append element (list s-in s-out))))
    (t (error "DCG: unknown body element ~S" element))))

(defun dcg-transform-body (body s-in s-out)
  "Transform a DCG body (list of elements) into a list of Prolog goals,
   chaining fresh state variables between elements."
  (if (null body)
      (list `(= ,s-in ,s-out))
      (labels ((walk (elements current acc)
                 (let* ((next (if (null (cdr elements))
                                  s-out
                                  (dcg-fresh-var)))
                        (chunk (dcg-transform-body-element (car elements)
                                                           current
                                                           next))
                        (acc (nconc acc chunk)))
                   (if (cdr elements)
                       (walk (cdr elements) next acc)
                       acc))))
        (walk body s-in nil))))

(defmacro def-dcg-rule (name &body body)
  "Define a DCG rule. Transforms (name --> body...) into a Prolog rule
   with difference-list state threading.
   Usage: (def-dcg-rule expr term (terminal (+)) term)"
  (let ((s-in (gensym "?S-IN"))
        (s-out (gensym "?S-OUT")))
    `(def-rule (,name ,s-in ,s-out)
       ,@(dcg-transform-body body s-in s-out))))
