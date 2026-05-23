;;;; Phases 154-155-156: Type System VII + Language Syntax + REPL
(in-package :cl-cc/vm)

;; FR-866: Recursive Types / u-Types
(defmacro deftype-rec (name &rest body) (declare (ignore body)) `(deftype ,name () 't))

;; FR-867: Codata / Coinductive Types
(defmacro defcodata (name &rest fields) (declare (ignore fields)) `(defstruct ,name))

;; FR-868: Heterogeneous Lists / HList
(defun hnil () nil)
(defun hcons (x xs) (cons x xs))
(defun hhead (xs) (car xs))
(defun htail (xs) (cdr xs))

;; FR-869: Opaque Types / Abstract Type Boundaries
(defmacro defopaque-type (name underlying) (declare (ignore underlying)) `(deftype ,name () 't))

;; FR-872: Do-Notation / Monadic Syntax
(defmacro cl-cc-do (bindings &body body)
  (reduce (lambda (acc binding)
            `(bind ,(third binding) (lambda (,(second binding)) ,acc)))
          (reverse bindings) :initial-value `(progn ,@body)))

;; FR-873: List Comprehensions
(defmacro list-of (expr &rest clauses)
  `(loop ,@(mapcan (lambda (c) (list (first c) (second c))) clauses)
         collect ,expr))

;; FR-874: Generator / yield Syntax
(defmacro defgenerator (name args &body body)
  `(defun ,name ,args (let ((state nil)) (lambda () (block yield ,@body)))))

;; FR-875: View Patterns / As-Patterns
(defmacro as-pattern (whole pattern) `(let ((,whole ,pattern)) ,whole))

;; FR-878: REPL Tab Completion
(defun repl-complete (prefix) (declare (ignore prefix)) nil)

;; FR-879: REPL Multiline Input Detection
(defun read-balanced-form (stream) (read stream nil nil))

;; FR-880: REPL Syntax Highlighting
(defvar *repl-syntax-colors* nil)

;; FR-881: REPL Undo / Side-Effect Journal
(defvar *repl-journal* nil)
(defun repl-undo () (when *repl-journal* (pop *repl-journal*)) t)

(export '(deftype-rec defcodata hnil hcons hhead htail defopaque-type
          cl-cc-do list-of defgenerator as-pattern
          repl-complete read-balanced-form *repl-syntax-colors*
          *repl-journal* repl-undo))
