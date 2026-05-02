;;;; tests/unit/expand/expander-definitions-tests.lisp — Definition-form expander tests

(in-package :cl-cc/test)

(defsuite expander-definitions-suite
  :description "Definition-form expander unit tests"
  :parent cl-cc-unit-suite)

(in-suite expander-definitions-suite)

(deftest expander-docstring-helpers
  "%strip-docstring and %extract-docstring handle leading documentation strings."
  (let* ((body '("Doc" (foo) (bar)))
         (stripped (cl-cc/expand::%strip-docstring body)))
    (assert-equal '((foo) (bar)) stripped)
    (assert-equal "Doc" (cl-cc/expand::%extract-docstring body))))

(deftest expander-defsetf-short-form-registers-updater
  "defsetf short form registers an updater and rewrites setf to the updater call."
  (let ((accessor 'expander-defsetf-short-accessor)
        (updater 'expander-defsetf-short-updater))
    (cl-cc/expand:compiler-macroexpand-all `(defsetf ,accessor ,updater))
    (assert-true (gethash accessor cl-cc/expand::*setf-compound-place-handlers*))
    (let ((result (cl-cc/expand:compiler-macroexpand-all `(setf (,accessor obj arg) value))))
      (assert-eq updater (car result))
      (assert-equal '(obj arg value) (cdr result)))))

(deftest expander-defconstant-populates-constant-table
  "defconstant side-effect: value is stored in *constant-table* for constant folding."
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(defconstant +expander-def-constant+ 42))))
    (assert-eq 'defparameter (car result))
    (assert-equal 42 (third result))
    (assert-equal 42 (gethash '+expander-def-constant+ cl-cc/expand:*constant-table*))))

(deftest expander-define-symbol-macro-and-get-decoded-time
  "define-symbol-macro populates the symbol macro table and get-decoded-time expands to decode-universal-time."
  (let* ((name 'expander-definitions-symbol-macro)
         (expansion '(+ 1 2))
         (result (cl-cc/expand:compiler-macroexpand-all `(define-symbol-macro ,name ,expansion))))
    (assert-eq 'quote (car result))
    (assert-eq name (second result))
    (assert-equal expansion (gethash name cl-cc/expand:*symbol-macro-table*)))
  (assert-equal '(decode-universal-time (get-universal-time))
                (cl-cc/expand:compiler-macroexpand-all '(get-decoded-time))))
