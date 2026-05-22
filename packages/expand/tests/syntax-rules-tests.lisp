(in-package :cl-cc/test)

(defsuite syntax-rules-suite
  :description "Runtime-stdlib-2 syntax-rules scaffold tests"
  :parent cl-cc-unit-suite)

(in-suite syntax-rules-suite)

(deftest syntax-rules-source-scaffold-loads
  "The syntax-rules source is wired into :cl-cc-expand."
  :timeout 5
  (assert-true (asdf:find-system :cl-cc-expand nil)))

(deftest syntax-rules-define-syntax-basic-expansion
  "FR-804: DEFINE-SYNTAX registers a syntax-rules transformer."
  :timeout 10
  (cl-cc/expand:define-syntax sr-when
    (cl-cc/expand:syntax-rules ()
      ((sr-when test body |...|) (if test (progn body |...|)))))
  (multiple-value-bind (expanded expanded-p)
      (cl-cc/expand:our-macroexpand-1 '(sr-when ok (print 1) (print 2)))
    (assert-true expanded-p)
    (assert-equal '(if ok (progn (print 1) (print 2))) expanded)))

(deftest syntax-rules-hygiene-introduces-uninterned-symbol
  "FR-804/805: identifiers introduced by syntax-rules are gensym-hygienic."
  :timeout 10
  (cl-cc/expand:define-syntax sr-capture-safe
    (cl-cc/expand:syntax-rules ()
      ((sr-capture-safe expr) (let ((tmp expr)) tmp))))
  (let ((expanded (cl-cc/expand:our-macroexpand-1 '(sr-capture-safe tmp))))
    (assert-eq 'let (car expanded))
    (assert-true (null (symbol-package (caar (second expanded)))))))

(deftest syntax-case-partial-supports-guarded-pattern
  "FR-804: SYNTAX-CASE partial support matches patterns with guards."
  :timeout 10
  (let ((result (cl-cc/expand:syntax-case '(twice 21) ()
                  ((twice x) (numberp x) `(+ ,x ,x)))))
    (assert-equal '(+ 21 21) result)))
