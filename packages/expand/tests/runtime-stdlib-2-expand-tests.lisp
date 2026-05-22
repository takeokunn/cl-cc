(in-package :cl-cc/test)

(defsuite runtime-stdlib-2-expand-suite
  :description "Runtime-stdlib-2 macro expansion scaffold tests"
  :parent cl-cc-unit-suite)

(in-suite runtime-stdlib-2-expand-suite)

(deftest runtime-stdlib-2-expand-system-loads
  "The expand system remains loadable with runtime-stdlib-2 scaffolding."
  :timeout 5
  (assert-true (asdf:find-system :cl-cc-expand nil)))

(deftest runtime-stdlib-2-delay-force-caches-nil
  "FR-856: FORCE is idempotent and caches NIL values."
  :timeout 10
  (let* ((calls 0)
         (promise (eval '(cl-cc/expand::%make-promise (lambda () (incf calls) nil) nil nil))))
    (assert-true (cl-cc/expand:promisep promise))
    (assert-null (cl-cc/expand:force promise))
    (assert-null (cl-cc/expand:force promise))
    (assert-= 1 calls)))

(deftest runtime-stdlib-2-memoize-stats-and-clear
  "FR-857: MEMOIZE tracks hits/misses and supports cache clearing."
  :timeout 10
  (let* ((calls 0)
         (fn (cl-cc/expand:memoize (lambda (x) (incf calls) (* x x)))))
    (assert-= 9 (funcall fn 3))
    (assert-= 9 (funcall fn 3))
    (let ((stats (cl-cc/expand:memoize-stats fn)))
      (assert-= 1 (getf stats :hits))
      (assert-= 1 (getf stats :misses))
      (assert-= 1 (getf stats :size)))
    (assert-true (cl-cc/expand:memoize-clear fn))
    (assert-= 9 (funcall fn 3))
    (assert-= 2 calls)))

(deftest runtime-stdlib-2-quasiquote-folds-static-splice-nil
  "FR-908: quasiquote expansion folds static parts and ,@NIL."
  :timeout 10
  (assert-equal '(list 'a 'b 'c)
                (cl-cc/expand:our-macroexpand-all '(backquote (a b (unquote-splicing nil) c)))))

(deftest runtime-stdlib-2-quasiquote-single-splice-copy-list
  "FR-908: `(,@list) expands to COPY-LIST."
  :timeout 10
  (assert-equal '(copy-list xs)
                (cl-cc/expand:our-macroexpand-all '(backquote ((unquote-splicing xs))))))
