;;;; tests/framework-meta.lisp — CL-CC Test Framework (Meta-Testing)
;;;; Mutation testing, metamorphic testing, invariants.
;;;; Coverage helpers and TAP output are in framework-tap.lisp.

;;; Load sb-cover contrib before the reader encounters sb-cover: symbols.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (ignore-errors (require :sb-cover)))

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Notes on shared state (defined in framework.lisp)
;;; ------------------------------------------------------------

;;; definvariant is defined in framework.lisp
;;; *invariant-registry* is defined in framework.lisp
;;; *metamorphic-relations* is defined in framework.lisp
;;; *tap-mutex* is defined in framework.lisp
;;; %fail-test is defined in framework.lisp

;;; ------------------------------------------------------------
;;; Metamorphic Testing (FR-036)
;;; ------------------------------------------------------------

(defmacro defmetamorphic (name &key transform relation applicable-when)
  "Define a metamorphic relation. At suite-run time, applies TRANSFORM to
   all test expressions that match APPLICABLE-WHEN, and verifies RELATION
   holds between original and transformed results.

   Example:
     (defmetamorphic commutativity
       :transform (lambda (expr)
         (destructuring-bind (op a b) (cdr expr)
           `(,op ,b ,a)))
       :relation #'=
       :applicable-when (lambda (expr)
         (member (car expr) '(+ *))))"
  `(progn
     (push (list :name ',name
                 :transform ,transform
                 :relation ,relation
                 :applicable-when ,applicable-when)
           *metamorphic-relations*)
     ',name))

(defun %verify-metamorphic-relations (expressions)
  "Check all registered metamorphic relations against the given expressions.
   Signals test-failure via %fail-test if any relation is violated."
  (dolist (relation *metamorphic-relations*)
    (let ((transform      (getf relation :transform))
          (rel-fn         (getf relation :relation))
          (applicable-when (getf relation :applicable-when)))
      (dolist (expr expressions)
        (when (and applicable-when (ignore-errors (funcall applicable-when expr)))
          (let* ((transformed        (ignore-errors (funcall transform expr)))
                 (original-result    (ignore-errors (eval expr)))
                 (transformed-result (ignore-errors (eval transformed))))
            (when (and original-result transformed-result)
              (unless (ignore-errors (funcall rel-fn original-result transformed-result))
                (%fail-test (format nil "Metamorphic relation ~S violated"
                                    (getf relation :name))
                            :expected (list expr original-result)
                            :actual   (list transformed transformed-result))))))))))

