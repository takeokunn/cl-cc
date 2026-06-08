;;;; cps.lisp - Bootstrap Continuation-Passing Style Transformation
;;;;
;;;; This module provides:
;;;; - S-expression based CPS transformation (minimal, for bootstrap)
;;;;
;;;; CPS simplification helpers live in cps-simplify.lisp.
;;;; AST-based CPS transformation with full special form support lives in
;;;; cps-ast*.lisp.

(in-package :cl-cc/cps)

(defparameter *enable-trmc* t
  "When true, optimize tail recursion modulo cons in CPS-lowered DEFUN forms.")

;;; Tail Recursion Modulo Cons (TRMC)
;;;
;;; The AST CPS path emits DEFUN forms conservatively.  Before emitting such a
;;; DEFUN, this pass rewrites list-building self recursion of the shape:
;;;
;;;   (cons h1 (cons h2 (f next)))
;;;
;;; into an internal worker with an accumulator:
;;;
;;;   (labels ((worker (... acc)
;;;              ... (worker next (cons h2 (cons h1 acc))) ...))
;;;     (worker ... nil))
;;;
;;; Non-recursive tail exits return (NRECONC ACC result), which is equivalent to
;;; appending the accumulated prefix to the original base result.  The common NIL
;;; base case therefore becomes the requested (NREVERSE ACC) behavior while also
;;; preserving dotted or non-NIL base results.

(defun %trmc-self-call-p (form function-name)
  "Return T when FORM is a direct call to FUNCTION-NAME."
  (and (consp form)
       (eq (car form) function-name)))

(defun %trmc-cons-chain-parts (form function-name)
  "Return recursive call args and cons heads for a TRMC cons chain.
The heads are returned in source order."
  (labels ((walk (current heads)
             (cond
               ((and (consp current)
                     (eq (car current) 'cons)
                     (= (length current) 3))
                (walk (third current) (cons (second current) heads)))
               ((%trmc-self-call-p current function-name)
                (values (cdr current) (nreverse heads) t))
               (t
                (values nil nil nil)))))
    (walk form nil)))

(defun %trmc-tail-cons-chain-p (form function-name)
  "Return T when a tail-position FORM contains a CONS-wrapped chain ending in a
self call. Pure tail calls with no cons heads are NOT TRMC candidates — without
the HEADS check this wrongly transformed plain tail-recursive functions."
  (multiple-value-bind (args heads foundp)
      (%trmc-cons-chain-parts form function-name)
    (declare (ignore args))
    (and foundp (not (null heads)))))

(defun %trmc-sequence-tail-cons-chain-p (forms function-name)
  "Return T when the final form in FORMS is a TRMC candidate."
  (and forms
       (%trmc-tail-candidate-p (car (last forms)) function-name)))

(defun %trmc-tail-candidate-p (form function-name)
  "Return T when FORM has a TRMC cons-chain candidate in tail position."
  (cond
    ((%trmc-tail-cons-chain-p form function-name) t)
    ((and (consp form) (eq (car form) 'if))
     (or (%trmc-tail-candidate-p (third form) function-name)
         (%trmc-tail-candidate-p (fourth form) function-name)))
    ((and (consp form) (eq (car form) 'progn))
     (%trmc-sequence-tail-cons-chain-p (cdr form) function-name))
    ((and (consp form) (member (car form) '(let let*) :test #'eq))
     (%trmc-sequence-tail-cons-chain-p (cddr form) function-name))
    ((and (consp form) (eq (car form) 'block))
     (%trmc-sequence-tail-cons-chain-p (cddr form) function-name))
    (t nil)))

(defun %trmc-body-candidate-p (body function-name)
  "Return T when BODY contains a tail-position TRMC candidate."
  (%trmc-sequence-tail-cons-chain-p body function-name))

(defun %trmc-accumulate-heads-form (heads acc-var)
  "Build the accumulator expression after adding HEADS in source order."
  (reduce (lambda (acc head)
            (list 'cons head acc))
          heads
          :initial-value acc-var))

(defun %trmc-base-return-form (form acc-var)
  "Return FORM with accumulated prefix restored in original order."
  (if (null form)
      (list 'nreverse acc-var)
      (list 'nreconc acc-var form)))

(defun %trmc-worker-call-form (worker-name args acc-form)
  "Build a tail call to WORKER-NAME with ARGS and ACC-FORM."
  (append (list worker-name) args (list acc-form)))

(defun %trmc-cons-chain-call-form (form function-name worker-name acc-var)
  "Rewrite a TRMC cons chain into a worker tail call, or return NIL."
  (multiple-value-bind (args heads foundp)
      (%trmc-cons-chain-parts form function-name)
    (when foundp
      (let ((temps (loop repeat (length heads) collect (gensym "TRMC-HEAD"))))
        (list 'let
              (mapcar #'list temps heads)
              (%trmc-worker-call-form
               worker-name
               args
               (%trmc-accumulate-heads-form temps acc-var)))))))

(defun %trmc-transform-tail-form (form function-name worker-name acc-var)
  "Transform FORM in tail position for a TRMC worker body."
  (or (%trmc-cons-chain-call-form form function-name worker-name acc-var)
      (cond
        ((%trmc-self-call-p form function-name)
         (%trmc-worker-call-form worker-name (cdr form) acc-var))
        ((and (consp form) (eq (car form) 'if))
         (list 'if
               (second form)
               (%trmc-transform-tail-form (third form) function-name worker-name acc-var)
               (%trmc-transform-tail-form (fourth form) function-name worker-name acc-var)))
        ((and (consp form) (eq (car form) 'progn))
         (let ((prefix (butlast (cdr form)))
               (last-form (car (last (cdr form)))))
           (cons 'progn
                 (append prefix
                         (list (%trmc-transform-tail-form last-form function-name worker-name acc-var))))))
        ((and (consp form) (member (car form) '(let let*) :test #'eq))
         (let ((prefix (butlast (cddr form)))
               (last-form (car (last (cddr form)))))
           (append (list (car form) (second form))
                   prefix
                   (list (%trmc-transform-tail-form last-form function-name worker-name acc-var)))))
        ((and (consp form) (eq (car form) 'block))
         (let ((prefix (butlast (cddr form)))
               (last-form (car (last (cddr form)))))
           (append (list 'block (second form))
                   prefix
                   (list (%trmc-transform-tail-form last-form function-name worker-name acc-var)))))
        (t
         (%trmc-base-return-form form acc-var)))))

(defun %trmc-transform-body (body function-name worker-name acc-var)
  "Transform BODY as the body of a TRMC worker."
  (let ((prefix (butlast body))
        (last-form (car (last body))))
    (append prefix
            (list (%trmc-transform-tail-form last-form function-name worker-name acc-var)))))

(defun trmc-transform-defun-form (form)
  "Apply TRMC to a simple DEFUN form when enabled and safe.
Only required-argument functions are rewritten; optional/rest/key lambda lists
are left unchanged to avoid changing call semantics."
  (if (not (and *enable-trmc*
                (consp form)
                (eq (car form) 'defun)))
      form
      (destructuring-bind (defun-symbol name lambda-list &rest body) form
        (declare (ignore defun-symbol))
        (if (or (not (every #'symbolp lambda-list))
                (not (%trmc-body-candidate-p body name)))
            form
            (let ((worker-name (gensym (format nil "~A-TRMC" name)))
                  (acc-var (gensym "TRMC-ACC")))
              (list 'defun name lambda-list
                    (list 'labels
                          (list (append (list worker-name
                                              (append lambda-list (list acc-var)))
                                        (%trmc-transform-body body name worker-name acc-var)))
                          (%trmc-worker-call-form worker-name lambda-list nil))))))))

(defstruct cps-trampoline-thunk
  "A zero-argument CPS trampoline thunk returned from a tail continuation."
  (function (lambda () nil) :type function))

(defun cps-trampoline-run (value)
  "Force CPS trampoline thunks until VALUE is no longer a thunk."
  (loop for current = value then (funcall (cps-trampoline-thunk-function current))
        while (cps-trampoline-thunk-p current)
        finally (return current)))

(defun %cps-tail-funcall-form-p (form)
  "Return T when FORM is a direct tail-position FUNCALL form."
  (and (consp form)
       (eq (car form) 'funcall)))

(defun %cps-trampoline-tail-form (form)
  "Return FORM wrapped as a trampoline thunk for deferred tail dispatch."
  (list 'list
        :cps-trampoline-thunk
        (list 'lambda nil form)))

(defun %cps-wrap-trampoline-run (form)
  "Wrap FORM in a self-contained trampoline loop understood by generated CPS code."
  (list 'labels
        (list (list 'cps-trampoline-run-internal
                    (list 'value)
                    (list 'if
                          (list 'and
                                (list 'consp 'value)
                                (list 'eq (list 'car 'value) :cps-trampoline-thunk))
                          (list 'cps-trampoline-run-internal
                                (list 'funcall (list 'cadr 'value)))
                          'value)))
        (list 'cps-trampoline-run-internal form)))

(defun cps-trampoline-form (form)
  "Convert continuation lambdas whose body is only a tail call into thunk producers."
  (cond
    ((and (consp form)
          (eq (car form) 'lambda)
          (consp (cdr form))
          (consp (second form))
          (null (cddr (second form)))
          (= (length form) 3)
          (%cps-tail-funcall-form-p (third form)))
     (list 'lambda
           (second form)
           (%cps-trampoline-tail-form (cps-trampoline-form (third form)))))
    ((consp form)
     (mapcar #'cps-trampoline-form form))
    (t form)))

;;; S-Expression Based CPS Transformation (Minimal Bootstrap)
;;;
;;; Handles a small subset of CL sufficient for bootstrapping:
;;; integers, symbols, arithmetic (+/-/*), if, progn, let, print.

(defun %cps-sexp-binop (op a b k)
  "CPS-transform (OP A B) — evaluates A then B, applies OP, passes result to K."
  (let ((va (gensym "A"))
        (vb (gensym "B")))
    (%cps-sexp-node a
      (list 'lambda (list va)
            (%cps-sexp-node b
                            (list 'lambda (list vb)
                                  (list 'funcall k (list op va vb))))))))

(defun %cps-sexp-progn (forms k)
  "CPS-transform a sequence of forms, passing only the last result to K."
  (if (null (cdr forms))
      (%cps-sexp-node (car forms) k)
      (let ((tmp (gensym "TMP")))
        (%cps-sexp-node (car forms)
          (list 'lambda (list tmp)
                (list 'declare (list 'ignore tmp))
                (%cps-sexp-progn (cdr forms) k))))))

(defun %cps-sexp-let-bindings (bindings body k)
  "CPS-transform LET bindings left-to-right, then execute BODY with continuation K."
  (if (null bindings)
      (%cps-sexp-node body k)
      (let* ((sym (first  (car bindings)))
             (val (second (car bindings)))
             (tmp (gensym (symbol-name sym))))
        (%cps-sexp-node val
          (list 'lambda (list tmp)
                (list 'let (list (list sym tmp))
                      (%cps-sexp-let-bindings (cdr bindings) body k)))))))

(defun %cps-falsep (value)
  "Return true when VALUE is falsy in the bootstrap CPS language.

Bootstrap CPS follows the language-level truthiness used by compiled code:
both NIL and numeric zero are false."
  (or (null value)
      (and (numberp value)
           (zerop value))))

(defun %cps-sexp-if (node k)
  "CPS-transform an IF form represented by NODE."
  (let ((v (gensym "COND")))
    (%cps-sexp-node (second node)
      (list 'lambda (list v)
            (list 'if (list '%cps-falsep v)
                  (%cps-sexp-node (fourth node) k)
                  (%cps-sexp-node (third node) k))))))

(defun %cps-sexp-print (node k)
  "CPS-transform a PRINT form represented by NODE."
  (let ((v (gensym "PRINT")))
    (%cps-sexp-node (second node)
      (list 'lambda (list v)
            (list 'print v)
            (list 'funcall k v)))))

;;; Adapter functions: bridge the (node k) calling convention to each
;;; implementation function's own argument shape.

(defun %cps-sexp-binop-adapter (node k)
  (%cps-sexp-binop (car node) (second node) (third node) k))

(defun %cps-sexp-progn-adapter (node k)
  (%cps-sexp-progn (cdr node) k))

(defun %cps-sexp-let-adapter (node k)
  (%cps-sexp-let-bindings (second node)
                          (cons 'progn (cddr node))
                          k))

;;; Dispatch table: maps each recognised special form / operator to its adapter.
;;; if and print share the (node k) signature with their impl fns — no adapter needed.

(defun %make-cps-sexp-dispatch-table ()
  "Build the bootstrap CPS dispatch table at load time.

Keeping the source spec local here avoids carrying a second global spec object at
runtime while preserving the data-driven dispatch shape."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (spec '((+     . %cps-sexp-binop-adapter)
                    (-     . %cps-sexp-binop-adapter)
                    (*     . %cps-sexp-binop-adapter)
                    (if    . %cps-sexp-if)
                    (progn . %cps-sexp-progn-adapter)
                    (let   . %cps-sexp-let-adapter)
                    (print . %cps-sexp-print))
                  table)
      (setf (gethash (car spec) table)
            (symbol-function (cdr spec))))))

(defparameter *cps-sexp-dispatch-table*
  (load-time-value (%make-cps-sexp-dispatch-table))
  "Hash table mapping bootstrap CPS special forms/operators to handler functions.")

(defun %cps-sexp-node (node k)
  "CPS-transform a single bootstrap S-expression NODE with continuation K."
  (cond
    ((integerp node) (list 'funcall k node))
    ((symbolp  node) (list 'funcall k node))
    ((consp node)
     (let ((handler (gethash (car node) *cps-sexp-dispatch-table*)))
       (if handler
           (funcall handler node k)
           (error "Unsupported form in CPS: ~S" (car node)))))
    (t
      (error "Unsupported node in CPS: ~S" node))))

;;; CPS simplification helpers live here because the bootstrap transform depends
;;; on them immediately, and keeping them contiguous avoids hidden load-order
;;; coupling in the compile subsystem.

(defun %cps-simplify-subtree (tree var replacement)
  (cond
    ((eq tree var) replacement)
    ((consp tree)
     (cons (%cps-simplify-subtree (car tree) var replacement)
           (%cps-simplify-subtree (cdr tree) var replacement)))
    (t tree)))

(defun %single-param-lambda-parts (form)
  "Return (values param body) when FORM is a one-argument lambda, else NIL values."
  (when (and (consp form)
             (eq (car form) 'lambda)
             (let ((params (second form)))
               (and (consp params) (null (cdr params))))
             (= (length form) 3))
    (values (first (second form))
            (third form))))

(defun %single-param-lambda-p (form)
  "T if FORM is (lambda (x) body) with exactly one parameter."
  (multiple-value-bind (param body)
      (%single-param-lambda-parts form)
    (declare (ignore body))
    (not (null param))))

(defun %funcall-single-lambda-parts (form)
  "Return (values param body arg) when FORM is a beta-reducible funcall, else NIL values."
  (when (and (consp form)
             (eq (car form) 'funcall)
             (not (null (cddr form)))
             (null (cdddr form)))
    (multiple-value-bind (param body)
        (%single-param-lambda-parts (second form))
      (when param
        (values param body (third form))))))

(defun %funcall-of-single-lambda-p (form)
  "T if FORM is (funcall (lambda (x) body) arg) — beta-reducible."
  (multiple-value-bind (param body arg)
      (%funcall-single-lambda-parts form)
    (declare (ignore body arg))
    (not (null param))))

(defun %eta-reducible-lambda-p (form)
  "T if FORM is (lambda (x) (funcall f x)) — eta-reducible."
  (multiple-value-bind (param body)
      (%single-param-lambda-parts form)
    (and param
         (consp body)
         (eq (car body) 'funcall)
         (consp (cddr body))
         (null (cdddr body))
         (eq (third body) param))))

(defun %cps-beta-reduce (form)
  "Beta-reduce (funcall (lambda (x) body) arg) → body[x/arg]."
  (multiple-value-bind (param body arg)
      (%funcall-single-lambda-parts form)
    (if param
        (%cps-simplify-subtree body param arg)
        form)))

(defun %cps-eta-reduce (form)
  "Eta-reduce (lambda (x) (funcall f x)) → f."
  (if (%eta-reducible-lambda-p form)
      (second (third form))
      form))

(defun %cps-simplify-walk (x)
  "Recursively apply beta/eta reduction bottom-up to sexp X."
  (if (consp x)
      (%cps-eta-reduce
       (%cps-beta-reduce
        (mapcar #'%cps-simplify-walk x)))
      x))

(defun %cps-simplify-fixed-point (form step-fn)
  "Apply STEP-FN to FORM until a fixed point is reached."
  (loop
    for current = form then next
    for next = (funcall step-fn current)
    until (equal current next)
    finally (return current)))

(defun cps-simplify-form (form)
  "Repeatedly simplify FORM until a fixed point is reached."
  (%cps-simplify-fixed-point form #'%cps-simplify-walk))

(defun cps-transform (expr)
  "Minimal CPS conversion for bootstrap language. Produces (lambda (k) ...).
The outer continuation parameter is always named K for inspection and tests."
  (let ((body (cps-trampoline-form (%cps-sexp-node expr 'k))))
    (cps-simplify-form (list 'lambda '(k) (%cps-wrap-trampoline-run body)))))
