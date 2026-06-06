;;;; cps-trmc.lisp — Tail Recursion Modulo Cons (TRMC) for CPS-lowered DEFUN forms
;;;;
;;;; This module provides:
;;;; - *enable-trmc* parameter
;;;; - All %trmc-* helper functions
;;;; - trmc-transform-defun-form — the public TRMC rewrite entry point
;;;;
;;;; Load order: before cps.lisp (cps.lisp calls cps-trampoline-form which
;;;; does not depend on TRMC; this file is self-contained).
;;;;
;;;; PARALLEL IMPLEMENTATION NOTE:
;;;; packages/optimize/src/optimizer-trmc.lisp provides a more capable parallel
;;;; version of this algorithm:  it handles list* chains
;;;; (opt-trmc-list*-parts), strips declare forms before scanning the body, and
;;;; exposes opt-pass-trmc as a VM optimizer pass.  When changing the TRMC
;;;; algorithm here — new shape recognizers, different accumulator structure,
;;;; etc. — keep the two files in sync.  Conversely, new optimizer-trmc.lisp
;;;; features should usually be back-ported here as well.

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

(defun %trmc-list*-chain-parts (form function-name)
  "Return recursive call args and source-order heads for a (list* h... (self ...)) chain.
Returns three values: args heads foundp, like %trmc-cons-chain-parts."
  (when (and (consp form) (eq (car form) 'list*) (cddr form))
    (let ((tail (car (last (cdr form))))
          (heads (butlast (cdr form))))
      (when (%trmc-self-call-p tail function-name)
        (values (cdr tail) heads t)))))

(defun %trmc-cons-chain-parts (form function-name)
  "Return recursive call args and cons heads for a TRMC cons or list* chain.
The heads are returned in source order."
  (or (%trmc-list*-chain-parts form function-name)
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
        (walk form nil))))

(defun %trmc-tail-cons-chain-p (form function-name)
  "Return T when a tail-position FORM contains a CONS-wrapped chain ending in self call.
Pure tail calls with no cons heads are NOT TRMC candidates."
  (multiple-value-bind (args heads foundp)
      (%trmc-cons-chain-parts form function-name)
    (declare (ignore args))
    (and foundp (not (null heads)))))

(defun %trmc-tail-candidate-p (form function-name)
  "Return T when FORM has a TRMC cons-chain candidate in tail position."
  (cond
    ((%trmc-tail-cons-chain-p form function-name) t)
    ((and (consp form) (eq (car form) 'if))
     (or (%trmc-tail-candidate-p (third form) function-name)
         (%trmc-tail-candidate-p (fourth form) function-name)))
    ((and (consp form) (eq (car form) 'progn))
     (let ((forms (cdr form)))
       (and forms (%trmc-tail-candidate-p (car (last forms)) function-name))))
    ((and (consp form) (member (car form) '(let let*) :test #'eq))
     (let ((forms (cddr form)))
       (and forms (%trmc-tail-candidate-p (car (last forms)) function-name))))
    ((and (consp form) (eq (car form) 'block))
     (let ((forms (cddr form)))
       (and forms (%trmc-tail-candidate-p (car (last forms)) function-name))))
    (t nil)))

(defun %trmc-body-candidate-p (body function-name)
  "Return T when BODY contains a tail-position TRMC candidate."
  (and body (%trmc-tail-candidate-p (car (last body)) function-name)))

(defun %trmc-accumulate-heads-form (heads acc-var)
  "Build the accumulator expression after adding HEADS in source order."
  (reduce (lambda (acc head) `(cons ,head ,acc)) heads :initial-value acc-var))

(defun %trmc-base-return-form (form acc-var)
  "Return FORM with accumulated prefix restored in original order."
  (if (null form)
      `(nreverse ,acc-var)
      `(nreconc ,acc-var ,form)))

(defun %trmc-worker-call-form (worker-name args acc-form)
  "Build a tail call to WORKER-NAME with ARGS and ACC-FORM."
  `(,worker-name ,@args ,acc-form))

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
