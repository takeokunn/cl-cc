;;;; optimizer-cps-reduce.lisp --- FR-674 CPS administrative reduction

(in-package :cl-cc/optimize)

(defun %cps-reduce-substitute (tree var replacement)
  (cond
    ((eq tree var) replacement)
    ((consp tree)
     (cons (%cps-reduce-substitute (car tree) var replacement)
           (%cps-reduce-substitute (cdr tree) var replacement)))
    (t tree)))

(defun %cps-reduce-one-arg-lambda (form)
  (when (and (consp form)
             (eq (first form) 'lambda)
             (consp (second form))
             (null (cdr (second form)))
             (= (length form) 3))
    (values (first (second form)) (third form) t)))

(defun %cps-reduce-eta (form)
  "Eta-reduce (lambda (x) (k x)) and (lambda (x) (funcall k x)) to K."
  (multiple-value-bind (param body ok) (%cps-reduce-one-arg-lambda form)
    (if (and ok
             (consp body)
             (or (and (= (length body) 2) (eq (second body) param))
                 (and (= (length body) 3)
                      (eq (first body) 'funcall)
                      (eq (third body) param))))
        (if (eq (first body) 'funcall) (second body) (first body))
        form)))

(defun %cps-reduce-beta-funcall (form)
  "Beta-reduce (funcall (lambda (x) body) value)."
  (if (and (consp form)
           (eq (first form) 'funcall)
           (= (length form) 3))
      (multiple-value-bind (param body ok) (%cps-reduce-one-arg-lambda (second form))
        (if ok (%cps-reduce-substitute body param (third form)) form))
      form))

(defun %cps-reduce-let-continuation (form)
  "Beta-reduce administrative LET continuation bindings.
  (let ((k (lambda (x) body))) (funcall k v)) => (let ((x v)) body)."
  (if (and (consp form)
           (eq (first form) 'let)
           (= (length form) 3)
           (consp (second form))
           (null (cdr (second form))))
      (destructuring-bind (binding) (second form)
        (destructuring-bind (name value) binding
          (multiple-value-bind (param body ok) (%cps-reduce-one-arg-lambda value)
            (if (and ok
                     (consp (third form))
                     (eq (first (third form)) 'funcall)
                     (eq (second (third form)) name)
                     (= (length (third form)) 3))
                (list 'let (list (list param (third (third form)))) body)
                form))))
      form))

(defun %cps-reduce-tail-return (form)
  "Convert tail continuation application to a direct return marker.
Only the canonical identity continuation is eliminated; semantic continuations
remain intact."
  (if (and (consp form)
           (eq (first form) 'funcall)
           (= (length form) 3)
           (member (second form) '(identity values) :test #'eq))
      (third form)
      form))

(defun %cps-reduce-walk (form)
  (let ((walked (if (consp form) (mapcar #'%cps-reduce-walk form) form)))
    (if (consp walked)
        (%cps-reduce-tail-return
         (%cps-reduce-eta
          (%cps-reduce-beta-funcall
           (%cps-reduce-let-continuation walked))))
        walked)))

(defun cps-reduce-form (form)
  "Reduce administrative continuations in CPS S-expression FORM to a fixed point."
  (loop for current = form then next
        for next = (%cps-reduce-walk current)
        until (equal current next)
        finally (return current)))

(defun opt-pass-cps-reduce (instructions)
  "FR-674 optimizer pass entry point.
VM instruction streams are returned unchanged; CPS S-expression pipelines can
call the same pass function directly and receive reduced CPS output."
  (if (and (consp instructions)
           (not (every (lambda (x) (typep x 'vm-instruction)) instructions)))
      (cps-reduce-form instructions)
      instructions))
