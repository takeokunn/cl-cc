;;;; optimizer-trmc.lisp — Tail Recursion Modulo Cons (FR-1025)

(in-package :cl-cc/optimize)

(defparameter *opt-enable-trmc* t
  "Enable Tail Recursion Modulo Cons rewrites for source-level DEFUN forms.

This optimizer recognizes tail-position list construction whose final cdr is a
direct self-recursive call, for example `(cons (car l) (copy-list (cdr l)))`.
The emitted worker is tail-recursive and uses an accumulator plus NRECONC/NREVERSE,
which is equivalent to the forward-pointer technique used by native backends:
the list spine is produced in O(1) stack space and the final cdr is patched by
NRECONC rather than growing the host/control stack.")

(defun opt-trmc-enabled-declaration-p (declarations)
  "Return true when DECLARATIONS enable `(optimize (tail-recursion-modulo-cons t))`."
  (some (lambda (form)
          (and (consp form)
               (eq (car form) 'declare)
               (some (lambda (decl)
                       (and (consp decl)
                            (eq (car decl) 'optimize)
                            (some (lambda (quality)
                                    (and (consp quality)
                                         (eq (car quality) 'tail-recursion-modulo-cons)
                                         (second quality)))
                                  (cdr decl))))
                     (cdr form))))
        declarations))

(defun %opt-trmc-self-call-p (form function-name)
  (and (consp form) (eq (car form) function-name)))

(defun %opt-trmc-list*-parts (form function-name)
  "Return recursive args and heads for `(list* ... (self ...))`."
  (when (and (consp form) (eq (car form) 'list*) (cddr form))
    (let ((tail (car (last (cdr form))))
          (heads (butlast (cdr form))))
      (when (%opt-trmc-self-call-p tail function-name)
        (values (cdr tail) heads t)))))

(defun %opt-trmc-cons-parts (form function-name)
  "Return recursive call args and source-order cons heads for a TRMC chain."
  (or (%opt-trmc-list*-parts form function-name)
      (labels ((walk (current heads)
                 (cond
                   ((and (consp current) (eq (car current) 'cons) (= (length current) 3))
                    (walk (third current) (cons (second current) heads)))
                   ((%opt-trmc-self-call-p current function-name)
                    (values (cdr current) (nreverse heads) t))
                   (t (values nil nil nil)))))
        (walk form nil))))

(defun opt-trmc-tail-candidate-p (form function-name)
  "Return true when FORM contains a TRMC candidate in tail position."
  (multiple-value-bind (_args _heads foundp)
      (%opt-trmc-cons-parts form function-name)
    (declare (ignore _args _heads))
    (or foundp
        (and (consp form)
             (case (car form)
               (if (or (opt-trmc-tail-candidate-p (third form) function-name)
                       (opt-trmc-tail-candidate-p (fourth form) function-name)))
               (progn (opt-trmc-body-candidate-p (cdr form) function-name))
               ((let let*) (opt-trmc-body-candidate-p (cddr form) function-name))
               (block (opt-trmc-body-candidate-p (cddr form) function-name))
               (otherwise nil))))))

(defun opt-trmc-body-candidate-p (body function-name)
  (and body (opt-trmc-tail-candidate-p (car (last body)) function-name)))

(defun %opt-trmc-accumulate (heads acc-var)
  (reduce (lambda (acc head) `(cons ,head ,acc)) heads :initial-value acc-var))

(defun %opt-trmc-base-return (form acc-var)
  (if (null form) `(nreverse ,acc-var) `(nreconc ,acc-var ,form)))

(defun %opt-trmc-worker-call (worker args acc-form)
  (append (list worker) args (list acc-form)))

(defun %opt-trmc-cons-call (form function-name worker acc-var)
  (multiple-value-bind (args heads foundp)
      (%opt-trmc-cons-parts form function-name)
    (when foundp
      (let ((temps (loop repeat (length heads) collect (gensym "TRMC-HEAD"))))
        `(let ,(mapcar #'list temps heads)
           ,(%opt-trmc-worker-call worker args (%opt-trmc-accumulate temps acc-var)))))))

(defun opt-trmc-transform-tail-form (form function-name worker acc-var)
  "Rewrite tail-position FORM for a TRMC worker."
  (or (%opt-trmc-cons-call form function-name worker acc-var)
      (cond
        ((%opt-trmc-self-call-p form function-name)
         (%opt-trmc-worker-call worker (cdr form) acc-var))
        ((and (consp form) (eq (car form) 'if))
         `(if ,(second form)
              ,(opt-trmc-transform-tail-form (third form) function-name worker acc-var)
              ,(opt-trmc-transform-tail-form (fourth form) function-name worker acc-var)))
        ((and (consp form) (eq (car form) 'progn))
         (append '(progn)
                 (butlast (cdr form))
                 (list (opt-trmc-transform-tail-form (car (last (cdr form))) function-name worker acc-var))))
        ((and (consp form) (member (car form) '(let let*) :test #'eq))
         (append (list (car form) (second form))
                 (butlast (cddr form))
                 (list (opt-trmc-transform-tail-form (car (last (cddr form))) function-name worker acc-var))))
        ((and (consp form) (eq (car form) 'block))
         (append (list 'block (second form))
                 (butlast (cddr form))
                 (list (opt-trmc-transform-tail-form (car (last (cddr form))) function-name worker acc-var))))
        (t (%opt-trmc-base-return form acc-var)))))

(defun opt-trmc-transform-defun-form (form &key force)
  "Transform a DEFUN form with TRMC when enabled by declaration or FORCE."
  (if (not (and *opt-enable-trmc* (consp form) (eq (car form) 'defun)))
      form
      (destructuring-bind (_defun name lambda-list &rest body) form
        (declare (ignore _defun))
        (let* ((decls (loop while (and body (consp (first body)) (eq (caar body) 'declare))
                            collect (pop body)))
               (enabled (or force (opt-trmc-enabled-declaration-p decls))))
          (if (not (and enabled
                        (every #'symbolp lambda-list)
                        (opt-trmc-body-candidate-p body name)))
              form
              (let ((worker (gensym (format nil "~A-TRMC" name)))
                    (acc-var (gensym "TRMC-ACC")))
                `(defun ,name ,lambda-list
                   ,@decls
                   (labels ((,worker ,(append lambda-list (list acc-var))
                              ,@(append (butlast body)
                                        (list (opt-trmc-transform-tail-form
                                               (car (last body)) name worker acc-var)))))
                     ,(%opt-trmc-worker-call worker lambda-list nil)))))))))

(defun opt-trmc-transform-form (form)
  "Transform DEFUN forms and recursively scan top-level PROGN/EVAL-WHEN wrappers."
  (cond
    ((and (consp form) (eq (car form) 'defun)) (opt-trmc-transform-defun-form form))
    ((and (consp form) (member (car form) '(progn eval-when) :test #'eq))
     (cons (car form) (mapcar #'opt-trmc-transform-form (cdr form))))
    (t form)))

(defun opt-pass-trmc (instructions)
  "Apply FR-1025 TRMC to source DEFUN forms stored in VM constants when present.

Most cl-cc pipelines run TRMC before VM lowering, but this pass gives the optimizer
registry a concrete hook and preserves non-source VM instruction streams unchanged."
  (let ((changed nil))
    (values
     (mapcar (lambda (inst)
               (if (and (typep inst 'vm-const) (consp (vm-value inst)))
                   (let ((new (opt-trmc-transform-form (vm-value inst))))
                     (if (equal new (vm-value inst))
                         inst
                         (progn
                           (setf changed t)
                           (make-vm-const :dst (vm-dst inst) :value new))))
                   inst))
             instructions)
     changed)))
