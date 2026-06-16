;;;; optimizer-delimited-cont.lisp — FR-677 delimited continuation lowering

(in-package :cl-cc/optimize)

(defmacro reset (&body body)
  "Delimited-continuation delimiter used by the FR-677 source-level lowering.

At macroexpansion time this is intentionally just PROGN.  The optimizer pass
recognizes source-like RESET forms and lowers SHIFT occurrences by CPS rewriting."
  `(progn ,@body))

(defmacro shift ((k) &body body)
  "Delimited-continuation capture form recognized by FR-677 CPS lowering."
  (declare (ignore k body))
  (error "SHIFT must appear dynamically inside a RESET form before execution."))

(defun %dc-substitute (tree var replacement)
  (cond
    ((eq tree var) replacement)
    ((consp tree)
     (cons (%dc-substitute (car tree) var replacement)
           (%dc-substitute (cdr tree) var replacement)))
    (t tree)))

(defun %dc-walk (form)
  "Lower nested RESET forms while leaving non-continuation code structural."
  (cond
    ((atom form) form)
    ((eq (first form) 'reset)
     (%dc-reset->direct (rest form)))
    (t (mapcar #'%dc-walk form))))

(defun %dc-apply-continuation (k value)
  (if (eq k 'identity)
      value
      (list 'funcall k value)))

(defun %dc-cps (form k)
  "Basic one-shot CPS transform for source-like SHIFT/RESET S-expressions."
  (cond
    ((atom form)
     (%dc-apply-continuation k form))
    ((and (consp form) (eq (first form) 'reset))
     (%dc-apply-continuation k (%dc-reset->direct (rest form))))
    ((and (consp form) (eq (first form) 'shift))
     (destructuring-bind (_shift (captured-k) &body body) form
       (declare (ignore _shift))
       (let ((arg (gensym "SHIFT-VALUE")))
         (%dc-reset->direct
          (mapcar (lambda (body-form)
                    (%dc-substitute body-form captured-k
                                    `(lambda (,arg) ,(%dc-apply-continuation k arg))))
                  body)))))
    ((and (consp form) (eq (first form) 'if) (= (length form) 4))
     `(if ,(second form)
          ,(%dc-cps (third form) k)
          ,(%dc-cps (fourth form) k)))
    ((and (consp form) (eq (first form) 'progn))
     (%dc-cps-progn (rest form) k))
    ((and (consp form) (eq (first form) 'let))
     (destructuring-bind (_let bindings &body body) form
       (declare (ignore _let))
       `(let ,(mapcar (lambda (binding)
                        (if (consp binding)
                            (list (first binding) (%dc-walk (second binding)))
                            binding))
                      bindings)
          ,(%dc-cps-progn body k))))
    (t
     (%dc-apply-continuation k (mapcar #'%dc-walk form)))))

(defun %dc-cps-progn (forms k)
  (cond
    ((null forms) (%dc-apply-continuation k nil))
    ((null (rest forms)) (%dc-cps (first forms) k))
    (t `(progn ,(%dc-walk (first forms))
               ,(%dc-cps-progn (rest forms) k)))))

(defun %dc-reset->direct (body)
  "Lower a RESET body to direct style using IDENTITY as the delimiter answer type."
  (%dc-cps-progn body 'identity))

(defun opt-delimited-continuations-form (form)
  "Lower source-like SHIFT/RESET forms in FORM using a small CPS transform."
  (loop for current = form then next
        for next = (%dc-walk current)
        until (equal current next)
        finally (return next)))

(defun opt-pass-delimited-continuations (instructions)
  "FR-677 explicit pass for delimited continuations.

VM instruction streams are returned unchanged.  Source-like S-expression callers
can run the pass explicitly to lower RESET/SHIFT into continuation-passing direct
forms.  This pass is registered by ASDF/package exports only and is not part of
the default optimizer convergence pipeline."
  (if (and (consp instructions)
           (not (every (lambda (x) (typep x 'vm-instruction)) instructions)))
      (opt-delimited-continuations-form instructions)
      instructions))
