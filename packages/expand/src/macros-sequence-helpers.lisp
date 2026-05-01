(in-package :cl-cc/expand)

;;; List / sequence helper macros split from macros-stdlib.lisp

;;; ─── SUBST-IF / SUBST-IF-NOT (FR-657) ───────────────────────────────────────

(register-macro 'subst-if
  (lambda (form env)
    (declare (ignore env))
    (let ((new (second form))
          (pred (third form))
          (tree (fourth form))
          (fn (gensym "FN"))
          (n (gensym "NEW"))
          (rec (gensym "REC")))
      `(let ((,fn ,pred) (,n ,new))
         (labels ((,rec (tr)
                    (cond ((funcall ,fn tr) ,n)
                          ((atom tr) tr)
                          (t (cons (,rec (car tr)) (,rec (cdr tr)))))))
           (,rec ,tree))))))

(register-macro 'subst-if-not
  (lambda (form env)
    (declare (ignore env))
    (list 'subst-if (second form) (list 'complement (third form)) (fourth form))))

;;; ─── VECTOR constructor (FR-651) ─────────────────────────────────────────────

(register-macro 'vector
  (lambda (form env)
    (declare (ignore env))
    (let ((args (cdr form)))
      (if (null args)
          '(make-array 0)
          (let ((v (gensym "V")))
            `(let ((,v (make-array ,(length args))))
               ,@(loop for i from 0 for x in args
                       collect `(setf (aref ,v ,i) ,x))
               ,v))))))

;;; ─── MEMBER-IF / MEMBER-IF-NOT (FR-499) ──────────────────────────────────────

(defun %member-if-expand (pred list key)
  (let ((fn (gensym "FN")) (tail (gensym "TAIL")))
    (if key
        (let ((kfn (gensym "KEY")))
          `(let ((,fn ,pred) (,kfn ,key))
             (do ((,tail ,list (cdr ,tail)))
                 ((null ,tail) nil)
               (when (funcall ,fn (funcall ,kfn (car ,tail)))
                 (return ,tail)))))
        `(let ((,fn ,pred))
           (do ((,tail ,list (cdr ,tail)))
               ((null ,tail) nil)
             (when (funcall ,fn (car ,tail))
               (return ,tail)))))))

(register-macro 'member-if
  (lambda (form env)
    (declare (ignore env))
    (%member-if-expand (second form) (third form) (getf (cdddr form) :key))))

(register-macro 'member-if-not
  (lambda (form env)
    (declare (ignore env))
    (let ((pred (second form))
          (list-form (third form))
          (key (getf (cdddr form) :key)))
      (if key
          (list 'member-if (list 'complement pred) list-form :key key)
          (list 'member-if (list 'complement pred) list-form)))))

;;; ─── MAPHASH (FR-675) ────────────────────────────────────────────────────────

(register-macro 'maphash
  (lambda (form env)
    (declare (ignore env))
    (let ((fn (second form))
          (table (third form))
          (fn-var (gensym "FN"))
          (tbl-var (gensym "TBL"))
          (k (gensym "K")))
      `(let ((,fn-var ,fn) (,tbl-var ,table))
         (dolist (,k (hash-table-keys ,tbl-var) nil)
           (funcall ,fn-var ,k (gethash ,k ,tbl-var)))))))
