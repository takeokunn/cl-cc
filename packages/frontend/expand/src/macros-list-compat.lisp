(in-package :cl-cc/expand)

;;; List / sequence compatibility macros split from macros-stdlib.lisp

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
      (list 'let (list (list fn pred) (list n new))
            (list 'labels
                  (list (list rec '(tr)
                              (list 'cond
                                    (list (list 'funcall fn 'tr) n)
                                    '((atom tr) tr)
                                    (list 't (list 'cons (list rec '(car tr))
                                                   (list rec '(cdr tr)))))))
                  (list rec tree))))))

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
            (cons 'let
                  (cons (list (list v (list 'make-array (length args))))
                        (append (loop for i from 0 for x in args
                                      collect (list 'setf (list 'aref v i) x))
                                (list v)))))))))

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
      (list 'let (list (list fn-var fn)
                       (list tbl-var table))
            (list 'dolist (list k (list 'hash-table-keys tbl-var) nil)
                  (list 'funcall fn-var k (list 'gethash k tbl-var)))))))
