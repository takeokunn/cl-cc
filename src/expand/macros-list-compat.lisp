(in-package :cl-cc)

;;; List / sequence compatibility macros split from macros-stdlib.lisp

;;; ─── SUBST-IF / SUBST-IF-NOT (FR-657) ───────────────────────────────────────

(our-defmacro subst-if (new pred tree)
  "Replace every subtree in TREE for which PRED is true with NEW."
  (let ((fn (gensym "FN")) (n (gensym "NEW")) (rec (gensym "REC")))
    `(let ((,fn ,pred) (,n ,new))
       (labels ((,rec (tr)
                  (cond ((funcall ,fn tr) ,n)
                        ((atom tr) tr)
                        (t (cons (,rec (car tr)) (,rec (cdr tr)))))))
         (,rec ,tree)))))

(our-defmacro subst-if-not (new pred tree)
  "Replace every subtree in TREE for which PRED is false with NEW."
  `(subst-if ,new (complement ,pred) ,tree))

;;; ─── VECTOR constructor (FR-651) ─────────────────────────────────────────────

(our-defmacro vector (&rest args)
  "Create a simple vector with given elements."
  (if (null args)
      '(make-array 0)
      (let ((v (gensym "V")))
        `(let ((,v (make-array ,(length args))))
           ,@(loop for i from 0 for x in args
                   collect `(setf (aref ,v ,i) ,x))
           ,v))))

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

(our-defmacro member-if (pred list &key key)
  "Return the tail of LIST starting from first element satisfying PRED."
  (%member-if-expand pred list key))

(our-defmacro member-if-not (pred list &key key)
  "Return the tail of LIST starting from first element not satisfying PRED."
  (if key
      `(member-if (complement ,pred) ,list :key ,key)
      `(member-if (complement ,pred) ,list)))

;;; ─── MAPHASH (FR-675) ────────────────────────────────────────────────────────

(our-defmacro maphash (fn table)
  "Apply FN to each key-value pair in hash TABLE. Returns nil."
  (let ((fn-var (gensym "FN"))
        (tbl-var (gensym "TBL"))
        (k (gensym "K")))
    `(let ((,fn-var ,fn)
           (,tbl-var ,table))
       (dolist (,k (hash-table-keys ,tbl-var) nil)
         (funcall ,fn-var ,k (gethash ,k ,tbl-var))))))
