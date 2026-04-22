;;;; compile/stdlib-source.lisp — Standard Library Source String
;;;;
;;;; Defines *standard-library-source* as a Lisp string consumed by pipeline.lisp.
;;;; Kept in a separate file so the stdlib functions can be read and edited
;;;; without scrolling through the pipeline API code.
;;;;
;;;; Each defun is its own concatenation argument so editors can navigate
;;;; the boundaries between functions easily.

(in-package :cl-cc)

(defparameter *standard-library-source-core*
  (concatenate 'string
    "(defun %stdlib-truthy-p (value)
   (not (or (null value) (eql value 0))))"

    "(defun mapcar (fn lst)
   (if (null lst) nil
     (cons (funcall fn (car lst))
           (mapcar fn (cdr lst)))))"

    "(defun mapc (fn lst)
   (if (null lst) nil
     (progn (funcall fn (car lst))
            (mapc fn (cdr lst))))
   lst)"

    "(defun mapcan (fn lst)
   (if (null lst) nil
     (nconc (funcall fn (car lst))
            (mapcan fn (cdr lst)))))"

    "(defun remove-if (pred lst)
   (if (null lst) nil
      (if (%stdlib-truthy-p (funcall pred (car lst)))
          (remove-if pred (cdr lst))
          (cons (car lst) (remove-if pred (cdr lst))))))"

    "(defun remove-if-not (pred lst)
   (if (null lst) nil
      (if (%stdlib-truthy-p (funcall pred (car lst)))
          (cons (car lst) (remove-if-not pred (cdr lst)))
          (remove-if-not pred (cdr lst)))))"

    "(defun find-if (pred lst)
   (if (null lst) nil
      (if (%stdlib-truthy-p (funcall pred (car lst)))
          (car lst)
          (find-if pred (cdr lst)))))"

    "(defun every (pred lst)
   (if (null lst) t
      (if (%stdlib-truthy-p (funcall pred (car lst)))
          (every pred (cdr lst))
          nil)))"

    "(defun some (pred lst)
   (if (null lst) nil
      (let ((r (funcall pred (car lst))))
        (if (%stdlib-truthy-p r) r (some pred (cdr lst))))))"

    "(defun reduce-init (fn lst acc)
   (if (null lst) acc
     (reduce-init fn (cdr lst) (funcall fn acc (car lst)))))"

    "(defun reduce (fn lst &optional initial-value has-init)
   (if has-init
       (reduce-init fn lst initial-value)
       (if (null (cdr lst)) (car lst)
         (reduce-init fn (cdr (cdr lst))
                      (funcall fn (car lst) (car (cdr lst)))))))"

    "(defun count-if (pred lst)
   (if (null lst) 0
      (+ (if (%stdlib-truthy-p (funcall pred (car lst))) 1 0)
         (count-if pred (cdr lst)))))"

    "(defun position-if (pred lst)
   (labels ((pos-helper (pred lst idx)
               (if (null lst) nil
                 (if (%stdlib-truthy-p (funcall pred (car lst))) idx
                   (pos-helper pred (cdr lst) (+ idx 1))))))
      (pos-helper pred lst 0)))"

    "(defun notevery (pred lst) (not (every pred lst)))"

    "(defun notany (pred lst) (not (some pred lst)))"

    "(defun member-eql (item lst)
   (if (null lst) nil
     (if (eql item (car lst)) lst
       (member-eql item (cdr lst)))))"

    "(defun set-difference (lst1 lst2)
   (if (null lst1) nil
     (if (member-eql (car lst1) lst2)
         (set-difference (cdr lst1) lst2)
         (cons (car lst1) (set-difference (cdr lst1) lst2)))))"

    "(defun union-lists (lst1 lst2)
   (if (null lst1) lst2
     (if (member-eql (car lst1) lst2)
         (union-lists (cdr lst1) lst2)
         (cons (car lst1) (union-lists (cdr lst1) lst2)))))"

    "(defun last-cons (lst)
   (if (null (cdr lst)) lst
     (last-cons (cdr lst))))"

    "(defun append-lists (lst1 lst2)
   (if (null lst1) lst2
     (cons (car lst1) (append-lists (cdr lst1) lst2))))"

    "(defun maphash-fn (fn ht)
   (dolist (k (hash-table-keys ht))
     (funcall fn k (gethash k ht)))
   nil)"

    "(defun getf (plist indicator &optional default)
   (if (null plist) default
     (if (eql (car plist) indicator) (car (cdr plist))
       (getf (cdr (cdr plist)) indicator default))))"

    "(defun intersection (lst1 lst2)
   (if (null lst1) nil
     (if (member-eql (car lst1) lst2)
         (cons (car lst1) (intersection (cdr lst1) lst2))
         (intersection (cdr lst1) lst2))))"

    "(defun remove (item lst)
   (remove-if (lambda (x) (eql x item)) lst))"

    "(defun find (item lst &key key test)
   (let ((test-fn (if test test (lambda (a b) (eql a b)))))
     (dolist (x lst nil)
       (let ((val (if key (funcall key x) x)))
         (when (funcall test-fn item val)
           (return x))))))"

    "(defun position (item lst &key key test)
   (let ((test-fn (if test test (lambda (a b) (eql a b))))
         (idx 0))
     (dolist (x lst nil)
       (let ((val (if key (funcall key x) x)))
         (when (funcall test-fn item val)
           (return idx)))
       (setq idx (+ idx 1)))))"

    "(defun assoc-if (pred alist)
   (if (null alist) nil
     (if (funcall pred (car (car alist))) (car alist)
       (assoc-if pred (cdr alist)))))"

    "(defun assoc (item alist &key test key test-not)
   (let ((test-fn (cond (test-not (complement test-not))
                        (test test)
                        (t #'eql)))
         (key-fn (if key key #'identity)))
     (if (null alist) nil
       (if (funcall test-fn item (funcall key-fn (car (car alist))))
           (car alist)
           (assoc item (cdr alist) :test test-fn :key key-fn)))))"

    "(defun rassoc (item alist)
   (if (null alist) nil
     (if (eql item (cdr (car alist))) (car alist)
       (rassoc item (cdr alist)))))"

    "(defun pairlis (keys values &optional alist)
   (if (null keys) alist
     (cons (cons (car keys) (car values))
           (pairlis (cdr keys) (cdr values) alist))))"
  )
  "Core standard library source — list/sequence baseline definitions.")
