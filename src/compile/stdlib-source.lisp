;;;; compile/stdlib-source.lisp — Standard Library Source String
;;;;
;;;; Defines *standard-library-source* as a Lisp string consumed by pipeline.lisp.
;;;; Kept in a separate file so the stdlib functions can be read and edited
;;;; without scrolling through the pipeline API code.
;;;;
;;;; Each defun is its own concatenation argument so editors can navigate
;;;; the boundaries between functions easily.

(in-package :cl-cc)

(defparameter *standard-library-source*
  (concatenate 'string
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
     (if (funcall pred (car lst))
         (remove-if pred (cdr lst))
         (cons (car lst) (remove-if pred (cdr lst))))))"

    "(defun remove-if-not (pred lst)
   (if (null lst) nil
     (if (funcall pred (car lst))
         (cons (car lst) (remove-if-not pred (cdr lst)))
         (remove-if-not pred (cdr lst)))))"

    "(defun find-if (pred lst)
   (if (null lst) nil
     (if (funcall pred (car lst))
         (car lst)
         (find-if pred (cdr lst)))))"

    "(defun every (pred lst)
   (if (null lst) t
     (if (funcall pred (car lst))
         (every pred (cdr lst))
         nil)))"

    "(defun some (pred lst)
   (if (null lst) nil
     (if (funcall pred (car lst))
         t
         (some pred (cdr lst)))))"

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
     (+ (if (funcall pred (car lst)) 1 0)
        (count-if pred (cdr lst)))))"

    "(defun position-if (pred lst)
   (labels ((pos-helper (pred lst idx)
              (if (null lst) nil
                (if (funcall pred (car lst)) idx
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

    "(defun rassoc (item alist)
   (if (null alist) nil
     (if (eql item (cdr (car alist))) (car alist)
       (rassoc item (cdr alist)))))"

    "(defun pairlis (keys values &optional alist)
   (if (null keys) alist
     (cons (cons (car keys) (car values))
           (pairlis (cdr keys) (cdr values) alist))))"

    "(defun identity (x) x)"

    "(defun constantly (value) (lambda (&rest args) (declare (ignore args)) value))"

    "(defun complement (fn) (lambda (&rest args) (not (apply fn args))))"

    "(defun sort-impl (sequence predicate key)
   (if (null sequence) nil
     (let ((pivot (car sequence))
           (less nil)
           (greater nil))
       (dolist (x (cdr sequence))
         (let ((a (if key (funcall key x) x))
               (b (if key (funcall key pivot) pivot)))
           (if (funcall predicate a b)
               (push x less)
               (push x greater))))
       (append (sort-impl less predicate key)
               (cons pivot (sort-impl greater predicate key))))))"

    "(defun sort (sequence predicate &key key)
   (sort-impl sequence predicate key))"

    "(defun stable-sort (sequence predicate &key key)
   (sort-impl sequence predicate key))"

    "(defun remove-duplicates (lst)
   (let ((result nil))
     (dolist (x lst)
       (unless (member-eql x result)
         (push x result)))
     (nreverse result)))")
  "Standard library source defining higher-order functions and set operations.")
