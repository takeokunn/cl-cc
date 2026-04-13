;;;; macros-hof-search.lisp — Position, count, and association HOF macros
;;;
;;; Extracted from macros-hof.lisp.
;;; Contains: position, count, count-if, find-if-not, position-if,
;;;           position-if-not, count-if-not, assoc-if, assoc, assoc-if-not,
;;;           rassoc-if, rassoc-if-not.
;;;
;;; Depends on macros-hof.lisp (complement, our-defmacro, dolist).
;;; Load order: immediately after macros-hof.lisp.

(in-package :cl-cc)

;; POSITION: index of first element eql to item, or nil (with optional keywords)
(our-defmacro position (item list &key test key test-not)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (idx (gensym "IDX")))
    (if (or test key test-not)
        (let ((tst-var (gensym "TST")) (kfn-var (gensym "KEY")))
          `(let ((,item-var ,item) (,idx 0)
                 (,tst-var ,(cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
                 (,kfn-var ,(or key '#'identity)))
             (block nil
               (dolist (,x ,list nil)
                 (when (funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
                   (return ,idx))
                 (setq ,idx (+ ,idx 1))))))
        `(let ((,item-var ,item) (,idx 0))
           (block nil
             (dolist (,x ,list nil)
               (when (eql ,item-var ,x)
                 (return ,idx))
               (setq ,idx (+ ,idx 1))))))))

;; COUNT: number of elements eql to item (with optional :test/:key)
(defun %count-key-expand (item list test key test-not)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (cnt (gensym "CNT"))
        (tst-var (gensym "TST"))
        (kfn-var (gensym "KEY")))
    `(let ((,item-var ,item)
           (,cnt 0)
           (,tst-var ,(cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
           (,kfn-var ,(or key '#'identity)))
       (dolist (,x ,list ,cnt)
         (when (funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
           (setq ,cnt (+ ,cnt 1)))))))

(our-defmacro count (item list &key test key test-not)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (cnt (gensym "CNT")))
    (if (or test key test-not)
        (%count-key-expand item list test key test-not)
        `(let ((,item-var ,item) (,cnt 0))
           (dolist (,x ,list ,cnt)
             (when (eql ,item-var ,x)
               (setq ,cnt (+ ,cnt 1))))))))

;; COUNT-IF: number of elements for which pred is true (with optional :key)
(defun %count-if-key-expand (pred list key)
  (let ((fn-var (gensym "FN"))
        (kfn (gensym "KEY"))
        (x (gensym "X"))
        (cnt (gensym "CNT")))
    `(let ((,fn-var ,pred)
           (,kfn ,key)
           (,cnt 0))
       (dolist (,x ,list ,cnt)
         (when (funcall ,fn-var (funcall ,kfn ,x))
           (setq ,cnt (+ ,cnt 1)))))))

(our-defmacro count-if (pred list &key key)
  (if key
      (%count-if-key-expand pred list key)
      (let ((fn-var (gensym "FN")) (x (gensym "X")) (cnt (gensym "CNT")))
        `(let ((,fn-var ,pred) (,cnt 0))
           (dolist (,x ,list ,cnt)
             (when (funcall ,fn-var ,x)
               (setq ,cnt (+ ,cnt 1))))))))

;; FIND-IF-NOT: first element for which pred is false (FR-610), with optional :key
(our-defmacro find-if-not (pred list &key key)
  (if key
      `(find-if (complement ,pred) ,list :key ,key)
      `(find-if (complement ,pred) ,list)))

;; POSITION-IF: index of first element satisfying pred (FR-610), with optional :key
(defun %position-if-key-expand (pred list key)
  (let ((fn-var (gensym "FN"))
        (kfn (gensym "KEY"))
        (x (gensym "X"))
        (idx (gensym "IDX")))
    `(let ((,fn-var ,pred)
           (,kfn ,key)
           (,idx 0))
       (block nil
         (dolist (,x ,list nil)
           (when (funcall ,fn-var (funcall ,kfn ,x))
             (return ,idx))
           (setq ,idx (+ ,idx 1)))))))

(our-defmacro position-if (pred list &key key)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (idx (gensym "IDX")))
    (if key
        (%position-if-key-expand pred list key)
        `(let ((,fn-var ,pred) (,idx 0))
           (block nil
             (dolist (,x ,list nil)
               (when (funcall ,fn-var ,x)
                 (return ,idx))
               (setq ,idx (+ ,idx 1))))))))

;; POSITION-IF-NOT: index of first element not satisfying pred (FR-610), with optional :key
(our-defmacro position-if-not (pred list &key key)
  (if key
      `(position-if (complement ,pred) ,list :key ,key)
      `(position-if (complement ,pred) ,list)))

;; COUNT-IF-NOT: count elements for which pred is false (FR-610), with optional :key
(our-defmacro count-if-not (pred list &key key)
  (if key
      `(count-if (complement ,pred) ,list :key ,key)
      `(count-if (complement ,pred) ,list)))

;; ASSOC-IF: return the first alist entry whose CAR satisfies PRED.
(our-defmacro assoc-if (pred alist)
  (let ((fn-var (gensym "FN"))
        (pair   (gensym "PAIR")))
    `(let ((,fn-var ,pred))
       (dolist (,pair ,alist nil)
         (when (and ,pair (funcall ,fn-var (car ,pair)))
           (return ,pair))))))

;; ASSOC: return the first alist entry whose CAR matches ITEM, with optional
;; :test/:key/:test-not keyword arguments.
(our-defmacro assoc (item alist &key test key test-not)
  (let ((item-var (gensym "ITEM"))
        (pair     (gensym "PAIR")))
    (if (or test key test-not)
        (let ((tst-var (gensym "TST"))
              (kfn-var (gensym "KEY")))
          `(let ((,item-var ,item)
                 (,tst-var ,(cond (test-not `(complement ,test-not))
                                  (test test)
                                  (t '#'eql)))
                 (,kfn-var ,(or key '#'identity)))
             (block nil
               (dolist (,pair ,alist nil)
                 (when (and ,pair
                            (funcall ,tst-var ,item-var
                                     (funcall ,kfn-var (car ,pair))))
                   (return ,pair))))))
        `(let ((,item-var ,item))
           (block nil
             (dolist (,pair ,alist nil)
               (when (and ,pair (eql ,item-var (car ,pair)))
                 (return ,pair))))))))

;; ASSOC-IF-NOT: negate the predicate and reuse ASSOC-IF.
(our-defmacro assoc-if-not (pred alist)
  `(assoc-if (complement ,pred) ,alist))

;; RASSOC-IF: return the first alist entry whose CDR satisfies PRED.
(our-defmacro rassoc-if (pred alist)
  (let ((fn-var (gensym "FN"))
        (pair   (gensym "PAIR")))
    `(let ((,fn-var ,pred))
       (dolist (,pair ,alist nil)
         (when (and ,pair (funcall ,fn-var (cdr ,pair)))
           (return ,pair))))))

;; RASSOC-IF-NOT: negate the predicate and reuse RASSOC-IF.
(our-defmacro rassoc-if-not (pred alist)
  `(rassoc-if (complement ,pred) ,alist))
