(in-package :cl-cc/expand)

;;; ─── List and set operations split from stdlib ───────────────────────────────

;;; ── Shared macro expansion helpers ──────────────────────────────────────────
;;;
;;; These are called at macro expansion time (not runtime) to compute code forms.

(defun %test-predicate-form (test test-not)
  "Return a code form for the test predicate: complement of test-not, test itself, or #'eql."
  (cond (test-not `(complement ,test-not))
        (test     test)
        (t        '#'eql)))

(defun %keyword-test-args (test test-not)
  "Return the :test/:test-not keyword arg list for member/find/etc., or nil if neither given."
  (cond (test     `(:test ,test))
        (test-not `(:test-not ,test-not))
        (t        nil)))

(defun %keyword-test-key-args (test test-not key)
  "Return combined :test/:test-not + optional :key keyword arg list."
  (let ((kws (%keyword-test-args test test-not)))
    (if key (append kws `(:key ,key)) kws)))

;; REMOVE: remove elements matching item (with optional :test/:key/:count)
(our-defmacro remove (item list &key test key test-not count from-end)
  (let ((item-var (gensym "ITEM")) (x (gensym "X")) (acc (gensym "ACC"))
        (tst-var (gensym "TST")) (kfn-var (gensym "KEY"))
        (cnt-var (gensym "CNT")) (lim-var (gensym "LIM"))
        (rev-var (gensym "REV")))
    (let* ((has-keys (or test key test-not))
           (test-e (%test-predicate-form test test-not))
           (key-bindings (when has-keys
                           `((,tst-var ,test-e)
                             (,kfn-var ,(or key '#'identity)))))
           (match-form
             (if has-keys
                 `(funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
                 `(eql ,item-var ,x))))
      (cond
        ;; :count present, :from-end t — walk reversed input, skip last N matches
        ;; acc ends up in original left-to-right order (double-reversal effect)
        ((and count from-end)
         `(let* ((,item-var ,item) ,@key-bindings
                 (,rev-var (reverse ,list))
                 (,acc nil) (,cnt-var 0) (,lim-var ,count))
            (dolist (,x ,rev-var)
              (if (and (< ,cnt-var ,lim-var) ,match-form)
                  (setq ,cnt-var (+ ,cnt-var 1))
                  (setq ,acc (cons ,x ,acc))))
            ,acc))
        ;; :count present, forward — remove first N occurrences left-to-right
        (count
         `(let* ((,item-var ,item) ,@key-bindings
                 (,acc nil) (,cnt-var 0) (,lim-var ,count))
            (dolist (,x ,list (nreverse ,acc))
              (if (and (< ,cnt-var ,lim-var) ,match-form)
                  (setq ,cnt-var (+ ,cnt-var 1))
                  (setq ,acc (cons ,x ,acc))))))
        ;; No :count — remove all occurrences
        (has-keys
         `(let* ((,item-var ,item) ,@key-bindings (,acc nil))
            (dolist (,x ,list (nreverse ,acc))
              (unless ,match-form
                (setq ,acc (cons ,x ,acc))))))
        (t
         `(let ((,item-var ,item) (,acc nil))
            (dolist (,x ,list (nreverse ,acc))
              (unless ,match-form
                (setq ,acc (cons ,x ,acc))))))))))

;; REMOVE-DUPLICATES: keep only the first occurrence of each element (with optional :test/:key)
(our-defmacro remove-duplicates (list &key test key test-not)
  (if (or test key test-not)
      (let ((x (gensym "X")) (acc (gensym "ACC"))
            (tst-var (gensym "TST")) (kfn-var (gensym "KEY")))
        `(let ((,acc nil)
               (,tst-var ,(%test-predicate-form test test-not))
               (,kfn-var ,(or key '#'identity)))
            (dolist (,x ,list (nreverse ,acc))
              (unless (find (funcall ,kfn-var ,x) ,acc :test ,tst-var :key ,kfn-var)
                (setq ,acc (cons ,x ,acc))))))
      (let ((x (gensym "X")) (acc (gensym "ACC")))
        `(let ((,acc nil))
           (dolist (,x ,list (nreverse ,acc))
             (unless (member ,x ,acc)
               (setq ,acc (cons ,x ,acc))))))))

;; MEMBER: find first tail where (test item element), or nil (with optional :test/:key/:test-not)
;; Shadows the binary builtin to add keyword arg support.
(our-defmacro member (item list &key test key test-not)
  (let ((item-var (gensym "ITEM")) (lst-var (gensym "LST")) (tag (gensym "MLOOP")))
    (if (or test key test-not)
        (let ((tst-var (gensym "TST")) (kfn-var (gensym "KEY")))
          `(let ((,item-var ,item) (,lst-var ,list)
                 (,tst-var ,(%test-predicate-form test test-not))
                 (,kfn-var ,(or key '#'identity)))
             (block nil
               (tagbody
                ,tag
                  (when (null ,lst-var) (return nil))
                  (when (funcall ,tst-var ,item-var (funcall ,kfn-var (car ,lst-var)))
                    (return ,lst-var))
                  (setq ,lst-var (cdr ,lst-var))
                  (go ,tag)))))
        `(let ((,item-var ,item) (,lst-var ,list))
           (block nil
             (tagbody
              ,tag
                (when (null ,lst-var) (return nil))
                (when (eql ,item-var (car ,lst-var)) (return ,lst-var))
                (setq ,lst-var (cdr ,lst-var))
                (go ,tag)))))))

;; UNION: all elements present in either list, no duplicates (with optional :test/:key)
(our-defmacro union (list1 list2 &key test key test-not)
  (let ((l1 (gensym "L1")) (l2 (gensym "L2")) (x (gensym "X")) (acc (gensym "ACC")))
    (let ((kws (%keyword-test-key-args test test-not key)))
      (list (quote let)
            (list (list l1 list1) (list l2 list2) (list acc nil))
            (list (quote dolist)
                  (list x l2)
                  (list (quote setq) acc (list (quote cons) x acc)))
            (list (quote dolist)
                  (list x l1 (list (quote nreverse) acc))
                  (list (quote unless)
                        (cons (quote member) (append (list x l2) kws))
                        (list (quote setq) acc (list (quote cons) x acc))))))))

;; Shared expansion for set-difference / intersection with optional :test/:key
(defun %set-filter-expand (list1 list2 keep-when &optional kws)
  (let ((l2  (gensym "L2"))
        (x   (gensym "X"))
        (acc (gensym "ACC")))
    (list (quote let)
          (list (list l2 list2) (list acc nil))
          (list (quote dolist)
                (list x list1 (list (quote nreverse) acc))
                (list keep-when
                      (cons (quote member) (append (list x l2) kws))
                      (list (quote setq) acc (list (quote cons) x acc)))))))

;; SET-DIFFERENCE: elements in list1 not present in list2 (with optional :test/:key)
(our-defmacro set-difference (list1 list2 &key test key test-not)
  (%set-filter-expand list1 list2 (quote unless) (%keyword-test-key-args test test-not key)))

;; INTERSECTION: elements present in both lists (with optional :test/:key)
(our-defmacro intersection (list1 list2 &key test key test-not)
  (%set-filter-expand list1 list2 (quote when) (%keyword-test-key-args test test-not key)))

;; SUBSETP: true iff every element of list1 is in list2 (with optional :test/:key)
(our-defmacro subsetp (list1 list2 &key test key test-not)
  (let ((l2 (gensym "L2")) (x (gensym "X")))
    (let ((kws (%keyword-test-key-args test test-not key)))
      (list (quote let)
            (list (list l2 list2))
            (list (quote every)
                  (list (quote lambda)
                        (list x)
                        (cons (quote member) (append (list x l2) kws)))
                  list1)))))

;; ADJOIN: cons item onto list only if not already present (with optional :test/:key)
(our-defmacro adjoin (item list &key test key test-not)
  (let ((item-var (gensym "ITEM")) (lst (gensym "LST")))
    (let ((kws (%keyword-test-key-args test test-not key)))
      (list (quote let)
            (list (list item-var item) (list lst list))
            (list (quote if)
                  (cons (quote member) (append (list item-var lst) kws))
                  lst
                  (list (quote cons) item-var lst))))))

;; RASSOC: find association pair by cdr value (with optional :test/:key/:test-not)
(our-defmacro rassoc (item alist &key test key test-not)
  (if (or test key test-not)
      (let ((item-var (gensym "ITEM")) (x (gensym "X"))
            (tst-var (gensym "TST")) (kfn-var (gensym "KEY")))
        `(let ((,item-var ,item)
               (,tst-var ,(%test-predicate-form test test-not))
               (,kfn-var ,(or key '#'identity)))
           (block nil
             (dolist (,x ,alist nil)
               (when (and (consp ,x)
                          (funcall ,tst-var ,item-var (funcall ,kfn-var (cdr ,x))))
                 (return ,x))))))
      (let ((item-var (gensym "ITEM")) (x (gensym "X")))
        `(let ((,item-var ,item))
           (block nil
             (dolist (,x ,alist nil)
               (when (and (consp ,x) (eql ,item-var (cdr ,x)))
                 (return ,x))))))))

;; PAIRLIS: zip keys and data lists into an association list
(our-defmacro pairlis (keys data &optional alist)
  (let ((ks (gensym "KS"))
        (ds (gensym "DS"))
        (acc (gensym "ACC")))
    `(let ((,ks ,keys)
           (,ds ,data)
           (,acc ,alist))
       (tagbody
        pairlis-loop
          (when (and ,ks ,ds)
            (setq ,acc (cons (cons (car ,ks) (car ,ds)) ,acc))
            (setq ,ks (cdr ,ks))
            (setq ,ds (cdr ,ds))
            (go pairlis-loop)))
       ,acc)))
