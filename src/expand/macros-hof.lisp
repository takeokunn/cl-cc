(in-package :cl-cc)

;;; ─── Higher-order list/search macros ─────────────────────────────────────────

;; Shared expansion for remove-if / remove-if-not.
;; KEEP-COND is the form head that gates accumulation ('when or 'unless).
(defun %filter-list-expand (keep-cond pred list)
  (let ((fn-var (gensym "FN"))
        (x      (gensym "X"))
        (acc    (gensym "ACC")))
    `(let ((,fn-var ,pred)
           (,acc nil))
       (dolist (,x ,list (nreverse ,acc))
         (,keep-cond (funcall ,fn-var ,x)
           (setq ,acc (cons ,x ,acc)))))))

(defun %filter-list-key-expand (keep-when-true-p pred list key)
  (let ((fn-var (gensym "FN"))
        (kfn (gensym "KEY"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,fn-var ,pred)
           (,kfn ,key)
           (,acc nil))
       (dolist (,x ,list (nreverse ,acc))
         (when ,(if keep-when-true-p
                    `(funcall ,fn-var (funcall ,kfn ,x))
                    `(not (funcall ,fn-var (funcall ,kfn ,x))))
           (setq ,acc (cons ,x ,acc)))))))

;; MAPCAR: apply fn to each element, collect results
(our-defmacro mapcar (fn list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,fn-var ,fn)
           (,acc nil))
       (dolist (,x ,list (nreverse ,acc))
         (setq ,acc (cons (funcall ,fn-var ,x) ,acc))))))

;; MAPC: side-effect loop, returns the original list
(our-defmacro mapc (fn list)
  (let ((fn-var (gensym "FN"))
        (lst (gensym "LST"))
        (x (gensym "X")))
    `(let ((,fn-var ,fn)
           (,lst ,list))
       (dolist (,x ,lst ,lst)
         (funcall ,fn-var ,x)))))

;; MAPCAN: apply fn to each element, nconc all results
(our-defmacro mapcan (fn list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,fn-var ,fn)
           (,acc nil))
       (dolist (,x ,list ,acc)
         (setq ,acc (nconc ,acc (funcall ,fn-var ,x)))))))

;; EVERY: true iff pred returns non-nil for every element
(our-defmacro every (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X")))
    `(let ((,fn-var ,pred))
       (block nil
         (dolist (,x ,list t)
           (unless (funcall ,fn-var ,x)
             (return nil)))))))

;; SOME: returns first truthy pred result, or nil
(our-defmacro some (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (result (gensym "R")))
    `(let ((,fn-var ,pred))
       (block nil
         (dolist (,x ,list nil)
           (let ((,result (funcall ,fn-var ,x)))
             (when ,result (return ,result))))))))

;; NOTANY: true iff pred returns nil for every element
(our-defmacro notany (pred list)
  `(not (some ,pred ,list)))

;; NOTEVERY: true iff pred returns nil for at least one element
(our-defmacro notevery (pred list)
  `(not (every ,pred ,list)))

;; COMPLEMENT: invert a predicate by wrapping it in NOT/APPLY.
(our-defmacro complement (fn)
  (let ((fn-var (gensym "FN")))
    `(let ((,fn-var ,fn))
       (lambda (&rest args)
         (not (apply ,fn-var args))))))

;; REMOVE-IF: keep elements for which pred is false (with optional :key)
(our-defmacro remove-if (pred list &key key)
  (if key
      (%filter-list-key-expand nil pred list key)
      (%filter-list-expand 'unless pred list)))

;; REMOVE-IF-NOT: keep elements for which pred is true (with optional :key)
(our-defmacro remove-if-not (pred list &key key)
  (if key
      (%filter-list-key-expand t pred list key)
      (%filter-list-expand 'when pred list)))

;; FIND: first element eql to item, or nil
(defun %find-key-expand (item list key test)
  (let ((item-var (gensym "ITEM"))
        (key-var (gensym "KEY"))
        (test-var (gensym "TEST"))
        (x (gensym "X")))
    `(let ((,item-var ,item)
           (,key-var ,key)
           (,test-var ,test))
       (block nil
         (dolist (,x ,list nil)
           (when (funcall ,test-var ,item-var (funcall ,key-var ,x))
             (return ,x)))))))

(our-defmacro find (item list &rest keys)
  (if keys
      ;; keyword args present — key/test-aware loop
      (%find-key-expand item list
                        (or (getf keys :key)  '#'identity)
                        (or (getf keys :test) '#'eql))
      ;; no keyword args — fast eql check
      (let ((item-var (gensym "ITEM")) (x (gensym "X")))
        `(let ((,item-var ,item))
           (block nil
             (dolist (,x ,list nil)
               (when (eql ,item-var ,x) (return ,x))))))))

(defun %find-if-key-expand (pred list key)
  (let ((fn-var (gensym "FN"))
        (kfn (gensym "KEY"))
        (x (gensym "X")))
    `(let ((,fn-var ,pred)
           (,kfn ,key))
       (block nil
         (dolist (,x ,list nil)
           (when (funcall ,fn-var (funcall ,kfn ,x))
             (return ,x)))))))

;; FIND-IF: first element for which pred is true, or nil (with optional :key)
(our-defmacro find-if (pred list &key key)
  (if key
      (%find-if-key-expand pred list key)
      (let ((fn-var (gensym "FN")) (x (gensym "X")))
        `(let ((,fn-var ,pred))
           (block nil
             (dolist (,x ,list nil)
               (when (funcall ,fn-var ,x)
                 (return ,x))))))))

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
