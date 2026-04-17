(in-package :cl-cc/expand)

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

;; position, count, count-if, find-if-not, position-if, position-if-not,
;; count-if-not, assoc-if, assoc, assoc-if-not, rassoc-if, rassoc-if-not
;; are in macros-hof-search.lisp (loaded next).
