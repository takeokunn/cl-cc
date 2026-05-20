(in-package :cl-cc/expand)

;;; ─── List and set operations split from stdlib ───────────────────────────────

;;; ── Shared macro expansion helpers ──────────────────────────────────────────
;;;
;;; These are called at macro expansion time (not runtime) to compute code forms.

(defparameter *set-hash-threshold* 20
  "Minimum input length before set-operation macros use hash-table membership.")

(defun %test-predicate-form (test test-not)
  "Return a code form for the test predicate: complement of test-not, test itself, or #'eql."
  (cond (test-not `(complement ,test-not))
        (test     test)
        (t        '#'eql)))

(defun %hash-test-designator-form (test test-not)
  "Return a hash-table :test designator form and whether TEST is hash-compatible."
  (cond
    (test-not (values nil nil))
    ((null test) (values ''eql t))
    ((and (consp test)
          (member (first test) '(quote function) :test #'eq)
          (member (second test) '(eq eql equal equalp) :test #'eq))
     (values `',(second test) t))
    (t (values nil nil))))

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
  (multiple-value-bind (hash-test hash-compatible-p)
      (%hash-test-designator-form test test-not)
    (let ((lst (gensym "LST")) (x (gensym "X")) (acc (gensym "ACC"))
          (seen (gensym "SEEN")) (kx (gensym "KEYED-X"))
          (tst-var (gensym "TST")) (kfn-var (gensym "KEY"))
          (i (gensym "I")))
      (let ((slow-form
               (if (or test key test-not)
                   `(let ((,acc nil)
                         (,tst-var ,(%test-predicate-form test test-not))
                         (,kfn-var ,(or key '#'identity)))
                     (dolist (,x ,lst (nreverse ,acc))
                       (unless (find (funcall ,kfn-var ,x) ,acc :test ,tst-var :key ,kfn-var)
                         (setq ,acc (cons ,x ,acc)))))
                   `(let ((,acc nil))
                      (dolist (,x ,lst (nreverse ,acc))
                        (unless (member ,x ,acc)
                          (setq ,acc (cons ,x ,acc)))))))
            (indexed-form
              (lambda (seq ref-form result-kind)
                (if hash-compatible-p
                    `(if (> (length ,seq) ,*set-hash-threshold*)
                         (let ((,acc nil)
                               (,seen (make-hash-table :test ,hash-test))
                               (,kfn-var ,(or key '#'identity)))
                           (dotimes (,i (length ,seq)
                                        ,(case result-kind
                                           (vector `(coerce (nreverse ,acc) 'vector))
                                           (string `(coerce (nreverse ,acc) 'string))
                                           (otherwise `(nreverse ,acc))))
                             (let* ((,x ,ref-form)
                                    (,kx (funcall ,kfn-var ,x)))
                               (unless (gethash ,kx ,seen)
                                 (setf (gethash ,kx ,seen) t)
                                 (setq ,acc (cons ,x ,acc))))))
                         (let ((,acc nil)
                               (,tst-var ,(%test-predicate-form test test-not))
                               (,kfn-var ,(or key '#'identity)))
                           (dotimes (,i (length ,seq)
                                        ,(case result-kind
                                           (vector `(coerce (nreverse ,acc) 'vector))
                                           (string `(coerce (nreverse ,acc) 'string))
                                           (otherwise `(nreverse ,acc))))
                             (let ((,x ,ref-form))
                               (unless (find (funcall ,kfn-var ,x) ,acc :test ,tst-var :key ,kfn-var)
                                 (setq ,acc (cons ,x ,acc)))))))
                    `(let ((,acc nil)
                           (,tst-var ,(%test-predicate-form test test-not))
                           (,kfn-var ,(or key '#'identity)))
                       (dotimes (,i (length ,seq)
                                    ,(case result-kind
                                       (vector `(coerce (nreverse ,acc) 'vector))
                                       (string `(coerce (nreverse ,acc) 'string))
                                       (otherwise `(nreverse ,acc))))
                         (let ((,x ,ref-form))
                           (unless (find (funcall ,kfn-var ,x) ,acc :test ,tst-var :key ,kfn-var)
                             (setq ,acc (cons ,x ,acc))))))))))
        (%sequence-dispatch-expand
         list
         (lambda (seq)
           `(let ((,lst ,seq))
              ,(if hash-compatible-p
                   `(if (> (length ,lst) ,*set-hash-threshold*)
                        (let ((,acc nil)
                              (,seen (make-hash-table :test ,hash-test))
                              (,kfn-var ,(or key '#'identity)))
                          (dolist (,x ,lst (nreverse ,acc))
                            (let ((,kx (funcall ,kfn-var ,x)))
                              (unless (gethash ,kx ,seen)
                                (setf (gethash ,kx ,seen) t)
                                (setq ,acc (cons ,x ,acc))))))
                        ,slow-form)
                   slow-form)))
         (lambda (seq) (funcall indexed-form seq `(aref ,seq ,i) 'vector))
         (lambda (seq) (funcall indexed-form seq `(char ,seq ,i) 'string)))))))

;; MEMBER: find first tail where (test item element), or nil (with optional :test/:key/:test-not)
;; Shadows the binary builtin to add keyword arg support.
(our-defmacro member (item list &key test key test-not)
  (%sequence-dispatch-expand
   list
   (lambda (seq)
     (let ((item-var (gensym "ITEM")) (lst-var (gensym "LST")))
       (if (or test key test-not)
           (let ((tst-var (gensym "TST")) (kfn-var (gensym "KEY")))
             `(let ((,item-var ,item) (,lst-var ,seq)
                    (,tst-var ,(%test-predicate-form test test-not))
                    (,kfn-var ,(or key '#'identity)))
                (loop
                  (when (null ,lst-var) (return nil))
                  (when (funcall ,tst-var ,item-var (funcall ,kfn-var (car ,lst-var)))
                    (return ,lst-var))
                  (setq ,lst-var (cdr ,lst-var)))))
           `(let ((,item-var ,item) (,lst-var ,seq))
              (loop
                (when (null ,lst-var) (return nil))
                (when (eql ,item-var (car ,lst-var)) (return ,lst-var))
                (setq ,lst-var (cdr ,lst-var)))))))
   (lambda (seq)
     (let ((item-var (gensym "ITEM")) (tst-var (gensym "TST")) (kfn-var (gensym "KEY"))
           (lst-var (gensym "LST")) (i (gensym "I")) (x (gensym "X")))
       `(let ((,item-var ,item)
              (,lst-var (coerce ,seq 'list))
              (,tst-var ,(%test-predicate-form test test-not))
              (,kfn-var ,(or key '#'identity)))
          (block nil
            (dotimes (,i (length ,seq) nil)
              (let ((,x (aref ,seq ,i)))
                (when (funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
                  (return (nthcdr ,i ,lst-var)))))))))
   (lambda (seq)
     (let ((item-var (gensym "ITEM")) (tst-var (gensym "TST")) (kfn-var (gensym "KEY"))
           (lst-var (gensym "LST")) (i (gensym "I")) (x (gensym "X")))
       `(let ((,item-var ,item)
              (,lst-var (coerce ,seq 'list))
              (,tst-var ,(%test-predicate-form test test-not))
              (,kfn-var ,(or key '#'identity)))
          (block nil
            (dotimes (,i (length ,seq) nil)
              (let ((,x (char ,seq ,i)))
                (when (funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
                  (return (nthcdr ,i ,lst-var)))))))))))

;; UNION: all elements present in either list, no duplicates (with optional :test/:key)
(our-defmacro union (list1 list2 &key test key test-not)
  (multiple-value-bind (hash-test hash-compatible-p)
      (%hash-test-designator-form test test-not)
    (let ((l1 (gensym "L1")) (l2 (gensym "L2")) (x (gensym "X"))
          (acc (gensym "ACC")) (seen (gensym "SEEN")) (kx (gensym "KEYED-X"))
          (kfn-var (gensym "KEY")))
      (let* ((kws (%keyword-test-key-args test test-not key))
             (slow-form
               `(let ((,acc nil))
                  (dolist (,x ,l2)
                    (unless (member ,x ,acc ,@kws)
                      (setq ,acc (cons ,x ,acc))))
                  (dolist (,x ,l1 (nreverse ,acc))
                    (unless (member ,x ,acc ,@kws)
                      (setq ,acc (cons ,x ,acc)))))))
        `(let ((,l1 ,list1) (,l2 ,list2))
           ,(if hash-compatible-p
                `(if (> (+ (length ,l1) (length ,l2)) ,*set-hash-threshold*)
                     (let ((,acc nil)
                           (,seen (make-hash-table :test ,hash-test))
                           (,kfn-var ,(or key '#'identity)))
                       (dolist (,x ,l2)
                         (let ((,kx (funcall ,kfn-var ,x)))
                           (unless (gethash ,kx ,seen)
                             (setf (gethash ,kx ,seen) t)
                             (setq ,acc (cons ,x ,acc)))))
                       (dolist (,x ,l1 (nreverse ,acc))
                         (let ((,kx (funcall ,kfn-var ,x)))
                           (unless (gethash ,kx ,seen)
                             (setf (gethash ,kx ,seen) t)
                             (setq ,acc (cons ,x ,acc))))))
                     ,slow-form)
                slow-form))))))

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

(defun %set-difference-expand (list1 list2 test key test-not)
  "Expand SET-DIFFERENCE with a hash-table fast path for large compatible inputs."
  (multiple-value-bind (hash-test hash-compatible-p)
      (%hash-test-designator-form test test-not)
    (let ((l1 (gensym "L1")) (l2 (gensym "L2")) (x (gensym "X"))
          (acc (gensym "ACC")) (seen (gensym "SEEN")) (kx (gensym "KEYED-X"))
          (kfn-var (gensym "KEY")))
      (let ((slow-form (%set-filter-expand l1 l2 'unless
                                           (%keyword-test-key-args test test-not key))))
        `(let ((,l1 ,list1) (,l2 ,list2))
           ,(if hash-compatible-p
                `(if (> (+ (length ,l1) (length ,l2)) ,*set-hash-threshold*)
                     (let ((,acc nil)
                           (,seen (make-hash-table :test ,hash-test))
                           (,kfn-var ,(or key '#'identity)))
                       (dolist (,x ,l2)
                         (setf (gethash (funcall ,kfn-var ,x) ,seen) t))
                       (dolist (,x ,l1 (nreverse ,acc))
                         (let ((,kx (funcall ,kfn-var ,x)))
                           (unless (gethash ,kx ,seen)
                             (setq ,acc (cons ,x ,acc))))))
                     ,slow-form)
                slow-form))))))

(defun %intersection-expand (list1 list2 test key test-not)
  "Expand INTERSECTION with a hash-table fast path for large compatible inputs."
  (multiple-value-bind (hash-test hash-compatible-p)
      (%hash-test-designator-form test test-not)
    (let ((l1 (gensym "L1")) (l2 (gensym "L2")) (short (gensym "SHORT"))
          (long (gensym "LONG")) (x (gensym "X")) (acc (gensym "ACC"))
          (seen (gensym "SEEN")) (kx (gensym "KEYED-X")) (kfn-var (gensym "KEY")))
      (let ((slow-form (%set-filter-expand l1 l2 'when
                                           (%keyword-test-key-args test test-not key))))
        `(let ((,l1 ,list1) (,l2 ,list2))
           ,(if hash-compatible-p
                `(if (> (+ (length ,l1) (length ,l2)) ,*set-hash-threshold*)
                     (let* ((,short (if (< (length ,l1) (length ,l2)) ,l1 ,l2))
                            (,long (if (< (length ,l1) (length ,l2)) ,l2 ,l1))
                            (,acc nil)
                            (,seen (make-hash-table :test ,hash-test))
                            (,kfn-var ,(or key '#'identity)))
                       (dolist (,x ,short)
                         (setf (gethash (funcall ,kfn-var ,x) ,seen) t))
                       (dolist (,x ,long (nreverse ,acc))
                         (let ((,kx (funcall ,kfn-var ,x)))
                           (when (gethash ,kx ,seen)
                             (setq ,acc (cons ,x ,acc))))))
                     ,slow-form)
                slow-form))))))

;; SET-DIFFERENCE: elements in list1 not present in list2 (with optional :test/:key)
(our-defmacro set-difference (list1 list2 &key test key test-not)
  (%set-difference-expand list1 list2 test key test-not))

;; INTERSECTION: elements present in both lists (with optional :test/:key)
(our-defmacro intersection (list1 list2 &key test key test-not)
  (%intersection-expand list1 list2 test key test-not))

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
       (loop while (and ,ks ,ds)
             do (setq ,acc (cons (cons (car ,ks) (car ,ds)) ,acc))
                (setq ,ks (cdr ,ks))
                (setq ,ds (cdr ,ds)))
       ,acc)))
