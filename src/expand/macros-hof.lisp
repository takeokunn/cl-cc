;;;; macros-hof.lisp — Higher-Order Function and Collection Macros
(in-package :cl-cc)

;;; ------------------------------------------------------------
;;; Higher-Order Function Macros
;;; ------------------------------------------------------------
;;; Each HOF is expanded to an explicit dolist-based loop.
;;; The compiler can handle dolist directly; no HOF primitives needed.

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

;; REMOVE-IF: keep elements for which pred is false
(our-defmacro remove-if (pred list)
  (%filter-list-expand 'unless pred list))

;; REMOVE-IF-NOT: keep elements for which pred is true
(our-defmacro remove-if-not (pred list)
  (%filter-list-expand 'when pred list))

;; FIND: first element eql to item, or nil
(our-defmacro find (item list &rest keys)
  (if keys
      ;; keyword args present — key/test-aware loop
      (let* ((key-expr  (or (getf keys :key)  '#'identity))
             (test-expr (or (getf keys :test) '#'eql))
             (item-var  (gensym "ITEM"))
             (key-var   (gensym "KEY"))
             (test-var  (gensym "TEST"))
             (x         (gensym "X")))
        `(let ((,item-var ,item) (,key-var ,key-expr) (,test-var ,test-expr))
           (block nil
             (dolist (,x ,list nil)
               (when (funcall ,test-var ,item-var (funcall ,key-var ,x))
                 (return ,x))))))
      ;; no keyword args — fast eql check
      (let ((item-var (gensym "ITEM")) (x (gensym "X")))
        `(let ((,item-var ,item))
           (block nil
             (dolist (,x ,list nil)
               (when (eql ,item-var ,x) (return ,x))))))))

;; FIND-IF: first element for which pred is true, or nil
(our-defmacro find-if (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X")))
    `(let ((,fn-var ,pred))
       (block nil
         (dolist (,x ,list nil)
           (when (funcall ,fn-var ,x)
             (return ,x)))))))

;; POSITION: index of first element eql to item, or nil
(our-defmacro position (item list)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (idx (gensym "IDX")))
    `(let ((,item-var ,item)
           (,idx 0))
       (block nil
         (dolist (,x ,list nil)
           (when (eql ,item-var ,x)
             (return ,idx))
           (setq ,idx (+ ,idx 1)))))))

;; COUNT: number of elements eql to item
(our-defmacro count (item list)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (cnt (gensym "CNT")))
    `(let ((,item-var ,item)
           (,cnt 0))
       (dolist (,x ,list ,cnt)
         (when (eql ,item-var ,x)
           (setq ,cnt (+ ,cnt 1)))))))

;; COUNT-IF: number of elements for which pred is true
(our-defmacro count-if (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (cnt (gensym "CNT")))
    `(let ((,fn-var ,pred)
           (,cnt 0))
       (dolist (,x ,list ,cnt)
         (when (funcall ,fn-var ,x)
           (setq ,cnt (+ ,cnt 1)))))))

;; FIND-IF-NOT: first element for which pred is false (FR-610)
(our-defmacro find-if-not (pred list)
  `(find-if (complement ,pred) ,list))

;; POSITION-IF: index of first element satisfying pred (FR-610)
(our-defmacro position-if (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (idx (gensym "IDX")))
    `(let ((,fn-var ,pred)
           (,idx 0))
       (block nil
         (dolist (,x ,list nil)
           (when (funcall ,fn-var ,x)
             (return ,idx))
           (setq ,idx (+ ,idx 1)))))))

;; POSITION-IF-NOT: index of first element not satisfying pred (FR-610)
(our-defmacro position-if-not (pred list)
  `(position-if (complement ,pred) ,list))

;; COUNT-IF-NOT: count elements for which pred is false (FR-610)
(our-defmacro count-if-not (pred list)
  `(count-if (complement ,pred) ,list))

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

;; REMOVE: remove all elements eql to item
(our-defmacro remove (item list)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,item-var ,item)
           (,acc nil))
       (dolist (,x ,list (nreverse ,acc))
         (unless (eql ,item-var ,x)
           (setq ,acc (cons ,x ,acc)))))))

;; REMOVE-DUPLICATES: keep only the first occurrence of each element
(our-defmacro remove-duplicates (list)
  (let ((x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,acc nil))
       (dolist (,x ,list (nreverse ,acc))
         (unless (member ,x ,acc)
           (setq ,acc (cons ,x ,acc)))))))

;; UNION: all elements present in either list, no duplicates
(our-defmacro union (list1 list2)
  (let ((l1 (gensym "L1"))
        (l2 (gensym "L2"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,l1 ,list1)
           (,l2 ,list2)
           (,acc nil))
       (dolist (,x ,l2)
         (setq ,acc (cons ,x ,acc)))
       (dolist (,x ,l1 (nreverse ,acc))
         (unless (member ,x ,l2)
           (setq ,acc (cons ,x ,acc)))))))

;; Shared expansion for set-difference / intersection.
;; KEEP-WHEN is 'when (intersection) or 'unless (set-difference).
(defun %set-filter-expand (list1 list2 keep-when)
  (let ((l2  (gensym "L2"))
        (x   (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,l2 ,list2) (,acc nil))
       (dolist (,x ,list1 (nreverse ,acc))
         (,keep-when (member ,x ,l2)
           (setq ,acc (cons ,x ,acc)))))))

;; SET-DIFFERENCE: elements in list1 not present in list2
(our-defmacro set-difference (list1 list2)
  (%set-filter-expand list1 list2 'unless))

;; INTERSECTION: elements present in both lists
(our-defmacro intersection (list1 list2)
  (%set-filter-expand list1 list2 'when))

;; SUBSETP: true iff every element of list1 is in list2
(our-defmacro subsetp (list1 list2)
  (let ((l2 (gensym "L2"))
        (x (gensym "X")))
    `(let ((,l2 ,list2))
       (every (lambda (,x) (member ,x ,l2)) ,list1))))

;; ADJOIN: cons item onto list only if not already present
(our-defmacro adjoin (item list)
  (let ((item-var (gensym "ITEM"))
        (lst (gensym "LST")))
    `(let ((,item-var ,item)
           (,lst ,list))
       (if (member ,item-var ,lst) ,lst (cons ,item-var ,lst)))))

;; RASSOC: find association pair by cdr value
(our-defmacro rassoc (item alist)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X")))
    `(let ((,item-var ,item))
       (block nil
         (dolist (,x ,alist nil)
           (when (and (consp ,x) (eql ,item-var (cdr ,x)))
             (return ,x)))))))

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

;; SORT: stable merge sort, with optional :key argument (FR-611)
(our-defmacro sort (list predicate &key key)
  (let ((pred    (gensym "PRED"))
        (keyfn   (gensym "KEY"))
        (len     (gensym "LEN"))
        (mid     (gensym "MID"))
        (left    (gensym "LEFT"))
        (right   (gensym "RIGHT"))
        (msort   (gensym "MSORT"))
        (mmerge  (gensym "MMERGE"))
        (take-n  (gensym "TAKEN")))
    (if key
        `(let ((,pred ,predicate)
               (,keyfn ,key))
           (labels ((,take-n (lst n)
                      (if (= n 0) nil
                          (cons (car lst) (,take-n (cdr lst) (- n 1)))))
                    (,mmerge (a b)
                      (cond ((null a) b)
                            ((null b) a)
                            ((funcall ,pred (funcall ,keyfn (car a)) (funcall ,keyfn (car b)))
                             (cons (car a) (,mmerge (cdr a) b)))
                            (t (cons (car b) (,mmerge a (cdr b))))))
                    (,msort (lst)
                      (let ((,len (length lst)))
                        (if (<= ,len 1) lst
                            (let* ((,mid (truncate ,len 2))
                                   (,left (,take-n lst ,mid))
                                   (,right (nthcdr ,mid lst)))
                              (,mmerge (,msort ,left) (,msort ,right)))))))
             (,msort ,list)))
        `(let ((,pred ,predicate))
           (labels ((,take-n (lst n)
                      (if (= n 0) nil
                          (cons (car lst) (,take-n (cdr lst) (- n 1)))))
                    (,mmerge (a b)
                      (cond ((null a) b)
                            ((null b) a)
                            ((funcall ,pred (car a) (car b))
                             (cons (car a) (,mmerge (cdr a) b)))
                            (t (cons (car b) (,mmerge a (cdr b))))))
                    (,msort (lst)
                      (let ((,len (length lst)))
                        (if (<= ,len 1) lst
                            (let* ((,mid (truncate ,len 2))
                                   (,left (,take-n lst ,mid))
                                   (,right (nthcdr ,mid lst)))
                              (,mmerge (,msort ,left) (,msort ,right)))))))
             (,msort ,list))))))

;; STABLE-SORT: same as sort (merge sort is inherently stable)
(our-defmacro stable-sort (list predicate &key key)
  (if key
      `(sort ,list ,predicate :key ,key)
      `(sort ,list ,predicate)))

;; MAP: map fn over seq, coerce result to result-type
(our-defmacro map (result-type fn seq)
  `(coerce (mapcar ,fn (coerce ,seq 'list)) ,result-type))

;; IGNORE-ERRORS: catch all errors, return nil on error
(our-defmacro ignore-errors (&body forms)
  (let ((e-var (gensym "E")))
    `(handler-case (progn ,@forms)
       (error (,e-var) nil))))

;; CONCATENATE: type-dispatching sequence concatenation (FR-615)
(our-defmacro concatenate (result-type &rest sequences)
  (let ((rtype (if (and (consp result-type) (eq (car result-type) 'quote))
                   (cadr result-type)
                   result-type)))
    (cond
      ((null sequences)
       (if (eq rtype 'string) "" nil))
      ((eq rtype 'list)
       `(append ,@sequences))
      ((member rtype '(vector simple-vector))
       `(coerce-to-vector (append ,@sequences)))
      ;; default: string concatenation
      (t
       (if (null (cdr sequences))
           (car sequences)
           (reduce (lambda (acc s) `(string-concat ,acc ,s))
                   (cdr sequences)
                   :initial-value (car sequences)))))))

;;; ─── LIST* (FR-609) ─────────────────────────────────────────────────────────
;;;
;;; (list* a) = a
;;; (list* a b) = (cons a b)
;;; (list* a b c) = (cons a (cons b c))

(our-defmacro list* (&rest args)
  (cond
    ((null args) (error "list* requires at least one argument"))
    ((null (cdr args)) (car args))
    (t (reduce (lambda (x acc) `(cons ,x ,acc))
               args
               :from-end t))))

;;; ─── List accessors sixth–tenth (FR-563) ───────────────────────────────────

(our-defmacro sixth   (list) `(nth 5 ,list))
(our-defmacro seventh (list) `(nth 6 ,list))
(our-defmacro eighth  (list) `(nth 7 ,list))
(our-defmacro ninth   (list) `(nth 8 ,list))
(our-defmacro tenth   (list) `(nth 9 ,list))

;;; ─── PUSHNEW (FR-587) ───────────────────────────────────────────────────────

(our-defmacro pushnew (item place &key (test '#'eql))
  (let ((item-var (gensym "ITEM")))
    (if (equal test '#'eql)
        ;; Fast path: 2-arg member (registered builtin, no keyword args needed)
        `(let ((,item-var ,item))
           (unless (member ,item-var ,place)
             (setf ,place (cons ,item-var ,place))))
        ;; General path: forward :test to member
        `(let ((,item-var ,item))
           (unless (member ,item-var ,place :test ,test)
             (setf ,place (cons ,item-var ,place)))))))

;;; ─── NRECONC (FR-640) ───────────────────────────────────────────────────────

(our-defmacro nreconc (list tail)
  `(nconc (nreverse ,list) ,tail))

;;; ─── ASSOC-IF / ASSOC-IF-NOT / RASSOC-IF / RASSOC-IF-NOT (FR-660, FR-500) ─

(our-defmacro assoc-if (pred alist)
  (let ((fn-var (gensym "FN"))
        (pair   (gensym "PAIR")))
    `(let ((,fn-var ,pred))
       (dolist (,pair ,alist nil)
         (when (and ,pair (funcall ,fn-var (car ,pair)))
           (return ,pair))))))

(our-defmacro assoc-if-not (pred alist)
  `(assoc-if (complement ,pred) ,alist))

(our-defmacro rassoc-if (pred alist)
  (let ((fn-var (gensym "FN"))
        (pair   (gensym "PAIR")))
    `(let ((,fn-var ,pred))
       (dolist (,pair ,alist nil)
         (when (and ,pair (funcall ,fn-var (cdr ,pair)))
           (return ,pair))))))

(our-defmacro rassoc-if-not (pred alist)
  `(rassoc-if (complement ,pred) ,alist))

;;; ─── MEMBER-IF / MEMBER-IF-NOT (FR-499) ────────────────────────────────────

(our-defmacro member-if (pred list)
  (let ((fn-var (gensym "FN"))
        (tail   (gensym "TAIL")))
    `(let ((,fn-var ,pred))
       (do ((,tail ,list (cdr ,tail)))
           ((null ,tail) nil)
         (when (funcall ,fn-var (car ,tail))
           (return ,tail))))))

(our-defmacro member-if-not (pred list)
  `(member-if (complement ,pred) ,list))

;;; ─── COMPLEMENT (FR-610) ────────────────────────────────────────────────────

(our-defmacro complement (pred)
  (let ((fn-var (gensym "FN"))
        (args   (gensym "ARGS")))
    `(let ((,fn-var ,pred))
       (lambda (&rest ,args) (not (apply ,fn-var ,args))))))

;;; ─── Y-OR-N-P (FR-578) ──────────────────────────────────────────────────────

(our-defmacro y-or-n-p (&optional (format-str "") &rest args)
  `(progn
     (when (not (string= ,format-str ""))
       (format *query-io* ,format-str ,@args))
     (format *query-io* " (y or n) ")
     (let ((ch (read-char *query-io*)))
       (or (char= ch #\y) (char= ch #\Y)))))

;;; ─── PROVIDE / REQUIRE fix (FR-680) ─────────────────────────────────────────

;; Override provide to avoid depending on pushnew
(our-defmacro provide (module)
  (let ((mod-var (gensym "MOD")))
    `(let ((,mod-var ,module))
       (unless (member ,mod-var *modules* :test #'string=)
         (setf *modules* (cons ,mod-var *modules*))))))

;;; ─── LIST* (FR-609) ──────────────────────────────────────────────────────────

(our-defmacro list* (first-arg &rest rest-args)
  "Build a list like LIST but the last element is the tail (not nil)."
  (if (null rest-args)
      first-arg
      `(cons ,first-arg (list* ,@rest-args))))

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

(our-defmacro member-if (pred list)
  "Return the tail of LIST starting from first element satisfying PRED."
  (let ((fn (gensym "FN")) (tl (gensym "TL")))
    `(let ((,fn ,pred))
       (labels ((,tl (lst)
                  (cond ((null lst) nil)
                        ((funcall ,fn (car lst)) lst)
                        (t (,tl (cdr lst))))))
         (,tl ,list)))))

(our-defmacro member-if-not (pred list)
  "Return the tail of LIST starting from first element not satisfying PRED."
  `(member-if (complement ,pred) ,list))

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

;;; ------------------------------------------------------------
