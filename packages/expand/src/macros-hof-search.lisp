;;;; macros-hof-search.lisp — Position, count, and association HOF macros
;;;
;;; Extracted from macros-hof.lisp.
;;; Contains: position, count, count-if, find-if-not, position-if,
;;;           position-if-not, count-if-not, assoc-if, assoc, assoc-if-not,
;;;           rassoc-if, rassoc-if-not.
;;;
;;; Depends on macros-hof.lisp (complement, our-defmacro, dolist).
;;; Load order: immediately after macros-hof.lisp.

(in-package :cl-cc/expand)

;; POSITION: index of first matching element, or nil (with optional sequence keywords)
(our-defmacro position (item list &key test key test-not start end from-end)
  (%sequence-dispatch-expand
   list
   (lambda (seq)
     (if (or start end from-end)
         (%fr450-search-expand item seq test key test-not start end from-end
                               :listp t :return-index-p t)
         (let ((item-var (gensym "ITEM"))
               (x (gensym "X"))
               (idx (gensym "IDX")))
           (if (or test key test-not)
               (let ((tst-var (gensym "TST")) (kfn-var (gensym "KEY")))
                 `(let ((,item-var ,item) (,idx 0)
                        (,tst-var ,(cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
                        (,kfn-var ,(or key '#'identity)))
                    (block nil
                      (dolist (,x ,seq nil)
                        (when (funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
                          (return ,idx))
                        (setq ,idx (+ ,idx 1))))))
               `(let ((,item-var ,item) (,idx 0))
                  (block nil
                    (dolist (,x ,seq nil)
                      (when (eql ,item-var ,x)
                        (return ,idx))
                      (setq ,idx (+ ,idx 1)))))))))
   (lambda (seq)
     (if (or start end from-end)
         (%fr450-search-expand item seq test key test-not start end from-end
                               :return-index-p t)
         (let ((item-var (gensym "ITEM")) (tst-var (gensym "TST")) (kfn-var (gensym "KEY"))
               (i (gensym "I")) (x (gensym "X")))
           `(let ((,item-var ,item)
                  (,tst-var ,(cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
                  (,kfn-var ,(or key '#'identity)))
              (block nil
                (dotimes (,i (length ,seq) nil)
                  (let ((,x (aref ,seq ,i)))
                    (when (funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
                      (return ,i)))))))))
   (lambda (seq)
     (if (or start end from-end)
         (%fr450-search-expand item seq test key test-not start end from-end
                               :stringp t :return-index-p t)
         (let ((item-var (gensym "ITEM")) (tst-var (gensym "TST")) (kfn-var (gensym "KEY"))
               (i (gensym "I")) (x (gensym "X")))
           `(let ((,item-var ,item)
                  (,tst-var ,(cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
                  (,kfn-var ,(or key '#'identity)))
              (block nil
                (dotimes (,i (length ,seq) nil)
                  (let ((,x (char ,seq ,i)))
                    (when (funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
                      (return ,i)))))))))))

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

(defun %count-indexed-expand (item sequence test key test-not &key stringp)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (cnt (gensym "CNT"))
        (i (gensym "I"))
        (tst-var (gensym "TST"))
        (kfn-var (gensym "KEY")))
    `(let ((,item-var ,item)
           (,cnt 0)
           (,tst-var ,(cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
           (,kfn-var ,(or key '#'identity)))
       (dotimes (,i (length ,sequence) ,cnt)
         (let ((,x ,(%indexed-ref-form sequence i stringp)))
            (when (funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
              (setq ,cnt (+ ,cnt 1))))))))

(defun %fr450-count-expand (item sequence test key test-not start end from-end
                            &key stringp listp)
  (let ((seq-var (gensym "SEQ"))
        (item-var (gensym "ITEM"))
        (len-var (gensym "LEN"))
        (start-var (gensym "START"))
        (end-var (gensym "END"))
        (from-end-var (gensym "FROM-END"))
        (offset (gensym "OFFSET"))
        (i (gensym "I"))
        (x (gensym "X"))
        (cnt (gensym "CNT"))
        (tst-var (gensym "TST"))
        (kfn-var (gensym "KEY")))
    `(let* ((,seq-var ,sequence)
            (,item-var ,item)
            (,len-var (length ,seq-var))
            (,start-var (or ,start 0))
            (,end-var (or ,end ,len-var))
            (,from-end-var ,from-end)
            (,cnt 0)
            (,tst-var ,(cond (test-not `(complement ,test-not))
                             (test test)
                             (t '#'eql)))
            (,kfn-var ,(or key '#'identity)))
       (dotimes (,offset (- ,end-var ,start-var) ,cnt)
         (let* ((,i (if ,from-end-var
                        (- ,end-var ,offset 1)
                        (+ ,start-var ,offset)))
                (,x ,(%fr450-sequence-ref-form seq-var i
                                                  :stringp stringp
                                                  :listp listp)))
           (when (funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
             (setq ,cnt (+ ,cnt 1))))))))

(our-defmacro count (item list &key test key test-not start end from-end)
  (%sequence-dispatch-expand
   list
   (lambda (seq)
     (if (or start end from-end)
         (%fr450-count-expand item seq test key test-not start end from-end
                              :listp t)
         (let ((item-var (gensym "ITEM"))
               (x (gensym "X"))
               (cnt (gensym "CNT")))
           (if (or test key test-not)
               (%count-key-expand item seq test key test-not)
               `(let ((,item-var ,item) (,cnt 0))
                  (dolist (,x ,seq ,cnt)
                    (when (eql ,item-var ,x)
                      (setq ,cnt (+ ,cnt 1)))))))))
   (lambda (seq)
     (if (or start end from-end)
         (%fr450-count-expand item seq test key test-not start end from-end)
         (%count-indexed-expand item seq test key test-not)))
   (lambda (seq)
     (if (or start end from-end)
         (%fr450-count-expand item seq test key test-not start end from-end
                              :stringp t)
         (%count-indexed-expand item seq test key test-not :stringp t)))))

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

(defun %count-if-indexed-expand (pred sequence key &key stringp)
  (let ((fn-var (gensym "FN"))
        (kfn (gensym "KEY"))
        (x (gensym "X"))
        (i (gensym "I"))
        (cnt (gensym "CNT")))
    `(let ((,fn-var ,pred)
           ,@(when key `((,kfn ,key)))
           (,cnt 0))
       (dotimes (,i (length ,sequence) ,cnt)
         (let ((,x ,(%indexed-ref-form sequence i stringp)))
           (when (funcall ,fn-var ,(if key `(funcall ,kfn ,x) x))
             (setq ,cnt (+ ,cnt 1))))))))

(our-defmacro count-if (pred list &key key)
  (%sequence-dispatch-expand
   list
   (lambda (seq)
     (if key
         (%count-if-key-expand pred seq key)
         (let ((fn-var (gensym "FN")) (x (gensym "X")) (cnt (gensym "CNT")))
           `(let ((,fn-var ,pred) (,cnt 0))
              (dolist (,x ,seq ,cnt)
                (when (funcall ,fn-var ,x)
                  (setq ,cnt (+ ,cnt 1))))))))
   (lambda (seq) (%count-if-indexed-expand pred seq key))
   (lambda (seq) (%count-if-indexed-expand pred seq key :stringp t))))

;; FIND-IF-NOT: first element for which pred is false (FR-610), with optional :key
(our-defmacro find-if-not (pred list &key key)
  (if key
      (list 'find-if (list 'complement pred) list :key key)
      (list 'find-if (list 'complement pred) list)))

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

(defun %position-if-indexed-expand (pred sequence key &key stringp)
  (let ((fn-var (gensym "FN"))
        (kfn (gensym "KEY"))
        (x (gensym "X"))
        (i (gensym "I")))
    `(let ((,fn-var ,pred)
           ,@(when key `((,kfn ,key))))
       (block nil
         (dotimes (,i (length ,sequence) nil)
           (let ((,x ,(%indexed-ref-form sequence i stringp)))
             (when (funcall ,fn-var ,(if key `(funcall ,kfn ,x) x))
               (return ,i))))))))

(our-defmacro position-if (pred list &key key)
  (%sequence-dispatch-expand
   list
   (lambda (seq)
     (let ((fn-var (gensym "FN"))
           (x (gensym "X"))
           (idx (gensym "IDX")))
       (if key
           (%position-if-key-expand pred seq key)
           `(let ((,fn-var ,pred) (,idx 0))
              (block nil
                (dolist (,x ,seq nil)
                  (when (funcall ,fn-var ,x)
                    (return ,idx))
                  (setq ,idx (+ ,idx 1))))))))
   (lambda (seq) (%position-if-indexed-expand pred seq key))
   (lambda (seq) (%position-if-indexed-expand pred seq key :stringp t))))

;; POSITION-IF-NOT: index of first element not satisfying pred (FR-610), with optional :key
(our-defmacro position-if-not (pred list &key key)
  (if key
      (list 'position-if (list 'complement pred) list :key key)
      (list 'position-if (list 'complement pred) list)))

;; COUNT-IF-NOT: count elements for which pred is false (FR-610), with optional :key
(our-defmacro count-if-not (pred list &key key)
  (if key
      (list 'count-if (list 'complement pred) list :key key)
      (list 'count-if (list 'complement pred) list)))

;; ASSOC-IF: return the first alist entry whose CAR satisfies PRED.
(our-defmacro assoc-if (pred alist)
  (%sequence-dispatch-expand
   alist
   (lambda (seq)
     (let ((fn-var (gensym "FN"))
           (pair   (gensym "PAIR")))
       `(let ((,fn-var ,pred))
          (dolist (,pair ,seq nil)
            (when (and ,pair (funcall ,fn-var (car ,pair)))
              (return ,pair))))))
   (lambda (seq)
     (let ((fn-var (gensym "FN")) (pair (gensym "PAIR")) (i (gensym "I")))
       `(let ((,fn-var ,pred))
          (block nil
            (dotimes (,i (length ,seq) nil)
              (let ((,pair (aref ,seq ,i)))
                (when (and ,pair (funcall ,fn-var (car ,pair)))
                  (return ,pair))))))))
   (lambda (seq) `(assoc-if ,pred (coerce ,seq 'list)))))

;; ASSOC: return the first alist entry whose CAR matches ITEM, with optional
;; :test/:key/:test-not keyword arguments.
(our-defmacro assoc (item alist &key test key test-not)
  (%sequence-dispatch-expand
   alist
   (lambda (seq)
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
                  (dolist (,pair ,seq nil)
                    (when (and ,pair
                               (funcall ,tst-var ,item-var
                                        (funcall ,kfn-var (car ,pair))))
                      (return ,pair))))))
           `(let ((,item-var ,item))
              (block nil
                (dolist (,pair ,seq nil)
                  (when (and ,pair (eql ,item-var (car ,pair)))
                    (return ,pair))))))))
   (lambda (seq)
     (let ((item-var (gensym "ITEM")) (pair (gensym "PAIR")) (i (gensym "I"))
           (tst-var (gensym "TST")) (kfn-var (gensym "KEY")))
       `(let ((,item-var ,item)
              (,tst-var ,(cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
              (,kfn-var ,(or key '#'identity)))
          (block nil
            (dotimes (,i (length ,seq) nil)
              (let ((,pair (aref ,seq ,i)))
                (when (and ,pair (funcall ,tst-var ,item-var (funcall ,kfn-var (car ,pair))))
                  (return ,pair))))))))
   (lambda (seq)
     (append (list 'assoc item (list 'coerce seq ''list))
             (when test (list :test test))
             (when key (list :key key))
             (when test-not (list :test-not test-not))))))

;; ASSOC-IF-NOT: negate the predicate and reuse ASSOC-IF.
(our-defmacro assoc-if-not (pred alist)
  (list 'assoc-if (list 'complement pred) alist))

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
  (list 'rassoc-if (list 'complement pred) alist))
