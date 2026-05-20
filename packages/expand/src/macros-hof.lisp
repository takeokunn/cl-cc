(in-package :cl-cc/expand)

;;; ─── Higher-order list/search macros ─────────────────────────────────────────

(defun %sequence-literal-kind (form)
  "Return a known sequence kind for literal sequence FORM, or NIL."
  (let ((value (if (and (consp form) (eq (car form) 'quote))
                   (second form)
                   form)))
    (cond ((stringp value) 'string)
          ((vectorp value) 'vector)
          ((or (null value) (consp value)) 'list)
          (t nil))))

(defun %sequence-dispatch-expand (sequence list-builder vector-builder string-builder)
  "Build sequence dispatch preserving the existing list path.
Each builder receives a single, already-evaluated sequence form.  Literal list,
vector, and string inputs select a path at macro-expansion time; all other inputs
emit a runtime TYPECASE with an OTHERWISE coercion to list."
  (let ((known-kind (%sequence-literal-kind sequence)))
    (case known-kind
      (list (funcall list-builder sequence))
      (vector (funcall vector-builder sequence))
      (string (funcall string-builder sequence))
      (otherwise
       (let ((seq-var (gensym "SEQ")))
         `(let ((,seq-var ,sequence))
            (typecase ,seq-var
              (list ,(funcall list-builder seq-var))
              (string ,(funcall string-builder seq-var))
              (vector ,(funcall vector-builder seq-var))
              (otherwise ,(funcall list-builder `(coerce ,seq-var 'list))))))))))

(defun %indexed-ref-form (sequence index stringp)
  (if stringp
      `(char ,sequence ,index)
      `(aref ,sequence ,index)))

(defun %fr450-sequence-ref-form (sequence index &key stringp listp)
  (cond (listp `(nth ,index ,sequence))
        (stringp `(char ,sequence ,index))
        (t `(aref ,sequence ,index))))

(defun %mapcar-indexed-expand (fn sequence &key stringp)
  (let ((fn-var (gensym "FN"))
        (i (gensym "I"))
        (acc (gensym "ACC")))
    `(let ((,fn-var ,fn)
           (,acc nil))
       (dotimes (,i (length ,sequence) (nreverse ,acc))
         (setq ,acc (cons (funcall ,fn-var ,(%indexed-ref-form sequence i stringp))
                          ,acc))))))

(defun %predicate-indexed-expand (pred sequence true-result false-result &key stringp return-value-p)
  (let ((fn-var (gensym "FN"))
        (i (gensym "I"))
        (result (gensym "R")))
    `(let ((,fn-var ,pred))
       (block nil
         (dotimes (,i (length ,sequence) ,false-result)
           (let ((,result (funcall ,fn-var ,(%indexed-ref-form sequence i stringp))))
             (when ,result
               (return ,(if return-value-p result true-result)))))))))

(defun %filter-indexed-expand (keep-when-true-p pred sequence key result-kind &key stringp)
  (let ((fn-var (gensym "FN"))
        (kfn (gensym "KEY"))
        (i (gensym "I"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let (,@(if key `((,kfn ,key)) nil)
           (,fn-var ,pred)
           (,acc nil))
       (dotimes (,i (length ,sequence)
                    ,(case result-kind
                       (vector `(coerce (nreverse ,acc) 'vector))
                       (string `(coerce (nreverse ,acc) 'string))
                       (otherwise `(nreverse ,acc))))
         (let ((,x ,(%indexed-ref-form sequence i stringp)))
           (when ,(let ((arg (if key `(funcall ,kfn ,x) x)))
                    (if keep-when-true-p
                        `(funcall ,fn-var ,arg)
                        `(not (funcall ,fn-var ,arg))))
             (setq ,acc (cons ,x ,acc))))))))

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
  (%sequence-dispatch-expand
   list
   (lambda (seq)
     (let ((fn-var (gensym "FN"))
           (x (gensym "X"))
           (acc (gensym "ACC")))
       `(let ((,fn-var ,fn)
              (,acc nil))
          (dolist (,x ,seq (nreverse ,acc))
            (setq ,acc (cons (funcall ,fn-var ,x) ,acc))))))
   (lambda (seq) (%mapcar-indexed-expand fn seq))
   (lambda (seq) (%mapcar-indexed-expand fn seq :stringp t))))

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
  (%sequence-dispatch-expand
   list
   (lambda (seq)
     (let ((fn-var (gensym "FN"))
           (x (gensym "X")))
       `(let ((,fn-var ,pred))
          (block nil
            (dolist (,x ,seq t)
              (unless (funcall ,fn-var ,x)
                (return nil)))))))
   (lambda (seq) (%predicate-indexed-expand pred seq t t))
   (lambda (seq) (%predicate-indexed-expand pred seq t t :stringp t))))

;; SOME: returns first truthy pred result, or nil
(our-defmacro some (pred list)
  (%sequence-dispatch-expand
   list
   (lambda (seq)
     (let ((fn-var (gensym "FN"))
           (x (gensym "X"))
           (result (gensym "R")))
       `(let ((,fn-var ,pred))
          (block nil
            (dolist (,x ,seq nil)
              (let ((,result (funcall ,fn-var ,x)))
                (when ,result (return ,result))))))))
   (lambda (seq) (%predicate-indexed-expand pred seq t nil :return-value-p t))
   (lambda (seq) (%predicate-indexed-expand pred seq t nil :stringp t :return-value-p t))))

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
  (%sequence-dispatch-expand
   list
   (lambda (seq)
     (if key
         (%filter-list-key-expand nil pred seq key)
         (%filter-list-expand 'unless pred seq)))
   (lambda (seq) (%filter-indexed-expand nil pred seq key 'vector))
   (lambda (seq) (%filter-indexed-expand nil pred seq key 'string :stringp t))))

;; REMOVE-IF-NOT: keep elements for which pred is true (with optional :key)
(our-defmacro remove-if-not (pred list &key key)
  (%sequence-dispatch-expand
   list
   (lambda (seq)
     (if key
         (%filter-list-key-expand t pred seq key)
         (%filter-list-expand 'when pred seq)))
   (lambda (seq) (%filter-indexed-expand t pred seq key 'vector))
   (lambda (seq) (%filter-indexed-expand t pred seq key 'string :stringp t))))

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

(defun %find-indexed-expand (item sequence key test test-not &key stringp)
  (let ((item-var (gensym "ITEM"))
        (tst-var (gensym "TST"))
        (kfn-var (gensym "KEY"))
        (i (gensym "I"))
        (x (gensym "X")))
    `(let ((,item-var ,item)
           (,tst-var ,(cond (test-not `(complement ,test-not))
                            (test test)
                            (t '#'eql)))
           (,kfn-var ,(or key '#'identity)))
       (block nil
         (dotimes (,i (length ,sequence) nil)
           (let ((,x ,(%indexed-ref-form sequence i stringp)))
              (when (funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
                (return ,x))))))))

(defun %fr450-search-expand (item sequence test key test-not start end from-end
                             &key stringp listp return-index-p)
  (let ((seq-var (gensym "SEQ"))
        (item-var (gensym "ITEM"))
        (len-var (gensym "LEN"))
        (start-var (gensym "START"))
        (end-var (gensym "END"))
        (from-end-var (gensym "FROM-END"))
        (offset (gensym "OFFSET"))
        (i (gensym "I"))
        (x (gensym "X"))
        (tst-var (gensym "TST"))
        (kfn-var (gensym "KEY")))
    `(let* ((,seq-var ,sequence)
            (,item-var ,item)
            (,len-var (length ,seq-var))
            (,start-var (or ,start 0))
            (,end-var (or ,end ,len-var))
            (,from-end-var ,from-end)
            (,tst-var ,(cond (test-not `(complement ,test-not))
                             (test test)
                             (t '#'eql)))
            (,kfn-var ,(or key '#'identity)))
       (block nil
         (dotimes (,offset (- ,end-var ,start-var) nil)
           (let* ((,i (if ,from-end-var
                          (- ,end-var ,offset 1)
                          (+ ,start-var ,offset)))
                  (,x ,(%fr450-sequence-ref-form seq-var i
                                                    :stringp stringp
                                                    :listp listp)))
             (when (funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
               (return ,(if return-index-p i x)))))))))

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

;; FIND: first element matching ITEM, or NIL (with optional sequence keywords)
(our-defmacro find (item list &key test key test-not start end from-end)
  (%sequence-dispatch-expand
   list
   (lambda (seq)
     (if (or start end from-end)
         (%fr450-search-expand item seq test key test-not start end from-end
                               :listp t)
         (let ((item-var (gensym "ITEM"))
               (x (gensym "X")))
           (if (or test key test-not)
               (%find-key-expand item seq
                                 (or key '#'identity)
                                 (cond (test-not `(complement ,test-not))
                                       (test test)
                                       (t '#'eql)))
               `(let ((,item-var ,item))
                  (block nil
                    (dolist (,x ,seq nil)
                      (when (eql ,item-var ,x)
                        (return ,x)))))))))
   (lambda (seq)
     (if (or start end from-end)
         (%fr450-search-expand item seq test key test-not start end from-end)
         (%find-indexed-expand item seq key test test-not)))
   (lambda (seq)
     (if (or start end from-end)
         (%fr450-search-expand item seq test key test-not start end from-end
                               :stringp t)
         (%find-indexed-expand item seq key test test-not :stringp t)))))

;; FIND-IF: first element for which pred is true, or nil (with optional :key)
(our-defmacro find-if (pred list &key key)
  (%sequence-dispatch-expand
   list
   (lambda (seq)
     (if key
         (%find-if-key-expand pred seq key)
         (let ((fn-var (gensym "FN")) (x (gensym "X")))
           `(let ((,fn-var ,pred))
              (block nil
                (dolist (,x ,seq nil)
                  (when (funcall ,fn-var ,x)
                    (return ,x))))))))
   (lambda (seq)
     (let ((fn-var (gensym "FN")) (kfn (gensym "KEY")) (i (gensym "I")) (x (gensym "X")))
       `(let ((,fn-var ,pred) ,@(when key `((,kfn ,key))))
          (block nil
            (dotimes (,i (length ,seq) nil)
              (let ((,x (aref ,seq ,i)))
                (when (funcall ,fn-var ,(if key `(funcall ,kfn ,x) x))
                  (return ,x))))))))
   (lambda (seq)
     (let ((fn-var (gensym "FN")) (kfn (gensym "KEY")) (i (gensym "I")) (x (gensym "X")))
       `(let ((,fn-var ,pred) ,@(when key `((,kfn ,key))))
          (block nil
            (dotimes (,i (length ,seq) nil)
              (let ((,x (char ,seq ,i)))
                (when (funcall ,fn-var ,(if key `(funcall ,kfn ,x) x))
                  (return ,x))))))))))

;; position, count, count-if, find-if-not, position-if, position-if-not,
;; count-if-not, assoc-if, assoc, assoc-if-not, rassoc-if, rassoc-if-not
;; are in macros-hof-search.lisp (loaded next).
