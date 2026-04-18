;;;; macros-sequence-fold.lisp — Sequence fold/transform/tail macros (FR-500)
;;;
;;; Contains:
;;;   - reduce (fold with :initial-value, :from-end, :key)
;;;   - nsubstitute, nsubstitute-if, nsubstitute-if-not (destructive delegates)
;;;   - map-into (fill sequence with mapped results)
;;;   - merge (merge two sorted sequences)
;;;   - last, butlast, nbutlast (tail operations)
;;;   - search (subsequence matching with :key/:test/:start/:end)
;;;
;;; Element query/modification macros (copy-seq, fill, replace, mismatch,
;;; delete, substitute) are in macros-sequence.lisp (loads before).
;;;
;;; Load order: after macros-sequence.lisp.
(in-package :cl-cc/expand)

;;; REDUCE (FR-500 adjacent): fold a sequence using a binary function
;;; Uses the proven dolist-style tagbody pattern with (go end-tag) inside.
(our-defmacro reduce (fn seq &rest keys)
  "Fold SEQ using FN. Supports :initial-value, :from-end, :key (static at macro-expand time)."
  (let* ((fn-var   (gensym "FN"))
         (cur-var  (gensym "CUR"))
         (acc-var  (gensym "ACC"))
         (start-lbl (gensym "REDUCE-START"))
         (end-lbl   (gensym "REDUCE-END"))
         (has-iv   (not (null (member :initial-value keys))))
         (iv-expr  (getf keys :initial-value nil))
         (fe-expr  (getf keys :from-end nil))
         (key-fn   (getf keys :key nil))
         (seq-init (if fe-expr `(reverse ,seq) seq))
         (elem-form (if key-fn
                        `(funcall ,key-fn (car ,cur-var))
                        `(car ,cur-var))))
    (if has-iv
        ;; With initial-value: acc starts at iv, cur iterates entire seq
        `(let ((,fn-var ,fn)
               (,acc-var ,iv-expr)
               (,cur-var ,seq-init))
           (tagbody
            ,start-lbl
            (if (null ,cur-var) (go ,end-lbl))
            (setq ,acc-var (funcall ,fn-var ,acc-var ,elem-form))
            (setq ,cur-var (cdr ,cur-var))
            (go ,start-lbl)
            ,end-lbl)
           ,acc-var)
        ;; No initial-value: acc starts at first element, cur at rest
        `(let ((,fn-var ,fn)
               (,cur-var ,seq-init))
           (let ((,acc-var ,(if key-fn
                                `(funcall ,key-fn (car ,cur-var))
                                `(car ,cur-var))))
              (setq ,cur-var (cdr ,cur-var))
              (tagbody
              ,start-lbl
              (if (null ,cur-var) (go ,end-lbl))
              (setq ,acc-var (funcall ,fn-var ,acc-var ,elem-form))
              (setq ,cur-var (cdr ,cur-var))
              (go ,start-lbl)
              ,end-lbl)
             ,acc-var)))))

;;; NSUBSTITUTE / NSUBSTITUTE-IF / NSUBSTITUTE-IF-NOT (FR-505): destructive delegates

(our-defmacro nsubstitute (new old seq &key test key test-not)
  "Destructive substitute (delegates to substitute with keyword forwarding)."
  `(substitute ,new ,old ,seq
               ,@(when test     `(:test ,test))
               ,@(when key      `(:key ,key))
               ,@(when test-not `(:test-not ,test-not))))

(our-defmacro nsubstitute-if (new pred seq &key key)
  "Destructive substitute-if (delegates to substitute-if)."
  `(substitute-if ,new ,pred ,seq ,@(when key `(:key ,key))))

(our-defmacro nsubstitute-if-not (new pred seq &key key)
  "Destructive substitute-if-not (delegates to substitute-if-not)."
  `(substitute-if-not ,new ,pred ,seq ,@(when key `(:key ,key))))

;;; MAP-INTO (FR-503): fill sequence with mapped results

(our-defmacro map-into (dest fn &rest seqs)
  "Fill DEST with (fn (elt seq1 i) ...) for each i."
  (if (= (length seqs) 1)
      (let ((d (gensym "DEST"))
             (src (gensym "SRC"))
             (dp (gensym "DP"))
             (sp (gensym "SP"))
             (fn-var (gensym "FN"))
             (loop-fn (gensym "MAP-INTO-LOOP")))
        `(let ((,d ,dest)
               (,src ,(first seqs))
               (,fn-var ,fn))
           (labels ((,loop-fn (,dp ,sp)
                      (when (and ,dp ,sp)
                        (setf (car ,dp) (funcall ,fn-var (car ,sp)))
                        (,loop-fn (cdr ,dp) (cdr ,sp)))))
             (,loop-fn ,d ,src))
           ,d))
      `(progn ,dest)))

;;; MERGE (FR-504): merge two sorted sequences using predicate

(our-defmacro merge (result-type seq1 seq2 pred &rest keys)
  "Merge two sorted sequences SEQ1 and SEQ2 into a sorted sequence using PRED."
  (when keys)
  (when result-type)
  (let ((l1 (gensym "L1"))
        (l2 (gensym "L2"))
        (fn-var (gensym "PRED"))
        (loop-fn (gensym "MERGE-LOOP")))
    `(let ((,l1 ,seq1)
           (,l2 ,seq2)
           (,fn-var ,pred))
       (labels ((,loop-fn (a b)
                  (cond
                    ((null a) b)
                    ((null b) a)
                    ((funcall ,fn-var (car a) (car b))
                     (cons (car a) (,loop-fn (cdr a) b)))
                    (t
                     (cons (car b) (,loop-fn a (cdr b)))))))
         (,loop-fn ,l1 ,l2)))))

;;; LAST/BUTLAST/SEARCH (FR-500 adjacent): sequence tail and subsequence helpers

(our-defmacro last (list &optional (n 1))
  "Return the last N conses of LIST."
  (let ((lst (gensym "LST"))
        (nv  (gensym "N"))
        (len (gensym "LEN")))
    (list 'let* (list (list lst list)
                      (list nv n)
                      (list len (list 'length lst)))
          (list 'nthcdr (list 'max 0 (list '- len nv)) lst))))

(our-defmacro butlast (list &optional (n 1))
  "Return a copy of LIST without the last N conses."
  (let ((lst (gensym "LST"))
        (nv  (gensym "N"))
        (len (gensym "LEN")))
    (list 'let* (list (list lst list)
                      (list nv n)
                      (list len (list 'length lst)))
          (list 'when (list '> len nv)
                (list 'subseq lst 0 (list '- len nv))))))

(our-defmacro nbutlast (list &optional (n 1))
  "Destructively trim the last N conses from LIST (delegates to butlast in cl-cc)."
  (list 'butlast list n))

(our-defmacro search (pattern sequence &key (test '#'eql) (start1 0) end1 (start2 0) end2 key)
  "Search for PATTERN as a subsequence in SEQUENCE (from-end not supported)."
  (let ((pat   (gensym "PAT"))
        (seq   (gensym "SEQ"))
        (fn    (gensym "FN"))
        (kfn   (gensym "KFN"))
        (s1    (gensym "S1"))
        (e1    (gensym "E1"))
        (s2    (gensym "S2"))
        (e2    (gensym "E2"))
        (plen  (gensym "PLEN"))
        (i     (gensym "I"))
        (j     (gensym "J"))
        (match (gensym "MATCH")))
    (list 'let* (list (list pat pattern)
                      (list seq sequence)
                      (list fn test)
                      (list kfn (if key key '(function identity)))
                      (list s1 start1)
                      (list e1 (or end1 (list 'length pat)))
                      (list s2 start2)
                      (list e2 (or end2 (list 'length seq)))
                      (list plen (list '- e1 s1)))
          (list 'when (list '<= plen (list '- e2 s2))
                (list 'block 'found
                      (list 'do (list (list i s2 (list '+ i 1)))
                            (list (list '> (list '+ i plen) e2) nil)
                            (list 'let (list (list match t))
                                  (list 'do (list (list j 0 (list '+ j 1)))
                                        (list (list 'or (list 'not match) (list '= j plen)))
                                        (list 'unless
                                              (list 'funcall fn
                                                    (list 'funcall kfn (list 'elt pat (list '+ s1 j)))
                                                    (list 'funcall kfn (list 'elt seq (list '+ i j))))
                                              (list 'setq match nil)))
                                  (list 'when match (list 'return-from 'found i)))))))))
