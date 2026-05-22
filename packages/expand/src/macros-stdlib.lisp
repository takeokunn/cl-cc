(in-package :cl-cc/expand)
;;; Stdlib macros kept outside the dedicated mutation/list/set/sequence splits

;; 1+ and 1- utility functions
(our-defmacro 1+ (n)
  `(+ ,n 1))

(our-defmacro 1- (n)
  `(- ,n 1))

;; SIGNUM: returns -1, 0, or 1 based on sign
;; FR-684: signum — type-preserving (ANSI CL 12.2.17)
(our-defmacro signum (n)
  (let ((nv (gensym "N")))
    `(let ((,nv ,n))
       (cond ((zerop ,nv) ,nv)
             ((> ,nv 0) (if (integerp ,nv) 1 1.0))
             (t (if (integerp ,nv) -1 -1.0))))))

;; ISQRT: integer square root (floor of real square root)
;; FR-683: isqrt — integer Newton's method for precision on large integers
(our-defmacro isqrt (n)
  (let ((nvar (gensym "N")) (g (gensym "G")) (prev (gensym "P")))
    `(let* ((,nvar ,n)
            (,g (floor (sqrt (float ,nvar))))
            (,prev nil))
       (loop
         (when (eql ,g ,prev) (return ,g))
         (setq ,prev ,g)
         (setq ,g (floor (+ ,g (floor ,nvar ,g)) 2))))))

;; WITH-OPEN-STREAM: like with-open-file but for existing streams
(our-defmacro with-open-stream (var-stream &body body)
  (let ((var (first var-stream))
        (stream (second var-stream)))
    `(let ((,var ,stream))
       (unwind-protect (progn ,@body)
         (close ,var)))))

;; RETURN macro (return from nil block)
(our-defmacro return (&optional value)
  "Return VALUE from the nearest NIL block."
  `(return-from nil ,value))

(our-defmacro reset (&body body)
  "Evaluate BODY under the default delimited-continuation prompt."
  `(call-with-continuation-prompt (lambda () ,@body) 'reset))

(our-defmacro shift (k &body body)
  "Capture the continuation up to the nearest RESET prompt and bind it to K."
  `(call/cc (lambda (,k)
              (abort-to-prompt 'reset (progn ,@body)))))

;; ROTATEF macro
(our-defmacro rotatef (&rest places)
  (labels ((shift-forms (remaining acc)
             (if (or (null remaining) (null (cdr remaining)))
                 (nreverse acc)
                 (shift-forms (cdr remaining)
                              (cons (list (quote setq) (car remaining) (cadr remaining))
                                    acc))))
           (last-place (remaining)
             (if (null (cdr remaining))
                 (car remaining)
                 (last-place (cdr remaining)))))
    (if (< (length places) 2)
        nil
        (let ((tmp (gensym "TMP")))
          (cons (quote let)
                (cons (list (list tmp (first places)))
                      (append (shift-forms places nil)
                              (list (list (quote setq) (last-place places) tmp)
                                    nil))))))))

;; DESTRUCTURING-BIND macro
(our-defmacro destructuring-bind (pattern expr &body body)
  "Bind variables in PATTERN to corresponding parts of EXPR.
   Supports: required, &optional, &rest, &body, &key, &aux."
  (let ((expr-var (gensym "EXPR")))
    `(let ((,expr-var ,expr))
       (let* ,(destructure-lambda-list pattern expr-var)
         ,@body))))

;; PROG macro - let + tagbody + block
(our-defmacro prog (vars &body body)
  "Establish bindings with LET, wrap body in BLOCK NIL + TAGBODY."
  `(block nil
     (let ,vars
       (tagbody ,@body))))

;; PROG* macro - let* + tagbody + block
(our-defmacro prog* (vars &body body)
  "Like PROG but with sequential bindings (LET*)."
  `(block nil
     (let* ,vars
       (tagbody ,@body))))

;; WITH-SLOTS macro - bind slot accessors as local variables
(our-defmacro with-slots (slot-names instance &body body)
  "Evaluate BODY with SLOT-NAMES as symbol macros for slot-value of INSTANCE.
ANSI: uses symbol-macrolet so setf on slot names writes back to the object."
  (let ((inst-var (gensym "INST")))
    `(let ((,inst-var ,instance))
       (symbol-macrolet ,(mapcar (lambda (slot)
                                   (if (listp slot)
                                       `(,(first slot) (slot-value ,inst-var ',(second slot)))
                                       `(,slot (slot-value ,inst-var ',slot))))
                                 slot-names)
          ,@body))))

;;; FR-839: Iteration protocol.
;;;
;;; The host-side iterator object gives macro tests and expander clients a small
;;; protocol object. DOITERATOR/WITH-ITERATOR expand to public names so the same
;;; surface protocol can be provided by the VM standard library.

(defstruct (%iterator (:constructor %make-iterator (sequence index limit elt-fn)))
  sequence
  (index 0 :type fixnum)
  (limit 0 :type fixnum)
  elt-fn)

(defun %iterator-protocol-length (sequence)
  "Return SEQUENCE length using the extensible protocol when available."
  (if (and (fboundp 'cl-cc/vm::sequence-protocol-p)
           (cl-cc/vm::sequence-protocol-p sequence))
      (cl-cc/vm::length sequence)
      (length sequence)))

(defun %iterator-protocol-elt (sequence index)
  "Return SEQUENCE element INDEX using the extensible protocol when available."
  (if (and (fboundp 'cl-cc/vm::sequence-protocol-p)
           (cl-cc/vm::sequence-protocol-p sequence))
      (cl-cc/vm::elt sequence index)
      (elt sequence index)))

(defun make-iterator (sequence)
  "Create an iterator object over SEQUENCE."
  (%make-iterator sequence 0 (%iterator-protocol-length sequence)
                  #'%iterator-protocol-elt))

(defun iterator-next (iterator)
  "Return (values VALUE HAS-MORE-P) for ITERATOR."
  (let ((index (%iterator-index iterator))
        (limit (%iterator-limit iterator)))
    (if (>= index limit)
        (values nil nil)
        (multiple-value-prog1
            (values (funcall (%iterator-elt-fn iterator)
                             (%iterator-sequence iterator)
                             index)
                    t)
          (setf (%iterator-index iterator) (+ index 1))))))

(our-defmacro with-iterator ((iterator sequence) &body body)
  "Bind ITERATOR to an iterator over SEQUENCE while evaluating BODY."
  `(let ((,iterator (make-iterator ,sequence)))
     ,@body))

(our-defmacro doiterator ((var sequence &optional result) &body body)
  "Iterate VAR over SEQUENCE using MAKE-ITERATOR/ITERATOR-NEXT."
  (let ((iterator (gensym "ITERATOR"))
        (has-more-p (gensym "HAS-MORE-P")))
    `(with-iterator (,iterator ,sequence)
       (loop
         (multiple-value-bind (,var ,has-more-p) (iterator-next ,iterator)
           (unless ,has-more-p
             (return ,result))
           ,@body)))))

;; NTH-VALUE macro - extract nth return value from multiple-values form
;; FR-402: capture values list directly for stable semantics across all N.
(our-defmacro nth-value (n form)
  (let ((vals-var (gensym "VALS")))
    `(let ((,vals-var (multiple-value-list ,form)))
       (nth ,n ,vals-var))))

;;; FR-179: source-level sequence operation fusion.
;;;
;;; Compiler macros run before ordinary macro expansion, so they can still see
;;; chains such as (mapcar f (mapcar g xs)) before MAPCAR/REMOVE-IF expand into
;;; DOLIST/DOTIMES loops.  The generated code is deliberately a single explicit
;;; loop over the original sequence, which gives the later VM optimizer an
;;; already-fused instruction stream with no intermediate list allocation.

(defun %fr179-form-head-p (form name)
  "Return T when FORM is a list whose head symbol has symbol-name NAME."
  (and (consp form)
       (symbolp (car form))
       (string= (symbol-name (car form)) name)))

(defun %fr179-simple-mapcar-form-p (form)
  "Return T when FORM is a simple two-argument MAPCAR call."
  (and (%fr179-form-head-p form "MAPCAR")
       (= (length form) 3)))

(defun %fr179-simple-filter-form-p (form)
  "Return T when FORM is a simple REMOVE-IF or REMOVE-IF-NOT call without keys."
  (and (or (%fr179-form-head-p form "REMOVE-IF")
           (%fr179-form-head-p form "REMOVE-IF-NOT"))
       (= (length form) 3)))

(defun %fr179-filter-keep-true-p (filter-head)
  "Return T when FILTER-HEAD keeps elements whose predicate result is true."
  (string= (symbol-name filter-head) "REMOVE-IF-NOT"))

(defun %fr179-ref-form (sequence index kind)
  "Return the element reference form for SEQUENCE at INDEX of KIND."
  (case kind
    (string `(char ,sequence ,index))
    (vector `(aref ,sequence ,index))
    (otherwise `(nth ,index ,sequence))))

(defun %fr179-loop-body (kind seq-var item-var index-var element-form final-form body-form)
  "Build a sequence loop for KIND executing BODY-FORM for each element."
  (case kind
    (list `(dolist (,item-var ,seq-var ,final-form)
             ,body-form))
    ((vector string)
     `(dotimes (,index-var (length ,seq-var) ,final-form)
        (let ((,item-var ,element-form))
          ,body-form)))))

(defun %fr179-fused-map-map-form (outer-fn inner-fn sequence)
  "Expand (mapcar OUTER-FN (mapcar INNER-FN SEQUENCE)) into one loop."
  (let ((outer-var (gensym "OUTER-FN"))
        (inner-var (gensym "INNER-FN"))
        (seq-var   (gensym "SEQUENCE"))
        (item-var  (gensym "ITEM"))
        (index-var (gensym "INDEX"))
        (acc-var   (gensym "ACC")))
    `(let ((,outer-var ,outer-fn)
           (,inner-var ,inner-fn)
           (,seq-var ,sequence))
       (typecase ,seq-var
         (list
          (let ((,acc-var nil))
            ,(%fr179-loop-body
              'list seq-var item-var index-var item-var `(nreverse ,acc-var)
              `(setq ,acc-var
                     (cons (funcall ,outer-var (funcall ,inner-var ,item-var))
                           ,acc-var)))))
         (vector
          (let ((,acc-var nil))
            ,(%fr179-loop-body
              'vector seq-var item-var index-var `(aref ,seq-var ,index-var)
              `(nreverse ,acc-var)
              `(setq ,acc-var
                     (cons (funcall ,outer-var (funcall ,inner-var ,item-var))
                           ,acc-var)))))
         (string
          (let ((,acc-var nil))
            ,(%fr179-loop-body
              'string seq-var item-var index-var `(char ,seq-var ,index-var)
              `(nreverse ,acc-var)
              `(setq ,acc-var
                     (cons (funcall ,outer-var (funcall ,inner-var ,item-var))
                           ,acc-var)))))
         (otherwise
          (error ,(%type-error-form seq-var '(or list vector string))))))))

(defun %fr179-fused-map-filter-form (map-fn filter-form)
  "Expand (mapcar MAP-FN (remove-if[-not] PRED SEQUENCE)) into one loop."
  (let ((pred (second filter-form))
        (sequence (third filter-form))
        (keep-true-p (%fr179-filter-keep-true-p (car filter-form)))
        (map-var (gensym "MAP-FN"))
        (pred-var (gensym "PRED-FN"))
        (seq-var (gensym "SEQUENCE"))
        (item-var (gensym "ITEM"))
        (index-var (gensym "INDEX"))
        (mapped-var (gensym "MAPPED"))
        (acc-var (gensym "ACC")))
    (labels ((body ()
               `(let ((,mapped-var (funcall ,map-var ,item-var)))
                  (when ,(if keep-true-p
                             `(funcall ,pred-var ,item-var)
                             `(not (funcall ,pred-var ,item-var)))
                    (setq ,acc-var (cons ,mapped-var ,acc-var))))))
      `(let ((,map-var ,map-fn)
             (,pred-var ,pred)
             (,seq-var ,sequence))
         (typecase ,seq-var
           (list
            (let ((,acc-var nil))
              ,(%fr179-loop-body 'list seq-var item-var index-var item-var
                                  `(nreverse ,acc-var) (body))))
           (vector
            (let ((,acc-var nil))
              ,(%fr179-loop-body 'vector seq-var item-var index-var `(aref ,seq-var ,index-var)
                                  `(nreverse ,acc-var) (body))))
           (string
            (let ((,acc-var nil))
              ,(%fr179-loop-body 'string seq-var item-var index-var `(char ,seq-var ,index-var)
                                  `(nreverse ,acc-var) (body))))
           (otherwise
            (error ,(%type-error-form seq-var '(or list vector string)))))))))

(defun %fr179-fused-filter-map-form (filter-head pred map-form)
  "Expand (remove-if[-not] PRED (mapcar MAP-FN SEQUENCE)) into one loop."
  (let ((map-fn (second map-form))
        (sequence (third map-form))
        (keep-true-p (%fr179-filter-keep-true-p filter-head))
        (map-var (gensym "MAP-FN"))
        (pred-var (gensym "PRED-FN"))
        (seq-var (gensym "SEQUENCE"))
        (item-var (gensym "ITEM"))
        (index-var (gensym "INDEX"))
        (mapped-var (gensym "MAPPED"))
        (acc-var (gensym "ACC")))
    (labels ((body ()
               `(let ((,mapped-var (funcall ,map-var ,item-var)))
                  (when ,(if keep-true-p
                             `(funcall ,pred-var ,mapped-var)
                             `(not (funcall ,pred-var ,mapped-var)))
                    (setq ,acc-var (cons ,mapped-var ,acc-var))))))
      `(let ((,map-var ,map-fn)
             (,pred-var ,pred)
             (,seq-var ,sequence))
         (typecase ,seq-var
           (list
            (let ((,acc-var nil))
              ,(%fr179-loop-body 'list seq-var item-var index-var item-var
                                  `(nreverse ,acc-var) (body))))
           (vector
            (let ((,acc-var nil))
              ,(%fr179-loop-body 'vector seq-var item-var index-var `(aref ,seq-var ,index-var)
                                  `(nreverse ,acc-var) (body))))
           (string
            (let ((,acc-var nil))
              ,(%fr179-loop-body 'string seq-var item-var index-var `(char ,seq-var ,index-var)
                                  `(nreverse ,acc-var) (body))))
           (otherwise
            (error ,(%type-error-form seq-var '(or list vector string)))))))))

(unless (boundp '*compiler-macro-table*)
  (setf *compiler-macro-table* (make-hash-table :test #'eq)))

(register-compiler-macro
 'mapcar
 (lambda (form env)
   (declare (ignore env))
   (if (and (= (length form) 3)
            (%fr179-simple-mapcar-form-p (third form)))
       (%fr179-fused-map-map-form (second form) (second (third form)) (third (third form)))
       (if (and (= (length form) 3)
                (%fr179-simple-filter-form-p (third form)))
           (%fr179-fused-map-filter-form (second form) (third form))
           form))))

(dolist (name '(remove-if remove-if-not))
  (register-compiler-macro
   name
   (lambda (form env)
     (declare (ignore env))
     (if (and (= (length form) 3)
              (%fr179-simple-mapcar-form-p (third form)))
         (%fr179-fused-filter-map-form (car form) (second form) (third form))
         form))))

;; MAPCAR — list accumulation over lists and vectors (FR-341)
(our-defmacro mapcar (function sequence)
  "Apply FUNCTION to each element of SEQUENCE and return a fresh list of results."
  (labels ((quoted-value (form)
             (when (and (consp form) (eq (car form) 'quote))
               (values (cadr form) t)))
           (list-expansion (fn-var seq-var result-var item-var)
             `(let ((,result-var nil))
                (dolist (,item-var ,seq-var (nreverse ,result-var))
                  (setq ,result-var (cons (funcall ,fn-var ,item-var)
                                           ,result-var)))))
           (vector-expansion (fn-var seq-var result-var index-var)
             `(let ((,result-var nil))
                (dotimes (,index-var (length ,seq-var) (nreverse ,result-var))
                  (setq ,result-var (cons (funcall ,fn-var (aref ,seq-var ,index-var))
                                           ,result-var))))))
    (let ((fn-var (gensym "FUNCTION"))
          (seq-var (gensym "SEQUENCE"))
          (result-var (gensym "RESULT"))
          (item-var (gensym "ITEM"))
          (index-var (gensym "INDEX")))
      (multiple-value-bind (literal-value literalp) (quoted-value sequence)
        `(let ((,fn-var ,function)
               (,seq-var ,sequence))
           ,(if literalp
                (typecase literal-value
                  (list (list-expansion fn-var seq-var result-var item-var))
                  (vector (vector-expansion fn-var seq-var result-var index-var))
                  (otherwise
                   `(error ,(%type-error-form seq-var '(or list vector)))))
                `(typecase ,seq-var
                   (list ,(list-expansion fn-var seq-var result-var item-var))
                   (vector ,(vector-expansion fn-var seq-var result-var index-var))
                   (otherwise
                    (error ,(%type-error-form seq-var '(or list vector)))))))))))

;; ECASE — signal type-error when no clause matches
(our-defmacro ecase (keyform &body cases)
  "Like CASE but signals a TYPE-ERROR if no case matches."
  (let ((key-var (gensym "KEY"))
        (keys (mapcan (lambda (c) (let ((k (car c)))
                                    (if (listp k) (copy-list k) (list k))))
                      cases)))
    `(let ((,key-var ,keyform))
       (case ,key-var
         ,@cases
         (otherwise
           (error ,(%type-error-form key-var `'(member ,@keys))))))))

;; ETYPECASE — signal type-error when no clause matches
(our-defmacro etypecase (keyform &body cases)
  "Like TYPECASE but signals a TYPE-ERROR if no case matches."
  (let ((key-var (gensym "KEY"))
        (types (mapcar #'car cases)))
    `(let ((,key-var ,keyform))
       (typecase ,key-var
         ,@cases
         (otherwise
           (error ,(%type-error-form key-var `'(or ,@types))))))))

;; CCASE — correctable case with STORE-VALUE restart (FR-354)
(our-defmacro ccase (keyform &body cases)
  "Like CASE but signals a correctable TYPE-ERROR if no case matches."
  (let ((key-var (gensym "KEY"))
        (condition-var (gensym "CONDITION"))
        (retry-tag (gensym "CCASE-RETRY"))
        (keys (mapcan (lambda (c) (let ((k (car c)))
                                     (if (listp k) (copy-list k) (list k))))
                      cases)))
    `(block nil
       (tagbody
         ,retry-tag
         (let ((,key-var ,keyform))
           (return
             (case ,key-var
               ,@cases
               (otherwise
                 (restart-case
                    (let ((,condition-var ,(%type-error-form key-var `'(member ,@keys))))
                       (signal ,condition-var)
                       (error ,condition-var))
                   (store-value (new-value)
                     (setf ,keyform new-value)
                     (go ,retry-tag)))))))))))

;; CTYPECASE — correctable typecase with STORE-VALUE restart (FR-354)
(our-defmacro ctypecase (keyform &body cases)
  "Like TYPECASE but signals a correctable TYPE-ERROR if no case matches."
  (let ((key-var (gensym "KEY"))
        (condition-var (gensym "CONDITION"))
        (retry-tag (gensym "CTYPECASE-RETRY"))
        (types (mapcar #'car cases)))
    `(block nil
       (tagbody
         ,retry-tag
         (let ((,key-var ,keyform))
           (return
             (typecase ,key-var
               ,@cases
               (otherwise
                 (restart-case
                    (let ((,condition-var ,(%type-error-form key-var `'(or ,@types))))
                       (signal ,condition-var)
                       (error ,condition-var))
                   (store-value (new-value)
                     (setf ,keyform new-value)
                     (go ,retry-tag)))))))))))
