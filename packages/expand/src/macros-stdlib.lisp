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
       ;; Newton correction loop: converges in 0-2 iterations
       (block nil
         (tagbody
           :loop
           (when (eql ,g ,prev) (return ,g))
           (setq ,prev ,g)
           (setq ,g (floor (+ ,g (floor ,nvar ,g)) 2))
           (go :loop))))))

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

;; NTH-VALUE macro - extract nth return value from multiple-values form
;; FR-402: capture values list directly for stable semantics across all N.
(our-defmacro nth-value (n form)
  (let ((vals-var (gensym "VALS")))
    `(let ((,vals-var (multiple-value-list ,form)))
       (nth ,n ,vals-var))))

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
           (error (%make-type-error ,key-var '(member ,@keys))))))))

;; ETYPECASE — signal type-error when no clause matches
(our-defmacro etypecase (keyform &body cases)
  "Like TYPECASE but signals a TYPE-ERROR if no case matches."
  (let ((key-var (gensym "KEY"))
        (types (mapcar #'car cases)))
    `(let ((,key-var ,keyform))
       (typecase ,key-var
         ,@cases
         (otherwise
           (error (%make-type-error ,key-var '(or ,@types))))))))

;; CCASE — correctable case: like ecase but signals continuable type-error (FR-354)
;; Without full restart system, behaves like ecase (signals type-error).
(our-defmacro ccase (keyform &body cases)
  "Like CASE but signals a correctable TYPE-ERROR if no case matches."
  (let ((key-var (gensym "KEY"))
        (keys (mapcan (lambda (c) (let ((k (car c)))
                                    (if (listp k) (copy-list k) (list k))))
                      cases)))
    `(let ((,key-var ,keyform))
       (case ,key-var
         ,@cases
         (otherwise
           (error (%make-type-error ,key-var '(member ,@keys))))))))

;; CTYPECASE — correctable typecase: like etypecase but signals continuable type-error (FR-354)
(our-defmacro ctypecase (keyform &body cases)
  "Like TYPECASE but signals a correctable TYPE-ERROR if no case matches."
  (let ((key-var (gensym "KEY"))
        (types (mapcar #'car cases)))
    `(let ((,key-var ,keyform))
       (typecase ,key-var
         ,@cases
         (otherwise
           (error (%make-type-error ,key-var '(or ,@types))))))))
