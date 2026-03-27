(in-package :cl-cc)
;;; List Mutation Macros

;; PUSH macro
(our-defmacro push (value place)
  "Push VALUE onto the front of list PLACE."
  `(setf ,place (cons ,value ,place)))

;; POP macro
(our-defmacro pop (place)
  "Remove and return the first element of list PLACE."
  (let ((tmp (gensym "TMP")))
    `(let ((,tmp (car ,place)))
       (setf ,place (cdr ,place))
       ,tmp)))

;; INCF macro
(our-defmacro incf (place &optional (delta 1))
  "Increment PLACE by DELTA (default 1)."
  `(setf ,place (+ ,place ,delta)))

;; DECF macro
(our-defmacro decf (place &optional (delta 1))
  "Decrement PLACE by DELTA (default 1)."
  `(setf ,place (- ,place ,delta)))

;; 1+ and 1- utility functions
(our-defmacro 1+ (n)
  `(+ ,n 1))

(our-defmacro 1- (n)
  `(- ,n 1))

;; SIGNUM: returns -1, 0, or 1 based on sign
(our-defmacro signum (n)
  (let ((nv (gensym "N")))
    `(let ((,nv ,n))
       (cond ((zerop ,nv) 0)
             ((plusp ,nv) 1)
             (t -1)))))

;; ISQRT: integer square root (floor of real square root)
(our-defmacro isqrt (n)
  `(floor (sqrt (float ,n))))

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
(our-defmacro rotatef (a b)
  "Swap the values of A and B."
  (let ((tmp (gensym "TMP")))
    `(let ((,tmp ,a))
       (setq ,a ,b)
       (setq ,b ,tmp)
       nil)))

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
  "Evaluate BODY with SLOT-NAMES bound to slot values of INSTANCE."
  (let ((inst-var (gensym "INST")))
    `(let ((,inst-var ,instance))
       (let ,(mapcar (lambda (slot)
                       (if (listp slot)
                           `(,(first slot) (slot-value ,inst-var ',(second slot)))
                           `(,slot (slot-value ,inst-var ',slot))))
                     slot-names)
         ,@body))))

;; NTH-VALUE macro - extract nth return value from multiple-values form
(our-defmacro nth-value (n form)
  "Return the Nth value from a multiple-values-producing FORM."
  (let ((vals-var (gensym "VALS")))
    `(let ((,vals-var (multiple-value-list ,form)))
       (nth ,n ,vals-var))))

;; ECASE macro (exhaustive case)
(our-defmacro ecase (keyform &body cases)
  "Like CASE but signals error if no case matches."
  (let ((key-var (gensym "KEY")))
    `(let ((,key-var ,keyform))
       (case ,key-var
         ,@cases
         (otherwise (error "ECASE: no matching clause"))))))

;; ETYPECASE macro (exhaustive typecase)
(our-defmacro etypecase (keyform &body cases)
  "Like TYPECASE but signals error if no case matches."
  (let ((key-var (gensym "KEY")))
    `(let ((,key-var ,keyform))
       (typecase ,key-var
         ,@cases
         (otherwise (error "ETYPECASE: no matching clause"))))))

;;; ------------------------------------------------------------
;;; ANSI CL Phase 1 Macros (FR-201 through FR-212)
;;; ------------------------------------------------------------

;; PSETF (FR-207) — parallel setf: evaluate all new values before setting any place
(our-defmacro psetf (&rest pairs)
  "Parallel SETF: evaluates all new values before assigning to any place.
   (psetf p1 v1 p2 v2 ...) — all vi are evaluated, then all pi are set."
  (when (oddp (length pairs))
    (error "PSETF requires an even number of arguments"))
  (let (places vals)
    (do ((rest pairs (cddr rest)))
        ((null rest))
      (push (first rest) places)
      (push (second rest) vals))
    (setf places (nreverse places)
          vals   (nreverse vals))
    (let ((temps (mapcar (lambda (v) (declare (ignore v)) (gensym "PSETF")) vals)))
      `(let ,(mapcar #'list temps vals)
         ,@(mapcar (lambda (place temp) `(setf ,place ,temp))
                   places temps)
         nil))))

;; SHIFTF (FR-206) — shift values through a series of places
(our-defmacro shiftf (&rest args)
  "Shift values: (shiftf place1 ... placeN newval) sets each place to the
   next place's old value, stores NEWVAL in the last place; returns old
   value of first place."
  (when (< (length args) 2)
    (error "SHIFTF requires at least 2 arguments (one place and one new value)"))
  (let* ((places (butlast args))
         (newval (car (last args)))
         (temps  (mapcar (lambda (p) (declare (ignore p)) (gensym "SHIFT")) places)))
    `(let ,(mapcar #'list temps places)
       ,@(mapcar (lambda (place val)
                   `(setf ,place ,val))
                 places
                 (append (cdr temps) (list newval)))
       ,(car temps))))

;; WITH-ACCESSORS (FR-205) — bind local vars to accessor function results
(our-defmacro with-accessors (slot-entries instance &body body)
  "Binds local variables to slot values read via accessor functions.
   Each entry is (local-var-name accessor-function-name)."
  (let ((inst-var (gensym "INST")))
    `(let ((,inst-var ,instance))
       (let ,(mapcar (lambda (entry)
                       (destructuring-bind (var-name accessor-name) entry
                         `(,var-name (,accessor-name ,inst-var))))
                     slot-entries)
         ,@body))))

;; DEFINE-MODIFY-MACRO (FR-208) — define a read-modify-write macro
(our-defmacro define-modify-macro (name lambda-list function &optional doc)
  (declare (ignore doc))   ; before docstring — declare after string is invalid in our-defmacro
  "Define a macro NAME that reads PLACE, applies FUNCTION (with extra args),
   and stores the result back into PLACE."
  (let* ((place-var (gensym "PLACE"))
         (param-names
           (mapcar (lambda (p) (if (listp p) (first p) p))
                   (remove-if (lambda (p)
                                (member p '(&optional &rest &key &aux &allow-other-keys)))
                              lambda-list))))
    `(our-defmacro ,name (,place-var ,@lambda-list)
       `(setf ,,place-var (,',function ,,place-var ,,@param-names)))))

;; ASSERT (FR-203) — signal an error if a test fails
(our-defmacro assert (test &optional places datum &rest args)
  (declare (ignore places))   ; PLACES not supported in this impl; declare before docstring
  "Signal a correctable error if TEST is false."
  `(unless ,test
     ,(if datum
          `(error ,datum ,@args)
          `(error "Assertion failed: ~S" ',test))))

;; DEFINE-CONDITION (FR-204) — define a condition type (expands to defclass)
(our-defmacro define-condition (name parent-list slot-specs &rest options)
  (declare (ignore options))   ; before docstring — declare after string is invalid in our-defmacro
  "Define a condition type NAME."
  (let ((parent-names (if parent-list parent-list '(error))))
    `(defclass ,name ,parent-names
       ,slot-specs)))

;; HANDLER-BIND (FR-201) — simplified via HANDLER-CASE
(our-defmacro handler-bind (bindings &body body)
  "Establish condition handlers around BODY (simplified: stack unwinds)."
  (if (null bindings)
      `(progn ,@body)
      `(handler-case (progn ,@body)
         ,@(mapcar (lambda (binding)
                     (let ((cvar (gensym "COND")))
                       `(,(first binding) (,cvar)
                          (funcall ,(second binding) ,cvar))))
                   bindings))))

;; RESTART-CASE (FR-202) — simplified
(our-defmacro restart-case (form &rest clauses)
  "Execute FORM with named restarts (simplified; restarts not invokable)."
  (if (null clauses)
      form
      (let* ((first-clause (first clauses))
             (first-body   (cdddr first-clause))
             (err-var (gensym "ERR")))
        `(handler-case ,form
           (error (,err-var)
             (declare (ignore ,err-var))
             ,@(if first-body first-body '(nil)))))))

;; RESTART-BIND (FR-202) — stub
(our-defmacro restart-bind (bindings &body body)
  (declare (ignore bindings))   ; before docstring — declare after string is invalid in our-defmacro
  "Establish restart bindings (stub: ignored)."
  `(progn ,@body))

;;; FR-804/FR-805: Restart utility functions (stubs for ANSI compatibility)
;;; Note: declare after docstring is invalid in our-defmacro bodies (docstring
;;; is first body form; declare must be first). All stubs use minimal bodies.

(our-defmacro invoke-restart (name &rest args)
  `(error "No restart named ~S is active" ,name ,@args))

(our-defmacro find-restart (name &optional condition)
  (let ((_n name) (_c condition)) (declare (ignore _n _c))
    `nil))

(our-defmacro compute-restarts (&optional condition)
  (let ((_c condition)) (declare (ignore _c))
    `nil))

(our-defmacro restart-name (restart)
  `(if (hash-table-p ,restart)
       (gethash :name ,restart)
       ,restart))

;;; FR-805: Standard restart functions

(our-defmacro abort (&optional condition)
  (let ((_c condition)) (declare (ignore _c))
    `(error "ABORT invoked")))

(our-defmacro continue (&optional condition)
  (let ((_c condition)) (declare (ignore _c))
    `nil))

(our-defmacro muffle-warning (&optional condition)
  (let ((_c condition)) (declare (ignore _c))
    `nil))

(our-defmacro use-value (value &optional condition)
  (let ((_c condition)) (declare (ignore _c))
    `,value))

(our-defmacro store-value (value &optional condition)
  (let ((_c condition)) (declare (ignore _c))
    `,value))

;; WITH-INPUT-FROM-STRING (FR-209)
(our-defmacro with-input-from-string (binding &body body)
  "Execute BODY with (first binding) bound to a string input stream."
  (let* ((var    (first binding))
         (string (second binding)))
    `(let ((,var (make-string-input-stream ,string)))
       ,@body)))

;; WITH-OUTPUT-TO-STRING
(our-defmacro with-output-to-string (binding &body body)
  "Execute BODY with (first binding) bound to a string output stream."
  (let ((var (first binding)))
    `(let ((,var (make-string-output-stream)))
       ,@body
       (get-output-stream-string ,var))))

;; WITH-STANDARD-IO-SYNTAX (FR-210) — stub
(our-defmacro with-standard-io-syntax (&body body)
  "Execute BODY with standard I/O syntax bindings (stub)."
  `(progn ,@body))

;; WITH-PACKAGE-ITERATOR (FR-211) — stub
(our-defmacro with-package-iterator (binding &body body)
  "Iterate over package symbols (stub: iterator always exhausted)."
  (let ((name (first binding)))
    `(let ((,name (lambda () (values nil nil nil nil))))
       ,@body)))

;; DEFINE-COMPILER-MACRO (FR-212) — stub
(our-defmacro define-compiler-macro (name lambda-list &body body)
  "Define a compiler macro for NAME (stub: ignored)."
  (when (or name lambda-list body))
  nil)

;;; ------------------------------------------------------------
;;; CXR Accessor Macros (algorithmic registration)
;;; ------------------------------------------------------------
;;; cXXr..cXXXXr forms are registered programmatically by
;;; analysing the sequence of a/d letters between the outer c/r.

(let ((cxr-names '(caar cadr cdar cddr
                   caaar cdaar cadar cddar
                   caadr cdadr caddr cdddr
                   caaaar cadaar caadar caddar
                   cdaaar cddaar cdadar cdddar
                   caaadr cadadr caaddr cadddr
                   cdaadr cddadr cdaddr cddddr)))
  (flet ((expand-cxr (sym arg)
           (let* ((name (symbol-name sym))
                  (letters (subseq name 1 (1- (length name)))))
             (reduce (lambda (acc letter)
                       (if (char-equal letter #\a) `(car ,acc) `(cdr ,acc)))
                     (reverse (coerce letters 'list))
                     :initial-value arg))))
    (dolist (cxr-sym cxr-names)
      (let ((cxr cxr-sym))
        (register-macro cxr
                        (lambda (form env)
                          (declare (ignore env))
                          (expand-cxr cxr (second form))))))))

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

;; SET-DIFFERENCE: elements in list1 not present in list2
(our-defmacro set-difference (list1 list2)
  (let ((l2 (gensym "L2"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,l2 ,list2)
           (,acc nil))
       (dolist (,x ,list1 (nreverse ,acc))
         (unless (member ,x ,l2)
           (setq ,acc (cons ,x ,acc)))))))

;; INTERSECTION: elements present in both lists
(our-defmacro intersection (list1 list2)
  (let ((l2 (gensym "L2"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,l2 ,list2)
           (,acc nil))
       (dolist (,x ,list1 (nreverse ,acc))
         (when (member ,x ,l2)
           (setq ,acc (cons ,x ,acc)))))))

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

;; SORT: stable merge sort
(our-defmacro sort (list predicate)
  (let ((pred (gensym "PRED"))
        (len (gensym "LEN"))
        (mid (gensym "MID"))
        (left (gensym "LEFT"))
        (right (gensym "RIGHT"))
        (msort (gensym "MSORT"))
        (mmerge (gensym "MMERGE"))
        (take-n (gensym "TAKEN")))
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
         (,msort ,list)))))

;; STABLE-SORT: same as sort (merge sort is inherently stable)
(our-defmacro stable-sort (list predicate)
  `(sort ,list ,predicate))

;; MAP: map fn over seq, coerce result to result-type
(our-defmacro map (result-type fn seq)
  `(coerce (mapcar ,fn (coerce ,seq 'list)) ,result-type))

;; IGNORE-ERRORS: catch all errors, return nil on error
(our-defmacro ignore-errors (&body forms)
  (let ((e-var (gensym "E")))
    `(handler-case (progn ,@forms)
       (error (,e-var) nil))))

;; CONCATENATE: concatenate strings via nested string-concat calls
(our-defmacro concatenate (result-type &rest strings)
  (if (null strings)
      ""
      (if (null (cdr strings))
          (car strings)
          (reduce (lambda (acc s) `(string-concat ,acc ,s))
                  (cdr strings)
                  :initial-value (car strings)))))

;;; ------------------------------------------------------------
