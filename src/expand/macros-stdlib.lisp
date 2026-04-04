(in-package :cl-cc)
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
  (cond
    ((< (length places) 2) nil)
    ((= (length places) 2)
     (let ((tmp (gensym "TMP")))
       `(let ((,tmp ,(first places)))
          (setq ,(first places) ,(second places))
          (setq ,(second places) ,tmp)
          nil)))
    (t (let ((tmp (gensym "TMP")))
         `(let ((,tmp ,(first places)))
            ,@(loop for (a b) on places
                    while b
                    collect `(setf ,a ,b))
            (setf ,(car (last places)) ,tmp)
            nil)))))

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
;; FR-402: when N is a constant integer, expand to multiple-value-bind directly
(our-defmacro nth-value (n form)
  (if (and (integerp n) (<= 0 n 19))
      (let ((vars (loop for i from 0 to n collect (gensym (format nil "V~D-" i)))))
        `(multiple-value-bind ,vars ,form
           (declare (ignore ,@(butlast vars)))
           ,(car (last vars))))
      (let ((vals-var (gensym "VALS")))
        `(let ((,vals-var (multiple-value-list ,form)))
           (nth ,n ,vals-var)))))

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
  "Binds symbol macros to accessor calls for slot access.
   Each entry is (local-var-name accessor-function-name).
   ANSI: uses symbol-macrolet so setf writes back through accessor."
  (let ((inst-var (gensym "INST")))
    `(let ((,inst-var ,instance))
       (symbol-macrolet ,(mapcar (lambda (entry)
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

;; ASSERT (FR-203) — signal a continuable error if a test fails
(our-defmacro assert (test &optional places datum &rest args)
  (declare (ignore places))   ; PLACES not supported in this impl; declare before docstring
  "Signal a continuable error if TEST is false."
  `(unless ,test
     ,(if datum
          `(cerror "Continue." ,datum ,@args)
          `(cerror "Continue." "Assertion failed: ~S" ',test))))

;; DEFINE-CONDITION (FR-204) — define a condition type (expands to defclass)
;; FR-417: now handles :report option → defmethod print-object
(our-defmacro define-condition (name parent-list slot-specs &rest options)
  "Define a condition type NAME with optional :report."
  (let* ((parent-names (if parent-list parent-list '(error)))
         (report-opt (find :report options
                           :key (lambda (o) (when (listp o) (first o)))))
         (report-fn (when report-opt (second report-opt)))
         (defclass-form `(defclass ,name ,parent-names ,slot-specs))
         (report-form
           (when report-fn
             (let ((c (gensym "C")) (s (gensym "S")))
               (cond
                 ((stringp report-fn)
                  `(defmethod print-object ((,c ,name) ,s)
                     (write-string ,report-fn ,s)))
                 ((and (consp report-fn) (eq (car report-fn) 'lambda))
                  `(defmethod print-object ((,c ,name) ,s)
                     (funcall ,report-fn ,c ,s)))
                 (t
                  `(defmethod print-object ((,c ,name) ,s)
                     (funcall (function ,report-fn) ,c ,s))))))))
    (if report-form
        `(progn ,defclass-form ,report-form (quote ,name))
        defclass-form)))

;; WITH-INPUT-FROM-STRING (FR-209)
(our-defmacro with-input-from-string (binding &body body)
  "Execute BODY with (first binding) bound to a string input stream.
Supports :start, :end, and :index keyword arguments."
  (let* ((var    (first binding))
         (string (second binding))
         (rest   (cddr binding))
         (start  (getf rest :start))
         (end    (getf rest :end))
         (index  (getf rest :index))
         (str-var (gensym "STR")))
    `(let* ((,str-var ,(if (or start end)
                           `(subseq ,string ,(or start 0) ,end)
                           string))
            (,var (make-string-input-stream ,str-var)))
       ,@(when index `((setf ,index (length ,str-var))))
       ,@body)))

;; WITH-OUTPUT-TO-STRING (FR-613: optional string argument ignored in cl-cc)
(our-defmacro with-output-to-string (binding &body body)
  "Execute BODY with (first binding) bound to a string output stream.
 The optional second element of BINDING is written to the fresh stream first,
 so the returned string preserves the ANSI-visible prefix behavior."
  (let ((var (first binding))
        (initial-string (second binding)))
    `(let ((,var (make-string-output-stream)))
       ,@(when initial-string `((write-string ,initial-string ,var)))
       ,@body
       (get-output-stream-string ,var))))

;; WITH-STANDARD-IO-SYNTAX (FR-210) — bind all ANSI-standard I/O variables
(our-defmacro with-standard-io-syntax (&body body)
  "Execute BODY with all ANSI-standard I/O variable bindings."
  `(let ((*print-array*              t)
         (*print-base*               10)
         (*print-case*               :upcase)
         (*print-circle*             nil)
         (*print-escape*             t)
         (*print-gensym*             t)
         (*print-length*             nil)
         (*print-level*              nil)
         (*print-lines*              nil)
         (*print-miser-width*        nil)
         (*print-pretty*             nil)
         (*print-radix*              nil)
         (*print-readably*           nil)
         (*print-right-margin*       nil)
         (*read-base*                10)
         (*read-default-float-format* 'single-float)
         (*read-eval*                t)
         (*read-suppress*            nil)
         (*package*                  (find-package "COMMON-LISP-USER")))
     ,@body))

;; WITH-PACKAGE-ITERATOR (FR-211) — real implementation via host bridge
(register-macro 'with-package-iterator
  (lambda (form env)
    (declare (ignore env))
    (let* ((binding (second form))
           (body (cddr form))
           (name (first binding))
           (packages (second binding))
           (symbol-types (cddr binding))
           (syms-var (gensym "SYMS"))
           (idx-var (gensym "IDX"))
           (len-var (gensym "LEN")))
      (declare (ignore symbol-types))
      ;; Collect external symbols from the given packages
      `(let* ((,syms-var (let ((acc nil))
                           (dolist (p (if (listp ,packages) ,packages (list ,packages)))
                             (let ((pkg (find-package p)))
                               (when pkg
                                 (dolist (s (%package-external-symbols pkg))
                                   (push (list s :external pkg) acc)))))
                           (nreverse acc)))
              (,idx-var 0)
              (,len-var (length ,syms-var)))
         (let ((,name (lambda ()
                        (if (>= ,idx-var ,len-var)
                            (values nil nil nil nil)
                            (let ((entry (nth ,idx-var ,syms-var)))
                              (setq ,idx-var (1+ ,idx-var))
                              (values t (first entry) (second entry) (third entry)))))))
           ,@body)))))

;; DEFINE-COMPILER-MACRO (FR-212) — accepts and returns name (no compile-time expansion)
(register-macro 'define-compiler-macro
  (lambda (form env)
    (declare (ignore env))
    (let ((name (second form))
          (lambda-list (third form))
          (body (cdddr form)))
      (register-compiler-macro name (make-compiler-macro-expander lambda-list body))
      `(quote ,name))))

;;; ─── List utilities: tailp, ldiff, copy-alist (FR-495, FR-496) ───────────────

(our-defmacro tailp (object list)
  "Return true if OBJECT is the same as some tail of LIST (EQ identity)."
  (let ((tail (gensym "TAIL")))
    `(do ((,tail ,list (cdr ,tail)))
         ((atom ,tail) (eq ,tail ,object))
       (when (eq ,tail ,object) (return t)))))

(our-defmacro ldiff (list object)
  "Return a fresh list of the elements of LIST before OBJECT (EQ identity)."
  (let ((result (gensym "RES"))
        (tail   (gensym "TAIL")))
    `(let ((,result nil))
       (do ((,tail ,list (cdr ,tail)))
           ((or (atom ,tail) (eq ,tail ,object))
            (nreverse ,result))
         (push (car ,tail) ,result)))))

(our-defmacro copy-alist (alist)
  "Return a fresh copy of ALIST — each cons cell is new, values are shared."
  (let ((result (gensym "RES"))
        (pair   (gensym "PAIR")))
    `(let ((,result nil))
       (dolist (,pair ,alist (nreverse ,result))
         (push (if (consp ,pair) (cons (car ,pair) (cdr ,pair)) ,pair) ,result)))))

;;; ─── tree-equal (FR-496) ──────────────────────────────────────────────────────

(our-defmacro tree-equal (x y &key (test '#'eql))
  "Return true if two trees are equal, comparing leaves with TEST."
  (let ((fn (gensym "FN"))
        (xv (gensym "X"))
        (yv (gensym "Y")))
    `(labels ((,fn (,xv ,yv)
                (if (consp ,xv)
                    (and (consp ,yv)
                         (,fn (car ,xv) (car ,yv))
                         (,fn (cdr ,xv) (cdr ,yv)))
                    (and (not (consp ,yv))
                         (funcall ,test ,xv ,yv)))))
       (,fn ,x ,y))))

;;; ─── get-properties (FR-540) ──────────────────────────────────────────────────

(our-defmacro get-properties (plist indicator-list)
  "Search PLIST for any key in INDICATOR-LIST. Returns (values key value tail) or (values nil nil nil)."
  (let ((pl   (gensym "PL"))
        (keys (gensym "KEYS")))
    `(do ((,pl ,plist (cddr ,pl)))
         ((null ,pl) (values nil nil nil))
       (when (member (car ,pl) ,indicator-list)
         (return (values (car ,pl) (cadr ,pl) ,pl))))))

;;; ─── Destructive set operations: nunion/nintersection/nset-difference (FR-547) ─

(our-defmacro nunion (list1 list2 &key (test '#'eql))
  "Destructive union (delegates to union in cl-cc)."
  `(union ,list1 ,list2 :test ,test))

(our-defmacro nintersection (list1 list2 &key (test '#'eql))
  "Destructive intersection (delegates to intersection)."
  `(intersection ,list1 ,list2 :test ,test))

(our-defmacro nset-difference (list1 list2 &key (test '#'eql))
  "Destructive set-difference (delegates to set-difference)."
  `(set-difference ,list1 ,list2 :test ,test))

(our-defmacro nset-exclusive-or (list1 list2 &key (test '#'eql))
  "Destructive set-exclusive-or (delegates to set-exclusive-or)."
  `(set-exclusive-or ,list1 ,list2 :test ,test))

;;; ─── nsubst / nsubst-if / nsubst-if-not (FR-496) ────────────────────────────

(our-defmacro nsubst (new old tree &key test)
  "Destructively substitute NEW for OLD in TREE (delegates to subst in cl-cc)."
  (if test
      `(subst-if ,new (lambda (%v) (funcall ,test %v ,old)) ,tree)
      `(subst ,new ,old ,tree)))

(our-defmacro nsubst-if (new pred tree)
  "Destructively substitute NEW where PRED is true (delegates to subst-if)."
  `(subst-if ,new ,pred ,tree))

(our-defmacro nsubst-if-not (new pred tree)
  "Destructively substitute NEW where PRED is false."
  `(subst-if-not ,new ,pred ,tree))

;;; ─── nstring-upcase / nstring-downcase / nstring-capitalize (FR-475) ─────────

(our-defmacro nstring-upcase (string &key start end)
  "Destructively uppercase STRING (returns uppercased string in cl-cc)."
  (if (or start end)
      `(string-upcase ,string :start ,(or start 0) ,@(when end `(:end ,end)))
      `(string-upcase ,string)))

(our-defmacro nstring-downcase (string &key start end)
  "Destructively lowercase STRING."
  (if (or start end)
      `(string-downcase ,string :start ,(or start 0) ,@(when end `(:end ,end)))
      `(string-downcase ,string)))

(our-defmacro nstring-capitalize (string &key start end)
  "Destructively capitalize STRING."
  (if (or start end)
      `(string-capitalize ,string :start ,(or start 0) ,@(when end `(:end ,end)))
      `(string-capitalize ,string)))

;;; ─── Array predicates (FR-564) ────────────────────────────────────────────────

(our-defmacro bit-vector-p (object)
  "Return true if OBJECT is a bit-vector."
  (let ((o (gensym "O")))
    `(let ((,o ,object))
       (and (vectorp ,o) (eq (array-element-type ,o) 'bit)))))

(our-defmacro simple-string-p (object)
  "Return true if OBJECT is a simple string."
  `(stringp ,object))

(our-defmacro simple-bit-vector-p (object)
  "Return true if OBJECT is a simple bit-vector."
  `(bit-vector-p ,object))

;;; ─── Array utilities (FR-541, FR-553, FR-564) ────────────────────────────────

(our-defmacro array-element-type (array)
  "Return the element type of ARRAY (cl-cc arrays are untyped: always T)."
  `(progn ,array 't))

(our-defmacro array-in-bounds-p (array &rest subscripts)
  "Return true if SUBSCRIPTS are valid indices into ARRAY."
  (let ((arr  (gensym "ARR"))
        (subs (gensym "SUBS")))
    `(let ((,arr ,array)
           (,subs (list ,@subscripts)))
       (and (= (array-rank ,arr) (length ,subs))
            (every (lambda (d s) (and (>= s 0) (< s d)))
                   (array-dimensions ,arr) ,subs)))))

(our-defmacro upgraded-array-element-type (type)
  "Return the upgraded array element type (cl-cc uses T for all types)."
  `(progn ,type 't))

;;; equalp is defined in macros-introspection.lisp
