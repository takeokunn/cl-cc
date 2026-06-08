(in-package :cl-cc/expand)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Stdlib — List, Tree, String, and Array Utility Macros
;;;
;;; Contains: tailp, ldiff, copy-alist, tree-equal, get-properties,
;;; nunion/nintersection/nset-difference/nset-exclusive-or,
;;; nsubst/nsubst-if/nsubst-if-not, nstring-upcase/nstring-downcase/
;;; nstring-capitalize, bit-vector-p/simple-string-p/simple-bit-vector-p,
;;; array-element-type/array-in-bounds-p/upgraded-array-element-type.
;;;
;;; ANSI CL Phase 1 macros (psetf through define-compiler-macro) are in
;;; macros-stdlib-ansi.lisp (loads before).
;;;
;;; Load order: after macros-stdlib-ansi.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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

;;; ─── set-exclusive-or (FR-496) ───────────────────────────────────────────────

(our-defmacro set-exclusive-or (list1 list2 &key (test '#'eql))
  "Return elements in LIST1 or LIST2 but not both (symmetric difference)."
  (let ((l1 (gensym "L1")) (l2 (gensym "L2")))
    `(let ((,l1 ,list1)
           (,l2 ,list2))
       (append
        (remove-if (lambda (x) (member x ,l2 :test ,test)) ,l1)
        (remove-if (lambda (x) (member x ,l1 :test ,test)) ,l2)))))

(our-defmacro nset-exclusive-or (list1 list2 &key (test '#'eql))
  "Destructive set-exclusive-or (delegates to set-exclusive-or)."
  `(set-exclusive-or ,list1 ,list2 :test ,test))

;;; ─── subst / subst-if / subst-if-not / nsubst* (FR-496, ANSI CL) ───────────

(our-defmacro subst (new old tree &key (test '#'eql))
  "Substitute NEW for occurrences of OLD anywhere in TREE (non-destructive)."
  (let ((n (gensym "N")) (o (gensym "O")) (fn (gensym "FN")))
    `(let ((,n ,new) (,o ,old) (,fn ,test))
       (labels ((%sr (s)
                  (cond ((funcall ,fn s ,o) ,n)
                        ((consp s) (cons (%sr (car s)) (%sr (cdr s))))
                        (t s))))
         (%sr ,tree)))))

(our-defmacro subst-if (new pred tree)
  "Substitute NEW where PRED is true anywhere in TREE."
  (let ((n (gensym "N")) (p (gensym "P")))
    `(let ((,n ,new) (,p ,pred))
       (labels ((%sif (s)
                  (cond ((funcall ,p s) ,n)
                        ((consp s) (cons (%sif (car s)) (%sif (cdr s))))
                        (t s))))
         (%sif ,tree)))))

(our-defmacro subst-if-not (new pred tree)
  "Substitute NEW where PRED is false anywhere in TREE."
  (let ((p (gensym "P")))
    `(let ((,p ,pred))
       (subst-if ,new (lambda (x) (not (funcall ,p x))) ,tree))))

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
      (append (list 'string-upcase string :start (or start 0))
              (when end (list :end end)))
      (list 'string-upcase string)))

(our-defmacro nstring-downcase (string &key start end)
  "Destructively lowercase STRING."
  (if (or start end)
      (append (list 'string-downcase string :start (or start 0))
              (when end (list :end end)))
      (list 'string-downcase string)))

(our-defmacro nstring-capitalize (string &key start end)
  "Destructively capitalize STRING."
  (if (or start end)
      (append (list 'string-capitalize string :start (or start 0))
              (when end (list :end end)))
      (list 'string-capitalize string)))

;;; ─── bounded string transforms / comparisons ──────────────────────────────

(our-defmacro string-upcase (string &key start end)
  "Uppercase STRING, honoring :START/:END via explicit subseq reconstruction."
  (if (or start end)
      (let ((s (gensym "S"))
            (st (gensym "START"))
            (en (gensym "END")))
        `(let* ((,s ,string)
                (,st ,(or start 0))
                (,en ,(or end `(length ,s))))
           (concatenate 'string
                        (subseq ,s 0 ,st)
                        (string-upcase (subseq ,s ,st ,en))
                        (subseq ,s ,en))))
      `(cl:string-upcase ,string)))

(our-defmacro string-downcase (string &key start end)
  "Lowercase STRING, honoring :START/:END via explicit subseq reconstruction."
  (if (or start end)
      (let ((s (gensym "S"))
            (st (gensym "START"))
            (en (gensym "END")))
        `(let* ((,s ,string)
                (,st ,(or start 0))
                (,en ,(or end `(length ,s))))
           (concatenate 'string
                        (subseq ,s 0 ,st)
                        (string-downcase (subseq ,s ,st ,en))
                        (subseq ,s ,en))))
      `(cl:string-downcase ,string)))

(our-defmacro string-capitalize (string &key start end)
  "Capitalize STRING, honoring :START/:END via explicit subseq reconstruction."
  (if (or start end)
      (let ((s (gensym "S"))
            (st (gensym "START"))
            (en (gensym "END")))
        `(let* ((,s ,string)
                (,st ,(or start 0))
                (,en ,(or end `(length ,s))))
           (concatenate 'string
                        (subseq ,s 0 ,st)
                        (string-capitalize (subseq ,s ,st ,en))
                        (subseq ,s ,en))))
      `(cl:string-capitalize ,string)))

(defun %bounded-string-comparison-form (op string1 string2 start1 end1 start2 end2)
  "Build explicit substring comparison form for OP without macro-generated free vars." 
  (if (or start1 end1 start2 end2)
      (let ((s1 (gensym "S1"))
            (s2 (gensym "S2"))
            (st1 (gensym "START1"))
            (en1 (gensym "END1"))
            (st2 (gensym "START2"))
            (en2 (gensym "END2")))
        `(let* ((,s1 ,string1)
                (,s2 ,string2)
                (,st1 ,(or start1 0))
                (,en1 ,(or end1 `(length ,s1)))
                (,st2 ,(or start2 0))
                (,en2 ,(or end2 `(length ,s2))))
           (,op (subseq ,s1 ,st1 ,en1)
                (subseq ,s2 ,st2 ,en2))))
      `(,op ,string1 ,string2)))

(our-defmacro string< (string1 string2 &key start1 end1 start2 end2)
  (%bounded-string-comparison-form 'cl:string< string1 string2 start1 end1 start2 end2))

(our-defmacro string> (string1 string2 &key start1 end1 start2 end2)
  (%bounded-string-comparison-form 'cl:string> string1 string2 start1 end1 start2 end2))

(our-defmacro string<= (string1 string2 &key start1 end1 start2 end2)
  (%bounded-string-comparison-form 'cl:string<= string1 string2 start1 end1 start2 end2))

(our-defmacro string>= (string1 string2 &key start1 end1 start2 end2)
  (%bounded-string-comparison-form 'cl:string>= string1 string2 start1 end1 start2 end2))

(our-defmacro string-equal (string1 string2 &key start1 end1 start2 end2)
  (%bounded-string-comparison-form 'cl:string-equal string1 string2 start1 end1 start2 end2))

(our-defmacro string= (string1 string2 &key start1 end1 start2 end2)
  (%bounded-string-comparison-form 'cl:string= string1 string2 start1 end1 start2 end2))

(our-defmacro string-not-equal (string1 string2 &key start1 end1 start2 end2)
  (%bounded-string-comparison-form 'cl:string-not-equal string1 string2 start1 end1 start2 end2))

(our-defmacro string-lessp (string1 string2 &key start1 end1 start2 end2)
  (%bounded-string-comparison-form 'cl:string-lessp string1 string2 start1 end1 start2 end2))

(our-defmacro string-greaterp (string1 string2 &key start1 end1 start2 end2)
  (%bounded-string-comparison-form 'cl:string-greaterp string1 string2 start1 end1 start2 end2))

(our-defmacro string-not-greaterp (string1 string2 &key start1 end1 start2 end2)
  (%bounded-string-comparison-form 'cl:string-not-greaterp string1 string2 start1 end1 start2 end2))

(our-defmacro string-not-lessp (string1 string2 &key start1 end1 start2 end2)
  (%bounded-string-comparison-form 'cl:string-not-lessp string1 string2 start1 end1 start2 end2))

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

;;; ─── String stream operations (ANSI CL) ──────────────────────────────────────

(our-defmacro make-string-input-stream (string &optional (start 0) (end nil))
  "Create an input stream that reads characters from STRING."
  `(cl:make-string-input-stream ,string ,start ,@(when end (list end))))

(our-defmacro make-string-output-stream ()
  "Create an output stream that collects characters."
  `(cl:make-string-output-stream))

(our-defmacro get-output-stream-string (stream)
  "Return the string collected by a string output stream."
  `(cl:get-output-stream-string ,stream))

(our-defmacro with-input-from-string ((var string &key (start 0) end index) &body body)
  "Bind VAR to a string input stream reading from STRING, evaluate BODY."
  `(cl:with-input-from-string (,var ,string :start ,start ,@(when end (list :end end))
                                            ,@(when index (list :index index)))
     ,@body))

(our-defmacro with-output-to-string ((var &optional string) &body body)
  "Bind VAR to a string output stream, evaluate BODY, return collected string."
  (if string
      `(cl:with-output-to-string (,var ,string) ,@body)
      `(cl:with-output-to-string (,var) ,@body)))

(our-defmacro open-stream-p (stream)
  "Return true if STREAM is open."
  `(cl:open-stream-p ,stream))

(our-defmacro close (stream &key abort)
  "Close STREAM."
  `(cl:close ,stream ,@(when abort (list :abort abort))))

(our-defmacro stream-element-type (stream)
  "Return the element type of STREAM."
  `(cl:stream-element-type ,stream))

(our-defmacro input-stream-p (stream) `(cl:input-stream-p ,stream))
(our-defmacro output-stream-p (stream) `(cl:output-stream-p ,stream))

(our-defmacro read-char-no-hang (&optional stream (eof-error-p t) eof-value recursive-p)
  "Read a character without blocking, or return EOF-VALUE if none available."
  `(cl:read-char-no-hang ,@(when stream (list stream))
                          ,@(when (not eof-error-p) (list nil eof-value))
                          ,@(when recursive-p (list recursive-p))))

(our-defmacro peek-char (&optional peek-type stream (eof-error-p t) eof-value recursive-p)
  "Peek at the next character."
  `(cl:peek-char ,@(when peek-type (list peek-type))
                  ,@(when stream (list stream))
                  ,@(when (not eof-error-p) (list nil eof-value))
                  ,@(when recursive-p (list recursive-p))))

(our-defmacro unread-char (char &optional stream)
  "Unread CHAR back to STREAM."
  `(cl:unread-char ,char ,@(when stream (list stream))))

(our-defmacro listen (&optional stream)
  "Return true if input is available."
  `(cl:listen ,@(when stream (list stream))))

(our-defmacro read-line (&optional stream (eof-error-p t) eof-value recursive-p)
  "Read a line from STREAM."
  `(cl:read-line ,@(when stream (list stream))
                  ,@(when (not eof-error-p) (list nil eof-value))
                  ,@(when recursive-p (list recursive-p))))

;;; equalp is defined in macros-introspection.lisp

;;; ─── values-list / copy-tree / char-* (ANSI CL) ─────────────────────────────

(our-defmacro values-list (list)
  "Return the elements of LIST as multiple values."
  `(apply #'values ,list))

(our-defmacro copy-tree (tree)
  "Return a copy of TREE with fresh cons cells but the same atoms."
  (let ((x (gensym "X")))
    `(labels ((%ct (,x)
                (if (consp ,x)
                    (cons (%ct (car ,x)) (%ct (cdr ,x)))
                    ,x)))
       (%ct ,tree))))

(our-defmacro char-upcase (char)
  "Return uppercase version of CHAR."
  `(cl:char-upcase ,char))

(our-defmacro char-downcase (char)
  "Return lowercase version of CHAR."
  `(cl:char-downcase ,char))

(our-defmacro char-code (char)
  "Return integer code of CHAR."
  `(cl:char-code ,char))

(our-defmacro code-char (code)
  "Return character for integer CODE."
  `(cl:code-char ,code))

(our-defmacro digit-char-p (char &optional (radix 10))
  "Return digit weight of CHAR in RADIX or nil."
  `(cl:digit-char-p ,char ,radix))

(our-defmacro alpha-char-p (char) `(cl:alpha-char-p ,char))
(our-defmacro alphanumericp  (char) `(cl:alphanumericp ,char))
(our-defmacro upper-case-p   (char) `(cl:upper-case-p ,char))
(our-defmacro lower-case-p   (char) `(cl:lower-case-p ,char))
(our-defmacro both-case-p    (char) `(cl:both-case-p ,char))
(our-defmacro graphic-char-p (char) `(cl:graphic-char-p ,char))
(our-defmacro char-int       (char) `(cl:char-code ,char))
(our-defmacro char-name      (char) `(cl:char-name ,char))

;;; ─── Missing ANSI character comparisons ──────────────────────────────────────

(our-defmacro char-not-equal (c &rest chars)
  `(not (cl:char= ,c ,@chars)))

(our-defmacro char-not-lessp (c &rest chars)
  `(not (cl:char< ,c ,@chars)))

(our-defmacro char-not-greaterp (c &rest chars)
  `(not (cl:char> ,c ,@chars)))

;;; ─── Missing ANSI list / string operations ────────────────────────────────────

(our-defmacro copy-list (lst)
  `(let ((l ,lst))
     (loop for x in l collect x)))

(our-defmacro list-length (lst)
  `(length ,lst))

(our-defmacro string-left-trim (bag string)
  `(cl:string-left-trim ,bag ,string))

(our-defmacro string-right-trim (bag string)
  `(cl:string-right-trim ,bag ,string))

;;; ─── ANSI bit / numeric ───────────────────────────────────────────────────────

(our-defmacro arithmetic-shift (integer count)
  `(cl:ash ,integer ,count))

(our-defmacro integer-length (n)
  `(cl:integer-length ,n))

(our-defmacro logcount (n)
  `(cl:logcount ,n))

(our-defmacro float-sign (float1 &optional float2)
  (if float2
      `(cl:float-sign ,float1 ,float2)
      `(cl:float-sign ,float1)))

(our-defmacro float-digits  (f) `(cl:float-digits ,f))
(our-defmacro float-radix   (f) `(cl:float-radix ,f))
(our-defmacro float-precision (f) `(cl:float-precision ,f))
(our-defmacro decode-float  (f) `(cl:decode-float ,f))
(our-defmacro scale-float   (f e) `(cl:scale-float ,f ,e))
(our-defmacro integer-decode-float (f) `(cl:integer-decode-float ,f))

;;; ─── ANSI control ─────────────────────────────────────────────────────────────

(our-defmacro multiple-value-prog1 (first-form &rest other-forms)
  (let ((vals (gensym "MV")))
    `(let ((,vals (multiple-value-list ,first-form)))
       ,@other-forms
       (values-list ,vals))))

;;; ─── ANSI sequence ────────────────────────────────────────────────────────────

(our-defmacro make-sequence (type length &rest kwargs)
  `(cond ((subtypep ,type 'list)   (make-list ,length ,@kwargs))
         ((subtypep ,type 'vector) (make-array ,length ,@kwargs))
         ((subtypep ,type 'string) (make-string ,length ,@kwargs))
         (t                         (make-list ,length ,@kwargs))))

;;; ─── ANSI hash extras ─────────────────────────────────────────────────────────

(our-defmacro sxhash (x) `(cl:sxhash ,x))
(our-defmacro hash-table-rehash-size (ht) `(cl:hash-table-rehash-size ,ht))
(our-defmacro hash-table-rehash-threshold (ht) `(cl:hash-table-rehash-threshold ,ht))

;;; ─── ANSI CLOS slot ───────────────────────────────────────────────────────────

(our-defmacro slot-boundp (instance slot-name)
  `(cl:slot-boundp ,instance ,slot-name))

(our-defmacro slot-makunbound (instance slot-name)
  `(cl:slot-makunbound ,instance ,slot-name))

(our-defmacro slot-exists-p (instance slot-name)
  `(cl:slot-exists-p ,instance ,slot-name))

;;; ─── ANSI pathname ────────────────────────────────────────────────────────────

(our-defmacro namestring   (x) `(cl:namestring ,x))
(our-defmacro truename     (x) `(cl:truename ,x))
(our-defmacro probe-file   (x) `(cl:probe-file ,x))
(our-defmacro pathname     (x) `(cl:pathname ,x))
(our-defmacro make-pathname (&rest args) `(cl:make-pathname ,@args))
(our-defmacro pathname-name      (x) `(cl:pathname-name ,x))
(our-defmacro pathname-type      (x) `(cl:pathname-type ,x))
(our-defmacro pathname-directory (x) `(cl:pathname-directory ,x))
(our-defmacro pathname-host      (x) `(cl:pathname-host ,x))
(our-defmacro pathname-device    (x) `(cl:pathname-device ,x))
(our-defmacro pathname-version   (x) `(cl:pathname-version ,x))
(our-defmacro merge-pathnames (p &optional d v) (if v `(cl:merge-pathnames ,p ,d ,v) (if d `(cl:merge-pathnames ,p ,d) `(cl:merge-pathnames ,p))))
(our-defmacro enough-namestring (p &optional d) (if d `(cl:enough-namestring ,p ,d) `(cl:enough-namestring ,p)))
(our-defmacro file-namestring  (x) `(cl:file-namestring ,x))
(our-defmacro directory-namestring (x) `(cl:directory-namestring ,x))
(our-defmacro translate-pathname (s f to-wildcard) `(cl:translate-pathname ,s ,f ,to-wildcard))
(our-defmacro wild-pathname-p (x &optional f) (if f `(cl:wild-pathname-p ,x ,f) `(cl:wild-pathname-p ,x)))
