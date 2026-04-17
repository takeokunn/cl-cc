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
