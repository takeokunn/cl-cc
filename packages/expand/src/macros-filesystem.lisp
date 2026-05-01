(in-package :cl-cc/expand)

;;; ─── File system operations (FR-479) ─────────────────────────────────────────
;;; probe-file, rename-file, delete-file, file-write-date, file-author,
;;; directory, ensure-directories-exist are registered as host bridges in vm.lisp.

;;; ─── Pathname accessors (FR-566) — host bridge ────────────────────────────────
;;; These delegate to host CL pathname functions via vm-host-bridge.

;;; ─── time (FR-431) ────────────────────────────────────────────────────────────

(our-defmacro time (form)
  "Evaluate FORM, print elapsed time, return its value."
  (let ((start (gensym "START"))
        (result (gensym "RESULT")))
    `(let* ((,start  (get-universal-time))
            (,result ,form))
       (format t "~&Elapsed: ~A second(s)~%" (- (get-universal-time) ,start))
       ,result)))

;;; ─── room (FR-434) ────────────────────────────────────────────────────────────

(our-defmacro room (&optional detail)
  "Print memory usage information via the host SBCL runtime."
  (if detail
      `(sb-ext::room ,detail)
      '(sb-ext::room)))

;;; ─── dribble (FR-514) ─────────────────────────────────────────────────────────

(our-defmacro dribble (&optional path)
  "Start/stop dribbling I/O to a file via the host SBCL runtime."
  `(progn (cl:dribble ,path) nil))

;;; ─── pprint (FR-357) ──────────────────────────────────────────────────────────

(our-defmacro pprint (object &optional stream)
  "Pretty-print OBJECT (delegates to print in cl-cc)."
  (if stream
      `(print ,object ,stream)
      `(print ,object)))

;;; ─── write (FR-569) — accepts all keywords, delegates to princ/prin1 ─────────

(our-defmacro write (object &key stream (escape t) readably pretty circle
                             gensym array base radix case level length lines
                             right-margin miser-width pprint-dispatch)
  "Write OBJECT to STREAM, binding print control variables from keyword args."
  (let ((obj-g  (gensym "OBJ"))
        (str-g  (gensym "STR"))
        (base-g (gensym "BASE")) (radix-g (gensym "RADIX"))
        (esc-g  (gensym "ESC"))  (level-g (gensym "LVL"))
        (len-g  (gensym "LEN")))
     `(let* ((,obj-g   ,object)
             (,str-g   ,(or stream '*standard-output*))
             (,base-g  ,(or base *print-base*))
             (,radix-g ,(or radix *print-radix*))
             (,esc-g   ,(or escape *print-escape*))
             (,level-g ,(or level *print-level*))
             (,len-g   ,(or length *print-length*)))
        ;; Evaluate remaining ignored args for side effects
        (progn ,readably ,pretty ,circle ,gensym ,array ,case
               ,lines ,right-margin ,miser-width ,pprint-dispatch)
       (let ((*print-base*   ,base-g)  (*print-radix*  ,radix-g)
             (*print-escape* ,esc-g)   (*print-level*  ,level-g)
             (*print-length* ,len-g))
         (write-string (write-to-string ,obj-g) ,str-g))
       ,obj-g)))

;;; ─── locally (FR-397) ─────────────────────────────────────────────────────────

(our-defmacro locally (&rest forms)
  "Evaluate FORMS in a local declaration scope (declarations ignored in cl-cc)."
  (let ((body (if (and forms (listp (car forms))
                       (eq (caar forms) 'declare))
                  (cdr forms)
                  forms)))
    `(progn ,@body)))

;;; ─── documentation / (setf documentation) (FR-607) ─────────────────────────
;;; Docstrings are registered at compile time in *documentation-table* (CL-level
;;; hash table) by the defun expander. At runtime, the VM queries this table via
;;; the %get-documentation host bridge function.

(our-defmacro documentation (x doc-type)
  "Return the documentation string for X of type DOC-TYPE, or nil."
  `(%get-documentation ,x ,doc-type))

;;; ─── with-hash-table-iterator (FR-497) ───────────────────────────────────────

(our-defmacro with-hash-table-iterator (spec &rest body)
  "Create a hash table iterator named (first spec) over (second spec) within BODY."
  (let ((name     (car spec))
        (table    (cadr spec))
        (keys-var (gensym "KEYS"))
        (tbl-var  (gensym "TBL")))
    `(let* ((,tbl-var  ,table)
            (,keys-var (hash-table-keys ,tbl-var)))
       (flet ((,name ()
                (if ,keys-var
                    (let ((k (car ,keys-var)))
                      (setf ,keys-var (cdr ,keys-var))
                      (values t k (gethash k ,tbl-var)))
                    (values nil nil nil))))
         ,@body))))

;;; ─── read-preserving-whitespace (FR-658) ────────────────────────────────────

(our-defmacro read-preserving-whitespace (&optional (stream '*standard-input*)
                                           (eof-error-p t) eof-value recursive-p)
  "Read a form, preserving terminal whitespace (delegates to read in cl-cc)."
  `(read ,stream ,eof-error-p ,eof-value ,recursive-p))

;;; ─── write-sequence / read-sequence helpers (FR-590) ─────────────────────────

(our-defmacro write-sequence (sequence stream &key (start 0) end)
  "Write elements of SEQUENCE to STREAM."
  (let ((seq (gensym "SEQ"))
        (str (gensym "STR"))
        (i   (gensym "I"))
        (ev  (gensym "E")))
    `(let* ((,seq ,sequence)
            (,str ,stream)
            (,ev  ,(or end `(length ,seq))))
       (do ((,i ,start (+ ,i 1)))
           ((>= ,i ,ev) ,seq)
         (let ((elem (elt ,seq ,i)))
           (if (characterp elem)
               (write-char elem ,str)
               (write-byte elem ,str)))))))

(our-defmacro read-sequence (sequence stream &key (start 0) end)
  "Read from STREAM into SEQUENCE."
  (let ((seq (gensym "SEQ"))
        (str (gensym "STR"))
        (i   (gensym "I"))
        (ev  (gensym "E"))
        (ch  (gensym "CH")))
    `(let* ((,seq ,sequence)
            (,str ,stream)
            (,ev  ,(or end `(length ,seq))))
       (do ((,i ,start (+ ,i 1)))
           ((>= ,i ,ev) ,i)
         (let ((,ch (read-char ,str nil nil)))
           (if (null ,ch)
               (return ,i)
               (setf (elt ,seq ,i) ,ch)))))))

;;; ─── read-delimited-list (FR-659) ───────────────────────────────────────────

;; read-delimited-list is registered in *vm-host-bridge-functions* — no macro needed.
;; Calls with an optional stream arg default to *standard-input* via host CL.

;;; ─── read-char-no-hang (FR-568) ──────────────────────────────────────────────

(register-macro 'read-char-no-hang
  (lambda (form env)
    (declare (ignore env))
    (list 'read-char
          (or (second form) '*standard-input*)
          (if (third form) (third form) t)
          (fourth form)
          (fifth form))))

;;; ─── bit-nor / bit-nand / bit-eqv / bit-andc1 / bit-andc2 / bit-orc1 / bit-orc2 (FR-635) ──

(our-defmacro bit-nor (bit-array1 bit-array2)
  "Element-wise NOR of two bit arrays."
  `(bit-not (bit-ior ,bit-array1 ,bit-array2)))

(our-defmacro bit-nand (bit-array1 bit-array2)
  "Element-wise NAND of two bit arrays."
  `(bit-not (bit-and ,bit-array1 ,bit-array2)))

(our-defmacro bit-eqv (bit-array1 bit-array2)
  "Element-wise XNOR (equivalence) of two bit arrays."
  `(bit-not (bit-xor ,bit-array1 ,bit-array2)))

(our-defmacro bit-andc1 (bit-array1 bit-array2)
  "Element-wise AND of (NOT bit-array1) and bit-array2."
  `(bit-and (bit-not ,bit-array1) ,bit-array2))

(our-defmacro bit-andc2 (bit-array1 bit-array2)
  "Element-wise AND of bit-array1 and (NOT bit-array2)."
  `(bit-and ,bit-array1 (bit-not ,bit-array2)))

(our-defmacro bit-orc1 (bit-array1 bit-array2)
  "Element-wise OR of (NOT bit-array1) and bit-array2."
  `(bit-ior (bit-not ,bit-array1) ,bit-array2))

(our-defmacro bit-orc2 (bit-array1 bit-array2)
  "Element-wise OR of bit-array1 and (NOT bit-array2)."
  `(bit-ior ,bit-array1 (bit-not ,bit-array2)))

;;; end of filesystem/runtime helpers
