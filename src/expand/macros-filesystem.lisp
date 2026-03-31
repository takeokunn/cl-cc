(in-package :cl-cc)

;;; ─── File system operations (FR-479) ─────────────────────────────────────────
;;; probe-file, rename-file, delete-file, file-write-date, file-author,
;;; directory, ensure-directories-exist are registered as host bridges in vm.lisp.

;;; ─── Pathname accessors (FR-566) — host bridge ────────────────────────────────
;;; These delegate to host CL pathname functions via vm-host-bridge.

;;; ─── with-compilation-unit (FR-363) ──────────────────────────────────────────

(our-defmacro with-compilation-unit (options &rest body)
  "Execute BODY as a compilation unit (stub — no special behavior in cl-cc)."
  `(progn ,@body))

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

;;; ─── break (FR-557) ──────────────────────────────────────────────────────────

(our-defmacro break (&optional (format-str "") &rest args)
  "Signal a breakpoint (stub — prints message and continues in cl-cc)."
  `(progn (format t ,(concatenate 'string "~&BREAK: " format-str "~%") ,@args) nil))

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

;;; ─── formatter macro (FR-698) ────────────────────────────────────────────────

(our-defmacro formatter (control-string)
  "Return a function that formats using CONTROL-STRING (cl-cc stub)."
  `(lambda (stream &rest args)
     (apply #'format stream ,control-string args)))

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

;;; ─── write-sequence / read-sequence stubs (FR-590) ───────────────────────────

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

;;; ─── bit-nor / bit-nand / bit-eqv / bit-andc1 / bit-andc2 / bit-orc1 / bit-orc2 (FR-635) ──

;; Note: The optional result-bit-array arg is accepted for ANSI compatibility
;; but ignored — cl-cc's bit-and/bit-or/bit-xor are binary-only builtins.
(our-defmacro bit-nor (bit-array1 bit-array2 &optional result-bit-array)
  "Element-wise NOR of two bit arrays."
  `(progn ,result-bit-array (bit-not (bit-ior ,bit-array1 ,bit-array2))))

(our-defmacro bit-nand (bit-array1 bit-array2 &optional result-bit-array)
  "Element-wise NAND of two bit arrays."
  `(progn ,result-bit-array (bit-not (bit-and ,bit-array1 ,bit-array2))))

(our-defmacro bit-eqv (bit-array1 bit-array2 &optional result-bit-array)
  "Element-wise XNOR (equivalence) of two bit arrays."
  `(progn ,result-bit-array (bit-not (bit-xor ,bit-array1 ,bit-array2))))

(our-defmacro bit-andc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Element-wise AND of (NOT bit-array1) and bit-array2."
  `(progn ,result-bit-array (bit-and (bit-not ,bit-array1) ,bit-array2)))

(our-defmacro bit-andc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Element-wise AND of bit-array1 and (NOT bit-array2)."
  `(progn ,result-bit-array (bit-and ,bit-array1 (bit-not ,bit-array2))))

(our-defmacro bit-orc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Element-wise OR of (NOT bit-array1) and bit-array2."
  `(progn ,result-bit-array (bit-ior (bit-not ,bit-array1) ,bit-array2)))

(our-defmacro bit-orc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Element-wise OR of bit-array1 and (NOT bit-array2)."
  `(progn ,result-bit-array (bit-ior ,bit-array1 (bit-not ,bit-array2))))

;;; ─── pprint-related stubs (FR-357) ───────────────────────────────────────────

(our-defmacro pprint-fill (stream object &optional colon-p at-sign-p)
  "Pretty-print list with fill style (stub — delegates to prin1)."
  `(progn ,colon-p ,at-sign-p (prin1 ,object ,stream)))

(our-defmacro pprint-linear (stream object &optional colon-p at-sign-p)
  "Pretty-print list with linear style (stub — delegates to prin1)."
  `(progn ,colon-p ,at-sign-p (prin1 ,object ,stream)))

(our-defmacro pprint-tabular (stream object &optional colon-p at-sign-p tabsize)
  "Pretty-print list with tabular style (stub)."
  `(progn ,colon-p ,at-sign-p ,tabsize (prin1 ,object ,stream)))

(our-defmacro pprint-newline (kind &optional stream)
  "Emit a conditional newline (stub — ignored in cl-cc)."
  `(progn ,kind ,stream nil))

(our-defmacro pprint-tab (kind column colinc &optional stream)
  "Move to column for tabbing (stub — ignored in cl-cc)."
  `(progn ,kind ,column ,colinc ,stream nil))

(our-defmacro pprint-indent (relative-to n &optional stream)
  "Set indentation (stub — ignored in cl-cc)."
  `(progn ,relative-to ,n ,stream nil))

(our-defmacro pprint-logical-block (spec &rest body)
  "Execute BODY as a logical block for pretty printing (stub)."
  (let* ((stream-symbol (car spec))
         (rest-spec     (cddr spec))  ; skip stream-sym and list
         (prefix        (getf rest-spec :prefix))
         (per-line-prefix (getf rest-spec :per-line-prefix))
         (suffix        (getf rest-spec :suffix)))
    `(progn ,prefix ,per-line-prefix ,suffix ,stream-symbol ,@body)))

;;; ─── file-string-length stub (FR-591) ────────────────────────────────────────

(our-defmacro file-string-length (stream object)
  "Return the number of UTF-8 octets needed to write OBJECT to STREAM."
  (let ((obj-g (gensym "OBJ")))
    `(progn ,stream
       (let ((,obj-g ,object))
         (if (characterp ,obj-g)
             (length (string-to-octets (string ,obj-g) :encoding :utf-8))
             (length (string-to-octets ,obj-g :encoding :utf-8)))))))

;;; ─── stream-external-format stub (FR-562) ────────────────────────────────────

(our-defmacro stream-external-format (stream)
  "Return the external format of STREAM (stub returns :default)."
  `(progn ,stream :default))

;;; with-input-from-string :start/:end/:index (FR-608) handled in the definition above (line ~486).

;;; ─── trace / untrace stubs (FR-432) ──────────────────────────────────────────

(our-defmacro trace (&rest function-names)
  "Enable tracing for FUNCTION-NAMES via the host SBCL runtime."
  `(progn (cl:trace ,@function-names) nil))

(our-defmacro untrace (&rest function-names)
  "Disable tracing for FUNCTION-NAMES via the host SBCL runtime."
  `(progn (cl:untrace ,@function-names) nil))

;;; ─── step stub (FR-433) ───────────────────────────────────────────────────────

(our-defmacro step (form)
  "Single-step through FORM via the host SBCL runtime."
  `(cl:step ,form))

;;; ─── disassemble stub (FR-576) ───────────────────────────────────────────────

(our-defmacro disassemble (function)
  "Print disassembly of FUNCTION via the host SBCL runtime."
  `(progn (cl:disassemble ,function) nil))

;;; ─── inspect stub (FR-577) ────────────────────────────────────────────────────

(our-defmacro inspect (object)
  "Inspect OBJECT via the host SBCL runtime."
  `(progn (cl:inspect ,object) nil))

;;; ─── apropos / apropos-list (FR-435) — host bridge ───────────────────────────
;;; Registered in vm.lisp as host bridges.

;;; ─── ed stub (FR-515) ─────────────────────────────────────────────────────────

(our-defmacro ed (&optional x)
  "Invoke editor on X (stub — no-op in cl-cc)."
  `(progn ,x nil))

;;; ─── invoke-debugger stub (FR-557) ───────────────────────────────────────────

(our-defmacro invoke-debugger (condition)
  "Invoke the debugger on CONDITION (stub — signals error in cl-cc)."
  `(error ,condition))

;;; ─── declare stub wrappers for unsupported declaration types ─────────────────
;;; The compiler already accepts declare at the syntax level; unsupported
;;; specifiers are silently ignored. No macro wrappers needed.

;;; ─── compiler-let stub (FR-439) ───────────────────────────────────────────────

(our-defmacro compiler-let (bindings &rest body)
  "Bind variables at compile time (stub — acts as let at runtime in cl-cc)."
  `(let ,bindings ,@body))

;;; ─── #P pathname literal — registered in vm.lisp as host bridge ──────────────

;;; ─── read-char-no-hang (FR-568) ──────────────────────────────────────────────

(our-defmacro read-char-no-hang (&optional (stream '*standard-input*)
                                  (eof-error-p t) eof-value recursive-p)
  "Return next char if available without waiting, else nil (stub — delegates to read-char)."
  `(read-char ,stream ,eof-error-p ,eof-value ,recursive-p))

;;; ─── FR-358: Readtable API stubs ─────────────────────────────────────────────
;;; cl-cc uses a fixed built-in lexer; readtables are not user-customizable.
;;; These stubs accept args for ANSI CL compatibility without signaling errors.

(our-defmacro readtablep (x)
  "Return nil (cl-cc has no user-defined readtables)."
  `(progn ,x nil))

(our-defmacro copy-readtable (&optional from to)
  "Return nil (cl-cc readtable stub)."
  `(progn ,from ,to nil))

(our-defmacro readtable-case (readtable)
  "Return :upcase (cl-cc always uses upcase)."
  `(progn ,readtable :upcase))

(our-defmacro set-macro-character (char fn &optional non-terminating-p readtable)
  "No-op stub — cl-cc's reader is not user-extensible."
  `(progn ,char ,fn ,non-terminating-p ,readtable t))

(our-defmacro get-macro-character (char &optional readtable)
  "Return (values nil nil) — no user-defined macro chars."
  `(progn ,char ,readtable (values nil nil)))

(our-defmacro set-dispatch-macro-character (disp-char sub-char fn &optional readtable)
  "No-op stub."
  `(progn ,disp-char ,sub-char ,fn ,readtable t))

(our-defmacro get-dispatch-macro-character (disp-char sub-char &optional readtable)
  "Return nil — no user-defined dispatch chars."
  `(progn ,disp-char ,sub-char ,readtable nil))

(our-defmacro make-dispatch-macro-character (char &optional non-terminating-p readtable)
  "No-op stub."
  `(progn ,char ,non-terminating-p ,readtable t))

(our-defmacro set-syntax-from-char (to-char from-char &optional to-readtable from-readtable)
  "No-op stub."
  `(progn ,to-char ,from-char ,to-readtable ,from-readtable t))

;;; ─── FR-589: compile-file stub ───────────────────────────────────────────────

(our-defmacro compile-file (pathname &rest keys)
  ;; Load PATHNAME (cl-cc compiles by loading). Extra keyword args ignored.
  (let ((path-var (gensym "PATH")))
    (declare (ignore keys))
    `(let ((,path-var ,pathname))
       (our-load ,path-var)
       (values (probe-file ,path-var) nil nil))))

;;; ─── ------------------------------------------------------------
