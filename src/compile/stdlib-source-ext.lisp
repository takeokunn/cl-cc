;;;; compile/stdlib-source-ext.lisp — Standard Library Source Extension
;;;;
;;;; Appends additional stdlib definitions to *standard-library-source* defined in
;;;; stdlib-source.lisp (loads first). Kept separate to keep each file manageable.
;;;;
;;;; Contains (appended to *standard-library-source*):
;;;;   - Type predicates (FR-386): rationalp, complexp, realp, floatp
;;;;   - Boole constants + boole-* (FR-493)
;;;;   - Byte manipulation: byte, ldb, dpb, etc. (FR-492/532/494)
;;;;   - digit-char (FR-477), break/invoke-debugger (FR-557)
;;;;   - Restart + handler-bind protocol (FR-421/201)
;;;;   - Method error functions (FR-584)
;;;;   - random-state-p, gensym-counter, macroexpand-hook (FR-509/510/429)
;;;;   - function-lambda-expression, upgraded-array-element-type (FR-549/553)
;;;;   - copy-symbol, compiler-macro-function stubs (FR-536/365)
;;;;   - make-load-form, make-condition, parse-integer, parse-number (FR-550/427/478/391)
;;;;   - Sequence utilities: concatenate variants
;;;;   - Reader/printer/pathname control variables (FR-535/570/573/566/574/395/358)
;;;;
;;;; Load order: after compile/stdlib-source.lisp.
(in-package :cl-cc)

(setf *standard-library-source*
      (concatenate 'string
        *standard-library-source*
    ;; ── FR-386: Type predicates ────────────────────────────────────────────
    "(defun rationalp (x) (or (integerp x) (typep x 'ratio)))"
    "(defun complexp (x) (typep x 'complex))"
    "(defun realp (x) (or (integerp x) (typep x 'ratio) (numberp x)))"
    "(defun floatp (x) (and (numberp x) (not (integerp x)) (not (typep x 'ratio))))"

    ;; ── FR-493: boole + boole-* constants ────────────────────────────────────
    "(defvar boole-clr 0)"
    "(defvar boole-set 1)"
    "(defvar boole-1 2)"
    "(defvar boole-2 3)"
    "(defvar boole-c1 4)"
    "(defvar boole-c2 5)"
    "(defvar boole-and 6)"
    "(defvar boole-ior 7)"
    "(defvar boole-xor 8)"
    "(defvar boole-eqv 9)"
    "(defvar boole-nand 10)"
    "(defvar boole-nor 11)"
    "(defvar boole-andc1 12)"
    "(defvar boole-andc2 13)"
    "(defvar boole-orc1 14)"
    "(defvar boole-orc2 15)"

    "(defun boole (op integer-1 integer-2)
   (cond ((eql op 0) 0)
         ((eql op 1) -1)
         ((eql op 2) integer-1)
         ((eql op 3) integer-2)
         ((eql op 4) (lognot integer-1))
         ((eql op 5) (lognot integer-2))
         ((eql op 6) (logand integer-1 integer-2))
         ((eql op 7) (logior integer-1 integer-2))
         ((eql op 8) (logxor integer-1 integer-2))
         ((eql op 9) (logeqv integer-1 integer-2))
         ((eql op 10) (lognand integer-1 integer-2))
         ((eql op 11) (lognor integer-1 integer-2))
         ((eql op 12) (logandc1 integer-1 integer-2))
         ((eql op 13) (logandc2 integer-1 integer-2))
         ((eql op 14) (logorc1 integer-1 integer-2))
         ((eql op 15) (logorc2 integer-1 integer-2))
         (t (error \"Invalid boole operation: ~S\" op))))"

    ;; ── FR-492/FR-532/FR-494: Byte manipulation ──────────────────────────────
    "(defun byte (size position) (cons size position))"
    "(defun byte-size (byte-spec) (car byte-spec))"
    "(defun byte-position (byte-spec) (cdr byte-spec))"

    "(defun ldb (byte-spec integer)
   (logand (ash integer (- (byte-position byte-spec)))
           (1- (ash 1 (byte-size byte-spec)))))"

    "(defun ldb-test (byte-spec integer)
   (not (zerop (ldb byte-spec integer))))"

    "(defun dpb (newbyte byte-spec integer)
   (let ((size (byte-size byte-spec))
         (pos  (byte-position byte-spec)))
     (logior (logand integer (lognot (ash (1- (ash 1 size)) pos)))
             (ash (logand newbyte (1- (ash 1 size))) pos))))"

    "(defun deposit-field (newbyte byte-spec integer)
   (let ((size (byte-size byte-spec))
         (pos  (byte-position byte-spec)))
     (logior (logand integer (lognot (ash (1- (ash 1 size)) pos)))
             (logand newbyte (ash (1- (ash 1 size)) pos)))))"

    "(defun mask-field (byte-spec integer)
   (logand integer (ash (1- (ash 1 (byte-size byte-spec)))
                        (byte-position byte-spec))))"

    ;; ── FR-477: digit-char ───────────────────────────────────────────────────
    "(defun digit-char (weight &optional (radix 10))
   (if (and (>= weight 0) (< weight radix))
       (if (< weight 10)
           (code-char (+ weight 48))
           (code-char (+ weight 55)))
       nil))"

    ;; ── FR-557: break / invoke-debugger ──────────────────────────────────────
    "(defvar *debugger-hook* nil)"
    "(defvar *break-on-signals* nil)"

    ;; ── FR-421: restart protocol dynamic variable ──────────────────────────
    "(defvar *%active-restarts* nil)"

    ;; ── FR-201: handler-bind dynamic handler registry ────────────────────
    "(defvar *%condition-handlers* nil)"

    ;; signal is defined as a macro in macros-stdlib.lisp for correct symbol resolution

    "(defun invoke-debugger (condition)
   (if *debugger-hook*
       (let ((hook *debugger-hook*))
         (setq *debugger-hook* nil)
         (funcall hook condition hook))
       (error condition)))"

    "(defun break (&optional format-control)
   (when format-control
     (format *error-output* format-control)
     (terpri *error-output*))
   nil)"

    ;; ── FR-584: Method error functions ───────────────────────────────────────
    "(defun invalid-method-error (method format-control)
   (error format-control))"

    "(defun method-combination-error (format-control)
   (error format-control))"

    ;; ── FR-509: random-state-p ───────────────────────────────────────────────
    "(defun random-state-p (x) (typep x 'random-state))"

    ;; ── FR-511: gentemp ──────────────────────────────────────────────────────
    "(defvar *gentemp-counter* 0)"
    "(defun gentemp (&optional (prefix \"T\") (package *package*))
   (declare (ignore package))
   (setq *gentemp-counter* (+ *gentemp-counter* 1))
   (intern (concatenate 'string prefix (write-to-string *gentemp-counter*))))"

    ;; ── FR-510: *gensym-counter* ─────────────────────────────────────────────
    "(defvar *gensym-counter* 0)"

    ;; ── FR-429: *macroexpand-hook* ───────────────────────────────────────────
    "(defvar *macroexpand-hook* #'funcall)"

    ;; ── FR-549: function-lambda-expression ───────────────────────────────────
    "(defun function-lambda-expression (fn)
   (declare (ignore fn))
   (values nil nil nil))"

    ;; ── FR-553: upgraded-array-element-type / upgraded-complex-part-type ─────
    "(defun upgraded-array-element-type (typespec &optional environment)
   (declare (ignore typespec environment))
   t)"

    "(defun upgraded-complex-part-type (typespec &optional environment)
   (declare (ignore typespec environment))
   'real)"

    ;; ── FR-536: copy-symbol (basic — no property copying) ──────────────────
    "(defun copy-symbol (symbol &optional copy-properties)
   (declare (ignore copy-properties))
   (make-symbol (symbol-name symbol)))"

    ;; ── FR-365: compiler-macro-function (stub) ──────────────────────────────
    "(defun compiler-macro-function (name &optional environment)
   (declare (ignore name environment))
   nil)"

    ;; ── FR-550: make-load-form / make-load-form-saving-slots ────────────────
    "(defgeneric make-load-form (object &optional environment))"
    "(defmethod make-load-form ((object t) &optional environment)
       (declare (ignore environment))
       (cond
         ((or (numberp object) (characterp object)) (values object nil))
         ((symbolp object) (values (list 'quote object) nil))
         ((stringp object) (values object nil))
         (t (values nil nil))))"

    "(defun make-load-form-saving-slots (object &rest slot-names)
       (declare (ignore slot-names))
       (if (hash-table-p object)
           (let ((class (gethash :__class__ object)))
             (if class
                 (values (list 'make-instance (list 'quote (gethash :__name__ class))) nil)
                 (values nil nil)))
           (values nil nil)))"

    ;; FR-624: subtypep — removed stub; now delegated to host CL via vm-host-bridge

    ;; ── FR-427: make-condition ──────────────────────────────────────────────
    "(defun make-condition (type &rest initargs)
   (apply #'make-instance type initargs))"

    ;; ── FR-478: parse-integer (basic implementation) ─────────────────────────
    "(defun %parse-integer-impl (string start end radix junk-allowed)
   (let ((i start) (sign 1) (result 0) (found nil) (len (if end (min end (length string)) (length string))))
     (loop while (and (< i len) (char= (char string i) #\\Space))
           do (setq i (+ i 1)))
     (when (< i len)
       (cond ((char= (char string i) #\\+) (setq i (+ i 1)))
             ((char= (char string i) #\\-) (setq sign -1) (setq i (+ i 1)))))
     (loop while (< i len)
           do (let ((code (char-code (char string i))))
                (let ((digit (cond
                               ((and (>= code 48) (< code (min 58 (+ 48 radix))))
                                (- code 48))
                               ((and (>= radix 11) (>= code 65) (< code (+ 55 radix)))
                                (- code 55))
                               ((and (>= radix 11) (>= code 97) (< code (+ 87 radix)))
                                (- code 87))
                               (t nil))))
                  (if digit
                      (progn (setq result (+ (* result radix) digit))
                             (setq found t)
                             (setq i (+ i 1)))
                      (if junk-allowed
                          (return nil)
                          (error \"not a valid integer\"))))))
     (loop while (and (< i len) (char= (char string i) #\\Space))
           do (setq i (+ i 1)))
     (when (and (< i len) (not junk-allowed))
       (error \"junk in string\"))
     (if found (values (* sign result) i)
       (if junk-allowed (values nil i)
         (error \"not an integer\")))))"

    ;; ── FR-391: parse-number (delegating to read-from-string) ──────────
    "(defun parse-number (string &key (start 0) (end nil) (radix 10))
   (let* ((substr (subseq string start (or end (length string))))
          (result (read-from-string substr)))
     (if (numberp result) result
       (error \"~S is not a number\" substr))))"

;;; ── ANSI Base Classes (FR-528) ──────────────────────────────────────
    "(defclass standard-object () ())"
    "(defclass structure-object () ())"

;;; ── Setf Expansion (FR-355) ────────────────────────────────────────

    "(defun get-setf-expansion (place &optional env)
       (declare (ignore env))
       (if (symbolp place)
           (let ((store (gensym \"STORE\")))
             (values nil nil (list store) (list 'setq place store) place))
           (let* ((temps (mapcar (lambda (a) (declare (ignore a)) (gensym \"T\")) (cdr place)))
                  (store (gensym \"STORE\"))
                  (access (cons (car place) temps))
                  (setter (list 'setf access store)))
             (values temps (cdr place) (list store) setter access))))"

;;; ── CLOS Protocol Functions (FR-379/524/525) ───────────────────────────

    "(defun ensure-generic-function (name &rest options)
       (declare (ignore options))
       name)"

    "(defgeneric initialize-instance (instance &rest initargs))"
    "(defmethod initialize-instance ((instance t) &rest initargs)
       (declare (ignore initargs))
       instance)"

    "(defgeneric allocate-instance (class &rest initargs))"
    "(defmethod allocate-instance ((class t) &rest initargs)
       (declare (ignore initargs))
       (make-instance class))"

    "(defgeneric slot-unbound (class instance slot-name))"
    "(defmethod slot-unbound ((class t) (instance t) slot-name)
       (error 'unbound-slot :name slot-name :instance instance))"

    "(defgeneric slot-missing (class object slot-name operation &optional new-value))"
    "(defmethod slot-missing ((class t) object slot-name operation &optional new-value)
       (declare (ignore new-value))
       (error (format nil \"The slot ~A is missing from object ~A of class ~A (operation: ~A)\"
                       slot-name object class operation)))"

    "(defgeneric no-applicable-method (gf &rest args))"
    "(defmethod no-applicable-method ((gf t) &rest args)
       (error (format nil \"No applicable method for ~A with args ~A\" gf args)))"

    "(defgeneric no-next-method (gf method &rest args))"
    "(defmethod no-next-method ((gf t) method &rest args)
       (declare (ignore method))
       (error (format nil \"There is no next method for ~A when called with ~A\" gf args)))"

    "(defgeneric print-object (object stream))"
    "(defmethod print-object ((object t) stream)
       (prin1 object stream))"

    "(defgeneric update-instance-for-different-class (previous current &rest initargs))"
    "(defmethod update-instance-for-different-class ((previous t) current &rest initargs)
       (declare (ignore previous))
       (apply #'reinitialize-instance current initargs))"

    "(defgeneric update-instance-for-changed-class (instance added-slots discarded-slots plist &rest initargs))"
    "(defmethod update-instance-for-changed-class ((instance t) added-slots discarded-slots plist &rest initargs)
       (declare (ignore added-slots discarded-slots plist))
       (apply #'reinitialize-instance instance initargs))"

    "(defgeneric compute-applicable-methods (gf args))"
    "(defmethod compute-applicable-methods ((gf t) args)
       (declare (ignore args))
       nil)"

    "(defgeneric find-method (gf qualifiers specializers &optional errorp))"
    "(defmethod find-method ((gf t) qualifiers specializers &optional errorp)
       (let* ((methods-ht (gethash :__methods__ gf))
              (spec-key (if (and (listp specializers) (= (length specializers) 1))
                            (let ((s (first specializers)))
                              (if (symbolp s) (symbol-name s) s))
                            specializers))
              (method (when methods-ht (gethash spec-key methods-ht))))
         (if method
             method
             (if errorp
                 (error (format nil \"No method on ~A with qualifiers ~A and specializers ~A\"
                                gf qualifiers specializers))
                 nil))))"

    "(defgeneric add-method (gf method))"
    "(defmethod add-method ((gf t) method)
       (let ((methods-ht (gethash :__methods__ gf)))
         (when methods-ht
           (setf (gethash (if (functionp method) \"T\" \"T\") methods-ht) method))
         gf))"

    "(defgeneric remove-method (gf method))"
    "(defmethod remove-method ((gf t) method)
       (let ((methods-ht (gethash :__methods__ gf)))
         (when methods-ht
           (maphash (lambda (k v)
                      (when (eq v method)
                        (remhash k methods-ht)))
                    methods-ht))
         gf))"

;;; ── ANSI Condition Type Hierarchy (FR-424) ─────────────────────────────

    "(define-condition serious-condition (condition) ())"
    "(define-condition simple-condition (condition)
       ((format-control :initarg :format-control :reader simple-condition-format-control)
        (format-arguments :initarg :format-arguments :reader simple-condition-format-arguments)))"
    "(define-condition simple-warning (simple-condition warning) ())"
    "(define-condition simple-error (simple-condition error) ())"
    "(define-condition type-error (error)
       ((datum :initarg :datum :reader type-error-datum)
        (expected-type :initarg :expected-type :reader type-error-expected-type)))"
    "(define-condition simple-type-error (simple-condition type-error) ())"
    "(define-condition arithmetic-error (error)
       ((operation :initarg :operation :reader arithmetic-error-operation)
        (operands :initarg :operands :reader arithmetic-error-operands)))"
    "(define-condition division-by-zero (arithmetic-error) ())"
    "(define-condition floating-point-overflow (arithmetic-error) ())"
    "(define-condition floating-point-underflow (arithmetic-error) ())"
    "(define-condition cell-error (error)
       ((name :initarg :name :reader cell-error-name)))"
    "(define-condition unbound-variable (cell-error) ())"
    "(define-condition undefined-function (cell-error) ())"
    "(define-condition unbound-slot (cell-error)
       ((instance :initarg :instance :reader unbound-slot-instance)))"
    "(define-condition control-error (error) ())"
    "(define-condition program-error (error) ())"
    "(define-condition package-error (error)
       ((package :initarg :package :reader package-error-package)))"
    "(define-condition stream-error (error)
       ((stream :initarg :stream :reader stream-error-stream)))"
    "(define-condition end-of-file (stream-error) ())"
    "(define-condition file-error (error)
       ((pathname :initarg :pathname :reader file-error-pathname)))"
    "(define-condition print-not-readable (error)
       ((object :initarg :object :reader print-not-readable-object)))"
    "(define-condition storage-condition (serious-condition) ())"
    "(define-condition parse-error (error) ())"
    "(define-condition reader-error (parse-error stream-error) ())"

    ;; ── FR-535: Reader control variables ─────────────────────────────────────
    "(defvar *read-base* 10)"
    "(defvar *read-default-float-format* 'single-float)"
    "(defvar *read-suppress* nil)"
    "(defvar *standard-input* *standard-input*)"
    "(defvar *standard-output* *standard-output*)"
    "(defvar *error-output* *error-output*)"
    "(defvar *trace-output* *trace-output*)"
    "(defvar *debug-io* *debug-io*)"
    "(defvar *query-io* *query-io*)"
    ;; ── FR-573: *read-eval* ───────────────────────────────────────────────────
    "(defvar *read-eval* t)"
    ;; ── FR-358: *readtable* (stub — full Readtable API not yet implemented) ───
    "(defvar *readtable* nil)"
    ;; ── FR-570: Printer control variables ────────────────────────────────────
    "(defvar *print-circle* nil)"
    "(defvar *print-gensym* t)"
    "(defvar *print-case* :upcase)"
    ;; ── FR-535: More printer variables ───────────────────────────────────────
    "(defvar *print-array* t)"
    "(defvar *print-readably* nil)"
    ;; ── FR-357: Pretty-printer variables ─────────────────────────────────────
    "(defvar *print-pretty* nil)"
    "(defvar *print-right-margin* nil)"
    "(defvar *print-miser-width* nil)"
    "(defvar *print-lines* nil)"
    "(defvar *print-pprint-dispatch* nil)"
    ;; ── FR-566: Pathname variables ────────────────────────────────────────────
    "(defvar *default-pathname-defaults* nil)"
    ;; ── FR-574: Load/compile pathname variables ───────────────────────────────
    "(defvar *load-pathname* nil)"
    "(defvar *load-truename* nil)"
    "(defvar *compile-file-pathname* nil)"
    "(defvar *compile-file-truename* nil)"
    "(defvar *load-verbose* t)"
    "(defvar *load-print* nil)"
    "(defvar *compile-verbose* t)"
    "(defvar *compile-print* nil)"
    ;; ── FR-395: Compiler policy ───────────────────────────────────────────────
    "(defvar *compiler-policy* nil)"
    ;; ── FR-358: Readtable variable ────────────────────────────────────────────
    "(defvar *readtable* nil)"

      ))
