;;;; compile/stdlib-source-ext.lisp — Standard Library Source Extension
;;;;
;;;; Appends additional stdlib definitions to *standard-library-source* defined in
;;;; stdlib-source.lisp (loads first). Kept separate to keep each file manageable.
;;;;
;;;; Contains (appended to *standard-library-source*):
;;;;   - Type predicates (FR-386): rationalp, complexp, realp, floatp
;;;;   - Boole constants + boole-* (FR-493)
;;;;   - Byte manipulation: byte, ldb, dpb, etc. (FR-492/532/494)
;;;;   - digit-char (FR-477)
;;;;   - Restart + handler-bind protocol (FR-421/201)
;;;;   - Method error functions (FR-584)
;;;;   - random-state-p, gensym-counter, macroexpand-hook (FR-509/510/429)
;;;;   - function-lambda-expression, upgraded-array-element-type (FR-549/553)
;;;;   - make-load-form, make-condition, parse-integer, parse-number (FR-550/427/478/391)
;;;;   - Sequence utilities: concatenate variants
;;;;
;;;; CLOS protocol, condition hierarchy, and reader/printer/pathname control
;;;; variables are in stdlib-source-clos.lisp.
;;;;
;;;; Load order: after compile/stdlib-source.lisp.
(in-package :cl-cc/stdlib)

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

    ;; ── debugger-related dynamic vars ───────────────────────────────────────
    "(defvar *debugger-hook* nil)"
    "(defvar *break-on-signals* nil)"

    ;; ── FR-421: restart protocol dynamic variable ──────────────────────────
    "(defvar *%active-restarts* nil)"

    ;; ── FR-201: handler-bind dynamic handler registry ────────────────────
    "(defvar *%condition-handlers* nil)"

    ;; signal is defined as a macro in macros-stdlib.lisp for correct symbol resolution

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
    "(defclass standard-class (standard-object) ())"
    "(defclass structure-object () ())"
    "(defclass method-combination (standard-object)
       ((name :initarg :name :reader method-combination-name)))"
    "(defclass funcallable-standard-class (standard-class) ())"

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

       ))
