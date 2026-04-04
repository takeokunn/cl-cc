;;;; compile/stdlib-source.lisp — Standard Library Source String
;;;;
;;;; Defines *standard-library-source* as a Lisp string consumed by pipeline.lisp.
;;;; Kept in a separate file so the stdlib functions can be read and edited
;;;; without scrolling through the pipeline API code.
;;;;
;;;; Each defun is its own concatenation argument so editors can navigate
;;;; the boundaries between functions easily.

(in-package :cl-cc)

(defparameter *standard-library-source*
  (concatenate 'string
    "(defun mapcar (fn lst)
   (if (null lst) nil
     (cons (funcall fn (car lst))
           (mapcar fn (cdr lst)))))"

    "(defun mapc (fn lst)
   (if (null lst) nil
     (progn (funcall fn (car lst))
            (mapc fn (cdr lst))))
   lst)"

    "(defun mapcan (fn lst)
   (if (null lst) nil
     (nconc (funcall fn (car lst))
            (mapcan fn (cdr lst)))))"

    "(defun remove-if (pred lst)
   (if (null lst) nil
     (if (funcall pred (car lst))
         (remove-if pred (cdr lst))
         (cons (car lst) (remove-if pred (cdr lst))))))"

    "(defun remove-if-not (pred lst)
   (if (null lst) nil
     (if (funcall pred (car lst))
         (cons (car lst) (remove-if-not pred (cdr lst)))
         (remove-if-not pred (cdr lst)))))"

    "(defun find-if (pred lst)
   (if (null lst) nil
     (if (funcall pred (car lst))
         (car lst)
         (find-if pred (cdr lst)))))"

    "(defun every (pred lst)
   (if (null lst) t
     (if (funcall pred (car lst))
         (every pred (cdr lst))
         nil)))"

    "(defun some (pred lst)
   (if (null lst) nil
     (let ((r (funcall pred (car lst))))
       (if r r (some pred (cdr lst))))))"

    "(defun reduce-init (fn lst acc)
   (if (null lst) acc
     (reduce-init fn (cdr lst) (funcall fn acc (car lst)))))"

    "(defun reduce (fn lst &optional initial-value has-init)
   (if has-init
       (reduce-init fn lst initial-value)
       (if (null (cdr lst)) (car lst)
         (reduce-init fn (cdr (cdr lst))
                      (funcall fn (car lst) (car (cdr lst)))))))"

    "(defun count-if (pred lst)
   (if (null lst) 0
     (+ (if (funcall pred (car lst)) 1 0)
        (count-if pred (cdr lst)))))"

    "(defun position-if (pred lst)
   (labels ((pos-helper (pred lst idx)
              (if (null lst) nil
                (if (funcall pred (car lst)) idx
                  (pos-helper pred (cdr lst) (+ idx 1))))))
     (pos-helper pred lst 0)))"

    "(defun notevery (pred lst) (not (every pred lst)))"

    "(defun notany (pred lst) (not (some pred lst)))"

    "(defun member-eql (item lst)
   (if (null lst) nil
     (if (eql item (car lst)) lst
       (member-eql item (cdr lst)))))"

    "(defun set-difference (lst1 lst2)
   (if (null lst1) nil
     (if (member-eql (car lst1) lst2)
         (set-difference (cdr lst1) lst2)
         (cons (car lst1) (set-difference (cdr lst1) lst2)))))"

    "(defun union-lists (lst1 lst2)
   (if (null lst1) lst2
     (if (member-eql (car lst1) lst2)
         (union-lists (cdr lst1) lst2)
         (cons (car lst1) (union-lists (cdr lst1) lst2)))))"

    "(defun last-cons (lst)
   (if (null (cdr lst)) lst
     (last-cons (cdr lst))))"

    "(defun append-lists (lst1 lst2)
   (if (null lst1) lst2
     (cons (car lst1) (append-lists (cdr lst1) lst2))))"

    "(defun maphash-fn (fn ht)
   (dolist (k (hash-table-keys ht))
     (funcall fn k (gethash k ht)))
   nil)"

    "(defun getf (plist indicator &optional default)
   (if (null plist) default
     (if (eql (car plist) indicator) (car (cdr plist))
       (getf (cdr (cdr plist)) indicator default))))"

    "(defun intersection (lst1 lst2)
   (if (null lst1) nil
     (if (member-eql (car lst1) lst2)
         (cons (car lst1) (intersection (cdr lst1) lst2))
         (intersection (cdr lst1) lst2))))"

    "(defun remove (item lst)
   (remove-if (lambda (x) (eql x item)) lst))"

    "(defun find (item lst &key key test)
   (let ((test-fn (if test test (lambda (a b) (eql a b)))))
     (dolist (x lst nil)
       (let ((val (if key (funcall key x) x)))
         (when (funcall test-fn item val)
           (return x))))))"

    "(defun position (item lst &key key test)
   (let ((test-fn (if test test (lambda (a b) (eql a b))))
         (idx 0))
     (dolist (x lst nil)
       (let ((val (if key (funcall key x) x)))
         (when (funcall test-fn item val)
           (return idx)))
       (setq idx (+ idx 1)))))"

    "(defun assoc-if (pred alist)
   (if (null alist) nil
     (if (funcall pred (car (car alist))) (car alist)
       (assoc-if pred (cdr alist)))))"

    "(defun assoc (item alist &key test key test-not)
   (let ((test-fn (cond (test-not (complement test-not))
                        (test test)
                        (t #'eql)))
         (key-fn (if key key #'identity)))
     (if (null alist) nil
       (if (funcall test-fn item (funcall key-fn (car (car alist))))
           (car alist)
           (assoc item (cdr alist) :test test-fn :key key-fn)))))"

    "(defun rassoc (item alist)
   (if (null alist) nil
     (if (eql item (cdr (car alist))) (car alist)
       (rassoc item (cdr alist)))))"

    "(defun pairlis (keys values &optional alist)
   (if (null keys) alist
     (cons (cons (car keys) (car values))
           (pairlis (cdr keys) (cdr values) alist))))"

    ;; FR-630: runtime coerce dispatch for dynamic type arguments
    "(defun %coerce-runtime (value type)
   (cond
     ((or (eq type 'string) (eq type 'simple-string) (eq type 'base-string))
      (coerce-to-string value))
     ((eq type 'list) (coerce-to-list value))
     ((or (eq type 'vector) (eq type 'simple-vector)) (coerce-to-vector value))
     ((eq type 'character) (character value))
     ((or (eq type 'float) (eq type 'single-float) (eq type 'double-float))
      (float value))
     (t (error \"Cannot coerce to type\"))))"

    "(defun identity (x) x)"

    ;; FR-586: set (obsolete but ANSI) — dynamic symbol-value assignment
    "(defun set (sym val) (setf (symbol-value sym) val) val)"

    ;; FR-548: (setf fdefinition) / (setf symbol-function)
    "(defun set-fdefinition (fn name) (setf (symbol-function name) fn) fn)"

    ;; FR-552: (setf find-class) — register class in host find-class
    "(defun %set-find-class (name class) (setf (find-class name) class) class)"

    ;; ── FR-523〜528: MOP introspection ──────────────────────────────────────
    ;; Class hash tables store :__superclasses__, :__slots__, :__name__, etc.
    "(defun class-direct-superclasses (class)
  (when (hash-table-p class)
    (gethash :__superclasses__ class)))"

    "(defun class-direct-slots (class)
  (when (hash-table-p class)
    (%class-slot-definitions class)))"

    "(defun class-slots (class)
  (when (hash-table-p class)
    (%class-slot-definitions class)))"

    "(defun class-direct-default-initargs (class)
  (when (hash-table-p class)
    (gethash :__default-initargs__ class)))"

    "(defun generic-function-methods (gf)
  (let ((methods-ht (and (hash-table-p gf) (gethash :__methods__ gf))))
    (when methods-ht
      (hash-table-values methods-ht)))"

    "(defun generic-function-method-combination (gf)
  (if (and (hash-table-p gf) (gethash :__method-combination__ gf))
      (gethash :__method-combination__ gf)
      'standard))"

    "(defun slot-definition-name (slot)
  (cond
    ((symbolp slot) slot)
    ((hash-table-p slot) (gethash :name slot))
    (t nil)))"

    "(defun slot-definition-initform (slot)
  (if (hash-table-p slot)
      (gethash :initform slot)
      nil))"

    "(defun slot-definition-initargs (slot)
  (if (hash-table-p slot)
      (gethash :initargs slot)
      nil))"

    "(defun slot-definition-allocation (slot)
  (if (hash-table-p slot)
      (or (gethash :allocation slot) :instance)
      :instance))"

    "(defun class-precedence-list (class)
  (when (hash-table-p class)
    (let ((name (gethash :__name__ class)))
      (when name
        (let ((result (list name))
              (seen (list name)))
          (labels ((walk (supers)
                     (dolist (s supers)
                       (unless (member s seen)
                         (push s result)
                         (push s seen)
                         (let ((sht (find-class s)))
                           (when (hash-table-p sht)
                             (walk (gethash :__superclasses__ sht))))))))
            (walk (gethash :__superclasses__ class)))
          (nreverse result))))))"

    ;; FR-428: (setf macro-function) — store macro function
    "(defun %set-macro-function (name fn) (setf (macro-function name) fn) fn)"

    "(defun constantly (value) (lambda (&rest args) (declare (ignore args)) value))"

    "(defun complement (fn) (lambda (&rest args) (not (apply fn args))))"

    "(defun sort-impl (sequence predicate key)
   (if (null sequence) nil
     (let ((pivot (car sequence))
           (less nil)
           (greater nil))
       (dolist (x (cdr sequence))
         (let ((a (if key (funcall key x) x))
               (b (if key (funcall key pivot) pivot)))
           (if (funcall predicate a b)
               (push x less)
               (push x greater))))
       (append (sort-impl less predicate key)
               (cons pivot (sort-impl greater predicate key))))))"

    "(defun sort (sequence predicate &key key)
   (sort-impl sequence predicate key))"

    "(defun stable-sort (sequence predicate &key key)
   (sort-impl sequence predicate key))"

    "(defun remove-duplicates (lst)
   (let ((result nil))
     (dolist (x lst)
       (unless (member-eql x result)
         (push x result)))
     (nreverse result)))"

    ;; ── FR-551: ANSI CL constants ──────────────────────────────────────────
    "(defvar call-arguments-limit 50)"
    "(defvar lambda-parameters-limit 50)"
    "(defvar lambda-list-keywords
       '(&optional &rest &key &allow-other-keys &aux &body &whole &environment))"
    "(defvar multiple-values-limit 20)"

    ;; ── FR-538: constantp / special-operator-p ─────────────────────────────
    "(defun constantp (form &optional environment)
   (declare (ignore environment))
   (or (and (not (symbolp form)) (atom form))
       (eq form t)
       (eq form nil)
       (keywordp form)
       (and (consp form) (eq (car form) 'quote))))"

    "(defun special-operator-p (symbol)
   (if (member symbol '(block catch eval-when flet function go if labels
                         let let* load-time-value locally macrolet
                         multiple-value-call multiple-value-prog1
                         progn progv quote return-from setq
                         symbol-macrolet tagbody the throw
                         unwind-protect))
       t nil))"

    ;; ── FR-360: CDR mapping functions ──────────────────────────────────────
    "(defun maplist (fn lst)
   (if (null lst) nil
     (cons (funcall fn lst)
           (maplist fn (cdr lst)))))"

    "(defun mapl (fn lst)
   (let ((tail lst))
     (loop
       (when (null tail) (return lst))
       (funcall fn tail)
       (setq tail (cdr tail)))))"

    "(defun mapcon (fn lst)
   (if (null lst) nil
     (nconc (funcall fn lst)
            (mapcon fn (cdr lst)))))"

    ;; ── FR-548: fdefinition ────────────────────────────────────────────────
    "(defun fdefinition (name)
   (if (and (consp name) (eq (car name) 'setf))
       (symbol-function (car (cdr name)))
       (symbol-function name)))"

    ;; ── FR-582: equalp ────────────────────────────────────────────────────
    "(defun equalp (x y)
   (cond
     ((eql x y) t)
     ((and (characterp x) (characterp y))
      (char-equal x y))
     ((and (numberp x) (numberp y))
      (= x y))
     ((and (stringp x) (stringp y))
      (string-equal x y))
     ((and (consp x) (consp y))
      (and (equalp (car x) (car y))
           (equalp (cdr x) (cdr y))))
     ((and (vectorp x) (vectorp y) (not (stringp x)) (not (stringp y)))
      (and (= (length x) (length y))
           (let ((i 0) (len (length x)) (result t))
             (loop while (and result (< i len))
                   do (unless (equalp (aref x i) (aref y i))
                        (setq result nil))
                      (setq i (+ i 1)))
             result)))
     ((and (hash-table-p x) (hash-table-p y))
      (and (= (hash-table-count x) (hash-table-count y))
           (let ((result t))
             (maphash (lambda (k v)
                        (multiple-value-bind (v2 found) (gethash k y)
                          (unless (and found (equalp v v2))
                            (setq result nil))))
                      x)
             result)))
     (t nil)))"

    ;; ── FR-664: /= (not equal) ─────────────────────────────────────────────
    "(defun /= (a b) (not (= a b)))"

    ;; ── FR-529: Derived logical operations ─────────────────────────────────
    "(defun lognand (a b) (lognot (logand a b)))"
    "(defun lognor (a b) (lognot (logior a b)))"
    "(defun logandc1 (a b) (logand (lognot a) b))"
    "(defun logandc2 (a b) (logand a (lognot b)))"
    "(defun logorc1 (a b) (logior (lognot a) b))"
    "(defun logorc2 (a b) (logior a (lognot b)))"

    ;; ── FR-508: cis ────────────────────────────────────────────────────────
    "(defun cis (theta) (complex (cos theta) (sin theta)))"

    ;; ── FR-679: get-decoded-time ────────────────────────────────────────────
    "(defun get-decoded-time () (decode-universal-time (get-universal-time)))"

    ;; ── FR-560/FR-561: Numeric and float constants ─────────────────────────
    "(defvar most-positive-fixnum 4611686018427387903)"
    "(defvar most-negative-fixnum -4611686018427387904)"
    "(defvar internal-time-units-per-second 1000000)"
    "(defvar pi 3.141592653589793d0)"
    "(defvar double-float-epsilon 1.1102230246251568d-16)"
    "(defvar single-float-epsilon 1.1920929e-7)"
    "(defvar short-float-epsilon 1.1920929e-7)"
    "(defvar long-float-epsilon 1.1102230246251568d-16)"
    "(defvar most-positive-double-float 1.7976931348623157d+308)"
    "(defvar most-negative-double-float -1.7976931348623157d+308)"
    "(defvar most-positive-single-float 3.4028235e+38)"
    "(defvar most-negative-single-float -3.4028235e+38)"
    "(defvar most-positive-short-float 3.4028235e+38)"
    "(defvar most-negative-short-float -3.4028235e+38)"
    "(defvar most-positive-long-float 1.7976931348623157d+308)"
    "(defvar most-negative-long-float -1.7976931348623157d+308)"
    "(defvar least-positive-double-float 5.0d-324)"
    "(defvar least-negative-double-float -5.0d-324)"
    "(defvar least-positive-single-float 1.4e-45)"
    "(defvar least-negative-single-float -1.4e-45)"
    "(defvar least-positive-short-float 1.4e-45)"
    "(defvar least-negative-short-float -1.4e-45)"
    "(defvar least-positive-long-float 5.0d-324)"
    "(defvar least-negative-long-float -5.0d-324)"
    "(defvar least-positive-normalized-double-float 2.2250738585072014d-308)"
    "(defvar least-positive-normalized-single-float 1.17549435e-38)"
    "(defvar least-negative-normalized-double-float -2.2250738585072014d-308)"
    "(defvar least-negative-normalized-single-float -1.17549435e-38)"
    "(defvar array-dimension-limit 1073741823)"
    "(defvar array-total-size-limit 1073741823)"
    "(defvar array-rank-limit 8)"
    "(defvar char-code-limit 1114112)"

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

)
  "Standard library source defining higher-order functions and set operations.")
