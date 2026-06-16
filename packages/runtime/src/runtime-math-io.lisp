;;;; packages/runtime/src/runtime-math-io.lisp - CL-CC Runtime: Symbols, Hash Tables, Conditions, Misc
;;;
;;; Contains: rt-symbol-*, rt-intern, rt-make-hash-table, rt-gethash/sethash/remhash/clrhash/maphash,
;;;            rt-hash-count/test/size/rehash-size/rehash-threshold,
;;; rt-signal-error, rt-signal, rt-warn-fn, rt-cerror, rt-boundp/fboundp,
;;; rt-random, rt-coerce, rt-read-from-string, rt-read-sexp.
;;;
;;; Strings/characters are in runtime-strings.lisp; CLOS/generic dispatch in runtime-clos.lisp.
;;; Depends on runtime.lisp. Load order: after runtime-ops.lisp.

(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Symbols
;;; ------------------------------------------------------------

(defun rt-make-symbol (name) (make-symbol name))
(defvar *rt-global-var-registry* (make-hash-table :test #'eq)
  "Runtime global variable registry used instead of host symbol-value cells.")

(defstruct (rt-special-variable-metadata (:constructor %make-rt-special-variable-metadata))
  "Metadata for runtime special variables.

GLOBAL-ONLY-P remains true until the symbol is dynamically bound.  GC scanners
can skip global-only variables when walking binding stacks because their value is
kept in *RT-GLOBAL-VAR-REGISTRY* rather than in thread-local dynamic frames."
  symbol
  (global-only-p t :type boolean))

(defvar *rt-special-variable-metadata* (make-hash-table :test #'eq)
  "Symbol -> RT-SPECIAL-VARIABLE-METADATA.")

(defvar *rt-dynamic-binding-stacks* (make-hash-table :test #'equal)
  "Logical thread id -> stack of runtime dynamic binding frames.")

(defparameter *rt-current-binding-thread-id* :main
  "Current logical thread id for runtime dynamic special bindings.")

(defun rt-register-special-variable (sym &key (global-only-p t))
  "Register SYM as a special variable and return its metadata object."
  (let ((metadata (or (gethash sym *rt-special-variable-metadata*)
                      (%make-rt-special-variable-metadata :symbol sym))))
    (setf (rt-special-variable-metadata-global-only-p metadata) global-only-p
          (gethash sym *rt-special-variable-metadata*) metadata)
    metadata))

(defun rt-special-variable-global-only-p (sym)
  "Return true when SYM has never been dynamically bound."
  (let ((metadata (gethash sym *rt-special-variable-metadata*)))
    (or (null metadata)
        (rt-special-variable-metadata-global-only-p metadata))))

(defun %rt-binding-symbol (binding)
  (cond
    ((and (consp binding) (getf binding :symbol)) (getf binding :symbol))
    ((consp binding) (car binding))
    (t nil)))

(defun %rt-binding-value (binding)
  (cond
    ((and (consp binding) (getf binding :value)) (getf binding :value))
    ((consp binding) (cdr binding))
    (t nil)))

(defun %rt-set-binding-value (binding value)
  (cond
    ((and (consp binding) (getf binding :value))
     (setf (getf binding :value) value))
    ((consp binding)
     (setf (cdr binding) value)))
  binding)

(defun %rt-current-binding-stack (&optional (thread-id *rt-current-binding-thread-id*))
  (gethash thread-id *rt-dynamic-binding-stacks*))

(defun rt-dynamic-bind (sym value &optional (thread-id *rt-current-binding-thread-id*))
  "Push a dynamic binding for special variable SYM on THREAD-ID's binding stack."
  (let ((metadata (rt-register-special-variable sym :global-only-p nil)))
    (setf (rt-special-variable-metadata-global-only-p metadata) nil)
    (push (cons sym value) (gethash thread-id *rt-dynamic-binding-stacks*))
    value))

(defun rt-dynamic-unbind (&optional (thread-id *rt-current-binding-thread-id*))
  "Pop the most recent dynamic binding for THREAD-ID."
  (let ((stack (gethash thread-id *rt-dynamic-binding-stacks*)))
    (when stack
      (prog1 (pop stack)
        (setf (gethash thread-id *rt-dynamic-binding-stacks*) stack)))))

(defmacro rt-with-dynamic-binding ((sym value &optional (thread-id '*rt-current-binding-thread-id*)) &body body)
  "Execute BODY with SYM dynamically bound to VALUE for THREAD-ID."
  (let ((tid (gensym "THREAD-ID")))
    `(let ((,tid ,thread-id))
       (rt-dynamic-bind ,sym ,value ,tid)
       (unwind-protect
            (progn ,@body)
         (rt-dynamic-unbind ,tid)))))

(defun %rt-dynamic-binding-cell (sym &optional (thread-id *rt-current-binding-thread-id*))
  (find sym (%rt-current-binding-stack thread-id)
        :key #'%rt-binding-symbol
        :test #'eq))

(defun rt-symbol-value (sym)
  (let ((binding (%rt-dynamic-binding-cell sym)))
    (if binding
        (%rt-binding-value binding)
        (multiple-value-bind (value present-p) (gethash sym *rt-global-var-registry*)
          (if present-p
              value
              (error "Unbound runtime variable: ~S" sym))))))

(defun rt-set-symbol-value (sym val)
  (let ((binding (%rt-dynamic-binding-cell sym)))
    (if binding
        (%rt-set-binding-value binding val)
        (progn
          (rt-register-special-variable sym :global-only-p t)
          (setf (gethash sym *rt-global-var-registry*) val))))
  val)
(defun rt-symbol-plist (sym) (symbol-plist sym))
(defun rt-get-prop (sym indicator) (get sym indicator))
(defun rt-put-prop (sym indicator val) (setf (get sym indicator) val))
(defun rt-remprop (sym indicator) (remprop sym indicator))

;;; ------------------------------------------------------------
;;; Pure scalar/sequence helpers used by the VM bridge
;;; ------------------------------------------------------------

(defun rt-1+ (x) (1+ x))
(defun rt-1- (x) (1- x))
(defun rt-+ (&rest xs) (apply #'+ xs))
(defun rt-- (&rest xs) (apply #'- xs))
(defun rt-* (&rest xs) (apply #'* xs))
(defun rt-/ (&rest xs) (apply #'/ xs))
(defun rt-< (&rest xs) (apply #'< xs))
(defun rt-> (&rest xs) (apply #'> xs))
(defun rt-<= (&rest xs) (apply #'<= xs))
(defun rt->= (&rest xs) (apply #'>= xs))
(defun rt-max (&rest xs) (apply #'max xs))
(defun rt-min (&rest xs) (apply #'min xs))
(defun rt-length (x) (length x))
(defun rt-char= (&rest xs) (apply #'char= xs))
(defun rt-char-equal (&rest xs) (apply #'char-equal xs))
(defun rt-equalp (a b) (equalp a b))
(defun rt-elt (sequence index) (elt sequence index))
(defun rt-append (&rest lists) (apply #'append lists))

;;; ------------------------------------------------------------
;;; Hash Tables
;;; ------------------------------------------------------------

(defparameter +rt-hash-table-weakness-modes+
  '(nil :key :value :key-and-value :key-or-value)
  "Supported runtime hash-table weakness modes.")

(defstruct (rt-weak-hash-table (:constructor %make-rt-weak-hash-table))
  "Runtime hash-table wrapper that records the requested weakness mode."
  table
  weakness
  entries)

(defun %rt-valid-hash-weakness-p (weakness)
  (member weakness +rt-hash-table-weakness-modes+ :test #'eq))

(defun %rt-make-backing-hash-table (test size rehash-size rehash-threshold weakness)
  "Create a strong host hash table for RT hash-table wrappers.

Weakness is represented by RT-WEAK-HASH-TABLE metadata and GC cleanup rather
than by host weak tables, so the backing table is always strong."
  (declare (ignore weakness))
  (let ((args (list :test test)))
    (when size
      (setf args (append args (list :size size))))
    (when rehash-size
      (setf args (append args (list :rehash-size rehash-size))))
    (when rehash-threshold
      (setf args (append args (list :rehash-threshold rehash-threshold))))
    (apply #'cl:make-hash-table args)))

(defun %rt-hash-table-backing (ht)
  (etypecase ht
    (rt-weak-hash-table (rt-weak-hash-table-table ht))
    (hash-table ht)))

(defun rt-hash-table-weakness (ht)
  "Return HT's weakness mode, or NIL for ordinary strong hash tables."
  (etypecase ht
    (rt-weak-hash-table (rt-weak-hash-table-weakness ht))
    (hash-table nil)))

(defun rt-hash-table-p (x)
  (if (or (hash-table-p x) (rt-weak-hash-table-p x)) 1 0))

(defun rt-make-hash-table (&key (test #'eql) size rehash-size rehash-threshold weakness &allow-other-keys)
  "Create a runtime hash table with optional weak-key/value semantics."
  (unless (%rt-valid-hash-weakness-p weakness)
    (error "Unsupported hash-table weakness mode: ~S" weakness))
  (let ((table (%rt-make-backing-hash-table test size rehash-size rehash-threshold weakness)))
    (if weakness
        (let ((weak-table (%make-rt-weak-hash-table
                           :table table
                           :weakness weakness
                           :entries (cl:make-hash-table :test test
                                                        :rehash-size rehash-size
                                                        :rehash-threshold rehash-threshold))))
          (pushnew weak-table *rt-weak-hash-table-registry* :test #'eq)
          weak-table)
        table)))

(defun %rt-record-weak-hash-entry (ht key val)
  "Record weak-entry metadata and attach ephemerons for GC reference processing."
  (when (rt-weak-hash-table-p ht)
    (let* ((weakness (rt-weak-hash-table-weakness ht))
           (entry (make-rt-weak-hash-entry
                   :key key
                   :value val
                   :key-ephemeron (when (member weakness '(:key :key-and-value :key-or-value))
                                    (rt-make-ephemeron key val))
                   :value-ephemeron (when (member weakness '(:value :key-and-value :key-or-value))
                                      (rt-make-ephemeron val key)))))
      (setf (gethash key (rt-weak-hash-table-entries ht)) entry)
      entry)))

(defun %rt-sweep-weak-hash-table (ht)
  "Drop metadata for entries no longer present in a host weak backing table."
  (when (rt-weak-hash-table-p ht)
    (let ((backing (rt-weak-hash-table-table ht))
          (entries (rt-weak-hash-table-entries ht)))
      (maphash (lambda (key entry)
                 (declare (ignore entry))
                 (unless (nth-value 1 (gethash key backing))
                   (remhash key entries)))
               entries)))
  ht)

(defun rt-gethash (key ht)
  (when (rt-weak-hash-table-p ht) (%rt-sweep-weak-hash-table ht))
  (gethash key (%rt-hash-table-backing ht)))

(defun rt-sethash (key ht val)
  (setf (gethash key (%rt-hash-table-backing ht)) val)
  (%rt-record-weak-hash-entry ht key val)
  val)

(defun rt-remhash (key ht)
  (prog1 (remhash key (%rt-hash-table-backing ht))
    (when (rt-weak-hash-table-p ht)
      (remhash key (rt-weak-hash-table-entries ht)))))

(defun rt-clrhash (ht)
  (clrhash (%rt-hash-table-backing ht))
  (when (rt-weak-hash-table-p ht)
    (clrhash (rt-weak-hash-table-entries ht)))
  ht)

(defun rt-hash-count (ht)
  (when (rt-weak-hash-table-p ht) (%rt-sweep-weak-hash-table ht))
  (hash-table-count (%rt-hash-table-backing ht)))

(defun rt-hash-size (ht)
  (when (rt-weak-hash-table-p ht) (%rt-sweep-weak-hash-table ht))
  (hash-table-size (%rt-hash-table-backing ht)))

(defun rt-hash-rehash-size (ht)
  (when (rt-weak-hash-table-p ht) (%rt-sweep-weak-hash-table ht))
  (hash-table-rehash-size (%rt-hash-table-backing ht)))

(defun rt-hash-rehash-threshold (ht)
  (when (rt-weak-hash-table-p ht) (%rt-sweep-weak-hash-table ht))
  (hash-table-rehash-threshold (%rt-hash-table-backing ht)))

(defun rt-hash-test (ht) (hash-table-test (%rt-hash-table-backing ht)))

(defun rt-maphash (fn ht)
  (when (rt-weak-hash-table-p ht) (%rt-sweep-weak-hash-table ht))
  (maphash fn (%rt-hash-table-backing ht)))
(defun rt-hash-keys (ht)
  (let (keys) (rt-maphash (lambda (k v) (declare (ignore v)) (push k keys)) ht) keys))
(defun rt-hash-values (ht)
  (let (vals) (rt-maphash (lambda (k v) (declare (ignore k)) (push v vals)) ht) vals))

;;; ------------------------------------------------------------
;;; Conditions / Error Handling
;;; ------------------------------------------------------------

(defun %rt-current-signal-heap ()
  (and (boundp '*rt-current-gc-heap*)
       *rt-current-gc-heap*))

(defmacro %rt-with-signal-gc-inhibit (&body body)
  "Run BODY while runtime signal handling temporarily inhibits GC when possible."
  (let ((heap (gensym "HEAP"))
        (old (gensym "OLD-INHIBIT")))
    `(let* ((,heap (%rt-current-signal-heap))
            (,old (and ,heap (rt-heap-gc-inhibit ,heap))))
       (unwind-protect
            (progn
              (when (and ,heap (fboundp 'rt-gc-signal-handler-enter))
                (rt-gc-signal-handler-enter ,heap))
              ,@body)
         (when (and ,heap (fboundp 'rt-gc-signal-handler-leave))
           (rt-gc-signal-handler-leave ,heap ,old))))))

(defun rt-signal-error (condition)
  (%rt-with-signal-gc-inhibit
    (multiple-value-bind (result handled-p) (rt-dispatch-signal condition)
      (if handled-p
          result
          (error condition)))))

(defun rt-signal (condition)
  (%rt-with-signal-gc-inhibit
    (multiple-value-bind (result handled-p) (rt-dispatch-signal condition)
      (if handled-p
          result
          (signal condition)))))

(defun rt-warn-fn (condition)
  (%rt-with-signal-gc-inhibit
    (multiple-value-bind (result handled-p) (rt-dispatch-signal condition)
      (if handled-p
          result
          (warn "~A" condition)))))

(defun rt-cerror (continue-string condition)
  (%rt-with-signal-gc-inhibit
    (rt-establish-restart 'continue (lambda () nil)
      (lambda ()
        (multiple-value-bind (result handled-p) (rt-dispatch-signal condition)
          (if handled-p
              result
              (cerror continue-string "~A" condition)))))))

(defun rt-invoke-restart (name &rest args)
  (multiple-value-bind (result handled-p) (rt-dispatch-restart name args)
    (if handled-p
        result
        (apply #'invoke-restart name args))))

;;; ------------------------------------------------------------
;;; Misc
;;; ------------------------------------------------------------

(defun rt-boundp (sym)
  (if (nth-value 1 (gethash sym *rt-global-var-registry*)) 1 0))

(defun rt-makunbound (sym)
  (remhash sym *rt-global-var-registry*)
  sym)
(defun rt-random (n) (random n))
(defun rt-make-random-state (&optional state)
  (if state (make-random-state state) (make-random-state)))
(defun rt-get-universal-time () (get-universal-time))
(defun rt-get-internal-real-time () (get-internal-real-time))
(defun rt-get-internal-run-time () (get-internal-run-time))
(defun rt-read-from-string (s) (read-from-string s))
(defun rt-read-sexp (stream) (read stream))
(defun rt-coerce (x type) (coerce x type))
