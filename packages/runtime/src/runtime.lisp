;;;; packages/runtime/src/runtime.lisp - CL-CC Runtime Library
;;;
;;; This library provides the runtime support for x86-64 native code.
;;; Each function here corresponds to a cl_rt_* assembly symbol called
;;; by the generated machine code via CALL instructions.
;;;
;;; Tagged pointer layout (3-bit tag in bits[2:0]):
;;;   000 (0) = fixnum: integer value in bits[63:3]
;;;   001 (1) = cons: pointer to cons cell
;;;   010 (2) = symbol: pointer to symbol
;;;   011 (3) = function/closure: pointer to function
;;;   100 (4) = character: char-code in bits[63:3]
;;;   101 (5) = array/vector: pointer to array
;;;   110 (6) = string: pointer to string
;;;   111 (7) = other heap object

(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Tagged Pointer Constants
;;; ------------------------------------------------------------

(defconstant +tag-fixnum+     0)
(defconstant +rt-tag-cons+    1)
(defconstant +rt-tag-symbol+  2)
(defconstant +rt-tag-function+ 3)
(defconstant +tag-character+  4)
(defconstant +tag-array+      5)
(defconstant +rt-tag-string+  6)
(defconstant +tag-other+      7)

(defun rt-tag-fixnum (n)    (ash n 3))
(defun rt-untag-fixnum (x)  (ash x -3))
(defun rt-tag-bits (x)      (logand x 7))

;;; ------------------------------------------------------------
;;; Multiple Values Buffer
;;; ------------------------------------------------------------

(defvar *rt-values-buffer* nil
  "Thread-local buffer for multiple return values.")

(defun rt-values-clear ()
  (setf *rt-values-buffer* nil))

(defun rt-values-push (val)
  (setf *rt-values-buffer* (append *rt-values-buffer* (list val))))

(defun rt-values-count ()
  (length *rt-values-buffer*))

(defun rt-values-ref (idx)
  (nth idx *rt-values-buffer*))

(defun rt-values-to-list ()
  *rt-values-buffer*)

(defun rt-spread-values (val)
  "If val is a list (from multiple values), spread it; otherwise push single."
  (if (listp val)
      (dolist (v val) (rt-values-push v))
      (rt-values-push val))
  val)

(defun rt-ensure-values (val)
  "Ensure at least one value is in the buffer."
  (when (null *rt-values-buffer*)
    (rt-values-push val))
  val)

;;; ------------------------------------------------------------
;;; Closure Support
;;; ------------------------------------------------------------

(defstruct (rt-closure-obj (:conc-name rt-closure-))
  fn
  env)

(defun rt-make-closure (fn env-list)
  (make-rt-closure-obj :fn fn :env (coerce env-list 'vector)))

(defun rt-closure-ref (closure idx)
  (aref (rt-closure-env closure) idx))

(defun rt-call-fn (fn &rest args)
  (if (rt-closure-obj-p fn)
      (apply (rt-closure-fn fn) args)
      (apply fn args)))

(defun rt-apply-fn (fn args-list)
  (if (rt-closure-obj-p fn)
      (apply (rt-closure-fn fn) args-list)
      (apply fn args-list)))

(defvar *rt-method-context-stack* nil
  "Dynamic stack of native-runtime generic-function method contexts.

Each frame is a plist with at least :NEXT-THUNK and :ARGS.  RT-CALL-NEXT-METHOD
uses this stack to emulate the VM method-call stack for host-backed runtime
dispatch.")

(defun rt-call-next-method (&rest args)
  "Invoke the next applicable native-runtime method.

When ARGS is empty, reuse the current generic-function arguments.  Otherwise use
ARGS as the replacement argument list, matching Common Lisp CALL-NEXT-METHOD
calling convention at the native runtime layer."
  (let* ((ctx (first *rt-method-context-stack*))
         (next-thunk (and ctx (getf ctx :next-thunk))))
    (unless next-thunk
      (error "call-next-method: no next method"))
    (funcall next-thunk (if args args (getf ctx :args)))))

(defun rt-next-method-p ()
  "Return true when a next method is available in the current runtime context."
  (let ((ctx (first *rt-method-context-stack*)))
    (and ctx (getf ctx :next-thunk) t)))

(defun rt-register-function (name fn)
  (setf (symbol-function name) fn))

;;; ------------------------------------------------------------
;;; Runtime Condition / Restart Support (FR-300)
;;; ------------------------------------------------------------

(defstruct (rt-handler (:constructor make-rt-handler (condition-type handler-function)))
  "Runtime condition handler frame."
  condition-type
  handler-function)

(defstruct (rt-restart (:constructor make-rt-restart (name function)))
  "Runtime restart frame."
  name
  function)

(defvar *handler-stack* nil
  "Dynamic stack of runtime condition handlers.

The native backend pushes frames here before signaling conditions.  Each frame
contains a condition type and a handler function; the most recent matching
frame is invoked first.")

(defvar *restart-stack* nil
  "Dynamic stack of runtime restart bindings.

Each frame contains a restart name and a function to call when that restart is
invoked through RT-INVOKE-RESTART.")

(defun rt-push-handler (condition-type handler-function)
  "Push HANDLER-FUNCTION for CONDITION-TYPE onto the runtime handler stack."
  (check-type condition-type (or symbol cons class))
  (check-type handler-function (or function symbol rt-closure-obj))
  (push (make-rt-handler condition-type handler-function) *handler-stack*))

(defun rt-pop-handler ()
  "Pop and return the top runtime handler frame, or NIL when the stack is empty."
  (pop *handler-stack*))

(defun rt-establish-handler (condition-type handler-function thunk)
  "Run THUNK with HANDLER-FUNCTION established for CONDITION-TYPE.

This is the native-runtime analogue of a handler frame save point: the frame is
always removed when THUNK exits, including non-local exits and errors."
  (check-type thunk (or function symbol rt-closure-obj))
  (rt-push-handler condition-type handler-function)
  (unwind-protect
       (rt-call-fn thunk)
    (rt-pop-handler)))

(defun rt-find-handler (condition)
  "Return the most recent runtime handler matching CONDITION, or NIL."
  (find-if (lambda (handler)
             (typep condition (rt-handler-condition-type handler)))
           *handler-stack*))

(defun rt-dispatch-signal (condition)
  "Dispatch CONDITION to the runtime handler stack.

Returns two values: the handler's return value and true when a runtime handler
was found; NIL and NIL otherwise."
  (let ((handler (rt-find-handler condition)))
    (if handler
        (values (rt-call-fn (rt-handler-handler-function handler) condition) t)
        (values nil nil))))

(defun rt-push-restart (name function)
  "Push FUNCTION as the runtime restart named NAME."
  (check-type function (or function symbol rt-closure-obj))
  (push (make-rt-restart name function) *restart-stack*))

(defun rt-pop-restart ()
  "Pop and return the top runtime restart frame, or NIL when the stack is empty."
  (pop *restart-stack*))

(defun rt-find-restart (name)
  "Return the most recent runtime restart named NAME, or NIL."
  (find name *restart-stack* :key #'rt-restart-name :test #'eq))

(defun rt-establish-restart (name function thunk)
  "Run THUNK with FUNCTION established as runtime restart NAME."
  (check-type thunk (or function symbol rt-closure-obj))
  (rt-push-restart name function)
  (unwind-protect
       (rt-call-fn thunk)
    (rt-pop-restart)))

(defun rt-restart-bind (bindings thunk)
  "Run THUNK with runtime restart BINDINGS.

BINDINGS is a list of (NAME FUNCTION) entries.  Frames are removed in
unwind-protect style."
  (labels ((run-with-runtime-bindings (remaining)
             (if (endp remaining)
                 (rt-call-fn thunk)
                 (destructuring-bind (name function) (first remaining)
                   (rt-push-restart name function)
                   (unwind-protect
                        (run-with-runtime-bindings (rest remaining))
                     (rt-pop-restart))))))
    (run-with-runtime-bindings bindings)))

(defun rt-restart-case (thunk clauses)
  "Run THUNK with restart CLAUSES established.

CLAUSES is a list of (NAME FUNCTION) entries.  This function-level API is used
by generated native runtime calls; source-level RESTART-CASE still expands in
the compiler/VM layer."
  (rt-restart-bind clauses thunk))

(defun rt-dispatch-restart (name args)
  "Invoke runtime restart NAME with ARGS.

Returns two values: restart result and true when a runtime restart was found;
NIL and NIL otherwise."
  (let ((restart (rt-find-restart name)))
    (if restart
        (values (apply #'rt-call-fn (rt-restart-function restart) args) t)
        (values nil nil))))

;;; ------------------------------------------------------------
;;; JIT Code Cache Management (FR-379 / FR-437)
;;; ------------------------------------------------------------

(defstruct (rt-code-cache-entry (:conc-name rt-code-cache-entry-))
  function-entry
  code
  (size 1 :type integer)
  (warmth 0 :type integer)
  (last-used 0 :type integer)
  class-key
  reachable-p)

(defstruct (rt-code-cache (:constructor make-rt-code-cache
                              (&key (capacity 1024)
                                     (entries (make-hash-table :test #'equal))
                                     (lru-clock 0)
                                     (size 0)
                                     (eviction-threshold 0.9)
                                     (hits 0)
                                     (misses 0)
                                     (evictions 0))))
  (capacity 1024 :type integer)
  (entries (make-hash-table :test #'equal) :type hash-table)
  (lru-clock 0 :type integer)
  (size 0 :type integer)
  (eviction-threshold 0.9 :type real)
  (hits 0 :type integer)
  (misses 0 :type integer)
  (evictions 0 :type integer))

(defparameter *rt-code-cache* (make-rt-code-cache)
  "Global runtime JIT code cache.

Evicted compiled code is deliberately removed only from this cache.  Runtime
callers should fall back to bytecode interpretation or recompilation when a
function entry no longer has a compiled-code cache entry.")

(defun rt-code-cache-lookup (function-entry &optional (cache *rt-code-cache*))
  "Return compiled code for FUNCTION-ENTRY and refresh its LRU timestamp."
  (let ((entry (gethash function-entry (rt-code-cache-entries cache))))
    (if entry
        (progn
          (incf (rt-code-cache-hits cache))
          (incf (rt-code-cache-lru-clock cache))
          (incf (rt-code-cache-entry-warmth entry))
          (setf (rt-code-cache-entry-last-used entry) (rt-code-cache-lru-clock cache))
          (rt-code-cache-entry-code entry))
        (progn
          (incf (rt-code-cache-misses cache))
          nil))))

(defun rt-code-cache-evict (function-entry &optional (cache *rt-code-cache*))
  "Evict FUNCTION-ENTRY from CACHE and return the evicted entry, if present.

After eviction, callers are expected to fall back to interpretation until the
function is compiled again."
  (let ((entry (gethash function-entry (rt-code-cache-entries cache))))
    (when entry
      (remhash function-entry (rt-code-cache-entries cache))
      (decf (rt-code-cache-size cache) (rt-code-cache-entry-size entry))
      (incf (rt-code-cache-evictions cache))
      ;; Host Lisp owns ordinary code objects; dropping references is the free path.
      (setf (rt-code-cache-entry-code entry) nil
            (rt-code-cache-entry-reachable-p entry) nil)
      entry)))

(defun %rt-code-cache-coldest-entry (cache)
  (let ((oldest-key nil)
        (oldest-entry nil))
    (maphash (lambda (key entry)
                (when (or (null oldest-entry)
                          (< (rt-code-cache-entry-warmth entry)
                             (rt-code-cache-entry-warmth oldest-entry))
                          (and (= (rt-code-cache-entry-warmth entry)
                                  (rt-code-cache-entry-warmth oldest-entry))
                          (< (rt-code-cache-entry-last-used entry)
                                (rt-code-cache-entry-last-used oldest-entry))))
                  (setf oldest-key key
                        oldest-entry entry)))
              (rt-code-cache-entries cache))
    (values oldest-key oldest-entry)))

(defun %rt-code-cache-evict-until-room (cache requested-size)
  (let* ((capacity (rt-code-cache-capacity cache))
         (threshold-size (max requested-size
                              (floor (* capacity (rt-code-cache-eviction-threshold cache))))))
    (loop while (or (> (+ (rt-code-cache-size cache) requested-size) capacity)
                    (> (+ (rt-code-cache-size cache) requested-size) threshold-size))
          do (multiple-value-bind (key entry) (%rt-code-cache-coldest-entry cache)
               (declare (ignore entry))
               (unless key (return))
               (rt-code-cache-evict key cache)))))

(defun rt-code-cache-store (function-entry code &key (size 1) class-key
                                          (cache *rt-code-cache*))
  "Store CODE for FUNCTION-ENTRY, evicting least-recently-used entries as needed."
  (check-type size (integer 0 *))
  (let ((old-entry (gethash function-entry (rt-code-cache-entries cache))))
    (when old-entry
      (decf (rt-code-cache-size cache) (rt-code-cache-entry-size old-entry)))
    (%rt-code-cache-evict-until-room cache size)
    (incf (rt-code-cache-lru-clock cache))
    (let ((entry (make-rt-code-cache-entry
                  :function-entry function-entry
                   :code code
                   :size size
                   :warmth 1
                   :last-used (rt-code-cache-lru-clock cache)
                  :class-key class-key
                  :reachable-p t)))
      (setf (gethash function-entry (rt-code-cache-entries cache)) entry)
      (incf (rt-code-cache-size cache) size)
      entry)))

(defun rt-code-cache-stats (&optional (cache *rt-code-cache*))
  "Return a plist of code-cache occupancy, hit-rate, and eviction counters."
  (let* ((hits (rt-code-cache-hits cache))
         (misses (rt-code-cache-misses cache))
         (total (+ hits misses)))
    (list :size (rt-code-cache-size cache)
          :capacity (rt-code-cache-capacity cache)
          :threshold (rt-code-cache-eviction-threshold cache)
          :entries (hash-table-count (rt-code-cache-entries cache))
          :hits hits
          :misses misses
          :hit-rate (if (plusp total) (/ hits total) 0.0)
          :evictions (rt-code-cache-evictions cache))))

(defun rt-gc-unload-code (heap code-addr &optional (cache *rt-code-cache*))
  "Unload compiled code CODE-ADDR from the JIT cache.

HEAP is accepted for GC integration symmetry and currently not inspected.  The
evicted function will fall back to interpretation if called later."
  (declare (ignore heap))
  (or (rt-code-cache-evict code-addr cache)
      (let (keys removed)
        (maphash (lambda (key entry)
                   (when (eql (rt-code-cache-entry-code entry) code-addr)
                     (push key keys)))
                 (rt-code-cache-entries cache))
        (dolist (key keys)
          (push (rt-code-cache-evict key cache) removed))
        (nreverse removed))))

;;; ------------------------------------------------------------
;;; Global Bindings
;;; ------------------------------------------------------------

(defun rt-get-global (sym)
  (symbol-value sym))

(defun rt-set-global (sym val)
  (setf (symbol-value sym) val))

;;; ------------------------------------------------------------
;;; Type Predicates
;;; ------------------------------------------------------------

(defmacro define-rt-predicate (name predicate)
  "Define an rt-* unary predicate returning 1/0 based on PREDICATE applied to its argument."
  `(defun ,name (x) (if (,predicate x) 1 0)))

(defmacro define-rt-binary-predicate (name op)
  "Define an rt-* binary predicate returning 1/0 based on OP applied to (a b)."
  `(defun ,name (a b) (if (,op a b) 1 0)))

(define-rt-predicate rt-consp        consp)
(define-rt-predicate rt-null-p       null)
(define-rt-predicate rt-symbolp      symbolp)
(define-rt-predicate rt-numberp      numberp)
(define-rt-predicate rt-integerp     integerp)
(define-rt-predicate rt-floatp       floatp)
(define-rt-predicate rt-stringp      stringp)
(define-rt-predicate rt-characterp   characterp)
(define-rt-predicate rt-vectorp      vectorp)
(define-rt-predicate rt-listp        listp)
(define-rt-predicate rt-atomp        atom)
(define-rt-predicate rt-keywordp     keywordp)
(define-rt-predicate rt-hash-table-p hash-table-p)

;;; rt-functionp has a compound check — stays explicit
(defun rt-functionp (x)
  (if (or (functionp x) (rt-closure-obj-p x)) 1 0))

(defun rt-typep (x type-name)
  (if (typep x (find-symbol (string type-name) :cl)) 1 0))

(defun rt-type-of (x)
  (type-of x))

;;; ------------------------------------------------------------
;;; Cons / List Operations
;;; ------------------------------------------------------------

(defstruct (rt-cow-list (:constructor %make-rt-cow-list))
  "Runtime copy-on-write list wrapper used by rt-copy-list/rt-rplac* operations."
  (backing nil)
  (refcount 1 :type integer))

(defparameter *rt-cow-list-enabled* t
  "When true, rt-copy-list returns copy-on-write wrappers for list values.")

(defun %rt-cow-list-materialize (value)
  (if (rt-cow-list-p value)
      (rt-cow-list-backing value)
      value))

(defun %rt-cow-list-share (value)
  (if (rt-cow-list-p value)
      (progn
        (incf (rt-cow-list-refcount value))
        (%make-rt-cow-list :backing (rt-cow-list-backing value)
                           :refcount (rt-cow-list-refcount value)))
      ;; Plain lists are assumed shared with at least one external owner.
      ;; Start with refcount=2 so first write performs copy-on-write.
      (%make-rt-cow-list :backing value :refcount 2)))

(defun %rt-cow-list-ensure-writable (value)
  (if (rt-cow-list-p value)
      (progn
        (when (> (rt-cow-list-refcount value) 1)
          (decf (rt-cow-list-refcount value))
          (setf (rt-cow-list-backing value) (copy-list (rt-cow-list-backing value))
                (rt-cow-list-refcount value) 1))
        (rt-cow-list-backing value))
      value))

(defun rt-cons (car cdr) (cons car cdr))
(defun rt-car (x) (car (%rt-cow-list-materialize x)))
(defun rt-cdr (x) (cdr (%rt-cow-list-materialize x)))
(defun rt-rplaca (cons val)
  (rplaca (%rt-cow-list-ensure-writable cons) val)
  nil)
(defun rt-rplacd (cons val)
  (rplacd (%rt-cow-list-ensure-writable cons) val)
  nil)
(defun rt-make-list (n &optional (init nil)) (make-list n :initial-element init))
(defun rt-list-length (l) (length (%rt-cow-list-materialize l)))
(defun rt-nconc (a b) (nconc (%rt-cow-list-ensure-writable a)
                            (%rt-cow-list-materialize b)))
(defun rt-reverse (l) (reverse (%rt-cow-list-materialize l)))
(defun rt-nreverse (l) (nreverse (%rt-cow-list-ensure-writable l)))
(defun rt-member (x l) (member x (%rt-cow-list-materialize l)))
(defun rt-nth (n l) (nth n (%rt-cow-list-materialize l)))
(defun rt-nthcdr (n l) (nthcdr n (%rt-cow-list-materialize l)))
(defun rt-last (l) (last (%rt-cow-list-materialize l)))
(defun rt-butlast (l) (butlast (%rt-cow-list-materialize l)))
(defun rt-copy-list (l)
  (let ((materialized (%rt-cow-list-materialize l)))
    (if *rt-cow-list-enabled*
        (%rt-cow-list-share materialized)
        (copy-list materialized))))
(defun rt-copy-tree (l) (copy-tree l))
(defun rt-assoc (key alist) (assoc key alist))
(defun rt-acons (key val alist) (acons key val alist))
(defun rt-subst (new old tree) (subst new old tree))
(defun rt-first (l) (first l))
(defun rt-second (l) (second l))
(defun rt-third (l) (third l))
(defun rt-fourth (l) (fourth l))
(defun rt-fifth (l) (fifth l))
(defun rt-rest (l) (rest l))
(define-rt-predicate rt-endp endp)
(define-rt-predicate rt-null null)
(defun rt-push-list (val list-place) (cons val list-place))
(defun rt-pop-list (list-place)
  (let ((list-value (%rt-cow-list-materialize list-place)))
    (values (car list-value) (cdr list-value))))
(define-rt-binary-predicate rt-equal equal)
(defun rt-string-coerce (x) (string x))
(defun rt-coerce-to-string (x) (if (stringp x) x (format nil "~A" x)))
(defun rt-coerce-to-list (x) (coerce x 'list))
(defun rt-coerce-to-vector (x) (coerce x 'vector))

;; Arrays/Vectors, Arithmetic, Bitwise, Comparisons, and Math rt-* wrappers
;; are in runtime-ops.lisp (loaded next).
