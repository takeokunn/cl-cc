(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Dispatch Protocol and Call-frame Helpers
;;;
;;; Contains: execution-context state for synchronous sub-invocations,
;;; vm-resolve-function, vm-save/restore-registers, vm-push-call-frame,
;;; vm-bind-closure-args, vm-list-to-lisp-list, generic-function dispatch helpers
;;; (vm-classify-arg, vm-resolve-gf-method, vm-resolve-multi-dispatch,
;;; vm-try-dispatch-combinations, vm-try-dispatch-sub), method-combination
;;; helpers (vm-get-all-applicable-methods, %lookup-qualified-methods,
;;; %collect-combo-methods, %vm-dispatch-custom-combination,
;;; vm-dispatch-generic-call), and qualified return handling
;;; (%vm-ret-qualified-dispatch).
;;;
;;; Load order: after vm.lisp, before vm-execute.lisp and vm-clos.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Execution context for sub-invocations (custom method combination) ────

(defvar *vm-exec-flat* nil
  "When non-nil, the flat instruction vector of the currently executing VM program.
Used by custom method combination to do synchronous sub-calls.")

(defvar *vm-exec-labels* nil
  "When non-nil, the label table of the currently executing VM program.")

(defun %vm-call-closure-sync (closure state args)
  "Call a VM closure synchronously, returning its result value.
Requires *vm-exec-flat* and *vm-exec-labels* to be bound.
Saves and restores call stack around the sub-invocation."
  (unless (and *vm-exec-flat* *vm-exec-labels*)
    (error "No VM execution context for synchronous sub-call"))
  (let* ((result-reg (intern "R0" :keyword))
         (saved-stack-depth (length (vm-call-stack state)))
         (entry-pc (gethash (vm-closure-entry-label closure) *vm-exec-labels*)))
    (unless entry-pc
      (error "Cannot resolve entry label ~A" (vm-closure-entry-label closure)))
    ;; Push a call frame; return-pc is irrelevant since we detect return by stack depth
    (vm-push-call-frame state 0 result-reg)
    (push nil (vm-method-call-stack state))
    (vm-bind-closure-args closure state args)
    ;; Mini execution loop — run until our frame is popped
    (loop with pc = entry-pc
          do (when (or (null pc) (>= pc (length *vm-exec-flat*)))
               (return (vm-reg-get state result-reg)))
             (multiple-value-bind (next-pc halt-p result)
                 (execute-instruction (aref *vm-exec-flat* pc) state pc *vm-exec-labels*)
               (when halt-p (return (or result (vm-reg-get state result-reg))))
               (when (<= (length (vm-call-stack state)) saved-stack-depth)
                 ;; Our frame was popped — method returned
                 (return (vm-reg-get state result-reg)))
               (setf pc next-pc)))))

(defun vm-resolve-function (state value)
  "Resolve VALUE to a closure, generic function, or host bridge function.
If VALUE is already a closure, return it.
If VALUE is a hash table with :__methods__, return it (generic function).
If VALUE is a symbol, look it up in the function registry first, then
check the host bridge whitelist."
  (cond
    ((typep value 'vm-closure-object) value)
    ((vm-generic-function-p value) value)
    ((functionp value) value)
    ((symbolp value)
     (let ((entry (gethash value (vm-function-registry state))))
       (cond
         (entry entry)
         ;; Only bridge whitelisted host functions
         ((and (gethash value *vm-host-bridge-functions*)
               (fboundp value))
          (symbol-function value))
         (t (error "Undefined function: ~S" value)))))
    (t (error "Invalid function designator: ~S" value))))

;;; ── Call-frame helpers ───────────────────────────────────────────────────

(defun vm-save-registers (state)
  "Return a snapshot copy of the current register file."
  (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
    (maphash (lambda (k v) (setf (gethash k copy) v))
             (vm-state-registers state))
    copy))

(defun vm-restore-registers (state saved-regs)
  "Replace the current register file with the SAVED-REGS snapshot."
  (when saved-regs
    (clrhash (vm-state-registers state))
    (maphash (lambda (k v) (setf (gethash k (vm-state-registers state)) v))
             saved-regs)))

(defun vm-push-call-frame (state return-pc dst-reg)
  "Save current environment and push a call frame onto the call stack."
  (push (list return-pc dst-reg (vm-closure-env state) (vm-save-registers state))
        (vm-call-stack state)))

(defun vm-bind-closure-args (closure state arg-values)
  "Bind ARG-VALUES to CLOSURE's parameter registers in STATE.
Restores captured environment, then handles required, &optional, &rest, and &key."
  (let ((params     (vm-closure-params closure))
        (opt-params (vm-closure-optional-params closure))
        (rest-param (vm-closure-rest-param closure))
        (key-params (vm-closure-key-params closure))
        (captured   (vm-closure-captured-values closure)))
    ;; Restore captured environment into registers
    (map nil (lambda (binding)
               (vm-reg-set state (car binding) (cdr binding)))
         captured)
    ;; Required parameters
    (loop for param in params
          for val   in arg-values
          do (vm-reg-set state param val))
    ;; &optional parameters
    (let* ((n-req      (length params))
           (n-opt      (length opt-params))
           (after-req  (nthcdr n-req arg-values)))
      (when opt-params
        (loop for (reg default) in opt-params
              for i from 0
              do (vm-reg-set state reg
                             (if (< i (length after-req))
                                 (nth i after-req)
                                 default))))
       ;; &rest parameter
        (when rest-param
          (vm-reg-set state rest-param
                      (vm-build-list state (nthcdr (+ n-req n-opt) arg-values)
                                     :stack-allocate-p (vm-closure-rest-stack-alloc-p closure))))
       ;; &key parameters
       (when key-params
         (let ((kw-args (nthcdr (+ n-req n-opt) arg-values)))
           (if (>= (length key-params) 4)
               (let ((kw-table (make-hash-table :test #'eq)))
                (loop for tail on kw-args by #'cddr
                      while (consp (cdr tail))
                      do (let ((keyword (first tail))
                               (value (second tail)))
                           (multiple-value-bind (existing foundp)
                               (gethash keyword kw-table)
                             (declare (ignore existing))
                             (unless foundp
                               (setf (gethash keyword kw-table) value)))))
                (dolist (entry key-params)
                  (destructuring-bind (keyword reg default) entry
                    (multiple-value-bind (value foundp)
                        (gethash keyword kw-table)
                      (vm-reg-set state reg (if foundp value default))))))
               (loop for (keyword reg default) in key-params
                     do (let ((pos (position keyword kw-args)))
                          (vm-reg-set state reg
                                      (if pos (nth (1+ pos) kw-args) default))))))))
    ;; Activate closure environment for nested closures
    (when captured
      (setf (vm-closure-env state) captured))))

(defun vm-list-to-lisp-list (state value)
  "Convert a VM list (possibly using vm-cons-cell heap objects) to a Lisp list."
  (cond
    ((null value) nil)
    ((consp value) value)
    ((typep value 'vm-cons-cell)
     (cons (vm-cons-cell-car value)
           (vm-list-to-lisp-list state (vm-cons-cell-cdr value))))
    ((integerp value)
     ;; Could be a heap address
     (let ((obj (vm-heap-get state value)))
       (if (typep obj 'vm-cons-cell)
           (cons (vm-cons-cell-car obj)
                 (vm-list-to-lisp-list state (vm-cons-cell-cdr obj)))
           (list value))))
    (t (list value))))

;;; ── Generic dispatch helpers ──────────────────────────────────────────────

(defun vm-classify-arg (arg state)
  "Determine the class name of an argument for generic dispatch."
  (declare (ignore state))
  (if (hash-table-p arg)
      (let ((class-ht (gethash :__class__ arg)))
        (if class-ht
            (gethash :__name__ class-ht)
            t))
      (typecase arg
        (integer 'integer)
        (string 'string)
        (symbol 'symbol)
        (t t))))

(defun %eql-specializer-p (key)
  "Return T if KEY is an eql specializer form (eql value)."
  (and (consp key) (eq (car key) 'eql)))

(defun %eql-specializer-matches-p (spec-key arg)
  "Test if an eql specializer key matches ARG.
SPEC-KEY is (eql value) — matches if (eql arg value)."
  (and (%eql-specializer-p spec-key)
       (eql arg (second spec-key))))

(defun %vm-extract-eql-specializer-keys (specializer)
  "Return the eql specializer values embedded in SPECIALIZER.
Only single-argument eql specializers are indexed for fast lookup."
  (cond
    ((%eql-specializer-p specializer)
     (list (second specializer)))
    ((and (consp specializer)
          (= (length specializer) 1)
          (%eql-specializer-p (car specializer)))
     (list (second (car specializer))))
    (t nil)))

(defun %vm-gf-eql-methods (gf-ht first-arg)
  "Return fast-path eql-specializer methods for FIRST-ARG, if indexed."
  (let ((eql-index (and (hash-table-p gf-ht) (gethash :__eql-index__ gf-ht))))
    (when eql-index
      (let ((m (gethash first-arg eql-index)))
        (cond
          ((null m) nil)
          ((listp m) m)
          (t (list m)))))))

(defun vm-get-all-applicable-methods (gf-ht state all-args)
  "Return list of all applicable method closures for GF-HT and ALL-ARGS, most-specific first.
EQL specializers are checked first (most specific), then class-based dispatch via CPL."
  (let* ((methods-ht (gethash :__methods__ gf-ht))
         (first-arg (car all-args))
         (class-name (vm-classify-arg first-arg state))
         (class-ht (gethash class-name (vm-class-registry state)))
         (cpl (if class-ht
                  (let ((c (gethash :__cpl__ class-ht)))
                    (if (member t c) c (append c (list t))))
                  (list class-name t)))
         (result nil))
     ;; 1. Check eql specializers first (most specific)
     (dolist (method (%vm-gf-eql-methods gf-ht first-arg))
       (push method result))
     ;; Fallback: linear scan for backwards compatibility / non-indexed tables
     (unless result
       (maphash (lambda (key method)
                  (when (or (and (%eql-specializer-p key)
                                 (%eql-specializer-matches-p key first-arg))
                            (and (consp key) (not (%eql-specializer-p key))
                                 (= 1 (length key))
                                 (%eql-specializer-p (car key))
                                 (%eql-specializer-matches-p (car key) first-arg)))
                    (push method result)))
                methods-ht))
    ;; 2. Collect class-based methods in CPL order (most-specific first)
    (dolist (ancestor cpl)
      (let ((m (or (gethash (list ancestor) methods-ht)
                   (gethash ancestor methods-ht))))
        (when m (push m result))))
    ;; 3. Fallback: t-specializer (if not already collected)
    (let ((t-method (gethash t methods-ht)))
      (when (and t-method (not (member t-method result)))
        (push t-method result)))
    (nreverse result)))

(defun %lookup-qualified-methods (gf-ht qual-key state all-arg-values)
  "Look up qualified methods for QUAL-KEY (:__BEFORE__ or :__AFTER__) in GF-HT.
Returns a list of applicable closures for the given argument values."
  (let ((qual-ht (gethash qual-key gf-ht)))
    (when qual-ht
      (let ((first-arg (car all-arg-values))
            (class-name (vm-classify-arg (car all-arg-values) state))
            (result nil))
        (declare (ignore first-arg))
        ;; Collect matching methods: exact class match, then inheritance
        (let ((direct (gethash (list class-name) qual-ht)))
          (when direct
            (if (listp direct)
                (dolist (m direct) (push m result))
                (push direct result))))
        (let ((t-match (gethash (list t) qual-ht)))
          (when t-match
            (if (listp t-match)
                (dolist (m t-match) (push m result))
                (push t-match result))))
        ;; Also check single-key entries for backward compat
        (let ((direct-single (gethash class-name qual-ht)))
          (when direct-single
            (if (listp direct-single)
                (dolist (m direct-single) (push m result))
                (push direct-single result))))
        (let ((t-single (gethash t qual-ht)))
          (when t-single
            (if (listp t-single)
                (dolist (m t-single) (push m result))
                (push t-single result))))
        (nreverse result)))))

(defun %collect-combo-methods (gf-ht qual-key state first-arg)
  "Collect all applicable methods for custom combination by walking the CPL.
Returns methods most-specific-first, deduplicated."
  (let* ((qual-ht (gethash qual-key gf-ht))
         (class-name (vm-classify-arg first-arg state))
         (class-ht (gethash class-name (vm-class-registry state)))
         (cpl (if class-ht
                  (let ((c (gethash :__cpl__ class-ht)))
                    (if (member t c) c (append c (list t))))
                  (list class-name t)))
         (result nil)
         (seen nil))
    (when qual-ht
      ;; Walk CPL (deduplicated) and collect methods from each level
      (dolist (ancestor cpl)
        (unless (member ancestor seen)
          (push ancestor seen)
          ;; Check plain key
          (let ((m (gethash ancestor qual-ht)))
            (when m
              (if (listp m)
                  (dolist (method m)
                    (unless (member method result)
                      (push method result)))
                  (unless (member m result)
                    (push m result)))))
          ;; Also check list-wrapped keys for multi-dispatch compat
          (let ((m (gethash (list ancestor) qual-ht)))
            (when m
              (if (listp m)
                  (dolist (method m)
                    (unless (member method result)
                      (push method result)))
                  (unless (member m result)
                    (push m result))))))))
    (nreverse result)))

(defun %vm-dispatch-custom-combination (gf-ht state pc arg-regs dst-reg labels combination)
  "Dispatch a generic function with custom method combination.
COMBINATION is the combination name (e.g. +, LIST, APPEND).
Calls all applicable methods with the matching qualifier synchronously
and folds results using the combination's operator."
  (let* ((all-arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
         (qual-key (intern (format nil "__~A__" (string-upcase (string combination))) :keyword))
         (combo-methods (%collect-combo-methods gf-ht qual-key state (car all-arg-values)))
         ;; Also try primary methods as fallback (some users just define primary methods)
         (primary-methods (vm-get-all-applicable-methods gf-ht state all-arg-values))
         (methods (or combo-methods primary-methods))
         ;; Resolve the operator function
         (operator (cond
                     ((eq combination '+) #'+)
                     ((eq combination '*) #'* )
                     ((eq combination 'list) #'list)
                     ((eq combination 'append) #'append)
                     ((eq combination 'nconc) #'nconc)
                     ((eq combination 'and) (lambda (&rest args) (every #'identity args)))
                     ((eq combination 'or) (lambda (&rest args) (some #'identity args)))
                     ((eq combination 'progn) (lambda (&rest args) (car (last args))))
                     ((eq combination 'max) #'max)
                     ((eq combination 'min) #'min)
                     (t (error "Unknown method combination operator: ~S" combination)))))
    (if (null methods)
        (error "No applicable methods for ~S with combination ~S" gf-ht combination)
        (if *vm-exec-flat*
            ;; We have execution context — call each method synchronously
            (let ((results (mapcar (lambda (m)
                                     (%vm-call-closure-sync m state all-arg-values))
                                   methods)))
              (vm-reg-set state dst-reg (apply operator results))
              (values (1+ pc) nil nil))
            ;; No execution context (shouldn't happen in practice) — call first method only
            (let ((method-closure (car methods)))
              (vm-push-call-frame state (1+ pc) dst-reg)
              (push (list gf-ht methods all-arg-values) (vm-method-call-stack state))
              (vm-bind-closure-args method-closure state all-arg-values)
              (values (gethash (vm-closure-entry-label method-closure) labels) nil nil))))))

(defun vm-dispatch-generic-call (gf-ht state pc arg-regs dst-reg labels)
  "Dispatch a generic function call. GF-HT is the generic function dispatch table.
Supports multiple dispatch by passing all argument values for composite key lookup.
Handles standard method combination: :around → :before → primary → :after.
Custom method combination: calls all qualified methods and folds with operator.
Returns (values next-pc halt-p result) like execute-instruction."
  ;; Check for custom method combination
  (let ((combination (gethash :__method-combination__ gf-ht)))
    (when (and combination (not (eq combination 'standard)))
      (return-from vm-dispatch-generic-call
        (%vm-dispatch-custom-combination gf-ht state pc arg-regs dst-reg labels combination))))
  (let* ((all-arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
         (all-methods    (vm-get-all-applicable-methods gf-ht state all-arg-values))
         (method-closure (or (car all-methods)
                             (vm-resolve-gf-method gf-ht state
                                                   (car all-arg-values) all-arg-values)))
         ;; Look up qualified methods
         (before-methods (%lookup-qualified-methods gf-ht :__BEFORE__ state all-arg-values))
         (after-methods  (%lookup-qualified-methods gf-ht :__AFTER__ state all-arg-values))
         (around-methods (%lookup-qualified-methods gf-ht :__AROUND__ state all-arg-values)))
    (if (and (null before-methods) (null after-methods) (null around-methods))
        ;; Fast path: no qualified methods — original behavior
        (progn
          (vm-push-call-frame state (1+ pc) dst-reg)
          (push (list gf-ht all-methods all-arg-values) (vm-method-call-stack state))
          (vm-bind-closure-args method-closure state all-arg-values)
          (values (gethash (vm-closure-entry-label method-closure) labels) nil nil))
        ;; Standard method combination with around/before/after
        (let* ((has-around (not (null around-methods)))
               (has-before (not (null before-methods)))
               ;; If around exists, call it first; otherwise start with before or primary
               (first-method (cond (has-around (car around-methods))
                                   (has-before (car before-methods))
                                   (t method-closure))))
          (vm-push-call-frame state (1+ pc) dst-reg)
          (push (list gf-ht all-methods all-arg-values
                      :qualified t
                      ;; Around method tracking
                      :around-pending (if has-around (cdr around-methods) nil)
                      ;; Before/after tracking (deferred until around calls next-method)
                      :before-pending (if (and (not has-around) has-before)
                                          (cdr before-methods) nil)
                      :primary (cond (has-around method-closure)
                                     (has-before method-closure)
                                     (t nil))
                      :after-pending after-methods
                      :arg-values all-arg-values
                      :before-methods before-methods
                      ;; Phase tracking
                      :phase (cond (has-around :around)
                                   (has-before :before)
                                   (t :primary)))
                (vm-method-call-stack state))
          (vm-bind-closure-args first-method state all-arg-values)
          (values (gethash (vm-closure-entry-label first-method) labels) nil nil)))))

(defun %vm-dispatch-call (func state pc labels arg-regs dst-reg tail-p)
  "Shared call dispatch for vm-call and vm-tail-call.
   TAIL-P suppresses frame push for TCO: the current frame's return address
   is reused, keeping the call stack O(1) for tail-recursive functions."
  (cond
    ;; Generic function: delegate to multi-dispatch resolver (always non-tail for safety)
    ((vm-generic-function-p func)
     (vm-dispatch-generic-call func state pc arg-regs dst-reg labels))
    ;; Host CL function (whitelist bridge) — apply directly, no frame ops needed
    ((functionp func)
     (vm-reg-set state dst-reg
                 (apply func (mapcar (lambda (r) (vm-reg-get state r)) arg-regs)))
     (values (1+ pc) nil nil))
    ;; Normal closure — optionally push frame (skipped for TCO), bind args, jump
    (t
     (let ((arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs)))
       (unless tail-p
         (vm-push-call-frame state (1+ pc) dst-reg)
         (push nil (vm-method-call-stack state)))
       (vm-bind-closure-args func state arg-values)
       (values (gethash (vm-closure-entry-label func) labels) nil nil)))))

(defun %vm-ret-qualified-dispatch (state result labels method-entry)
  "Handle return from a qualified method. Supports standard method combination:
:around → :before → primary → :after. Around methods wrap everything; their return
value is the final result. call-next-method from around triggers before/primary/after."
  (let ((props (cdddr method-entry)))
    (let ((before-pending (getf props :before-pending))
          (primary        (getf props :primary))
          (after-pending  (getf props :after-pending))
          (arg-values     (getf props :arg-values))
          (phase          (getf props :phase)))
      (cond
        ;; Around phase: around method returned without call-next-method
        ;; (if it called call-next-method, phase was already changed to :before/:primary)
        ((eq phase :around)
         ;; Around's return value is the final result
         (pop (vm-method-call-stack state))
         (if (vm-call-stack state)
             (destructuring-bind (return-pc dst-reg old-closure-env saved-regs)
                 (pop (vm-call-stack state))
               (vm-restore-registers state saved-regs)
               (vm-reg-set state dst-reg result)
               (when old-closure-env
                 (setf (vm-closure-env state) old-closure-env))
               (values return-pc nil nil))
             (values nil t result)))
        ;; More :before methods pending
        (before-pending
         (let ((next-before (car before-pending)))
           (setf (getf (cdddr method-entry) :before-pending) (cdr before-pending))
           (vm-bind-closure-args next-before state arg-values)
           (values (gethash (vm-closure-entry-label next-before) labels) nil nil)))
        ;; All befores done, call primary (only if we haven't called it yet)
        ((and primary (eq phase :before))
         (setf (getf (cdddr method-entry) :primary) nil)
         (setf (getf (cdddr method-entry) :phase) :primary)
         (vm-bind-closure-args primary state arg-values)
         (values (gethash (vm-closure-entry-label primary) labels) nil nil))
        ;; Primary done, call :after methods
        (after-pending
         (let ((next-after (car after-pending)))
           (setf (getf (cdddr method-entry) :after-pending) (cdr after-pending))
           ;; Save the primary result before calling after methods
           (when (eq phase :primary)
             (setf (getf (cdddr method-entry) :primary-result) result)
             (setf (getf (cdddr method-entry) :phase) :after))
           (vm-bind-closure-args next-after state arg-values)
           (values (gethash (vm-closure-entry-label next-after) labels) nil nil)))
        ;; All before/primary/after done
        (t
         (let ((primary-result (if (eq phase :primary)
                                   result
                                   (or (getf props :primary-result) result))))
           ;; Check if returning to an around method's call-next-method
           (if (getf props :around-return-phase)
               ;; Return to around method: set phase back, give primary result
               (progn
                 (setf (getf (cdddr method-entry) :phase) :around)
                 (setf (getf (cdddr method-entry) :around-return-phase) nil)
                 (vm-reg-set state (getf props :around-cnm-dst) primary-result)
                 (values (getf props :around-return-pc) nil nil))
               ;; No around, normal return
               (progn
                 (pop (vm-method-call-stack state))
                 (if (vm-call-stack state)
                     (destructuring-bind (return-pc dst-reg old-closure-env saved-regs)
                         (pop (vm-call-stack state))
                       (vm-restore-registers state saved-regs)
                       (vm-reg-set state dst-reg primary-result)
                       (when old-closure-env
                         (setf (vm-closure-env state) old-closure-env))
                       (values return-pc nil nil))
                     (values nil t primary-result))))))))))

(defun vm-resolve-gf-method (gf-ht state first-arg &optional all-args)
  "Resolve the applicable method closure for generic function GF-HT.
Supports multiple dispatch: if ALL-ARGS is provided, builds a composite key
from all argument classes. Falls back to single dispatch on FIRST-ARG."
  (let* ((methods-ht (gethash :__methods__ gf-ht))
         ;; Check if methods table uses composite keys (lists) or single keys (symbols)
         (uses-composite-keys (block check
                                (maphash (lambda (k v)
                                           (declare (ignore v))
                                           (when (listp k)
                                             (return-from check t)))
                                         methods-ht)
                                nil)))
    (if (and uses-composite-keys all-args)
        ;; Multiple dispatch: build composite key from all args
        (let* ((arg-classes (mapcar (lambda (arg) (vm-classify-arg arg state))
                                   all-args))
               (method-closure
                 (or
                   ;; Exact match by class
                   (gethash arg-classes methods-ht)
                    ;; Try eql specializer match: use indexed first-arg lookup when possible,
                    ;; then fall back to a full scan for composite keys.
                    (or (car (%vm-gf-eql-methods gf-ht first-arg))
                        (block eql-scan
                          (maphash (lambda (key method)
                                     (when (and (listp key) (= (length key) (length all-args))
                                                (every (lambda (spec arg)
                                                         (or (eq spec t)
                                                             (eq spec (vm-classify-arg arg state))
                                                             (%eql-specializer-matches-p spec arg)))
                                                       key all-args))
                                       (return-from eql-scan method)))
                                   methods-ht)
                          nil))
                    ;; Try with inheritance on each position
                    (vm-resolve-multi-dispatch methods-ht state arg-classes)
                    ;; All-t fallback
                    (gethash (make-list (length arg-classes) :initial-element t) methods-ht))))
          (unless method-closure
            (error "No applicable method for generic function ~S on classes ~S"
                   (gethash :__name__ gf-ht) arg-classes))
          method-closure)
        ;; Single dispatch (backward compatible)
        (let* ((class-name (vm-classify-arg first-arg state))
               (method-closure
                 (or
                    ;; Try eql specializer match first via fast index
                    (or (car (%vm-gf-eql-methods gf-ht first-arg))
                        (block eql-single
                          (maphash (lambda (key method)
                                     (when (%eql-specializer-matches-p key first-arg)
                                       (return-from eql-single method)))
                                   methods-ht)
                          nil))
                   (gethash class-name methods-ht)
                   (let ((class-ht (gethash class-name (vm-class-registry state))))
                     (when class-ht
                       (let ((cpl (gethash :__cpl__ class-ht)))
                         (loop for ancestor in (cdr cpl)
                               for m = (gethash ancestor methods-ht)
                               when m return m))))
                   (gethash t methods-ht))))
          (unless method-closure
            (error "No applicable method for generic function ~S on class ~S"
                   (gethash :__name__ gf-ht) class-name))
          method-closure))))

(defun vm-resolve-multi-dispatch (methods-ht state arg-classes)
  "Try to find a method by substituting ancestor classes in each position.
Uses class precedence lists for inheritance-based fallback."
  ;; Build CPLs for each argument position, always ending with T
  (let ((cpls (mapcar (lambda (class-name)
                        (let* ((class-ht (gethash class-name (vm-class-registry state)))
                               (cpl (if class-ht
                                        (gethash :__cpl__ class-ht)
                                        (list class-name))))
                          ;; Ensure T is always at the end of the CPL
                          (if (member t cpl)
                              cpl
                              (append cpl (list t)))))
                      arg-classes)))
    ;; Try all combinations, preferring earlier positions
    (vm-try-dispatch-combinations methods-ht cpls (length arg-classes))))

(defun vm-try-dispatch-combinations (methods-ht cpls n)
  "Try dispatch key combinations from CPLs, most-specific first."
  (when (= n 0)
    (return-from vm-try-dispatch-combinations (gethash nil methods-ht)))
  ;; Simple strategy: try substituting each position one at a time
  (let ((first-cpl (car cpls))
        (rest-cpls (cdr cpls)))
    (dolist (class first-cpl)
      (if (= n 1)
          (let ((m (gethash (list class) methods-ht)))
            (when m (return-from vm-try-dispatch-combinations m)))
          ;; Recursively try remaining positions
          (let ((sub-result (vm-try-dispatch-sub methods-ht rest-cpls (list class))))
            (when sub-result
              (return-from vm-try-dispatch-combinations sub-result))))))
  nil)

(defun vm-try-dispatch-sub (methods-ht cpls prefix)
  "Recursive helper for multi-dispatch combination search."
  (if (null cpls)
      (gethash prefix methods-ht)
      (let ((first-cpl (car cpls))
            (rest-cpls (cdr cpls)))
        (dolist (class first-cpl)
          (let ((result (vm-try-dispatch-sub methods-ht rest-cpls (append prefix (list class)))))
            (when result
              (return-from vm-try-dispatch-sub result))))
        nil)))
