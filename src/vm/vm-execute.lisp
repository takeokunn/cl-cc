(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — execute-instruction Methods for Core Instructions
;;;
;;; Contains: vm-resolve-function, vm-falsep, call-frame helpers
;;; (vm-save/restore-registers, vm-push-call-frame, vm-bind-closure-args),
;;; vm-list-to-lisp-list, multi-dispatch helpers (vm-classify-arg,
;;; vm-resolve-gf-method, vm-resolve-multi-dispatch, vm-get-all-applicable-methods,
;;; vm-dispatch-generic-call, %vm-dispatch-call),
;;; and execute-instruction methods for: const, move, add/sub/mul, label,
;;; jump, jump-zero, print, halt, closure, call, tail-call, ret, func-ref,
;;; make-closure, closure-ref-idx, values, mv-bind, values-to-list,
;;; spread-values, clear-values, ensure-values, next-method-p,
;;; call-next-method, apply, register-function, set-global, get-global.
;;;
;;; Load order: after vm.lisp, before vm-clos.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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

(defmethod execute-instruction ((inst vm-const) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (vm-value inst))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-move) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (vm-reg-get state (vm-src inst)))
  (values (1+ pc) nil nil))

(define-simple-instruction vm-add :binary +)
(define-simple-instruction vm-sub :binary -)
(define-simple-instruction vm-mul :binary *)

(defmethod execute-instruction ((inst vm-label) state pc labels)
  (declare (ignore state labels))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-jump) state pc labels)
  (declare (ignore state pc))
  (values (gethash (vm-label-name inst) labels) nil nil))

(defun vm-falsep (value)
  "Return T if VALUE is falsy (nil, 0, or false)."
  (or (null value) (eql value 0)))

(defmethod execute-instruction ((inst vm-jump-zero) state pc labels)
  (if (vm-falsep (vm-reg-get state (vm-reg inst)))
      (values (gethash (vm-label-name inst) labels) nil nil)
      (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-print) state pc labels)
  (declare (ignore labels))
  (format (vm-output-stream state) "~A~%" (vm-reg-get state (vm-reg inst)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-halt) state pc labels)
  (declare (ignore pc labels))
  (values nil t (vm-reg-get state (vm-reg inst))))

(defmethod execute-instruction ((inst vm-closure) state pc labels)
  (declare (ignore labels))
  ;; captured is a list of (symbol . register) pairs from the compiler
  ;; We store values keyed by REGISTER name so vm-call restores to the correct register
  (let* ((dst-reg (vm-dst inst))
         (captured-vals (mapcar (lambda (binding)
                                  (cons (cdr binding) (vm-reg-get state (cdr binding))))
                                (vm-captured-vars inst)))
         (closure (make-instance 'vm-closure-object
                                 :entry-label (vm-label-name inst)
                                 :params (vm-closure-params inst)
                                 :optional-params (vm-closure-optional-params inst)
                                 :rest-param (vm-closure-rest-param inst)
                                 :key-params (vm-closure-key-params inst)
                                 :captured-values captured-vals)))
    (vm-reg-set state dst-reg closure)
    ;; Fix self-references for recursive labels: if a captured register
    ;; is the same as dst-reg, the closure captures itself (not yet created at lookup time)
    (dolist (cv-pair captured-vals)
      (when (eql (car cv-pair) dst-reg)
        (setf (cdr cv-pair) closure)))
    (values (1+ pc) nil nil)))

(defun vm-get-all-applicable-methods (gf-ht state all-args)
  "Return list of all applicable method closures for GF-HT and ALL-ARGS, most-specific first."
  (let* ((methods-ht (gethash :__methods__ gf-ht))
         (first-arg (car all-args))
         (class-name (vm-classify-arg first-arg state))
         (class-ht (gethash class-name (vm-class-registry state)))
         (cpl (if class-ht
                  (let ((c (gethash :__cpl__ class-ht)))
                    (if (member t c) c (append c (list t))))
                  (list class-name t)))
         (result nil))
    ;; Collect methods in CPL order (most-specific first)
    (dolist (ancestor cpl)
      (let ((m (or (gethash (list ancestor) methods-ht)
                   (gethash ancestor methods-ht))))
        (when m (push m result))))
    ;; Fallback: t-specializer (if not already collected)
    (let ((t-method (gethash t methods-ht)))
      (when (and t-method (not (member t-method result)))
        (push t-method result)))
    (nreverse result)))

(defun vm-dispatch-generic-call (gf-ht state pc arg-regs dst-reg labels)
  "Dispatch a generic function call. GF-HT is the generic function dispatch table.
Supports multiple dispatch by passing all argument values for composite key lookup.
Returns (values next-pc halt-p result) like execute-instruction."
  (let* ((all-arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
         (all-methods    (vm-get-all-applicable-methods gf-ht state all-arg-values))
         (method-closure (or (car all-methods)
                             (vm-resolve-gf-method gf-ht state
                                                   (car all-arg-values) all-arg-values))))
    (vm-push-call-frame state (1+ pc) dst-reg)
    (push (list gf-ht all-methods all-arg-values) (vm-method-call-stack state))
    (vm-bind-closure-args method-closure state all-arg-values)
    (values (gethash (vm-closure-entry-label method-closure) labels) nil nil)))

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

(defmethod execute-instruction ((inst vm-call) state pc labels)
  (let ((func     (vm-resolve-function state (vm-reg-get state (vm-func-reg inst)))))
    (%vm-dispatch-call func state pc labels (vm-args inst) (vm-dst inst) nil)))

(defmethod execute-instruction ((inst vm-tail-call) state pc labels)
  (let ((func     (vm-resolve-function state (vm-reg-get state (vm-func-reg inst)))))
    (%vm-dispatch-call func state pc labels (vm-args inst) (vm-dst inst) t)))

(defmethod execute-instruction ((inst vm-ret) state pc labels)
  (declare (ignore pc labels))
  (let ((result (vm-reg-get state (vm-reg inst))))
    (if (vm-call-stack state)
        (destructuring-bind (return-pc dst-reg old-closure-env saved-regs)
            (pop (vm-call-stack state))
          ;; Pop method-call-stack in sync with call-stack
          (when (vm-method-call-stack state)
            (pop (vm-method-call-stack state)))
          (vm-restore-registers state saved-regs)
          ;; Write the return value into the destination register
          (vm-reg-set state dst-reg result)
          (when old-closure-env
            (setf (vm-closure-env state) old-closure-env))
          (values return-pc nil nil))
        (values nil t result))))

(defmethod execute-instruction ((inst vm-func-ref) state pc labels)
  (declare (ignore pc labels))
  ;; Try the function registry first — user-defined functions (defun) register
  ;; closures with proper entry-label, params, and captured-values.
  ;; Fall back to a bare closure for local labels in the same compilation unit.
  (let* ((label-str (vm-label-name inst))
         (sym (find-symbol label-str :cl-cc))
         (registered (when sym (gethash sym (vm-function-registry state)))))
    (vm-reg-set state (vm-dst inst)
                (or registered
                    (make-instance 'vm-closure-object
                                   :entry-label label-str
                                   :params nil
                                   :captured-values nil))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-make-closure) state pc labels)
  (declare (ignore labels))
  (let* ((captured-values (mapcar (lambda (reg) (vm-reg-get state reg)) (vm-env-regs inst)))
         (closure (make-instance 'vm-closure-object
                                 :entry-label (vm-label-name inst)
                                 :params (vm-make-closure-params inst)
                                 :captured-values captured-values))
         (addr (vm-heap-alloc state closure)))
    (vm-reg-set state (vm-dst inst) addr)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-closure-ref-idx) state pc labels)
  (declare (ignore labels))
  (let* ((addr (vm-reg-get state (vm-closure-reg inst)))
         (closure (vm-heap-get state addr))
         (idx (vm-closure-index inst))
         (values-list (vm-closure-captured-values closure)))
    (when (>= idx (length values-list))
      (error "Closure ref index ~D out of bounds (captured ~D values)" idx (length values-list)))
    (vm-reg-set state (vm-dst inst) (nth idx values-list))
    (values (1+ pc) nil nil)))

;;; ── Multiple Values and Apply ────────────────────────────────────────────

(defmethod execute-instruction ((inst vm-values) state pc labels)
  (declare (ignore labels))
  (let* ((src-regs (vm-src-regs inst))
         (all-values (mapcar (lambda (reg) (vm-reg-get state reg)) src-regs)))
    ;; Store all values in state
    (setf (vm-values-list state) all-values)
    ;; Primary value goes to dst
    (vm-reg-set state (vm-dst inst) (if all-values (first all-values) nil))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-mv-bind) state pc labels)
  (declare (ignore labels))
  (let ((vals (vm-values-list state))
        (dst-regs (vm-dst-regs inst)))
    (loop for reg in dst-regs
          for i from 0
          do (vm-reg-set state reg (if (< i (length vals))
                                       (nth i vals)
                                       nil)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-values-to-list) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (copy-list (vm-values-list state)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-spread-values) state pc labels)
  (declare (ignore labels))
  (let ((lst (vm-reg-get state (vm-src inst))))
    (setf (vm-values-list state) (if (listp lst) lst (list lst)))
    (vm-reg-set state (vm-dst inst) (if (listp lst) (first lst) lst))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-clear-values) state pc labels)
  (declare (ignore labels inst))
  (setf (vm-values-list state) nil)
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-ensure-values) state pc labels)
  (declare (ignore labels))
  (unless (vm-values-list state)
    (setf (vm-values-list state) (list (vm-reg-get state (vm-src inst)))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-next-method-p) state pc labels)
  (declare (ignore labels))
  ;; Look for the innermost method context in method-call-stack
  (let ((ctx (car (vm-method-call-stack state))))
    (vm-reg-set state (vm-dst inst)
                (if (and ctx (cddr ctx) (cadr ctx) (cdadr ctx))
                    t
                    nil)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-call-next-method) state pc labels)
  ;; Find current method context (top of method-call-stack)
  (let ((ctx (car (vm-method-call-stack state))))
    (unless ctx
      (error "call-next-method called outside of a generic function method"))
    (let* ((gf-ht (first ctx))
           (methods-list (second ctx))    ; [current-method next1 next2 ...]
           (orig-args (third ctx))
           (next-method (cadr methods-list)))
      (unless next-method
        (error "No next method for ~S" (gethash :__name__ gf-ht)))
      ;; Determine arguments: use supplied args or original args
      (let* ((call-args (if (vm-cnm-args-reg inst)
                            (vm-reg-get state (vm-cnm-args-reg inst))
                            orig-args)))
        (vm-push-call-frame state (1+ pc) (vm-dst inst))
        (push (list gf-ht (cdr methods-list) call-args) (vm-method-call-stack state))
        (vm-bind-closure-args next-method state call-args)
        (values (gethash (vm-closure-entry-label next-method) labels) nil nil)))))

;;; ── Call-frame helpers ───────────────────────────────────────────────────
;;;
;;; These three functions eliminate duplicated call-setup logic that previously
;;; appeared verbatim in vm-call, vm-dispatch-generic-call, vm-call-next-method,
;;; and vm-apply.

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
    (dolist (binding captured)
      (vm-reg-set state (car binding) (cdr binding)))
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
                    (vm-build-list state (nthcdr (+ n-req n-opt) arg-values))))
      ;; &key parameters
      (when key-params
        (let ((kw-args (nthcdr (+ n-req n-opt) arg-values)))
          (loop for (keyword reg default) in key-params
                do (let ((pos (position keyword kw-args)))
                     (vm-reg-set state reg
                                 (if pos (nth (1+ pos) kw-args) default)))))))
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

(defun vm-classify-arg (arg state)
  "Determine the class name of an argument for generic dispatch."
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
                   ;; Exact match
                   (gethash arg-classes methods-ht)
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

(defmethod execute-instruction ((inst vm-apply) state pc labels)
  (let* ((func      (vm-resolve-function state (vm-reg-get state (vm-func-reg inst))))
         (arg-regs  (vm-args inst))
         (dst-reg   (vm-dst inst))
         ;; Spread the last argument: (apply fn a b list) → args are (a b . list)
         (arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
         (spread-args (if arg-values
                          (append (butlast arg-values)
                                  (vm-list-to-lisp-list state (car (last arg-values))))
                          nil)))
    (cond
      ;; Host CL function — apply directly, no frame push
      ((functionp func)
       (vm-reg-set state dst-reg (apply func spread-args))
       (values (1+ pc) nil nil))
      ;; Generic function or closure
      (t
       (let ((closure (if (vm-generic-function-p func)
                          (vm-resolve-gf-method func state (car spread-args) spread-args)
                          func)))
         (vm-push-call-frame state (1+ pc) dst-reg)
         (push nil (vm-method-call-stack state))
         (vm-bind-closure-args closure state spread-args)
         (values (gethash (vm-closure-entry-label closure) labels) nil nil))))))

(defmethod execute-instruction ((inst vm-register-function) state pc labels)
  (declare (ignore labels))
  (let ((name (vm-func-name inst))
        (closure (vm-reg-get state (vm-src inst))))
    (setf (gethash name (vm-function-registry state)) closure)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-set-global) state pc labels)
  (declare (ignore labels))
  (let ((name (vm-global-name inst))
        (value (vm-reg-get state (vm-src inst))))
    (setf (gethash name (vm-global-vars state)) value)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-get-global) state pc labels)
  (declare (ignore labels))
  (let ((name (vm-global-name inst)))
    (multiple-value-bind (value found-p) (gethash name (vm-global-vars state))
      (unless found-p
        (error "Unbound global variable: ~S" name))
      (vm-reg-set state (vm-dst inst) value)
      (values (1+ pc) nil nil))))
