(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Generic Function Dispatch
;;;
;;; Contains: %gethash-multi-key (shared key-lookup helper),
;;; vm-classify-arg, EQL specializer helpers (%eql-specializer-p,
;;; %eql-specializer-matches-p, %vm-extract-eql-specializer-keys,
;;; %vm-gf-eql-methods), vm-get-all-applicable-methods,
;;; %lookup-qualified-methods, %collect-combo-methods,
;;; *method-combination-operators*, %resolve-combination-operator,
;;; %vm-dispatch-custom-combination, vm-dispatch-generic-call,
;;; %vm-dispatch-call.
;;;
;;; Multi-dispatch resolution (%vm-gf-uses-composite-keys-p,
;;; %vm-resolve-single-dispatch, %vm-resolve-composite-dispatch,
;;; vm-resolve-gf-method, vm-resolve-multi-dispatch,
;;; vm-try-dispatch-combinations, vm-try-dispatch-sub) lives in
;;; vm-dispatch-gf-multi.lisp.
;;;
;;; Load order: after vm-dispatch.lisp, before vm-execute.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Generic dispatch helpers ──────────────────────────────────────────────

(defun %gethash-multi-key (ht keys)
  "Try each key form in KEYS against HT; collect all matches into a flat list.
Values stored as lists are spliced in; scalar values are wrapped in a list."
  (loop for key in keys
        for val = (gethash key ht)
        when val nconc (if (listp val) (copy-list val) (list val))))

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
  "Look up qualified methods for QUAL-KEY (:__BEFORE__, :__AFTER__, :__AROUND__) in GF-HT.
Checks four key forms in priority order: (list class-name), (list t), class-name, t.
Returns a flat list of applicable closures, most-specific first."
  (let ((qual-ht (gethash qual-key gf-ht)))
    (when qual-ht
      (let ((class-name (vm-classify-arg (car all-arg-values) state)))
        ;; Keys tried in priority order: list-wrapped exact, list-wrapped t,
        ;; bare exact, bare t (backward compat for single-key entries)
        (%gethash-multi-key qual-ht
                            (list (list class-name) (list t)
                                  class-name t))))))

(defun %collect-combo-methods (gf-ht qual-key state first-arg)
  "Collect all applicable methods for custom combination by walking the CPL.
Returns methods most-specific-first, deduplicated across both plain and
list-wrapped key forms (for multi-dispatch backward compat)."
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
      ;; Walk CPL (deduplicated); for each ancestor try both plain and
      ;; list-wrapped keys, adding newly seen methods to result.
      (dolist (ancestor cpl)
        (unless (member ancestor seen)
          (push ancestor seen)
          (dolist (method (%gethash-multi-key qual-ht (list ancestor (list ancestor))))
            (unless (member method result)
              (push method result))))))
    (nreverse result)))

(defparameter *method-combination-operators*
  `((+      . ,#'+)
    (*      . ,#'*)
    (list   . ,#'list)
    (append . ,#'append)
    (nconc  . ,#'nconc)
    (max    . ,#'max)
    (min    . ,#'min)
    (and    . ,(lambda (&rest args) (every #'identity args)))
    (or     . ,(lambda (&rest args) (some  #'identity args)))
    (progn  . ,(lambda (&rest args) (car (last args)))))
  "Alist mapping method combination names to their combining operator functions.
Add entries here to support new combination types without touching dispatch logic.")

(defun %resolve-combination-operator (combination)
  "Return the operator function for COMBINATION, or signal an error."
  (let ((entry (assoc combination *method-combination-operators*)))
    (if entry
        (cdr entry)
        (error "Unknown method combination operator: ~S" combination))))

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
         (operator (%resolve-combination-operator combination)))
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
              (vm-profile-enter-call state (vm-closure-entry-label method-closure))
              (vm-bind-closure-args method-closure state all-arg-values)
          (values (vm-label-table-lookup labels (vm-closure-entry-label method-closure)) nil nil))))))

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
          (vm-profile-enter-call state (vm-closure-entry-label method-closure))
          (vm-bind-closure-args method-closure state all-arg-values)
          (values (vm-label-table-lookup labels (vm-closure-entry-label method-closure)) nil nil))
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
          (vm-profile-enter-call state (vm-closure-entry-label first-method))
          (vm-bind-closure-args first-method state all-arg-values)
          (values (vm-label-table-lookup labels (vm-closure-entry-label first-method)) nil nil)))))

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
        (vm-profile-enter-call state (vm-closure-entry-label func) :tail-p tail-p)
        (vm-bind-closure-args func state arg-values)
           (values (vm-label-table-lookup labels (vm-closure-entry-label func)) nil nil)))))

