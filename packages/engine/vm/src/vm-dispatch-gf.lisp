(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Generic Function Dispatch
;;;
;;; Contains: %gethash-multi-key (shared key-lookup helper),
;;; vm-classify-arg, EQL specializer helpers (%eql-specializer-p,
;;; %eql-specializer-matches-p, %vm-extract-eql-specializer-keys,
;;; %vm-gf-eql-methods), vm-get-all-applicable-methods,
;;; %lookup-qualified-methods, %collect-combo-methods,
;;; *method-combination-operators*, %resolve-combination-operator.
;;;
;;; Multi-dispatch resolution (%vm-gf-uses-composite-keys-p, etc.) lives in
;;; vm-dispatch-gf-multi.lisp.
;;; Call dispatch (%vm-dispatch-custom-combination, vm-dispatch-generic-call,
;;; %vm-dispatch-call) lives in vm-dispatch-gf-call.lisp.
;;;
;;; Load order: after vm-dispatch.lisp, before vm-dispatch-gf-multi.lisp.
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

(defun %vm-key-has-eql-specializer-p (key)
  "Return T when KEY includes at least one eql specializer." 
  (cond
    ((%eql-specializer-p key) t)
    ((listp key) (some #'%eql-specializer-p key))
    (t nil)))

(defun %vm-specializer-key-matches-args-p (key all-args state)
  "Return T when dispatch KEY applies to ALL-ARGS under STATE classification." 
  (cond
    ((%eql-specializer-p key)
     (and (= (length all-args) 1)
          (%eql-specializer-matches-p key (car all-args))))
    ((listp key)
     (and (= (length key) (length all-args))
          (every (lambda (spec arg)
                   (or (eq spec t)
                       (%eql-specializer-matches-p spec arg)
                       (eq spec (vm-classify-arg arg state))))
                 key all-args)))
    (t
     (and (= (length all-args) 1)
          (let ((class-name (vm-classify-arg (car all-args) state)))
            (or (eq key t)
                (eq key class-name)))))))

(defun %vm-arg-cpls (state all-args)
  "Return class-precedence lists for ALL-ARGS, each extended with T." 
  (mapcar (lambda (arg)
            (let* ((class-name (vm-classify-arg arg state))
                   (class-ht (gethash class-name (vm-class-registry state)))
                   (cpl (if class-ht
                            (gethash :__cpl__ class-ht)
                            (list class-name))))
              (if (member t cpl) cpl (append cpl (list t)))))
          all-args))

(defun %vm-dispatch-key-collect (remaining prefix)
  "Generate all dispatch-key tuples from REMAINING CPL lists, prepending PREFIX."
  (if (null remaining)
      (list (nreverse prefix))
      (loop for class in (car remaining)
            nconc (%vm-dispatch-key-collect (cdr remaining) (cons class prefix)))))

(defun %vm-dispatch-key-combinations (cpls)
  "Return dispatch-key combinations from CPLs, most-specific first."
  (%vm-dispatch-key-collect cpls nil))

(defun %vm-canonical-dispatch-key (combo)
  "Return the canonical dispatch-table key for COMBO." 
  (if (= (length combo) 1)
      (first combo)
      combo))

(defun %vm-method-value->list (value)
  "Normalize dispatch-table VALUE to a flat method list." 
  (cond
    ((null value) nil)
    ((listp value) (copy-list value))
    (t (list value))))

(defun %vm-collect-applicable-methods (methods-ht state all-args)
  "Collect applicable methods from METHODS-HT in most-specific-first order." 
  (let ((result nil)
        (seen nil))
    ;; EQL-specialized methods first.
    (maphash (lambda (key value)
               (when (and (%vm-key-has-eql-specializer-p key)
                          (%vm-specializer-key-matches-args-p key all-args state))
                 (dolist (method (%vm-method-value->list value))
                   (unless (member method seen)
                     (push method seen)
                     (setf result (append result (list method)))))))
             methods-ht)
    ;; Then walk class-precedence combinations in canonical key form.
    (dolist (combo (%vm-dispatch-key-combinations (%vm-arg-cpls state all-args)))
      (dolist (method (%vm-method-value->list
                       (gethash (%vm-canonical-dispatch-key combo) methods-ht)))
        (unless (member method seen)
          (push method seen)
          (setf result (append result (list method))))))
    result))

(defun vm-get-all-applicable-methods (gf-ht state all-args)
  "Return list of all applicable method closures for GF-HT and ALL-ARGS, most-specific first.
EQL specializers are checked first (most specific), then class-based dispatch via CPL."
  (%vm-collect-applicable-methods (gethash :__methods__ gf-ht) state all-args))

(defun %lookup-qualified-methods (gf-ht qual-key state all-arg-values)
  "Look up qualified methods for QUAL-KEY (:__BEFORE__, :__AFTER__, :__AROUND__) in GF-HT.
Returns a flat list of applicable closures, most-specific first."
  (let ((qual-ht (gethash qual-key gf-ht)))
    (when qual-ht
      (%vm-collect-applicable-methods qual-ht state all-arg-values))))

(defun %collect-combo-methods (gf-ht qual-key state all-arg-values)
  "Collect all applicable methods for custom combination by walking the CPL.
Returns methods most-specific-first using the canonical specializer key shape."
  (let ((qual-ht (gethash qual-key gf-ht)))
    (when qual-ht
      (%vm-collect-applicable-methods qual-ht state all-arg-values))))

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
