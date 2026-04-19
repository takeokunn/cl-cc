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

