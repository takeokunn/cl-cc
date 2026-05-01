(in-package :cl-cc/expand)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; LOOP — FOR Sub-parsers
;;;
;;; Reads from: loop-data.lisp (*loop-boundary-keywords*, *loop-hash-iter-keywords*,
;;;             *loop-using-keywords*, *loop-accum-keyword-table*)
;;; Provides:   loop-kw-p, loop-kw-member-p, %loop-opt-*/collect-body,
;;;             *loop-for-parsers*, define-loop-for-parser,
;;;             %parse-for-clause
;;;
;;; Internal organisation:
;;;   2b. Token predicates    — loop-kw-p, loop-kw-member-p
;;;   2c. CPS token utilities — %loop-opt-kw, %loop-opt-next, %loop-opt-by, …
;;;   2d. FOR sub-parsers     — one defun per FOR variant + define-loop-for-parser
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; ── 2b. Token predicates ─────────────────────────────────────────────────

(defun loop-kw-p (sym name)
  "True when SYM is a symbol whose name equals NAME (case-insensitive)."
  (and (symbolp sym) (string-equal (symbol-name sym) name)))

(defun loop-kw-member-p (sym)
  "True when SYM is a recognised LOOP boundary keyword."
  (and (symbolp sym)
       (member (symbol-name sym) *loop-boundary-keywords* :test #'string-equal)))

;;; ── 2c. CPS token utilities ──────────────────────────────────────────────
;;;
;;; Convention: each utility returns (values result remaining-tokens).
;;; The caller threads REMAINING explicitly — no hidden state.

(defun %loop-opt-kw (kw remaining)
  "Consume KW if REMAINING starts with it.
Returns (values found? remaining)."
  (if (and remaining (loop-kw-p (car remaining) kw))
      (values t (cdr remaining))
      (values nil remaining)))

(defun %loop-opt-next (kw remaining)
  "Consume KW and the token immediately after it.
Returns (values token remaining-after-token), or (values nil remaining) if KW absent.
This is the canonical CPS pattern for optional two-token consumption."
  (multiple-value-bind (found? rest) (%loop-opt-kw kw remaining)
    (if found? (values (car rest) (cdr rest)) (values nil remaining))))

(defun %loop-opt-by (remaining)
  "Optionally consume BY expr.  Returns (values by-form-or-nil remaining)."
  (%loop-opt-next "BY" remaining))

(defun %loop-opt-into (remaining)
  "Optionally consume INTO var.  Returns (values var-or-nil remaining)."
  (%loop-opt-next "INTO" remaining))

(defun %loop-collect-body (remaining)
  "Consume non-keyword forms until a boundary keyword or end of tokens.
Returns (values forms remaining)."
  (loop with forms = nil
        while (and remaining (not (loop-kw-member-p (car remaining))))
        do (push (pop remaining) forms)
        finally (return (values (nreverse forms) remaining))))

;;; ── 2d. FOR sub-parsers ──────────────────────────────────────────────────
;;;
;;; Contract: (var remaining) → (values spec-plist remaining).
;;; Each parser consumes its own tokens from REMAINING and returns the rest.
;;;
;;; define-loop-for-parser registers a named function into *loop-for-parsers*
;;; in a single form — symmetric with define-loop-iter-emitter.

(defvar *loop-for-parsers* nil
  "Alist: FOR sub-keyword string → parser fn.
Each entry is (keyword-string . fn) where fn: (var remaining) → (values spec remaining).
Populated by define-loop-for-parser.")

(defmacro %define-loop-for-parser (keyword fn-name (var remaining) &body body)
  `(progn
     (defun ,fn-name (,var ,remaining) ,@body)
     (setf *loop-for-parsers*
           (cons (cons ,keyword #',fn-name)
                 (remove ,keyword *loop-for-parsers* :key #'car :test #'string=)))))

(defmacro define-loop-for-parser (keyword (var remaining) &body body)
  "Define a FOR sub-parser for KEYWORD string and register it in *loop-for-parsers*.
The parser receives (VAR REMAINING) and must return (values spec-plist remaining)."
  (let ((fn-name (intern (format nil "%PARSE-FOR-~A" keyword))))
    `(%define-loop-for-parser ,keyword ,fn-name (,var ,remaining) ,@body)))

;;; ── End-condition data tables ────────────────────────────────────────────
;;;
;;; Each entry is (keyword-string . (plist-key . set-downward-p)).
;;; %loop-parse-end-condition uses these as Prolog-style facts:
;;;   (end-kw . (place . direction)) → update spec accordingly.

(defparameter *loop-from-end-conditions*
  '(("TO"     . (:to    . nil))
    ("BELOW"  . (:below . nil))
    ("UPTO"   . (:to    . nil))
    ("DOWNTO" . (:to    . t))
    ("ABOVE"  . (:above . t)))
  "End-condition keyword table for the FROM iterator.")

(defparameter *loop-downfrom-end-conditions*
  '(("TO"     . (:to    . nil))
    ("DOWNTO" . (:to    . nil))
    ("ABOVE"  . (:above . nil)))
  "End-condition keyword table for the DOWNFROM iterator.
DOWNFROM already sets :downward in the initial spec, so no entry needs it.")

(defun %loop-parse-end-condition (spec remaining end-table)
  "Optionally consume one end-condition keyword from REMAINING using END-TABLE.
Returns (values spec remaining).  SPEC is modified in place when a match is found."
  (let ((entry (and remaining
                    (assoc (symbol-name (car remaining)) end-table
                           :test #'string-equal))))
    (if entry
        (progn
          (pop remaining)
          (setf (getf spec (cadr entry)) (pop remaining))
          (when (cddr entry)
            (setf (getf spec :downward) t))
          (values spec remaining))
        (values spec remaining))))

;;; FR-695: FOR variant: var DOWNFROM expr [TO/DOWNTO/ABOVE expr] [BY expr]

(define-loop-for-parser "DOWNFROM" (var remaining)
  "Parse: var DOWNFROM expr [TO/DOWNTO/ABOVE expr] [BY expr]"
  (let* ((from (pop remaining))
         (spec (list :for var :type :from :from from :downward t)))
    (multiple-value-bind (spec rest)
        (%loop-parse-end-condition spec remaining *loop-downfrom-end-conditions*)
      (multiple-value-bind (by rest2) (%loop-opt-by rest)
        (when by (setf (getf spec :by) by))
        (values spec rest2)))))

;;; FR-695: Also handle DOWNTO/ABOVE as end-condition keywords in FROM parser

(define-loop-for-parser "FROM" (var remaining)
  "Parse: var FROM expr [TO/BELOW/UPTO/DOWNTO/ABOVE expr] [BY expr]"
  (let* ((from (pop remaining))
         (spec (list :for var :type :from :from from)))
    (multiple-value-bind (spec rest)
        (%loop-parse-end-condition spec remaining *loop-from-end-conditions*)
      (multiple-value-bind (by rest2) (%loop-opt-by rest)
        (when by (setf (getf spec :by) by))
        (values spec rest2)))))

;;; FOR variant: var IN expr [BY fn]

(define-loop-for-parser "IN" (var remaining)
  "Parse: var IN expr [BY fn]"
  (let ((list-form (pop remaining)))
    (multiple-value-bind (by rest) (%loop-opt-by remaining)
      (values (list :for var :type :in :in list-form :by by) rest))))

;;; FOR variant: var ON expr [BY fn]

(define-loop-for-parser "ON" (var remaining)
  "Parse: var ON expr [BY fn]"
  (let ((list-form (pop remaining)))
    (multiple-value-bind (by rest) (%loop-opt-by remaining)
      (values (list :for var :type :on :on list-form :by by) rest))))

;;; FOR variant: var ACROSS expr

(define-loop-for-parser "ACROSS" (var remaining)
  "Parse: var ACROSS expr"
  (values (list :for var :type :across :across (pop remaining)) remaining))

;;; FOR variant: var = expr [THEN step]

(define-loop-for-parser "=" (var remaining)
  "Parse: var = expr [THEN step]"
  (let* ((init (pop remaining))
         (spec (list :for var :type :equals :equals init)))
    (when (and remaining (loop-kw-p (car remaining) "THEN"))
      (pop remaining) (setf (getf spec :then) (pop remaining)))
    (values spec remaining)))

;;; FOR variant: var BEING [THE] HASH-KEYS/HASH-VALUES OF/IN table [USING (...)]
;;;
;;; This is the most complex FOR variant because it has three optional sub-clauses.
;;; The parsing is sequenced as: THE? → hash-type → OF/IN → table → USING?.

(define-loop-for-parser "BEING" (var remaining)
  "Parse: var BEING [THE] HASH-KEYS/HASH-VALUES OF/IN table [USING (kind paired-var)]"
  ;; Optional THE
  (when (and remaining (loop-kw-p (car remaining) "THE"))
    (pop remaining))
  ;; Hash iteration type
  (let* ((hash-kw   (pop remaining))
         (iter-type (or (cdr (assoc (symbol-name hash-kw) *loop-hash-iter-keywords*
                                    :test #'string-equal))
                        (error "Expected HASH-KEYS or HASH-VALUES after BEING [THE], got ~S"
                               hash-kw))))
    ;; OF or IN
    (let ((of-kw (pop remaining)))
      (unless (or (loop-kw-p of-kw "OF") (loop-kw-p of-kw "IN"))
        (error "Expected OF or IN after ~S, got ~S" hash-kw of-kw)))
    ;; Table form
    (let ((spec (list :for var :type iter-type :hash-table (pop remaining))))
      ;; Optional USING (kind paired-var)
      (when (and remaining (loop-kw-p (car remaining) "USING"))
        (pop remaining)
        (let ((using-spec (pop remaining)))
          (when (and (consp using-spec) (>= (length using-spec) 2))
            (let ((using-type (or (cdr (assoc (symbol-name (car using-spec))
                                              *loop-using-keywords*
                                              :test #'string-equal))
                                  (error "Expected HASH-VALUE or HASH-KEY in USING, got ~S"
                                         (car using-spec)))))
              (setf (getf spec :using-type) using-type)
              (setf (getf spec :using-var)  (cadr using-spec))))))
      (values spec remaining))))

(defun %parse-for-clause (remaining)
  "Dispatch FOR var sub-kw ... → (values spec remaining) via *loop-for-parsers*."
  (let* ((var    (pop remaining))
         (kw     (pop remaining))
         (parser (cdr (assoc (symbol-name kw) *loop-for-parsers* :test #'string-equal))))
    (unless parser
      (error "Unknown FOR keyword ~S.  Valid keywords: FROM IN ON ACROSS = BEING" kw))
    (funcall parser var remaining)))

) ; end eval-when (for sub-parsers)
