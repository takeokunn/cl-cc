(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; LOOP — Parser Layer
;;;
;;; Reads from: loop-data.lisp (*loop-* vars)
;;; Produces:   a plist IR consumed by the generator in loop.lisp
;;;
;;; Internal organisation:
;;;   2a. Parse state struct  — typed record replacing 7 mutable locals
;;;   2b. Token predicates    — loop-kw-p, loop-kw-member-p
;;;   2c. CPS token utilities — %loop-opt-kw, %loop-opt-next, %loop-opt-by, …
;;;   2d. FOR sub-parsers     — one defun per FOR variant + define-loop-for-parser
;;;   2e. Top-level clause handlers  — CPS contract: (state rest) → (values state rest)
;;;   2f. Named wrappers for parameterised types
;;;   2g. Clause dispatch table (derived from data layer)
;;;   2h. Filter helpers
;;;   2i. Main dispatch loop  — parse-loop-clauses
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; ── 2a. Parse state ──────────────────────────────────────────────────────
;;;
;;; All list fields are accumulated in reverse order during parsing.
;;; finalize-loop-state nreverses them before handing off to the generator.

(defstruct (loop-parse-state (:conc-name lps-) (:copier nil) (:predicate nil))
  (iterations    nil)   ; FOR / WITH / REPEAT specs  (reversed)
  (body-forms    nil)   ; DO and implicit body forms  (reversed)
  (accumulations nil)   ; COLLECT / SUM / COUNT / …  (reversed)
  (conditions    nil)   ; WHILE / UNTIL / ALWAYS / … (reversed)
  (initially     nil)   ; INITIALLY forms             (reversed)
  (finally       nil)   ; FINALLY forms               (reversed)
  (pending-filter nil)  ; active WHEN/UNLESS filter, or NIL
  (loop-name     nil))  ; FR-638: named loop block name

(defun finalize-loop-state (state)
  "Build the IR plist from the parse state.
:body is left reversed so the generator can push condition/accumulation forms
before nreversing everything at assembly time.  All other fields are nreversed here."
  (list :iterations    (nreverse (lps-iterations    state))
        :body          (lps-body-forms    state)   ; reversed; generator nreverses at assembly
        :accumulations (nreverse (lps-accumulations state))
        :conditions    (nreverse (lps-conditions    state))
        :initially     (nreverse (lps-initially     state))
        :finally       (nreverse (lps-finally       state))
        :loop-name     (lps-loop-name state)))

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

;;; ── 2e. Top-level clause handlers ────────────────────────────────────────
;;;
;;; CPS contract: (state remaining-after-keyword) → (values state remaining).
;;; The dispatch table has already consumed the top-level keyword; REMAINING
;;; starts at the first token after it.

(defun %clause-for (state remaining)
  (multiple-value-bind (spec rest) (%parse-for-clause remaining)
    (push spec (lps-iterations state))
    (values state rest)))

(defun %clause-with (state remaining)
  (let ((var (pop remaining))
        (val nil))
    (when (and remaining (loop-kw-p (car remaining) "="))
      (pop remaining) (setf val (pop remaining)))
    (push (list :type :with :for var :init val) (lps-iterations state))
    (values state remaining)))

(defun %clause-repeat (state remaining)
  (push (list :type :repeat :count-var (gensym "COUNT") :n (pop remaining))
        (lps-iterations state))
  (values state remaining))

(defun %clause-do (state remaining)
  (multiple-value-bind (forms rest) (%loop-collect-body remaining)
    (let ((filter (lps-pending-filter state)))
      (setf (lps-pending-filter state) nil)
      (if filter
          ;; Wrap all DO forms together under the active filter.
          (push (list* (%loop-filter-sym filter) (cadr filter) forms)
                (lps-body-forms state))
          (dolist (f forms) (push f (lps-body-forms state)))))
    (values state rest)))

(defun %clause-filter (state remaining filter-type)
  (setf (lps-pending-filter state) (list filter-type (pop remaining)))
  (values state remaining))

(defun %clause-condition (state remaining cond-type)
  (push (list cond-type (pop remaining)) (lps-conditions state))
  (values state remaining))

(defun %clause-initially (state remaining)
  (multiple-value-bind (forms rest) (%loop-collect-body remaining)
    (dolist (f forms) (push f (lps-initially state)))
    (values state rest)))

(defun %clause-finally (state remaining)
  (multiple-value-bind (forms rest) (%loop-collect-body remaining)
    (dolist (f forms) (push f (lps-finally state)))
    (values state rest)))

;; FR-638: NAMED clause — (loop named foo ...)
(defun %clause-named (state remaining)
  (setf (lps-loop-name state) (pop remaining))
  (values state remaining))

(defun %clause-accum (state remaining acc-type)
  (let ((form   (pop remaining))
        (filter (lps-pending-filter state)))
    (setf (lps-pending-filter state) nil)
    (multiple-value-bind (into-var rest) (%loop-opt-into remaining)
      (push (list acc-type form into-var filter) (lps-accumulations state))
      (values state rest))))

;;; ── 2f. Parameterised clause wrappers ────────────────────────────────────
;;;
;;; Each row: (keyword-string base-fn type-keyword).
;;; The macro generates a named defun for each, fixing the type argument.
;;; Named functions (not lambdas) keep *loop-top-clause-parsers* a pure
;;; (string . #'fn) alist and give readable backtraces.
;;;
;;; To add a clause: one row here → wrapper defun generated automatically.

(defmacro define-parameterized-clauses (&rest specs)
  "Generate named defuns from SPECS, each of form (keyword-string base-fn type).
Produces: (defun %clause-<keyword> (state remaining) (base-fn state remaining type))"
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (keyword base-fn type) spec
                   (let ((fn-name (intern (format nil "%CLAUSE-~A" keyword) :cl-cc)))
                     `(defun ,fn-name (state remaining)
                        (,base-fn state remaining ,type)))))
               specs)))

(define-parameterized-clauses
  ("WHEN"    %clause-filter    :when)
  ("IF"      %clause-filter    :when)    ; IF is a WHEN alias in LOOP
  ("UNLESS"  %clause-filter    :unless)
  ("WHILE"   %clause-condition :while)
  ("UNTIL"   %clause-condition :until)
  ("ALWAYS"  %clause-condition :always)
  ("NEVER"   %clause-condition :never)
  ("THEREIS" %clause-condition :thereis))

;;; ── 2g. Top-level clause dispatch table ──────────────────────────────────
;;;
;;; Accumulation handlers are derived from *loop-accum-keyword-table*
;;; (Prolog-style rule derivation: each (keyword . type) fact → (keyword . handler) rule).

(defun %make-accum-handler (acc-type)
  "Build a clause handler that calls %clause-accum with ACC-TYPE fixed.
This is the Prolog rule-derivation step: (keyword . type) fact → (keyword . handler)."
  (lambda (s r) (%clause-accum s r acc-type)))

(defvar *loop-top-clause-parsers*
  (append
   (list (cons "FOR"       #'%clause-for)
         (cons "WITH"      #'%clause-with)
         (cons "REPEAT"    #'%clause-repeat)
         (cons "DO"        #'%clause-do)
         (cons "WHEN"      #'%clause-when)
         (cons "IF"        #'%clause-if)
         (cons "UNLESS"    #'%clause-unless)
         (cons "WHILE"     #'%clause-while)
         (cons "UNTIL"     #'%clause-until)
         (cons "ALWAYS"    #'%clause-always)
         (cons "NEVER"     #'%clause-never)
         (cons "THEREIS"   #'%clause-thereis)
         (cons "INITIALLY" #'%clause-initially)
         (cons "FINALLY"   #'%clause-finally)
         (cons "NAMED"     #'%clause-named))
   ;; Derive accumulation handlers from the data table.
   (mapcar (lambda (entry)
             (cons (car entry) (%make-accum-handler (cdr entry))))
           *loop-accum-keyword-table*))
  "Alist: top-level keyword string → clause handler fn.
Handler contract: (state remaining-after-kw) → (values state remaining).
To add a clause: one %clause-* defun + one entry here (or update *loop-accum-keyword-table*).")

;;; ── 2h. Filter helpers ───────────────────────────────────────────────────

(defun %loop-filter-sym (filter)
  "Return WHEN or UNLESS for a filter spec (:when test) or (:unless test)."
  (if (eq (car filter) :when) 'when 'unless))

(defun %loop-wrap-filter (form filter)
  "Wrap FORM with (WHEN/UNLESS test form) if FILTER is non-nil, else return FORM."
  (if filter
      (list (%loop-filter-sym filter) (cadr filter) form)
      form))

;;; ── 2i. Main dispatch loop ───────────────────────────────────────────────
;;;
;;; All clause complexity lives in the handler functions above.
;;; This function is deliberately trivial: iterate → dispatch → collect.

(defun parse-loop-clauses (clauses)
  "Parse LOOP CLAUSES into IR via table-driven CPS dispatch.
Returns a plist with keys: :iterations :body :accumulations :conditions :initially :finally."
  (loop with state     = (make-loop-parse-state)
        with remaining = clauses
        until (null remaining)
        do (let* ((token   (car remaining))
                  (handler (and (symbolp token)
                                (cdr (assoc (symbol-name token)
                                            *loop-top-clause-parsers*
                                            :test #'string-equal)))))
             (if handler
                 (multiple-value-setq (state remaining)
                   (funcall handler state (cdr remaining)))
                 (push (pop remaining) (lps-body-forms state))))
        finally (return (finalize-loop-state state))))

) ; end eval-when (parser layer)
