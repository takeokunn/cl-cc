(in-package :cl-cc/expand)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; LOOP — Parser Layer
;;;
;;; Reads from: loop-data.lisp (*loop-* vars)
;;; Produces:   a plist IR consumed by the generator in loop.lisp
;;;
;;; Internal organisation:
;;;   2a. Parse state struct  — typed record replacing 7 mutable locals
;;;   2e. Top-level clause handlers  — CPS contract: (state rest) → (values state rest)
;;;   2f. Named wrappers for parameterised types
;;;   2g. Clause dispatch table (derived from data layer)
;;;   2h. Filter helpers
;;;   2i. Main dispatch loop  — parse-loop-clauses
;;;
;;; 2b–2d (token predicates, CPS utilities, FOR sub-parsers) live in loop-parser-for.lisp
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
                   (let ((fn-name (intern (format nil "%CLAUSE-~A" keyword))))
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
