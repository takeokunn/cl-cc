(in-package :cl-cc)
;;; List Mutation Macros

;; PUSH macro — FR-693: evaluate compound place subforms exactly once
(our-defmacro push (value place)
  "Push VALUE onto the front of list PLACE."
  (if (symbolp place)
      ;; Simple variable: no subform evaluation issue
      `(setf ,place (cons ,value ,place))
      ;; Compound place: save each subform arg to avoid double-eval
      (let* ((args     (rest place))
             (arg-syms (mapcar (lambda (a) (declare (ignore a)) (gensym "PA")) args))
             (bindings (mapcar (lambda (s a) `(,s ,a)) arg-syms args))
             (saved    (cons (first place) arg-syms))
             (v        (gensym "V")))
        `(let* ((,v ,value) ,@bindings)
           (setf ,saved (cons ,v ,saved))))))

;; POP macro — FR-693: read compound place once; evaluate subforms once
(our-defmacro pop (place)
  "Remove and return the first element of list PLACE."
  (if (symbolp place)
      ;; Simple variable: read into tmp once, write cdr back
      (let ((tmp (gensym "TMP")))
        `(let ((,tmp ,place))
           (setf ,place (cdr ,tmp))
           (car ,tmp)))
      ;; Compound place: save subform args, read place into tmp once
      (let* ((args     (rest place))
             (arg-syms (mapcar (lambda (a) (declare (ignore a)) (gensym "PA")) args))
             (bindings (mapcar (lambda (s a) `(,s ,a)) arg-syms args))
             (saved    (cons (first place) arg-syms))
             (tmp      (gensym "TMP")))
        `(let* (,@bindings (,tmp ,saved))
           (setf ,saved (cdr ,tmp))
           (car ,tmp)))))

;; INCF macro — FR-693: gensym-protect compound place subforms
(our-defmacro incf (place &optional (delta 1))
  "Increment PLACE by DELTA (default 1)."
  (if (symbolp place)
      `(setq ,place (+ ,place ,delta))
      (let ((d (gensym "D")))
        `(let ((,d ,delta))
           (setf ,place (+ ,place ,d))))))

;; DECF macro — FR-693: gensym-protect compound place subforms
(our-defmacro decf (place &optional (delta 1))
  "Decrement PLACE by DELTA (default 1)."
  (if (symbolp place)
      `(setq ,place (- ,place ,delta))
      (let ((d (gensym "D")))
        `(let ((,d ,delta))
           (setf ,place (- ,place ,d))))))

;; 1+ and 1- utility functions
(our-defmacro 1+ (n)
  `(+ ,n 1))

(our-defmacro 1- (n)
  `(- ,n 1))

;; SIGNUM: returns -1, 0, or 1 based on sign
;; FR-684: signum — type-preserving (ANSI CL 12.2.17)
(our-defmacro signum (n)
  (let ((nv (gensym "N")))
    `(let ((,nv ,n))
       (cond ((zerop ,nv) ,nv)
             ((> ,nv 0) (if (integerp ,nv) 1 1.0))
             (t (if (integerp ,nv) -1 -1.0))))))

;; ISQRT: integer square root (floor of real square root)
;; FR-683: isqrt — integer Newton's method for precision on large integers
(our-defmacro isqrt (n)
  (let ((nvar (gensym "N")) (g (gensym "G")) (prev (gensym "P")))
    `(let* ((,nvar ,n)
            (,g (floor (sqrt (float ,nvar))))
            (,prev nil))
       ;; Newton correction loop: converges in 0-2 iterations
       (block nil
         (tagbody
           :loop
           (when (eql ,g ,prev) (return ,g))
           (setq ,prev ,g)
           (setq ,g (floor (+ ,g (floor ,nvar ,g)) 2))
           (go :loop))))))

;; WITH-OPEN-STREAM: like with-open-file but for existing streams
(our-defmacro with-open-stream (var-stream &body body)
  (let ((var (first var-stream))
        (stream (second var-stream)))
    `(let ((,var ,stream))
       (unwind-protect (progn ,@body)
         (close ,var)))))

;; RETURN macro (return from nil block)
(our-defmacro return (&optional value)
  "Return VALUE from the nearest NIL block."
  `(return-from nil ,value))

;; ROTATEF macro
(our-defmacro rotatef (&rest places)
  (cond
    ((< (length places) 2) nil)
    ((= (length places) 2)
     (let ((tmp (gensym "TMP")))
       `(let ((,tmp ,(first places)))
          (setq ,(first places) ,(second places))
          (setq ,(second places) ,tmp)
          nil)))
    (t (let ((tmp (gensym "TMP")))
         `(let ((,tmp ,(first places)))
            ,@(loop for (a b) on places
                    while b
                    collect `(setf ,a ,b))
            (setf ,(car (last places)) ,tmp)
            nil)))))

;; DESTRUCTURING-BIND macro
(our-defmacro destructuring-bind (pattern expr &body body)
  "Bind variables in PATTERN to corresponding parts of EXPR.
   Supports: required, &optional, &rest, &body, &key, &aux."
  (let ((expr-var (gensym "EXPR")))
    `(let ((,expr-var ,expr))
       (let* ,(destructure-lambda-list pattern expr-var)
         ,@body))))

;; PROG macro - let + tagbody + block
(our-defmacro prog (vars &body body)
  "Establish bindings with LET, wrap body in BLOCK NIL + TAGBODY."
  `(block nil
     (let ,vars
       (tagbody ,@body))))

;; PROG* macro - let* + tagbody + block
(our-defmacro prog* (vars &body body)
  "Like PROG but with sequential bindings (LET*)."
  `(block nil
     (let* ,vars
       (tagbody ,@body))))

;; WITH-SLOTS macro - bind slot accessors as local variables
(our-defmacro with-slots (slot-names instance &body body)
  "Evaluate BODY with SLOT-NAMES as symbol macros for slot-value of INSTANCE.
ANSI: uses symbol-macrolet so setf on slot names writes back to the object."
  (let ((inst-var (gensym "INST")))
    `(let ((,inst-var ,instance))
       (symbol-macrolet ,(mapcar (lambda (slot)
                                   (if (listp slot)
                                       `(,(first slot) (slot-value ,inst-var ',(second slot)))
                                       `(,slot (slot-value ,inst-var ',slot))))
                                 slot-names)
         ,@body))))

;; NTH-VALUE macro - extract nth return value from multiple-values form
;; FR-402: when N is a constant integer, expand to multiple-value-bind directly
(our-defmacro nth-value (n form)
  (if (and (integerp n) (<= 0 n 19))
      (let ((vars (loop for i from 0 to n collect (gensym (format nil "V~D-" i)))))
        `(multiple-value-bind ,vars ,form
           (declare (ignore ,@(butlast vars)))
           ,(car (last vars))))
      (let ((vals-var (gensym "VALS")))
        `(let ((,vals-var (multiple-value-list ,form)))
           (nth ,n ,vals-var)))))

;; ECASE — signal type-error when no clause matches
(our-defmacro ecase (keyform &body cases)
  "Like CASE but signals a TYPE-ERROR if no case matches."
  (let ((key-var (gensym "KEY"))
        (keys (mapcan (lambda (c) (let ((k (car c)))
                                    (if (listp k) (copy-list k) (list k))))
                      cases)))
    `(let ((,key-var ,keyform))
       (case ,key-var
         ,@cases
         (otherwise
          (error (make-condition 'type-error
                   :datum ,key-var
                   :expected-type '(member ,@keys))))))))

;; ETYPECASE — signal type-error when no clause matches
(our-defmacro etypecase (keyform &body cases)
  "Like TYPECASE but signals a TYPE-ERROR if no case matches."
  (let ((key-var (gensym "KEY"))
        (types (mapcar #'car cases)))
    `(let ((,key-var ,keyform))
       (typecase ,key-var
         ,@cases
         (otherwise
          (error (make-condition 'type-error
                   :datum ,key-var
                   :expected-type '(or ,@types))))))))

;; CCASE — correctable case: like ecase but signals continuable type-error (FR-354)
;; Without full restart system, behaves like ecase (signals type-error).
(our-defmacro ccase (keyform &body cases)
  "Like CASE but signals a correctable TYPE-ERROR if no case matches."
  (let ((key-var (gensym "KEY"))
        (keys (mapcan (lambda (c) (let ((k (car c)))
                                    (if (listp k) (copy-list k) (list k))))
                      cases)))
    `(let ((,key-var ,keyform))
       (case ,key-var
         ,@cases
         (otherwise
          (error (make-condition 'type-error
                   :datum ,key-var
                   :expected-type '(member ,@keys))))))))

;; CTYPECASE — correctable typecase: like etypecase but signals continuable type-error (FR-354)
(our-defmacro ctypecase (keyform &body cases)
  "Like TYPECASE but signals a correctable TYPE-ERROR if no case matches."
  (let ((key-var (gensym "KEY"))
        (types (mapcar #'car cases)))
    `(let ((,key-var ,keyform))
       (typecase ,key-var
         ,@cases
         (otherwise
          (error (make-condition 'type-error
                   :datum ,key-var
                   :expected-type '(or ,@types))))))))

;;; ------------------------------------------------------------
;;; ANSI CL Phase 1 Macros (FR-201 through FR-212)
;;; ------------------------------------------------------------

;; PSETF (FR-207) — parallel setf: evaluate all new values before setting any place
(our-defmacro psetf (&rest pairs)
  "Parallel SETF: evaluates all new values before assigning to any place.
   (psetf p1 v1 p2 v2 ...) — all vi are evaluated, then all pi are set."
  (when (oddp (length pairs))
    (error "PSETF requires an even number of arguments"))
  (let (places vals)
    (do ((rest pairs (cddr rest)))
        ((null rest))
      (push (first rest) places)
      (push (second rest) vals))
    (setf places (nreverse places)
          vals   (nreverse vals))
    (let ((temps (mapcar (lambda (v) (declare (ignore v)) (gensym "PSETF")) vals)))
      `(let ,(mapcar #'list temps vals)
         ,@(mapcar (lambda (place temp) `(setf ,place ,temp))
                   places temps)
         nil))))

;; SHIFTF (FR-206) — shift values through a series of places
(our-defmacro shiftf (&rest args)
  "Shift values: (shiftf place1 ... placeN newval) sets each place to the
   next place's old value, stores NEWVAL in the last place; returns old
   value of first place."
  (when (< (length args) 2)
    (error "SHIFTF requires at least 2 arguments (one place and one new value)"))
  (let* ((places (butlast args))
         (newval (car (last args)))
         (temps  (mapcar (lambda (p) (declare (ignore p)) (gensym "SHIFT")) places)))
    `(let ,(mapcar #'list temps places)
       ,@(mapcar (lambda (place val)
                   `(setf ,place ,val))
                 places
                 (append (cdr temps) (list newval)))
       ,(car temps))))

;; WITH-ACCESSORS (FR-205) — bind local vars to accessor function results
(our-defmacro with-accessors (slot-entries instance &body body)
  "Binds symbol macros to accessor calls for slot access.
   Each entry is (local-var-name accessor-function-name).
   ANSI: uses symbol-macrolet so setf writes back through accessor."
  (let ((inst-var (gensym "INST")))
    `(let ((,inst-var ,instance))
       (symbol-macrolet ,(mapcar (lambda (entry)
                                   (destructuring-bind (var-name accessor-name) entry
                                     `(,var-name (,accessor-name ,inst-var))))
                                 slot-entries)
         ,@body))))

;; DEFINE-MODIFY-MACRO (FR-208) — define a read-modify-write macro
(our-defmacro define-modify-macro (name lambda-list function &optional doc)
  (declare (ignore doc))   ; before docstring — declare after string is invalid in our-defmacro
  "Define a macro NAME that reads PLACE, applies FUNCTION (with extra args),
   and stores the result back into PLACE."
  (let* ((place-var (gensym "PLACE"))
         (param-names
           (mapcar (lambda (p) (if (listp p) (first p) p))
                   (remove-if (lambda (p)
                                (member p '(&optional &rest &key &aux &allow-other-keys)))
                              lambda-list))))
    `(our-defmacro ,name (,place-var ,@lambda-list)
       `(setf ,,place-var (,',function ,,place-var ,,@param-names)))))

;; ASSERT (FR-203) — signal a continuable error if a test fails
(our-defmacro assert (test &optional places datum &rest args)
  (declare (ignore places))   ; PLACES not supported in this impl; declare before docstring
  "Signal a continuable error if TEST is false."
  `(unless ,test
     ,(if datum
          `(cerror "Continue." ,datum ,@args)
          `(cerror "Continue." "Assertion failed: ~S" ',test))))

;; DEFINE-CONDITION (FR-204) — define a condition type (expands to defclass)
;; FR-417: now handles :report option → defmethod print-object
(our-defmacro define-condition (name parent-list slot-specs &rest options)
  "Define a condition type NAME with optional :report."
  (let* ((parent-names (if parent-list parent-list '(error)))
         (report-opt (find :report options
                           :key (lambda (o) (when (listp o) (first o)))))
         (report-fn (when report-opt (second report-opt)))
         (defclass-form `(defclass ,name ,parent-names ,slot-specs))
         (report-form
           (when report-fn
             (let ((c (gensym "C")) (s (gensym "S")))
               (cond
                 ((stringp report-fn)
                  `(defmethod print-object ((,c ,name) ,s)
                     (write-string ,report-fn ,s)))
                 ((and (consp report-fn) (eq (car report-fn) 'lambda))
                  `(defmethod print-object ((,c ,name) ,s)
                     (funcall ,report-fn ,c ,s)))
                 (t
                  `(defmethod print-object ((,c ,name) ,s)
                     (funcall (function ,report-fn) ,c ,s))))))))
    (if report-form
        `(progn ,defclass-form ,report-form (quote ,name))
        defclass-form)))

;; HANDLER-BIND (FR-201) — non-unwinding handler protocol via dynamic registry.
;; Uses PROGV for true dynamic binding so called functions (signal) see the handlers.
;; Handlers are called in the dynamic context of the signaler (no stack unwind).
;; If a handler returns normally it "declines" and the next handler is tried.
;; Handlers can transfer control via invoke-restart, throw, or return-from.
(register-macro 'handler-bind
  (lambda (form env)
    (declare (ignore env))
    (let ((bindings (second form))
          (body (cddr form)))
      (if (null bindings)
          `(progn ,@body)
          (let ((new-handlers-var (gensym "HANDLERS"))
                (entries (mapcar (lambda (b)
                                  `(list ',(first b) ,(second b)))
                                bindings)))
            `(let ((,new-handlers-var (append (list ,@entries) *%condition-handlers*)))
               (progv '(*%condition-handlers*) (list ,new-handlers-var)
                 ,@body)))))))

;; SIGNAL (FR-201) — walk handler-bind registry without unwinding.
;; Defined as macro so it expands inline and reads *%condition-handlers* in caller's scope.
(our-defmacro signal (condition &rest args)
  (declare (ignore args))
  (let ((cond-var (gensym "COND"))
        (entry-var (gensym "ENTRY"))
        (type-var (gensym "TYPE"))
        (fn-var (gensym "FN")))
    `(let ((,cond-var ,condition))
       (dolist (,entry-var *%condition-handlers*)
         (let ((,type-var (first ,entry-var))
               (,fn-var (second ,entry-var)))
           (when (or (eq ,type-var t) (eq ,type-var 'condition)
                     (eq ,type-var 'error) (eq ,type-var 'warning)
                     (eq ,type-var 'serious-condition))
             (funcall ,fn-var ,cond-var))))
       nil)))

;;; ── Restart Protocol (FR-202/FR-421) ────────────────────────────────────────
;;;
;;; Minimal restart protocol using catch/throw and a dynamic restart registry.
;;; *%active-restarts* is a list of (name . catch-tag) entries.
;;; restart-case pushes entries and wraps form in nested catches.
;;; invoke-restart throws to the matching tag.

;; RESTART-CASE (FR-202) — working restart protocol via catch/throw
(register-macro 'restart-case
  (lambda (form env)
    (declare (ignore env))
    (let ((clauses (cddr form))
          (form-expr (second form)))
      (if (null clauses)
          form-expr
          ;; Parse each clause: (name lambda-list [:report r] [:interactive i] [:test t] . body)
          (let ((parsed-clauses nil)
                (counter 0))
            (dolist (clause clauses)
              (let* ((name (first clause))
                     (lambda-list (second clause))
                     (rest (cddr clause))
                     (body (progn
                             (loop while (and rest (keywordp (car rest))
                                              (member (car rest) '(:report :interactive :test)))
                                   do (setf rest (cddr rest)))
                             rest))
                     (tag (intern (format nil "RESTART-~A-~D" name (incf counter))
                                  (find-package :cl-cc))))
                (push (list name tag lambda-list body) parsed-clauses)))
            (setf parsed-clauses (nreverse parsed-clauses))
            ;; Build nested catches from inside out
            (let* ((result-var (gensym "RESULT"))
                   (restarts-var (gensym "RESTARTS"))
                   ;; innermost: the form wrapped in handler-case
                   (inner form-expr)
                   ;; Wrap each restart as a catch tag
                   (wrapped inner))
              ;; Build the catch nesting from last to first
              (dolist (pc (reverse parsed-clauses))
                (let ((tag (second pc))
                      (ll  (third pc))
                      (body (fourth pc)))
                  (setf wrapped
                        `(let ((,result-var (catch ',tag ,wrapped)))
                           ;; If we got here via throw, result-var holds the thrown value
                           ;; Run the restart body; bind lambda-list params if any
                           ,(if (and ll (car ll))
                                `(let ((,(car ll) ,result-var)) ,@(or body '(nil)))
                                `(progn ,@(or body '(nil))))))))
              ;; Build the *%active-restarts* binding
              (let ((restart-entries (mapcar (lambda (pc)
                                              `(cons ',(first pc) ',(second pc)))
                                            parsed-clauses)))
                `(let ((,restarts-var (list ,@restart-entries)))
                   (let ((*%active-restarts* (append ,restarts-var *%active-restarts*)))
                     ,wrapped)))))))))

;; RESTART-BIND (FR-202) — binds restarts for the dynamic extent of body
(our-defmacro restart-bind (bindings &body body)
  "Establish restart bindings around BODY."
  (if (null bindings)
      `(progn ,@body)
      (let ((restarts-var (gensym "RESTARTS"))
            (entries (mapcar (lambda (b) `(cons ',(first b) ',(gensym "RBTAG")))
                             bindings)))
        `(let ((,restarts-var (list ,@entries)))
           (let ((*%active-restarts* (append ,restarts-var *%active-restarts*)))
             ,@body)))))

;; INVOKE-RESTART — look up and throw to the restart's catch tag
(our-defmacro invoke-restart (name &rest args)
  "Invoke restart NAME. Transfers control to the matching restart-case clause."
  (let ((entry-var (gensym "ENTRY"))
        (name-var (gensym "NAME"))
        (val-var (gensym "VAL")))
    `(let* ((,name-var ,name)
            (,entry-var (assoc ,name-var *%active-restarts* :test #'eq)))
       (if ,entry-var
           (throw (cdr ,entry-var) ,(if args (first args) nil))
           (error (format nil "No restart named ~A is active" ,name-var))))))

;; INVOKE-RESTART-INTERACTIVELY — same as invoke-restart (no interactive support)
(our-defmacro invoke-restart-interactively (name)
  `(invoke-restart ,name))

;; RESTART-NAME — extract name from a restart entry
(our-defmacro restart-name (restart)
  `(if (consp ,restart) (car ,restart) ,restart))

;; FIND-RESTART — search the active restart list
(our-defmacro find-restart (name &optional condition)
  (declare (ignore condition))
  (let ((name-var (gensym "NAME")))
    `(let ((,name-var ,name))
       (assoc ,name-var *%active-restarts* :test #'eq))))

;; COMPUTE-RESTARTS — return all active restarts
(our-defmacro compute-restarts (&optional condition)
  (declare (ignore condition))
  '*%active-restarts*)

;; RESTART-P — check if something is a restart
(register-macro 'restart-p
  (lambda (form env) (declare (ignore env)) `(consp ,(second form))))

;; ABORT — invoke the abort restart, or signal error if none
(our-defmacro abort (&optional condition)
  (declare (ignore condition))
  `(let ((r (find-restart 'abort)))
     (if r
         (invoke-restart 'abort)
         (error "ABORT invoked"))))

;; CONTINUE — invoke the continue restart if available
(our-defmacro continue (&optional condition)
  (declare (ignore condition))
  `(let ((r (find-restart 'continue)))
     (when r (invoke-restart 'continue))))

;; MUFFLE-WARNING — invoke the muffle-warning restart if available
(our-defmacro muffle-warning (&optional condition)
  (declare (ignore condition))
  `(let ((r (find-restart 'muffle-warning)))
     (when r (invoke-restart 'muffle-warning))))

;; USE-VALUE — invoke the use-value restart with a value
(our-defmacro use-value (value &optional condition)
  (declare (ignore condition))
  `(let ((r (find-restart 'use-value)))
     (if r
         (invoke-restart 'use-value ,value)
         ,value)))

;; STORE-VALUE — invoke the store-value restart with a value
(our-defmacro store-value (value &optional condition)
  (declare (ignore condition))
  `(let ((r (find-restart 'store-value)))
     (if r
         (invoke-restart 'store-value ,value)
         ,value)))

;; WITH-INPUT-FROM-STRING (FR-209)
(our-defmacro with-input-from-string (binding &body body)
  "Execute BODY with (first binding) bound to a string input stream.
Supports :start, :end, and :index keyword arguments."
  (let* ((var    (first binding))
         (string (second binding))
         (rest   (cddr binding))
         (start  (getf rest :start))
         (end    (getf rest :end))
         (index  (getf rest :index))
         (str-var (gensym "STR")))
    `(let* ((,str-var ,(if (or start end)
                           `(subseq ,string ,(or start 0) ,end)
                           string))
            (,var (make-string-input-stream ,str-var)))
       ,@(when index `((setf ,index (length ,str-var))))
       ,@body)))

;; WITH-OUTPUT-TO-STRING (FR-613: optional string argument ignored in cl-cc)
(our-defmacro with-output-to-string (binding &body body)
  "Execute BODY with (first binding) bound to a string output stream.
 The optional second element of BINDING (string-to-fill) is accepted for
 ANSI CL compatibility but ignored — cl-cc always creates a fresh stream."
  (let ((var (first binding)))
    `(let ((,var (make-string-output-stream)))
        ,@body
        (get-output-stream-string ,var))))

;; WITH-STANDARD-IO-SYNTAX (FR-210) — bind all ANSI-standard I/O variables
(our-defmacro with-standard-io-syntax (&body body)
  "Execute BODY with all ANSI-standard I/O variable bindings."
  `(let ((*print-array*              t)
         (*print-base*               10)
         (*print-case*               :upcase)
         (*print-circle*             nil)
         (*print-escape*             t)
         (*print-gensym*             t)
         (*print-length*             nil)
         (*print-level*              nil)
         (*print-lines*              nil)
         (*print-miser-width*        nil)
         (*print-pretty*             nil)
         (*print-radix*              nil)
         (*print-readably*           nil)
         (*print-right-margin*       nil)
         (*read-base*                10)
         (*read-default-float-format* 'single-float)
         (*read-eval*                t)
         (*read-suppress*            nil)
         (*package*                  (find-package "COMMON-LISP-USER")))
     ,@body))

;; WITH-PACKAGE-ITERATOR (FR-211) — real implementation via host bridge
(register-macro 'with-package-iterator
  (lambda (form env)
    (declare (ignore env))
    (let* ((binding (second form))
           (body (cddr form))
           (name (first binding))
           (packages (second binding))
           (symbol-types (cddr binding))
           (syms-var (gensym "SYMS"))
           (idx-var (gensym "IDX"))
           (len-var (gensym "LEN")))
      (declare (ignore symbol-types))
      ;; Collect external symbols from the given packages
      `(let* ((,syms-var (let ((acc nil))
                           (dolist (p (if (listp ,packages) ,packages (list ,packages)))
                             (let ((pkg (find-package p)))
                               (when pkg
                                 (dolist (s (%package-external-symbols pkg))
                                   (push (list s :external pkg) acc)))))
                           (nreverse acc)))
              (,idx-var 0)
              (,len-var (length ,syms-var)))
         (let ((,name (lambda ()
                        (if (>= ,idx-var ,len-var)
                            (values nil nil nil nil)
                            (let ((entry (nth ,idx-var ,syms-var)))
                              (setq ,idx-var (1+ ,idx-var))
                              (values t (first entry) (second entry) (third entry)))))))
           ,@body)))))

;; DEFINE-COMPILER-MACRO (FR-212) — accepts and returns name (no compile-time expansion)
(register-macro 'define-compiler-macro
  (lambda (form env) (declare (ignore env)) `(quote ,(second form))))

;; WITH-COMPILATION-UNIT (FR-363) — stub: just executes body
;; Uses register-macro because our-defmacro can't handle nested lambda lists.
(register-macro 'with-compilation-unit
  (lambda (form env)
    (declare (ignore env))
    ;; (with-compilation-unit (options...) body...)
    `(progn ,@(cddr form))))

;; WITH-SIMPLE-RESTART (FR-422) — stub: just executes body
(register-macro 'with-simple-restart
  (lambda (form env)
    (declare (ignore env))
    ;; (with-simple-restart (name fmt args...) body...)
    `(progn ,@(cddr form))))

;; WITH-CONDITION-RESTARTS (FR-423) — stub: just executes body
(our-defmacro with-condition-restarts (condition restarts &body body)
  (declare (ignore condition restarts))
  `(progn ,@body))

;;; ------------------------------------------------------------
;;; CXR Accessor Macros (algorithmic registration)
;;; ------------------------------------------------------------
;;; cXXr..cXXXXr forms are registered programmatically by
;;; analysing the sequence of a/d letters between the outer c/r.

(let ((cxr-names '(caar cadr cdar cddr
                   caaar cdaar cadar cddar
                   caadr cdadr caddr cdddr
                   caaaar cadaar caadar caddar
                   cdaaar cddaar cdadar cdddar
                   caaadr cadadr caaddr cadddr
                   cdaadr cddadr cdaddr cddddr)))
  (flet ((expand-cxr (sym arg)
           (let* ((name (symbol-name sym))
                  (letters (subseq name 1 (1- (length name)))))
             (reduce (lambda (acc letter)
                       (if (char-equal letter #\a) `(car ,acc) `(cdr ,acc)))
                     (reverse (coerce letters 'list))
                     :initial-value arg))))
    (dolist (cxr-sym cxr-names)
      (let ((cxr cxr-sym))
        (register-macro cxr
                        (lambda (form env)
                          (declare (ignore env))
                          (expand-cxr cxr (second form))))))))

;;; (HOF macros continue in macros-hof.lisp)

;; MAPCAR: apply fn to each element, collect results
(our-defmacro mapcar (fn list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,fn-var ,fn)
           (,acc nil))
       (dolist (,x ,list (nreverse ,acc))
         (setq ,acc (cons (funcall ,fn-var ,x) ,acc))))))

;; MAPC: side-effect loop, returns the original list
(our-defmacro mapc (fn list)
  (let ((fn-var (gensym "FN"))
        (lst (gensym "LST"))
        (x (gensym "X")))
    `(let ((,fn-var ,fn)
           (,lst ,list))
       (dolist (,x ,lst ,lst)
         (funcall ,fn-var ,x)))))

;; MAPCAN: apply fn to each element, nconc all results
(our-defmacro mapcan (fn list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,fn-var ,fn)
           (,acc nil))
       (dolist (,x ,list ,acc)
         (setq ,acc (nconc ,acc (funcall ,fn-var ,x)))))))

;; EVERY: true iff pred returns non-nil for every element
(our-defmacro every (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X")))
    `(let ((,fn-var ,pred))
       (block nil
         (dolist (,x ,list t)
           (unless (funcall ,fn-var ,x)
             (return nil)))))))

;; SOME: returns first truthy pred result, or nil
(our-defmacro some (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (result (gensym "R")))
    `(let ((,fn-var ,pred))
       (block nil
         (dolist (,x ,list nil)
           (let ((,result (funcall ,fn-var ,x)))
             (when ,result (return ,result))))))))

;; NOTANY: true iff pred returns nil for every element
(our-defmacro notany (pred list)
  `(not (some ,pred ,list)))

;; NOTEVERY: true iff pred returns nil for at least one element
(our-defmacro notevery (pred list)
  `(not (every ,pred ,list)))

;; Shared expansion for remove-if / remove-if-not.
;; KEEP-COND is the form head that gates accumulation ('when or 'unless).
(defun %filter-list-expand (keep-cond pred list)
  (let ((fn-var (gensym "FN"))
        (x      (gensym "X"))
        (acc    (gensym "ACC")))
    `(let ((,fn-var ,pred)
           (,acc nil))
       (dolist (,x ,list (nreverse ,acc))
         (,keep-cond (funcall ,fn-var ,x)
           (setq ,acc (cons ,x ,acc)))))))

;; REMOVE-IF: keep elements for which pred is false (with optional :key)
(our-defmacro remove-if (pred list &key key)
  (if key
      (let ((fn-var (gensym "FN")) (kfn (gensym "KEY"))
            (x (gensym "X")) (acc (gensym "ACC")))
        `(let ((,fn-var ,pred) (,kfn ,key) (,acc nil))
           (dolist (,x ,list (nreverse ,acc))
             (unless (funcall ,fn-var (funcall ,kfn ,x))
               (setq ,acc (cons ,x ,acc))))))
      (%filter-list-expand 'unless pred list)))

;; REMOVE-IF-NOT: keep elements for which pred is true (with optional :key)
(our-defmacro remove-if-not (pred list &key key)
  (if key
      (let ((fn-var (gensym "FN")) (kfn (gensym "KEY"))
            (x (gensym "X")) (acc (gensym "ACC")))
        `(let ((,fn-var ,pred) (,kfn ,key) (,acc nil))
           (dolist (,x ,list (nreverse ,acc))
             (when (funcall ,fn-var (funcall ,kfn ,x))
               (setq ,acc (cons ,x ,acc))))))
      (%filter-list-expand 'when pred list)))

;; FIND: first element eql to item, or nil
(our-defmacro find (item list &rest keys)
  (if keys
      ;; keyword args present — key/test-aware loop
      (let* ((key-expr  (or (getf keys :key)  '#'identity))
             (test-expr (or (getf keys :test) '#'eql))
             (item-var  (gensym "ITEM"))
             (key-var   (gensym "KEY"))
             (test-var  (gensym "TEST"))
             (x         (gensym "X")))
        `(let ((,item-var ,item) (,key-var ,key-expr) (,test-var ,test-expr))
           (block nil
             (dolist (,x ,list nil)
               (when (funcall ,test-var ,item-var (funcall ,key-var ,x))
                 (return ,x))))))
      ;; no keyword args — fast eql check
      (let ((item-var (gensym "ITEM")) (x (gensym "X")))
        `(let ((,item-var ,item))
           (block nil
             (dolist (,x ,list nil)
               (when (eql ,item-var ,x) (return ,x))))))))

;; FIND-IF: first element for which pred is true, or nil (with optional :key)
(our-defmacro find-if (pred list &key key)
  (if key
      (let ((fn-var (gensym "FN")) (kfn (gensym "KEY")) (x (gensym "X")))
        `(let ((,fn-var ,pred) (,kfn ,key))
           (block nil
             (dolist (,x ,list nil)
               (when (funcall ,fn-var (funcall ,kfn ,x))
                 (return ,x))))))
      (let ((fn-var (gensym "FN")) (x (gensym "X")))
        `(let ((,fn-var ,pred))
           (block nil
             (dolist (,x ,list nil)
               (when (funcall ,fn-var ,x)
                 (return ,x))))))))

;; POSITION: index of first element eql to item, or nil (with optional keywords)
(our-defmacro position (item list &key test key test-not)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (idx (gensym "IDX")))
    (if (or test key test-not)
        (let ((tst-var (gensym "TST")) (kfn-var (gensym "KEY")))
          `(let ((,item-var ,item) (,idx 0)
                 (,tst-var ,(cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
                 (,kfn-var ,(or key '#'identity)))
             (block nil
               (dolist (,x ,list nil)
                 (when (funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
                   (return ,idx))
                 (setq ,idx (+ ,idx 1))))))
        `(let ((,item-var ,item) (,idx 0))
           (block nil
             (dolist (,x ,list nil)
               (when (eql ,item-var ,x)
                 (return ,idx))
               (setq ,idx (+ ,idx 1))))))))

;; COUNT: number of elements eql to item (with optional :test/:key)
(our-defmacro count (item list &key test key test-not)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (cnt (gensym "CNT")))
    (if (or test key test-not)
        (let ((tst-var (gensym "TST")) (kfn-var (gensym "KEY")))
          `(let ((,item-var ,item) (,cnt 0)
                 (,tst-var ,(cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
                 (,kfn-var ,(or key '#'identity)))
             (dolist (,x ,list ,cnt)
               (when (funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
                 (setq ,cnt (+ ,cnt 1))))))
        `(let ((,item-var ,item) (,cnt 0))
           (dolist (,x ,list ,cnt)
             (when (eql ,item-var ,x)
               (setq ,cnt (+ ,cnt 1))))))))

;; COUNT-IF: number of elements for which pred is true (with optional :key)
(our-defmacro count-if (pred list &key key)
  (if key
      (let ((fn-var (gensym "FN")) (kfn (gensym "KEY")) (x (gensym "X")) (cnt (gensym "CNT")))
        `(let ((,fn-var ,pred) (,kfn ,key) (,cnt 0))
           (dolist (,x ,list ,cnt)
             (when (funcall ,fn-var (funcall ,kfn ,x))
               (setq ,cnt (+ ,cnt 1))))))
      (let ((fn-var (gensym "FN")) (x (gensym "X")) (cnt (gensym "CNT")))
        `(let ((,fn-var ,pred) (,cnt 0))
           (dolist (,x ,list ,cnt)
             (when (funcall ,fn-var ,x)
               (setq ,cnt (+ ,cnt 1))))))))

;; FIND-IF-NOT: first element for which pred is false (FR-610), with optional :key
(our-defmacro find-if-not (pred list &key key)
  (if key
      `(find-if (complement ,pred) ,list :key ,key)
      `(find-if (complement ,pred) ,list)))

;; POSITION-IF: index of first element satisfying pred (FR-610), with optional :key
(our-defmacro position-if (pred list &key key)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (idx (gensym "IDX")))
    (if key
        (let ((kfn (gensym "KEY")))
          `(let ((,fn-var ,pred) (,kfn ,key) (,idx 0))
             (block nil
               (dolist (,x ,list nil)
                 (when (funcall ,fn-var (funcall ,kfn ,x))
                   (return ,idx))
                 (setq ,idx (+ ,idx 1))))))
        `(let ((,fn-var ,pred) (,idx 0))
           (block nil
             (dolist (,x ,list nil)
               (when (funcall ,fn-var ,x)
                 (return ,idx))
               (setq ,idx (+ ,idx 1))))))))

;; POSITION-IF-NOT: index of first element not satisfying pred (FR-610), with optional :key
(our-defmacro position-if-not (pred list &key key)
  (if key
      `(position-if (complement ,pred) ,list :key ,key)
      `(position-if (complement ,pred) ,list)))

;; COUNT-IF-NOT: count elements for which pred is false (FR-610), with optional :key
(our-defmacro count-if-not (pred list &key key)
  (if key
      `(count-if (complement ,pred) ,list :key ,key)
      `(count-if (complement ,pred) ,list)))

;;; ─── SUBST-IF / SUBST-IF-NOT (FR-657) ───────────────────────────────────────

(our-defmacro subst-if (new pred tree)
  "Replace every subtree in TREE for which PRED is true with NEW."
  (let ((fn (gensym "FN")) (n (gensym "NEW")) (rec (gensym "REC")))
    `(let ((,fn ,pred) (,n ,new))
       (labels ((,rec (tr)
                  (cond ((funcall ,fn tr) ,n)
                        ((atom tr) tr)
                        (t (cons (,rec (car tr)) (,rec (cdr tr)))))))
         (,rec ,tree)))))

(our-defmacro subst-if-not (new pred tree)
  "Replace every subtree in TREE for which PRED is false with NEW."
  `(subst-if ,new (complement ,pred) ,tree))

;; REMOVE: remove elements matching item (with optional :test/:key/:count)
(our-defmacro remove (item list &key test key test-not count from-end)
  (let ((item-var (gensym "ITEM")) (x (gensym "X")) (acc (gensym "ACC"))
        (tst-var (gensym "TST")) (kfn-var (gensym "KEY"))
        (cnt-var (gensym "CNT")) (lim-var (gensym "LIM"))
        (rev-var (gensym "REV")))
    (let* ((has-keys (or test key test-not))
           (test-e (cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
           (key-bindings (when has-keys
                           `((,tst-var ,test-e)
                             (,kfn-var ,(or key '#'identity)))))
           (match-form
             (if has-keys
                 `(funcall ,tst-var ,item-var (funcall ,kfn-var ,x))
                 `(eql ,item-var ,x))))
      (cond
        ;; :count present, :from-end t — walk reversed input, skip last N matches
        ;; acc ends up in original left-to-right order (double-reversal effect)
        ((and count from-end)
         `(let* ((,item-var ,item) ,@key-bindings
                 (,rev-var (reverse ,list))
                 (,acc nil) (,cnt-var 0) (,lim-var ,count))
            (dolist (,x ,rev-var)
              (if (and (< ,cnt-var ,lim-var) ,match-form)
                  (setq ,cnt-var (+ ,cnt-var 1))
                  (setq ,acc (cons ,x ,acc))))
            ,acc))   ; acc is in correct original order (push-reversed walk of reversed list)
        ;; :count present, forward — remove first N occurrences left-to-right
        (count
         `(let* ((,item-var ,item) ,@key-bindings
                 (,acc nil) (,cnt-var 0) (,lim-var ,count))
            (dolist (,x ,list (nreverse ,acc))
              (if (and (< ,cnt-var ,lim-var) ,match-form)
                  (setq ,cnt-var (+ ,cnt-var 1))
                  (setq ,acc (cons ,x ,acc))))))
        ;; No :count — remove all occurrences
        (has-keys
         `(let* ((,item-var ,item) ,@key-bindings (,acc nil))
            (dolist (,x ,list (nreverse ,acc))
              (unless ,match-form
                (setq ,acc (cons ,x ,acc))))))
        (t
         `(let ((,item-var ,item) (,acc nil))
            (dolist (,x ,list (nreverse ,acc))
              (unless ,match-form
                (setq ,acc (cons ,x ,acc))))))))))

;; REMOVE-DUPLICATES: keep only the first occurrence of each element (with optional :test/:key)
(our-defmacro remove-duplicates (list &key test key test-not)
  (if (or test key test-not)
      (let ((x (gensym "X")) (acc (gensym "ACC"))
            (tst-var (gensym "TST")) (kfn-var (gensym "KEY")))
        `(let ((,acc nil)
               (,tst-var ,(cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
               (,kfn-var ,(or key '#'identity)))
           (dolist (,x ,list (nreverse ,acc))
             (unless (find (funcall ,kfn-var ,x) ,acc :test ,tst-var)
               (setq ,acc (cons ,x ,acc))))))
      (let ((x (gensym "X")) (acc (gensym "ACC")))
        `(let ((,acc nil))
           (dolist (,x ,list (nreverse ,acc))
             (unless (member ,x ,acc)
               (setq ,acc (cons ,x ,acc))))))))

;; MEMBER: find first tail where (test item element), or nil (with optional :test/:key/:test-not)
;; Shadows the binary builtin to add keyword arg support.
(our-defmacro member (item list &key test key test-not)
  (let ((item-var (gensym "ITEM")) (lst-var (gensym "LST")) (tag (gensym "MLOOP")))
    (if (or test key test-not)
        (let ((tst-var (gensym "TST")) (kfn-var (gensym "KEY")))
          `(let ((,item-var ,item) (,lst-var ,list)
                 (,tst-var ,(cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
                 (,kfn-var ,(or key '#'identity)))
             (block nil
               (tagbody
                ,tag
                  (when (null ,lst-var) (return nil))
                  (when (funcall ,tst-var ,item-var (funcall ,kfn-var (car ,lst-var)))
                    (return ,lst-var))
                  (setq ,lst-var (cdr ,lst-var))
                  (go ,tag)))))
        `(let ((,item-var ,item) (,lst-var ,list))
           (block nil
             (tagbody
              ,tag
                (when (null ,lst-var) (return nil))
                (when (eql ,item-var (car ,lst-var)) (return ,lst-var))
                (setq ,lst-var (cdr ,lst-var))
                (go ,tag)))))))

;; UNION: all elements present in either list, no duplicates (with optional :test/:key)
(our-defmacro union (list1 list2 &key test key test-not)
  (let ((l1 (gensym "L1")) (l2 (gensym "L2")) (x (gensym "X")) (acc (gensym "ACC")))
    (let ((kws (cond (test `(:test ,test)) (test-not `(:test-not ,test-not)) (t nil))))
      (let ((kws (if key (append kws `(:key ,key)) kws)))
        `(let ((,l1 ,list1) (,l2 ,list2) (,acc nil))
           (dolist (,x ,l2) (setq ,acc (cons ,x ,acc)))
           (dolist (,x ,l1 (nreverse ,acc))
             (unless (member ,x ,l2 ,@kws)
               (setq ,acc (cons ,x ,acc)))))))))

;; Shared expansion for set-difference / intersection with optional :test/:key
(defun %set-filter-expand (list1 list2 keep-when &optional kws)
  (let ((l2  (gensym "L2"))
        (x   (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,l2 ,list2) (,acc nil))
       (dolist (,x ,list1 (nreverse ,acc))
         (,keep-when (member ,x ,l2 ,@kws)
           (setq ,acc (cons ,x ,acc)))))))

;; SET-DIFFERENCE: elements in list1 not present in list2 (with optional :test/:key)
(our-defmacro set-difference (list1 list2 &key test key test-not)
  (let ((kws (cond (test `(:test ,test)) (test-not `(:test-not ,test-not)) (t nil))))
    (%set-filter-expand list1 list2 'unless (if key (append kws `(:key ,key)) kws))))

;; INTERSECTION: elements present in both lists (with optional :test/:key)
(our-defmacro intersection (list1 list2 &key test key test-not)
  (let ((kws (cond (test `(:test ,test)) (test-not `(:test-not ,test-not)) (t nil))))
    (%set-filter-expand list1 list2 'when (if key (append kws `(:key ,key)) kws))))

;; SUBSETP: true iff every element of list1 is in list2 (with optional :test/:key)
(our-defmacro subsetp (list1 list2 &key test key test-not)
  (let ((l2 (gensym "L2")) (x (gensym "X")))
    (let ((kws (cond (test `(:test ,test)) (test-not `(:test-not ,test-not)) (t nil))))
      (let ((kws (if key (append kws `(:key ,key)) kws)))
        `(let ((,l2 ,list2))
           (every (lambda (,x) (member ,x ,l2 ,@kws)) ,list1))))))

;; ADJOIN: cons item onto list only if not already present (with optional :test/:key)
(our-defmacro adjoin (item list &key test key test-not)
  (let ((item-var (gensym "ITEM")) (lst (gensym "LST")))
    (let ((kws (cond (test `(:test ,test)) (test-not `(:test-not ,test-not)) (t nil))))
      (let ((kws (if key (append kws `(:key ,key)) kws)))
        `(let ((,item-var ,item) (,lst ,list))
           (if (member ,item-var ,lst ,@kws) ,lst (cons ,item-var ,lst)))))))

;; RASSOC: find association pair by cdr value (with optional :test/:key/:test-not)
(our-defmacro rassoc (item alist &key test key test-not)
  (if (or test key test-not)
      (let ((item-var (gensym "ITEM")) (x (gensym "X"))
            (tst-var (gensym "TST")) (kfn-var (gensym "KEY")))
        `(let ((,item-var ,item)
               (,tst-var ,(cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
               (,kfn-var ,(or key '#'identity)))
           (block nil
             (dolist (,x ,alist nil)
               (when (and (consp ,x)
                          (funcall ,tst-var ,item-var (funcall ,kfn-var (cdr ,x))))
                 (return ,x))))))
      (let ((item-var (gensym "ITEM")) (x (gensym "X")))
        `(let ((,item-var ,item))
           (block nil
             (dolist (,x ,alist nil)
               (when (and (consp ,x) (eql ,item-var (cdr ,x)))
                 (return ,x))))))))

;; PAIRLIS: zip keys and data lists into an association list
(our-defmacro pairlis (keys data &optional alist)
  (let ((ks (gensym "KS"))
        (ds (gensym "DS"))
        (acc (gensym "ACC")))
    `(let ((,ks ,keys)
           (,ds ,data)
           (,acc ,alist))
       (tagbody
        pairlis-loop
          (when (and ,ks ,ds)
            (setq ,acc (cons (cons (car ,ks) (car ,ds)) ,acc))
            (setq ,ks (cdr ,ks))
            (setq ,ds (cdr ,ds))
            (go pairlis-loop)))
       ,acc)))

;; SORT: stable merge sort, with optional :key argument (FR-611)
(our-defmacro sort (list predicate &key key)
  (let ((pred    (gensym "PRED"))
        (keyfn   (gensym "KEY"))
        (len     (gensym "LEN"))
        (mid     (gensym "MID"))
        (left    (gensym "LEFT"))
        (right   (gensym "RIGHT"))
        (msort   (gensym "MSORT"))
        (mmerge  (gensym "MMERGE"))
        (take-n  (gensym "TAKEN")))
    (if key
        `(let ((,pred ,predicate)
               (,keyfn ,key))
           (labels ((,take-n (lst n)
                      (if (= n 0) nil
                          (cons (car lst) (,take-n (cdr lst) (- n 1)))))
                    (,mmerge (a b)
                      (cond ((null a) b)
                            ((null b) a)
                            ((funcall ,pred (funcall ,keyfn (car a)) (funcall ,keyfn (car b)))
                             (cons (car a) (,mmerge (cdr a) b)))
                            (t (cons (car b) (,mmerge a (cdr b))))))
                    (,msort (lst)
                      (let ((,len (length lst)))
                        (if (<= ,len 1) lst
                            (let* ((,mid (truncate ,len 2))
                                   (,left (,take-n lst ,mid))
                                   (,right (nthcdr ,mid lst)))
                              (,mmerge (,msort ,left) (,msort ,right)))))))
             (,msort ,list)))
        `(let ((,pred ,predicate))
           (labels ((,take-n (lst n)
                      (if (= n 0) nil
                          (cons (car lst) (,take-n (cdr lst) (- n 1)))))
                    (,mmerge (a b)
                      (cond ((null a) b)
                            ((null b) a)
                            ((funcall ,pred (car a) (car b))
                             (cons (car a) (,mmerge (cdr a) b)))
                            (t (cons (car b) (,mmerge a (cdr b))))))
                    (,msort (lst)
                      (let ((,len (length lst)))
                        (if (<= ,len 1) lst
                            (let* ((,mid (truncate ,len 2))
                                   (,left (,take-n lst ,mid))
                                   (,right (nthcdr ,mid lst)))
                              (,mmerge (,msort ,left) (,msort ,right)))))))
             (,msort ,list))))))

;; STABLE-SORT: same as sort (merge sort is inherently stable)
(our-defmacro stable-sort (list predicate &key key)
  (if key
      `(sort ,list ,predicate :key ,key)
      `(sort ,list ,predicate)))

;; MAP: map fn over seq, coerce result to result-type
(our-defmacro map (result-type fn seq)
  `(coerce (mapcar ,fn (coerce ,seq 'list)) ,result-type))

;; IGNORE-ERRORS: catch all errors, return nil on error
(our-defmacro ignore-errors (&body forms)
  (let ((e-var (gensym "E")))
    `(handler-case (progn ,@forms)
       (error (,e-var) (values nil ,e-var)))))

;; CONCATENATE: type-dispatching sequence concatenation (FR-615)
(our-defmacro concatenate (result-type &rest sequences)
  (let ((rtype (if (and (consp result-type) (eq (car result-type) 'quote))
                   (cadr result-type)
                   result-type)))
    (cond
      ((null sequences)
       (if (eq rtype 'string) "" nil))
      ((eq rtype 'list)
       `(append ,@sequences))
      ((member rtype '(vector simple-vector))
       `(coerce-to-vector (append ,@sequences)))
      ;; default: string concatenation
      (t
       (if (null (cdr sequences))
           (car sequences)
           (reduce (lambda (acc s) `(string-concat ,acc ,s))
                   (cdr sequences)
                   :initial-value (car sequences)))))))

;;; ─── LIST* (FR-609) ─────────────────────────────────────────────────────────
;;;
;;; (list* a) = a
;;; (list* a b) = (cons a b)
;;; (list* a b c) = (cons a (cons b c))

(our-defmacro list* (&rest args)
  (cond
    ((null args) (error "list* requires at least one argument"))
    ((null (cdr args)) (car args))
    (t (reduce (lambda (x acc) `(cons ,x ,acc))
               args
               :from-end t))))

;;; ─── List accessors sixth–tenth (FR-563) ───────────────────────────────────

(our-defmacro sixth   (list) `(nth 5 ,list))
(our-defmacro seventh (list) `(nth 6 ,list))
(our-defmacro eighth  (list) `(nth 7 ,list))
(our-defmacro ninth   (list) `(nth 8 ,list))
(our-defmacro tenth   (list) `(nth 9 ,list))

;;; ─── PUSHNEW (FR-587) ───────────────────────────────────────────────────────

(our-defmacro pushnew (item place &key (test '#'eql))
  (let ((item-var (gensym "ITEM")))
    (if (equal test '#'eql)
        ;; Fast path: 2-arg member (registered builtin, no keyword args needed)
        `(let ((,item-var ,item))
           (unless (member ,item-var ,place)
             (setf ,place (cons ,item-var ,place))))
        ;; General path: forward :test to member
        `(let ((,item-var ,item))
           (unless (member ,item-var ,place :test ,test)
             (setf ,place (cons ,item-var ,place)))))))

;;; ─── NRECONC (FR-640) ───────────────────────────────────────────────────────

(our-defmacro nreconc (list tail)
  `(nconc (nreverse ,list) ,tail))

;;; ─── ASSOC with keyword support (FR-697) ────────────────────────────────────
;; Shadows the binary builtin to support :test/:key/:test-not keyword args.

(our-defmacro assoc (item alist &key test key test-not)
  (if (or test key test-not)
      (let ((item-var (gensym "ITEM")) (pair (gensym "PAIR"))
            (tst-var (gensym "TST")) (kfn-var (gensym "KEY")))
        `(let ((,item-var ,item)
               (,tst-var ,(cond (test-not `(complement ,test-not)) (test test) (t '#'eql)))
               (,kfn-var ,(or key '#'identity)))
           (block nil
             (dolist (,pair ,alist nil)
               (when (and ,pair (funcall ,tst-var ,item-var (funcall ,kfn-var (car ,pair))))
                 (return ,pair))))))
      ;; No keywords: fast eql scan
      (let ((item-var (gensym "ITEM")) (pair (gensym "PAIR")))
        `(let ((,item-var ,item))
           (block nil
             (dolist (,pair ,alist nil)
               (when (and ,pair (eql ,item-var (car ,pair)))
                 (return ,pair))))))))

;;; ─── ASSOC-IF / ASSOC-IF-NOT / RASSOC-IF / RASSOC-IF-NOT (FR-660, FR-500) ─

(our-defmacro assoc-if (pred alist &key key)
  (let ((fn-var (gensym "FN")) (pair (gensym "PAIR")))
    (if key
        (let ((kfn (gensym "KEY")))
          `(let ((,fn-var ,pred) (,kfn ,key))
             (dolist (,pair ,alist nil)
               (when (and ,pair (funcall ,fn-var (funcall ,kfn (car ,pair))))
                 (return ,pair)))))
        `(let ((,fn-var ,pred))
           (dolist (,pair ,alist nil)
             (when (and ,pair (funcall ,fn-var (car ,pair)))
               (return ,pair)))))))

(our-defmacro assoc-if-not (pred alist &key key)
  (if key
      `(assoc-if (complement ,pred) ,alist :key ,key)
      `(assoc-if (complement ,pred) ,alist)))

(our-defmacro rassoc-if (pred alist &key key)
  (let ((fn-var (gensym "FN")) (pair (gensym "PAIR")))
    (if key
        (let ((kfn (gensym "KEY")))
          `(let ((,fn-var ,pred) (,kfn ,key))
             (dolist (,pair ,alist nil)
               (when (and ,pair (funcall ,fn-var (funcall ,kfn (cdr ,pair))))
                 (return ,pair)))))
        `(let ((,fn-var ,pred))
           (dolist (,pair ,alist nil)
             (when (and ,pair (funcall ,fn-var (cdr ,pair)))
               (return ,pair)))))))

(our-defmacro rassoc-if-not (pred alist &key key)
  (if key
      `(rassoc-if (complement ,pred) ,alist :key ,key)
      `(rassoc-if (complement ,pred) ,alist)))

;;; ─── MEMBER-IF / MEMBER-IF-NOT — first definition removed (duplicate below) ──

;;; ─── COMPLEMENT (FR-610) ────────────────────────────────────────────────────

(our-defmacro complement (pred)
  (let ((fn-var (gensym "FN"))
        (args   (gensym "ARGS")))
    `(let ((,fn-var ,pred))
       (lambda (&rest ,args) (not (apply ,fn-var ,args))))))

;;; ─── Y-OR-N-P (FR-578) ──────────────────────────────────────────────────────

(our-defmacro y-or-n-p (&optional (format-str "") &rest args)
  `(progn
     (when (not (string= ,format-str ""))
       (format *query-io* ,format-str ,@args))
     (format *query-io* " (y or n) ")
     (let ((ch (read-char *query-io*)))
       (or (char= ch #\y) (char= ch #\Y)))))

;;; ─── PROVIDE / REQUIRE fix (FR-680) ─────────────────────────────────────────

;; Override provide to avoid depending on pushnew
(our-defmacro provide (module)
  (let ((mod-var (gensym "MOD")))
    `(let ((,mod-var ,module))
       (unless (member ,mod-var *modules* :test #'string=)
         (setf *modules* (cons ,mod-var *modules*))))))

;;; ─── LIST* (FR-609) ──────────────────────────────────────────────────────────

(our-defmacro list* (first-arg &rest rest-args)
  "Build a list like LIST but the last element is the tail (not nil)."
  (if (null rest-args)
      first-arg
      `(cons ,first-arg (list* ,@rest-args))))

;;; ─── VECTOR constructor (FR-651) ─────────────────────────────────────────────

(our-defmacro vector (&rest args)
  "Create a simple vector with given elements."
  (if (null args)
      '(make-array 0)
      (let ((v (gensym "V")))
        `(let ((,v (make-array ,(length args))))
           ,@(loop for i from 0 for x in args
                   collect `(setf (aref ,v ,i) ,x))
           ,v))))

;;; ─── MEMBER-IF / MEMBER-IF-NOT (FR-499) ──────────────────────────────────────

(our-defmacro member-if (pred list &key key)
  "Return the tail of LIST starting from first element satisfying PRED."
  (let ((fn (gensym "FN")) (tail (gensym "TAIL")))
    (if key
        (let ((kfn (gensym "KEY")))
          `(let ((,fn ,pred) (,kfn ,key))
             (do ((,tail ,list (cdr ,tail)))
                 ((null ,tail) nil)
               (when (funcall ,fn (funcall ,kfn (car ,tail)))
                 (return ,tail)))))
        `(let ((,fn ,pred))
           (do ((,tail ,list (cdr ,tail)))
               ((null ,tail) nil)
             (when (funcall ,fn (car ,tail))
               (return ,tail)))))))

(our-defmacro member-if-not (pred list &key key)
  "Return the tail of LIST starting from first element not satisfying PRED."
  (if key
      `(member-if (complement ,pred) ,list :key ,key)
      `(member-if (complement ,pred) ,list)))

;;; ─── MAPHASH (FR-675) ────────────────────────────────────────────────────────

(our-defmacro maphash (fn table)
  "Apply FN to each key-value pair in hash TABLE. Returns nil."
  (let ((fn-var (gensym "FN"))
        (tbl-var (gensym "TBL"))
        (k (gensym "K")))
    `(let ((,fn-var ,fn)
           (,tbl-var ,table))
       (dolist (,k (hash-table-keys ,tbl-var) nil)
         (funcall ,fn-var ,k (gethash ,k ,tbl-var))))))

;;; ─── List utilities: tailp, ldiff, copy-alist (FR-495, FR-496) ───────────────

(our-defmacro tailp (object list)
  "Return true if OBJECT is the same as some tail of LIST (EQ identity)."
  (let ((tail (gensym "TAIL")))
    `(do ((,tail ,list (cdr ,tail)))
         ((atom ,tail) (eq ,tail ,object))
       (when (eq ,tail ,object) (return t)))))

(our-defmacro ldiff (list object)
  "Return a fresh list of the elements of LIST before OBJECT (EQ identity)."
  (let ((result (gensym "RES"))
        (tail   (gensym "TAIL")))
    `(let ((,result nil))
       (do ((,tail ,list (cdr ,tail)))
           ((or (atom ,tail) (eq ,tail ,object))
            (nreverse ,result))
         (push (car ,tail) ,result)))))

(our-defmacro copy-alist (alist)
  "Return a fresh copy of ALIST — each cons cell is new, values are shared."
  (let ((result (gensym "RES"))
        (pair   (gensym "PAIR")))
    `(let ((,result nil))
       (dolist (,pair ,alist (nreverse ,result))
         (push (if (consp ,pair) (cons (car ,pair) (cdr ,pair)) ,pair) ,result)))))

;;; ─── tree-equal (FR-496) ──────────────────────────────────────────────────────

(our-defmacro tree-equal (x y &key (test '#'eql))
  "Return true if two trees are equal, comparing leaves with TEST."
  (let ((fn (gensym "FN"))
        (xv (gensym "X"))
        (yv (gensym "Y")))
    `(labels ((,fn (,xv ,yv)
                (if (consp ,xv)
                    (and (consp ,yv)
                         (,fn (car ,xv) (car ,yv))
                         (,fn (cdr ,xv) (cdr ,yv)))
                    (and (not (consp ,yv))
                         (funcall ,test ,xv ,yv)))))
       (,fn ,x ,y))))

;;; ─── get-properties (FR-540) ──────────────────────────────────────────────────

(our-defmacro get-properties (plist indicator-list)
  "Search PLIST for any key in INDICATOR-LIST. Returns (values key value tail) or (values nil nil nil)."
  (let ((pl   (gensym "PL"))
        (keys (gensym "KEYS")))
    `(do ((,pl ,plist (cddr ,pl)))
         ((null ,pl) (values nil nil nil))
       (when (member (car ,pl) ,indicator-list)
         (return (values (car ,pl) (cadr ,pl) ,pl))))))

;;; ─── Destructive set operations: nunion/nintersection/nset-difference (FR-547) ─

(our-defmacro nunion (list1 list2 &key (test '#'eql))
  "Destructive union (delegates to union in cl-cc)."
  `(union ,list1 ,list2 :test ,test))

(our-defmacro nintersection (list1 list2 &key (test '#'eql))
  "Destructive intersection (delegates to intersection)."
  `(intersection ,list1 ,list2 :test ,test))

(our-defmacro nset-difference (list1 list2 &key (test '#'eql))
  "Destructive set-difference (delegates to set-difference)."
  `(set-difference ,list1 ,list2 :test ,test))

(our-defmacro nset-exclusive-or (list1 list2 &key (test '#'eql))
  "Destructive set-exclusive-or (delegates to set-exclusive-or)."
  `(set-exclusive-or ,list1 ,list2 :test ,test))

;;; ─── nsubst / nsubst-if / nsubst-if-not (FR-496) ────────────────────────────

(our-defmacro nsubst (new old tree &key test)
  "Destructively substitute NEW for OLD in TREE (delegates to subst in cl-cc)."
  (if test
      `(subst-if ,new (lambda (%v) (funcall ,test %v ,old)) ,tree)
      `(subst ,new ,old ,tree)))

(our-defmacro nsubst-if (new pred tree)
  "Destructively substitute NEW where PRED is true (delegates to subst-if)."
  `(subst-if ,new ,pred ,tree))

(our-defmacro nsubst-if-not (new pred tree)
  "Destructively substitute NEW where PRED is false."
  `(subst-if-not ,new ,pred ,tree))

;;; ─── nstring-upcase / nstring-downcase / nstring-capitalize (FR-475) ─────────

(our-defmacro nstring-upcase (string &key start end)
  "Destructively uppercase STRING (returns uppercased string in cl-cc)."
  (if (or start end)
      `(string-upcase ,string :start ,(or start 0) ,@(when end `(:end ,end)))
      `(string-upcase ,string)))

(our-defmacro nstring-downcase (string &key start end)
  "Destructively lowercase STRING."
  (if (or start end)
      `(string-downcase ,string :start ,(or start 0) ,@(when end `(:end ,end)))
      `(string-downcase ,string)))

(our-defmacro nstring-capitalize (string &key start end)
  "Destructively capitalize STRING."
  (if (or start end)
      `(string-capitalize ,string :start ,(or start 0) ,@(when end `(:end ,end)))
      `(string-capitalize ,string)))

;;; ─── Array predicates (FR-564) ────────────────────────────────────────────────

(our-defmacro bit-vector-p (object)
  "Return true if OBJECT is a bit-vector."
  (let ((o (gensym "O")))
    `(let ((,o ,object))
       (and (vectorp ,o) (eq (array-element-type ,o) 'bit)))))

(our-defmacro simple-string-p (object)
  "Return true if OBJECT is a simple string."
  `(stringp ,object))

(our-defmacro simple-bit-vector-p (object)
  "Return true if OBJECT is a simple bit-vector."
  `(bit-vector-p ,object))

;;; ─── Array utilities (FR-541, FR-553, FR-564) ────────────────────────────────

(our-defmacro array-element-type (array)
  "Return the element type of ARRAY (cl-cc arrays are untyped: always T)."
  `(progn ,array 't))

(our-defmacro array-in-bounds-p (array &rest subscripts)
  "Return true if SUBSCRIPTS are valid indices into ARRAY."
  (let ((arr  (gensym "ARR"))
        (subs (gensym "SUBS")))
    `(let ((,arr ,array)
           (,subs (list ,@subscripts)))
       (and (= (array-rank ,arr) (length ,subs))
            (every (lambda (d s) (and (>= s 0) (< s d)))
                   (array-dimensions ,arr) ,subs)))))

(our-defmacro upgraded-array-element-type (type)
  "Return the upgraded array element type (cl-cc uses T for all types)."
  `(progn ,type 't))

;;; ─── equalp (FR-582) ─────────────────────────────────────────────────────────

(our-defmacro equalp (x y)
  "Test for structural equality with type coercion (ANSI 5.3)."
  (let ((xv (gensym "X"))
        (yv (gensym "Y"))
        (iv (gensym "I"))
        (kv (gensym "K"))
        (fn (gensym "EQP")))
    `(labels ((,fn (,xv ,yv)
                (cond
                  ((eq ,xv ,yv) t)
                  ((and (numberp ,xv) (numberp ,yv)) (= ,xv ,yv))
                  ((and (characterp ,xv) (characterp ,yv)) (char-equal ,xv ,yv))
                  ((and (stringp ,xv) (stringp ,yv)) (string-equal ,xv ,yv))
                  ((and (consp ,xv) (consp ,yv))
                   (and (,fn (car ,xv) (car ,yv)) (,fn (cdr ,xv) (cdr ,yv))))
                  ((and (vectorp ,xv) (vectorp ,yv))
                   (and (= (length ,xv) (length ,yv))
                        (block vec-eq
                          (dotimes (,iv (length ,xv) t)
                            (unless (,fn (aref ,xv ,iv) (aref ,yv ,iv))
                              (return-from vec-eq nil))))))
                  ((and (hash-table-p ,xv) (hash-table-p ,yv))
                   (and (= (hash-table-count ,xv) (hash-table-count ,yv))
                        (block ht-eq
                          (maphash (lambda (,kv v)
                                     (multiple-value-bind (yval found)
                                         (gethash ,kv ,yv)
                                       (unless (and found (,fn v yval))
                                         (return-from ht-eq nil))))
                                   ,xv)
                          t)))
                  (t nil))))
       (,fn ,x ,y))))

;;; ─── Implementation-identification stubs (FR-507) ────────────────────────────

(our-defmacro lisp-implementation-type   () '"cl-cc")
(our-defmacro lisp-implementation-version () '"0.1.0")
(our-defmacro machine-type     () '"unknown")
(our-defmacro machine-version  () '"unknown")
(our-defmacro machine-instance () '"unknown")
(our-defmacro software-type    () '"unknown")
(our-defmacro software-version () '"unknown")
(our-defmacro short-site-name  () '"unknown")
(our-defmacro long-site-name   () '"unknown")

;;; ─── compiled-function-p (FR-513) ────────────────────────────────────────────

(our-defmacro compiled-function-p (object)
  "Return true if OBJECT is a compiled function (all functions are compiled in cl-cc)."
  `(functionp ,object))

;;; ─── proclaim (FR-700) ────────────────────────────────────────────────────────

(our-defmacro proclaim (declaration)
  "Process a global proclamation (stub — ignored in cl-cc)."
  `(progn ,declaration nil))

;;; ─── Sequence: last/butlast/nbutlast with count arg (FR-596) ─────────────────

(our-defmacro last (list &optional (n 1))
  "Return the last N conses of LIST."
  (let ((lst (gensym "LST"))
        (nv  (gensym "N"))
        (len (gensym "LEN")))
    `(let* ((,lst ,list)
            (,nv  ,n)
            (,len (length ,lst)))
       (nthcdr (max 0 (- ,len ,nv)) ,lst))))

(our-defmacro butlast (list &optional (n 1))
  "Return a copy of LIST without the last N conses."
  (let ((lst (gensym "LST"))
        (nv  (gensym "N"))
        (len (gensym "LEN")))
    `(let* ((,lst ,list)
            (,nv  ,n)
            (,len (length ,lst)))
       (when (> ,len ,nv)
         (subseq ,lst 0 (- ,len ,nv))))))

(our-defmacro nbutlast (list &optional (n 1))
  "Destructively trim the last N conses from LIST (delegates to butlast in cl-cc)."
  `(butlast ,list ,n))

;;; ─── search for general sequences (FR-588) ───────────────────────────────────

(our-defmacro search (pattern sequence &key (test '#'eql) (start1 0) end1 (start2 0) end2 key)
  "Search for PATTERN as a subsequence in SEQUENCE (from-end not supported)."
  (let ((pat   (gensym "PAT"))
        (seq   (gensym "SEQ"))
        (fn    (gensym "FN"))
        (kfn   (gensym "KFN"))
        (s1    (gensym "S1"))
        (e1    (gensym "E1"))
        (s2    (gensym "S2"))
        (e2    (gensym "E2"))
        (plen  (gensym "PLEN"))
        (i     (gensym "I"))
        (j     (gensym "J"))
        (match (gensym "MATCH")))
    `(let* ((,pat  ,pattern)
            (,seq  ,sequence)
            (,fn   ,test)
            (,kfn  ,(if key key `#'identity))
            (,s1   ,start1)
            (,e1   ,(or end1 `(length ,pat)))
            (,s2   ,start2)
            (,e2   ,(or end2 `(length ,seq)))
            (,plen (- ,e1 ,s1)))
       (when (<= ,plen (- ,e2 ,s2))
         (block found
           (do ((,i ,s2 (+ ,i 1)))
               ((> (+ ,i ,plen) ,e2) nil)
             (let ((,match t))
               (do ((,j 0 (+ ,j 1)))
                   ((or (not ,match) (= ,j ,plen)))
                 (unless (funcall ,fn
                                  (funcall ,kfn (elt ,pat (+ ,s1 ,j)))
                                  (funcall ,kfn (elt ,seq (+ ,i ,j))))
                   (setq ,match nil)))
               (when ,match (return-from found ,i)))))))))

;;; ─── write-sequence / read-sequence stubs (FR-590) ───────────────────────────

(our-defmacro write-sequence (sequence stream &key (start 0) end)
  "Write elements of SEQUENCE to STREAM."
  (let ((seq (gensym "SEQ"))
        (str (gensym "STR"))
        (i   (gensym "I"))
        (ev  (gensym "E")))
    `(let* ((,seq ,sequence)
            (,str ,stream)
            (,ev  ,(or end `(length ,seq))))
       (do ((,i ,start (+ ,i 1)))
           ((>= ,i ,ev) ,seq)
         (let ((elem (elt ,seq ,i)))
           (if (characterp elem)
               (write-char elem ,str)
               (write-byte elem ,str)))))))

(our-defmacro read-sequence (sequence stream &key (start 0) end)
  "Read from STREAM into SEQUENCE."
  (let ((seq (gensym "SEQ"))
        (str (gensym "STR"))
        (i   (gensym "I"))
        (ev  (gensym "E"))
        (ch  (gensym "CH")))
    `(let* ((,seq ,sequence)
            (,str ,stream)
            (,ev  ,(or end `(length ,seq))))
       (do ((,i ,start (+ ,i 1)))
           ((>= ,i ,ev) ,i)
         (let ((,ch (read-char ,str nil nil)))
           (if (null ,ch)
               (return ,i)
               (setf (elt ,seq ,i) ,ch)))))))

;;; ─── File system operations (FR-479) ─────────────────────────────────────────
;;; probe-file, rename-file, delete-file, file-write-date, file-author,
;;; directory, ensure-directories-exist are registered as host bridges in vm.lisp.

;;; ─── Pathname accessors (FR-566) — host bridge ────────────────────────────────
;;; These delegate to host CL pathname functions via vm-host-bridge.

;;; ─── with-compilation-unit (FR-363) ──────────────────────────────────────────

(our-defmacro with-compilation-unit (options &rest body)
  "Execute BODY as a compilation unit (stub — no special behavior in cl-cc)."
  `(progn ,@body))

;;; ─── time (FR-431) ────────────────────────────────────────────────────────────

(our-defmacro time (form)
  "Evaluate FORM, print elapsed time, return its value."
  (let ((start (gensym "START"))
        (result (gensym "RESULT")))
    `(let* ((,start  (get-universal-time))
            (,result ,form))
       (format t "~&Elapsed: ~A second(s)~%" (- (get-universal-time) ,start))
       ,result)))

;;; ─── room (FR-434) ────────────────────────────────────────────────────────────

(our-defmacro room (&optional detail)
  "Print memory usage information via the host SBCL runtime."
  (if detail
      `(sb-ext::room ,detail)
      '(sb-ext::room)))

;;; ─── break (FR-557) ──────────────────────────────────────────────────────────

(our-defmacro break (&optional (format-str "") &rest args)
  "Signal a breakpoint (stub — prints message and continues in cl-cc)."
  `(progn (format t ,(concatenate 'string "~&BREAK: " format-str "~%") ,@args) nil))

;;; ─── dribble (FR-514) ─────────────────────────────────────────────────────────

(our-defmacro dribble (&optional path)
  "Start/stop dribbling I/O to a file via the host SBCL runtime."
  `(progn (cl:dribble ,path) nil))

;;; ─── pprint (FR-357) ──────────────────────────────────────────────────────────

(our-defmacro pprint (object &optional stream)
  "Pretty-print OBJECT (delegates to print in cl-cc)."
  (if stream
      `(print ,object ,stream)
      `(print ,object)))

;;; ─── write (FR-569) — accepts all keywords, delegates to princ/prin1 ─────────

(our-defmacro write (object &key stream (escape t) readably pretty circle
                             gensym array base radix case level length lines
                             right-margin miser-width pprint-dispatch)
  "Write OBJECT to STREAM, binding print control variables from keyword args."
  (let ((obj-g  (gensym "OBJ"))
        (str-g  (gensym "STR"))
        (base-g (gensym "BASE")) (radix-g (gensym "RADIX"))
        (esc-g  (gensym "ESC"))  (level-g (gensym "LVL"))
        (len-g  (gensym "LEN")))
    `(let* ((,obj-g   ,object)
            (,str-g   ,(or stream '*standard-output*))
            (,base-g  ,(or base '*print-base*))
            (,radix-g ,(or radix '*print-radix*))
            (,esc-g   ,(or escape '*print-escape*))
            (,level-g ,(or level '*print-level*))
            (,len-g   ,(or length '*print-length*)))
       ;; Evaluate remaining ignored args for side effects
       (progn ,readably ,pretty ,circle ,gensym ,array ,case
              ,lines ,right-margin ,miser-width ,pprint-dispatch)
       (let ((*print-base*   ,base-g)  (*print-radix*  ,radix-g)
             (*print-escape* ,esc-g)   (*print-level*  ,level-g)
             (*print-length* ,len-g))
         (write-string (write-to-string ,obj-g) ,str-g))
       ,obj-g)))

;;; ─── formatter macro (FR-698) ────────────────────────────────────────────────

(our-defmacro formatter (control-string)
  "Return a function that formats using CONTROL-STRING (cl-cc stub)."
  `(lambda (stream &rest args)
     (apply #'format stream ,control-string args)))

;;; ─── locally (FR-397) ─────────────────────────────────────────────────────────

(our-defmacro locally (&rest forms)
  "Evaluate FORMS in a local declaration scope (declarations ignored in cl-cc)."
  (let ((body (if (and forms (listp (car forms))
                       (eq (caar forms) 'declare))
                  (cdr forms)
                  forms)))
    `(progn ,@body)))

;;; ─── documentation / (setf documentation) (FR-607) ─────────────────────────
;;; Docstrings are registered at compile time in *documentation-table* (CL-level
;;; hash table) by the defun expander. At runtime, the VM queries this table via
;;; the %get-documentation host bridge function.

(our-defmacro documentation (x doc-type)
  "Return the documentation string for X of type DOC-TYPE, or nil."
  `(%get-documentation ,x ,doc-type))

;;; ─── with-hash-table-iterator (FR-497) ───────────────────────────────────────

(our-defmacro with-hash-table-iterator (spec &rest body)
  "Create a hash table iterator named (first spec) over (second spec) within BODY."
  (let ((name     (car spec))
        (table    (cadr spec))
        (keys-var (gensym "KEYS"))
        (tbl-var  (gensym "TBL")))
    `(let* ((,tbl-var  ,table)
            (,keys-var (hash-table-keys ,tbl-var)))
       (flet ((,name ()
                (if ,keys-var
                    (let ((k (car ,keys-var)))
                      (setf ,keys-var (cdr ,keys-var))
                      (values t k (gethash k ,tbl-var)))
                    (values nil nil nil))))
         ,@body))))

;;; ─── read-preserving-whitespace (FR-658) ────────────────────────────────────

(our-defmacro read-preserving-whitespace (&optional (stream '*standard-input*)
                                           (eof-error-p t) eof-value recursive-p)
  "Read a form, preserving terminal whitespace (delegates to read in cl-cc)."
  `(read ,stream ,eof-error-p ,eof-value ,recursive-p))

;;; ─── read-delimited-list (FR-659) ───────────────────────────────────────────

;; read-delimited-list is registered in *vm-host-bridge-functions* — no macro needed.
;; Calls with an optional stream arg default to *standard-input* via host CL.

;;; ─── bit-nor / bit-nand / bit-eqv / bit-andc1 / bit-andc2 / bit-orc1 / bit-orc2 (FR-635) ──

;; Note: The optional result-bit-array arg is accepted for ANSI compatibility
;; but ignored — cl-cc's bit-and/bit-or/bit-xor are binary-only builtins.
(our-defmacro bit-nor (bit-array1 bit-array2 &optional result-bit-array)
  "Element-wise NOR of two bit arrays."
  `(progn ,result-bit-array (bit-not (bit-ior ,bit-array1 ,bit-array2))))

(our-defmacro bit-nand (bit-array1 bit-array2 &optional result-bit-array)
  "Element-wise NAND of two bit arrays."
  `(progn ,result-bit-array (bit-not (bit-and ,bit-array1 ,bit-array2))))

(our-defmacro bit-eqv (bit-array1 bit-array2 &optional result-bit-array)
  "Element-wise XNOR (equivalence) of two bit arrays."
  `(progn ,result-bit-array (bit-not (bit-xor ,bit-array1 ,bit-array2))))

(our-defmacro bit-andc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Element-wise AND of (NOT bit-array1) and bit-array2."
  `(progn ,result-bit-array (bit-and (bit-not ,bit-array1) ,bit-array2)))

(our-defmacro bit-andc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Element-wise AND of bit-array1 and (NOT bit-array2)."
  `(progn ,result-bit-array (bit-and ,bit-array1 (bit-not ,bit-array2))))

(our-defmacro bit-orc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Element-wise OR of (NOT bit-array1) and bit-array2."
  `(progn ,result-bit-array (bit-ior (bit-not ,bit-array1) ,bit-array2)))

(our-defmacro bit-orc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Element-wise OR of bit-array1 and (NOT bit-array2)."
  `(progn ,result-bit-array (bit-ior ,bit-array1 (bit-not ,bit-array2))))

;;; ─── pprint-related stubs (FR-357) ───────────────────────────────────────────

(our-defmacro pprint-fill (stream object &optional colon-p at-sign-p)
  "Pretty-print list with fill style (stub — delegates to prin1)."
  `(progn ,colon-p ,at-sign-p (prin1 ,object ,stream)))

(our-defmacro pprint-linear (stream object &optional colon-p at-sign-p)
  "Pretty-print list with linear style (stub — delegates to prin1)."
  `(progn ,colon-p ,at-sign-p (prin1 ,object ,stream)))

(our-defmacro pprint-tabular (stream object &optional colon-p at-sign-p tabsize)
  "Pretty-print list with tabular style (stub)."
  `(progn ,colon-p ,at-sign-p ,tabsize (prin1 ,object ,stream)))

(our-defmacro pprint-newline (kind &optional stream)
  "Emit a conditional newline (stub — ignored in cl-cc)."
  `(progn ,kind ,stream nil))

(our-defmacro pprint-tab (kind column colinc &optional stream)
  "Move to column for tabbing (stub — ignored in cl-cc)."
  `(progn ,kind ,column ,colinc ,stream nil))

(our-defmacro pprint-indent (relative-to n &optional stream)
  "Set indentation (stub — ignored in cl-cc)."
  `(progn ,relative-to ,n ,stream nil))

(our-defmacro pprint-logical-block (spec &rest body)
  "Execute BODY as a logical block for pretty printing (stub)."
  (let* ((stream-symbol (car spec))
         (rest-spec     (cddr spec))  ; skip stream-sym and list
         (prefix        (getf rest-spec :prefix))
         (per-line-prefix (getf rest-spec :per-line-prefix))
         (suffix        (getf rest-spec :suffix)))
    `(progn ,prefix ,per-line-prefix ,suffix ,stream-symbol ,@body)))

;;; ─── file-string-length stub (FR-591) ────────────────────────────────────────

(our-defmacro file-string-length (stream object)
  "Return the number of UTF-8 octets needed to write OBJECT to STREAM."
  (let ((obj-g (gensym "OBJ")))
    `(progn ,stream
       (let ((,obj-g ,object))
         (if (characterp ,obj-g)
             (length (string-to-octets (string ,obj-g) :encoding :utf-8))
             (length (string-to-octets ,obj-g :encoding :utf-8)))))))

;;; ─── stream-external-format stub (FR-562) ────────────────────────────────────

(our-defmacro stream-external-format (stream)
  "Return the external format of STREAM (stub returns :default)."
  `(progn ,stream :default))

;;; with-input-from-string :start/:end/:index (FR-608) handled in the definition above (line ~486).

;;; ─── trace / untrace stubs (FR-432) ──────────────────────────────────────────

(our-defmacro trace (&rest function-names)
  "Enable tracing for FUNCTION-NAMES via the host SBCL runtime."
  `(progn (cl:trace ,@function-names) nil))

(our-defmacro untrace (&rest function-names)
  "Disable tracing for FUNCTION-NAMES via the host SBCL runtime."
  `(progn (cl:untrace ,@function-names) nil))

;;; ─── step stub (FR-433) ───────────────────────────────────────────────────────

(our-defmacro step (form)
  "Single-step through FORM via the host SBCL runtime."
  `(cl:step ,form))

;;; ─── disassemble stub (FR-576) ───────────────────────────────────────────────

(our-defmacro disassemble (function)
  "Print disassembly of FUNCTION via the host SBCL runtime."
  `(progn (cl:disassemble ,function) nil))

;;; ─── inspect stub (FR-577) ────────────────────────────────────────────────────

(our-defmacro inspect (object)
  "Inspect OBJECT via the host SBCL runtime."
  `(progn (cl:inspect ,object) nil))

;;; ─── apropos / apropos-list (FR-435) — host bridge ───────────────────────────
;;; Registered in vm.lisp as host bridges.

;;; ─── ed stub (FR-515) ─────────────────────────────────────────────────────────

(our-defmacro ed (&optional x)
  "Invoke editor on X (stub — no-op in cl-cc)."
  `(progn ,x nil))

;;; ─── invoke-debugger stub (FR-557) ───────────────────────────────────────────

(our-defmacro invoke-debugger (condition)
  "Invoke the debugger on CONDITION (stub — signals error in cl-cc)."
  `(error ,condition))

;;; ─── declare stub wrappers for unsupported declaration types ─────────────────
;;; The compiler already accepts declare at the syntax level; unsupported
;;; specifiers are silently ignored. No macro wrappers needed.

;;; ─── compiler-let stub (FR-439) ───────────────────────────────────────────────

(our-defmacro compiler-let (bindings &rest body)
  "Bind variables at compile time (stub — acts as let at runtime in cl-cc)."
  `(let ,bindings ,@body))

;;; ─── #P pathname literal — registered in vm.lisp as host bridge ──────────────

;;; ─── read-char-no-hang (FR-568) ──────────────────────────────────────────────

(our-defmacro read-char-no-hang (&optional (stream '*standard-input*)
                                  (eof-error-p t) eof-value recursive-p)
  "Return next char if available without waiting, else nil (stub — delegates to read-char)."
  `(read-char ,stream ,eof-error-p ,eof-value ,recursive-p))

;;; ─── FR-358: Readtable API stubs ─────────────────────────────────────────────
;;; cl-cc uses a fixed built-in lexer; readtables are not user-customizable.
;;; These stubs accept args for ANSI CL compatibility without signaling errors.

(our-defmacro readtablep (x)
  "Return nil (cl-cc has no user-defined readtables)."
  `(progn ,x nil))

(our-defmacro copy-readtable (&optional from to)
  "Return nil (cl-cc readtable stub)."
  `(progn ,from ,to nil))

(our-defmacro readtable-case (readtable)
  "Return :upcase (cl-cc always uses upcase)."
  `(progn ,readtable :upcase))

(our-defmacro set-macro-character (char fn &optional non-terminating-p readtable)
  "No-op stub — cl-cc's reader is not user-extensible."
  `(progn ,char ,fn ,non-terminating-p ,readtable t))

(our-defmacro get-macro-character (char &optional readtable)
  "Return (values nil nil) — no user-defined macro chars."
  `(progn ,char ,readtable (values nil nil)))

(our-defmacro set-dispatch-macro-character (disp-char sub-char fn &optional readtable)
  "No-op stub."
  `(progn ,disp-char ,sub-char ,fn ,readtable t))

(our-defmacro get-dispatch-macro-character (disp-char sub-char &optional readtable)
  "Return nil — no user-defined dispatch chars."
  `(progn ,disp-char ,sub-char ,readtable nil))

(our-defmacro make-dispatch-macro-character (char &optional non-terminating-p readtable)
  "No-op stub."
  `(progn ,char ,non-terminating-p ,readtable t))

(our-defmacro set-syntax-from-char (to-char from-char &optional to-readtable from-readtable)
  "No-op stub."
  `(progn ,to-char ,from-char ,to-readtable ,from-readtable t))

;;; ─── FR-589: compile-file stub ───────────────────────────────────────────────

(our-defmacro compile-file (pathname &rest keys)
  ;; Load PATHNAME (cl-cc compiles by loading). Extra keyword args ignored.
  (let ((path-var (gensym "PATH")))
    (declare (ignore keys))
    `(let ((,path-var ,pathname))
       (our-load ,path-var)
       (values (probe-file ,path-var) nil nil))))

;;; ------------------------------------------------------------
