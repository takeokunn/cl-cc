(in-package :cl-cc)
;;; List Mutation Macros

;; PUSH macro
(our-defmacro push (value place)
  "Push VALUE onto the front of list PLACE."
  `(setf ,place (cons ,value ,place)))

;; POP macro
(our-defmacro pop (place)
  "Remove and return the first element of list PLACE."
  (let ((tmp (gensym "TMP")))
    `(let ((,tmp (car ,place)))
       (setf ,place (cdr ,place))
       ,tmp)))

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
  "Execute BODY with (first binding) bound to a string input stream."
  (let* ((var    (first binding))
         (string (second binding)))
    `(let ((,var (make-string-input-stream ,string)))
       ,@body)))

;; WITH-OUTPUT-TO-STRING
(our-defmacro with-output-to-string (binding &body body)
  "Execute BODY with (first binding) bound to a string output stream."
  (let ((var (first binding)))
    `(let ((,var (make-string-output-stream)))
       ,@body
       (get-output-stream-string ,var))))

;; WITH-STANDARD-IO-SYNTAX (FR-210) — stub
(our-defmacro with-standard-io-syntax (&body body)
  "Execute BODY with standard I/O syntax bindings (stub)."
  `(progn ,@body))

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

;; REMOVE-IF: keep elements for which pred is false
(our-defmacro remove-if (pred list)
  (%filter-list-expand 'unless pred list))

;; REMOVE-IF-NOT: keep elements for which pred is true
(our-defmacro remove-if-not (pred list)
  (%filter-list-expand 'when pred list))

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

;; FIND-IF: first element for which pred is true, or nil
(our-defmacro find-if (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X")))
    `(let ((,fn-var ,pred))
       (block nil
         (dolist (,x ,list nil)
           (when (funcall ,fn-var ,x)
             (return ,x)))))))

;; POSITION: index of first element eql to item, or nil
(our-defmacro position (item list)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (idx (gensym "IDX")))
    `(let ((,item-var ,item)
           (,idx 0))
       (block nil
         (dolist (,x ,list nil)
           (when (eql ,item-var ,x)
             (return ,idx))
           (setq ,idx (+ ,idx 1)))))))

;; COUNT: number of elements eql to item
(our-defmacro count (item list)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (cnt (gensym "CNT")))
    `(let ((,item-var ,item)
           (,cnt 0))
       (dolist (,x ,list ,cnt)
         (when (eql ,item-var ,x)
           (setq ,cnt (+ ,cnt 1)))))))

;; COUNT-IF: number of elements for which pred is true
(our-defmacro count-if (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (cnt (gensym "CNT")))
    `(let ((,fn-var ,pred)
           (,cnt 0))
       (dolist (,x ,list ,cnt)
         (when (funcall ,fn-var ,x)
           (setq ,cnt (+ ,cnt 1)))))))

;; FIND-IF-NOT: first element for which pred is false (FR-610)
(our-defmacro find-if-not (pred list)
  `(find-if (complement ,pred) ,list))

;; POSITION-IF: index of first element satisfying pred (FR-610)
(our-defmacro position-if (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (idx (gensym "IDX")))
    `(let ((,fn-var ,pred)
           (,idx 0))
       (block nil
         (dolist (,x ,list nil)
           (when (funcall ,fn-var ,x)
             (return ,idx))
           (setq ,idx (+ ,idx 1)))))))

;; POSITION-IF-NOT: index of first element not satisfying pred (FR-610)
(our-defmacro position-if-not (pred list)
  `(position-if (complement ,pred) ,list))

;; COUNT-IF-NOT: count elements for which pred is false (FR-610)
(our-defmacro count-if-not (pred list)
  `(count-if (complement ,pred) ,list))

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

;; REMOVE: remove all elements eql to item
(our-defmacro remove (item list)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,item-var ,item)
           (,acc nil))
       (dolist (,x ,list (nreverse ,acc))
         (unless (eql ,item-var ,x)
           (setq ,acc (cons ,x ,acc)))))))

;; REMOVE-DUPLICATES: keep only the first occurrence of each element
(our-defmacro remove-duplicates (list)
  (let ((x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,acc nil))
       (dolist (,x ,list (nreverse ,acc))
         (unless (member ,x ,acc)
           (setq ,acc (cons ,x ,acc)))))))

;; UNION: all elements present in either list, no duplicates
(our-defmacro union (list1 list2)
  (let ((l1 (gensym "L1"))
        (l2 (gensym "L2"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,l1 ,list1)
           (,l2 ,list2)
           (,acc nil))
       (dolist (,x ,l2)
         (setq ,acc (cons ,x ,acc)))
       (dolist (,x ,l1 (nreverse ,acc))
         (unless (member ,x ,l2)
           (setq ,acc (cons ,x ,acc)))))))

;; Shared expansion for set-difference / intersection.
;; KEEP-WHEN is 'when (intersection) or 'unless (set-difference).
(defun %set-filter-expand (list1 list2 keep-when)
  (let ((l2  (gensym "L2"))
        (x   (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,l2 ,list2) (,acc nil))
       (dolist (,x ,list1 (nreverse ,acc))
         (,keep-when (member ,x ,l2)
           (setq ,acc (cons ,x ,acc)))))))

;; SET-DIFFERENCE: elements in list1 not present in list2
(our-defmacro set-difference (list1 list2)
  (%set-filter-expand list1 list2 'unless))

;; INTERSECTION: elements present in both lists
(our-defmacro intersection (list1 list2)
  (%set-filter-expand list1 list2 'when))

;; SUBSETP: true iff every element of list1 is in list2
(our-defmacro subsetp (list1 list2)
  (let ((l2 (gensym "L2"))
        (x (gensym "X")))
    `(let ((,l2 ,list2))
       (every (lambda (,x) (member ,x ,l2)) ,list1))))

;; ADJOIN: cons item onto list only if not already present
(our-defmacro adjoin (item list)
  (let ((item-var (gensym "ITEM"))
        (lst (gensym "LST")))
    `(let ((,item-var ,item)
           (,lst ,list))
       (if (member ,item-var ,lst) ,lst (cons ,item-var ,lst)))))

;; RASSOC: find association pair by cdr value
(our-defmacro rassoc (item alist)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X")))
    `(let ((,item-var ,item))
       (block nil
         (dolist (,x ,alist nil)
           (when (and (consp ,x) (eql ,item-var (cdr ,x)))
             (return ,x)))))))

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

;;; ─── ASSOC-IF / ASSOC-IF-NOT / RASSOC-IF / RASSOC-IF-NOT (FR-660, FR-500) ─

(our-defmacro assoc-if (pred alist)
  (let ((fn-var (gensym "FN"))
        (pair   (gensym "PAIR")))
    `(let ((,fn-var ,pred))
       (dolist (,pair ,alist nil)
         (when (and ,pair (funcall ,fn-var (car ,pair)))
           (return ,pair))))))

(our-defmacro assoc-if-not (pred alist)
  `(assoc-if (complement ,pred) ,alist))

(our-defmacro rassoc-if (pred alist)
  (let ((fn-var (gensym "FN"))
        (pair   (gensym "PAIR")))
    `(let ((,fn-var ,pred))
       (dolist (,pair ,alist nil)
         (when (and ,pair (funcall ,fn-var (cdr ,pair)))
           (return ,pair))))))

(our-defmacro rassoc-if-not (pred alist)
  `(rassoc-if (complement ,pred) ,alist))

;;; ─── MEMBER-IF / MEMBER-IF-NOT (FR-499) ────────────────────────────────────

(our-defmacro member-if (pred list)
  (let ((fn-var (gensym "FN"))
        (tail   (gensym "TAIL")))
    `(let ((,fn-var ,pred))
       (do ((,tail ,list (cdr ,tail)))
           ((null ,tail) nil)
         (when (funcall ,fn-var (car ,tail))
           (return ,tail))))))

(our-defmacro member-if-not (pred list)
  `(member-if (complement ,pred) ,list))

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

(our-defmacro member-if (pred list)
  "Return the tail of LIST starting from first element satisfying PRED."
  (let ((fn (gensym "FN")) (tail (gensym "TAIL")))
    `(let ((,fn ,pred))
       (do ((,tail ,list (cdr ,tail)))
           ((null ,tail) nil)
         (when (funcall ,fn (car ,tail))
           (return ,tail))))))

(our-defmacro member-if-not (pred list)
  "Return the tail of LIST starting from first element not satisfying PRED."
  `(member-if (complement ,pred) ,list))

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

;;; ------------------------------------------------------------
