(in-package :cl-cc/expand)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Stdlib — ANSI CL Phase 1 Macros (FR-201 through FR-212)
;;;
;;; Contains: psetf, shiftf, with-accessors, define-modify-macro, assert,
;;; define-condition, with-input-from-string, with-output-to-string,
;;; with-standard-io-syntax, with-package-iterator (registered),
;;; define-compiler-macro (registered).
;;;
;;; Basic numeric/control macros (1+/1-/signum/isqrt/ecase/etypecase/ccase/
;;; ctypecase/return/rotatef/destructuring-bind/prog/prog*/with-slots/nth-value)
;;; are in macros-stdlib.lisp (loads before).
;;;
;;; Load order: after macros-stdlib.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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
 The optional second element of BINDING is written to the fresh stream first,
 so the returned string preserves the ANSI-visible prefix behavior."
  (let ((var (first binding))
        (initial-string (second binding)))
    `(let ((,var (make-string-output-stream)))
       ,@(when initial-string `((write-string ,initial-string ,var)))
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
         (*package*                  (rt-find-package "COMMON-LISP-USER")))
     ,@body))

;; WITH-PACKAGE-ITERATOR (FR-211) — runtime-backed package iteration helper
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
                              (let ((pkg (rt-find-package p)))
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
  (lambda (form env)
    (declare (ignore env))
    (let ((name (second form))
          (lambda-list (third form))
          (body (cdddr form)))
      (register-compiler-macro name (make-compiler-macro-expander lambda-list body))
      `(quote ,name))))
