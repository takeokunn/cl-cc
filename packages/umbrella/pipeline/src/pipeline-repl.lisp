(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Compile — REPL Persistent State and Incremental Execution
;;;
;;; Contains: *repl-vm-state*, *repl-accessor-map*, *repl-pool-instructions*,
;;; *repl-pool-labels*, *repl-global-vars-persistent*, *repl-defstruct-registry*,
;;; reset-repl-state, %ensure-repl-state, run-string-repl,
;;; %prescan-in-package, our-load.
;;;
;;; Load order: after pipeline.lisp (compile-expression, our-eval).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── REPL Persistent State ────────────────────────────────────────────────
;;;
;;; The REPL accumulates all compiled instructions into a shared pool so that
;;; closures defined in one expression (with entry labels in that expression's
;;; instruction range) remain callable in subsequent expressions.  Each new
;;; compile-string result is appended to *repl-pool-instructions*, and its
;;; labels are inserted into *repl-pool-labels* with a global offset.  Only
;;; the newly-added slice is executed, but the full pool's label table is used
;;; for all label lookups — so cross-call closure invocations work correctly.

(defvar *repl-vm-state* nil
  "Persistent VM state for the interactive REPL.
Reused across form evaluations so that functions and variables defined
in one expression remain accessible in subsequent ones.")

(defvar *repl-accessor-map* nil
  "Persistent accessor map for the REPL.
Accumulates defstruct slot accessor mappings across form evaluations.")

(defvar *repl-pool-instructions* nil
  "Adjustable vector accumulating ALL instructions from the current REPL session.
Enables cross-expression closure calls (body labels remain globally valid).")

(defvar *repl-pool-labels* nil
  "Hash table mapping label names to absolute PCs in *repl-pool-instructions*.")

(defvar *repl-global-vars-persistent* nil
  "Persistent hash table tracking global variable names defined across REPL calls.
When non-nil, this is bound to *repl-global-variables* during compilation
so that variables from (defvar ...) in one REPL call are visible in the next.")

(defvar *repl-defstruct-registry* nil
  "Persistent defstruct slot registry for the REPL.
Accumulates slot info across form evaluations so :include works across calls.")

(defvar *our-load-host-definition-mode* nil
  "When true, OUR-LOAD may evaluate host-only top-level definition forms directly.")

(defun reset-repl-state ()
  "Reset the REPL persistent state, starting a completely fresh session."
  (setf *repl-vm-state* nil
        *repl-accessor-map* nil
        *repl-pool-instructions* nil
        *repl-pool-labels* nil
        *repl-global-vars-persistent* nil
        *repl-defstruct-registry* nil
        *repl-label-counter* nil))

(defmacro with-fresh-repl-state (&body body)
  "Execute BODY with a dynamically-scoped fresh REPL state.
Unlike reset-repl-state (setf-based, clobbers outer state), this binds
each persistent REPL special to nil for the dynamic extent of BODY, so
the outer REPL state is preserved after BODY returns.

Use in tests and selfhost phases that need isolated REPL state without
disturbing any enclosing REPL session."
  (cons 'let
        (cons '((*repl-vm-state* nil)
                (*repl-accessor-map* nil)
                (*repl-pool-instructions* nil)
                (*repl-pool-labels* nil)
                (*repl-global-vars-persistent* nil)
                (*repl-label-counter* nil)
                (*repl-defstruct-registry* nil))
              body)))

(export 'with-fresh-repl-state :cl-cc)

(defun %ensure-repl-state ()
  "Lazily initialise all persistent REPL state variables on first use."
  (unless *repl-vm-state*
    (setf *repl-vm-state* (make-instance 'vm-io-state :output-stream *standard-output*)))
  (unless *repl-accessor-map*
    (setf *repl-accessor-map* (make-hash-table :test #'eq)))
  (unless *repl-pool-instructions*
    (setf *repl-pool-instructions* (make-array 64 :adjustable t :fill-pointer 0 :element-type t)))
  (unless *repl-pool-labels*
    (setf *repl-pool-labels* (make-hash-table :test #'eql)))
  (unless *repl-global-vars-persistent*
    (setf *repl-global-vars-persistent* (make-hash-table :test #'eq)))
  (unless *repl-defstruct-registry*
    (setf *repl-defstruct-registry* (make-hash-table :test #'eq))))

(defun %run-form-repl-impl (result)
  "Finalize compilation RESULT into the REPL persistent pool and execute
   the newly-added instruction slice.  Factors out the shared post-compile
   machinery used by both run-string-repl and run-form-repl."
  (let* ((program (compilation-result-program result))
         (new-insts (vm-program-instructions program))
         ;; PC where the new code will start in the global pool
         (start-pc (fill-pointer *repl-pool-instructions*)))
    ;; Persist the label counter for the next compilation
    (when (integerp *repl-capture-label-counter*)
      (setf *repl-label-counter* *repl-capture-label-counter*))
    ;; Track any new global variables defined by this compilation
    (dolist (inst new-insts)
      (when (typep inst 'vm-set-global)
        (setf (gethash (vm-global-name inst) *repl-global-vars-persistent*) t)))
    ;; Append new instructions to the shared pool
    (dolist (inst new-insts)
      (vector-push-extend inst *repl-pool-instructions*))
    ;; Merge new labels (with global offset) into the pool label table
    (let ((new-labels (build-label-table new-insts)))
      (maphash (lambda (_key bucket)
                 (declare (ignore _key))
                 (dolist (entry bucket)
                   (vm-label-table-store *repl-pool-labels*
                                         (car entry)
                                         (+ start-pc (cdr entry)))))
               new-labels))
    ;; Execute only the new slice, using the full pool for label resolution
    (run-program-slice *repl-pool-instructions* *repl-pool-labels*
                       start-pc *repl-vm-state*)))

(defun run-string-repl (source)
  "Compile and run SOURCE using the persistent REPL state.
Unlike run-string, this reuses the VM state (function-registry, class-registry,
heap) across calls so that top-level definitions persist into later expressions.
Cross-expression closure calls work because all instructions share one pool.

Example:
  (run-string-repl \"(defun double (x) (* x 2))\")
  (run-string-repl \"(double 21)\")  ; => 42"
  (%ensure-repl-state)
  (let* ((*package* *package*)
         (*accessor-slot-map* *repl-accessor-map*)
         (*defstruct-slot-registry* *repl-defstruct-registry*)
         (*labels-boxed-fns* nil)
         ;; Bind persistent globals so compiler-context picks them up
         (*repl-global-variables* *repl-global-vars-persistent*)
         ;; Enable label counter capture so we can persist it
         (*repl-capture-label-counter* t))
    (let ((forms (parse-source-for-language source :lisp)))
      (when (and (= (length forms) 1)
                 (consp (first forms))
                 (eq (caar forms) 'in-package))
        (let ((pkg (find-package (second (first forms)))))
          (unless pkg
            (error "Unknown package: ~S" (second (first forms))))
          (setf *package* pkg)
          (return-from run-string-repl (second (first forms)))))
      (%run-form-repl-impl (compile-string source :target :vm)))))

(defun %normalize-register-macro-form (form)
  "Register a host macro expander from a top-level REGISTER-MACRO FORM.
If the expander is a lambda, normalize the expansion result through
COMPILER-MACROEXPAND-ALL before storing the host callable in macro-env."
  (destructuring-bind (_ name-form expander-form) form
    (declare (ignore _))
    (let ((name (if (and (consp name-form) (eq (car name-form) 'quote))
                    (second name-form)
                    name-form)))
      (if (and (consp expander-form)
               (symbolp (car expander-form))
               (string= (symbol-name (car expander-form)) "LAMBDA"))
          (let* ((lambda-list (second expander-form))
                 (body        (cddr expander-form))
                 (expanded    (mapcar #'compiler-macroexpand-all body))
                 (host-fn     (eval (list* 'lambda lambda-list expanded))))
            (register-macro name host-fn)
            name)
          (progn
            (eval form)
            name)))))

(defun %path-prefix-p (prefix path)
  "Return T when PATH starts with PREFIX." 
  (let ((mismatch-index (mismatch prefix path)))
    (or (null mismatch-index)
        (= mismatch-index (length prefix)))))

(defun %project-source-load-p (path)
  "Return T when PATH appears to be inside the current project root." 
  (let* ((root (namestring (truename #P"./")))
         (normalized-root (if (and (> (length root) 0)
                                   (char/= (char root (1- (length root))) #\/))
                              (concatenate 'string root "/")
                              root)))
    (%path-prefix-p normalized-root path)))

(defun %host-only-top-level-form-p (form)
  "Return T when FORM is a top-level definition form better handled by host EVAL during project source loading." 
  (and (consp form)
       (symbolp (car form))
       (member (symbol-name (car form))
               '("EVAL-WHEN" "DEFPACKAGE" "DEFCLASS" "DEFSTRUCT" "DEFGENERIC"
                 "DEFMETHOD" "DEFVAR" "DEFPARAMETER" "DEFCONSTANT")
               :test #'string=)))

(defun %skip-redundant-defpackage-p (form)
  "Return T when FORM is a DEFPACKAGE for a package that already exists." 
  (and (consp form)
       (symbolp (car form))
       (string= (symbol-name (car form)) "DEFPACKAGE")
       (second form)
       (find-package (second form))))

(defun %top-level-project-macro-p (symbol)
  "Return T when SYMBOL names a registered project macro rather than a core bootstrap macro." 
  (let ((pkg (symbol-package symbol)))
    (and pkg
         (not (member (package-name pkg)
                      '("COMMON-LISP" "CL-CC/BOOTSTRAP")
                      :test #'string=))
         (or (cl-cc/expand::lookup-macro symbol)
             (macro-function symbol)))))

(defun %host-only-top-level-macro-name-p (symbol)
  "Return T when SYMBOL names a project macro that is safe and preferable to execute directly on the host during source loads." 
  (and (symbolp symbol)
       (member (symbol-name symbol)
               '("DEFINE-RT-PREDICATE"
                 "DEFINE-RT-BINARY-PREDICATE"
                 "DEFINE-RT-STREAM-OP"
                 "DEFINE-LIST-LOWERER"
                 "DEFINE-VM-INSTRUCTION"
                 "DEFINE-VM-UNARY-INSTRUCTION"
                 "DEFINE-VM-BINARY-INSTRUCTION"
                 "DEFINE-VM-CHAR-COMPARISON"
                 "DEFINE-VM-STRING-COMPARISON"
                 "DEFINE-VM-STRING-TRIM-INSTRUCTION"
                 "DEFINE-VM-STRING-TRIM-EXECUTOR"
                 "DEFINE-VM-FLOAT-ROUNDING-EXECUTORS"
                 "DEFINE-VM-HASH-PROPERTY-EXECUTORS")
               :test #'string=)))

(defun %remember-host-global-definition (form)
  "Record host-evaluated global variable definitions in the REPL global registry." 
  (when (and *repl-global-vars-persistent*
             (consp form)
             (symbolp (car form))
             (member (symbol-name (car form)) '("DEFVAR" "DEFPARAMETER" "DEFCONSTANT")
                     :test #'string=)
             (symbolp (second form)))
    (setf (gethash (second form) *repl-global-vars-persistent*) t)))

(defun %form-contains-symbol-name-p (form names)
  "Return T when FORM recursively contains any symbol whose name is in NAMES." 
  (cond
    ((symbolp form)
     (member (symbol-name form) names :test #'string=))
    ((consp form)
     (or (%form-contains-symbol-name-p (car form) names)
         (%form-contains-symbol-name-p (cdr form) names)))
    (t nil)))

(defun %form-contains-bootstrap-quasiquote-p (form)
  "Return T when FORM still contains bootstrap quasiquote operators." 
  (%form-contains-symbol-name-p form '("BACKQUOTE" "UNQUOTE" "UNQUOTE-SPLICING")))

(defun %host-only-top-level-registration-form-p (form)
  "Return T when FORM is a top-level project registration/mutation form better handled by host EVAL." 
  (and (consp form)
       (or (%form-contains-symbol-name-p
            form
            '("*PHASE2-BUILTIN-HANDLERS*"
              "*STANDARD-LIBRARY-SOURCE*"
              "*EXPANDER-HEAD-TABLE*"
              "*VARIADIC-EXPANDER-SPECS*"
              "*SETF-COMPOUND-PLACE-HANDLERS*"
              "*INSTRUCTION-CONSTRUCTORS*"
              "*BUILTIN-PREDICATES*"
              "*LIST-LOWERING-TABLE*"
              "*%CONDITION-HANDLERS*"
              "*X86-64-TARGET*"
              "*AARCH64-TARGET*"
              "*RISCV64-TARGET*"
              "*WASM32-TARGET*"))
           (%form-contains-symbol-name-p
            form
            '("%REGISTER-STRING-CMP-HANDLER"
              "%REGISTER-STRING-CASE-HANDLER"
              "%REGISTER-SLOT-PREDICATE-HANDLER"
              "%REGISTER-BUILTINS"
              "%REGISTER-SLOTS-BUILTINS"
              "REGISTER-TARGET")))))

(defun %make-top-level-host-macro-expander (form)
  "Build the host expander for a top-level DEFMACRO/OUR-DEFMACRO form.
MAKE-MACRO-EXPANDER already normalizes bootstrap quasiquote markers for the
returned expansion. Re-expanding that result with COMPILER-MACROEXPAND-ALL is
too aggressive and can macroexpand inside quasiquoted templates, breaking forms
such as PUSH over an UNQUOTE place during selfhost loading.

Also force MAKE-MACRO-EXPANDER to build a pure host closure here. If it keeps
delegating each invocation back through OUR-EVAL, macros like LIST can recurse
through their own quasiquote-expanded helper forms during selfhost loading."
  (let ((*macro-eval-fn* #'eval))
    (let ((base-expander (make-macro-expander (third form) (cdddr form))))
      (lambda (call-form env)
        (our-macroexpand-all (funcall base-expander call-form env))))))

(defun %our-defmacro->register-macro-form (form)
  "Translate a top-level OUR-DEFMACRO form into the equivalent REGISTER-MACRO form.
This lets RUN-FORM-REPL reuse the more reliable REGISTER-MACRO normalization path
for plain list-based macro bodies instead of relying on host macroexpansion of the
OUR-DEFMACRO macro itself."
  (destructuring-bind (_ name lambda-list &rest body) form
    (declare (ignore _))
    (let* ((form-var (gensym "FORM"))
           (env-var (gensym "ENV"))
           (info (cl-cc/expand::parse-lambda-list lambda-list))
           (env-sym (cl-cc/expand::lambda-list-info-environment info))
           (bindings (cl-cc/expand::generate-lambda-bindings lambda-list form-var))
           (lambda-body
             (if env-sym
                 (list (list 'let (list (list env-sym env-var))
                             (cons 'let* (cons bindings body))))
                 (list (list 'declare (list 'ignore env-var))
                       (cons 'let* (cons bindings body))))))
      (list 'register-macro
            (list 'quote name)
            (cons 'lambda
                  (cons (list form-var env-var)
                        lambda-body))))))

(defun run-form-repl (form)
  "Like run-string-repl, but accepts an already-parsed S-expression FORM
   (or AST node).  Skips the parse-source-for-language round-trip so that
   callers who already have a form (e.g. our-load) do not need to serialize
   it back to a string and re-parse.  The single in-package shortcut is
   NOT handled here — callers must handle in-package themselves when they
   have the parsed form in hand (our-load already does)."
  (when (and (consp form) (eq (car form) 'in-package))
    (error "run-form-repl does not handle (in-package ...) forms; caller must dispatch before calling."))
  (%ensure-repl-state)
  (when (and (consp form)
             (symbolp (car form))
             (string= (symbol-name (car form)) "DEFMACRO"))
    ;; Outside project-source OUR-LOAD we still need an explicit host expander for
    ;; REPL/guardrail scenarios. During project-source OUR-LOAD, a plain host
    ;; DEFMACRO is enough only when the macro body no longer contains bootstrap
    ;; quasiquote operators. Otherwise we must keep the custom host expander so
    ;; forms like DEFINE-PHASE2-HANDLER and WITH-FRESH-REPL-STATE expand safely.
    (if (and *our-load-host-definition-mode*
             (not (%form-contains-bootstrap-quasiquote-p (cdddr form))))
        (eval form)
        (register-macro (second form)
                        (%make-top-level-host-macro-expander form)))
    (return-from run-form-repl (second form)))
  (when (and (consp form)
             (symbolp (car form))
             (string= (symbol-name (car form)) "OUR-DEFMACRO"))
    ;; Like DEFMACRO, OUR-DEFMACRO only needs the custom host expander when the
    ;; macro body still contains bootstrap quasiquote operators. Plain list-based
    ;; bodies can go through the REGISTER-MACRO normalization path directly.
    (if (not (%form-contains-bootstrap-quasiquote-p (cdddr form)))
        (%normalize-register-macro-form (%our-defmacro->register-macro-form form))
        (register-macro (second form)
                        (%make-top-level-host-macro-expander form)))
    (return-from run-form-repl (second form)))
  (when (and (consp form)
             (symbolp (car form))
             (string= (symbol-name (car form)) "REGISTER-MACRO"))
    ;; Some selfhost support files define macros directly with REGISTER-MACRO.
    ;; Normalize those lambda bodies before storing the host expander.
    (return-from run-form-repl (%normalize-register-macro-form form)))
  (when (and *our-load-host-definition-mode*
             (consp form)
             (%host-only-top-level-macro-name-p (car form)))
    (return-from run-form-repl (eval form)))
  (when (and (consp form)
             (symbolp (car form))
             (%top-level-project-macro-p (car form)))
    (multiple-value-bind (expanded expanded-p)
        (if (cl-cc/expand::lookup-macro (car form))
            (cl-cc/expand::our-macroexpand-1 form)
            (macroexpand-1 form))
      (when expanded-p
        (return-from run-form-repl (run-form-repl expanded)))))
  (when (and *our-load-host-definition-mode*
             (%skip-redundant-defpackage-p form))
    (return-from run-form-repl (second form)))
  (when (and *our-load-host-definition-mode*
             (%host-only-top-level-registration-form-p form))
    (return-from run-form-repl (eval form)))
  (when (and *our-load-host-definition-mode*
             (%host-only-top-level-form-p form))
    (let ((result (eval form)))
      (%remember-host-global-definition form)
      (return-from run-form-repl result)))
  (let* ((*package* *package*)
         (*accessor-slot-map* *repl-accessor-map*)
         (*defstruct-slot-registry* *repl-defstruct-registry*)
         (*labels-boxed-fns* nil)
         (*repl-global-variables* *repl-global-vars-persistent*)
         (*repl-capture-label-counter* t))
    (%run-form-repl-impl (compile-expression form :target :vm))))

;;; ─── Self-Hosting Load ──────────────────────────────────────────────────
;;;
;;; (our-load pathname) reads a file, parses all forms, and compiles+executes
;;; them in the persistent REPL state. This is the key primitive for cl-cc to
;;; load its own source files.

(defun %whitespace-symbol-p (form)
  "T if FORM is a non-NIL, non-keyword symbol whose name has no non-space graphic chars."
  (and (symbolp form) (not (null form)) (not (keywordp form))
       (let ((name (symbol-name form)))
         (and (> (length name) 0)
              (every (lambda (c) (or (not (graphic-char-p c)) (eql c #\Space))) name)))))

(defun %prescan-in-package (source)
  "Pre-scan SOURCE for an (in-package ...) form and return the package name string.
   Returns nil if not found. Used to set *package* before full parsing so that
   #. read-time eval resolves symbols in the correct package."
  (let ((pos (search "(in-package " source :test #'char-equal)))
    (when pos
      (let* ((start (+ pos (length "(in-package ")))
             (trimmed (string-trim '(#\Space #\Tab) (subseq source start))))
        ;; Handle :pkg, :pkg), "pkg", 'pkg forms
        (cond
          ((and (> (length trimmed) 0) (char= (first (coerce trimmed 'list)) #\:))
           (let ((end (position-if (lambda (c) (or (char= c #\)) (char= c #\Space))) trimmed)))
             (when end (subseq trimmed 1 end))))
          ((and (> (length trimmed) 0) (char= (first (coerce trimmed 'list)) #\"))
           (let ((end (position #\" trimmed :start 1)))
             (when end (subseq trimmed 1 end))))
          (t nil))))))

(defun our-load (pathname &key (verbose nil) (print nil) (if-does-not-exist :error)
                                 (external-format :default))
  "Load a Lisp source file by reading, compiling, and executing each form.
Uses the persistent REPL state so definitions accumulate across forms.
VERBOSE prints the file being loaded. PRINT prints each form's result.
IF-DOES-NOT-EXIST controls behavior when file is missing (:error or nil).
EXTERNAL-FORMAT is accepted but ignored (UTF-8 assumed)."
  (declare (ignore external-format))
  (when (and (eq if-does-not-exist nil) (not (probe-file pathname)))
    (return-from our-load nil))
  (let ((path (namestring (truename pathname))))
    (when verbose
      (format *standard-output* "; Loading ~A~%" path))
    (let ((source (with-open-file (in path :direction :input)
                    (let ((buf (make-string (file-length in))))
                      (read-sequence buf in)
                      buf))))
      ;; Pre-scan for (in-package ...) to set *package* before parsing,
      ;; so #. read-time eval resolves symbols in the correct package.
      (let* ((pkg-name (%prescan-in-package source))
             (pkg (when pkg-name (find-package (string-upcase pkg-name))))
             (*package* (or pkg *package*))
             (*our-load-host-definition-mode* (%project-source-load-p path)))
        ;; Parse all forms and compile/run each through the REPL pipeline
        (let ((forms (parse-all-forms source))
              (last-result nil))
          (dolist (form forms last-result)
            (let* ((package-form-p (and (consp form) (eq (car form) 'in-package)))
                   (whitespace-symbol-p (%whitespace-symbol-p form))
                   (unsupported-p (and (consp form)
                                       (member (car form) '(deftype defopcode)))))
              (cond
                (package-form-p
                 (let ((pkg (find-package (second form))))
                   (unless pkg
                     (error "Unknown package: ~S" (second form)))
                   (setf *package* pkg)
                   (setf last-result (second form))))
                ((or whitespace-symbol-p unsupported-p)
                 nil)
                (t
                 (setf last-result
                       (handler-case (run-form-repl form)
                         (error (e)
                           (format *error-output* "; Error loading ~A: ~A~%  Form: ~S~%"
                                   path e form)
                           nil)))
                 (when print
                   (format *standard-output* "~S~%" last-result)))))))))))

;;; Register run-string-repl and our-load in the VM host bridge.
;;; (run-string and compile-* are registered in pipeline.lisp.)
(eval-when (:load-toplevel :execute)
  (dolist (sym '(run-string-repl our-load))
    (vm-register-host-bridge sym)))
