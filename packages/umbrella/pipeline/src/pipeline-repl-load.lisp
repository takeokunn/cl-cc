(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Compile — REPL Host-Load and Top-Level Macro Normalization
;;;
;;; Contains: top-level macro normalization helpers, run-form-repl, our-load,
;;; and VM host-bridge registration for REPL loading entry points.
;;;
;;; Persistent REPL state and run-string-repl live in pipeline-repl-state.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(eval-when (:load-toplevel :execute)
  ;; Coverage/instrumented loads can revisit parts of the pipeline in a slightly
  ;; different order than the normal test path. Keep the steady-state contract
  ;; explicit here too: after the pipeline is loaded, macro evaluation must run
  ;; through OUR-EVAL, not host EVAL.
  (setf *macro-eval-fn* #'our-eval))

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
                 (expanded    (if (some #'%form-contains-bootstrap-quasiquote-p body)
                                  (mapcar #'compiler-macroexpand-all body)
                                  body))
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
               *host-only-top-level-form-names*
               :test #'string=)))

(defun %host-only-top-level-file-specific-form-p (form)
  "Return T for project-source top-level forms that are better handled by host EVAL.
At this stage, ordinary top-level DEFUN forms in repo source are more reliable
through host CL than through the VM compile path during OUR-LOAD selfhost runs."
  (and (consp form)
       (symbolp (car form))
       (string= (symbol-name (car form)) "DEFUN")))

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
               *host-only-top-level-macro-names*
               :test #'string=)))

(defun %coerce-host-macro-head (form)
  "Replace FORM's head with the current package's symbol of the same name when available.
This avoids evaluating top-level project macros through stale bootstrap-package symbols."
  (if (and (consp form) (symbolp (car form)))
      (multiple-value-bind (host-sym status)
          (find-symbol (symbol-name (car form)) *package*)
        (if (and host-sym status (macro-function host-sym))
            (cons host-sym (cdr form))
            form))
      form))

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
            *host-only-registration-symbol-names*)
           (%form-contains-symbol-name-p
            form
            *host-only-registration-helper-names*))))

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
  "Like run-string-repl, but accepts an already-parsed S-expression FORM."
  (when (and (consp form) (eq (car form) 'in-package))
    (error "run-form-repl does not handle (in-package ...) forms; caller must dispatch before calling."))
  (%ensure-repl-state)
  (when (and (consp form)
             (symbolp (car form))
             (string= (symbol-name (car form)) "DEFMACRO"))
    (if (and *our-load-host-definition-mode*
             (not (%form-contains-bootstrap-quasiquote-p (cdddr form))))
        (eval form)
        (register-macro (second form)
                        (%make-top-level-host-macro-expander form)))
    (return-from run-form-repl (second form)))
  (when (and (consp form)
             (symbolp (car form))
             (string= (symbol-name (car form)) "OUR-DEFMACRO"))
    (if (not (%form-contains-bootstrap-quasiquote-p (cdddr form)))
        (%normalize-register-macro-form (%our-defmacro->register-macro-form form))
        (register-macro (second form)
                        (%make-top-level-host-macro-expander form)))
    (return-from run-form-repl (second form)))
  (when (and (consp form)
             (symbolp (car form))
             (string= (symbol-name (car form)) "REGISTER-MACRO"))
    (return-from run-form-repl (%normalize-register-macro-form form)))
  (when (and *our-load-host-definition-mode*
             (consp form)
             (%host-only-top-level-macro-name-p (car form)))
    (return-from run-form-repl (eval (%coerce-host-macro-head form))))
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
             (%host-only-top-level-file-specific-form-p form))
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
