(in-package :cl-cc/repl)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Compile — REPL Host-Load and Top-Level Macro Normalization
;;;
;;; Contains: top-level macro normalization helpers, run-form-repl, our-load,
;;; and VM host-bridge registration for REPL loading entry points.
;;;
;;; Persistent REPL state and run-string-repl live in pipeline-repl-state.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar *macro-eval-fn* #'our-eval
  "Current compile-time evaluator hook used by selfhost/REPL expansion.")

(eval-when (:load-toplevel :execute)
  ;; Coverage/instrumented loads can revisit parts of the pipeline in a slightly
  ;; different order than the normal test path. Keep the steady-state contract
  ;; explicit here too: after the pipeline is loaded, macro evaluation must run
  ;; through OUR-EVAL, not host EVAL.
  (setf *macro-eval-fn* #'our-eval))

(defun %normalize-register-macro-form (form)
  "Register a top-level REGISTER-MACRO lambda expander.
Lambda bodies are normalized through COMPILER-MACROEXPAND-ALL before storing the
descriptor in macro-env. Non-lambda register-macro forms are rejected — the
loader never host-evals arbitrary expander forms."
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
                 (descriptor  (list :kind :register-macro-expander
                                    :parameters lambda-list
                                    :body expanded)))
            (register-macro name descriptor)
            name)
          (error "Top-level REGISTER-MACRO requires a lambda expander: ~S" form)))))

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

(defun %skip-redundant-defpackage-p (form)
  "Return T when FORM is a DEFPACKAGE for a package that already exists."
  (and (consp form)
       (symbolp (car form))
       (string= (symbol-name (car form)) "DEFPACKAGE")
       (second form)
       (cl-cc/runtime:rt-find-package (second form))))

(defun %top-level-project-macro-p (symbol)
  "Return T when SYMBOL names a registered project macro."
  (and (symbolp symbol)
       (cl-cc/expand::lookup-macro symbol)))

(defun %coerce-host-macro-head (form)
  "Replace FORM's head with the current package's symbol of the same name when available.
This keeps top-level project macro expansion on the runtime package layer rather than
probing host package/macro tables directly."
  (if (and (consp form) (symbolp (car form)))
      (let ((pkg-sym (cl-cc/runtime:rt-intern (symbol-name (car form)) *package*)))
        (if (cl-cc/expand::lookup-macro pkg-sym)
            (cons pkg-sym (cdr form))
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

(defun %make-top-level-host-macro-expander (form)
  "Build the expander for a top-level DEFMACRO/OUR-DEFMACRO form.
MAKE-MACRO-EXPANDER already normalizes bootstrap quasiquote markers for the
returned expansion. Re-expanding that result with COMPILER-MACROEXPAND-ALL is
too aggressive and can macroexpand inside quasiquoted templates, breaking forms
such as PUSH over an UNQUOTE place during selfhost loading.

Build a descriptor so top-level macro definitions stay on the selfhost path
instead of forcing host-closure execution."
  (let ((*macro-eval-fn* #'our-eval))
    (append (make-macro-expander (third form) (cdddr form))
            (list :post-expand :our-macroexpand-all))))

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

(defun %handle-top-level-defmacro-form (form)
  "Handle a top-level DEFMACRO form for REPL/source loading and return its name."
  (register-macro (second form)
                  (%make-top-level-host-macro-expander form))
  (second form))

(defun %handle-top-level-our-defmacro-form (form)
  "Handle a top-level OUR-DEFMACRO form for REPL/source loading and return its name."
  (if (not (%form-contains-bootstrap-quasiquote-p (cdddr form)))
      (%normalize-register-macro-form (%our-defmacro->register-macro-form form))
      (register-macro (second form)
                      (%make-top-level-host-macro-expander form)))
  (second form))

(defun %handle-host-only-top-level-form (form)
  "Handle host-only top-level forms during project-source loading.
Returns two values: result and handled-p." 
  (cond
    ((and *our-load-host-definition-mode* (%skip-redundant-defpackage-p form))
     (values (second form) t))
    (t (values nil nil))))

(defun run-form-repl (form)
  "Like run-string-repl, but accepts an already-parsed S-expression FORM."
  (when (and (consp form)
             (symbolp (car form))
             (string= (symbol-name (car form)) "IN-PACKAGE"))
    (error "run-form-repl does not handle (in-package ...) forms; caller must dispatch before calling."))
  (%ensure-repl-state)
  (when (and (consp form)
             (symbolp (car form))
             (string= (symbol-name (car form)) "DEFMACRO"))
    (return-from run-form-repl (%handle-top-level-defmacro-form form)))
  (when (and (consp form)
             (symbolp (car form))
             (string= (symbol-name (car form)) "OUR-DEFMACRO"))
    (return-from run-form-repl (%handle-top-level-our-defmacro-form form)))
  (when (and (consp form)
             (symbolp (car form))
             (string= (symbol-name (car form)) "REGISTER-MACRO"))
    (return-from run-form-repl (%normalize-register-macro-form form)))
  (when (and (consp form)
             (symbolp (car form))
             (%top-level-project-macro-p (car form)))
    (multiple-value-bind (expanded expanded-p)
        (if (cl-cc/expand::lookup-macro (car form))
            (cl-cc/expand::our-macroexpand-1 form)
            (macroexpand-1 form))
      (when expanded-p
        (return-from run-form-repl (run-form-repl expanded)))))
  (multiple-value-bind (host-result handled-p) (%handle-host-only-top-level-form form)
    (when handled-p
      (return-from run-form-repl host-result)))
  (let* ((*package* *package*)
         (*macro-eval-fn* #'our-eval)
          (*accessor-slot-map* *repl-accessor-map*)
          (*defstruct-slot-registry* *repl-defstruct-registry*)
          (*labels-boxed-fns* nil)
          (*repl-global-variables* *repl-global-vars-persistent*)
          (*repl-capture-label-counter* t))
    (%run-form-repl-impl (compile-expression form :target :vm))))
