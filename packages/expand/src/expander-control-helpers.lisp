(in-package :cl-cc/expand)

;;; Helpers used only by expander-control.lisp.

(defvar *let-binding-expansion-stack* nil)

(defvar *compiler-local-function-names* nil
  "Function names shadowed by local FLET/LABELS bindings during expansion.")

(defun %list-contains-equal (needle haystack)
  "Return T when NEEDLE is EQUAL to an element of HAYSTACK."
  (and (member needle haystack :test #'equal) t))

(defun %expansion-cycle-key (form)
  "Build a stable key for expansion-cycle detection.
Uninterned symbols (gensyms) are canonicalized so alpha-variant forms share a key."
  (labels ((normalize (x)
             (cond
               ((consp x)
                (cons (normalize (car x)) (normalize (cdr x))))
               ((symbolp x)
                (if (null (symbol-package x))
                    :|<GENSYM>|
                    x))
               (t x))))
    (normalize form)))

(defun %declaration-form-p (form)
  (and (consp form)
       (symbolp (car form))
       (string= (symbol-name (car form)) "DECLARE")))

(defun %ignore-form-env-declaration-p (form)
  (and (%declaration-form-p form)
       (consp (second form))
       (= (length (second form)) 3)
       (let ((decl (second form)))
         (and (symbolp (first decl)) (string= (symbol-name (first decl)) "IGNORE")
              (symbolp (second decl)) (string= (symbol-name (second decl)) "FORM")
              (symbolp (third decl)) (string= (symbol-name (third decl)) "ENV")))))

(defun %expand-let-body-form (form)
  (if (%ignore-form-env-declaration-p form)
      form
      (compiler-macroexpand-all form)))

(defun expand-let-binding (b)
  "Macro-expand the value in a LET binding, leaving the binding name untouched."
  (if (and (consp b) (symbolp (car b)))
      (let ((init (cadr b)))
        (let ((key (%expansion-cycle-key init)))
          (if (%list-contains-equal key *let-binding-expansion-stack*)
            (list (car b) init)
            (let ((*let-binding-expansion-stack*
                    (cons key *let-binding-expansion-stack*)))
              (list (car b) (compiler-macroexpand-all init))))))
      b))

(defun expand-flet-labels-binding (binding)
  "Macro-expand only the body forms of an FLET/LABELS binding; leave params untouched."
  (if (and (consp binding) (>= (length binding) 3))
      (let ((params (second binding))
            (rest (cddr binding)))
        (if (and (consp rest) (symbolp (first rest)) (eq (first rest) '&body))
            (list* (first binding) params
                   (mapcar #'compiler-macroexpand-all (cdr rest)))
            (list* (first binding) params
                   (mapcar #'compiler-macroexpand-all rest))))
      binding))

(defun %flet-labels-binding-names (bindings)
  "Return local function names from FLET/LABELS BINDINGS."
  (loop for binding in bindings
        when (and (consp binding) (symbolp (first binding)))
          collect (first binding)))

(defun %with-local-function-expansion-shadows (names thunk)
  "Call THUNK while preventing compiler macros for local function NAMES."
  (let ((*compiler-local-function-names*
          (append names *compiler-local-function-names*)))
    (funcall thunk)))

(defun %apply-final-list-form-p (form)
  "Return T when FORM is a source-level (LIST ...) spread for APPLY."
  (and (consp form)
       (symbolp (car form))
       (eq (car form) 'list)))

(defun %expand-apply-form (form)
  "Expand APPLY while preserving final (LIST ...) spreads for FR-044 codegen."
  (if (cdr form)
      (let* ((operands (cdr form))
             (leading (butlast operands))
             (spread (car (last operands))))
        (append (list 'apply)
                (mapcar #'compiler-macroexpand-all leading)
                (list (if (%apply-final-list-form-p spread)
                          (cons (car spread)
                                (mapcar #'compiler-macroexpand-all (cdr spread)))
                          (compiler-macroexpand-all spread)))))
      form))

(defun %any-destructuring-let-binding-p (bindings)
  "Return T when BINDINGS contains a destructuring LET binding."
  (some (lambda (b) (and (consp b) (>= (length b) 2) (consp (first b))))
        bindings))

(defun %expand-let-form (form)
  "Expand LET forms, preserving destructuring semantics and body expansion."
  (cond
    ((and (>= (length form) 2) (listp (second form)) (null (second form)))
     (cons 'progn (mapcar #'%expand-let-body-form (cddr form))))
    ((and (>= (length form) 2) (listp (second form))
          (%any-destructuring-let-binding-p (second form)))
     (loop for b in (second form)
           if (and (consp b) (>= (length b) 2) (consp (first b)))
             collect b into destructuring
           else
             collect b into simple
           finally
           (return
       (let ((inner (if simple
                        (list* 'let
                               (mapcar #'expand-let-binding simple)
                               (cddr form))
                        (cons 'progn (cddr form)))))
         (dolist (d (reverse destructuring))
           (setf inner (list 'destructuring-bind (first d) (second d) inner)))
         inner))))
    ((and (>= (length form) 2) (listp (second form)))
     (list* 'let
             (mapcar #'expand-let-binding (second form))
             (mapcar #'%expand-let-body-form (cddr form))))
    (t
     (cons 'let (mapcar #'compiler-macroexpand-all (cdr form))))))

(defun %expand-flet-or-labels (head form)
  "Expand FLET/LABELS bodies while preserving binding structure."
  (if (and (>= (length form) 3) (listp (second form)))
      (if (null (second form))
          (cons 'progn (mapcar #'compiler-macroexpand-all (cddr form)))
          (let* ((bindings (second form))
                 (names (%flet-labels-binding-names bindings))
                 (body-expander (lambda ()
                                  (mapcar #'compiler-macroexpand-all (cddr form))))
                 (binding-expander (lambda ()
                                     (mapcar #'expand-flet-labels-binding bindings))))
            (list* head
                   (if (eq head 'labels)
                       (%with-local-function-expansion-shadows names binding-expander)
                       (funcall binding-expander))
                   (%with-local-function-expansion-shadows names body-expander))))
      (cons head (mapcar #'compiler-macroexpand-all (cdr form)))))

(defun %expand-handler-case-form (form)
  "Expand HANDLER-CASE, including the :NO-ERROR clause lowering."
  (let* ((protected (second form))
         (all-clauses (cddr form))
         (no-error-clause (find :no-error all-clauses :key #'car))
         (error-clauses (remove :no-error all-clauses :key #'car)))
    (if no-error-clause
        (let ((tag (gensym "NO-ERROR-"))
              (ne-vars (second no-error-clause))
              (ne-body (cddr no-error-clause)))
          (compiler-macroexpand-all
           (let ((result-var (if (and ne-vars (car ne-vars)) (car ne-vars) (gensym "R-"))))
             (list 'block tag
                   (list 'let
                         (list
                          (list result-var
                                (cons 'handler-case
                                      (cons protected
                                            (mapcar (lambda (c)
                                                      (list (first c) (second c)
                                                            (list 'return-from tag
                                                                  (cons 'progn (cddr c)))))
                                                    error-clauses)))))
                         (if ne-body
                             (cons 'progn ne-body)
                             nil))))))
        (cons 'handler-case (mapcar #'compiler-macroexpand-all (cdr form))))))
