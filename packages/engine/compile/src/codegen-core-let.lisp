(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Let-Binding Analysis Layer
;;;
;;; Contains:
;;;   - AST predicate helpers (cons/array/ignored detection)
;;;   - Sink-if candidate analysis (%ast-let-sink-if-candidate)
;;;   - %define-binding-walker macro + three generated walkers:
;;;       %array-binding-static-access-p
;;;       %instance-binding-static-slot-only-p
;;;       %closure-binding-direct-call-only-p
;;;
;;; Classification predicates, binding emitters, and compile-ast (ast-let)
;;; are in codegen-core-let-emit.lisp (loads after).
;;;
;;; Load order: after codegen-core.lisp, before codegen-core-let-emit.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── AST predicate helpers ────────────────────────────────────────────────

(defun %ast-let-binding-ignored-p (name declarations)
  (some (lambda (decl)
          (and (consp decl)
               (member (car decl) '(ignore ignorable))
               (member name (cdr decl) :test #'eq)))
        declarations))

(defun %ast-call-named-p (node fn-name nargs)
  "True if NODE is an ast-call to a function named FN-NAME with exactly NARGS arguments.
FN-NAME is compared case-insensitively via SYMBOL-NAME so both symbol and ast-var funcs match."
  (and (typep node 'ast-call)
       (= (length (ast-call-args node)) nargs)
       (let ((func (ast-call-func node)))
         (flet ((name= (sym) (string= (symbol-name sym) fn-name)))
           (or (and (symbolp func)             (name= func))
               (and (typep func 'ast-var) (name= (ast-var-name func))))))))

(defun %ast-cons-call-p (node)
  (%ast-call-named-p node "CONS" 2))

(defun %ast-make-array-call-p (node)
  (%ast-call-named-p node "MAKE-ARRAY" 1))

(defun %ast-make-array-int-call-p (node)
  (and (%ast-make-array-call-p node)
       (typep (first (ast-call-args node)) 'ast-int)))

(defun %binding-mentioned-in-body-p (body-forms binding-name)
  (and (listp body-forms)
       (find binding-name
             (find-free-variables (make-ast-progn :forms body-forms))
             :test #'eq)))

(defun %ast-lambda-bound-names (node)
  (append (copy-list (ast-lambda-params node))
          (loop for spec in (ast-lambda-optional-params node)
                collect (if (consp spec) (first spec) spec))
          (when (ast-lambda-rest-param node)
            (list (ast-lambda-rest-param node)))
          (loop for spec in (ast-lambda-key-params node)
                collect (let ((name (if (consp spec) (first spec) spec)))
                          (if (consp name) (second name) name)))))

(defun %ast-as-body-forms (node)
  (if (typep node 'ast-progn)
      (ast-progn-forms node)
      (list node)))

(defun %ast-wrap-bindings (bindings body)
  (if bindings
      (make-ast-let :bindings bindings :body body)
      (if (= (length body) 1)
          (first body)
          (make-ast-progn :forms body))))

(defun %sink-if-branch-body (if-node branch)
  (ecase branch
    (:then (%ast-as-body-forms (ast-if-then if-node)))
    (:else (%ast-as-body-forms (ast-if-else if-node)))))

(defun %sink-if-wrap-branch (if-node branch binding branch-forms outer-bindings)
  (let ((sunken (make-ast-let :bindings (list binding) :body branch-forms)))
    (%ast-wrap-bindings
     outer-bindings
     (list (make-ast-if :cond (ast-if-cond if-node)
                        :then (if (eq branch :then) sunken (ast-if-then if-node))
                        :else (if (eq branch :else) sunken (ast-if-else if-node)))))))

(defun %sink-if-branch-uses-p (then-uses else-uses branch)
  (ecase branch
    (:then (and then-uses (not else-uses)))
    (:else (and else-uses (not then-uses)))))

(defun %sink-if-build-branch (if-node binding branch outer-bindings)
  (%sink-if-wrap-branch if-node
                        branch
                        binding
                        (%sink-if-branch-body if-node branch)
                        outer-bindings))

(defun %sink-if-instance-slot-names (expr)
  (loop for initarg in (ast-make-instance-initargs expr)
        collect (symbol-name (car initarg))))

(defun %sink-if-array-candidate-p (expr if-node branch name then-uses else-uses)
  (and (%ast-make-array-call-p expr)
       (%sink-if-branch-uses-p then-uses else-uses branch)
       (%array-binding-static-access-p (%sink-if-branch-body if-node branch) name nil)))

(defun %sink-if-instance-candidate-p (expr if-node branch name then-uses else-uses)
  (and (typep expr 'ast-make-instance)
       (%sink-if-branch-uses-p then-uses else-uses branch)
       (%instance-binding-static-slot-only-p (%sink-if-branch-body if-node branch)
                                             name
                                             (%sink-if-instance-slot-names expr))))

(defun %sink-if-cons-candidate-p (expr if-node branch name then-uses else-uses)
  (and (%ast-cons-call-p expr)
       (%sink-if-branch-uses-p then-uses else-uses branch)
       (not (binding-escapes-in-body-p (%sink-if-branch-body if-node branch)
                                       name
                                       :safe-consumers '("CAR" "CDR")))))

;;; Data table: predicates tried in order; each is tried for :then then :else.
;;; Separating predicates from the branch axis eliminates the 3×2 Cartesian enumeration.
(defparameter *sink-if-candidate-predicates*
  (list #'%sink-if-array-candidate-p
        #'%sink-if-instance-candidate-p
        #'%sink-if-cons-candidate-p)
  "Predicates for sinking a let binding into an if branch.")

(defun %sink-if-binding-candidate (if-node binding outer-bindings then-uses else-uses)
  (let ((name (car binding))
        (expr (cdr binding)))
    (loop for pred-fn in *sink-if-candidate-predicates*
          thereis (loop for branch in '(:then :else)
                        when (funcall pred-fn expr if-node branch name then-uses else-uses)
                          return (%sink-if-build-branch if-node binding branch outer-bindings)))))

;;; ── Sink-if candidate analysis ───────────────────────────────────────────
;;;
;;; When a let has exactly one binding and exactly one body form that is an
;;; IF, and the binding is used in only one branch, we can sink the binding
;;; into that branch to reduce unnecessary allocation.

(defun %ast-let-sink-if-candidate (node)
  (let ((bindings (ast-let-bindings node))
        (body     (ast-let-body node)))
    (unless (and (= (length body) 1) (typep (first body) 'ast-if))
      (return-from %ast-let-sink-if-candidate nil))
    (let* ((if-node    (first body))
           (then-forms (%ast-as-body-forms (ast-if-then if-node)))
           (else-forms (%ast-as-body-forms (ast-if-else if-node))))
      (loop for binding in bindings
            for idx from 0
            for name = (car binding)
            for then-uses = (%binding-mentioned-in-body-p then-forms name)
            for else-uses = (%binding-mentioned-in-body-p else-forms name)
            for outer-bindings = (append (subseq bindings 0 idx)
                                         (subseq bindings (1+ idx)))
            for candidate = (unless (and then-uses else-uses)
                              (%sink-if-binding-candidate if-node binding outer-bindings
                                                          then-uses else-uses))
            when candidate
              return candidate))))

;;; Binding noescape walkers are split into codegen-core-let-walkers.lisp.
