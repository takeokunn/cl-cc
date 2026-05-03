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
  (let ((decls declarations))
    (tagbody
     scan-decls
       (if (null decls) (return-from %ast-let-binding-ignored-p nil))
       (let ((decl (car decls)))
         (if (and (consp decl)
                  (or (eq (car decl) 'ignore) (eq (car decl) 'ignorable)))
             (let ((names (cdr decl)))
               (tagbody
                scan-names
                  (if (null names) (go done-names))
                  (if (eq name (car names))
                      (return-from %ast-let-binding-ignored-p t))
                  (setq names (cdr names))
                  (go scan-names)
                done-names))))
       (setq decls (cdr decls))
       (go scan-decls))))

(defun %ast-call-named-p (node fn-name nargs)
  "True if NODE is an ast-call to a function named FN-NAME with exactly NARGS arguments.
FN-NAME is compared case-insensitively via SYMBOL-NAME so both symbol and ast-var funcs match."
  (and (typep node 'ast-call)
       (= (length (ast-call-args node)) nargs)
       (let ((func (ast-call-func node)))
         (or (and (symbolp func)        (string= (symbol-name func) fn-name))
             (and (typep func 'ast-var) (string= (symbol-name (ast-var-name func)) fn-name))))))

(defun %ast-cons-call-p (node)
  (%ast-call-named-p node "CONS" 2))

(defun %ast-make-array-call-p (node)
  (%ast-call-named-p node "MAKE-ARRAY" 1))

(defun %ast-make-array-int-call-p (node)
  (and (%ast-make-array-call-p node)
       (typep (first (ast-call-args node)) 'ast-int)))

(defun %binding-mentioned-in-body-p (body-forms binding-name)
  (if (listp body-forms)
      (let ((vars (find-free-variables (make-ast-progn :forms body-forms))))
        (tagbody
         scan
           (if (null vars) (return-from %binding-mentioned-in-body-p nil))
           (if (eq binding-name (car vars))
               (return-from %binding-mentioned-in-body-p t))
           (setq vars (cdr vars))
           (go scan)))
      nil))

(defun %ast-lambda-bound-names (node)
  (let ((result nil))
    (let ((xs (ast-lambda-params node)))
      (tagbody
       scan-params
         (if (null xs) (go done-params))
         (setq result (cons (car xs) result))
         (setq xs (cdr xs))
         (go scan-params)
       done-params))
    (let ((xs (ast-lambda-optional-params node)))
      (tagbody
       scan-optional
         (if (null xs) (go done-optional))
         (let ((spec (car xs)))
           (setq result (cons (if (consp spec) (car spec) spec) result)))
         (setq xs (cdr xs))
         (go scan-optional)
       done-optional))
    (if (ast-lambda-rest-param node)
        (setq result (cons (ast-lambda-rest-param node) result)))
    (let ((xs (ast-lambda-key-params node)))
      (tagbody
       scan-key
         (if (null xs) (go done-key))
         (let* ((spec (car xs))
                (name (if (consp spec) (car spec) spec))
                (var (if (consp name) (car (cdr name)) name)))
           (setq result (cons var result)))
         (setq xs (cdr xs))
         (go scan-key)
       done-key))
    (nreverse result)))

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
  (if (eq branch :then)
      (%ast-as-body-forms (ast-if-then if-node))
      (if (eq branch :else)
          (%ast-as-body-forms (ast-if-else if-node))
          (error "Unknown sink-if branch: ~S" branch))))

(defun %sink-if-wrap-branch (if-node branch binding branch-forms outer-bindings)
  (let ((sunken (make-ast-let :bindings (list binding) :body branch-forms)))
    (%ast-wrap-bindings
     outer-bindings
     (list (make-ast-if :cond (ast-if-cond if-node)
                        :then (if (eq branch :then) sunken (ast-if-then if-node))
                        :else (if (eq branch :else) sunken (ast-if-else if-node)))))))

(defun %sink-if-branch-uses-p (then-uses else-uses branch)
  (if (eq branch :then)
      (and then-uses (not else-uses))
      (if (eq branch :else)
          (and else-uses (not then-uses))
          (error "Unknown sink-if branch: ~S" branch))))

(defun %sink-if-build-branch (if-node binding branch outer-bindings)
  (%sink-if-wrap-branch if-node
                        branch
                        binding
                        (%sink-if-branch-body if-node branch)
                        outer-bindings))

(defun %sink-if-instance-slot-names (expr)
  (let ((xs (ast-make-instance-initargs expr))
        (result nil))
    (tagbody
     scan
       (if (null xs) (return-from %sink-if-instance-slot-names (nreverse result)))
       (setq result (cons (symbol-name (car (car xs))) result))
       (setq xs (cdr xs))
       (go scan))))

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

(defun %bindings-excluding-index (bindings skip-index)
  (let ((xs bindings)
        (idx 0)
        (result nil))
    (tagbody
     scan
       (if (null xs) (return-from %bindings-excluding-index (nreverse result)))
       (if (not (= idx skip-index))
           (setq result (cons (car xs) result)))
       (setq xs (cdr xs))
       (setq idx (+ idx 1))
       (go scan))))

(defun %sink-if-binding-candidate (if-node binding outer-bindings then-uses else-uses)
  (let ((name (car binding))
        (expr (cdr binding))
        (preds *sink-if-candidate-predicates*))
    (tagbody
     scan-preds
       (if (null preds) (return-from %sink-if-binding-candidate nil))
       (let ((pred-fn (car preds))
             (branches '(:then :else)))
         (tagbody
          scan-branches
            (if (null branches) (go done-branches))
            (let ((branch (car branches)))
              (if (funcall pred-fn expr if-node branch name then-uses else-uses)
                  (return-from %sink-if-binding-candidate
                    (%sink-if-build-branch if-node binding branch outer-bindings))))
            (setq branches (cdr branches))
            (go scan-branches)
          done-branches))
       (setq preds (cdr preds))
       (go scan-preds))))

(defun %ast-let-sink-if-candidate (node)
  (let ((bindings (ast-let-bindings node))
        (body (ast-let-body node)))
    (if (not (and (= (length body) 1) (typep (car body) 'ast-if)))
        (return-from %ast-let-sink-if-candidate nil))
    (let* ((if-node (car body))
           (then-forms (%ast-as-body-forms (ast-if-then if-node)))
           (else-forms (%ast-as-body-forms (ast-if-else if-node)))
           (xs bindings)
           (idx 0))
      (tagbody
       scan
         (if (null xs) (return-from %ast-let-sink-if-candidate nil))
         (let* ((binding (car xs))
                (name (car binding))
                (then-uses (%binding-mentioned-in-body-p then-forms name))
                (else-uses (%binding-mentioned-in-body-p else-forms name)))
           (if (not (and then-uses else-uses))
               (let ((candidate
                       (%sink-if-binding-candidate
                        if-node binding
                        (%bindings-excluding-index bindings idx)
                        then-uses else-uses)))
                 (if candidate
                     (return-from %ast-let-sink-if-candidate candidate)))))
         (setq xs (cdr xs))
         (setq idx (+ idx 1))
         (go scan)))))

;;; Binding noescape walkers are split into codegen-core-let-walkers.lisp.
