(in-package :cl-cc/optimize)

;;; ─── FR-209/210/211 Partial Evaluation Helper Layer ────────────────────────

(defun %opt-normalize-binding-cell-value (cell)
  (let ((tail (cdr cell)))
    (if (and (consp tail) (null (cdr tail)))
        (car tail)
        tail)))

(defun %opt-binding-value (key bindings)
  (cond
    ((hash-table-p bindings)
     (gethash key bindings))
    (bindings
     (let ((cell (assoc key bindings :test #'equal)))
       (if cell
           (values (%opt-normalize-binding-cell-value cell) t)
           (values nil nil))))
    (t (values nil nil))))

(defun %opt-parameter-constant (parameter index constant-bindings)
  (multiple-value-bind (value present-p)
      (%opt-binding-value parameter constant-bindings)
    (if present-p
        (values value t)
        (%opt-binding-value index constant-bindings))))

(defun %opt-constant-binding-signature (parameters constant-bindings)
  (loop for parameter in parameters
        for index from 0
        append (multiple-value-bind (value present-p)
                   (%opt-parameter-constant parameter index constant-bindings)
                 (when present-p
                   (list (cons parameter value))))))

(defun %opt-remove-shadowed-bindings (signature shadowed)
  (remove-if (lambda (cell)
               (member (car cell) shadowed :test #'equal))
             signature))

(defun %opt-lambda-binding-symbol (item)
  (cond
    ((and (symbolp item)
          (not (member item '(&optional &rest &key &aux &body &whole
                              &environment &allow-other-keys)
                       :test #'eq)))
     item)
    ((and (consp item) (symbolp (car item)))
     (car item))
    (t nil)))

(defun %opt-let-binding-symbol (binding)
  (cond
    ((symbolp binding) binding)
    ((and (consp binding) (symbolp (car binding))) (car binding))
    (t nil)))

(defun %opt-substitute-let-bindings (bindings signature sequential-p substitute-fn)
  (let ((lookup-signature signature)
        (eval-signature signature)
        (shadowed nil)
        (result nil))
    (dolist (binding bindings)
      (let ((var (%opt-let-binding-symbol binding)))
        (push (if (and (consp binding) (cdr binding))
                  (multiple-value-bind (value-form updated-signature)
                      (funcall substitute-fn (second binding) lookup-signature)
                    (setf lookup-signature updated-signature
                          eval-signature updated-signature)
                    (list var value-form))
                  binding)
              result)
        (when var
          (push var shadowed)
          (when sequential-p
            (setf lookup-signature
                  (%opt-remove-shadowed-bindings lookup-signature (list var)))))))
    (values (nreverse result)
            (if sequential-p
                lookup-signature
                (%opt-remove-shadowed-bindings lookup-signature shadowed))
            eval-signature
            shadowed)))

(defun %opt-merge-body-effects-into-outer-signature (outer-signature body-signature shadowed)
  (let* ((shadowed-set shadowed)
         (result nil))
    (dolist (cell outer-signature)
      (let* ((var (car cell))
             (body-cell (assoc var body-signature :test #'equal)))
        (cond
          ((member var shadowed-set :test #'equal)
           (push cell result))
          (body-cell
           (push (cons var (cdr body-cell)) result))
          (t
           nil))))
    (dolist (cell body-signature)
      (let ((var (car cell)))
        (when (and (not (member var shadowed-set :test #'equal))
                   (null (assoc var result :test #'equal)))
          (push cell result))))
    (nreverse result)))

(defun %opt-tree-substitute-constants (form signature)
  (labels ((contains-setq-p (node)
             (cond
               ((atom node) nil)
               ((eq (car node) 'quote) nil)
               ((eq (car node) 'function) nil)
               ((eq (car node) 'setq) t)
               (t (some #'contains-setq-p node))))
           (substitute-body (forms active-signature)
             (let ((current-signature active-signature)
                   (result nil))
                (dolist (body-form forms)
                  (multiple-value-bind (new-form new-signature)
                      (substitute-node body-form current-signature)
                   (push new-form result)
                   (setf current-signature new-signature)))
               (values (nreverse result) current-signature)))
           (substitute-setq (pairs active-signature)
             (let ((current-signature active-signature)
                   (result nil))
                (loop for (place value) on pairs by #'cddr
                      do (push place result)
                         (multiple-value-bind (value-form updated-signature)
                             (substitute-node value current-signature)
                           (push value-form result)
                           (setf current-signature
                                 (if (contains-setq-p value)
                                     nil
                                     updated-signature)))
                         (when (symbolp place)
                           (setf current-signature
                                 (%opt-remove-shadowed-bindings current-signature
                                                                (list place)))))
               (values (cons 'setq (nreverse result)) current-signature)))
           (substitute-node (node active-signature)
             (cond
               ((symbolp node)
                (let ((cell (assoc node active-signature :test #'equal)))
                  (values (if cell (cdr cell) node)
                          active-signature)))
               ((atom node)
                (values node active-signature))
               ((eq (car node) 'quote)
                (values node active-signature))
               ((eq (car node) 'function)
                (values node active-signature))
               ((eq (car node) 'lambda)
                (let* ((lambda-list (second node))
                       (shadowed (remove nil
                                         (mapcar #'%opt-lambda-binding-symbol
                                                 lambda-list)))
                       (body-signature
                         (%opt-remove-shadowed-bindings active-signature shadowed)))
                  (multiple-value-bind (new-body _)
                      (substitute-body (cddr node) body-signature)
                    (declare (ignore _))
                    (values (list* 'lambda lambda-list new-body)
                            active-signature))))
               ((eq (car node) 'progn)
                (multiple-value-bind (new-body updated-signature)
                    (substitute-body (cdr node) active-signature)
                  (values (cons 'progn new-body) updated-signature)))
               ((member (car node) '(let let*) :test #'eq)
                (multiple-value-bind (bindings body-signature outward-signature shadowed)
                    (%opt-substitute-let-bindings
                     (second node)
                     active-signature
                     (eq (car node) 'let*)
                     #'substitute-node)
                  (multiple-value-bind (new-body body-updated-signature)
                      (substitute-body (cddr node) body-signature)
                    (let ((merged-signature
                            (%opt-merge-body-effects-into-outer-signature
                             outward-signature
                             body-updated-signature
                             shadowed)))
                      (values (list* (car node) bindings new-body)
                              merged-signature)))))
               ((eq (car node) 'setq)
                (substitute-setq (cdr node) active-signature))
               ((symbolp (car node))
                (multiple-value-bind (new-args updated-signature)
                    (substitute-body (cdr node) active-signature)
                  (values (cons (car node) new-args)
                          updated-signature)))
               (t
                (multiple-value-bind (new-seq updated-signature)
                    (substitute-body node active-signature)
                  (values new-seq updated-signature))))))
    (substitute-node form signature)))

(defun %opt-dynamic-parameters (parameters signature)
  (remove-if (lambda (parameter)
               (assoc parameter signature :test #'equal))
             parameters))

(defun %opt-specialized-name (function-name signature)
  (format nil "~A__spec__~A"
          (%opt-stable-print-string function-name)
          (%opt-stable-print-string signature)))

(defun %opt-body-vm-instructions-p (body)
  (every (lambda (node) (typep node 'vm-instruction)) body))

(defun %opt-build-vm-residual-body (body signature)
  "Build residual VM body by materializing static parameter values as vm-const.

When BODY is VM instruction objects (as used by FR-211 clone emission),
tree-level substitution is not sufficient. We conservatively emit one vm-const
per static binding at function entry, preserving original instruction order."
  (append
   (loop for (parameter . value) in signature
         collect (make-vm-const :dst parameter :value value))
   body))

(defun opt-specialize-constant-args (function-name parameters body constant-bindings
                                     &key specialized-name)
  "Build a residual helper copy of BODY with constant PARAMETERS substituted.

This is a helper-level partial-evaluation primitive: it does not execute code or
install a clone in the function registry. It records the static signature and
returns a residual body that later passes can fold safely."
  (let* ((signature (%opt-constant-binding-signature parameters constant-bindings))
          (name (or specialized-name
                    (%opt-specialized-name function-name signature))))
    (make-opt-partial-specialization
     :original-name function-name
     :specialized-name name
     :signature signature
     :static-args signature
     :dynamic-args (%opt-dynamic-parameters parameters signature)
     :residual-body (if (%opt-body-vm-instructions-p body)
                        (%opt-build-vm-residual-body body signature)
                        (cdr (%opt-tree-substitute-constants
                              (cons 'progn body)
                              signature))))))

(defun opt-partial-evaluate-function (function-name parameters body
                                      &key
                                        (constant-bindings nil)
                                        (lattice-bindings nil)
                                        specialized-name)
  "Partially evaluate one function body and return residual + BTA report.

This is a function-level FR-209/210 entrypoint that composes:
1) constant substitution residualization,
2) binding-time analysis merge, and
3) offline BTA classification over residual forms."
  (let* ((specialization
           (opt-specialize-constant-args
            function-name parameters body constant-bindings
            :specialized-name specialized-name))
         (binding-times
           (opt-run-binding-time-analysis
            parameters
            :constant-bindings constant-bindings
            :lattice-bindings lattice-bindings))
         (residual-body (opt-partial-spec-residual-body specialization))
         (form-kinds
           (opt-offline-bta-analyze-body
            residual-body
            :static-bindings (opt-partial-spec-signature specialization)
            :binding-times binding-times))
         (dynamic-body
           (loop for form in residual-body
                 for kind in form-kinds
                 unless (eq kind :static)
                 collect form)))
    (make-opt-partial-eval-result
     :function-name function-name
     :parameters parameters
     :signature (opt-partial-spec-signature specialization)
     :binding-times binding-times
     :form-kinds form-kinds
     :residual-body residual-body
     :dynamic-body dynamic-body
     :specialization specialization)))

(defun %opt-merge-constant-binding (const-map fn param value)
  "Merge inferred (PARAM . VALUE) into CONST-MAP for FN.

Returns T when map changed. Conflicting values conservatively drop knowledge."
  (let* ((fn-bindings (or (gethash fn const-map)
                          (setf (gethash fn const-map) (make-hash-table :test #'equal))))
         (present (nth-value 1 (gethash param fn-bindings))))
    (cond
      ((not present)
       (setf (gethash param fn-bindings) value)
       t)
      ((equal (gethash param fn-bindings) value)
       nil)
      (t
       ;; Conflict => unknown for that parameter.
       (remhash param fn-bindings)
       t))))

(defun %opt-extract-constant-calls-from-form (form static-signature)
  "Collect conservative call-site constant bindings from FORM.

Returns alist entries: (callee . ((param-index . const-value) ...))"
  (labels ((const-value (node)
             (cond
               ((symbolp node)
                (let ((cell (assoc node static-signature :test #'equal)))
                  (if cell (cdr cell) :unknown)))
               ((or (numberp node) (stringp node) (characterp node)
                    (keywordp node) (member node '(nil t) :test #'eq))
                node)
               ((and (consp node) (eq (car node) 'quote))
                (second node))
               (t :unknown)))
           (walk (node)
             (cond
               ((atom node) nil)
               ((member (car node) '(quote function) :test #'eq) nil)
               (t
                (append
                 (let ((head (car node)))
                   (when (symbolp head)
                     (let ((pairs nil))
                       (loop for arg in (cdr node)
                             for i from 0
                             for v = (const-value arg)
                             unless (eq v :unknown)
                             do (push (cons i v) pairs))
                       (when pairs
                         (list (cons head (nreverse pairs)))))))
                 (mapcan #'walk node))))))
    (walk form)))

(defun %opt-build-inferred-constant-bindings (function-definitions reports)
  "Infer inter-function constant bindings from REPORTS residual signatures.

Produces alist: ((fn . ((param . value) ...)) ...)."
  (let ((params-by-fn (make-hash-table :test #'equal))
        (const-map (make-hash-table :test #'equal)))
    (dolist (def function-definitions)
      (setf (gethash (first def) params-by-fn)
            (coerce (getf (rest def) :params) 'list)))
    (dolist (entry reports)
      (let* ((report (cdr entry))
             (sig (opt-partial-eval-signature report)))
        (dolist (form (opt-partial-eval-residual-body report))
          (dolist (call (%opt-extract-constant-calls-from-form form sig))
            (let* ((callee (car call))
                   (idx-vals (cdr call))
                   (params (gethash callee params-by-fn)))
              (when params
                (dolist (iv idx-vals)
                  (let* ((idx (car iv))
                         (value (cdr iv))
                         (param (nth idx params)))
                    (when param
                      (%opt-merge-constant-binding const-map callee param value))))))))))
    (let (result)
      (maphash
       (lambda (fn table)
         (let (pairs)
           (maphash (lambda (k v) (push (cons k v) pairs)) table)
           (push (cons fn (nreverse pairs)) result)))
       const-map)
      (nreverse result))))

(defun %opt-merge-constant-binding-alists (base inferred)
  "Merge INFERRED bindings into BASE conservatively.

BASE values are kept when conflicts arise; INFERRED only adds missing facts."
  (let ((result (copy-tree base)))
    (dolist (entry inferred result)
      (let* ((fn (car entry))
             (new-pairs (cdr entry))
             (cell (assoc fn result :test #'equal)))
        (if cell
            (dolist (pair new-pairs)
              (unless (assoc (car pair) (cdr cell) :test #'equal)
                (setf (cdr cell) (append (cdr cell) (list pair)))))
            (push (cons fn (copy-list new-pairs)) result))))))

(defun opt-partial-evaluate-program (function-definitions
                                     &key
                                       (constant-bindings-by-function nil)
                                       (lattice-bindings-by-function nil)
                                       (max-iterations 64))
  "Run function-level partial evaluation across FUNCTION-DEFINITIONS.

FUNCTION-DEFINITIONS format:
  ((fn-name :params (...) :body (...)) ...)

CONSTANT-BINDINGS-BY-FUNCTION and LATTICE-BINDINGS-BY-FUNCTION are alists:
  ((fn-name . ((param . value) ...)) ...)
  ((fn-name . ((param . lattice) ...)) ...)

Returns OPT-PARTIAL-PROGRAM-RESULT with per-function reports.

Performs a monotone inter-function fixpoint: inferred constants from residual
call-sites are propagated across function boundaries until convergence.
MAX-ITERATIONS is a safety guard for pathological inputs." 
  (let ((current-consts constant-bindings-by-function)
        (reports nil))
    (loop for iter from 1
          while (<= iter (max 1 max-iterations))
          do (setf reports
                   (loop for def in function-definitions
                         for fn = (first def)
                         for params = (getf (rest def) :params)
                         for body = (getf (rest def) :body)
                         for consts = (cdr (assoc fn current-consts :test #'equal))
                         for lattices = (cdr (assoc fn lattice-bindings-by-function :test #'equal))
                         collect (cons fn
                                       (opt-partial-evaluate-function
                                        fn params body
                                        :constant-bindings consts
                                        :lattice-bindings lattices))))
             (let* ((inferred (%opt-build-inferred-constant-bindings function-definitions reports))
                    (next-consts (%opt-merge-constant-binding-alists current-consts inferred)))
               (if (equal next-consts current-consts)
                   (return)
                   (setf current-consts next-consts))))
    (make-opt-partial-program-result :function-results reports)))

(defun %opt-lattice-binding-time-kind (lattice)
  (if (and (opt-lattice-value-p lattice)
           (eq (opt-lattice-value-kind lattice) :constant))
      :static
      :dynamic))

(defun opt-sccp-analyze-binding-times (parameters lattice-bindings)
  "Classify PARAMETERS as :STATIC or :DYNAMIC using SCCP lattice bindings."
  (loop for parameter in parameters
        for index from 0
        collect (multiple-value-bind (lattice present-p)
                    (%opt-parameter-constant parameter index lattice-bindings)
                  (let ((kind (if present-p
                                  (%opt-lattice-binding-time-kind lattice)
                                  :dynamic)))
                    (make-opt-binding-time
                     :parameter parameter
                     :kind kind
                     :value (and (eq kind :static)
                                 (opt-lattice-value-value lattice))
                     :lattice (and present-p lattice))))))

(defun opt-run-binding-time-analysis (parameters
                                      &key
                                        (constant-bindings nil)
                                        (lattice-bindings nil))
  "Run a conservative binding-time analysis for PARAMETERS.

Priority:
1) CONSTANT-BINDINGS are treated as compile-time static facts.
2) Remaining parameters are classified from LATTICE-BINDINGS via SCCP.

This provides an explicit BTA entrypoint (FR-210) that can be used by
partial-evaluation passes without requiring callers to manually merge sources."
  (let* ((seed (loop for (name . value) in constant-bindings
                     collect (cons name (opt-lattice-constant value))))
         (merged-lattice (append seed lattice-bindings)))
    (opt-sccp-analyze-binding-times parameters merged-lattice)))

(defun %opt-offline-bta-constant-atom-p (node static-set)
  (cond
    ((symbolp node)
     (or (member node '(nil t) :test #'eq)
         (keywordp node)
         (member node static-set :test #'equal)))
    (t (constantp node))))

(defun %opt-offline-bta-classify-form (form static-set)
  (labels ((all-static-p (forms env)
             (every (lambda (f)
                      (eq (%opt-offline-bta-classify-form f env) :static))
                    forms))
           (binding-symbol (binding)
             (cond
               ((symbolp binding) binding)
               ((and (consp binding) (symbolp (car binding))) (car binding))
               (t nil))))
    (cond
      ((atom form)
       (if (%opt-offline-bta-constant-atom-p form static-set) :static :dynamic))
      ((member (car form) '(quote function) :test #'eq)
       :static)
      ((eq (car form) 'if)
       (if (all-static-p (cdr form) static-set) :static :dynamic))
      ((eq (car form) 'progn)
       (if (all-static-p (cdr form) static-set) :static :dynamic))
      ((member (car form) '(let let*) :test #'eq)
       (let* ((bindings (second form))
              (new-static static-set))
         (dolist (binding bindings)
           (let* ((var (binding-symbol binding))
                  (rhs (if (and (consp binding) (cdr binding)) (second binding) nil))
                  (rhs-static-p (or (null rhs)
                                    (eq (%opt-offline-bta-classify-form rhs static-set)
                                        :static))))
             (when (and var rhs-static-p)
               (push var new-static))))
         (if (all-static-p (cddr form) new-static) :static :dynamic)))
      ((eq (car form) 'setq)
       (if (all-static-p (loop for (_ v) on (cdr form) by #'cddr
                               collect v)
                         static-set)
           :static
           :dynamic))
      ((and (symbolp (car form))
            (member (car form) *opt-offline-bta-pure-operators* :test #'eq))
       (if (all-static-p (cdr form) static-set) :static :dynamic))
      (t :dynamic))))

(defun opt-offline-bta-classify-form (form
                                      &key
                                        (static-bindings nil)
                                        (binding-times nil))
  "Classify FORM as :STATIC or :DYNAMIC using an offline BTA approximation.

STATIC-BINDINGS are explicit compile-time facts `(var . value)`.
BINDING-TIMES may include `opt-binding-time` entries (e.g. from SCCP merge).
Only bindings classified as :static are treated as compile-time-known names."
  (let ((static-set
          (append
           (mapcar #'car static-bindings)
           (loop for bt in binding-times
                 when (and (opt-binding-time-p bt)
                           (eq (opt-binding-time-kind bt) :static))
                 collect (opt-binding-time-parameter bt)))))
    (%opt-offline-bta-classify-form form static-set)))

(defun opt-offline-bta-analyze-body (body
                                     &key
                                       (static-bindings nil)
                                       (binding-times nil))
  "Classify each form in BODY as :STATIC or :DYNAMIC via offline BTA."
  (mapcar (lambda (form)
            (opt-offline-bta-classify-form
             form
             :static-bindings static-bindings
             :binding-times binding-times))
          body))

(defun opt-build-specialization-plan (callee-label arguments constant-bindings
                                      &key cache)
  "Build a conservative clone/call-redirection plan for known constant arguments.

Returns NIL when ARGUMENTS have no known constants. When CACHE is supplied, the
same `(callee . signature)` pair reuses the earlier specialized name and marks
the plan as a cache hit instead of requesting a new clone."
  (let ((signature (%opt-constant-binding-signature arguments constant-bindings)))
    (when signature
      (let* ((cache-key (list callee-label signature))
             (cached-name nil)
             (cache-hit-p nil))
        (when cache
          (multiple-value-setq (cached-name cache-hit-p)
            (gethash cache-key cache)))
        (let ((specialized-name (or cached-name
                                    (%opt-specialized-name callee-label signature))))
          (when (and cache (not cache-hit-p))
            (setf (gethash cache-key cache) specialized-name))
          (make-opt-specialization-plan
           :callee-label callee-label
           :specialized-name specialized-name
           :signature signature
           :static-args signature
           :dynamic-args (%opt-dynamic-parameters arguments signature)
           :clone-needed-p (not cache-hit-p)
           :cache-hit-p cache-hit-p))))))

(defun %opt-constant-bindings-from-call-args (params args const-track)
  (let ((result nil))
    (loop for param in params
          for arg in args
          do (multiple-value-bind (value present-p)
                 (gethash arg const-track)
               (when present-p
                 (push (cons param value) result))))
    (nreverse result)))

(defun %opt-dynamic-call-args (params args dynamic-params)
  (let ((result nil))
    (loop for param in params
          for arg in args
          do (when (member param dynamic-params :test #'equal)
               (push arg result)))
    (nreverse result)))

(defun %opt-make-call-like (inst func-reg args)
  (cond
    ((typep inst 'vm-tail-call)
     (make-vm-tail-call :dst (vm-dst inst) :func func-reg :args args))
    ((typep inst 'vm-apply)
     (make-vm-apply :dst (vm-dst inst)
                    :func func-reg
                    :args args
                    :tail-p (cl-cc/vm::vm-tail-p inst)))
    (t
     (make-vm-call :dst (vm-dst inst) :func func-reg :args args))))
