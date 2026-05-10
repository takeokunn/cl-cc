;;;; types-extended-advanced-init.lisp — make-type-advanced, %initialize-* functions, top-level call
(in-package :cl-cc/type)

(defun make-type-advanced (&key feature-id (name 'advanced) args properties evidence)
  "Construct a type-advanced node for FEATURE-ID."
  (let ((feature (or (lookup-type-advanced-feature feature-id)
                     (error "Unknown advanced feature id: ~S" feature-id))))
    (multiple-value-bind (normalized-args normalized-properties)
        (%type-advanced-normalize-graded-keyword-arg
         (type-advanced-feature-fr-id feature)
         args
         (%type-advanced-normalize-properties properties))
      (validate-type-advanced
       (%make-type-advanced :feature-id (type-advanced-feature-fr-id feature)
                            :name name
                            :args (copy-list normalized-args)
                            :properties normalized-properties
                            :evidence evidence)))))

(defun make-type-dynamic (value-type &key properties evidence)
  "Construct a Dynamic feature node (FR-2501) around VALUE-TYPE."
  (make-type-advanced :feature-id "FR-2501"
                      :name 'dynamic
                      :args (list value-type)
                      :properties properties
                      :evidence evidence))

(defun make-type-type-rep (represented-type &key properties evidence)
  "Construct a TypeRep feature node (FR-2502) around REPRESENTED-TYPE."
  (make-type-advanced :feature-id "FR-2502"
                      :name 'typerep
                      :args (list represented-type)
                      :properties properties
                      :evidence evidence))

(defun type-advanced-payload-children (value)
  "Return all type-node values nested inside VALUE."
  (cond
    ((typep value 'type-node) (list value))
    ((consp value)
     (append (type-advanced-payload-children (car value))
             (type-advanced-payload-children (cdr value))))
    (t nil)))

(defun type-advanced-payload-map (fn value)
  "Map FN over each type-node nested inside VALUE, preserving outer shape."
  (cond
    ((typep value 'type-node) (funcall fn value))
    ((consp value)
     (cons (type-advanced-payload-map fn (car value))
           (type-advanced-payload-map fn (cdr value))))
    (t value)))

(defun type-advanced-payload-equal-p (left right)
  "Return T when LEFT and RIGHT are structurally equal advanced payloads."
  (cond
    ((and (typep left 'type-node) (typep right 'type-node))
     (type-equal-p left right))
    ((or (typep left 'type-node) (typep right 'type-node)) nil)
    ((and (consp left) (consp right))
     (and (type-advanced-payload-equal-p (car left) (car right))
          (type-advanced-payload-equal-p (cdr left) (cdr right))))
    (t (equal left right))))

(defun type-advanced-properties-equal-p (props1 props2)
  "Return T when two property alists are key-order-insensitively equal."
  (and (= (length props1) (length props2))
       (loop for (key . val1) in props1
             for entry2 = (assoc key props2)
             always (and entry2
                         (type-advanced-payload-equal-p val1 (cdr entry2))))))

(defun %initialize-type-advanced-contract-registry ()
  "Populate the explicit advanced semantic contract registry."
  (clrhash *type-advanced-contract-registry*)
  (dolist (spec +type-advanced-contract-specs+)
    (register-type-advanced-contract
     (%make-type-advanced-contract-from-spec spec)))
  t)

(defun %initialize-type-advanced-implementation-evidence-registry ()
  "Populate the advanced implementation evidence registry."
  (clrhash *type-advanced-implementation-evidence-registry*)
  (dolist (spec +type-advanced-implementation-evidence-specs+)
    (register-type-advanced-implementation-evidence
     (%make-type-advanced-implementation-evidence-from-spec spec)))
  t)

(defun %ensure-type-advanced-contract-coverage ()
  "Ensure every tracked advanced feature id has exactly one explicit contract."
  (let* ((feature-ids (mapcar #'first +type-advanced-feature-specs+))
         (missing (remove-if #'lookup-type-advanced-contract feature-ids))
         (orphan nil))
    (maphash (lambda (feature-id _contract)
               (declare (ignore _contract))
               (unless (member feature-id feature-ids :test #'string=)
                 (push feature-id orphan)))
             *type-advanced-contract-registry*)
    (when missing
      (error "Missing advanced semantic contract(s) for ~{~A~^, ~}" missing))
    (when orphan
      (error "Orphan advanced semantic contract(s): ~{~A~^, ~}" (sort orphan #'string<)))
    (unless (= (hash-table-count *type-advanced-contract-registry*)
               (length feature-ids))
      (error "Advanced contract registry count (~D) does not match feature registry count (~D)"
             (hash-table-count *type-advanced-contract-registry*)
             (length feature-ids)))
    t))

(defun %ensure-type-advanced-implementation-evidence-coverage ()
  "Ensure every tracked advanced feature id has exactly one evidence record."
  (let* ((feature-ids (mapcar #'first +type-advanced-feature-specs+))
         (missing (remove-if #'lookup-type-advanced-implementation-evidence feature-ids))
         (orphan nil))
    (maphash (lambda (feature-id _evidence)
               (declare (ignore _evidence))
               (unless (member feature-id feature-ids :test #'string=)
                 (push feature-id orphan)))
             *type-advanced-implementation-evidence-registry*)
    (when missing
      (error "Missing advanced implementation evidence for ~{~A~^, ~}" missing))
    (when orphan
      (error "Orphan advanced implementation evidence: ~{~A~^, ~}" (sort orphan #'string<)))
    (unless (= (hash-table-count *type-advanced-implementation-evidence-registry*)
               (length feature-ids))
      (error "Advanced implementation evidence count (~D) does not match feature registry count (~D)"
             (hash-table-count *type-advanced-implementation-evidence-registry*)
             (length feature-ids)))
    t))

(defun %initialize-type-advanced-feature-registry ()
  "Populate the advanced feature and surface-head registries."
  (clrhash *type-advanced-feature-registry*)
  (clrhash *type-advanced-head-registry*)
  (dolist (spec +type-advanced-feature-specs+)
    (register-type-advanced-feature
     (make-type-advanced-feature :id (first spec) :title (second spec))))
  (dolist (spec +type-advanced-head-specs+)
    (register-type-advanced-head (car spec) (cdr spec)))
  (%initialize-type-advanced-contract-registry)
  (%initialize-type-advanced-implementation-evidence-registry)
  (%ensure-type-advanced-contract-coverage)
  (%ensure-type-advanced-implementation-evidence-coverage)
  t)

(%initialize-type-advanced-feature-registry)

