(in-package :cl-cc/compile)

;;; ── FR-351 MC/DC coverage metadata ────────────────────────────────────────
;;;
;;; Provides: %normalize-coverage-mode, mcdc-coverage-enabled-p,
;;;           *mcdc-boolean-operator-table*, %mcdc-boolean-op,
;;;           %mcdc-decision-outcome, %mcdc-independent-effect-pair,
;;;           %make-mcdc-decision-entry, %mcdc-walk-form,
;;;           %collect-mcdc-decision-entries, collect-mcdc-coverage

(defun %normalize-coverage-mode (coverage)
  "Normalize COVERAGE option into NIL, T, or :MCDC."
  (cond
    ((null coverage) nil)
    ((eq coverage :mcdc) :mcdc)
    ((and (stringp coverage) (string-equal coverage "mcdc")) :mcdc)
    (coverage t)))

(defun mcdc-coverage-enabled-p (coverage)
  "Return true when COVERAGE requests MC/DC condition-decision metadata."
  (eq (%normalize-coverage-mode coverage) :mcdc))

(defparameter *mcdc-boolean-operator-table*
  '(("AND" . :and) ("OR" . :or))
  "Maps AND/OR symbol name strings to MC/DC decision operator keywords.")

(defun %mcdc-boolean-op (form)
  "Return :AND or :OR when FORM is an AND/OR decision with 2+ conditions."
  (when (and (consp form) (symbolp (car form)) (>= (length (cdr form)) 2))
    (cdr (assoc (string-upcase (symbol-name (car form)))
                *mcdc-boolean-operator-table* :test #'string=))))

(defun %mcdc-decision-outcome (operator values)
  "Evaluate boolean decision OPERATOR over VALUES without evaluating source forms."
  (case operator
    (:and (every #'identity values))
    (:or  (some #'identity values))))

(defun %mcdc-independent-effect-pair (operator conditions index)
  "Build one non-masking MC/DC pair proving condition INDEX can affect decision."
  (let* ((neutral (case operator (:and t) (:or nil)))
         (false-row (loop for _ in conditions for i from 0
                          collect (if (= i index) nil neutral)))
         (true-row  (loop for _ in conditions for i from 0
                          collect (if (= i index) t neutral)))
         (false-outcome (%mcdc-decision-outcome operator false-row))
         (true-outcome  (%mcdc-decision-outcome operator true-row)))
    (list :condition (nth index conditions)
          :condition-index index
          :false-row false-row
          :false-outcome false-outcome
          :true-row true-row
          :true-outcome true-outcome
          :independent-effect (not (eql false-outcome true-outcome)))))

(defun %make-mcdc-decision-entry (form path)
  "Return MC/DC metadata for one AND/OR decision FORM."
  (let* ((operator (%mcdc-boolean-op form))
         (conditions (copy-list (cdr form))))
    (when operator
      (list :operator operator
            :form form
            :path path
            :conditions conditions
            :pairs (loop for i below (length conditions)
                         collect (%mcdc-independent-effect-pair operator conditions i))))))

(defun %mcdc-walk-form (node current-path)
  "Return MC/DC decision entries for NODE and all nested subforms at CURRENT-PATH.
Pure function: no side effects, returns a fresh list."
  (when (consp node)
    (let ((entry (%make-mcdc-decision-entry node current-path)))
      (append (and entry (list entry))
              (loop for child in node
                    for index from 0
                    append (%mcdc-walk-form child (append current-path (list index))))))))

(defun %collect-mcdc-decision-entries (form &optional (path nil))
  "Collect MC/DC decision entries from FORM and nested subforms."
  (%mcdc-walk-form form path))

(defun collect-mcdc-coverage (forms)
  "Return MC/DC coverage metadata for AND/OR decisions in FORMS."
  (let ((decisions (loop for form in forms
                         for index from 0
                         append (%collect-mcdc-decision-entries form (list index)))))
    (when decisions
      (list :mode :mcdc :decisions decisions))))
