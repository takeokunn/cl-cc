;;;; tests/framework-meta.lisp — CL-CC Test Framework (Meta-Testing)
;;;; Mutation testing, metamorphic testing, invariants.
;;;; Coverage helpers and TAP output are in framework-tap.lisp.

;;; Load sb-cover contrib before the reader encounters sb-cover: symbols.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (ignore-errors (require :sb-cover)))

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Notes on shared state (defined in framework.lisp)
;;; ------------------------------------------------------------

;;; definvariant is defined in framework.lisp
;;; *invariant-registry* is defined in framework.lisp
;;; *metamorphic-relations* is defined in framework.lisp
;;; *tap-mutex* is defined in framework.lisp
;;; %fail-test is defined in framework.lisp

;;; ------------------------------------------------------------
;;; Metamorphic Testing (FR-036)
;;; ------------------------------------------------------------

(defmacro defmetamorphic (name &key transform relation applicable-when)
  "Define a metamorphic relation. At suite-run time, applies TRANSFORM to
   all test expressions that match APPLICABLE-WHEN, and verifies RELATION
   holds between original and transformed results.

   Example:
     (defmetamorphic commutativity
       :transform (lambda (expr)
         (destructuring-bind (op a b) (cdr expr)
           `(,op ,b ,a)))
       :relation #'=
       :applicable-when (lambda (expr)
         (member (car expr) '(+ *))))"
  `(progn
     (push (list :name ',name
                 :transform ,transform
                 :relation ,relation
                 :applicable-when ,applicable-when)
           *metamorphic-relations*)
     ',name))

(defun %verify-metamorphic-relations (expressions)
  "Check all registered metamorphic relations against the given expressions.
   Signals test-failure via %fail-test if any relation is violated."
  (dolist (relation *metamorphic-relations*)
    (let ((transform      (getf relation :transform))
          (rel-fn         (getf relation :relation))
          (applicable-when (getf relation :applicable-when)))
      (dolist (expr expressions)
        (when (and applicable-when (ignore-errors (funcall applicable-when expr)))
          (let* ((transformed        (ignore-errors (funcall transform expr)))
                 (original-result    (ignore-errors (eval expr)))
                 (transformed-result (ignore-errors (eval transformed))))
            (when (and original-result transformed-result)
              (unless (ignore-errors (funcall rel-fn original-result transformed-result))
                (%fail-test (format nil "Metamorphic relation ~S violated"
                                    (getf relation :name))
                            :expected (list expr original-result)
                            :actual   (list transformed transformed-result))))))))))

;;; ------------------------------------------------------------
;;; Mutation Operators (FR-029)
;;; ------------------------------------------------------------

(defun %tree-map (form leaf-fn)
  "Recursively apply LEAF-FN to every atom in FORM, rebuilding cons structure."
  (cond
    ((null form) nil)
    ((consp form)
      (cons (%tree-map (car form) leaf-fn)
            (%tree-map (cdr form) leaf-fn)))
    (t (funcall leaf-fn form))))

(defun %substitute-symbol (form old new)
  "Recursively substitute all occurrences of symbol OLD with NEW in FORM."
  (%tree-map form (lambda (atom) (if (eq atom old) new atom))))

(defun %substitute-constant (form old new)
  "Recursively substitute the literal value OLD with NEW in FORM."
  (%tree-map form (lambda (atom) (if (eql atom old) new atom))))

(defun %negate-first-if-condition (form)
  "Find the first (if cond ...) subform and negate its condition."
  (cond
    ((and (consp form) (eq (car form) 'if) (>= (length form) 3))
     (let ((cond-expr (cadr form))
           (rest      (cddr form)))
       `(if (not ,cond-expr) ,@rest)))
    ((consp form)
     (cons (%negate-first-if-condition (car form))
           (cdr form)))
    (t form)))

(defun %return-nil-body (form)
  "If FORM is a (defun ...) or (defmethod ...) top-level definition,
   replace its body with NIL. Otherwise return FORM unchanged."
  (if (and (consp form)
           (member (car form) '(defun defmethod defgeneric)))
      ;; Keep (defun name params) prefix, replace body with nil
      (let* ((head    (car form))
             (fname   (cadr form))
             (params  (caddr form)))
        `(,head ,fname ,params nil))
      form))

(defparameter *mutation-operator-specs*
  (list
   (list :arithmetic-swap
         (lambda (form) (%substitute-symbol form (quote +) (quote -)))
         (lambda (form) (%substitute-symbol form (quote *) (quote /))))
   (list :condition-negate
         (lambda (form) (%negate-first-if-condition form)))
   (list :boundary-shift
         (lambda (form) (%substitute-symbol form (quote <) (quote <=)))
         (lambda (form) (%substitute-symbol form (quote >) (quote >=))))
   (list :constant-replace
         (lambda (form) (%substitute-constant form 0 1))
         (lambda (form) (%substitute-constant form 1 0)))
   (list :return-nil
         (lambda (form) (%return-nil-body form))))
  "Prolog-style fact table: mutation-type keyword → list of transformer lambdas.")

(defun %apply-mutation (form mutation-type)
  "Apply MUTATION-TYPE to FORM; return a list of distinct mutant forms (≠ FORM)."
  (let ((entry (assoc mutation-type *mutation-operator-specs*)))
    (unless entry (error "Unknown mutation type: ~S" mutation-type))
    (remove-duplicates
     (remove-if (lambda (m) (equal m form))
                (mapcar (lambda (tfn) (funcall tfn form)) (rest entry)))
     :test #'equal)))

;;; ------------------------------------------------------------
;;; Source File Reader
;;; ------------------------------------------------------------

(defparameter *whitespace-chars* '(#\Space #\Tab #\Newline #\Return)
  "Characters considered whitespace for source-form scanning.")

(defun %skip-whitespace (string start)
  "Return the index of the first non-whitespace char in STRING at or after START."
  (or (position-if-not (lambda (c) (member c *whitespace-chars*)) string :start start)
      (length string)))

(defun %read-all-forms (source-string)
  "Read all top-level forms from SOURCE-STRING; ignore read errors."
  (let ((forms '())
        (len   (length source-string)))
    (loop for pos = (%skip-whitespace source-string 0)
                then (%skip-whitespace source-string pos)
          while (< pos len)
          do (multiple-value-bind (form new-pos)
                 (ignore-errors (read-from-string source-string nil nil :start pos))
               (cond
                 ((or (null new-pos) (= new-pos pos)) (return))
                 (t (when form (push form forms))
                    (setf pos new-pos)))))
    (nreverse forms)))

;;; ------------------------------------------------------------
;;; Mutant Evaluation
;;; ------------------------------------------------------------

(defun %eval-form-safely (form)
  "Evaluate FORM in the current Lisp image.
   Returns T on success, NIL on any error."
  (ignore-errors
    (eval form)
    t))

(defun %mutant-killed-p (mutant-form suite-name)
  "Return T if MUTANT-FORM is detected by any test in SUITE-NAME.
A mutant that fails to compile counts as killed."
  (let* ((tests       (%collect-all-suite-tests suite-name nil))
         (compiled-ok (%eval-form-safely mutant-form)))
    (unless compiled-ok
      (return-from %mutant-killed-p t))
    (some (lambda (test-plist)
            (eq (getf (%run-single-test test-plist 0 '()) :status) :fail))
          tests)))

;;; ------------------------------------------------------------
;;; Mutation Score Reporter
;;; ------------------------------------------------------------

(defstruct mutation-record
  "Tracks a single mutant's outcome."
  (source-location "" :type string)
  (mutation-type   nil)
  (original-form   nil)
  (mutant-form     nil)
  (killed          nil :type boolean))

(defun %print-mutation-report (records)
  "Print a TAP-style mutation testing report."
  (let* ((total   (length records))
         (killed  (count-if #'mutation-record-killed records))
         (score   (if (zerop total)
                      100.0
                      (* 100.0 (/ killed total))))
         (survivors (remove-if #'mutation-record-killed records)))
    (format t "~%# ------------------------------------------------------------~%")
    (format t "# Mutation Testing Report~%")
    (format t "# ------------------------------------------------------------~%")
    (format t "# Mutation Score: ~,1F% (~A/~A mutants killed)~%"
            score killed total)
    (when survivors
      (format t "# Survivors (~A):~%" (length survivors))
      (dolist (rec survivors)
        (format t "#   ~A [~A] NOT caught~%"
                (mutation-record-source-location rec)
                (mutation-record-mutation-type rec))))
    (format t "# ------------------------------------------------------------~%")
    (force-output)
    (values score killed total)))

;;; ------------------------------------------------------------
;;; run-mutation-test (FR-029)
;;; ------------------------------------------------------------

(defparameter *mutable-definition-ops*
  '(defun defmethod defgeneric defmacro)
  "Top-level forms safe to replace in-image for mutation testing.")

(defun %trial-mutant (form mutant mutation-type location suite)
  "Eval MUTANT in-image (restoring the original defun after), return a mutation-record."
  (let* ((defun-p  (and (consp form) (eq (car form) 'defun)))
         (saved-fn (when defun-p (ignore-errors (symbol-function (cadr form)))))
         (killed   (%mutant-killed-p mutant suite)))
    (when (and defun-p saved-fn)
      (ignore-errors (setf (symbol-function (cadr form)) saved-fn)))
    (make-mutation-record :source-location location
                          :mutation-type   mutation-type
                          :original-form   form
                          :mutant-form     mutant
                          :killed          killed)))

(defun %collect-mutation-records (forms target mutations suite)
  "Generate one mutation-record per (form × mutation-type × mutant variant)."
  (loop for form in forms
        for form-index from 1
        when (and (consp form) (member (car form) *mutable-definition-ops*))
          nconc (loop for mutation-type in mutations
                      nconc (mapcar (lambda (mutant)
                                      (%trial-mutant form mutant mutation-type
                                                     (format nil "~A:form-~A" target form-index)
                                                     suite))
                                    (%apply-mutation form mutation-type)))))

(defun run-mutation-test (&key target suite
                               (mutations '(:arithmetic-swap :condition-negate
                                            :boundary-shift :constant-replace
                                            :return-nil)))
  "Run mutation testing on TARGET file using SUITE tests.
Returns (values score killed total)."
  (unless target (error "run-mutation-test: :target file path is required"))
  (unless suite  (error "run-mutation-test: :suite name is required"))
  (unless (probe-file target)
    (error "run-mutation-test: target file not found: ~A" target))
  (format t "# Mutation testing: ~A~%# Suite: ~A~%# Operators: ~{~A~^ ~}~%# Reading source forms...~%"
          target suite mutations)
  (force-output)
  (let ((forms (%read-all-forms (uiop:read-file-string target))))
    (format t "# Found ~A top-level forms~%" (length forms))
    (force-output)
    (%print-mutation-report
     (%collect-mutation-records forms target mutations suite))))


;;; ------------------------------------------------------------
;;; Convenience: Pre-defined Metamorphic Relations for cl-cc
;;; ------------------------------------------------------------

;; Arithmetic commutativity: (+ a b) == (+ b a)
(defmetamorphic commutativity
  :transform (lambda (expr)
               (when (and (consp expr) (>= (length expr) 3))
                 (destructuring-bind (op a b &rest rest) expr
                   (declare (ignore rest))
                   `(,op ,b ,a))))
  :relation #'equal
  :applicable-when (lambda (expr)
                     (and (consp expr)
                          (member (car expr) '(+ *)))))

;; Compiler idempotency: compiling twice should give structurally equal output
;; (checked at the instruction-count level as a lightweight proxy)
(defmetamorphic compiler-idempotency
  :transform (lambda (expr)
               ;; Second compilation of the same expression
               expr)
  :relation (lambda (r1 r2)
              ;; Both results should be non-nil and have same instruction count
              (and r1 r2
                   (let ((p1 (ignore-errors (compilation-result-program r1)))
                         (p2 (ignore-errors (compilation-result-program r2))))
                     (and p1 p2
                          (= (length (vm-program-instructions p1))
                             (length (vm-program-instructions p2)))))))
  :applicable-when (lambda (expr)
                     ;; Apply to string expressions that look compilable
                     (stringp expr)))
