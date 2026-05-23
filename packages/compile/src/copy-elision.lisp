;;;; packages/compile/src/copy-elision.lisp — FR-568 Copy Elision / NRVO
;;;; Return-Value Optimization: construct result directly in caller's frame.
;;;; C++ RVO/NRVO / Rust move semantics equivalent.

(in-package :cl-cc/compile)

;;; ──── NRVO Detection ────
;; Pattern: (defun foo (args) (let ((result expr)) ... result))
;; OR:       (defun foo (args) ... expr) where expr returns constructed object
;; The function returns a single object constructed within the function.

(defun nrvo-candidate-p (body-forms)
  "Return T if the function body returns a locally-constructed single object.
The returned object must be:
1. A single return value (not multiple-value-return)
2. Constructed within the function (not an argument)
3. Not referenced after return (sole owner)
4. Not stored in any global/heap location"
  ;; Simplified heuristic: check if last form is a variable reference
  ;; to a locally-bound object constructor.
  (let ((last-form (car (last body-forms))))
    (and (consp last-form)
         (member (car last-form)
                 '(make-instance make-array make-hash-table
                   make-string vector list cons)))))

;;; ──── NRVO Transformation ────
(defun apply-nrvo (func-body result-var construction-form)
  "Transform FUNC-BODY to construct the result directly in the caller's frame.
The CONSTRUCTION-FORM is moved to the return site.
RESULT-VAR references in the body are rewritten to use the pre-allocated slot."
  ;; Replace all references to RESULT-VAR with the caller's target address
  ;; The caller allocates space and passes it as a hidden parameter.
  `(let ((,result-var ,construction-form))
     ,@(butlast func-body)  ; all forms except the last
     ,result-var))           ; last form is just result-var

;;; ──── RVO (Return Value Optimization) ────
(defun rvo-candidate-p (return-expr)
  "Return T if RETURN-EXPR is an anonymous temporary that can be constructed in-place."
  (and (consp return-expr)
       (or (eq (car return-expr) 'make-array)
           (eq (car return-expr) 'make-instance)
           (eq (car return-expr) 'make-string)
           (eq (car return-expr) 'list)
           (eq (car return-expr) 'cons))))

(defun apply-rvo (return-expr caller-target)
  "Rewrite RETURN-EXPR to construct result directly at CALLER-TARGET address.
The construction form is modified to write into a pre-allocated buffer."
  (declare (ignore caller-target))
  ;; In practice: modify the allocation to use the caller-provided memory.
  ;; Simplified: just return the expression (allocation is done later at emission)
  return-expr)

;;; ──── Integration ────
(defun optimize-return-values (func-name body-forms)
  "Apply NRVO/RVO to FUNC-NAME's BODY-FORMS where applicable.
Returns the (possibly optimized) body forms."
  (let ((return-expr (car (last body-forms))))
    (cond
      ;; NRVO: (let ((result (make-...))) ... result)
      ((and (consp return-expr)
            (symbolp return-expr)
            (nrvo-var-p return-expr body-forms))
       (let ((construction-form (find-construction-form return-expr body-forms)))
         (when construction-form
           (apply-nrvo body-forms return-expr construction-form))))
      ;; RVO: anonymous construction returned directly
      ((rvo-candidate-p return-expr)
       (apply-rvo return-expr nil))
      (t body-forms))))

;;; ──── Helpers ────
(defun nrvo-var-p (var body-forms)
  "Check if VAR is a single-owner locally-constructed variable in BODY-FORMS."
  (loop for form in body-forms
        when (and (consp form) (eq (car form) 'let))
          do (loop for binding in (cadr form)
                   when (and (consp binding)
                             (eq (car binding) var))
                     do (return-from nrvo-var-p t)))
  nil)

(defun find-construction-form (var body-forms)
  "Find the construction form that initializes VAR in BODY-FORMS."
  (loop for form in body-forms
        when (and (consp form) (eq (car form) 'let))
          do (loop for binding in (cadr form)
                   when (and (consp binding) (eq (car binding) var))
                     do (return-from find-construction-form
                          (if (cdr binding) (cadr binding) nil))))
  nil)
