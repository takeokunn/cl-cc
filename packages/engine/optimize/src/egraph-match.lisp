;;;; egraph-match.lisp — E-Graph Pattern Matching and Rule Application
;;;;
;;;; Pattern variables, recursive pattern matching, and rule application
;;;; for equality saturation. Built on the core e-graph in egraph.lisp.
;;;;
;;;; Saturation loop and VM interface are in egraph-saturation.lisp (loads after).
;;;; Load order: after egraph.lisp, before egraph-saturation.lisp.

(in-package :cl-cc/optimize)

;;; ─── Pattern Matching ────────────────────────────────────────────────────
;;;
;;; Patterns are s-expressions with ?-prefixed symbols as pattern variables.
;;; Example: (add ?x (const 0))
;;; The pattern matcher walks the e-graph and collects all substitutions.

(defun egraph-pattern-var-p (x)
  "T if X is a pattern variable: a symbol whose name starts with '?'."
  (and (symbolp x)
       (> (length (symbol-name x)) 0)
       (char= (char (symbol-name x) 0) #\?)))

(defun egraph-match-pattern (eg pattern class-id &optional bindings)
  "Match PATTERN against the e-class CLASS-ID in e-graph EG.
   Returns a list of all binding alists that satisfy the match, or NIL.
   Pattern variables (?x) bind to e-class IDs."
  (let ((cid (egraph-find eg class-id)))
    (cond
      ;; Pattern variable: bind to this class (check consistency)
      ((egraph-pattern-var-p pattern)
       (let ((existing (assoc pattern bindings)))
         (if existing
             (if (= (egraph-find eg (cdr existing)) cid)
                 (list bindings)
                 nil)
             (list (cons (cons pattern cid) bindings)))))

      ;; Constant pattern: match if the class contains a const e-node with this value
      ((and (consp pattern) (eq (car pattern) 'const) (cdr pattern))
       (let* ((val (cadr pattern))
              (cls (gethash cid (eg-classes eg))))
         (when cls
           (loop for n in (ec-nodes cls)
                 when (and (eq (en-op n) 'const)
                           (null (en-children n))
                           (let ((cls-data (ec-data cls)))
                             (or (equal cls-data val)
                                 (and (consp cls-data) (equal (car cls-data) val)))))
                 collect bindings))))

      ;; Compound pattern: (op arg1 arg2 ...)
      ((consp pattern)
       (let ((op       (car pattern))
             (arg-pats (cdr pattern)))
         (let ((cls (gethash cid (eg-classes eg))))
           (when cls
             (loop for n in (ec-nodes cls)
                   when (and (eq (en-op n) op)
                             (= (length (en-children n)) (length arg-pats)))
                   nconc (egraph-match-pattern-args eg arg-pats (en-children n) bindings))))))

      ;; Literal symbol: match if the class has a node with that op and no children
      ((symbolp pattern)
       (let ((cls (gethash cid (eg-classes eg))))
         (when cls
           (loop for n in (ec-nodes cls)
                 when (and (eq (en-op n) pattern) (null (en-children n)))
                 collect bindings))))

      (t nil))))

(defun egraph-match-pattern-args (eg arg-pats arg-ids bindings)
  "Match argument patterns ARG-PATS against e-class IDs ARG-IDS.
   Returns list of all consistent binding alists."
  (if (null arg-pats)
      (list bindings)
      (let ((matches (egraph-match-pattern eg (car arg-pats) (car arg-ids) bindings)))
        (loop for b in matches
              nconc (egraph-match-pattern-args eg (cdr arg-pats) (cdr arg-ids) b)))))

;;; ─── Rule Application ────────────────────────────────────────────────────

(defun egraph-apply-rule (eg rule)
  "Apply one rewrite rule RULE = (:lhs pattern :rhs template :when guard-fn)
   to the e-graph EG.  For every match of :lhs, construct the :rhs and
   merge the resulting e-class with the matched class.
   Returns the number of new merges added."
  (let ((lhs      (getf rule :lhs))
        (rhs      (getf rule :rhs))
        (guard-fn (getf rule :when))
        (merges   0))
    (loop for cid being the hash-keys of (eg-classes eg)
          do (let ((matches (egraph-match-pattern eg lhs cid)))
               (dolist (bindings matches)
                 (when (or (null guard-fn) (funcall guard-fn bindings eg))
                   (let ((new-id (egraph-build-rhs eg rhs bindings)))
                     (when new-id
                       (unless (= (egraph-find eg cid) (egraph-find eg new-id))
                         (egraph-merge eg cid new-id)
                         (incf merges))))))))
    merges))

(defun egraph-build-rhs (eg template bindings)
  "Build the right-hand side TEMPLATE into the e-graph EG using BINDINGS.
   Returns the e-class ID of the constructed term."
  (cond
    ;; Pattern variable: look up binding
    ((egraph-pattern-var-p template)
     (let ((b (assoc template bindings)))
       (when b (cdr b))))

    ;; Compound template: (op arg1 arg2 ...)
    ((consp template)
     (let* ((op       (car template))
            (arg-tmps (cdr template))
            (arg-ids  (mapcar (lambda (a) (egraph-build-rhs eg a bindings))
                              arg-tmps)))
       (when (every #'identity arg-ids)
         (apply #'egraph-add eg op arg-ids))))

    ;; Literal op (nullary)
    ((symbolp template)
     (egraph-add eg template))

    ;; Numeric or string constant
    ((or (numberp template) (stringp template))
     (let ((id (egraph-add eg 'const)))
       ;; Store constant value in e-class data
       (let ((cls (gethash id (eg-classes eg))))
         (when cls (setf (ec-data cls) template)))
       id))

    (t nil)))

;;; ─── Saturation ──────────────────────────────────────────────────────────

;;; (Saturation, cost model, extraction, VM interface, and stats
;;;  are in egraph-saturation.lisp which loads after this file.)
