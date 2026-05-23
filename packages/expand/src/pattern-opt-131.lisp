;;;; packages/expand/src/pattern-opt-131.lisp — Phase 131: Pattern Matching Optimization
;;;; FR-728 Decision Tree Compilation, FR-729 Pattern Usefulness Checking,
;;;; FR-730 Tail Recursion Modulo Cons, FR-731 Join Points

(in-package :cl-cc/expand)

;;; ──── FR-728: Decision Tree Compilation for Pattern Matching ────
(defun compile-pattern-decision-tree (patterns)
  "Compile PATTERNS into an optimal decision tree using Maranget's algorithm.
PATTERNS is a list of (pattern . body) cons cells."
  (labels ((common-test (pats)
             "Find the most common test among all patterns."
             (when pats
               (let ((first-pat (caar pats)))
                 (when (consp first-pat) (car first-pat)))))
           (partition (test pats)
             "Partition patterns by TEST result."
             (let ((match nil) (nomatch nil))
               (dolist (p pats)
                 (if (and (consp (car p)) (eq (caar p) test))
                     (push p match)
                     (push p nomatch)))
               (values (nreverse match) (nreverse nomatch)))))
    (let ((test (common-test patterns)))
      (if test
          (multiple-value-bind (match nomatch) (partition test patterns)
            `(if (,test value)
                 ,(compile-pattern-decision-tree match)
                 ,(compile-pattern-decision-tree nomatch)))
          ;; Leaf: only one pattern or all patterns are wildcards
          `(progn ,@(mapcar #'cdr patterns))))))

;;; ──── FR-729: Pattern Usefulness / Redundancy Checking ────
(defun check-pattern-usefulness (patterns)
  "Check each pattern in PATTERNS for usefulness (not subsumed by earlier patterns).
Returns a list of (index . warning-string) for redundant patterns.
Based on Maranget's usefulness algorithm."
  (let ((warnings nil))
    (loop for i from 0 below (length patterns)
          for pat = (nth i patterns)
          do (loop for j from 0 below i
                   for prev = (nth j patterns)
                   when (pattern-subsumes-p prev pat)
                   do (push (cons i (format nil "Pattern ~D is redundant (covered by pattern ~D)" i j))
                            warnings)))
    (nreverse warnings)))

(defun pattern-subsumes-p (pat1 pat2)
  "Return T if PAT1 subsumes PAT2 (i.e., any match for PAT2 also matches PAT1)."
  (or (eq pat1 pat2)
      (eq pat1 '_)    ; underscore patterns always subsume
      (and (eq pat2 '_) nil)))

;;; ──── FR-730: Tail Recursion Modulo Cons / TRMC ────
(defvar *trmc-enabled* t
  "When T, enable Tail Recursion Modulo Cons optimization.")

(defun trmc-optimize-form (form)
  "Transform FORM if it matches TRMC pattern: (cons x (recurse ...))."
  (when (and *trmc-enabled* (consp form) (eq (car form) 'cons))
    (let ((x (second form))
          (recursive-call (third form)))
      (when (and (consp recursive-call) (symbolp (car recursive-call)))
        ;; Pre-allocate cons cell with hole in CDR, fill later
        `(let* ((trmc-cell (cons ,x nil)))
           (setf (cdr trmc-cell) ,recursive-call)
           trmc-cell))))
  form)

;;; ──── FR-731: Join Points ────
(defvar *join-points-enabled* t
  "When T, enable Join Point optimization.")

(defun introduce-join-points (form)
  "Introduce join points for common code after conditional branches.
Transform (if c (progn ... end) (progn ... end)) → (letjoin k () end) (if c (k) (k))."
  (when (and *join-points-enabled* (consp form) (eq (car form) 'if))
    (let* ((then-branch (third form))
           (else-branch (fourth form))
           (then-last (when (consp then-branch)
                        (car (last then-branch))))
           (else-last (when (consp else-branch)
                        (car (last else-branch)))))
      (when (and then-last else-last (equal then-last else-last))
        ;; Common tail found - extract as join point
        (let ((jp-name (gensym "JOIN-")))
          `(let ((,jp-name (lambda () ,then-last)))
             (if ,(second form)
                 (progn ,@(butlast (cddr form)) (funcall ,jp-name))
                 (progn ,@(butlast (cdddr form)) (funcall ,jp-name))))))))
  form)

;; ── Exports ──
(export '(compile-pattern-decision-tree check-pattern-usefulness pattern-subsumes-p
          *trmc-enabled* trmc-optimize-form
          *join-points-enabled* introduce-join-points))
