(in-package :cl-cc/prolog)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Prolog — Peephole Rewriting Layer
;;;
;;; Contains: %remove-self-move-p, %match-peephole-rule,
;;; %maybe-peephole-rewrite, apply-prolog-peephole.
;;;
;;; Rule data lives in prolog-data.lisp. Solver/query functions live in
;;; prolog-solver.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun %remove-self-move-p (instruction)
  "Return true when INSTRUCTION is a redundant self move."
  (and (consp instruction)
       (eq (car instruction) :move)
       (eql (cadr instruction) (caddr instruction))))

(defun %match-peephole-rule (rule current next)
  "Return replacement instructions when RULE matches CURRENT and NEXT."
  (destructuring-bind (cur-pat next-pat result-list) rule
    (let ((env (unify cur-pat current nil)))
      (unless (unify-failed-p env)
        (let ((env2 (unify next-pat next env)))
          (unless (unify-failed-p env2)
            (mapcar (lambda (template)
                      (logic-substitute template env2))
                    result-list)))))))

(defun %maybe-peephole-rewrite (current next)
  "Try all peephole rules for CURRENT/NEXT and return replacements if one matches."
  (dolist (rule *peephole-rules*)
    (let ((replacements (%match-peephole-rule rule current next)))
      (when replacements
        (return replacements)))))

(defun apply-prolog-peephole (instructions)
  "Apply Prolog-unification peephole rules over two-instruction windows.

   Rule format: each rule in *peephole-rules* is a three-element list
     (CURRENT-PATTERN NEXT-PATTERN REPLACEMENT-LIST)
   On a match, both instructions are consumed and REPLACEMENT-LIST sexps emitted.
   Self-moves (:move :Rx :Rx) are removed in a pre-pass."
  (labels ((walk (rest out)
             (cond
               ((null rest) (nreverse out))
               ((null (cdr rest)) (nreverse (cons (car rest) out)))
               (t
                (let ((replacements (%maybe-peephole-rewrite (car rest) (cadr rest))))
                  (if replacements
                      (walk (cddr rest) (revappend replacements out))
                      (walk (cdr rest) (cons (car rest) out))))))))
    (walk (remove-if #'%remove-self-move-p instructions) nil)))
