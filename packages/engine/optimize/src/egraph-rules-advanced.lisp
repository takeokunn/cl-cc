(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; E-Graph — Advanced Rules and Entry Point
;;;
;;; Extracted from egraph-rules.lisp.
;;; Contains:
;;;   - Advanced rewrite rules (mul-neg-neg, neg-sub) not yet in the optimizer
;;;   - egraph-builtin-rules — registry inspector
;;;   - optimize-with-egraph — main optimization entry point
;;;
;;; Load order: after egraph-rules.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── Advanced Rules (new — not in current optimizer) ─────────────────────

(defrule mul-neg-neg
  (mul (neg ?x) (neg ?y))
  (mul ?x ?y))

(defrule neg-sub
  (neg (sub ?x ?y))
  (sub ?y ?x))

;;; ─── All Built-In Rules ──────────────────────────────────────────────────

(defun egraph-builtin-rules ()
  "Return the list of built-in e-graph rewrite rules.
The primary source of truth is the Prolog `egraph-rule` fact set emitted by
`defrule`; the in-memory guard table is consulted only to attach existing `:when`
predicates to the fact-backed rule records." 
  (mapcar (lambda (fact)
            (let ((name (second fact))
                  (lhs  (third fact))
                  (rhs  (fourth fact)))
              (list :name name
                    :lhs lhs
                    :rhs rhs
                    :when (gethash name *egraph-rule-guards*))))
          (cl-cc/prolog:query-all '(egraph-rule ?name ?lhs ?rhs))))

;;; ─── E-Graph Optimization Entry Point ────────────────────────────────────

(defun optimize-with-egraph (instructions &key
                                            (rules (egraph-builtin-rules))
                                            (saturation-limit 30)
                                            (saturation-fuel 10000)
                                            (cost-fn #'egraph-default-cost))
  "Optimize a list of VM INSTRUCTIONS using e-graph equality saturation.
   Returns an optimized instruction list.

   Algorithm:
      1. Add instructions to e-graph (building reg→class mapping)
      2. Saturate with RULES until fixed-point or resource limit
      3. Lower destination classes proven equal to constants or register aliases

   This pass is wired into the main optimizer pipeline via `:egraph` and also
   participates in the broader `:prolog-rewrite` stage."
  (declare (ignore cost-fn))
  (when (null instructions) (return-from optimize-with-egraph instructions))
  (let* ((eg      (make-e-graph))
         (reg-map (egraph-add-instructions eg instructions)))
    ;; Saturate
    (egraph-saturate eg rules
                     :limit saturation-limit
                     :fuel saturation-fuel)
    (egraph-rebuild eg)
    (let ((class->regs (make-hash-table :test #'equal)))
      (maphash (lambda (reg class-id)
                 (push reg (gethash (egraph-find eg class-id) class->regs)))
               reg-map)
      (labels ((rewrite (inst)
                 (let ((dst (ignore-errors (vm-dst inst))))
                   (cond
                     ((null dst)
                      inst)
                     ((egraph-class-has-op-p eg (gethash dst reg-map) 'const)
                      (make-vm-const :dst dst
                                     :value (egraph-class-const-value eg (gethash dst reg-map))))
                     (t
                      (let* ((class-id (gethash dst reg-map))
                             (canon (and class-id (egraph-find eg class-id)))
                             (regs (and canon (gethash canon class->regs)))
                             (src (find-if (lambda (reg) (not (eq reg dst))) regs)))
                        (if src
                            (make-vm-move :dst dst :src src)
                            inst)))))))
        (mapcar #'rewrite instructions)))))
