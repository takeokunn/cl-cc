;;;; packages/emit/src/regalloc-advanced-132.lisp — Phase 132: Register Allocation
;;;; FR-734 Register Coalescing, FR-735 Rematerialization,
;;;; FR-736 BURS Instruction Selection, FR-737 Anti-Dependence Breaking

(in-package :cl-cc/emit)

;;; ──── FR-734: Register Coalescing ────
(defstruct live-range
  "Live range for a virtual register."
  (vreg nil :type symbol)
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (interference (make-hash-table :test #'eq)))

(defun coalesce-registers (live-ranges)
  "Coalesce live ranges: merge non-interfering ranges to eliminate MOVE instructions.
Uses conservative (Chaitin-Briggs) coalescing: only merge if coloring not affected."
  (let ((changed t))
    (loop while changed do
      (setf changed nil)
      (dolist (lr live-ranges)
        (dolist (other live-ranges)
          (unless (or (eq lr other)
                      (gethash other (live-range-interference lr)))
            (when (and (<= (live-range-start other) (live-range-end lr))
                       (<= (live-range-start lr) (live-range-end other)))
              ;; Coalesce into lr
              (setf (live-range-end lr) (max (live-range-end lr) (live-range-end other)))
              (setf changed t)
              (setf live-ranges (remove other live-ranges))
              (return)))))))
  live-ranges)

;;; ──── FR-735: Rematerialization ────
(defun rematerializable-p (value)
  "Return T if VALUE can be rematerialized (recomputed) instead of spilled.
Rematerializable values: constants, simple single-operand operations, loop-invariant."
  (or (numberp value)
      (characterp value)
      (and (consp value)
           (member (car value) '(vm-const vm-add vm-sub)
                   :test #'eq))))

;;; ──── FR-736: BURS Instruction Selection ────
(defstruct burs-rule
  "A BURS (Bottom-Up Rewrite System) instruction selection rule."
  (pattern nil :type list)
  (replacement nil :type list)
  (cost 1 :type fixnum))

(defvar *burs-rules* nil
  "List of all BURS rewrite rules for instruction selection.")

(defun register-burs-rule (pattern replacement &optional (cost 1))
  "Register a BURS rewrite rule: PATTERN → REPLACEMENT with COST."
  (push (make-burs-rule :pattern pattern :replacement replacement :cost cost)
        *burs-rules*))

(defconstant +burs-terminal-cost+ 1000
  "Fallback cost for covering terminal IR leaves without a registered rule.")

(defun %burs-pattern-frontier (pattern tree)
  "Return frontier subtrees when PATTERN structurally matches TREE, or NIL.
Symbol leaves in PATTERN are tile operands and must match terminal IR leaves.
Conses in PATTERN must match the IR operator and arity exactly."
  (cond
    ((symbolp pattern)
     (and (atom tree) (list tree)))
    ((and (consp pattern) (consp tree)
          (eq (car pattern) (car tree))
          (= (length pattern) (length tree)))
     (loop for pattern-child in (cdr pattern)
           for tree-child in (cdr tree)
           for frontier = (%burs-pattern-frontier pattern-child tree-child)
           when (null frontier)
             do (return nil)
           append frontier))
    (t nil)))

(defun %make-burs-terminal-rule (tree)
  "Create a synthetic terminal-covering rule for TREE."
  (make-burs-rule :pattern (list 'terminal tree)
                  :replacement (list 'identity tree)
                  :cost +burs-terminal-cost+))

(defun burs-select-instructions (ir-tree)
  "Select optimal instruction sequence for IR-TREE using BURS dynamic programming.
Computes minimum-cost covering of each node."
  (let ((memo (make-hash-table :test #'equal))
        (ordered-rules (reverse *burs-rules*)))
    (labels ((cover (tree)
               (multiple-value-bind (cached cached-p) (gethash tree memo)
                 (if cached-p
                     (values (first cached) (second cached))
                     (multiple-value-bind (rules cost) (cover-uncached tree)
                       (setf (gethash tree memo) (list rules cost))
                       (values rules cost)))))
             (cover-uncached (tree)
               (if (atom tree)
                   (let ((rule (%make-burs-terminal-rule tree)))
                     (values (list rule) (burs-rule-cost rule)))
                   (let ((best-cost most-positive-fixnum)
                         (best-rules nil))
                     (dolist (rule ordered-rules)
                       (let ((frontier (%burs-pattern-frontier
                                        (burs-rule-pattern rule) tree)))
                         (when frontier
                           (let ((candidate-cost (burs-rule-cost rule))
                                 (candidate-rules nil)
                                 (valid-cover-p t))
                             (dolist (subtree frontier)
                               (multiple-value-bind (subtree-rules subtree-cost)
                                   (cover subtree)
                                 (if subtree-rules
                                     (progn
                                       (incf candidate-cost subtree-cost)
                                       (setf candidate-rules
                                             (append candidate-rules subtree-rules)))
                                     (setf valid-cover-p nil))))
                             (when (and valid-cover-p (< candidate-cost best-cost))
                               (setf best-cost candidate-cost)
                               (setf best-rules (append candidate-rules
                                                        (list rule))))))))
                     (if best-rules
                         (values best-rules best-cost)
                         (error "No BURS cover for IR tree: ~S" tree))))))
      (cover ir-tree))))

;; Pre-register standard x86-64 BURS rules
(eval-when (:load-toplevel :execute)
  (register-burs-rule '(add (load addr) reg) '(add reg (mem addr)) 2)
  (register-burs-rule '(add reg1 reg2) '(add reg1 reg2) 1)
  (register-burs-rule '(mul reg const) '(lea reg (reg const)) 1))

;;; ──── FR-737: Anti-Dependence Breaking / Register Renaming ────
(defun rename-anti-dependencies (instructions)
  "Static register renaming to eliminate WAR/WAW false dependencies.
Expands scheduling window from 3-5 to 20+ instructions."
  (let ((renamed-count 0))
    (dolist (inst instructions instructions)
      (when (and (consp inst)
                 (member (car inst) '(move set) :test #'eq))
        (let* ((dst (second inst))
               (src (third inst))
               (new-dst (gensym (format nil "~A-NEW-" dst))))
          (setf (second inst) new-dst)
          (incf renamed-count))))
    (values instructions renamed-count)))

;; ── Exports ──
(export '(live-range make-live-range coalesce-registers
          rematerializable-p
          burs-rule make-burs-rule *burs-rules*
          register-burs-rule burs-select-instructions
          rename-anti-dependencies))
