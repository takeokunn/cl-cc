(in-package :cl-cc/codegen)

;;;; FR-057 / FR-175 / FR-299: MIR-backed instruction selection.
;;;;
;;;; This layer deliberately returns the existing VM instruction format.  Native
;;;; backends and register allocation therefore remain unchanged while the
;;;; native pipeline can exercise MIR SSA construction, MIR-level passes, and a
;;;; target-aware maximal-munch selector.

(define-condition isel-diagnostic (error)
  ((message :initarg :message :reader isel-diagnostic-message))
  (:report (lambda (condition stream)
             (format stream "MIR instruction selection failed: ~A"
                     (isel-diagnostic-message condition)))))

(defstruct isel-rule
  name
  target
  pattern
  result-op
  (cost 1 :type fixnum)
  (size 1 :type fixnum)
  emitter)

(defparameter *isel-rule-table* (make-hash-table :test #'eq)
  "TARGET keyword -> list of ISEL-RULE records.")

(defun register-isel-rule (rule)
  "Register RULE for its target and return RULE."
  (push rule (gethash (isel-rule-target rule) *isel-rule-table*))
  rule)

(defun isel-rules-for-target (target)
  "Return registered instruction-selection rules for TARGET."
  (copy-list (gethash target *isel-rule-table*)))

(defun %isel-variable-pattern-p (pattern)
  (and (symbolp pattern)
       (plusp (length (symbol-name pattern)))
       (char= (char (symbol-name pattern) 0) #\?)))

(defun %isel-tree-op (tree)
  (if (consp tree) (first tree) tree))

(defun %isel-tree-children (tree)
  (if (and (consp tree) (not (member (first tree) '(:reg :const :literal) :test #'eq)))
      (rest tree)
      nil))

(defun %isel-bind-pattern (pattern tree bindings)
  "Match PATTERN against TREE and return updated bindings, or NIL."
  (cond
    ((%isel-variable-pattern-p pattern)
     (let ((existing (assoc pattern bindings)))
       (cond
         ((null existing) (acons pattern tree bindings))
         ((equal (cdr existing) tree) bindings)
         (t nil))))
    ((atom pattern)
     (and (eql pattern (%isel-tree-op tree)) bindings))
    ((and (consp tree)
          (eql (first pattern) (first tree))
          (= (length (rest pattern)) (length (rest tree))))
     (loop for p in (rest pattern)
           for child in (rest tree)
           do (setf bindings (%isel-bind-pattern p child bindings))
           while bindings
           finally (return bindings)))
    (t nil)))

(defun %isel-rule-match (rule tree)
  (%isel-bind-pattern (isel-rule-pattern rule) tree '((:matched . t))))

(defun %isel-better-rule-p (candidate incumbent)
  "Largest tile first, then lowest estimated cost."
  (or (null incumbent)
      (> (isel-rule-size candidate) (isel-rule-size incumbent))
      (and (= (isel-rule-size candidate) (isel-rule-size incumbent))
           (< (isel-rule-cost candidate) (isel-rule-cost incumbent)))))

(defun %isel-best-rule (tree rules)
  (let ((best nil)
        (best-bindings nil))
    (dolist (rule rules (values best best-bindings))
      (let ((bindings (%isel-rule-match rule tree)))
        (when (and bindings (%isel-better-rule-p rule best))
          (setf best rule
                best-bindings bindings))))))

(defun isel-maximal-munch (tree target &key (rules (isel-rules-for-target target)))
  "Cover TREE with TARGET rules using maximal munch.

Returns a post-order list of (RULE . BINDINGS) tiles.  This function is used by
the MIR pipeline and is also intentionally small enough for direct unit tests."
  (labels ((cover (node)
             (multiple-value-bind (rule bindings) (%isel-best-rule node rules)
               (unless rule
                 (error 'isel-diagnostic
                        :message (format nil "no instruction-selection rule for ~S on ~S"
                                         node target)))
               (append (mapcan #'cover (%isel-tree-children node))
                       (list (cons rule bindings))))))
    (cover tree)))

(defun %mir-meta-get (inst key)
  (let ((meta (miri-meta inst)))
    (when (listp meta) (getf meta key))))

(defun %vm-reg-value (fn reg table)
  (or (gethash reg table)
      (setf (gethash reg table)
            (mir-new-value fn :name reg :type :any))))

(defun %const-mir-type (value)
  (cond
    ((integerp value) :integer)
    ((or (null value) (eq value t)) :boolean)
    (t :any)))

(defun %emit-pass-through-mir (block vm-inst)
  (mir-emit block :nop :meta (list :vm-inst vm-inst)))

(defun %lower-vm-inst-to-mir (fn block inst reg-map)
  "Lower one VM instruction into BLOCK, preserving the original instruction."
  (cond
    ((typep inst 'vm-const)
     (mir-emit block :const
               :dst (%vm-reg-value fn (vm-dst inst) reg-map)
               :srcs (list (make-mir-const :value (vm-value inst)
                                           :type (%const-mir-type (vm-value inst))))
               :meta (list :vm-inst inst)))
    ((typep inst 'vm-move)
     (mir-emit block :move
               :dst (%vm-reg-value fn (vm-dst inst) reg-map)
               :srcs (list (%vm-reg-value fn (vm-src inst) reg-map))
               :meta (list :vm-inst inst)))
    ((eq (type-of inst) 'vm-add)
     (mir-emit block :add
               :dst (%vm-reg-value fn (vm-dst inst) reg-map)
               :srcs (list (%vm-reg-value fn (vm-lhs inst) reg-map)
                           (%vm-reg-value fn (vm-rhs inst) reg-map))
               :meta (list :vm-inst inst)))
    ((eq (type-of inst) 'vm-sub)
     (mir-emit block :sub
               :dst (%vm-reg-value fn (vm-dst inst) reg-map)
               :srcs (list (%vm-reg-value fn (vm-lhs inst) reg-map)
                           (%vm-reg-value fn (vm-rhs inst) reg-map))
               :meta (list :vm-inst inst)))
    ((eq (type-of inst) 'vm-mul)
     (mir-emit block :mul
               :dst (%vm-reg-value fn (vm-dst inst) reg-map)
               :srcs (list (%vm-reg-value fn (vm-lhs inst) reg-map)
                           (%vm-reg-value fn (vm-rhs inst) reg-map))
               :meta (list :vm-inst inst)))
    ((typep inst 'vm-call)
     (mir-emit block :call
               :dst (%vm-reg-value fn (vm-dst inst) reg-map)
               :srcs (cons (%vm-reg-value fn (vm-func-reg inst) reg-map)
                           (mapcar (lambda (reg) (%vm-reg-value fn reg reg-map))
                                   (vm-args inst)))
               :meta (list :vm-inst inst :calling-convention :defer-to-vm)))
    ((typep inst 'vm-tail-call)
     (mir-emit block :tail-call
               :srcs (cons (%vm-reg-value fn (vm-func-reg inst) reg-map)
                           (mapcar (lambda (reg) (%vm-reg-value fn reg reg-map))
                                   (vm-args inst)))
               :meta (list :vm-inst inst :calling-convention :defer-to-vm)))
    ((typep inst 'vm-ret)
     (mir-emit block :ret
               :srcs (list (%vm-reg-value fn (vm-reg inst) reg-map))
               :meta (list :vm-inst inst)))
    ((typep inst 'vm-jump)
     (mir-emit block :jump :meta (list :vm-inst inst)))
    ((typep inst 'vm-jump-zero)
     (mir-emit block :branch
               :srcs (list (%vm-reg-value fn (vm-reg inst) reg-map))
               :meta (list :vm-inst inst)))
    ((typep inst 'vm-halt)
     (mir-emit block :ret
               :srcs (list (%vm-reg-value fn (vm-reg inst) reg-map))
               :meta (list :vm-inst inst :toplevel-halt-p t)))
    (t (%emit-pass-through-mir block inst))))

(defun %vm-label-name-string (inst)
  (and (typep inst 'vm-label) (vm-lbl-name inst)))

(defun vm-program->mir-module (program &key (name :toplevel))
  "Convert a flat VM PROGRAM into a MIR module.

The converter creates label-aligned MIR blocks, links obvious CFG edges, and
stores each source VM instruction in instruction metadata so unsupported ops can
round-trip safely through the MIR path."
  (unless (typep program 'vm-program)
    (error 'isel-diagnostic :message (format nil "expected VM-PROGRAM, got ~S" program)))
  (let* ((fn (mir-make-function name))
         (module (make-mir-module :functions (list fn)))
         (entry (mirf-entry fn))
         (current entry)
         (reg-map (make-hash-table :test #'eql))
         (label-blocks (make-hash-table :test #'equal)))
    (setf (gethash :entry label-blocks) entry)
    (dolist (inst (vm-program-instructions program))
      (if (typep inst 'vm-label)
          (let* ((label (%vm-label-name-string inst))
                 (block (or (gethash label label-blocks)
                            (setf (gethash label label-blocks)
                                  (mir-new-block fn :label (intern (string-upcase label) :keyword))))))
            (unless (eq current block)
              (when (and current (null (mirb-succs current)))
                (mir-add-succ current block))
              (setf current block))
            (%emit-pass-through-mir current inst))
          (%lower-vm-inst-to-mir fn current inst reg-map)))
    ;; Link branch targets after all label blocks have been discovered.
    (dolist (block (mirf-blocks fn))
      (dolist (inst (mirb-insts block))
        (let ((vm-inst (%mir-meta-get inst :vm-inst)))
          (when (typep vm-inst '(or vm-jump vm-jump-zero))
            (let ((target (gethash (vm-label-name vm-inst) label-blocks)))
              (when target (mir-add-succ block target)))))))
    (dolist (block (mirf-blocks fn))
      (unless (mirb-sealed-p block)
        (mir-seal-block fn block)))
    (mir-dominators fn)
    module))

(defun %operand-constant-value (operand constants)
  (cond
    ((mir-const-p operand) (values (mirc-value operand) t))
    ((mir-value-p operand)
     (let ((entry (gethash operand constants)))
       (if entry (values (cdr entry) t) (values nil nil))))
    (t (values nil nil))))

(defun %mir-constant-fold-value (op srcs constants)
  (when (= (length srcs) 2)
    (multiple-value-bind (a ap) (%operand-constant-value (first srcs) constants)
      (multiple-value-bind (b bp) (%operand-constant-value (second srcs) constants)
        (when (and ap bp (integerp a) (integerp b))
          (case op
            (:add (+ a b))
            (:sub (- a b))
            (:mul (* a b))
            (t nil)))))))

(defun optimize-mir-module-for-isel (module)
  "Run small MIR-level SSA passes needed by the MIR pipeline.

Implemented passes are intentionally conservative: integer constant folding and
pure-expression CSE inside each function.  Side-effecting VM instructions remain
metadata-preserved pass-through nodes."
  (dolist (fn (mirm-functions module) module)
    (dolist (block (mir-rpo fn))
      (let ((constants (make-hash-table :test #'eq))
            (cse (make-hash-table :test #'equal)))
        (dolist (inst (append (reverse (mirb-phis block)) (mirb-insts block)))
          (let ((dst (miri-dst inst)))
            (cond
              ((and dst (eq (miri-op inst) :const) (first (miri-srcs inst)))
               (let ((src (first (miri-srcs inst))))
                 (when (mir-const-p src)
                   (setf (gethash dst constants) (cons :const (mirc-value src))))))
              ((and dst (member (miri-op inst) '(:add :sub :mul) :test #'eq))
               (let ((folded (%mir-constant-fold-value (miri-op inst) (miri-srcs inst) constants)))
                 (if folded
                     (progn
                       (setf (miri-op inst) :const
                             (miri-srcs inst) (list (make-mir-const :value folded :type :integer)))
                       (setf (gethash dst constants) (cons :const folded)))
                     (let ((key (list (miri-op inst)
                                      (mapcar (lambda (src)
                                                (if (mir-value-p src) (mirv-id src) src))
                                              (miri-srcs inst)))))
                       (let ((previous (gethash key cse)))
                         (if previous
                             (setf (miri-op inst) :move
                                   (miri-srcs inst) (list previous))
                             (setf (gethash key cse) dst))))))))))))))

(defun %mir-operand-tree (operand)
  (cond
    ((mir-const-p operand) (list :const (mirc-value operand)))
    ((mir-value-p operand)
     (let ((def (mirv-def-inst operand)))
       (if (and def (member (miri-op def) '(:const :add :sub :mul :move :load) :test #'eq))
           (%mir-inst-tree def)
           (list :reg (mirv-name operand)))))
    (t (list :literal operand))))

(defun %mir-inst-tree (inst)
  (cons (miri-op inst) (mapcar #'%mir-operand-tree (miri-srcs inst))))

(defun %mir-dst-name (inst)
  (and (miri-dst inst) (mirv-name (miri-dst inst))))

(defun %emit-selected-vm-inst (inst target)
  (let ((original (%mir-meta-get inst :vm-inst)))
    (handler-case
        (progn
          (isel-maximal-munch (%mir-inst-tree inst) target)
          (case (miri-op inst)
            (:const (let ((src (first (miri-srcs inst))))
                      (if (mir-const-p src)
                          (make-vm-const :dst (%mir-dst-name inst) :value (mirc-value src))
                          original)))
            (:move (let ((src (first (miri-srcs inst))))
                     (if (mir-value-p src)
                         (make-vm-move :dst (%mir-dst-name inst) :src (mirv-name src))
                         original)))
            (:add (make-vm-add :dst (%mir-dst-name inst)
                                :lhs (mirv-name (first (miri-srcs inst)))
                                :rhs (mirv-name (second (miri-srcs inst)))))
            (:sub (make-vm-sub :dst (%mir-dst-name inst)
                                :lhs (mirv-name (first (miri-srcs inst)))
                                :rhs (mirv-name (second (miri-srcs inst)))))
            (:mul (make-vm-mul :dst (%mir-dst-name inst)
                                :lhs (mirv-name (first (miri-srcs inst)))
                                :rhs (mirv-name (second (miri-srcs inst)))))
            (t original)))
      (isel-diagnostic (condition)
        (if original
            original
            (error condition))))))

(defun %lower-phis-to-vm-moves (block)
  "Conservatively lower phi nodes to edge-like moves when a unique source exists."
  (loop for phi in (reverse (mirb-phis block))
        for dst = (and (miri-dst phi) (mirv-name (miri-dst phi)))
        for unique = (remove-duplicates (mapcar #'cdr (miri-srcs phi)) :test #'eq)
        when (and dst (= (length unique) 1) (mir-value-p (first unique)))
          collect (make-vm-move :dst dst :src (mirv-name (first unique)))))

(defun mir-module->vm-program (module template-program &key (target :x86-64))
  "Select target instructions from MODULE and return a VM-PROGRAM.

The selected representation is still the existing VM instruction format.  Spill
handling remains deferred to the current register allocator."
  (let ((instructions '()))
    (dolist (fn (mirm-functions module))
      (dolist (block (mir-rpo fn))
        (setf instructions (append instructions (%lower-phis-to-vm-moves block)))
        (dolist (inst (mirb-insts block))
          (let ((vm-inst (%emit-selected-vm-inst inst target)))
            (when vm-inst (push vm-inst instructions))))))
    (make-vm-program :instructions (nreverse instructions)
                     :result-register (vm-program-result-register template-program)
                     :leaf-p (vm-program-leaf-p template-program)
                     :calling-convention (vm-program-calling-convention template-program)
                     :function-conventions (vm-program-function-conventions template-program))))

(defun isel-vm-program (program &key (target :x86-64))
  "Route PROGRAM through VM -> MIR -> MIR passes -> ISel -> VM."
  (let ((module (vm-program->mir-module program)))
    (optimize-mir-module-for-isel module)
    (mir-module->vm-program module program :target target)))
