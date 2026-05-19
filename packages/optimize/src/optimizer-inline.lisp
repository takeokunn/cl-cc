;;;; optimizer-inline.lisp — Function Inlining Pass
;;;;
;;;; Inlines small, non-capturing, linear functions at call sites.
;;;; Eligibility: vm-closure with no captured vars, body ≤ threshold insts,
;;;;              no internal jumps (linear body ending in vm-ret).
;;;;
;;;; Algorithm:
;;;;   1. Scan once to map label → (params, linear-body).
;;;;   2. Forward scan: when a known small function's register is called,
;;;;      replace vm-call with: arg-moves + renamed body + result-move.
;;;;   Register renaming uses fresh :R<N> indices above the program's max.

(in-package :cl-cc/optimize)

(defvar *opt-enable-sealed-gf-devirtualization* t
  "Policy gate for sealed+satiated generic-function static dispatch.")

(defun opt-max-reg-index (instructions)
  "Return the maximum integer N where :RN is used in INSTRUCTIONS, or -1."
  (let ((max-idx -1))
    (dolist (inst instructions max-idx)
      (dolist (reg (cons (opt-inst-dst inst) (opt-inst-read-regs inst)))
        (when (and reg (opt-register-keyword-p reg))
          (let* ((name (symbol-name reg))
                 (idx (ignore-errors (parse-integer name :start 1))))
            (when (and idx (> idx max-idx))
              (setf max-idx idx))))))))

(defun opt-make-renaming (body-instructions base-index)
  "Build renaming table: existing-register → fresh :R<N> (starting at BASE-INDEX).
   Uses opt-inst-dst + opt-inst-read-regs to discover ALL registers, including
   those not serialized by instruction->sexp (e.g., vm-make-obj initarg-regs)."
  (let ((seen (make-hash-table :test #'eq))
        (counter base-index)
        (renaming (make-hash-table :test #'eq)))
    (flet ((add (r)
             (when (and r (opt-register-keyword-p r))
               (unless (gethash r seen)
                 (setf (gethash r seen) t)
                 (setf (gethash r renaming)
                       (intern (format nil "R~A" counter) :keyword))
                 (incf counter)))))
      (dolist (inst body-instructions)
        (add (opt-inst-dst inst))
        (dolist (r (opt-inst-read-regs inst)) (add r))))
    renaming))

(defun %opt-collect-sexp-regs-into-cell (form cell)
  "Recursively push register keywords from sexp FORM onto (car CELL)."
  (cond ((and (keywordp form) (opt-register-keyword-p form))
         (push form (car cell)))
        ((consp form)
         (%opt-collect-sexp-regs-into-cell (car form) cell)
         (%opt-collect-sexp-regs-into-cell (cdr form) cell))))

(defun opt-can-safely-rename-p (body-instructions)
  "T if all instructions in BODY can be safely register-renamed via sexp roundtrip.
   An instruction is safe when instruction->sexp captures all its registers —
   i.e., opt-inst-read-regs reports the same registers as appear in the sexp.
   Instructions with custom sexp methods (vm-make-obj, vm-slot-read, etc.) that
   omit registers from their sexp representation will cause this to return NIL."
  (dolist (inst body-instructions t)
    (let ((explicit-regs (remove nil
                                 (cons (opt-inst-dst inst)
                                       (opt-inst-read-regs inst))))
          (sexp-regs-cell (list nil)))
      (handler-case (%opt-collect-sexp-regs-into-cell (instruction->sexp inst) sexp-regs-cell)
        (error () (return-from opt-can-safely-rename-p nil)))
      (let ((sexp-regs (car sexp-regs-cell)))
        (unless (every (lambda (r) (member r sexp-regs)) explicit-regs)
          (return-from opt-can-safely-rename-p nil))))))

(defun opt-rename-regs-in-inst (inst renaming)
  "Return INST with all VM register keywords substituted per RENAMING.
   Uses instruction→sexp roundtrip; returns INST unchanged on any error."
  (handler-case
      (sexp->instruction
       (opt-map-tree (lambda (x) (if (and (keywordp x) (opt-register-keyword-p x))
                                     (or (gethash x renaming) x)
                                     x))
                     (instruction->sexp inst)))
    (error () inst)))

(defun opt-collect-function-defs (instructions)
  "Return hash-table label → (:params params :body body-insts).
    Only includes functions that are:
    - Registered via vm-closure/vm-func-ref with known params
    - Linear: no internal jumps; body ends with exactly one vm-ret"
  (let ((label-to-params (make-hash-table :test #'equal))
        (label-to-body   (make-hash-table :test #'equal))
        (in-fn nil) (cur-label nil) (cur-body nil) (has-jump nil))
    ;; Collect params from callable reference instructions
    (dolist (inst instructions)
      (when (and (typep inst '(or vm-closure vm-func-ref))
                 (vm-closure-params inst))
        (setf (gethash (vm-label-name inst) label-to-params)
              (vm-closure-params inst))))
    ;; Collect linear bodies (label → instructions ending in vm-ret)
    (dolist (inst instructions)
      (typecase inst
        (vm-label
         ;; Nested label: abandon any in-progress body (non-linear)
         (when in-fn
           (setf in-fn nil cur-label nil cur-body nil has-jump nil))
         (setf in-fn t cur-label (vm-name inst) cur-body nil has-jump nil))
        ((or vm-jump vm-jump-zero)
         (setf has-jump t)
         (when in-fn (push inst cur-body)))
        (vm-ret
         (when (and in-fn (not has-jump))
           (push inst cur-body)
           (setf (gethash cur-label label-to-body) (nreverse cur-body)))
         (setf in-fn nil cur-label nil cur-body nil has-jump nil))
        (t (when in-fn (push inst cur-body)))))
    ;; Combine: label must appear in both tables
    ;; Also build reverse map: label → callable reference instruction (for capture/metadata checks)
    (let ((label-to-closure (make-hash-table :test #'equal))
          (result (make-hash-table :test #'equal)))
      (dolist (inst instructions)
        (when (typep inst '(or vm-closure vm-func-ref))
          (setf (gethash (vm-label-name inst) label-to-closure) inst)))
      (maphash (lambda (lbl params)
                 (let ((body    (gethash lbl label-to-body))
                       (closure (gethash lbl label-to-closure)))
                   (when (and body closure)
                     (setf (gethash lbl result)
                           (list :closure closure :params params :body body)))))
               label-to-params)
      result)))

(defun opt-body-has-global-refs-p (body-instructions params)
  "Return T if BODY-INSTRUCTIONS read any register that is neither in PARAMS
   nor defined as a DST by a prior body instruction.  Such 'global registers'
   (e.g., class descriptors set by defclass at the top level) would be renamed
   to fresh uninitialized registers if the function were inlined, breaking
   correctness.  Functions with global refs must not be inlined."
  (let ((safe (make-hash-table :test #'eq)))
    (dolist (p params) (setf (gethash p safe) t))
    (dolist (inst body-instructions nil)
      (dolist (r (opt-inst-read-regs inst))
        (unless (gethash r safe)
          (return-from opt-body-has-global-refs-p t)))
      (let ((dst (opt-inst-dst inst)))
        (when dst (setf (gethash dst safe) t))))))

(defun opt-build-function-name-map (instructions)
  "Return symbol → function-label mapping for top-level function registrations."
  (let ((reg-track (make-hash-table :test #'eq))
        (name-to-label (make-hash-table :test #'eq)))
    (dolist (inst instructions)
      (typecase inst
        ((or vm-closure vm-func-ref)
         (let ((label (vm-label-name inst)))
           (when label
             (setf (gethash (vm-dst inst) reg-track) label))))
        (vm-register-function
         (let ((label (or (gethash (vm-src inst) reg-track)
                          (dolist (i instructions)
                            (when (and (vm-closure-p i)
                                       (eq (vm-dst i) (vm-src inst)))
                              (return (vm-label-name i)))))))
            (when label
              (setf (gethash (vm-func-name inst) name-to-label) label))))))
    name-to-label))

(defun opt-known-callee-labels (instructions)
  "Return reg -> known callee label mapping tracked through simple designators." 
  (let ((name-to-label (opt-build-function-name-map instructions))
        (reg-track (make-hash-table :test #'eq)))
    (dolist (inst instructions reg-track)
      (typecase inst
        ((or vm-closure vm-func-ref)
         (setf (gethash (vm-dst inst) reg-track) (vm-label-name inst)))
        (vm-const
         (let ((label (and (symbolp (vm-value inst))
                           (gethash (vm-value inst) name-to-label))))
           (if label
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track))))
        (vm-move
         (multiple-value-bind (label found-p)
             (gethash (vm-move-src inst) reg-track)
           (if found-p
                (setf (gethash (vm-dst inst) reg-track) label)
                (remhash (vm-dst inst) reg-track))))
        (t
         (let ((dst (opt-inst-dst inst)))
             (when dst
               (remhash dst reg-track))))))))

(defun %opt-call-site-split-fresh-label (used-labels)
  "Return a fresh after-call label not present in USED-LABELS."
  (loop for i from 0
        for label = (format nil "CALL-SITE-SPLIT-AFTER-~D" i)
        unless (gethash label used-labels)
          do (setf (gethash label used-labels) t)
             (return label)))

(defun %opt-known-callee-before-index (instructions end-index func-reg name-to-label)
  "Return FUNC-REG's known label immediately before END-INDEX, within one block."
  (loop for i downfrom (1- end-index) downto 0
        for inst = (nth i instructions)
        do (cond
             ((or (typep inst 'vm-label)
                  (typep inst 'vm-jump)
                  (typep inst 'vm-jump-zero)
                  (typep inst 'vm-ret)
                  (typep inst 'vm-halt))
              (return nil))
             ((eq (opt-inst-dst inst) func-reg)
              (return
                (typecase inst
                  ((or vm-closure vm-func-ref)
                   (vm-label-name inst))
                  (vm-const
                   (and (symbolp (vm-value inst))
                        (gethash (vm-value inst) name-to-label)))
                  (t nil)))))))

(defun %opt-copy-vm-call (call)
  "Return a fresh copy of CALL for predecessor-local call-site splitting."
  (make-vm-call :dst (vm-dst call)
                :func (vm-func-reg call)
                :args (copy-list (vm-args call))))

(defun %opt-call-like-p (inst)
  "Return T when INST is a call shape supported by call-site splitting."
  (typep inst '(or vm-call vm-tail-call vm-apply)))

(defun %opt-copy-call-like (call)
  "Return a fresh copy of CALL for predecessor-local call-site splitting."
  (typecase call
    (vm-call (%opt-copy-vm-call call))
    (vm-tail-call (make-vm-tail-call :dst (vm-dst call) :func (vm-func-reg call) :args (copy-list (vm-args call))))
    (vm-apply (make-vm-apply :dst (vm-dst call)
                             :func (vm-func-reg call)
                             :args (copy-list (vm-args call))
                             :tail-p (cl-cc/vm::vm-tail-p call)))))

(defun %opt-call-site-split-join-labels (instructions call-index)
  "Return all consecutive labels immediately preceding CALL-INDEX."
  (loop for i downfrom (1- call-index) downto 0
        for inst = (nth i instructions)
        while (typep inst 'vm-label)
        collect (vm-name inst)))

(defun %opt-callable-type-proof-before-index-p (instructions end-index func-reg)
  "Return T when the predecessor locally proves FUNC-REG is function-callable."
  (loop for i downfrom (1- end-index) downto 0
        for inst = (nth i instructions)
        do (cond
             ((or (typep inst 'vm-label) (typep inst 'vm-jump) (typep inst 'vm-ret) (typep inst 'vm-halt))
              (return nil))
             ((and (typep inst 'vm-typep)
                   (eq (vm-src inst) func-reg)
                   (member (vm-type-name inst) '(function compiled-function) :test #'eq))
              (return t))
             ((and (typep inst 'vm-function-p) (eq (vm-src inst) func-reg))
              (return t)))))

(defun %opt-call-site-split-replacement (call-inst func-reg callee-label after-label)
  "Build replacement instructions for a predecessor split of CALL-INST."
  (append (when callee-label (list (make-vm-func-ref :dst func-reg :label callee-label)))
          (list (%opt-copy-call-like call-inst))
          (list (make-vm-jump :label after-label))))

(defun opt-pass-call-site-splitting (instructions)
  "Duplicate join-block call sites into predecessors with known callees.
Handles consecutive multi-join labels and `vm-call`/`vm-tail-call`/`vm-apply`.
The original join call remains available for fall-through and unknown preds."
  (let* ((len (length instructions))
         (name-to-label (opt-build-function-name-map instructions))
         (used-labels (make-hash-table :test #'equal))
         (replacements (make-hash-table :test #'eql))
         (after-labels (make-hash-table :test #'eql)))
    (dolist (inst instructions)
      (when (typep inst 'vm-label)
        (setf (gethash (vm-name inst) used-labels) t)))
    (loop for call-index from 1 below len
          for call-inst = (nth call-index instructions)
          for join-labels = (%opt-call-site-split-join-labels instructions call-index)
          when (and join-labels (%opt-call-like-p call-inst))
            do (let ((func-reg (vm-func-reg call-inst))
                     (split-jumps nil))
                 (dolist (join-label join-labels)
                   (loop for jump-index from 0 below call-index
                         for jump-inst = (nth jump-index instructions)
                         when (and (typep jump-inst 'vm-jump)
                                   (equal (vm-label-name jump-inst) join-label))
                           do (let ((callee-label
                                      (%opt-known-callee-before-index
                                       instructions jump-index func-reg name-to-label)))
                                (when (or callee-label
                                          (%opt-callable-type-proof-before-index-p
                                           instructions jump-index func-reg))
                                  (push (list jump-index callee-label) split-jumps)))))
                 (when split-jumps
                   (let ((after-label (%opt-call-site-split-fresh-label used-labels)))
                     (setf (gethash call-index after-labels) after-label)
                     (dolist (split split-jumps)
                       (destructuring-bind (jump-index callee-label) split
                         (setf (gethash jump-index replacements)
                               (%opt-call-site-split-replacement
                                call-inst func-reg callee-label after-label))))))))
    (loop for index from 0 below len
          for inst = (nth index instructions)
          append (or (gethash index replacements)
                     (let ((after-label (gethash index after-labels)))
                       (if after-label
                           (list inst (make-vm-label :name after-label))
                           (list inst)))))))

(defun %opt-devirt-track-designator (inst name-to-label reg-track)
  "Update REG-TRACK with known callee information produced by INST."
  (typecase inst
    ((or vm-closure vm-func-ref)
     (setf (gethash (vm-dst inst) reg-track) (vm-label-name inst)))
    (vm-const
     (let ((label (and (symbolp (vm-value inst))
                       (gethash (vm-value inst) name-to-label))))
       (if label
           (setf (gethash (vm-dst inst) reg-track) label)
           (remhash (vm-dst inst) reg-track))))
    (vm-move
     (multiple-value-bind (label found-p)
         (gethash (vm-move-src inst) reg-track)
       (if found-p
           (setf (gethash (vm-dst inst) reg-track) label)
           (remhash (vm-dst inst) reg-track))))
    (t
     (let ((dst (opt-inst-dst inst)))
       (when dst
         (remhash dst reg-track))))))

(defun %opt-last-emitted-func-ref-p (result reg label)
  "Return T when RESULT already starts with REG's direct reference to LABEL."
  (let ((last (first result)))
    (and (typep last 'vm-func-ref)
         (eq (vm-dst last) reg)
         (equal (vm-label-name last) label))))

(defun %opt-devirt-call (inst reg-track result)
  "Emit a direct callee reference before INST when its callee register is known."
  (let* ((func-reg (vm-func-reg inst))
         (label (gethash func-reg reg-track)))
    (when (and label
               (not (%opt-last-emitted-func-ref-p result func-reg label)))
      (push (make-vm-func-ref :dst func-reg :label label) result))
    (push inst result)
    (let ((dst (opt-inst-dst inst)))
      (when dst
        (remhash dst reg-track)))
    result))

(defun %opt-clear-reg-facts (reg &rest tables)
  "Remove REG from each hash-table in TABLES."
  (when reg
    (dolist (table tables)
      (remhash reg table))))

(defun %opt-set-reg-fact (reg table value &rest other-tables)
  "Set REG's fact in TABLE and clear REG from OTHER-TABLES."
  (when reg
    (setf (gethash reg table) value)
    (dolist (other other-tables)
      (remhash reg other))))

(defun %opt-eql-specializer-p (specializer)
  "Return T when SPECIALIZER contains an EQL specializer."
  (cond
    ((and (consp specializer) (eq (car specializer) 'eql)) t)
    ((consp specializer) (some #'%opt-eql-specializer-p specializer))
    (t nil)))

(defun %opt-standard-combination-p (combination)
  "Return T when COMBINATION denotes the standard method combination."
  (or (null combination) (eq combination 'standard)))

(defun %opt-gf-info (gf-name gf-infos)
  "Return mutable compile-time metadata for GF-NAME."
  (or (gethash gf-name gf-infos)
      (setf (gethash gf-name gf-infos)
            (list :methods nil
                  :satiated nil
                  :combination nil
                  :unsafe nil))))

(defun %opt-hash-gf-info (gf-ht)
  "Build GF metadata from a literal GF hash table when possible."
  (when (and (hash-table-p gf-ht) (gethash :__methods__ gf-ht))
    (let ((methods nil)
          (methods-ht (gethash :__methods__ gf-ht)))
      (when (hash-table-p methods-ht)
        (maphash (lambda (specializer method)
                   (let* ((fn (and (hash-table-p method)
                                   (gethash :function method)))
                          (label (cond
                                   ((stringp fn) fn)
                                   ((typep fn 'vm-closure-object)
                                    (vm-closure-entry-label fn))
                                   (t nil))))
                     (push (list :specializer specializer
                                 :qualifier (and (hash-table-p method)
                                                 (first (gethash :qualifiers method)))
                                 :label label)
                           methods)))
                 methods-ht))
      (list :methods methods
            :satiated (and (gethash :__satiated__ gf-ht) t)
            :combination (gethash :__method-combination__ gf-ht)
            :unsafe nil))))

(defun %opt-track-sealed-gf-facts (inst class-sealed reg-name reg-class reg-object-class
                                   reg-const reg-closure-label reg-gf-literal gf-infos)
  "Update compile-time facts used by sealed generic-function devirtualization."
  (let ((dst (opt-inst-dst inst)))
    (typecase inst
      (vm-class-def
       (%opt-set-reg-fact dst reg-class (vm-class-name-sym inst)
                          reg-name reg-object-class reg-const reg-closure-label reg-gf-literal))
      (vm-get-global
       (%opt-set-reg-fact dst reg-name (vm-global-name inst)
                          reg-class reg-object-class reg-const reg-closure-label reg-gf-literal))
      (vm-const
       (%opt-set-reg-fact dst reg-const (vm-value inst)
                          reg-name reg-class reg-object-class reg-closure-label reg-gf-literal)
       (let ((literal-info (%opt-hash-gf-info (vm-value inst))))
         (when literal-info
           (setf (gethash dst reg-gf-literal) literal-info))))
      ((or vm-closure vm-func-ref)
       (%opt-set-reg-fact dst reg-closure-label (vm-label-name inst)
                          reg-name reg-class reg-object-class reg-const reg-gf-literal))
      (vm-move
       (let ((src (vm-move-src inst)))
         (%opt-clear-reg-facts dst reg-name reg-class reg-object-class
                               reg-const reg-closure-label reg-gf-literal)
         (dolist (pair `((,reg-name . ,(gethash src reg-name))
                         (,reg-class . ,(gethash src reg-class))
                         (,reg-object-class . ,(gethash src reg-object-class))
                         (,reg-const . ,(gethash src reg-const))
                         (,reg-closure-label . ,(gethash src reg-closure-label))
                         (,reg-gf-literal . ,(gethash src reg-gf-literal))))
           (multiple-value-bind (value found-p) (gethash src (car pair))
             (declare (ignore value))
             (when found-p
               (setf (gethash dst (car pair)) (cdr pair)))))))
      (vm-make-obj
       (let ((class-name (gethash (vm-class-reg inst) reg-name)))
         (%opt-clear-reg-facts dst reg-name reg-class reg-object-class
                               reg-const reg-closure-label reg-gf-literal)
         (when (and class-name (gethash class-name class-sealed))
           (setf (gethash dst reg-object-class) class-name))))
      (vm-register-method
       (let ((gf-name (gethash (vm-gf-reg inst) reg-name))
             (label (gethash (vm-method-reg inst) reg-closure-label)))
         (when gf-name
           (let ((info (%opt-gf-info gf-name gf-infos)))
             ;; Runtime registration re-opens a satiated GF.  The optimizer only
             ;; treats a later explicit :__satiated__ write as a closed-world seal.
             (setf (getf info :satiated) nil)
             (if (vm-method-qualifier inst)
                 (setf (getf info :unsafe) t)
                 (push (list :specializer (vm-method-specializer inst)
                             :qualifier nil
                             :label label)
                       (getf info :methods)))))))
      (vm-sethash
       (let ((table-name (gethash (vm-hash-table-reg inst) reg-name)))
         (when table-name
           (multiple-value-bind (key key-known-p)
               (gethash (vm-hash-key inst) reg-const)
             (multiple-value-bind (value value-known-p)
                 (gethash (vm-hash-value inst) reg-const)
               (let ((info (%opt-gf-info table-name gf-infos)))
                 (cond
                   ((and key-known-p (eq key :__satiated__))
                    (if value-known-p
                        (setf (getf info :satiated) (not (opt-falsep value)))
                        (setf (getf info :unsafe) t)))
                   ((and key-known-p (eq key :__method-combination__))
                    (if value-known-p
                        (setf (getf info :combination) value)
                        (setf (getf info :unsafe) t)))
                   ((not key-known-p)
                    (setf (getf info :unsafe) t)))))))))
      (t
       (%opt-clear-reg-facts dst reg-name reg-class reg-object-class
                             reg-const reg-closure-label reg-gf-literal)))))

(defun %opt-single-sealed-primary-method-label (inst class-sealed reg-name reg-object-class
                                                reg-gf-literal gf-infos)
  "Return direct method label for safe sealed+satiated generic call INST, or NIL."
  (when (= (length (vm-args inst)) 1)
    (let* ((arg-class (gethash (first (vm-args inst)) reg-object-class))
           (gf-name (gethash (vm-gf-reg inst) reg-name))
           (info (or (and gf-name (gethash gf-name gf-infos))
                     (gethash (vm-gf-reg inst) reg-gf-literal))))
      (when (and arg-class
                 (gethash arg-class class-sealed)
                 info
                 (getf info :satiated)
                 (not (getf info :unsafe))
                 (%opt-standard-combination-p (getf info :combination)))
        (let ((primary-methods
                (remove-if (lambda (method)
                             (or (getf method :qualifier)
                                 (%opt-eql-specializer-p (getf method :specializer))))
                           (copy-list (getf info :methods)))))
          (when (and (= (length primary-methods) 1)
                     (notany (lambda (method)
                               (%opt-eql-specializer-p (getf method :specializer)))
                             (getf info :methods)))
            (let ((method (first primary-methods)))
              (when (equal (getf method :specializer) arg-class)
                (getf method :label)))))))))

(defun %opt-fresh-register-generator (instructions)
  "Return a closure producing fresh VM register keywords for INSTRUCTIONS."
  (let ((next (1+ (opt-max-reg-index instructions))))
    (lambda ()
      (prog1 (intern (format nil "R~A" next) :keyword)
        (incf next)))))

(defun %opt-devirt-generic-call (inst class-sealed reg-name reg-object-class
                                 reg-gf-literal gf-infos fresh-reg result)
  "Replace INST with direct vm-call when sealed GF dispatch is statically unique."
  (let ((label (%opt-single-sealed-primary-method-label inst class-sealed reg-name
                                                       reg-object-class reg-gf-literal
                                                       gf-infos)))
    (if label
        (let ((func-reg (funcall fresh-reg)))
          (push (make-vm-call :dst (vm-dst inst) :func func-reg :args (copy-list (vm-args inst))) result)
          (push (make-vm-func-ref :dst func-reg :label label) result))
        (push inst result)))
  result)

(defun opt-pass-devirtualize (instructions)
  "Insert direct function references before calls whose callee register is known.
This implements a conservative called-value propagation slice: closure,
function-reference, registered symbol, and move designators are tracked through a
single forward scan, and `vm-call`/`vm-tail-call`/`vm-apply` sites with a known
callee receive a `vm-func-ref` immediately before the call.  The call itself is
left in place so existing VM semantics and the inline pass remain unchanged.

For sealed and explicitly satiated generic functions, this pass also performs a
more aggressive static-dispatch optimization: a `vm-generic-call` whose receiver
object is known to be an instance of a sealed class is rewritten to a direct
`vm-call` when the GF has exactly one applicable unqualified primary method,
uses the standard method combination, and has no EQL-specializer ambiguity."
  (let ((name-to-label (opt-build-function-name-map instructions))
        (reg-track (make-hash-table :test #'eq))
        (class-sealed (make-hash-table :test #'equal))
        (reg-name (make-hash-table :test #'eq))
        (reg-class (make-hash-table :test #'eq))
        (reg-object-class (make-hash-table :test #'eq))
        (reg-const (make-hash-table :test #'eq))
        (reg-closure-label (make-hash-table :test #'eq))
        (reg-gf-literal (make-hash-table :test #'eq))
        (gf-infos (make-hash-table :test #'equal))
        (fresh-reg (%opt-fresh-register-generator instructions))
        (result nil))
    (dolist (inst instructions)
      (when (typep inst 'vm-class-def)
        (setf (gethash (vm-class-name-sym inst) class-sealed)
              (and (cl-cc/vm::vm-sealed-p inst) t))))
    (dolist (inst instructions (nreverse result))
      (typecase inst
        ((or vm-call vm-tail-call vm-apply)
          (setf result (%opt-devirt-call inst reg-track result)))
        (vm-generic-call
         (when *opt-enable-sealed-gf-devirtualization*
           (setf result (%opt-devirt-generic-call inst class-sealed reg-name
                                                  reg-object-class reg-gf-literal
                                                  gf-infos fresh-reg result)))
         (unless *opt-enable-sealed-gf-devirtualization*
           (push inst result))
         (%opt-clear-reg-facts (opt-inst-dst inst) reg-track reg-name reg-class
                               reg-object-class reg-const reg-closure-label
                               reg-gf-literal))
        (t
          (%opt-devirt-track-designator inst name-to-label reg-track)
          (%opt-track-sealed-gf-facts inst class-sealed reg-name reg-class
                                      reg-object-class reg-const reg-closure-label
                                      reg-gf-literal gf-infos)
          (push inst result))))))
