;;;; optimizer-loop-fusion.lisp — FR-514 loop fusion and loop fission

(in-package :cl-cc/optimize)

(defparameter *opt-loop-fission-min-core-size* 8
  "Minimum pure core size considered for FR-514 loop fission.")

(defun %loop-fr514-copy-inst (inst)
  "Return a structural copy of INST when the VM sexp codec supports it."
  (handler-case
      (sexp->instruction (instruction->sexp inst))
    (error () inst)))

(defun %loop-fr514-label-name (base suffix)
  "Return a fresh-ish keyword label derived from BASE and SUFFIX."
  (intern (format nil "~A__~A" base suffix) :keyword))

(defun %loop-fr514-find-label-index (vec name &optional (start 0))
  (loop for i from start below (length vec)
        for inst = (aref vec i)
        when (and (typep inst 'vm-label) (equal (vm-name inst) name))
          do (return i)))

(defun %loop-fr514-parse-canonical-loop-at (vec i)
  "Parse the optimizer's linear canonical counted-loop shape at VEC[I]."
  (when (and (< (+ i 5) (length vec))
             (typep (aref vec i) 'vm-label)
             (typep (aref vec (1+ i)) 'vm-lt)
             (typep (aref vec (+ i 2)) 'vm-jump-zero))
    (let* ((head (aref vec i))
           (cmp (aref vec (1+ i)))
           (jz (aref vec (+ i 2)))
           (head-label (vm-name head))
           (exit-label (vm-label-name jz))
           (exit-idx (%loop-fr514-find-label-index vec exit-label (+ i 3))))
      (when (and exit-idx (> exit-idx (+ i 4)))
        (let* ((back-idx (1- exit-idx))
               (back (aref vec back-idx))
               (body (loop for k from (+ i 3) below back-idx collect (aref vec k)))
               (step (car (last body))))
          (when (and (typep back 'vm-jump)
                     (equal (vm-label-name back) head-label)
                     (typep step 'vm-add)
                     (eq (vm-dst step) (vm-lhs step))
                     (eq (vm-dst step) (vm-lhs cmp))
                     (eq (vm-reg jz) (vm-dst cmp)))
            (make-opt-canonical-loop
             :head-index i
             :cmp-index (1+ i)
             :jz-index (+ i 2)
             :back-index back-idx
             :exit-index exit-idx
             :head-label head-label
             :exit-label exit-label
             :iv-reg (vm-lhs cmp)
             :limit-reg (vm-rhs cmp)
             :step-reg (vm-rhs step)
             :cond-reg (vm-dst cmp)
             :body body)))))))

(defun %loop-fr514-core-and-step (lp)
  (let ((body (opt-loop-body lp)))
    (values (butlast body) (car (last body)))))

(defun %loop-fr514-loop-seq (vec lp)
  (loop for k from (opt-loop-head-index lp) to (opt-loop-exit-index lp)
        collect (aref vec k)))

(defun %loop-fr514-constant-init (vec lp)
  "Return last dominating integer init for LP's IV, or NIL when unknown."
  (let ((iv (opt-loop-iv-reg lp))
        (value nil))
    (loop for i from 0 below (opt-loop-head-index lp)
          for inst = (aref vec i)
          do (cond
               ((and (typep inst 'vm-const)
                     (eq (vm-dst inst) iv)
                     (integerp (vm-value inst)))
                (setf value (vm-value inst)))
               ((and (opt-inst-dst inst) (eq (opt-inst-dst inst) iv))
                (setf value nil))))
    value))

(defun %loop-fr514-pure-inst-p (inst)
  "Return T for instructions safe to duplicate or merge across loop boundaries."
  (and (member (vm-inst-effect-kind inst) '(:pure :read-only) :test #'eq)
       (not (typep inst '(or vm-label vm-jump vm-jump-zero vm-ret vm-halt
                          vm-call vm-tail-call vm-trampoline vm-generic-call
                          vm-apply vm-set-global vm-slot-write vm-aset)))))

(defun %loop-fr514-pure-core-p (core)
  (every #'%loop-fr514-pure-inst-p core))

(defun %loop-fr514-same-iteration-space-p (vec left right)
  "Return T when LEFT and RIGHT have equal init/limit/step values.

The IV registers may differ; fusion rewrites RIGHT's IV uses to LEFT's IV."
  (and (not (eq (opt-loop-iv-reg left) (opt-loop-iv-reg right)))
       (equal (opt-loop-limit-reg left) (opt-loop-limit-reg right))
       (equal (opt-loop-step-reg left) (opt-loop-step-reg right))
       (equal (%loop-fr514-constant-init vec left)
              (%loop-fr514-constant-init vec right))))

(defun %loop-fr514-inst-depends-on-p (producer consumer)
  (let ((dst (opt-inst-dst producer)))
    (and dst (member dst (opt-inst-read-regs consumer) :test #'eq))))

(defun %loop-fr514-register-dependencies-p (left-core right-core)
  (or (some (lambda (a)
              (some (lambda (b) (%loop-fr514-inst-depends-on-p a b)) right-core))
            left-core)
      (some (lambda (b)
              (some (lambda (a) (%loop-fr514-inst-depends-on-p b a)) left-core))
            right-core)))

(defun %loop-fr514-memory-inst-p (inst)
  (typep inst '(or vm-aref vm-aset vm-slot-read vm-slot-write vm-get-global vm-set-global)))

(defun %loop-fr514-write-inst-p (inst)
  (typep inst '(or vm-aset vm-slot-write vm-set-global)))

(defun %loop-fr514-affine-access (inst iv const-env def-env)
  "Return (:array A :write-p P :stride S :offset O) for simple affine accesses.

Recognizes (aref/aset A i) and indexes computed as (+ (* i stride) offset)
when STRIDE/OFFSET are known constants."
  (when (typep inst '(or vm-aref vm-aset))
    (let ((array-reg (vm-array-reg inst))
          (index-reg (vm-index-reg inst)))
      (labels ((const (reg) (gethash reg const-env))
               (affine (reg)
                 (cond
                   ((eq reg iv) (values 1 0 t))
                   ((gethash reg const-env) (values 0 (gethash reg const-env) t))
                   (t
                    (let ((def (gethash reg def-env)))
                      (cond
                        ((and (typep def 'vm-add))
                         (multiple-value-bind (s1 o1 ok1) (affine (vm-lhs def))
                           (multiple-value-bind (s2 o2 ok2) (affine (vm-rhs def))
                             (when (and ok1 ok2)
                               (values (+ s1 s2) (+ o1 o2) t)))))
                        ((and (typep def 'vm-mul))
                         (cond
                           ((eq (vm-lhs def) iv)
                            (let ((k (const (vm-rhs def))))
                              (when k (values k 0 t))))
                           ((eq (vm-rhs def) iv)
                            (let ((k (const (vm-lhs def))))
                              (when k (values k 0 t)))))))))))))
        (multiple-value-bind (stride offset ok) (affine index-reg)
          (when ok
            (list :array array-reg
                  :write-p (typep inst 'vm-aset)
                  :stride stride
                  :offset offset))))))

(defun %loop-fr514-build-envs (instructions end-index)
  "Build constant and local definition environments before END-INDEX."
  (let ((const-env (make-hash-table :test #'eq))
        (def-env (make-hash-table :test #'eq)))
    (loop for inst in instructions
          for i from 0 below end-index
          do (let ((dst (opt-inst-dst inst)))
               (when dst
                 (setf (gethash dst def-env) inst)
                 (if (and (typep inst 'vm-const) (integerp (vm-value inst)))
                     (setf (gethash dst const-env) (vm-value inst))
                     (remhash dst const-env)))))
    (values const-env def-env)))

(defun %loop-fr514-gcd-test-safe-p (a b)
  "Return T when the GCD test proves no integer solution for equal subscripts."
  (let* ((sa (abs (getf a :stride 0)))
         (sb (abs (getf b :stride 0)))
         (g (gcd sa sb))
         (delta (- (getf b :offset 0) (getf a :offset 0))))
    (or (zerop g) (not (zerop (mod delta g))))))

(defun %loop-fr514-banerjee-safe-p (a b trip-count)
  "Return T when a simple Banerjee bound excludes cross-iteration overlap."
  (let* ((n (max 0 (or trip-count 0)))
         (sa (getf a :stride 0))
         (sb (getf b :stride 0))
         (delta (- (getf b :offset 0) (getf a :offset 0)))
         (lo (+ (min 0 (* sa (1- n))) (min 0 (* (- sb) (1- n)))))
         (hi (+ (max 0 (* sa (1- n))) (max 0 (* (- sb) (1- n))))))
    (or (< delta lo) (> delta hi))))

(defun %loop-fr514-trip-count (vec lp)
  (let ((init (%loop-fr514-constant-init vec lp)))
    (multiple-value-bind (const-env _def-env)
        (%loop-fr514-build-envs (coerce vec 'list) (opt-loop-head-index lp))
      (declare (ignore _def-env))
      (let ((limit (gethash (opt-loop-limit-reg lp) const-env))
            (step (gethash (opt-loop-step-reg lp) const-env)))
        (and init limit step (plusp step)
             (ceiling (max 0 (- limit init)) step))))))

(defun %loop-fr514-memory-dependencies-p (instructions lp-a core-a lp-b core-b)
  "Return T when memory dependence cannot be disproved by GCD/Banerjee tests."
  (multiple-value-bind (const-env def-env)
      (%loop-fr514-build-envs instructions (min (opt-loop-head-index lp-a)
                                                (opt-loop-head-index lp-b)))
    (let* ((trip (or (%loop-fr514-trip-count (coerce instructions 'vector) lp-a)
                     (%loop-fr514-trip-count (coerce instructions 'vector) lp-b)))
           (mem-a (remove-if-not #'%loop-fr514-memory-inst-p core-a))
           (mem-b (remove-if-not #'%loop-fr514-memory-inst-p core-b))
           (acc-a (remove nil (mapcar (lambda (inst)
                                        (%loop-fr514-affine-access inst (opt-loop-iv-reg lp-a)
                                                                  const-env def-env))
                                      mem-a)))
           (acc-b (remove nil (mapcar (lambda (inst)
                                        (%loop-fr514-affine-access inst (opt-loop-iv-reg lp-b)
                                                                  const-env def-env))
                                      mem-b))))
      (or (/= (length mem-a) (length acc-a))
          (/= (length mem-b) (length acc-b))
          (some (lambda (a)
                  (some (lambda (b)
                          (and (eql (getf a :array) (getf b :array))
                               (or (getf a :write-p) (getf b :write-p))
                               (not (or (%loop-fr514-gcd-test-safe-p a b)
                                        (and trip (%loop-fr514-banerjee-safe-p a b trip))))))
                        acc-b))
                acc-a)))))

(defun %loop-fr514-fusion-legal-p (instructions vec left right)
  (multiple-value-bind (core-a _step-a) (%loop-fr514-core-and-step left)
    (declare (ignore _step-a))
    (multiple-value-bind (core-b _step-b) (%loop-fr514-core-and-step right)
      (declare (ignore _step-b))
      (and (%loop-fr514-same-iteration-space-p vec left right)
           (%loop-fr514-pure-core-p core-a)
           (%loop-fr514-pure-core-p core-b)
           (not (%loop-fr514-register-dependencies-p core-a core-b))
           (not (%loop-fr514-memory-dependencies-p instructions left core-a right core-b))))))

(defun opt-pass-loop-fusion (instructions)
  "FR-514: fuse adjacent canonical loops with identical iteration spaces.

Fusion is applied only when both loop bodies are side-effect-free and dependency
legality is proven by register checks plus conservative GCD/Banerjee memory tests."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (out nil)
         (changed nil)
         (i 0))
    (labels ((emit (x) (push x out)))
      (loop while (< i n)
            do (let ((lp (%loop-fr514-parse-canonical-loop-at vec i)))
                 (if (null lp)
                     (progn (emit (aref vec i)) (incf i))
                     (let* ((next-i (1+ (opt-loop-exit-index lp)))
                            (lp2 (and (< next-i n)
                                      (%loop-fr514-parse-canonical-loop-at vec next-i))))
                       (if (and lp2 (%loop-fr514-fusion-legal-p instructions vec lp lp2))
                           (multiple-value-bind (core-a step-a) (%loop-fr514-core-and-step lp)
                             (multiple-value-bind (core-b _step-b) (%loop-fr514-core-and-step lp2)
                               (declare (ignore _step-b))
                               (let ((copies (make-hash-table :test #'eq)))
                                 (setf (gethash (opt-loop-iv-reg lp2) copies) (opt-loop-iv-reg lp))
                                 (emit (aref vec (opt-loop-head-index lp)))
                                 (emit (aref vec (opt-loop-cmp-index lp)))
                                 (emit (aref vec (opt-loop-jz-index lp)))
                                 (dolist (inst core-a) (emit inst))
                                 (dolist (inst core-b) (emit (opt-rewrite-inst-regs (%loop-fr514-copy-inst inst) copies)))
                                 (emit step-a)
                                 (emit (aref vec (opt-loop-back-index lp)))
                                 (emit (make-vm-label :name (opt-loop-exit-label lp2)))
                                 (setf changed t
                                       i (1+ (opt-loop-exit-index lp2))))))
                           (progn
                             (dolist (inst (%loop-fr514-loop-seq vec lp)) (emit inst))
                             (setf i (1+ (opt-loop-exit-index lp)))))))))
      (if changed (nreverse out) instructions))))

(defun %loop-fr514-independent-split-index (core)
  "Return a dependency-safe split point in CORE, or NIL."
  (loop for split from 1 below (length core)
        for left = (subseq core 0 split)
        for right = (subseq core split)
        unless (%loop-fr514-register-dependencies-p left right)
          do (return split)))

(defun %loop-fr514-emit-loop (vec lp core suffix init-value result)
  "Emit a cloned loop over CORE into RESULT and return updated reversed result."
  (let* ((head-name (%loop-fr514-label-name (opt-loop-head-label lp) suffix))
         (exit-name (%loop-fr514-label-name (opt-loop-exit-label lp) suffix))
         (cmp (%loop-fr514-copy-inst (aref vec (opt-loop-cmp-index lp))))
         (jz (%loop-fr514-copy-inst (aref vec (opt-loop-jz-index lp))))
         (step (%loop-fr514-copy-inst (car (last (opt-loop-body lp))))))
    (when init-value
      (push (make-vm-const :dst (opt-loop-iv-reg lp) :value init-value) result))
    (push (make-vm-label :name head-name) result)
    (push cmp result)
    (push (make-vm-jump-zero :reg (vm-reg jz) :label exit-name) result)
    (dolist (inst core)
      (push (%loop-fr514-copy-inst inst) result))
    (push step result)
    (push (make-vm-jump :label head-name) result)
    (push (make-vm-label :name exit-name) result)
    result))

(defun %loop-fr514-fission-candidate-p (vec lp core split)
  (and split
       (>= (length core) *opt-loop-fission-min-core-size*)
       (%loop-fr514-pure-core-p core)
       (%loop-fr514-constant-init vec lp)
       (not (some #'%loop-fr514-memory-inst-p core))))

(defun opt-pass-loop-fission (instructions)
  "FR-514: split large independent loop bodies into separate loops.

The pass only fissions side-effect-free canonical loops whose core can be split
into two register-independent regions and whose IV can be reset to a known
constant initializer before the second loop.  This creates vectorization-friendly
single-purpose loops without changing loops with unknown state or memory effects."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (out nil)
         (changed nil)
         (i 0))
    (labels ((emit (x) (push x out)))
      (loop while (< i n)
            do (let ((lp (%loop-fr514-parse-canonical-loop-at vec i)))
                 (if (null lp)
                     (progn (emit (aref vec i)) (incf i))
                     (multiple-value-bind (core _step) (%loop-fr514-core-and-step lp)
                       (declare (ignore _step))
                       (let ((split (%loop-fr514-independent-split-index core))
                             (init (%loop-fr514-constant-init vec lp)))
                         (if (%loop-fr514-fission-candidate-p vec lp core split)
                             (let ((left (subseq core 0 split))
                                   (right (subseq core split)))
                               (setf out (%loop-fr514-emit-loop vec lp left "FISSION_A" nil out))
                               (setf out (%loop-fr514-emit-loop vec lp right "FISSION_B" init out))
                               (setf changed t
                                     i (1+ (opt-loop-exit-index lp))))
                             (progn
                               (dolist (inst (%loop-fr514-loop-seq vec lp)) (emit inst))
                               (setf i (1+ (opt-loop-exit-index lp))))))))))
      (if changed (nreverse out) instructions))))

(unless (fboundp 'opt-pass-loop-fusion)
  (defun opt-pass-loop-fusion (instructions)
    instructions))

(unless (fboundp 'opt-pass-loop-fission)
  (defun opt-pass-loop-fission (instructions)
    instructions))
