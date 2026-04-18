(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

;;; ── Direct opt-pass-fold Tests ─────────────────────────────────────────

(deftest fold-label-flushes-env
  "Branch-target labels flush the constant env — a constant known before a label must NOT
    be propagated after the label (other paths may define a different value)."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value 42)
                       (cl-cc::make-vm-jump :label "join")
                       (cl-cc::make-vm-label :name "join")
                        (cl-cc::make-vm-inc :dst :R1 :src :R0)))
         (out (cl-cc/optimize::opt-pass-fold instrs)))
    ;; The inc should survive (not folded to const 43) because the label flushes env
    (assert-true (find-if (lambda (i) (typep i 'cl-cc/vm::vm-inc)) out))))

(deftest fold-fallthrough-label-preserves-env
  "A non-target fallthrough label does not flush constant knowledge."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value 42)
                       (cl-cc::make-vm-label :name "join")
                       (cl-cc::make-vm-inc :dst :R1 :src :R0)))
         (out (cl-cc/optimize::opt-pass-fold instrs))
         (r1-const (find-if (lambda (i) (and (cl-cc::vm-const-p i)
                                             (eq (cl-cc/vm::vm-dst i) :R1)))
                            out)))
    (assert-true r1-const)
    (assert-equal 43 (cl-cc/vm::vm-value r1-const))))

(deftest-each fold-type-pred
  "Type predicate and vm-not instructions fold at compile time against a known constant."
  :cases (("number-p" 42  (cl-cc::make-vm-number-p :dst :R1 :src :R0) 1)
          ("symbol-p" 42  (cl-cc::make-vm-symbol-p :dst :R1 :src :R0) 0)
          ("not-nil"  nil (cl-cc::make-vm-not       :dst :R1 :src :R0) t)
          ("not-zero" 0   (cl-cc::make-vm-not       :dst :R1 :src :R0) nil))
  (const-val pred-inst expected)
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value const-val) pred-inst))
         (out (cl-cc/optimize::opt-pass-fold instrs))
         (r1-const (find-if (lambda (i) (and (cl-cc::vm-const-p i)
                                             (eq (cl-cc/vm::vm-dst i) :R1)))
                            out)))
    (assert-true r1-const)
    (assert-equal expected (cl-cc/vm::vm-value r1-const))))

(deftest-each fold-branch-known
  "vm-jump-zero with a known constant is eliminated or replaced by an unconditional jump."
  :cases (("known-true-no-branch"
           1
           (lambda (out)
             (assert-false (find-if #'cl-cc::vm-jump-p out))))
          ("known-false-jump"
           nil
           (lambda (out)
             (assert-true (find-if #'cl-cc::vm-jump-p out)))))
  (const-val verify)
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value const-val)
                       (cl-cc::make-vm-jump-zero :reg :R0 :label "target")))
          (out (cl-cc/optimize::opt-pass-fold instrs)))
    (funcall verify out)
    (assert-false (find-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out))))

;;; ── Direct opt-pass-copy-prop Tests ────────────────────────────────────

(deftest-each copy-prop-operand-resolution
  "opt-pass-copy-prop resolves add operands through copy chains, fallthrough labels, and kills."
  :cases (("chain-follows-two-moves"
           (list (cl-cc::make-vm-move :dst :R1 :src :R0)
                 (cl-cc::make-vm-move :dst :R2 :src :R1)
                 (cl-cc::make-vm-add :dst :R3 :lhs :R2 :rhs :R2))
           :R0)
          ("fallthrough-label-preserves-copies"
           (list (cl-cc::make-vm-move :dst :R1 :src :R0)
                 (cl-cc::make-vm-label :name "join")
                 (cl-cc::make-vm-add :dst :R2 :lhs :R1 :rhs :R1))
           :R0)
          ("overwrite-kills-alias"
           (list (cl-cc::make-vm-move :dst :R1 :src :R0)
                 (cl-cc::make-vm-const :dst :R0 :value 99)
                 (cl-cc::make-vm-add :dst :R2 :lhs :R1 :rhs :R1))
           :R1))
  (instrs expected-lhs)
  (let* ((out (cl-cc/optimize::opt-pass-copy-prop instrs))
         (add-inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out)))
    (assert-true add-inst)
    (assert-equal expected-lhs (cl-cc/vm::vm-lhs add-inst))))

(deftest copy-prop-reverse-map-kill
  "Reverse-map kill invalidates direct aliases without a full copy-table scan." 
  (let* ((copies (make-hash-table :test #'eq))
         (reverse nil))
    (setf (gethash :R1 copies) :R0)
    (setf (gethash :R2 copies) :R0)
    (setf (gethash :R3 copies) :R2)
    (setf reverse (cl-cc/optimize::%opt-copy-prop-build-reverse copies))
    (cl-cc/optimize::%opt-copy-prop-kill :R0 copies reverse)
    (assert-false (gethash :R1 copies))
    (assert-false (gethash :R2 copies))
    (assert-true (eql :R2 (gethash :R3 copies)))))

(deftest copy-prop-self-move-elim
  "Self-move after resolution is eliminated: R1←R0 then R0←R1 → second resolves
   to R0←R0 which is dropped."
  (let* ((instrs (list (cl-cc::make-vm-move :dst :R1 :src :R0)
                       (cl-cc::make-vm-move :dst :R0 :src :R1)))
         (out (cl-cc/optimize::opt-pass-copy-prop instrs)))
    ;; Only one move should remain (the first one); the second is a self-move
    (let ((moves (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-move)) out)))
      (assert-equal 1 (length moves)))))

(deftest copy-prop-join-point
  "Copies that agree on every predecessor survive a CFG join and rewrite uses."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R9 :value nil)
                       (cl-cc::make-vm-jump-zero :reg :R9 :label "else")
                       (cl-cc::make-vm-label :name "then")
                       (cl-cc::make-vm-move :dst :R1 :src :R0)
                       (cl-cc::make-vm-jump :label "join")
                       (cl-cc::make-vm-label :name "else")
                       (cl-cc::make-vm-move :dst :R1 :src :R0)
                       (cl-cc::make-vm-label :name "join")
                       (cl-cc::make-vm-add :dst :R2 :lhs :R1 :rhs :R1)))
         (out (cl-cc/optimize::opt-pass-copy-prop instrs)))
    (let ((add-inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out)))
      (assert-true add-inst)
      (assert-equal :R0 (cl-cc/vm::vm-lhs add-inst))
      (assert-equal :R0 (cl-cc/vm::vm-rhs add-inst)))))

(deftest heap-alias-must-alias-propagates-through-move
  "must-alias holds after copying a fresh alloc via vm-move; distinct roots do not alias."
  (let* ((alloc (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (copy  (make-vm-move :dst :r3 :src :r0))
         (roots (cl-cc/optimize::opt-compute-heap-aliases (list alloc copy))))
    (assert-true  (cl-cc/optimize::opt-must-alias-p :r0 :r3 roots))
    (assert-false (cl-cc/optimize::opt-must-alias-p :r0 :r9 roots))))

(deftest heap-alias-may-alias-conservative-for-unknown-roots
  "Distinct fresh allocs are not may-alias; an unknown root (:r9) conservatively may-alias."
  (let* ((alloc-a (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (alloc-b (make-vm-make-array :dst :r4 :size-reg :r5))
         (roots   (cl-cc/optimize::opt-compute-heap-aliases (list alloc-a alloc-b))))
    (assert-false (cl-cc/optimize::opt-may-alias-p :r0 :r4 roots))
    (assert-true  (cl-cc/optimize::opt-may-alias-p :r0 :r9 roots))))

(deftest points-to-helper-tracks-moves-and-kills
  "Flow-sensitive points-to propagates through vm-move and is killed by overwrite."
  (let* ((alloc (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (copy  (make-vm-move :dst :r3 :src :r0))
         (kill  (make-vm-const :dst :r3 :value 9))
         (pt1   (cl-cc/optimize::opt-compute-points-to (list alloc copy)))
         (pt2   (cl-cc/optimize::opt-compute-points-to (list alloc copy kill))))
    (assert-eq :r0 (cl-cc/optimize::opt-points-to-root :r0 pt1))
    (assert-eq :r0 (cl-cc/optimize::opt-points-to-root :r3 pt1))
    (assert-false (cl-cc/optimize::opt-points-to-root :r3 pt2))))

(deftest heap-kind-helper-distinguishes-object-classes
  "TBAA helper can prove non-aliasing across different fresh heap object kinds."
  (let* ((alloc-cons  (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (alloc-array (make-vm-make-array :dst :r4 :size-reg :r5))
         (points-to   (cl-cc/optimize::opt-compute-points-to (list alloc-cons alloc-array)))
         (heap-kinds  (cl-cc/optimize::opt-compute-heap-kinds (list alloc-cons alloc-array))))
    (assert-eq :cons (gethash :r0 heap-kinds))
    (assert-eq :array (gethash :r4 heap-kinds))
    (assert-false (cl-cc/optimize::opt-may-alias-by-type-p :r0 :r4 points-to heap-kinds))
    (assert-true  (cl-cc/optimize::opt-may-alias-by-type-p :r0 :r9 points-to heap-kinds))))

(deftest constant-interval-helper-propagates-basic-arithmetic
  "Constant interval propagation handles add/sub/mul in straight-line code."
  (let* ((c1 (make-vm-const :dst :r0 :value 3))
         (c2 (make-vm-const :dst :r1 :value 5))
         (a  (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (s  (make-vm-sub :dst :r3 :lhs :r2 :rhs :r0))
         (m  (make-vm-mul :dst :r4 :lhs :r3 :rhs :r1))
         (intervals (cl-cc/optimize::opt-compute-constant-intervals (list c1 c2 a s m))))
    (assert-equal '(8 . 8) (gethash :r2 intervals))
    (assert-equal '(5 . 5) (gethash :r3 intervals))
    (assert-equal '(25 . 25) (gethash :r4 intervals))))

;;; ─── opt-inst-read-regs ──────────────────────────────────────────────────────

(deftest-each opt-inst-read-regs-cases
  "opt-inst-read-regs returns the correct source register list for each instruction type."
  :cases (("const"      (make-vm-const      :dst :r0 :value 42)             '())
          ("func-ref"   (make-vm-func-ref   :dst :r0 :label "fn")           '())
          ("get-global" (make-vm-get-global :dst :r0 :name 'x)              '())
          ("move"       (make-vm-move       :dst :r0 :src :r1)              '(:r1))
          ("neg"        (make-vm-neg        :dst :r0 :src :r1)              '(:r1))
          ("null-p"     (make-vm-null-p     :dst :r0 :src :r1)              '(:r1))
          ("ret"        (make-vm-ret        :reg :r0)                        '(:r0))
          ("set-global" (make-vm-set-global :src :r0 :name 'x)              '(:r0))
          ("add"        (make-vm-add        :dst :r0 :lhs :r1 :rhs :r2)    '(:r1 :r2))
          ("lt"         (make-vm-lt         :dst :r0 :lhs :r1 :rhs :r2)    '(:r1 :r2))
          ("call"       (make-vm-call       :dst :r0 :func :r1 :args '(:r2 :r3)) '(:r1 :r2 :r3)))
  (inst expected-members)
  (let ((regs (cl-cc/optimize::opt-inst-read-regs inst)))
    (assert-equal (length expected-members) (length regs))
    (dolist (r expected-members)
      (assert-true (member r regs)))))

;;; ── opt-pass-dce: Dead Code Elimination ──────────────────────────────────

(deftest dce-removes-unused-const
  "DCE removes vm-const whose dst is never read; keeps the one used by ret."
  (let* ((i1  (make-vm-const :dst :r0 :value 42))
         (i2  (make-vm-const :dst :r1 :value 7))
         (ret (make-vm-ret :reg :r1))
         (out (cl-cc/optimize::opt-pass-dce (list i1 i2 ret))))
    (assert-false (member i1 out))
    (assert-true  (member i2 out))
    (assert-true  (member ret out))))

(deftest dce-keeps-const-read-by-add
  "DCE keeps vm-const whose dst is consumed by vm-add."
  (let* ((i1  (make-vm-const :dst :r0 :value 5))
         (i2  (make-vm-add  :dst :r1 :lhs :r0 :rhs :r0))
         (ret (make-vm-ret :reg :r1))
         (out (cl-cc/optimize::opt-pass-dce (list i1 i2 ret))))
    (assert-true (member i1 out))
    (assert-true (member i2 out))))

(deftest dce-removes-unused-move
  "DCE removes vm-move whose dst is never read; keeps the src-producing const."
  (let* ((c   (make-vm-const :dst :r0 :value 1))
         (m   (make-vm-move  :dst :r2 :src :r0))
         (ret (make-vm-ret :reg :r0))
         (out (cl-cc/optimize::opt-pass-dce (list c m ret))))
    (assert-false (member m out))
    (assert-true  (member c out))))

(deftest dce-preserves-impure-unused-dst
  "DCE does NOT remove a vm-call (impure) even if dst is unused."
  (let* ((fn  (make-vm-const     :dst :r0 :value 'f))
         (c   (make-vm-call      :dst :r1 :func :r0 :args nil))
         (ret (make-vm-ret :reg :r0))
         ;; r1 is never read — but vm-call is impure
         (out (cl-cc/optimize::opt-pass-dce (list fn c ret))))
    (assert-true (member c out))))

;;; ── opt-pass-jump: Jump Threading ────────────────────────────────────────

(deftest jump-threading-eliminates-jump-to-next-label
  "opt-pass-jump removes a jump that targets the immediately following label."
  (let* ((j   (make-vm-jump  :label "end"))
         (lbl (make-vm-label :name "end"))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc/optimize::opt-pass-jump (list j lbl ret))))
    ;; The jump is a no-op (falls through) → should be removed
    (assert-false (member j out))
    (assert-true  (member lbl out))
    (assert-true  (member ret out))))

(deftest jump-threading-chains-jumps
  "opt-pass-jump threads jump-to-jump chains to the ultimate target."
  ;; j1 → lbl1 → j2 → lbl2 (final dest)
  (let* ((j1   (make-vm-jump  :label "mid"))
         (lbl1 (make-vm-label :name "mid"))
         (j2   (make-vm-jump  :label "end"))
         (lbl2 (make-vm-label :name "end"))
         (ret  (make-vm-ret   :reg :r0))
         (out  (cl-cc/optimize::opt-pass-jump (list j1 lbl1 j2 lbl2 ret))))
    ;; j1 should be threaded to "end" directly
    (let ((j1-out (find-if #'cl-cc::vm-jump-p out)))
      (when j1-out
        (assert-equal (cl-cc/vm::vm-label-name j1-out) "end")))))

(deftest jump-threading-follows-long-acyclic-chain
  "opt-pass-jump fully threads long jump chains that exceed the old recursion cap."
  (let ((instructions nil))
    (loop for i from 0 below 25 do
      (push (make-vm-jump :label (format nil "L~D" (1+ i))) instructions)
      (push (make-vm-label :name (format nil "L~D" (1+ i))) instructions))
    (push (make-vm-ret :reg :r0) instructions)
    (let* ((out (cl-cc/optimize::opt-pass-jump (nreverse instructions)))
           (j0  (find-if #'cl-cc::vm-jump-p out)))
      (assert-true j0)
      (assert-equal (cl-cc/vm::vm-label-name j0) "L25"))))

(deftest jump-zero-threading-updates-label
  "opt-pass-jump threads vm-jump-zero target through a chain."
  (let* ((c   (make-vm-const     :dst :r0 :value 0))
         (jz  (make-vm-jump-zero :reg :r0 :label "mid"))
         (lbl (make-vm-label     :name "mid"))
         (j2  (make-vm-jump      :label "end"))
         (end (make-vm-label     :name "end"))
         (ret (make-vm-ret       :reg :r0))
         (out (cl-cc/optimize::opt-pass-jump (list c jz lbl j2 end ret))))
    ;; The jump-zero should now target "end" directly
    (let ((jz-out (find-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out)))
      (when jz-out
        (assert-equal (cl-cc/vm::vm-label-name jz-out) "end")))))

;;; CFG reachability / label cleanup tests moved to optimizer-tests.lisp.

(deftest global-dce-removes-unreachable-registered-function
  "opt-pass-global-dce removes a registered function when nothing at top level reaches it."
  (let* ((closure (make-vm-closure :dst :r0 :label "dead" :params '(:r1)
                                   :captured nil :optional-params nil :rest-param nil :key-params nil))
         (register (cl-cc::make-vm-register-function :name 'dead :src :r0))
         (label (make-vm-label :name "dead"))
         (body (make-vm-const :dst :r2 :value 7))
         (ret (make-vm-ret :reg :r2))
         (out (cl-cc/optimize::opt-pass-global-dce (list closure register label body ret))))
    (assert-false (member closure out))
    (assert-false (member register out))
    (assert-false (member label out))
    (assert-false (member body out))
    (assert-false (member ret out))))

(deftest global-dce-preserves-top-level-called-chain
  "opt-pass-global-dce keeps functions reachable from top-level known calls."
  (let* ((f-closure (make-vm-closure :dst :r0 :label "f" :params '(:r1)
                                     :captured nil :optional-params nil :rest-param nil :key-params nil))
         (f-register (cl-cc::make-vm-register-function :name 'f :src :r0))
         (f-label (make-vm-label :name "f"))
         (f-ref (make-vm-func-ref :dst :r2 :label "g"))
         (f-call (make-vm-call :dst :r3 :func :r2 :args '(:r1)))
         (f-ret (make-vm-ret :reg :r3))
         (g-closure (make-vm-closure :dst :r4 :label "g" :params '(:r5)
                                     :captured nil :optional-params nil :rest-param nil :key-params nil))
         (g-label (make-vm-label :name "g"))
         (g-body (make-vm-const :dst :r6 :value 9))
         (g-ret (make-vm-ret :reg :r6))
         (top-ref (make-vm-func-ref :dst :r7 :label "f"))
         (top-arg (make-vm-const :dst :r8 :value 1))
         (top-call (make-vm-call :dst :r9 :func :r7 :args '(:r8)))
         (top-ret (make-vm-ret :reg :r9))
         (out (cl-cc/optimize::opt-pass-global-dce
               (list f-closure f-register f-label f-ref f-call f-ret
                     g-closure g-label g-body g-ret
                     top-ref top-arg top-call top-ret))))
    (assert-true (member f-closure out))
    (assert-true (member f-label out))
    (assert-true (member g-closure out))
    (assert-true (member g-label out))
    (assert-true (member top-call out))))

(deftest-each known-callee-labels-track-const-func-ref-and-move
  "Known-callee helper resolves labels through closure/const/move chains."
  :cases (("direct-closure" :r0)
          ("const-value"    :r2)
          ("moved-copy"     :r3))
  (reg)
  (let* ((closure (make-vm-closure :dst :r0 :label "f" :params '(:r1)
                                   :captured nil :optional-params nil :rest-param nil :key-params nil))
         (regfun  (make-vm-register-function :name 'f :src :r0))
         (const   (make-vm-const :dst :r2 :value 'f))
         (move    (make-vm-move :dst :r3 :src :r2))
         (known   (cl-cc/optimize::opt-known-callee-labels (list closure regfun const move))))
    (assert-string= "f" (cl-cc/optimize::opt-known-callee-label reg known))))

(deftest-each optimizer-pass-pipeline-forms
  "optimize-instructions folds (+ 1 2) to nothing for both keyword-list and string pipeline forms."
  :cases (("keyword-list" '(:fold :dce))
          ("string"       "fold,dce"))
  (pipeline)
  (let* ((instrs (list (make-vm-const :dst :r0 :value 1)
                       (make-vm-const :dst :r1 :value 2)
                       (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)))
         (out (cl-cc/optimize::optimize-instructions instrs :pass-pipeline pipeline)))
    (assert-equal 0 (length out))))

(deftest optimizer-ir-verify-valid
  "opt-verify-instructions passes on a simple valid linear program."
  (let ((instrs (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r2))))
    (assert-true (cl-cc/optimize::opt-verify-instructions instrs :pass-name "test"))))

(deftest-each optimizer-ir-verify-rejects-invalid
  "opt-verify-instructions raises an error for invalid IR programs."
  :cases (("duplicate-label"
           (list (make-vm-label :name "L0")
                 (make-vm-label :name "L0")))
          ("missing-label-target"
           (list (make-vm-jump :label "MISSING")))
          ("use-before-def"
           (list (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))))
  (instrs)
  (assert-true
   (handler-case (progn (cl-cc/optimize::opt-verify-instructions instrs :pass-name "test") nil)
     (error () t))))

(deftest-each optimizer-pass-pipeline-output-modes
  "optimize-instructions diagnostic output modes: timings, stats, JSON trace, and remarks."
  :cases (("timings"
           (list (make-vm-const :dst :r0 :value 1))
           (list :print-pass-timings t :timing-stream nil)
           '("OPT-PASS-FOLD"))
          ("stats"
           (list (make-vm-const :dst :r0 :value 1))
           (list :print-pass-stats t :stats-stream nil)
           '("OPT-PASS-FOLD" "BEFORE=" "AFTER="))
          ("json-trace"
           (list (make-vm-const :dst :r0 :value 1))
           (list :trace-json-stream nil)
           '("\"traceEvents\"" "OPT-PASS-FOLD" "\"dur\""))
          ("remarks-changed"
           (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-const :dst :r1 :value 2)
                 (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1))
           (list :print-opt-remarks t :opt-remarks-mode :changed :opt-remarks-stream nil)
           '("OPT-PASS-FOLD" "CHANGED"))
          ("remarks-missed"
           (list (make-vm-const :dst :r0 :value 1))
           (list :print-opt-remarks t :opt-remarks-mode :missed :opt-remarks-stream nil)
           '("OPT-PASS-FOLD" "MISSED")))
  (instrs extra-opts expected-strings)
  (let* ((stream (make-string-output-stream))
         (patched-opts (loop for (k v) on extra-opts by #'cddr
                             nconc (if (null v) (list k stream) (list k v)))))
    (apply #'cl-cc/optimize::optimize-instructions instrs
           :pass-pipeline '(:fold)
           patched-opts)
    (let ((text (string-upcase (get-output-stream-string stream))))
      (dolist (s expected-strings)
        (assert-true (search (string-upcase s) text))))))

(deftest licm-does-not-hoist-loop-defined-value
  "opt-pass-licm keeps a pure instruction inside the loop when it reads a loop-defined register."
  (let* ((start (make-vm-label :name "start"))
         (jmp1  (make-vm-jump :label "loop"))
         (loop  (make-vm-label :name "loop"))
         (c1    (make-vm-const :dst :r1 :value 1))
         (a1    (make-vm-add :dst :r2 :lhs :r1 :rhs :r1))
         (back  (make-vm-jump :label "loop"))
         (ret   (make-vm-ret :reg :r2))
         (out   (cl-cc/optimize::opt-pass-licm (list start jmp1 loop c1 a1 back ret))))
    (assert-true (member a1 out))
    (assert-true (> (position a1 out :test #'eq)
                    (position loop out :test #'eq)))))

(deftest pre-hoists-partially-redundant-expression
  "opt-pass-pre removes a partially redundant join-point expression."
  (let* ((entry (make-vm-label :name "entry"))
         (c0    (make-vm-const :dst :r0 :value 1))
         (c2    (make-vm-const :dst :r2 :value 2))
         (br    (make-vm-jump-zero :reg :r0 :label "p2"))
         (p1    (make-vm-label :name "p1"))
         (a1    (make-vm-add :dst :r3 :lhs :r0 :rhs :r2))
         (j1    (make-vm-jump :label "join"))
         (p2    (make-vm-label :name "p2"))
         (x     (make-vm-const :dst :r4 :value 7))
         (j2    (make-vm-jump :label "join"))
         (join  (make-vm-label :name "join"))
         (a2    (make-vm-add :dst :r5 :lhs :r0 :rhs :r2))
         (ret   (make-vm-ret :reg :r5))
         (out   (cl-cc/optimize::opt-pass-pre (list entry c0 c2 br p1 a1 j1 p2 x j2 join a2 ret))))
    (assert-equal 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out))
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-move)
                               (or (and (eq :r3 (cl-cc/vm::vm-dst i))
                                        (eq :r5 (cl-cc/vm::vm-src i)))
                                   (and (eq :r5 (cl-cc/vm::vm-dst i))
                                        (eq :r3 (cl-cc/vm::vm-src i))))))
                       out))))

(deftest egraph-pass-lowers-constant-subtraction
  "opt-pass-egraph rewrites a simple constant subtraction to vm-const."
  (let* ((i1 (cl-cc:make-vm-const :dst :r0 :value 7))
         (i2 (cl-cc:make-vm-sub   :dst :r1 :lhs :r0 :rhs :r0))
         (out (cl-cc/optimize::opt-pass-egraph (list i1 i2)))
         (r1  (find-if (lambda (inst)
                         (eq :r1 (ignore-errors (cl-cc/vm::vm-dst inst))))
                       out)))
    (assert-true (cl-cc::vm-const-p r1))
    (assert-equal 0 (cl-cc/vm::vm-value r1))))

(deftest bswap-recognition-collapses-byte-swap-tree
  "opt-pass-bswap-recognition collapses the canonical byte-swap tree to vm-bswap."
  (let* ((input (make-bswap-tree-instructions))
         (out   (cl-cc/optimize::opt-pass-bswap-recognition input)))
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-bswap)) out))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-logior)) out))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out))))

(deftest rotate-recognition-collapses-shift-or-tree
  "opt-pass-rotate-recognition collapses a two-shift + OR idiom to vm-rotate."
  (let* ((c0  (make-vm-const :dst :r1 :value 8))
         (a0  (make-vm-ash   :dst :r2 :lhs :r0 :rhs :r1))
         (c1  (make-vm-const :dst :r3 :value -56))
         (a1  (make-vm-ash   :dst :r4 :lhs :r0 :rhs :r3))
         (o0  (make-vm-logior :dst :r5 :lhs :r2 :rhs :r4))
         (out (cl-cc/optimize::opt-pass-rotate-recognition (list c0 a0 c1 a1 o0))))
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-rotate)) out))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-logior)) out))))

(deftest-each dead-store-elim-cases
  "opt-pass-dead-store-elim: overwritten stores removed, live-read stores kept, alias-tracked slot writes handled."
  :cases (("overwritten-global"
           (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 1))
                    (s1  (make-vm-set-global :name "g" :src :r0))
                    (c2  (make-vm-const :dst :r1 :value 2))
                    (s2  (make-vm-set-global :name "g" :src :r1))
                    (ret (make-vm-ret :reg :r1)))
               (list (list c1 s1 c2 s2 ret) (list s1) (list s2 c1 c2 ret)))))
          ("keeps-live-read"
           (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 1))
                    (s1  (make-vm-set-global :name "g" :src :r0))
                    (g1  (make-vm-get-global :dst :r2 :name "g"))
                    (c2  (make-vm-const :dst :r1 :value 2))
                    (s2  (make-vm-set-global :name "g" :src :r1))
                    (ret (make-vm-ret :reg :r2)))
               (list (list c1 s1 g1 c2 s2 ret) nil (list s1 g1 s2 ret)))))
          ("slot-overwrite-alias"
           (lambda ()
             (let* ((obj  (make-vm-cons :dst :r0 :car-src :r8 :cdr-src :r9))
                    (obj2 (make-vm-move :dst :r3 :src :r0))
                    (c1   (make-vm-const :dst :r1 :value 10))
                    (w1   (cl-cc::make-vm-slot-write :obj-reg :r0 :slot-name 'x :value-reg :r1))
                    (c2   (make-vm-const :dst :r2 :value 20))
                    (w2   (cl-cc::make-vm-slot-write :obj-reg :r3 :slot-name 'x :value-reg :r2))
                    (ret  (make-vm-ret :reg :r2)))
                (list (list obj obj2 c1 w1 c2 w2 ret)
                      (list w1)
                      (list obj obj2 c1 c2 w2 ret))))))
  (make-case)
  (destructuring-bind (instrs absent present) (funcall make-case)
    (let ((out (cl-cc/optimize::opt-pass-dead-store-elim instrs)))
      (dolist (inst absent)   (assert-false (member inst out)))
      (dolist (inst present)  (assert-true  (member inst out))))))

(deftest sccp-eliminates-constant-branch-block
  "opt-pass-sccp removes the unreachable branch block after a constant condition."
  (let* ((start (make-vm-label :name "start"))
         (cond  (make-vm-const :dst :r0 :value 1))
         (br    (make-vm-jump-zero :reg :r0 :label "else"))
         (thenl (make-vm-label :name "then"))
         (thenv (make-vm-const :dst :r1 :value 10))
         (jmp   (make-vm-jump :label "end"))
         (elsel (make-vm-label :name "else"))
         (elsev (make-vm-const :dst :r1 :value 20))
         (endl  (make-vm-label :name "end"))
         (ret   (make-vm-ret :reg :r1))
         (out   (cl-cc/optimize::opt-pass-sccp (list start cond br thenl thenv jmp elsel elsev endl ret))))
    (assert-false (member elsel out))
    (assert-false (member elsev out))
    (assert-true  (some (lambda (i) (and (typep i 'cl-cc/vm::vm-const)
                                         (eq (cl-cc/vm::vm-dst i) :r1)
                                         (eql (cl-cc/vm::vm-value i) 10)))
                        out))))

(deftest-each store-to-load-forward-cases
  "opt-pass-store-to-load-forward replaces load instructions with vm-move (global and slot variants)."
  :cases (("global-direct"
           (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 1))
                    (s1  (make-vm-set-global :name "g" :src :r0))
                    (g1  (make-vm-get-global :dst :r1 :name "g"))
                    (ret (make-vm-ret :reg :r1)))
               (list (list c1 s1 g1 ret) g1 :r1 :r0)))
           )
          ("global-same-dst"
           (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 1))
                    (s1  (make-vm-set-global :name "g" :src :r0))
                    (g1  (make-vm-get-global :dst :r0 :name "g"))
                    (ret (make-vm-ret :reg :r0)))
               (list (list c1 s1 g1 ret) g1 :r0 :r0))))
          ("slot-direct"
           (lambda ()
             (let* ((obj (make-vm-const :dst :r0 :value 42))
                    (val (make-vm-const :dst :r1 :value 99))
                    (w   (cl-cc::make-vm-slot-write :obj-reg :r0 :slot-name 'x :value-reg :r1))
                    (r   (cl-cc::make-vm-slot-read :dst :r2 :obj-reg :r0 :slot-name 'x))
                    (ret (make-vm-ret :reg :r2)))
               (list (list obj val w r ret) r :r2 :r1))))
          ("slot-same-dst"
           (lambda ()
             (let* ((obj (make-vm-const :dst :r0 :value 42))
                    (val (make-vm-const :dst :r1 :value 99))
                    (w   (cl-cc::make-vm-slot-write :obj-reg :r0 :slot-name 'x :value-reg :r1))
                    (r   (cl-cc::make-vm-slot-read :dst :r1 :obj-reg :r0 :slot-name 'x))
                    (ret (make-vm-ret :reg :r1)))
               (list (list obj val w r ret) r :r1 :r1))))
          ("slot-through-moved-alias"
           (lambda ()
             (let* ((obj  (make-vm-cons :dst :r0 :car-src :r8 :cdr-src :r9))
                    (obj2 (make-vm-move :dst :r3 :src :r0))
                    (val  (make-vm-const :dst :r1 :value 99))
                    (w    (cl-cc::make-vm-slot-write :obj-reg :r0 :slot-name 'x :value-reg :r1))
                    (r    (cl-cc::make-vm-slot-read :dst :r2 :obj-reg :r3 :slot-name 'x))
                    (ret  (make-vm-ret :reg :r2)))
               (list (list obj obj2 val w r ret) r :r2 :r1)))))
  (make-case)
  (destructuring-bind (instrs read-instr expected-dst expected-src) (funcall make-case)
    (let ((out (cl-cc/optimize::opt-pass-store-to-load-forward instrs)))
      (assert-false (member read-instr out :test #'eq))
      (assert-true (some (lambda (i)
                           (and (typep i 'cl-cc/vm::vm-move)
                                (eq (cl-cc::vm-move-dst i) expected-dst)
                                (eq (cl-cc::vm-move-src i) expected-src)))
                         out)))))

;;; ── opt-pass-strength-reduce: Strength Reduction ─────────────────────────

(deftest-each strength-reduce-cases
  "opt-pass-strength-reduce: power-of-2 mul/div become vm-ash; mod-by-power-of-2 becomes vm-logand."
  :cases (("mul-rhs-pow2"
           8 (make-vm-mul :dst :r2 :lhs :r0 :rhs :r1)
           (lambda (out op)
             (assert-false (member op out))
             (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out))))
          ("mul-lhs-pow2"
           4 (make-vm-mul :dst :r2 :lhs :r1 :rhs :r0)
           (lambda (out op)
             (assert-false (member op out))
             (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out))))
          ("mul-non-power-of-2"
           7 (make-vm-mul :dst :r2 :lhs :r0 :rhs :r1)
           (lambda (out op)
             (assert-true (member op out))
             (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out))))
          ("div-rhs-pow2"
           8 (cl-cc::make-vm-div :dst :r2 :lhs :r0 :rhs :r1)
           (lambda (out op)
             (assert-false (member op out))
             (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out))))
          ("div-non-power-of-2"
           7 (cl-cc::make-vm-div :dst :r2 :lhs :r0 :rhs :r1)
           (lambda (out op)
             (assert-true (member op out))
             (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out))))
          ("mod-pow2"
           8 (cl-cc::make-vm-mod :dst :r2 :lhs :r0 :rhs :r1)
           (lambda (out op)
             (assert-false (member op out))
             (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-logand)) out))))
          ("mod-non-power-of-2"
           7 (cl-cc::make-vm-mod :dst :r2 :lhs :r0 :rhs :r1)
           (lambda (out op)
             (assert-true (member op out))
             (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-logand)) out)))))
  (const-val op verify)
  (let* ((c   (make-vm-const :dst :r1 :value const-val))
         (ret (make-vm-ret   :reg :r2))
         (out (cl-cc/optimize::opt-pass-strength-reduce (list c op ret))))
    (funcall verify out op)))

;;; ── opt-pass-reassociate: Arithmetic Reassociation ──────────────────────

(deftest-each reassociate-moves-constant-inward
  "opt-pass-reassociate moves constants toward the tail of nested op chains (add and logand)."
  :cases (("add"    1   (make-vm-add    :dst :r4 :lhs :r2 :rhs :r1) (make-vm-add    :dst :r5 :lhs :r4 :rhs :r3) 'cl-cc/vm::vm-add)
          ("logand" 255 (make-vm-logand :dst :r4 :lhs :r2 :rhs :r1) (make-vm-logand :dst :r5 :lhs :r4 :rhs :r3) 'cl-cc/vm::vm-logand))
  (const-val op1 op2 op-type)
  (let* ((c    (make-vm-const :dst :r1 :value const-val))
         (a    (make-vm-move  :dst :r2 :src :r8))
         (b    (make-vm-move  :dst :r3 :src :r9))
         (ret  (make-vm-ret   :reg :r5))
         (out  (cl-cc/optimize::opt-pass-reassociate (list c a b op1 op2 ret)))
         (ops  (remove-if-not (lambda (i) (typep i op-type)) out)))
    (assert-equal 2 (length ops))
    (assert-eq :r3 (cl-cc/vm::vm-lhs (first ops)))
    (assert-eq :r1 (cl-cc/vm::vm-rhs (first ops)))
    (assert-eq :r2 (cl-cc/vm::vm-lhs (second ops)))
    (assert-eq :r4 (cl-cc/vm::vm-rhs (second ops)))))

(deftest strength-reduce-mul-by-const-decomposes
  "opt-pass-strength-reduce: small non-power-of-2 constant multipliers are decomposed into shifts/adds."
  (let* ((c   (make-vm-const :dst :r1 :value 3))
         (mul (make-vm-mul :dst :r2 :lhs :r0 :rhs :r1))
         (ret (make-vm-ret :reg :r2))
         (out (cl-cc/optimize::opt-pass-strength-reduce (list c mul ret))))
    (assert-false (member mul out))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-add)) out))))

;;; ── %opt-mul-by-const-seq: Extracted Shift/Add Decomposition ────────────

(defun make-test-new-reg ()
  "Return a thunk that allocates fresh keyword registers :R1000, :R1001, ..."
  (let ((n 1000))
    (lambda () (prog1 (intern (format nil "R~A" n) :keyword) (incf n)))))

(deftest-each mul-by-const-seq-cases
  "%opt-mul-by-const-seq: correctly decomposes constant multipliers into shift/add sequences."
  :cases (("zero"     0  'cl-cc/vm::vm-const nil)
          ("one"      1  'cl-cc/vm::vm-move  nil)
          ("neg-one" -1  'cl-cc/vm::vm-move  'cl-cc/vm::vm-neg)
          ("two"      2  'cl-cc/vm::vm-ash   nil)
          ("three"    3  'cl-cc/vm::vm-add   nil)
          ("four"     4  'cl-cc/vm::vm-ash   nil)
          ("six"      6  'cl-cc/vm::vm-add   nil))
  (n expected-type-1 expected-type-2)
  (let* ((seq (cl-cc/optimize::%opt-mul-by-const-seq :r0 :r1 n (make-test-new-reg))))
    (assert-true (some (lambda (i) (typep i expected-type-1)) seq))
    (when expected-type-2
      (assert-true (some (lambda (i) (typep i expected-type-2)) seq)))))

(deftest mul-by-const-seq-correctness
  "%opt-mul-by-const-seq: emitted instructions have correct destination register."
  ;; n=5 = 2^0 + 2^2: should result in a vm-add with dst :r0
  (let* ((seq (cl-cc/optimize::%opt-mul-by-const-seq :r0 :r1 5 (make-test-new-reg)))
         (add (find-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) seq)))
    (assert-true add)
    (assert-equal :r0 (cl-cc/vm::vm-dst add))))

;;; ── opt-inline-eligible-p: Extracted Eligibility Predicate ──────────────

(deftest-each opt-inline-eligible-p-cases
  "opt-inline-eligible-p: eligible when cheap+no-captures; rejects captured vars or over-budget."
  :cases (("short-no-captures"
           nil
           (list (make-vm-add :dst :r2 :lhs :r1 :rhs :r1) (make-vm-ret :reg :r2))
           t)
          ("captured-vars-ineligible"
           (list (cons :x :r5))
           (list (make-vm-ret :reg :r1))
           nil)
          ("cheap-consts-eligible-over-threshold"
           nil
           (append (loop repeat 17 collect (make-vm-const :dst :r2 :value 0))
                   (list (make-vm-ret :reg :r1)))
           t)
          ("arith-instrs-over-budget-ineligible"
           nil
           (append (loop repeat 16 collect (make-vm-add :dst :r2 :lhs :r1 :rhs :r1))
                   (list (make-vm-ret :reg :r2)))
           nil))
  (captured body expected)
  (let* ((ci  (make-vm-closure :dst :r0 :label "f"
                                :params '(:r1) :captured captured
                                :optional-params nil :rest-param nil :key-params nil))
         (def (list :closure ci :params '(:r1) :body body)))
    (assert-equal expected (not (null (cl-cc/optimize::opt-inline-eligible-p def 15))))))

(deftest opt-adaptive-inline-threshold-cases
  "Cheap bodies get a higher threshold; call-heavy bodies get a tighter one."
  (let* ((cheap-ci (make-vm-closure :dst :r0 :label "cheap"
                                    :params '(:r1) :captured nil
                                    :optional-params nil :rest-param nil :key-params nil))
         (cheap-body (append (loop repeat 20 collect (make-vm-const :dst :r2 :value 0))
                             (list (make-vm-ret :reg :r1))))
         (cheap-def (list :closure cheap-ci :params '(:r1) :body cheap-body))
         (call-ci (make-vm-closure :dst :r0 :label "callish"
                                   :params '(:r1) :captured nil
                                   :optional-params nil :rest-param nil :key-params nil))
         (call-body (list (make-vm-call :dst :r2 :func :r3 :args '(:r1))
                          (make-vm-ret :reg :r2)))
         (call-def (list :closure call-ci :params '(:r1) :body call-body)))
    (assert-true (> (cl-cc/optimize::opt-adaptive-inline-threshold cheap-def) 15))
    (assert-true (< (cl-cc/optimize::opt-adaptive-inline-threshold call-def) 15))))

(deftest opt-pass-inline-iterative-uses-adaptive-threshold
  "The iterative inline wrapper runs with adaptive thresholds without error."
  (let* ((ci   (make-vm-closure :dst :r0 :label "f"
                                :params '(:r1) :captured nil
                                :optional-params nil :rest-param nil :key-params nil))
         (fref (make-vm-func-ref :dst :r3 :label "f"))
         (body (append (loop repeat 18 collect (make-vm-const :dst :r2 :value 0))
                       (list (make-vm-ret :reg :r1))))
         (call (make-vm-call :dst :r4 :func :r3 :args '(:r5)))
         (ret  (make-vm-ret :reg :r4))
         (out  (cl-cc/optimize::opt-pass-inline-iterative
                (append (list ci) body (list fref call ret)))))
    (assert-true out)))

(deftest-each opt-adaptive-max-iterations-cases
  "Adaptive max-iterations shrinks for small programs and grows for larger ones."
  :cases (("small" 10  #'<  20)
          ("large" 900 #'>  20))
  (n-insts pred threshold)
  (let ((iters (cl-cc/optimize::opt-adaptive-max-iterations
                (loop repeat n-insts collect (make-vm-const :dst :r0 :value 0)))))
    (assert-true (funcall pred iters threshold))))

(deftest optimize-instructions-accepts-adaptive-max-iterations
  "optimize-instructions accepts :adaptive as the iteration budget selector."
  (let ((out (cl-cc/optimize::optimize-instructions
              (list (make-vm-const :dst :r0 :value 1)
                    (make-vm-ret :reg :r0))
              :max-iterations :adaptive
              :pass-pipeline '(:fold :dce))))
    (assert-true out)))

;;; ─── opt-falsep ──────────────────────────────────────────────────────────

(deftest-each opt-falsep
  "opt-falsep correctly classifies falsy (nil only) and truthy values."
  :cases (("nil"      nil t)
          ("zero"     0   t)
          ("t"        t   nil)
          ("positive" 1   nil))
  (value expected)
  (assert-equal expected (cl-cc/optimize::opt-falsep value)))

;;; ─── opt-register-keyword-p ──────────────────────────────────────────────

(deftest-each opt-register-keyword-p
  "opt-register-keyword-p recognizes :RN keywords; rejects non-register symbols and plain keywords."
  :cases (("r0"            :r0  t)
          ("r15"           :r15 t)
          ("plain-symbol"  'r0  nil)
          ("plain-keyword" :foo nil))
  (value expected)
  (assert-equal expected (cl-cc/optimize::opt-register-keyword-p value)))

;;; ─── opt-binary-lhs-rhs-p / opt-unary-src-p ─────────────────────────────

(deftest-each opt-instruction-shape-predicates
  "opt-binary-lhs-rhs-p and opt-unary-src-p classify instruction shapes correctly."
  :cases (("binary-add"    #'cl-cc/optimize::opt-binary-lhs-rhs-p (make-vm-add    :dst :r0 :lhs :r1 :rhs :r2) t)
          ("binary-lt"     #'cl-cc/optimize::opt-binary-lhs-rhs-p (make-vm-lt     :dst :r0 :lhs :r1 :rhs :r2) t)
          ("binary-neg"    #'cl-cc/optimize::opt-binary-lhs-rhs-p (make-vm-neg    :dst :r0 :src :r1)           nil)
          ("unary-neg"     #'cl-cc/optimize::opt-unary-src-p      (make-vm-neg    :dst :r0 :src :r1)           t)
          ("unary-null-p"  #'cl-cc/optimize::opt-unary-src-p      (make-vm-null-p :dst :r0 :src :r1)           t)
          ("unary-add"     #'cl-cc/optimize::opt-unary-src-p      (make-vm-add    :dst :r0 :lhs :r1 :rhs :r2)  nil))
  (pred-fn inst expected)
  (assert-equal expected (not (null (funcall pred-fn inst)))))

;;; ─── opt-foldable-unary-arith-p / opt-foldable-type-pred-p ──────────────

(deftest-each opt-foldable-predicates
  "opt-foldable-unary-arith-p and opt-foldable-type-pred-p classify fold eligibility."
  :cases (("arith-neg"    #'cl-cc/optimize::opt-foldable-unary-arith-p (make-vm-neg    :dst :r0 :src :r1) t)
          ("arith-null-p" #'cl-cc/optimize::opt-foldable-unary-arith-p (make-vm-null-p :dst :r0 :src :r1) nil)
          ("pred-null-p"  #'cl-cc/optimize::opt-foldable-type-pred-p   (make-vm-null-p :dst :r0 :src :r1) t)
          ("pred-neg"     #'cl-cc/optimize::opt-foldable-type-pred-p   (make-vm-neg    :dst :r0 :src :r1) nil))
  (pred-fn inst expected)
  (assert-equal expected (funcall pred-fn inst)))
