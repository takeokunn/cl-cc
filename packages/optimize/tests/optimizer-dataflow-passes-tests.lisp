;;;; optimizer-dataflow-passes-tests.lisp — global-DCE, pipeline, LICM, PRE
(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

(deftest global-dce-removes-unreachable-registered-function
  "opt-pass-global-dce removes a registered function when nothing at top level reaches it."
  (let* ((closure (make-vm-closure :dst :r0 :label "dead" :params '(:r1)
                                   :captured nil :optional-params nil :rest-param nil :key-params nil))
         (register (cl-cc:make-vm-register-function :name 'dead :src :r0))
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
         (f-register (cl-cc:make-vm-register-function :name 'f :src :r0))
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
         (known   (cl-cc/optimize:opt-known-callee-labels (list closure regfun const move))))
    (assert-string= "f" (gethash reg known))))

(deftest-each optimizer-pass-pipeline-forms
  "optimize-instructions folds (+ 1 2) to nothing for both keyword-list and string pipeline forms."
  :cases (("keyword-list" '(:fold :dce))
          ("string"       "fold,dce"))
  (pipeline)
  (let* ((instrs (list (make-vm-const :dst :r0 :value 1)
                       (make-vm-const :dst :r1 :value 2)
                       (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)))
         (out (cl-cc/optimize:optimize-instructions instrs :pass-pipeline pipeline)))
    (assert-equal 0 (length out))))

(deftest optimizer-ir-verify-valid
  "opt-verify-instructions passes on a simple valid linear program."
  (let ((instrs (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r2))))
    (assert-true (cl-cc/optimize:opt-verify-instructions instrs :pass-name "test"))))

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
   (handler-case (progn (cl-cc/optimize:opt-verify-instructions instrs :pass-name "test") nil)
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
    (apply #'cl-cc/optimize:optimize-instructions instrs
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
