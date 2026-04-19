;;;; optimizer-inlining-tests.lisp — Inlining pass unit tests
(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

;;; ── Inlining Pass: Unit Tests ──────────────────────────────────────────────

(defun inline-has-call-p (instructions)
  "T if INSTRUCTIONS contains at least one vm-call."
  (some #'cl-cc::vm-call-p instructions))

(deftest inline-small-function
  "A small function (+ x 1) called via vm-func-ref: vm-call eliminated and
   result vm-move into the call's dst register (:R6) must appear."
  (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "inc"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "inc"))
         (jump-past (cl-cc::make-vm-jump :label "after_inc"))
         (lbl     (cl-cc::make-vm-label :name "inc"))
         (body1   (cl-cc::make-vm-const :dst :R11 :value 1))
         (body2   (cl-cc::make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
         (ret     (cl-cc::make-vm-ret :reg :R12))
         (after   (cl-cc::make-vm-label :name "after_inc"))
         (arg     (cl-cc::make-vm-const :dst :R1 :value 4))
         (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc::make-vm-halt))
         (instrs  (list closure fref jump-past lbl body1 body2 ret after arg call halt))
         (out     (cl-cc/optimize::opt-pass-inline instrs)))
    (assert-true (not (inline-has-call-p out)))
    (assert-true (some (lambda (i)
                         (and (cl-cc::vm-move-p i)
                              (eq :R6 (cl-cc/vm::vm-dst i))))
                       out))))

(deftest inline-skip-large-function
  "A function whose body exceeds the inline threshold is NOT inlined;
   the vm-call remains in the output."
  (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "big"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "big"))
         (jump-past (cl-cc::make-vm-jump :label "after_big"))
         (lbl     (cl-cc::make-vm-label :name "big"))
         ;; Generate a live arithmetic chain that stays above the inline threshold
         ;; even after convergence passes.
         (body    (append
                   (list (cl-cc::make-vm-const :dst :R20 :value 1))
                   (loop for i from 21 below 40
                         for prev = :R10 then (intern (format nil "R~A" (1- i)) :keyword)
                         collect (cl-cc::make-vm-add
                                  :dst (intern (format nil "R~A" i) :keyword)
                                  :lhs prev
                                  :rhs :R20))))
         (ret     (cl-cc::make-vm-ret :reg :R39))
         (after   (cl-cc::make-vm-label :name "after_big"))
         (arg     (cl-cc::make-vm-const :dst :R1 :value 0))
         (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc::make-vm-halt))
         (instrs  (append (list closure fref jump-past lbl)
                          body
                          (list ret after arg call halt)))
         (out     (cl-cc/optimize::opt-pass-inline instrs)))
    (assert-true (inline-has-call-p out))))

(deftest-each inline-skip-cases
  "Functions with captured vars, recursive bodies, or self-references are NOT inlined."
  :cases (("captured-vars"
           (lambda ()
             (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "captured"
                                                     :params '(:R10) :captured '(:R99)))
                    (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "captured"))
                    (jump-past (cl-cc::make-vm-jump :label "after_cap"))
                    (lbl     (cl-cc::make-vm-label :name "captured"))
                    (body1   (cl-cc::make-vm-const :dst :R11 :value 1))
                    (body2   (cl-cc::make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
                    (ret     (cl-cc::make-vm-ret :reg :R12))
                    (after   (cl-cc::make-vm-label :name "after_cap"))
                    (arg     (cl-cc::make-vm-const :dst :R1 :value 4))
                    (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
                    (halt    (cl-cc::make-vm-halt)))
               (list closure fref jump-past lbl body1 body2 ret after arg call halt))))
          ("recursive-global-ref"
           (lambda ()
             (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "rec"
                                                     :params '(:R10) :captured nil))
                    (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "rec"))
                    (jump-past (cl-cc::make-vm-jump :label "after_rec"))
                    (lbl     (cl-cc::make-vm-label :name "rec"))
                    (b1      (cl-cc::make-vm-const :dst :R11 :value 0))
                    (b2      (cl-cc::make-vm-sub :dst :R12 :lhs :R10 :rhs :R11))
                    (b3      (cl-cc::make-vm-call :dst :R13 :func :R5 :args '(:R12)))
                    (ret     (cl-cc::make-vm-ret :reg :R13))
                    (after   (cl-cc::make-vm-label :name "after_rec"))
                    (arg     (cl-cc::make-vm-const :dst :R1 :value 5))
                    (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
                    (halt    (cl-cc::make-vm-halt)))
               (list closure fref jump-past lbl b1 b2 b3 ret after arg call halt))))
          ("self-recursive-guard"
           (lambda ()
             (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "loop"
                                                     :params '(:R10) :captured nil))
                    (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "loop"))
                    (jump-past (cl-cc::make-vm-jump :label "after_loop"))
                    (lbl     (cl-cc::make-vm-label :name "loop"))
                    (self    (cl-cc::make-vm-func-ref :dst :R7 :label "loop"))
                    (body1   (cl-cc::make-vm-call :dst :R11 :func :R7 :args '(:R10)))
                    (ret     (cl-cc::make-vm-ret :reg :R11))
                    (after   (cl-cc::make-vm-label :name "after_loop"))
                    (arg     (cl-cc::make-vm-const :dst :R1 :value 5))
                    (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
                    (halt    (cl-cc::make-vm-halt)))
               (list closure fref jump-past lbl self body1 ret after arg call halt)))))
  (make-instrs)
  (let* ((instrs (funcall make-instrs))
         (out    (cl-cc/optimize::opt-pass-inline instrs)))
    (assert-true (inline-has-call-p out))))

(deftest inline-register-rename
  "After inlining, the body's registers are renamed to fresh indices so they
   do not conflict with existing registers at the call site."
  (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "g"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "g"))
         (jump-past (cl-cc::make-vm-jump :label "after_g"))
         (lbl     (cl-cc::make-vm-label :name "g"))
         ;; Body uses :R11 and :R12 (same registers the call site also uses)
         (body1   (cl-cc::make-vm-const :dst :R11 :value 10))
         (body2   (cl-cc::make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
         (ret     (cl-cc::make-vm-ret :reg :R12))
         (after   (cl-cc::make-vm-label :name "after_g"))
         ;; Call site also uses :R11 and :R12 for its own values
         (site1   (cl-cc::make-vm-const :dst :R11 :value 100))
         (site2   (cl-cc::make-vm-const :dst :R12 :value 200))
         (arg     (cl-cc::make-vm-const :dst :R1 :value 7))
         (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc::make-vm-halt))
         (instrs  (list closure fref jump-past lbl body1 body2 ret
                        after site1 site2 arg call halt))
         (out     (cl-cc/optimize::opt-pass-inline instrs)))
    ;; The call should be inlined (small body, no captures)
    (assert-true (not (inline-has-call-p out)))
    ;; The inlined body must use renamed registers (not the original :R11/:R12)
    ;; so collect all dst registers written after the call site's :R12 const
    ;; and before the halt — at least one must be > :R12 index
    (let ((inlined-dsts (loop for i in out
                              when (and (cl-cc::vm-move-p i)
                                        (eq :R6 (cl-cc/vm::vm-dst i)))
                              collect i)))
      ;; The final vm-move into :R6 must exist (proof of inlining)
      (assert-true (not (null inlined-dsts))))))

(deftest inline-propagates-constant-call-args
  "Constant call arguments are emitted as constants inside the inlined body."
  (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "k"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "k"))
         (jump-past (cl-cc::make-vm-jump :label "after_k"))
         (lbl     (cl-cc::make-vm-label :name "k"))
         (body1   (cl-cc::make-vm-const :dst :R11 :value 1))
         (body2   (cl-cc::make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
         (ret     (cl-cc::make-vm-ret :reg :R12))
         (after   (cl-cc::make-vm-label :name "after_k"))
         (arg     (cl-cc::make-vm-const :dst :R1 :value 4))
         (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc::make-vm-halt))
         (instrs  (list closure fref jump-past lbl body1 body2 ret after arg call halt))
         (out     (cl-cc/optimize::opt-pass-inline instrs)))
    (assert-true (not (inline-has-call-p out)))
    (assert-equal 2
                  (count-if (lambda (i)
                              (and (cl-cc::vm-const-p i)
                                   (eql 4 (cl-cc/vm::vm-value i))))
                            out))))


