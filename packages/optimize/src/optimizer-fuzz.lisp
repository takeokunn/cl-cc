;;;; optimizer-fuzz.lisp — FR-753 compiler fuzzing infrastructure

(in-package :cl-cc/optimize)

(defparameter *opt-fuzz-registers* '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7)
  "Register pool used by the FR-753 random valid IR generator.")

(defun %opt-fuzz-random-state (seed)
  (let ((state (make-random-state nil)))
    (dotimes (_ (mod (abs (or seed 0)) 997) state)
      (random 1000003 state))))

(defun %opt-fuzz-reg (index)
  (nth (mod index (length *opt-fuzz-registers*)) *opt-fuzz-registers*))

(defun %opt-fuzz-random-reg (state max-index)
  (%opt-fuzz-reg (random (max 1 max-index) state)))

(defun %opt-fuzz-random-const (state)
  (- (random 41 state) 20))

(defun %opt-fuzz-random-binop (state dst lhs rhs)
  (case (random 4 state)
    (0 (make-vm-add :dst dst :lhs lhs :rhs rhs))
    (1 (make-vm-sub :dst dst :lhs lhs :rhs rhs))
    (2 (make-vm-mul :dst dst :lhs lhs :rhs rhs))
    (t (make-vm-move :dst dst :src lhs))))

(defun opt-generate-random-ir-program (&key (state *random-state*) (max-program-length 16))
  "Generate a random valid straight-line integer VM IR program.

Generated programs initialize all live registers before use and terminate with a
VM-RET, making them suitable for semantic comparison against the local fuzz
interpreter."
  (let* ((limit (max 2 max-program-length))
         (initial-count (min 3 (length *opt-fuzz-registers*)))
         (program nil)
         (defined initial-count))
    (dotimes (i initial-count)
      (push (make-vm-const :dst (%opt-fuzz-reg i)
                           :value (%opt-fuzz-random-const state))
            program))
    (loop repeat (random (max 1 (- limit initial-count 1)) state)
          do (let* ((dst (%opt-fuzz-reg defined))
                    (lhs (%opt-fuzz-random-reg state defined))
                    (rhs (%opt-fuzz-random-reg state defined)))
               (push (%opt-fuzz-random-binop state dst lhs rhs) program)
               (setf defined (min (length *opt-fuzz-registers*) (1+ defined)))))
    (push (make-vm-ret :reg (%opt-fuzz-random-reg state defined)) program)
    (nreverse program)))

(defun %opt-fuzz-env-get (env reg)
  (multiple-value-bind (value foundp) (gethash reg env)
    (if foundp value 0)))

(defun opt-fuzz-interpret-ir (program)
  "Interpret the FR-753 straight-line fuzz IR subset and return its result."
  (let ((env (make-hash-table :test #'eq))
        (result nil)
        (returned nil))
    (dolist (inst program (values result returned))
      (unless returned
        (typecase inst
          (vm-const
           (setf (gethash (vm-dst inst) env) (vm-value inst)))
          (vm-move
           (setf (gethash (vm-dst inst) env) (%opt-fuzz-env-get env (vm-src inst))))
          (vm-add
           (setf (gethash (vm-dst inst) env)
                 (+ (%opt-fuzz-env-get env (vm-lhs inst))
                    (%opt-fuzz-env-get env (vm-rhs inst)))))
          (vm-sub
           (setf (gethash (vm-dst inst) env)
                 (- (%opt-fuzz-env-get env (vm-lhs inst))
                    (%opt-fuzz-env-get env (vm-rhs inst)))))
          (vm-mul
           (setf (gethash (vm-dst inst) env)
                 (* (%opt-fuzz-env-get env (vm-lhs inst))
                    (%opt-fuzz-env-get env (vm-rhs inst)))))
          (vm-ret
           (setf result (%opt-fuzz-env-get env (vm-reg inst))
                 returned t))
          (t
           (error "Unsupported fuzz IR instruction: ~S" inst)))))))

(defun %opt-fuzz-try-optimizer (program optimizer)
  (handler-case
      (funcall optimizer program)
    (error (condition)
      (values program condition))))

(defun %opt-fuzz-program-sexps (program)
  (mapcar (lambda (inst)
            (handler-case (instruction->sexp inst)
              (error () inst)))
          program))

(defun opt-run-compiler-fuzz (&key (trials 100)
                                   (seed 753)
                                   (max-program-length 16)
                                   (optimizer #'optimize-instructions))
  "Run FR-753 compiler fuzzing and return a result plist.

Each trial generates a random valid straight-line IR program, interprets it,
optimizes it with OPTIMIZER, and interprets the optimized program.  Any optimizer
error or semantic mismatch is reported in the returned plist instead of signaling,
so the harness can be used from CI or the REPL without destabilizing the default
test plan.  This is an explicit tool and is not wired into the default optimizer
pipeline."
  (let ((state (%opt-fuzz-random-state seed))
        (executed 0))
    (loop repeat (max 0 trials)
          do (incf executed)
             (let* ((program (opt-generate-random-ir-program
                              :state state
                              :max-program-length max-program-length))
                    (expected (multiple-value-list (opt-fuzz-interpret-ir program))))
               (multiple-value-bind (optimized error)
                   (%opt-fuzz-try-optimizer program optimizer)
                 (when error
                   (return-from opt-run-compiler-fuzz
                     (list :ok nil :kind :optimizer-error :trial executed
                           :error error :program (%opt-fuzz-program-sexps program))))
                 (let ((actual (multiple-value-list (opt-fuzz-interpret-ir optimized))))
                   (unless (equal expected actual)
                     (return-from opt-run-compiler-fuzz
                       (list :ok nil :kind :semantic-mismatch :trial executed
                             :expected expected :actual actual
                             :program (%opt-fuzz-program-sexps program)
                             :optimized (%opt-fuzz-program-sexps optimized))))))))
    (list :ok t :trials executed :seed seed)))
