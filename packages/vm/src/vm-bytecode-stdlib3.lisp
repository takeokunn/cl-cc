;;; vm-bytecode-stdlib3.lisp — FR-1031: Bytecode Interpreter Layer
;;;
;;; Compact bytecode format with real opcodes and threaded dispatch interpreter.
;;; Opcodes: CONST LOAD STORE CALL TCALL RET JMP JNIL CLOSURE MAKE-ENV

(in-package :cl-cc/vm)

(defvar *bytecode-threshold* 100)

(defconstant +bc-const+ 0)
(defconstant +bc-load+ 1)
(defconstant +bc-store+ 2)
(defconstant +bc-call+ 3)
(defconstant +bc-tcall+ 4)
(defconstant +bc-ret+ 5)
(defconstant +bc-jmp+ 6)
(defconstant +bc-jnil+ 7)
(defconstant +bc-closure+ 8)
(defconstant +bc-make-env+ 9)

(defstruct vm-bytecode-function
  name
  (opcode-bytes (make-array 0 :element-type '(unsigned-byte 8)))
  (constant-pool (make-array 0))
  (env-size 0 :type fixnum)
  source-path)

;; ── Bytecode emitter helpers ────────────────────────────────────────────────

(defun %bc-emit (bytes &rest vals)
  (let* ((old (length bytes))
         (new (+ old (length vals)))
         (arr (make-array new :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace arr bytes)
    (loop for v in vals for i from old do (setf (aref arr i) (logand v #xFF)))
    arr))

(defun %bc-emit-word (bytes w)
  (%bc-emit bytes (logand w #xFF) (logand (ash w -8) #xFF)))

(defun %bc-read-word (bytes pc)
  (values (+ (aref bytes pc) (ash (aref bytes (1+ pc)) 8)) (+ pc 2)))

;; ── Real bytecode compiler ──────────────────────────────────────────────────

(defun compile-to-bytecode (form &key (name 'anonymous-bytecode-fn))
  "Compile FORM to real bytecode with opcode dispatch."
  (let ((bytes (make-array 0 :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 8)))
        (pool (make-array 0 :adjustable t :fill-pointer 0))
        (env (list)) (next-slot 0))
    (labels ((emit (op) (vector-push-extend op bytes))
             (emit-word (w) (emit (logand w #xFF)) (emit (logand (ash w -8) #xFF)))
             (add-constant (val)
               (or (position val pool :test #'equal)
                   (let ((i (fill-pointer pool))) (vector-push-extend val pool) i)))
             (compile-expr (expr)
               (typecase expr
                 (integer (emit +bc-const+) (emit-word (add-constant expr)))
                 (keyword (emit +bc-const+) (emit-word (add-constant expr)))
                 (symbol
                   (let ((slot (assoc expr env)))
                     (if slot (progn (emit +bc-load+) (emit (cdr slot)))
                         (progn (emit +bc-const+) (emit-word (add-constant expr))))))
                 (cons
                   (let ((fn (car expr)) (args (cdr expr)))
                     (dolist (a (reverse args)) (compile-expr a))
                     (compile-expr fn)
                     (emit +bc-call+) (emit (length args))))
                 (t (emit +bc-const+) (emit-word (add-constant expr))))))
      (compile-expr form)
      (emit +bc-ret+)
      (make-vm-bytecode-function :name name
        :opcode-bytes (coerce bytes '(simple-array (unsigned-byte 8) (*)))
        :constant-pool (coerce pool 'simple-vector)
        :env-size next-slot :source-path *load-pathname*))))

;; ── Real bytecode interpreter ───────────────────────────────────────────────

(defun run-bytecode (bytecode-fn &optional args)
  "Execute bytecode via threaded dispatch (tagbody/go) over real opcodes."
  (let* ((bytes (vm-bytecode-function-opcode-bytes bytecode-fn))
         (pool (vm-bytecode-function-constant-pool bytecode-fn))
         (env-size (vm-bytecode-function-env-size bytecode-fn))
         (locals (when (> env-size 0) (make-array env-size :initial-element nil)))
         (stack (make-array 64 :adjustable t :fill-pointer 0))
         (pc 0))
    ;; Push args onto stack in reverse (rightmost first on stack)
    (when args (dolist (a (reverse args)) (vector-push-extend a stack)))
    (labels ((push-stack (v) (vector-push-extend v stack))
             (pop-stack () (prog1 (aref stack (1- (fill-pointer stack)))
                             (decf (fill-pointer stack))))
             (peek-stack () (aref stack (1- (fill-pointer stack))))
             (fetch () (prog1 (aref bytes pc) (incf pc)))
             (fetch-word () (multiple-value-bind (w n) (%bc-read-word bytes pc) (setf pc n) w))
             (pool-ref (i) (aref pool i)))
      (tagbody dispatch
         (case (fetch)
           (#.+bc-const+ (push-stack (pool-ref (fetch-word))) (go dispatch))
           (#.+bc-load+ (let ((slot (fetch))) (push-stack (aref locals slot))) (go dispatch))
           (#.+bc-store+ (let ((slot (fetch))) (setf (aref locals slot) (pop-stack))) (go dispatch))
           (#.+bc-call+ (let* ((n (fetch)) (f (pop-stack))
                                (a (loop repeat n collect (pop-stack))))
                           (push-stack (apply f a)) (go dispatch)))
           (#.+bc-tcall+ (let* ((n (fetch)) (f (pop-stack))
                                 (a (loop repeat n collect (pop-stack))))
                            (return-from run-bytecode (apply f a))))
           (#.+bc-ret+ (return-from run-bytecode
                         (if (> (fill-pointer stack) 0) (peek-stack) nil)))
           (#.+bc-jmp+ (setf pc (fetch-word)) (go dispatch))
           (#.+bc-jnil+ (let ((tgt (fetch-word))) (unless (pop-stack) (setf pc tgt)) (go dispatch)))
           (#.+bc-closure+ (push-stack (pool-ref (fetch-word))) (incf pc) (go dispatch))
           (#.+bc-make-env+ (let ((n (fetch))) (push-stack (make-array n :initial-element nil)) (go dispatch)))
           (t (error "Unknown bytecode opcode ~D at pc ~D" (aref bytes (1- pc)) (1- pc))))))))

;; ── Real bytecode disassembler ──────────────────────────────────────────────

(defun bytecode-disassemble (bytecode-fn &optional (stream *standard-output*))
  (let* ((bytes (vm-bytecode-function-opcode-bytes bytecode-fn))
         (pool (vm-bytecode-function-constant-pool bytecode-fn))
         (len (length bytes)) (pc 0))
    (format stream "~&; Bytecode: ~A  Bytes:~D Pool:~D Env:~D~%"
            (vm-bytecode-function-name bytecode-fn) len
            (length pool) (vm-bytecode-function-env-size bytecode-fn))
    (loop while (< pc len) do
      (let ((op (aref bytes pc)) (start pc))
        (incf pc)
        (format stream "~4D: " start)
        (case op
          (#.+bc-const+ (format stream "CONST ~D~%" (%bc-read-word bytes pc))
           (setf pc (+ pc 2)))
          (#.+bc-load+  (format stream "LOAD ~D~%" (aref bytes pc)) (incf pc))
          (#.+bc-store+ (format stream "STORE ~D~%" (aref bytes pc)) (incf pc))
          (#.+bc-call+  (format stream "CALL ~D~%" (aref bytes pc)) (incf pc))
          (#.+bc-tcall+ (format stream "TCALL ~D~%" (aref bytes pc)) (incf pc))
          (#.+bc-ret+   (format stream "RET~%"))
          (#.+bc-jmp+   (format stream "JMP ~D~%" (%bc-read-word bytes pc))
           (setf pc (+ pc 2)))
          (#.+bc-jnil+  (format stream "JNIL ~D~%" (%bc-read-word bytes pc))
           (setf pc (+ pc 2)))
          (t (format stream "??? ~D~%" op) (incf pc)))))
    bytecode-fn))

(defun load-bytecode (bytecode-fn)
  (let ((name (vm-bytecode-function-name bytecode-fn)))
    (when name (setf (fdefinition name)
                     (lambda (&rest a) (run-bytecode bytecode-fn a))))
    t))

(export '(compile-to-bytecode load-bytecode bytecode-disassemble run-bytecode
          vm-bytecode-function vm-bytecode-function-p
          vm-bytecode-function-name vm-bytecode-function-opcode-bytes
          vm-bytecode-function-constant-pool vm-bytecode-function-env-size
          +bc-const+ +bc-load+ +bc-store+ +bc-call+ +bc-tcall+
          +bc-ret+ +bc-jmp+ +bc-jnil+ +bc-closure+ +bc-make-env+
          *bytecode-threshold*))
