;;;; packages/emit/src/mlir.lisp — MIR to textual MLIR lowering

(in-package :cl-cc/emit)

(defstruct mlir-module
  "Textual MLIR module produced from CL-CC MIR through the clcc dialect."
  (name "clcc_module" :type string)
  (dialect "clcc" :type string)
  (body "" :type string)
  (metadata nil :type list))

(defstruct (clcc-op (:conc-name clccop-))
  "An operation in the internal cl-cc MLIR dialect before conversion.

NAME is one of CLCC.CONST, CLCC.ADD, CLCC.SUB, CLCC.MUL, CLCC.CALL,
CLCC.RETURN, CLCC.BRANCH, or CLCC.BR.  The public emitter lowers these ops to
MLIR builtin/func/arith/cf syntax that mlir-opt can parse."
  name
  results
  operands
  attrs
  type)

(defstruct (mlir-lower-context (:conc-name mlirctx-))
  (declarations (make-hash-table :test #'equal) :type hash-table)
  (temp-counter 0 :type fixnum)
  (param-names (make-hash-table :test #'eq) :type hash-table))

(defun mlir-bridge-capabilities ()
  "Return the implemented MLIR lowering capabilities for MIR input."
  '(:fr-id :fr-712
    :format :textual-mlir
    :input :mir
    :defined-dialect (:clcc
                      (:ops clcc.const clcc.add clcc.sub clcc.mul
                            clcc.call clcc.return clcc.branch clcc.br))
    :lowering-pipeline ((:clcc.const :arith.constant)
                        (:clcc.add :arith.addi)
                        (:clcc.sub :arith.subi)
                        (:clcc.mul :arith.muli)
                        (:clcc.call :func.call)
                        (:clcc.return :func.return)
                        (:clcc.branch :cf.cond_br)
                        (:clcc.br :cf.br)
                        (:arith :llvm))
    :dialects (:builtin :func :arith :cf :llvm)
    :types (:index :i1 :i8 :i64 :ptr :void)))

(defun %mlir-downcase-name (name)
  (string-downcase
   (cond
     ((keywordp name) (symbol-name name))
     ((symbolp name) (symbol-name name))
     ((stringp name) name)
     (t (format nil "~A" name)))))

(defun %mlir-identifier-char-p (ch)
  (or (alphanumericp ch)
      (find ch "_$." :test #'char=)))

(defun %mlir-sanitize-name (name &key (fallback "clcc"))
  (let* ((raw (%mlir-downcase-name name))
         (sanitized (with-output-to-string (out)
                      (loop for ch across raw
                            do (write-char (if (%mlir-identifier-char-p ch) ch #\_) out)))))
    (if (or (zerop (length sanitized))
            (digit-char-p (char sanitized 0)))
        (format nil "~A_~A" fallback sanitized)
        sanitized)))

(defun %mlir-symbol-name (name)
  (format nil "@~A" (%mlir-sanitize-name name :fallback "fn")))

(defun %mlir-block-label-name (block)
  (%mlir-sanitize-name (or (mirb-label block) (format nil "block~D" (mirb-id block)))
                       :fallback "block"))

(defun %mlir-block-ref (block)
  (format nil "^~A" (%mlir-block-label-name block)))

(defun %mlir-type (type)
  (case type
    ((:fixnum :integer :signed :word :any) "i64")
    ((:character :char) "i8")
    (:boolean "i1")
    ((:pointer :ptr :function :values) "!llvm.ptr")
    (:void "none")
    (otherwise "i64")))

(defun %mlir-value-type (value)
  (%mlir-type (mirv-type value)))

(defun %mlir-const-type (const)
  (%mlir-type (mirc-type const)))

(defun %mlir-operand-type (operand)
  (cond
    ((mir-value-p operand) (%mlir-value-type operand))
    ((mir-const-p operand) (%mlir-const-type operand))
    (t "i64")))

(defun %mlir-value-name (ctx value)
  (or (gethash value (mlirctx-param-names ctx))
      (format nil "%reg~D" (mirv-id value))))

(defun %mlir-next-temp (ctx &optional (prefix "c"))
  (prog1 (format nil "%~A~D" prefix (mlirctx-temp-counter ctx))
    (incf (mlirctx-temp-counter ctx))))

(defun %mlir-zero-value (type)
  (if (string= type "!llvm.ptr") "0" "0"))

(defun %mlir-result-type-list (type)
  (if (or (null type) (string= type "none")) "()" type))

(defun %mlir-const-value (const)
  (let ((value (mirc-value const)))
    (cond
      ((null value) "0")
      ((eq value t) "1")
      ((characterp value) (format nil "~D" (char-code value)))
      ((integerp value) (format nil "~D" value))
      (t "0"))))

(defun %mlir-call-target-name (operand)
  (cond
    ((mir-const-p operand)
     (let ((value (mirc-value operand)))
       (if (or (symbolp value) (stringp value))
           (%mlir-symbol-name value)
           (%mlir-symbol-name "clcc_indirect_call"))))
    ((or (symbolp operand) (stringp operand)) (%mlir-symbol-name operand))
    (t (%mlir-symbol-name "clcc_indirect_call"))))

(defun %mlir-register-declaration (ctx target return-type arg-types)
  (setf (gethash target (mlirctx-declarations ctx))
        (list return-type arg-types)))

(defun %mir-inst->clcc-op (inst)
  (let ((dst (miri-dst inst))
        (srcs (miri-srcs inst)))
    (case (miri-op inst)
      (:const
       (make-clcc-op :name 'clcc.const :results (and dst (list dst))
                     :operands srcs :type (and dst (%mlir-value-type dst))))
      (:move
       (make-clcc-op :name 'clcc.const :results (and dst (list dst))
                     :operands srcs :type (and dst (%mlir-value-type dst))))
      (:add
       (make-clcc-op :name 'clcc.add :results (and dst (list dst))
                     :operands srcs :type (and dst (%mlir-value-type dst))))
      (:sub
       (make-clcc-op :name 'clcc.sub :results (and dst (list dst))
                     :operands srcs :type (and dst (%mlir-value-type dst))))
      (:mul
       (make-clcc-op :name 'clcc.mul :results (and dst (list dst))
                     :operands srcs :type (and dst (%mlir-value-type dst))))
      (:call
       (make-clcc-op :name 'clcc.call :results (and dst (list dst))
                     :operands srcs :type (%mlir-type (miri-type inst))))
      (:ret
       (make-clcc-op :name 'clcc.return :operands srcs :type :void))
      (:jump
       (make-clcc-op :name 'clcc.br :operands srcs :type :void))
      (:branch
       (make-clcc-op :name 'clcc.branch :operands srcs :type :void))
      (otherwise
       (error "Unsupported MIR op for MLIR lowering: ~S" (miri-op inst))))))

(defun %mlir-emit-constant (out ctx const &optional result-name)
  (let ((name (or result-name (%mlir-next-temp ctx)))
        (type (%mlir-const-type const)))
    (format out "    ~A = arith.constant ~A : ~A~%" name (%mlir-const-value const) type)
    name))

(defun %mlir-operand (out ctx operand &optional expected-type)
  (cond
    ((mir-value-p operand) (%mlir-value-name ctx operand))
    ((mir-const-p operand) (%mlir-emit-constant out ctx operand))
    ((mir-block-p operand) (%mlir-block-ref operand))
    ((integerp operand)
     (%mlir-emit-constant out ctx (make-mir-const :value operand :type :integer)))
    ((null operand)
     (%mlir-emit-constant out ctx (make-mir-const :value 0 :type :integer)))
    ((eq operand t)
     (%mlir-emit-constant out ctx (make-mir-const :value 1 :type :boolean)))
    (t
     (%mlir-emit-constant out ctx (make-mir-const :value 0 :type (or expected-type :integer))))))

(defun %mlir-emit-bool-coercion (out ctx operand)
  (let ((type (%mlir-operand-type operand))
        (value (%mlir-operand out ctx operand)))
    (if (string= type "i1")
        value
        (let ((tmp (%mlir-next-temp ctx "cond")))
          (format out "    ~A = arith.cmpi ne, ~A, ~A : ~A~%"
                  tmp value (%mlir-zero-value type) type)
          tmp))))

(defun %mlir-lower-clcc-op (out ctx op)
  (case (clccop-name op)
    (clcc.const
     (let ((dst (%mlir-value-name ctx (first (clccop-results op))))
           (src (first (clccop-operands op))))
       (cond
         ((mir-const-p src) (%mlir-emit-constant out ctx src dst))
         (t (format out "    ~A = arith.addi ~A, ~A : ~A~%"
                    dst (%mlir-operand out ctx src (clccop-type op))
                    (%mlir-zero-value (clccop-type op)) (clccop-type op))))))
    ((clcc.add clcc.sub clcc.mul)
     (let* ((dst (%mlir-value-name ctx (first (clccop-results op))))
            (type (clccop-type op))
            (operands (clccop-operands op))
            (lhs (%mlir-operand out ctx (first operands) type))
            (rhs (%mlir-operand out ctx (second operands) type))
            (mlir-op (case (clccop-name op)
                       (clcc.add "arith.addi")
                       (clcc.sub "arith.subi")
                       (clcc.mul "arith.muli"))))
       (format out "    ~A = ~A ~A, ~A : ~A~%" dst mlir-op lhs rhs type)))
    (clcc.call
     (let* ((srcs (clccop-operands op))
            (target (%mlir-call-target-name (first srcs)))
            (args (rest srcs))
            (return-type (clccop-type op))
            (arg-types (mapcar #'%mlir-operand-type args))
            (arg-values (loop for arg in args collect (%mlir-operand out ctx arg)))
            (result (first (clccop-results op))))
       (%mlir-register-declaration ctx target return-type arg-types)
       (if result
           (format out "    ~A = func.call ~A(~{~A~^, ~}) : (~{~A~^, ~}) -> ~A~%"
                   (%mlir-value-name ctx result) target arg-values arg-types
                   (%mlir-result-type-list return-type))
           (format out "    func.call ~A(~{~A~^, ~}) : (~{~A~^, ~}) -> ()~%"
                   target arg-values arg-types))))
    (clcc.return
     (if (clccop-operands op)
         (let* ((value (first (clccop-operands op)))
                (type (%mlir-operand-type value)))
           (format out "    func.return ~A : ~A~%" (%mlir-operand out ctx value type) type))
         (format out "    func.return~%")))
    (clcc.br
     (format out "    cf.br ~A~%" (%mlir-block-ref (first (clccop-operands op)))))
    (clcc.branch
     (destructuring-bind (cond then-block else-block) (clccop-operands op)
       (format out "    cf.cond_br ~A, ~A, ~A~%"
               (%mlir-emit-bool-coercion out ctx cond)
               (%mlir-block-ref then-block)
               (%mlir-block-ref else-block))))
    (otherwise
     (error "Unsupported clcc op during MLIR lowering: ~S" (clccop-name op)))))

(defun %mlir-block-terminated-p (block)
  (let ((insts (mirb-insts block)))
    (and insts
         (member (miri-op (car (last insts)))
                 '(:ret :jump :branch)
                 :test #'eq))))

(defun %mlir-infer-function-return-type (fn)
  (let ((type nil))
    (dolist (block (mir-rpo fn) (or type "i64"))
      (dolist (inst (mirb-insts block))
        (when (eq (miri-op inst) :ret)
          (setf type (if (miri-srcs inst)
                         (%mlir-operand-type (first (miri-srcs inst)))
                         "none")))))))

(defun %mlir-emit-block (out ctx block return-type)
  (format out "  ~A:~%" (%mlir-block-ref block))
  (when (mirb-phis block)
    (error "MLIR lowering does not yet support MIR phi nodes in block ~A; lower phi nodes to block arguments first."
           (%mlir-block-ref block)))
  (dolist (inst (mirb-insts block))
    (%mlir-lower-clcc-op out ctx (%mir-inst->clcc-op inst)))
  (unless (%mlir-block-terminated-p block)
    (if (string= return-type "none")
        (format out "    func.return~%")
        (format out "    func.return ~A : ~A~%" (%mlir-zero-value return-type) return-type))))

(defun %mlir-register-params (ctx fn)
  (loop for param in (mirf-params fn)
        for index from 0
        do (setf (gethash param (mlirctx-param-names ctx)) (format nil "%arg~D" index))))

(defun %mlir-emit-function (ctx fn)
  (with-output-to-string (out)
    (%mlir-register-params ctx fn)
    (let* ((return-type (%mlir-infer-function-return-type fn))
           (params (loop for param in (mirf-params fn)
                         for index from 0
                         collect (format nil "%arg~D: ~A" index (%mlir-value-type param)))))
      (format out "  func.func ~A(~{~A~^, ~})"
              (%mlir-symbol-name (mirf-name fn)) params)
      (unless (string= return-type "none")
        (format out " -> ~A" return-type))
      (format out " {~%")
      (dolist (block (mir-rpo fn))
        (%mlir-emit-block out ctx block return-type))
      (format out "  }~%"))))

(defun %mlir-emit-declarations (ctx defined-functions)
  (with-output-to-string (out)
    (maphash
     (lambda (target signature)
       (unless (member target defined-functions :test #'string=)
         (destructuring-bind (return-type arg-types) signature
           (format out "  func.func private ~A(~{~A~^, ~})"
                   target arg-types)
           (unless (string= return-type "none")
             (format out " -> ~A" return-type))
           (terpri out))))
     (mlirctx-declarations ctx))))

(defun %mlir-module-functions (thing)
  (cond
    ((mir-module-p thing) (mirm-functions thing))
    ((mir-function-p thing) (list thing))
    (t nil)))

(defun vm-program->mlir-module (program &key (name "clcc_module"))
  "Lower PROGRAM, a MIR module/function, to a textual MLIR module.

The lowering pipeline is MIR → internal clcc dialect ops → func/arith/cf MLIR.
The emitted text is valid MLIR module syntax suitable for mlir-opt parsing."
  (let* ((ctx (make-mlir-lower-context))
         (functions (%mlir-module-functions program))
         (defined (mapcar (lambda (fn) (%mlir-symbol-name (mirf-name fn))) functions))
         (function-text (with-output-to-string (out)
                          (dolist (fn functions)
                            (write-string (%mlir-emit-function ctx fn) out))))
         (declarations (%mlir-emit-declarations ctx defined))
         (body (with-output-to-string (out)
                 (format out "module @~A {~%" (%mlir-sanitize-name name :fallback "module"))
                 (write-string declarations out)
                 (write-string function-text out)
                 (unless functions
                   (format out "  func.func @clcc_entry() -> i64 {~%")
                   (format out "  ^entry:~%")
                   (format out "    %reg0 = arith.constant 0 : i64~%")
                   (format out "    return %reg0 : i64~%")
                   (format out "  }~%"))
                 (format out "}~%"))))
    (make-mlir-module
     :name name
     :body body
     :metadata (list :function-count (length functions)
                     :capabilities (mlir-bridge-capabilities)))))

(defun emit-mlir (thing &key stream (name "clcc_module"))
  "Emit textual MLIR for THING, a MIR module/function or MLIR-MODULE.

Returns the MLIR string and optionally writes it to STREAM."
  (let* ((module (if (typep thing 'mlir-module)
                     thing
                     (vm-program->mlir-module thing :name name)))
         (text (mlir-module-body module)))
    (when stream (write-string text stream))
    text))
