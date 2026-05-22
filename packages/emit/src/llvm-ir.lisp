;;;; packages/emit/src/llvm-ir.lisp — MIR to textual LLVM IR lowering

(in-package :cl-cc/emit)

(defparameter +llvm-ir-default-target-triple+ "x86_64-unknown-linux-gnu")

(defparameter +llvm-ir-default-data-layout+
  "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128")

(defstruct llvm-ir-module
  "Textual LLVM IR module produced from CL-CC MIR."
  (name "clcc_module" :type string)
  (target-triple +llvm-ir-default-target-triple+ :type string)
  (data-layout +llvm-ir-default-data-layout+ :type string)
  (body "" :type string)
  (metadata nil :type list))

(defstruct (llvm-lower-context (:conc-name llvmctx-))
  (declarations (make-hash-table :test #'equal) :type hash-table)
  (temp-counter 0 :type fixnum))

(defun llvm-ir-bridge-capabilities ()
  "Return LLVM IR lowering capabilities for MIR input."
  '(:fr-id :fr-690
    :format :textual-llvm-ir
    :input :mir
    :lowering (:module :function :basic-block :ssa :phi
               :const :move :arithmetic :bitwise :compare
               :alloca :load :store :call :tail-call :branch :jump :ret)
    :types (:fixnum :integer :character :boolean :pointer :void)))

(defun %llvm-string-prefix-p (prefix string)
  (let ((plen (length prefix)))
    (and (<= plen (length string))
         (string= prefix string :end2 plen))))

(defun %llvm-downcase-name (name)
  (string-downcase
   (cond
     ((keywordp name) (symbol-name name))
     ((symbolp name) (symbol-name name))
     ((stringp name) name)
     (t (format nil "~A" name)))))

(defun %llvm-identifier-char-p (ch)
  (or (alphanumericp ch)
      (find ch "_$.-" :test #'char=)))

(defun %llvm-sanitize-name (name &key (fallback "clcc"))
  (let* ((raw (%llvm-downcase-name name))
         (sanitized (with-output-to-string (out)
                      (loop for ch across raw
                            do (write-char (if (%llvm-identifier-char-p ch) ch #\_) out)))))
    (if (or (zerop (length sanitized))
            (digit-char-p (char sanitized 0)))
        (format nil "~A_~A" fallback sanitized)
        sanitized)))

(defun %llvm-global-name (name)
  (format nil "@~A" (%llvm-sanitize-name name :fallback "fn")))

(defun %llvm-block-label-name (block)
  (%llvm-sanitize-name (or (mirb-label block) (format nil "block~D" (mirb-id block)))
                       :fallback "block"))

(defun %llvm-block-ref (block)
  (format nil "%~A" (%llvm-block-label-name block)))

(defun %llvm-value-name (value)
  (format nil "%reg~D" (mirv-id value)))

(defun %llvm-type (type)
  (case type
    ((:fixnum :integer :signed :word :any) "i64")
    ((:character :char) "i8")
    (:boolean "i1")
    ((:pointer :ptr :function :values) "ptr")
    (:void "void")
    (otherwise "i64")))

(defun %llvm-value-type (value)
  (%llvm-type (mirv-type value)))

(defun %llvm-const-type (const)
  (%llvm-type (mirc-type const)))

(defun %llvm-operand-type (operand)
  (cond
    ((mir-value-p operand) (%llvm-value-type operand))
    ((mir-const-p operand) (%llvm-const-type operand))
    (t "i64")))

(defun %llvm-zero-value (type)
  (if (string= type "ptr") "null" "0"))

(defun %llvm-const-value (const &optional (expected-type (%llvm-const-type const)))
  (let ((value (mirc-value const)))
    (cond
      ((and (string= expected-type "ptr") (null value)) "null")
      ((null value) "0")
      ((eq value t) "1")
      ((characterp value) (format nil "~D" (char-code value)))
      ((integerp value) (format nil "~D" value))
      ((and (symbolp value) (not (keywordp value)))
       (%llvm-global-name value))
      (t (format nil "0")))))

(defun %llvm-operand (operand &optional expected-type)
  (cond
    ((mir-value-p operand) (%llvm-value-name operand))
    ((mir-const-p operand) (%llvm-const-value operand (or expected-type (%llvm-const-type operand))))
    ((mir-block-p operand) (%llvm-block-ref operand))
    ((integerp operand) (format nil "~D" operand))
    ((null operand) (if (and expected-type (string= expected-type "ptr")) "null" "0"))
    ((eq operand t) "1")
    (t (format nil "0"))))

(defun %llvm-next-temp (ctx &optional (prefix "tmp"))
  (prog1 (format nil "%~A~D" prefix (llvmctx-temp-counter ctx))
    (incf (llvmctx-temp-counter ctx))))

(defun %llvm-emit-gep-if-needed (out ctx ptr offset)
  (let ((ptr-name (%llvm-operand ptr "ptr"))
        (offset-type (%llvm-operand-type offset))
        (offset-name (%llvm-operand offset "i64")))
    (if (and (mir-const-p offset)
             (integerp (mirc-value offset))
             (zerop (mirc-value offset)))
        ptr-name
        (let ((gep (%llvm-next-temp ctx "gep")))
          (format out "  ~A = getelementptr i8, ptr ~A, ~A ~A~%"
                  gep ptr-name offset-type offset-name)
          gep))))

(defun %llvm-call-target-name (operand)
  (cond
    ((mir-const-p operand)
     (let ((value (mirc-value operand)))
       (cond
         ((or (symbolp value) (stringp value)) (%llvm-global-name value))
         (t (%llvm-global-name "clcc_indirect_call")))))
    ((or (symbolp operand) (stringp operand)) (%llvm-global-name operand))
    (t (%llvm-global-name "clcc_indirect_call"))))

(defun %llvm-register-declaration (ctx target return-type arg-types)
  (setf (gethash target (llvmctx-declarations ctx))
        (list return-type arg-types)))

(defun %llvm-emit-bool-coercion (out ctx operand)
  (let ((type (%llvm-operand-type operand))
        (value (%llvm-operand operand)))
    (if (string= type "i1")
        value
        (let ((tmp (%llvm-next-temp ctx "cond")))
          (format out "  ~A = icmp ne ~A ~A, ~A~%"
                  tmp type value (%llvm-zero-value type))
          tmp))))

(defun %llvm-emit-const (out inst)
  (let* ((dst (miri-dst inst))
         (type (%llvm-value-type dst))
         (src (first (miri-srcs inst)))
         (value (%llvm-operand src type)))
    (cond
      ((string= type "ptr")
       (if (string= value "null")
           (format out "  ~A = inttoptr i64 0 to ptr~%" (%llvm-value-name dst))
           (format out "  ~A = bitcast ptr ~A to ptr~%" (%llvm-value-name dst) value)))
      ((string= type "i1")
       (format out "  ~A = icmp ne i64 ~A, 0~%" (%llvm-value-name dst) value))
      (t
       (format out "  ~A = add ~A 0, ~A~%" (%llvm-value-name dst) type value)))))

(defun %llvm-emit-move (out inst)
  (let* ((dst (miri-dst inst))
         (type (%llvm-value-type dst))
         (src (first (miri-srcs inst)))
         (value (%llvm-operand src type)))
    (cond
      ((string= type "ptr")
       (format out "  ~A = bitcast ptr ~A to ptr~%" (%llvm-value-name dst) value))
      ((string= type "i1")
       (format out "  ~A = or i1 ~A, false~%" (%llvm-value-name dst) value))
      (t
       (format out "  ~A = add ~A ~A, 0~%" (%llvm-value-name dst) type value)))))

(defun %llvm-emit-binary (out inst llvm-op)
  (let* ((dst (miri-dst inst))
         (type (%llvm-value-type dst))
         (srcs (miri-srcs inst))
         (lhs (%llvm-operand (first srcs) type))
         (rhs (%llvm-operand (second srcs) type)))
    (format out "  ~A = ~A ~A ~A, ~A~%"
            (%llvm-value-name dst) llvm-op type lhs rhs)))

(defun %llvm-emit-compare (out inst pred)
  (let* ((dst (miri-dst inst))
         (srcs (miri-srcs inst))
         (type (%llvm-operand-type (first srcs)))
         (lhs (%llvm-operand (first srcs) type))
         (rhs (%llvm-operand (second srcs) type)))
    (format out "  ~A = icmp ~A ~A ~A, ~A~%"
            (%llvm-value-name dst) pred type lhs rhs)))

(defun %llvm-emit-call (out ctx inst &key tail-p)
  (let* ((srcs (miri-srcs inst))
         (target (%llvm-call-target-name (first srcs)))
         (args (rest srcs))
         (return-type (if tail-p "i64" (%llvm-type (miri-type inst))))
         (arg-types (mapcar #'%llvm-operand-type args))
         (arg-text (loop for arg in args
                         for type in arg-types
                         collect (format nil "~A ~A" type (%llvm-operand arg type)))))
    (%llvm-register-declaration ctx target return-type arg-types)
    (if tail-p
        (let ((tmp (%llvm-next-temp ctx "tail")))
          (format out "  ~A = tail call ~A ~A(~{~A~^, ~})~%"
                  tmp return-type target arg-text)
          (format out "  ret ~A ~A~%" return-type tmp))
        (if (miri-dst inst)
            (format out "  ~A = call ~A ~A(~{~A~^, ~})~%"
                    (%llvm-value-name (miri-dst inst)) return-type target arg-text)
            (format out "  call ~A ~A(~{~A~^, ~})~%"
                    return-type target arg-text)))))

(defun %llvm-emit-inst (out ctx inst)
  (case (miri-op inst)
    (:nop nil)
    (:const (%llvm-emit-const out inst))
    (:move (%llvm-emit-move out inst))
    (:add (%llvm-emit-binary out inst "add"))
    (:sub (%llvm-emit-binary out inst "sub"))
    (:mul (%llvm-emit-binary out inst "mul"))
    (:div (%llvm-emit-binary out inst "sdiv"))
    (:mod (%llvm-emit-binary out inst "srem"))
    (:band (%llvm-emit-binary out inst "and"))
    (:bor (%llvm-emit-binary out inst "or"))
    (:bxor (%llvm-emit-binary out inst "xor"))
    (:shl (%llvm-emit-binary out inst "shl"))
    (:shr (%llvm-emit-binary out inst "ashr"))
    (:ushr (%llvm-emit-binary out inst "lshr"))
    (:neg
     (let* ((dst (miri-dst inst))
            (type (%llvm-value-type dst))
            (value (%llvm-operand (first (miri-srcs inst)) type)))
       (format out "  ~A = sub ~A 0, ~A~%" (%llvm-value-name dst) type value)))
    (:bnot
     (let* ((dst (miri-dst inst))
            (type (%llvm-value-type dst))
            (value (%llvm-operand (first (miri-srcs inst)) type)))
       (format out "  ~A = xor ~A ~A, -1~%" (%llvm-value-name dst) type value)))
    (:lt (%llvm-emit-compare out inst "slt"))
    (:le (%llvm-emit-compare out inst "sle"))
    (:gt (%llvm-emit-compare out inst "sgt"))
    (:ge (%llvm-emit-compare out inst "sge"))
    (:eq (%llvm-emit-compare out inst "eq"))
    (:ne (%llvm-emit-compare out inst "ne"))
    (:alloca
     (let* ((dst (miri-dst inst))
            (size (first (miri-srcs inst)))
            (size-type (%llvm-operand-type size)))
       (format out "  ~A = alloca i8, ~A ~A~%"
               (%llvm-value-name dst) size-type (%llvm-operand size "i64"))))
    (:load
     (destructuring-bind (ptr offset) (miri-srcs inst)
       (let* ((dst (miri-dst inst))
              (type (%llvm-value-type dst))
              (addr (%llvm-emit-gep-if-needed out ctx ptr offset)))
         (format out "  ~A = load ~A, ptr ~A~%" (%llvm-value-name dst) type addr))))
    (:store
     (destructuring-bind (ptr offset value) (miri-srcs inst)
       (let* ((type (%llvm-operand-type value))
              (addr (%llvm-emit-gep-if-needed out ctx ptr offset)))
         (format out "  store ~A ~A, ptr ~A~%" type (%llvm-operand value type) addr))))
    (:call (%llvm-emit-call out ctx inst))
    (:tail-call (%llvm-emit-call out ctx inst :tail-p t))
    (:ret
     (if (miri-srcs inst)
         (let* ((value (first (miri-srcs inst)))
                (type (%llvm-operand-type value)))
           (format out "  ret ~A ~A~%" type (%llvm-operand value type)))
         (format out "  ret void~%")))
    (:jump
     (format out "  br label ~A~%" (%llvm-block-ref (first (miri-srcs inst)))))
    (:branch
     (destructuring-bind (cond then-block else-block) (miri-srcs inst)
       (format out "  br i1 ~A, label ~A, label ~A~%"
               (%llvm-emit-bool-coercion out ctx cond)
               (%llvm-block-ref then-block)
               (%llvm-block-ref else-block))))
    (:safepoint
     (%llvm-register-declaration ctx "@llvm.donothing" "void" nil)
     (format out "  call void @llvm.donothing()~%"))
    (otherwise
     (error "Unsupported MIR op for LLVM IR lowering: ~S" (miri-op inst)))))

(defun %llvm-emit-phi (out phi)
  (let* ((dst (miri-dst phi))
         (type (%llvm-value-type dst))
         (incoming (loop for pair in (miri-srcs phi)
                         for pred = (car pair)
                         for value = (cdr pair)
                         collect (format nil "[ ~A, ~A ]"
                                         (%llvm-operand value type)
                                         (%llvm-block-ref pred)))))
    (format out "  ~A = phi ~A ~{~A~^, ~}~%"
            (%llvm-value-name dst) type incoming)))

(defun %llvm-block-terminated-p (block)
  (let ((insts (mirb-insts block)))
    (and insts
         (member (miri-op (car (last insts)))
                 '(:ret :jump :branch :tail-call)
                 :test #'eq))))

(defun %llvm-infer-function-return-type (fn)
  (let ((type nil))
    (dolist (block (mir-rpo fn) (or type "i64"))
      (dolist (inst (mirb-insts block))
        (when (eq (miri-op inst) :ret)
          (setf type (if (miri-srcs inst)
                         (%llvm-operand-type (first (miri-srcs inst)))
                         "void")))))))

(defun %llvm-emit-block (out ctx block return-type)
  (format out "~A:~%" (%llvm-block-label-name block))
  (dolist (phi (reverse (mirb-phis block)))
    (%llvm-emit-phi out phi))
  (dolist (inst (mirb-insts block))
    (%llvm-emit-inst out ctx inst))
  (unless (%llvm-block-terminated-p block)
    (if (string= return-type "void")
        (format out "  ret void~%")
        (format out "  ret ~A ~A~%" return-type (%llvm-zero-value return-type)))))

(defun %llvm-emit-function (ctx fn)
  (with-output-to-string (out)
    (let* ((return-type (%llvm-infer-function-return-type fn))
           (params (loop for param in (mirf-params fn)
                         collect (format nil "~A ~A"
                                         (%llvm-value-type param)
                                         (%llvm-value-name param)))))
      (format out "define ~A ~A(~{~A~^, ~}) {~%"
              return-type (%llvm-global-name (mirf-name fn)) params)
      (dolist (block (mir-rpo fn))
        (%llvm-emit-block out ctx block return-type))
      (format out "}~%~%"))))

(defun %llvm-emit-declarations (ctx defined-functions)
  (with-output-to-string (out)
    (maphash
     (lambda (target signature)
       (unless (member target defined-functions :test #'string=)
         (destructuring-bind (return-type arg-types) signature
           (format out "declare ~A ~A(~{~A~^, ~})~%"
                   return-type target arg-types))))
     (llvmctx-declarations ctx))
    (when (> (hash-table-count (llvmctx-declarations ctx)) 0)
      (terpri out))))

(defun %llvm-module-functions (thing)
  (cond
    ((mir-module-p thing) (mirm-functions thing))
    ((mir-function-p thing) (list thing))
    (t nil)))

(defun vm-program->llvm-ir-module (program &key (name "clcc_module")
                                              (target-triple +llvm-ir-default-target-triple+)
                                              (data-layout +llvm-ir-default-data-layout+))
  "Lower PROGRAM, a MIR module/function, to a textual LLVM IR module.

The public name is retained for compatibility with earlier emit callers.  When
PROGRAM is NIL or a VM program without MIR, an empty LLVM module is emitted."
  (let* ((ctx (make-llvm-lower-context))
         (functions (%llvm-module-functions program))
         (defined (mapcar (lambda (fn) (%llvm-global-name (mirf-name fn))) functions))
         (function-text (with-output-to-string (out)
                          (dolist (fn functions)
                            (write-string (%llvm-emit-function ctx fn) out))))
         (declarations (%llvm-emit-declarations ctx defined))
         (body (with-output-to-string (out)
                 (format out "; ModuleID = '~A'~%" name)
                 (format out "target datalayout = ~S~%" data-layout)
                 (format out "target triple = ~S~%~%" target-triple)
                 (write-string declarations out)
                 (write-string function-text out))))
    (make-llvm-ir-module
     :name name
     :target-triple target-triple
     :data-layout data-layout
     :body body
     :metadata (list :function-count (length functions)
                     :capabilities (llvm-ir-bridge-capabilities)))))

(defun emit-llvm-ir (thing &key stream (name "clcc_module")
                            (target-triple +llvm-ir-default-target-triple+)
                            (data-layout +llvm-ir-default-data-layout+))
  "Emit textual LLVM IR for THING, a MIR module/function or LLVM-IR-MODULE.

Returns the IR string and optionally writes it to STREAM."
  (let* ((module (if (typep thing 'llvm-ir-module)
                     thing
                     (vm-program->llvm-ir-module thing
                                                 :name name
                                                 :target-triple target-triple
                                                 :data-layout data-layout)))
         (text (llvm-ir-module-body module)))
    (when stream (write-string text stream))
    text))
