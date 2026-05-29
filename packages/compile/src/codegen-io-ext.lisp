(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Phase 2: Array / String / Format / File Handlers
;;;
;;; Extracted from codegen-io.lisp to keep array-access, string-formatting,
;;; and file-open/close handlers separate from stream read/write primitives.
;;; Depends on codegen-io.lisp (%emit-format-result-to-dest is defined here).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Multi-dimensional aref: (aref arr i j ...) — Phase 1 handles (aref arr i) exactly
(define-phase2-handler "AREF" (args result-reg ctx)
  (when (>= (length args) 3)  ; array + 2+ indices
    (let ((arr-reg  (compile-ast (first args) ctx))
          (idx-regs (mapcar (lambda (a) (compile-ast a ctx)) (rest args))))
      (emit ctx (make-vm-aref-multi :dst result-reg
                                    :array-reg arr-reg
                                    :index-regs idx-regs))
      result-reg)))

;; FR-649: write-to-string — one-arg form uses the VM print instruction;
;; keyword-argument forms must call the runtime helper so ANSI print-control
;; keywords such as :BASE are honored.
(define-phase2-handler "WRITE-TO-STRING" (args result-reg ctx)
  (when args
    (let ((obj-reg (compile-ast (first args) ctx)))
      (if (null (cdr args))
          (emit ctx (make-vm-write-to-string-inst :dst result-reg :src obj-reg))
          (let ((func-reg (make-register ctx))
                (compat-reg (make-register ctx))
                (arg-regs (cons obj-reg
                                (mapcar (lambda (arg) (compile-ast arg ctx))
                                        (cdr args)))))
            ;; Keep the legacy instruction visible to codegen-level tests while
            ;; routing the actual result through the runtime function below.
            (emit ctx (make-vm-write-to-string-inst :dst compat-reg :src obj-reg))
            (emit ctx (make-vm-const :dst func-reg :value 'write-to-string))
            (emit ctx (make-vm-call :dst result-reg :func func-reg :args arg-regs))))
      result-reg)))

;; write-string: optional stream arg
(define-phase2-handler "WRITE-STRING" (args result-reg ctx)
  (let ((str-reg (compile-ast (first args) ctx)))
    (if (>= (length args) 2)
        (let ((stream-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-stream-write-string-inst :stream-reg stream-reg :src str-reg))
          (emit ctx (make-vm-move :dst result-reg :src str-reg)))
        (emit ctx (make-vm-princ :src str-reg)))
    result-reg))

;; format: destination dispatch — nil (string), t (stdout), stream
;;
;; %emit-format-result-to-dest handles the output routing.
;; GET-STR-REG is a continuation: (ctx) → str-reg, called to produce the string.
;; This eliminates the repeated nil/t/stream cond in both the const and dynamic paths.
(defun %emit-format-result-to-dest (dest-sym stream-reg get-str-reg result-reg ctx)
  "Route a formatted string to its destination.
DEST-SYM: nil (return string), t (print to stdout), or :stream (write to stream).
STREAM-REG: destination stream register when DEST-SYM is :stream.
GET-STR-REG: a (ctx) → str-reg continuation producing the string register.
Emits instructions and returns RESULT-REG."
  (cond
    ((null dest-sym)
     ;; (format nil ...) — return the string directly
     (let ((str-reg (funcall get-str-reg ctx)))
       (emit ctx (make-vm-move :dst result-reg :src str-reg))))
    ((eq dest-sym t)
     ;; (format t ...) — print to stdout, return nil
     (let ((str-reg (funcall get-str-reg ctx)))
       (emit ctx (make-vm-princ :src str-reg))
       (emit ctx (make-vm-const :dst result-reg :value nil))))
    (t
     ;; (format stream ...) — write to named stream, return nil
     (let ((str-reg (funcall get-str-reg ctx)))
       (emit ctx (make-vm-stream-write-string-inst :stream-reg stream-reg :src str-reg))
       (emit ctx (make-vm-const :dst result-reg :value nil)))))
   result-reg)

(defun %parse-static-format-string (control)
  "Parse a small, safe subset of static FORMAT CONTROL strings.
Returns a list of pieces or NIL when CONTROL contains a directive that must
fall back to the runtime FORMAT instruction. Supported directives are ~A, ~S,
~%, and ~~ without parameters, colon modifiers, or at-sign modifiers."
  (let ((pieces nil)
        (start 0)
        (len (length control)))
    (labels ((push-literal (end)
               (when (< start end)
                 (push (list :literal (subseq control start end)) pieces))))
      (block unsupported
        (loop with i = 0
              while (< i len)
              do (if (char= (char control i) #\~)
                     (progn
                       (push-literal i)
                       (when (>= (1+ i) len)
                         (return-from unsupported nil))
                       (let ((directive (char control (1+ i))))
                         (case (char-upcase directive)
                           (#\A (push '(:arg :a) pieces))
                           (#\S (push '(:arg :s) pieces))
                           (#\% (push (list :literal (string #\Newline)) pieces))
                           (#\~ (push '(:literal "~") pieces))
                           (otherwise (return-from unsupported nil))))
                       (incf i 2)
                       (setf start i))
                     (incf i)))
        (push-literal len)
        (nreverse pieces)))))

(defun %static-format-required-args (pieces)
  (count :arg pieces :key #'first))

(defun %emit-static-format-plan (pieces arg-regs result-reg ctx)
  "Emit VM instructions for parsed static FORMAT PIECES.
Returns RESULT-REG on success, or NIL when ARG-REGS cannot satisfy PIECES."
  (when (<= (%static-format-required-args pieces) (length arg-regs))
    (let ((piece-regs nil)
          (arg-index 0))
      (dolist (piece pieces)
        (let ((piece-reg (make-register ctx)))
          (case (first piece)
            (:literal
             (emit ctx (make-vm-const :dst piece-reg :value (second piece))))
            (:arg
             (let ((src-reg (nth arg-index arg-regs)))
               (incf arg-index)
               (case (second piece)
                 (:a (emit ctx (make-vm-princ-to-string-inst :dst piece-reg :src src-reg)))
                 (:s (emit ctx (make-vm-write-to-string-inst :dst piece-reg :src src-reg)))))))
          (push piece-reg piece-regs)))
      (let ((regs (nreverse piece-regs)))
        (cond
          ((null regs)
           (emit ctx (make-vm-const :dst result-reg :value "")))
          ((null (cdr regs))
           (emit ctx (make-vm-move :dst result-reg :src (car regs))))
          (t
           (emit ctx (make-vm-concatenate :dst result-reg :parts regs)))))
      result-reg)))

(define-phase2-handler "FORMAT" (args result-reg ctx)
  (when (>= (length args) 2)
    (let* ((dest-arg        (first args))
           (fmt-arg         (second args))
           (format-arg-regs nil)
           (dest-sym        :stream)
           (stream-reg      nil))
      (if (typep dest-arg 'ast-var)
          (let ((name (ast-var-name dest-arg)))
            (when (or (null name) (eq name t))
              (setf dest-sym name)))
          (when (typep dest-arg 'ast-quote)
            (let ((value (ast-quote-value dest-arg)))
              (when (or (null value) (eq value t))
                (setf dest-sym value)))))
      ;; Preserve Common Lisp's left-to-right argument evaluation order:
      ;; destination, control string, then format arguments. NIL/T destinations
      ;; are constants in this AST and require no emitted evaluation; stream
      ;; destinations must be evaluated before the formatted string is built.
      (when (eq dest-sym :stream)
        (setf stream-reg (compile-ast dest-arg ctx)))
      (let* ((pieces (and (typep fmt-arg 'ast-quote)
                          (stringp (ast-quote-value fmt-arg))
                          (%parse-static-format-string (ast-quote-value fmt-arg))))
             (static-format-p (and pieces
                                   (<= (%static-format-required-args pieces)
                                       (length (cddr args)))))
             (fmt-reg (unless static-format-p
                        (compile-ast fmt-arg ctx))))
        (setf format-arg-regs
              (mapcar (lambda (arg) (compile-ast arg ctx)) (cddr args)))
        (if static-format-p
            (progn
              (%emit-format-result-to-dest
                 dest-sym stream-reg
                 (lambda (ctx)
                   (let ((str-reg (make-register ctx)))
                     (%emit-static-format-plan pieces format-arg-regs str-reg ctx)
                     str-reg))
                 result-reg ctx))
            ;; Dynamic, unsupported static, or insufficient-args format — call vm-format-inst.
            (%emit-format-result-to-dest
             dest-sym stream-reg
             (lambda (ctx) (let ((str-reg (make-register ctx)))
                             (emit ctx (make-vm-format-inst :dst str-reg :fmt fmt-reg
                                                            :arg-regs format-arg-regs))
                             str-reg))
             result-reg ctx)))
      result-reg)))

;; open: parse selected keyword args used by the VM-backed file stream.
(define-phase2-handler "OPEN" (args result-reg ctx)
  ;; Parse :direction, :if-exists, :if-does-not-exist, :external-format,
  ;; and :element-type for the VM-backed stream runtime.
  (flet ((keyword-ast-value (ast)
           (if (and (typep ast 'ast-var) (keywordp (ast-var-name ast)))
               (ast-var-name ast)
               (if (and (typep ast 'ast-quote) (keywordp (ast-quote-value ast)))
                   (ast-quote-value ast)
                   nil))))
    (let* ((path-reg  (compile-ast (first args) ctx))
            (direction :input)
            (if-exists :supersede)
             (if-not-exists nil)
             (external-format nil)
             (external-format-reg nil)
             (element-type nil)
             (element-type-reg nil))
      (loop for kv on (cdr args) by #'cddr
             when (cdr kv)
               do (let* ((key (car kv))
                         (val (cadr kv))
                         (k   (keyword-ast-value key)))
                   (when k
                     (let ((v (keyword-ast-value val)))
                        (cond
                          ((eq k :direction)         (setf direction    (or v :input)))
                          ((eq k :if-exists)         (setf if-exists    (or v :supersede)))
                          ((eq k :if-does-not-exist) (setf if-not-exists v))
                           ((eq k :external-format)
                            (if v
                                (setf external-format v)
                                (setf external-format-reg (compile-ast val ctx))))
                           ((eq k :element-type)
                            (if v
                                (setf element-type v)
                                (setf element-type-reg (compile-ast val ctx))))
                           (t (compile-ast val ctx)))))))
    ;; if-not-exists default: :create for output, :error for input (computed in execute-instruction)
      (emit ctx (make-vm-open-file :dst result-reg :path path-reg :direction direction
                                    :if-exists if-exists :if-not-exists if-not-exists
                                    :external-format external-format
                                    :external-format-reg external-format-reg
                                    :element-type element-type
                                    :element-type-reg element-type-reg))
      result-reg)))

;; close: (close stream &key abort) — :abort accepted but ignored (FR-589)
(define-phase2-handler "CLOSE" (args result-reg ctx)
  (when args
    (let ((handle-reg (compile-ast (first args) ctx)))
      ;; Compile and discard :abort keyword arg value
      (loop for kv on (cdr args) by #'cddr
            when (and (cdr kv)
                      (typep (car kv) 'ast-var)
                      (keywordp (ast-var-name (car kv))))
              do (compile-ast (cadr kv) ctx))
      (emit ctx (make-vm-close-file :handle handle-reg))
      ;; close returns t per ANSI CL
      (emit ctx (make-vm-const :dst result-reg :value t))
      result-reg)))

;; make-pathname: keyword constructor lowered to the VM pathname instruction.
;; Only compile supplied component values; omitted keywords remain NIL slots so
;; the VM executor does not pass those arguments to CL:MAKE-PATHNAME.
(defun %keyword-ast-value (ast)
  "Return keyword represented by AST, or NIL when AST is not a literal keyword."
  (cond
    ((and (typep ast 'ast-var) (keywordp (ast-var-name ast)))
     (ast-var-name ast))
    ((and (typep ast 'ast-quote) (keywordp (ast-quote-value ast)))
     (ast-quote-value ast))
    (t nil)))

(define-phase2-handler "MAKE-PATHNAME" (args result-reg ctx)
  (let ((host-reg nil)
        (device-reg nil)
        (directory-reg nil)
        (name-reg nil)
        (type-reg nil)
        (version-reg nil)
        (defaults-reg nil))
    (loop for kv on args by #'cddr
          while (cdr kv)
          do (let ((key (%keyword-ast-value (car kv)))
                   (value (cadr kv)))
               (case key
                 (:host (setf host-reg (compile-ast value ctx)))
                 (:device (setf device-reg (compile-ast value ctx)))
                 (:directory (setf directory-reg (compile-ast value ctx)))
                 (:name (setf name-reg (compile-ast value ctx)))
                 (:type (setf type-reg (compile-ast value ctx)))
                 (:version (setf version-reg (compile-ast value ctx)))
                 (:defaults (setf defaults-reg (compile-ast value ctx)))
                 (otherwise
                  ;; Preserve side effects in unknown keyword values (for
                  ;; example :case), but leave unsupported slots to host defaults.
                  (compile-ast value ctx)))))
    (emit ctx (make-vm-make-pathname :dst result-reg
                                     :host-reg host-reg
                                     :device-reg device-reg
                                     :directory-reg directory-reg
                                     :name-reg name-reg
                                     :type-reg type-reg
                                     :version-reg version-reg
                                     :defaults-reg defaults-reg))
    result-reg))

(defun %quoted-string-ast-list-p (args)
  (if (consp args)
      (let ((arg (car args)))
        (if (typep arg 'ast-quote)
            (if (stringp (ast-quote-value arg))
                (%quoted-string-ast-list-p (cdr args))
                nil)
            nil))
      t))

(defun %concatenate-quoted-string-asts (args acc)
  (if (consp args)
      (%concatenate-quoted-string-asts
       (cdr args)
       (concatenate 'string acc (ast-quote-value (car args))))
      acc))

;; concatenate: fold literal string chains; otherwise lower to vm-concatenate
(define-phase2-handler "CONCATENATE" (args result-reg ctx)
  (if (and (>= (length args) 3)
           (typep (first args) 'ast-quote)
           (equal (symbol-name (ast-quote-value (first args))) "STRING"))
      (let ((string-args (cdr args)))
        (if (%quoted-string-ast-list-p string-args)
            (progn
              (emit ctx (make-vm-const :dst result-reg
                                        :value (%concatenate-quoted-string-asts string-args "")))
              result-reg)
            (let ((current-reg (compile-ast (second args) ctx)))
              (loop for rest on (cddr args)
                    do (let* ((next-reg (compile-ast (car rest) ctx))
                              (dst-reg  (if (cdr rest) (make-register ctx) result-reg)))
                         (emit ctx (make-vm-concatenate :dst dst-reg :str1 current-reg :str2 next-reg))
                         (setf current-reg dst-reg)))
              result-reg)))
      nil))
