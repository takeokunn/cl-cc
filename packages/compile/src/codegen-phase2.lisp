(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Phase 2: AST-Introspecting Builtin Handlers
;;;
;;; Builtins in this file cannot be handled by the data-driven Phase 1
;;; *builtin-registry* because they need to inspect AST node types at
;;; compile time.
;;;
;;; The stream/reader/printer, hash-table, slot-predicate, and string-keyword
;;; clusters live in dedicated files so this file can stay focused on the
;;; remaining array / numeric / control handlers.
;;;
;;; Load order: after codegen-functions, before codegen.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar *phase2-builtin-handlers* (make-hash-table :test 'equal)
  "Maps uppercase builtin name strings to Phase 2 handler functions.
Handler lambda-list: (args result-reg ctx) → result-reg-or-nil.")

(defmacro define-phase2-handler (name (args result-reg ctx) &body body)
  "Register a Phase 2 handler for the builtin named NAME (uppercase string)."
  (list 'setf
        (list 'gethash name '*phase2-builtin-handlers*)
        (cons 'lambda (cons (list args result-reg ctx) body))))

(defun %phase2-keyword-name (node)
  "Return NODE's keyword value when NODE is a keyword AST, otherwise NIL."
  (cond
    ((and (typep node 'ast-var) (keywordp (ast-var-name node)))
     (ast-var-name node))
    ((and (typep node 'ast-quote) (keywordp (ast-quote-value node)))
     (ast-quote-value node))
    (t nil)))

(defun %phase2-static-value (node)
  "Return NODE's literal value and whether it is statically known."
  (cond
    ((typep node 'ast-int)
     (values (ast-int-value node) t))
    ((typep node 'ast-quote)
     (values (ast-quote-value node) t))
    ((typep node 'ast-var)
     (let ((name (ast-var-name node)))
       (cond
         ((eq name t) (values t t))
         ((eq name nil) (values nil t))
         ((keywordp name) (values name t))
         (t (values nil nil)))))
    (t (values nil nil))))

(defun %phase2-find-keyword-arg (tail key)
  "Return the value AST following KEY in keyword argument TAIL."
  (tagbody
   scan
     (if (or (null tail) (null (cdr tail)))
         (return-from %phase2-find-keyword-arg nil))
     (if (eq (%phase2-keyword-name (car tail)) key)
         (return-from %phase2-find-keyword-arg (cadr tail)))
     (setq tail (cddr tail))
     (go scan)))

(defun %phase2-static-keyword-value (tail key default)
  "Return KEY's static value from TAIL and whether it is known.
An absent keyword is known and yields DEFAULT; a present non-static value is
unknown so callers can fall through instead of silently changing semantics."
  (let ((node (%phase2-find-keyword-arg tail key)))
    (if node
        (multiple-value-bind (value known-p) (%phase2-static-value node)
          (values (if known-p value default) known-p))
        (values default t))))

(defun %phase2-emit-initial-contents (ctx array-reg contents)
  "Emit ASET instructions that copy literal CONTENTS into ARRAY-REG."
  (let ((index 0)
        (tail contents))
    (tagbody
     scan
       (if (null tail) (go done))
       (let ((idx-reg (make-register ctx))
             (val-reg (make-register ctx)))
         (emit ctx (make-vm-const :dst idx-reg :value index))
         (emit ctx (make-vm-const :dst val-reg :value (car tail)))
         (emit ctx (make-vm-aset :array-reg array-reg
                                 :index-reg idx-reg
                                 :val-reg val-reg)))
       (setq index (+ index 1))
       (setq tail (cdr tail))
       (go scan)
     done))
  array-reg)

;;; ── Handler registrations ──────────────────────────────────────────────────

;; make-array: fixed-size array
(define-phase2-handler "MAKE-ARRAY" (args result-reg ctx)
  (let* ((tail (rest args))
          (init-ast (%phase2-find-keyword-arg tail :initial-element))
          (contents-ast (%phase2-find-keyword-arg tail :initial-contents)))
    (multiple-value-bind (fill-pointer fill-pointer-known-p)
        (%phase2-static-keyword-value tail :fill-pointer nil)
      (multiple-value-bind (adjustable adjustable-known-p)
          (%phase2-static-keyword-value tail :adjustable nil)
        (when (and fill-pointer-known-p adjustable-known-p
                   (or (null contents-ast) (typep contents-ast 'ast-quote)))
          (let ((contents (and contents-ast (ast-quote-value contents-ast))))
            (when (or (null contents-ast) (listp contents))
              (let ((size-reg (compile-ast (first args) ctx))
                    (init-reg (and init-ast (compile-ast init-ast ctx))))
                (emit ctx (make-vm-make-array :dst result-reg :size-reg size-reg
                                              :initial-element init-reg
                                              :fill-pointer fill-pointer
                                              :adjustable adjustable))
                (when contents-ast
                  (%phase2-emit-initial-contents ctx result-reg contents))
                result-reg))))))))

;; make-adjustable-vector: array with fill-pointer
(define-phase2-handler "MAKE-ADJUSTABLE-VECTOR" (args result-reg ctx)
  (let ((size-reg (compile-ast (first args) ctx)))
    (emit ctx (make-vm-make-array :dst result-reg :size-reg size-reg
                                  :fill-pointer t :adjustable t))
    result-reg))

;; rt-slot-set: bootstrap/runtime helper lowered directly to slot-write.
(define-phase2-handler "RT-SLOT-SET" (args result-reg ctx)
  (when (and (= (length args) 3) (typep (second args) 'ast-quote))
    (let ((obj-reg (compile-ast (first args) ctx))
          (value-reg (compile-ast (third args) ctx))
          (slot-name (ast-quote-value (second args))))
      (emit ctx (make-vm-slot-write :obj-reg obj-reg
                                    :slot-name slot-name
                                    :value-reg value-reg))
      (if (eq result-reg value-reg)
          value-reg
          (progn
            (emit ctx (make-vm-move :dst result-reg :src value-reg))
            result-reg)))))

;; array-row-major-index: variadic subscripts → cons-chain
(define-phase2-handler "ARRAY-ROW-MAJOR-INDEX" (args result-reg ctx)
  (when (>= (length args) 1)
    (let ((arr-reg  (compile-ast (first args) ctx))
          (subs-reg (make-register ctx)))
      (emit ctx (make-vm-const :dst subs-reg :value nil))
      (dolist (sub (reverse (rest args)))
        (let ((sub-reg (compile-ast sub ctx))
              (new-reg (make-register ctx)))
          (emit ctx (make-vm-cons :dst new-reg :car-src sub-reg :cdr-src subs-reg))
          (setf subs-reg new-reg)))
      (emit ctx (make-vm-array-row-major-index :dst result-reg :arr arr-reg :subs subs-reg))
      result-reg)))

;; list: variadic constructor → cons chain ending in NIL
(define-phase2-handler "LIST" (args result-reg ctx)
  (emit ctx (make-vm-const :dst result-reg :value nil))
  (dolist (arg (reverse args))
    (let ((arg-reg (compile-ast arg ctx))
          (new-reg (make-register ctx)))
      (emit ctx (make-vm-cons :dst new-reg :car-src arg-reg :cdr-src result-reg))
      (setf result-reg new-reg)))
  result-reg)

;; encode-universal-time: 6 or 7 args → cons-list → single instruction
(define-phase2-handler "ENCODE-UNIVERSAL-TIME" (args result-reg ctx)
  (when (and (>= (length args) 6) (<= (length args) 7))
    (let ((arg-regs (mapcar (lambda (a) (compile-ast a ctx)) args))
          (list-reg (make-register ctx)))
      (emit ctx (make-vm-const :dst list-reg :value nil))
      (dolist (r (reverse arg-regs))
        (let ((new-reg (make-register ctx)))
          (emit ctx (make-vm-cons :dst new-reg :car-src r :cdr-src list-reg))
          (setf list-reg new-reg)))
      (emit ctx (make-vm-encode-universal-time :dst result-reg :args-reg list-reg))
      result-reg)))

;; make-string: optional :initial-element and/or :element-type keywords
;; :element-type is accepted but ignored (we only support character strings)
(define-phase2-handler "MAKE-STRING" (args result-reg ctx)
  (let ((size-reg (compile-ast (first args) ctx))
        (init-char-ast nil)
        (tail (rest args)))
    (tagbody
     scan-keywords
       (when (or (null tail) (null (cdr tail)))
         (go scan-done))
       (let ((k (car tail))
             (v (cadr tail)))
         (when (and (typep k 'ast-var)
                    (eq (ast-var-name k) :initial-element))
           (setf init-char-ast v)
           (go scan-done)))
       (setf tail (cddr tail))
       (go scan-keywords)
     scan-done)
    (if init-char-ast
        (let ((char-reg (compile-ast init-char-ast ctx)))
          (emit ctx (make-vm-make-string :dst result-reg :src size-reg :char char-reg)))
        (emit ctx (make-vm-make-string :dst result-reg :src size-reg)))
    result-reg))

;; FR-604: float 2-arg form — (float number prototype) drops prototype (all nums are double)
(define-phase2-handler "FLOAT" (args result-reg ctx)
  (when (= (length args) 2)
    ;; Evaluate and discard prototype (for side effects); convert number to float
    (compile-ast (second args) ctx)
    (let ((num-reg (compile-ast (first args) ctx)))
      (emit ctx (make-vm-float-inst :dst result-reg :src num-reg))
      result-reg)))

;; typep: fast path when type is a quoted symbol
(define-phase2-handler "TYPEP" (args result-reg ctx)
  (when (and (= (length args) 2) (typep (second args) 'ast-quote))
    (let ((val-reg  (compile-ast (first args) ctx))
          (type-sym (ast-quote-value (second args))))
      (emit ctx (make-vm-typep :dst result-reg :src val-reg :type-name type-sym))
      result-reg)))

;; call-next-method: 0 args or variadic args → cons-list
(define-phase2-handler "CALL-NEXT-METHOD" (args result-reg ctx)
  (let ((args-reg (when args
                    (let* ((arg-regs (mapcar (lambda (a) (compile-ast a ctx)) args))
                           (list-reg (make-register ctx)))
                      (emit ctx (make-vm-const :dst list-reg :value nil))
                      (dolist (r (reverse arg-regs))
                        (let ((new-reg (make-register ctx)))
                          (emit ctx (make-vm-cons :dst new-reg :car-src r :cdr-src list-reg))
                          (setf list-reg new-reg)))
                      list-reg))))
    (emit ctx (make-vm-call-next-method :dst result-reg :args-reg args-reg))
    result-reg))

;; set-fdefinition: register a compiled closure under a symbol name and return the closure
(define-phase2-handler "SET-FDEFINITION" (args result-reg ctx)
  (when (= (length args) 2)
    (let* ((fn-reg (compile-ast (first args) ctx))
           (name-ast (second args))
           (name (cond
                   ((typep name-ast 'ast-quote)
                    (ast-quote-value name-ast))
                   ((typep name-ast 'ast-var)
                    (ast-var-name name-ast))
                   (t nil))))
      (when (symbolp name)
        (emit ctx (make-vm-register-function :name name :src fn-reg))
        (if (eq result-reg fn-reg)
            fn-reg
            (progn
              (emit ctx (make-vm-move :dst result-reg :src fn-reg))
              result-reg))))))
