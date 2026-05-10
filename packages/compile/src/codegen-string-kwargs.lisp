(in-package :cl-cc/compile)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — string keyword-argument handlers
;;;
;;; Loaded after codegen-phase2 so define-phase2-handler is available, and
;;; before codegen so the registration table is complete.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; FR-637: string comparisons with :start1/:end1/:start2/:end2 keyword args
;; When args > 2 (keywords present), Phase 1 arity check rejects them and falls here.
;; Strategy: use vm-subseq to extract substrings, then emit the 2-arg comparison.

(defun %compile-maybe-subseq (str-ast start-ast end-ast ctx)
  "Compile str-ast, wrapping in vm-subseq when start-ast or end-ast is non-nil."
  (let ((str-reg (compile-ast str-ast ctx)))
    (if (or start-ast end-ast)
        (let* ((start-reg (if start-ast
                              (compile-ast start-ast ctx)
                              (let ((r (make-register ctx)))
                                (emit ctx (make-vm-const :dst r :value 0))
                                r)))
               (end-reg (when end-ast (compile-ast end-ast ctx)))
               (sub-reg (make-register ctx)))
          (emit ctx (make-vm-subseq :dst sub-reg :string str-reg
                                    :start start-reg :end end-reg))
          sub-reg)
        str-reg)))

(defun %parse-string-cmp-kwargs (args)
  "Return (values str1 str2 start1 end1 start2 end2) from an args list with keywords."
  (let (start1 end1 start2 end2)
    (loop for kv on (cddr args) by #'cddr
          while (cdr kv)
          when (and (typep (car kv) 'ast-var) (keywordp (ast-var-name (car kv))))
            do (let ((name (ast-var-name (car kv))) (val (cadr kv)))
                 (cond ((eq name :start1) (setf start1 val))
                       ((eq name :end1)   (setf end1   val))
                       ((eq name :start2) (setf start2 val))
                       ((eq name :end2)   (setf end2   val)))))
    (values (first args) (second args) start1 end1 start2 end2)))

(defun %register-string-cmp-handler (upper-name ctor)
  (setf (gethash upper-name *phase2-builtin-handlers*)
        (lambda (args result-reg ctx)
          (when (>= (length args) 2)
            (multiple-value-bind (str1 str2 start1 end1 start2 end2)
                (%parse-string-cmp-kwargs args)
              (let ((r1 (%compile-maybe-subseq str1 start1 end1 ctx))
                    (r2 (%compile-maybe-subseq str2 start2 end2 ctx)))
                (emit ctx (funcall (symbol-function ctor)
                                   :dst result-reg :str1 r1 :str2 r2))
                result-reg))))))

(dolist (entry '(("STRING="      . make-vm-string=)
                 ("STRING<"      . make-vm-string<)
                 ("STRING>"      . make-vm-string>)
                 ("STRING<="     . make-vm-string<=)
                 ("STRING>="     . make-vm-string>=)
                 ("STRING-EQUAL" . make-vm-string-equal)))
  (%register-string-cmp-handler (car entry) (cdr entry)))

;; FR-627: string-upcase/downcase/capitalize with :start/:end keyword args
;; When keywords present, Phase 1 rejects (arity mismatch) and falls here.
;; ANSI semantics: only the substring [start, end) is changed; prefix and suffix stay intact.
;; Strategy: concatenate prefix + case(slice) + suffix.

(defun %parse-string-case-kwargs (args)
  "Return (values str-ast start-ast end-ast) from args with keywords."
  (let (start end)
    (loop for kv on (cdr args) by #'cddr
          while (cdr kv)
          when (and (typep (car kv) 'ast-var) (keywordp (ast-var-name (car kv))))
            do (let ((name (ast-var-name (car kv))) (val (cadr kv)))
                 (cond ((eq name :start) (setf start val))
                       ((eq name :end)   (setf end   val)))))
    (values (first args) start end)))

(defun %register-string-case-handler (upper-name ctor)
  (setf (gethash upper-name *phase2-builtin-handlers*)
        (lambda (args result-reg ctx)
          (when args
            (multiple-value-bind (str-ast start-ast end-ast)
                (%parse-string-case-kwargs args)
              (if (or start-ast end-ast)
                  (let* ((str-reg   (compile-ast str-ast ctx))
                         (start-reg (if start-ast
                                        (compile-ast start-ast ctx)
                                        (let ((r (make-register ctx)))
                                          (emit ctx (make-vm-const :dst r :value 0))
                                          r)))
                         (len-reg   (make-register ctx))
                         (end-reg   (progn
                                      (emit ctx (make-vm-length :dst len-reg :src str-reg))
                                      (if end-ast (compile-ast end-ast ctx) len-reg)))
                         (zero-reg  (let ((r (make-register ctx)))
                                      (emit ctx (make-vm-const :dst r :value 0))
                                      r))
                         (pre-reg   (make-register ctx))
                         (slice-reg (make-register ctx))
                         (suf-reg   (make-register ctx))
                         (case-reg  (make-register ctx))
                         (tmp-reg   (make-register ctx)))
                    (emit ctx (make-vm-subseq :dst pre-reg :string str-reg
                                              :start zero-reg :end start-reg))
                    (emit ctx (make-vm-subseq :dst slice-reg :string str-reg
                                              :start start-reg :end end-reg))
                    (emit ctx (make-vm-subseq :dst suf-reg :string str-reg
                                              :start end-reg :end len-reg))
                    (emit ctx (funcall (symbol-function ctor) :dst case-reg :src slice-reg))
                    (emit ctx (make-vm-concatenate :dst tmp-reg :str1 pre-reg :str2 case-reg))
                    (emit ctx (make-vm-concatenate :dst result-reg :str1 tmp-reg :str2 suf-reg))
                    result-reg)
                  (let ((str-reg (compile-ast str-ast ctx)))
                    (emit ctx (funcall (symbol-function ctor) :dst result-reg :src str-reg))
                    result-reg)))))))

(dolist (entry '(("STRING-UPCASE"      . make-vm-string-upcase)
                 ("STRING-DOWNCASE"    . make-vm-string-downcase)
                 ("STRING-CAPITALIZE"  . make-vm-string-capitalize)
                 ("NSTRING-UPCASE"     . make-vm-nstring-upcase)
                 ("NSTRING-DOWNCASE"   . make-vm-nstring-downcase)
                 ("NSTRING-CAPITALIZE" . make-vm-nstring-capitalize)))
  (%register-string-case-handler (car entry) (cdr entry)))
