(in-package :cl-cc)

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
    (loop for (key val) on (cddr args) by #'cddr
          when (and (typep key 'ast-var) (keywordp (ast-var-name key)))
            do (case (ast-var-name key)
                 (:start1 (setf start1 val))
                 (:end1   (setf end1   val))
                 (:start2 (setf start2 val))
                 (:end2   (setf end2   val))))
    (values (first args) (second args) start1 end1 start2 end2)))

(defmacro define-string-cmp-handler (upper-name ctor)
  `(define-phase2-handler ,upper-name (args result-reg ctx)
     (when (>= (length args) 2)
       (multiple-value-bind (str1 str2 start1 end1 start2 end2)
           (%parse-string-cmp-kwargs args)
         (let ((r1 (%compile-maybe-subseq str1 start1 end1 ctx))
               (r2 (%compile-maybe-subseq str2 start2 end2 ctx)))
           (emit ctx (,ctor :dst result-reg :str1 r1 :str2 r2))
           result-reg)))))

(define-string-cmp-handler "STRING="       make-vm-string=)
(define-string-cmp-handler "STRING<"       make-vm-string<)
(define-string-cmp-handler "STRING>"       make-vm-string>)
(define-string-cmp-handler "STRING<="      make-vm-string<=)
(define-string-cmp-handler "STRING>="      make-vm-string>=)
(define-string-cmp-handler "STRING-EQUAL"  make-vm-string-equal)

;; FR-627: string-upcase/downcase/capitalize with :start/:end keyword args
;; When keywords present, Phase 1 rejects (arity mismatch) and falls here.
;; ANSI semantics: only the substring [start, end) is changed; prefix and suffix stay intact.
;; Strategy: concatenate prefix + case(slice) + suffix.

(defun %parse-string-case-kwargs (args)
  "Return (values str-ast start-ast end-ast) from args with keywords."
  (let (start end)
    (loop for (key val) on (cdr args) by #'cddr
          when (and (typep key 'ast-var) (keywordp (ast-var-name key)))
            do (case (ast-var-name key)
                 (:start (setf start val))
                 (:end   (setf end   val))))
    (values (first args) start end)))

(defmacro define-string-case-handler (upper-name ctor)
  `(define-phase2-handler ,upper-name (args result-reg ctx)
     (when args
       (multiple-value-bind (str-ast start-ast end-ast)
           (%parse-string-case-kwargs args)
         (if (or start-ast end-ast)
             ;; Keyword-arg form: reconstruct full string = prefix + case(slice) + suffix
             ;; (ANSI CL: only the [start,end) region is modified)
             (let* ((str-reg    (compile-ast str-ast ctx))
                    ;; start register (default 0)
                    (start-reg  (if start-ast
                                    (compile-ast start-ast ctx)
                                    (let ((r (make-register ctx)))
                                      (emit ctx (make-vm-const :dst r :value 0)) r)))
                    ;; end register (default length of string)
                    (len-reg    (make-register ctx))
                    (end-reg    (progn
                                  (emit ctx (make-vm-length :dst len-reg :src str-reg))
                                  (if end-ast (compile-ast end-ast ctx) len-reg)))
                    ;; prefix = str[0..start)
                    (zero-reg   (let ((r (make-register ctx)))
                                  (emit ctx (make-vm-const :dst r :value 0)) r))
                    (pre-reg    (make-register ctx))
                    ;; slice = str[start..end)
                    (slice-reg  (make-register ctx))
                    ;; suffix = str[end..length)
                    (suf-reg    (make-register ctx))
                    (case-reg   (make-register ctx))
                    (tmp-reg    (make-register ctx)))
               (emit ctx (make-vm-subseq :dst pre-reg   :string str-reg
                                         :start zero-reg :end start-reg))
               (emit ctx (make-vm-subseq :dst slice-reg  :string str-reg
                                         :start start-reg :end end-reg))
               (emit ctx (make-vm-subseq :dst suf-reg    :string str-reg
                                         :start end-reg   :end len-reg))
               (emit ctx (,ctor :dst case-reg :src slice-reg))
               (emit ctx (make-vm-concatenate :dst tmp-reg      :str1 pre-reg  :str2 case-reg))
               (emit ctx (make-vm-concatenate :dst result-reg   :str1 tmp-reg  :str2 suf-reg))
               result-reg)
             ;; No keyword args: simple 1-arg case op
             (let ((str-reg (compile-ast str-ast ctx)))
               (emit ctx (,ctor :dst result-reg :src str-reg))
               result-reg))))))

(define-string-case-handler "STRING-UPCASE"     make-vm-string-upcase)
(define-string-case-handler "STRING-DOWNCASE"   make-vm-string-downcase)
(define-string-case-handler "STRING-CAPITALIZE" make-vm-string-capitalize)
(define-string-case-handler "NSTRING-UPCASE"    make-vm-nstring-upcase)
(define-string-case-handler "NSTRING-DOWNCASE"  make-vm-nstring-downcase)
(define-string-case-handler "NSTRING-CAPITALIZE" make-vm-nstring-capitalize)
