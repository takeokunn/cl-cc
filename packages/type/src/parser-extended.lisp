;;;; parser-extended.lisp - Extended Type Parser (Arrows, Quantifiers, Effects, Row Types)
;;;;
;;;; Handles symbol-named compound forms that require string comparison
;;;; for package-independence: arrow multiplicities, binding quantifiers,
;;;; qualified/refinement/graded-modal types, record/variant row types,
;;;; and the effect-row parser.
;;;;
;;;; Depends on parser.lisp being loaded first (parse-type-specifier,
;;;; parse-compound-type, type-parse-error, type-creation functions).

(in-package :cl-cc/type)

;;; ─── Arrow form multiplicities ────────────────────────────────────────────

(defvar *arrow-mult-table*
  '(("->"  . :omega)
    ("->1" . :one)
    ("->0" . :zero))
  "Maps arrow syntax name strings to their multiplicity keywords.")

;;; ─── Binding quantifiers: data-driven (forall/∀, exists/∃, mu/μ, type-lambda) ──────────
;;; All share identical structure (var body) — only the constructor differs.

(defvar *binding-quantifier-table*
  `((("FORALL"      "∀") . ,#'make-type-forall)
    (("EXISTS"      "∃") . ,#'make-type-exists)
    (("MU"          "μ") . ,#'make-type-mu)
    (("TYPE-LAMBDA")     . ,#'make-type-lambda))
  "Maps lists of equivalent name strings to (var body) → type-node constructors.")

(defun %binding-quantifier-ctor (hn)
  "Return the constructor for quantifier name HN (case-sensitive), or nil."
  (cdr (assoc hn *binding-quantifier-table*
              :test (lambda (h names) (member h names :test #'string=)))))

(defun parse-compound-type-extended (head args)
  "Handle all symbol-named compound forms using string comparison for package-independence."
  (let ((hn (and (symbolp head) (symbol-name head))))
    (cond
       ;; (option T) → (or null T) — package-independent nullable sugar
       ((and hn (string= hn "OPTION"))
        (unless (= (length args) 1)
          (type-parse-error "option requires exactly 1 type"))
        (make-type-union (list type-null (parse-type-specifier (first args)))
                         :constructor-name head))

      ;; ─── Arrow forms (-> :omega, ->1 :one, ->0 :zero) ────────────────
      ((and hn (assoc hn *arrow-mult-table* :test #'string=))
       (parse-arrow-type args (cdr (assoc hn *arrow-mult-table* :test #'string=))))

      ;; ─── Binding quantifiers (forall/∀, exists/∃, mu/μ, type-lambda) ──
      ((and hn (%binding-quantifier-ctor hn))
       (unless (= (length args) 2)
         (type-parse-error "~A requires (~A var body)" hn (string-downcase hn)))
       (let* ((ctor (%binding-quantifier-ctor hn))
              (var  (fresh-type-var (first args)))
              (body (parse-type-specifier (second args))))
         (funcall ctor :var var :body body)))

       ;; ─── Qualified: (=> (C1 a) ... T) ────────────────────────────────
       ((and hn (string= hn "=>"))
        (unless (>= (length args) 2)
          (type-parse-error "=> requires (=> constraint... body)"))
        (let* ((body-spec    (car (last args)))
               (cst-specs    (butlast args))
               (body         (parse-type-specifier body-spec))
               (constraints  (mapcar #'parse-constraint-spec cst-specs)))
          (make-type-qualified :constraints constraints :body body)))

      ;; ─── Refinement: (Refine T pred) ─────────────────────────────────
      ((and hn (string= hn "REFINE"))
       (unless (= (length args) 2)
         (type-parse-error "Refine requires (Refine base-type predicate)"))
       (make-type-refinement :base (parse-type-specifier (first args))
                             :predicate (second args)))

      ;; ─── Graded modal: (! q T) or (!1 T) or (!ω T) / (!Ω T) ────────
      ;; Match any symbol starting with "!" — handles Unicode-uppercased !ω → !Ω
      ((and hn (> (length hn) 0) (char= (char hn 0) #\!))
       (let* ((suffix (subseq hn 1))
              (grade (cond ((string= suffix "1") :one)
                           ((string= suffix "0") :zero)
                           ((string= suffix "")  ; plain "!" form: (! multiplicity T)
                            (case (first args)
                              ((1 :one one)   :one)
                              ((0 :zero zero) :zero)
                              (otherwise      :omega)))
                           (t :omega)))  ; !ω, !Ω, or any other suffix → unrestricted
              (inner-spec (if (string= suffix "") (second args) (first args))))
         (make-type-linear :base (parse-type-specifier inner-spec) :grade grade)))

      ;; ─── Record: (Record (l1 T1) (l2 T2) ... | ρ) ────────────────────
      ((and hn (string= hn "RECORD"))
       (parse-row-type args :record))

      ;; ─── Variant: (Variant (L1 T1) (L2 T2) ... | ρ) ─────────────────
      ((and hn (string= hn "VARIANT"))
       (parse-row-type args :variant))

      ;; ─── ANSI CL function type: (function (A B ...) R) ───────────────
      ((and hn (string= hn "FUNCTION"))
       (parse-cl-function-type args))

      ;; ─── Type application fallback: (F A) ────────────────────────────
      ((symbolp head)
       (let ((fun (parse-type-specifier head)))
         (reduce (lambda (acc arg) (make-type-app :fun acc :arg (parse-type-specifier arg)))
                 args :initial-value fun)))

      (t (type-parse-error "Unrecognised compound type: ~S" (cons head args))))))

;;; ─── Arrow parsing ────────────────────────────────────────────────────────

(defun parse-arrow-type (args mult)
  "Parse (->/->1/->0 PARAMS... RETURN [! EFFECTS...]) or (-> P... / <...>)."
  ;; Check for effect annotation via ! (package-independent: compare by symbol-name)
  (let* ((bang-pos  (position-if (lambda (x) (and (symbolp x)
                                                    (string= (symbol-name x) "!")))
                                  args))
         ;; Check for (/ ...) effect annotation
         (slash-pos (and (null bang-pos)
                         (position-if (lambda (x) (and (symbolp x) (string= (symbol-name x) "/")))
                                      args)))
         (eff-start (or (and bang-pos (1+ bang-pos))
                        (and slash-pos (1+ slash-pos))))
         (main-args (if eff-start (subseq args 0 (1- eff-start)) args))
         (eff-specs  (when eff-start (subseq args eff-start))))
    (unless (>= (length main-args) 2)
      (type-parse-error "Arrow type needs at least one param and a return type"))
    (let* ((param-specs (butlast main-args))
           (return-spec (car (last main-args)))
           (params      (mapcar #'parse-type-specifier param-specs))
           (ret         (parse-type-specifier return-spec))
           (effects     (if eff-specs
                            (parse-effect-row-spec eff-specs)
                            +pure-effect-row+)))
       (make-type-arrow params ret :effects effects :mult mult))))

(defun parse-cl-function-type (args)
  "Parse ANSI CL (function (PARAM...) RETURN) into the internal arrow type."
  (unless (= (length args) 2)
    (type-parse-error "function type requires parameter list and return type"))
  (let ((param-specs (first args))
        (return-spec (second args)))
    (unless (listp param-specs)
      (type-parse-error "function parameter spec must be a list: ~S" param-specs))
    (make-type-arrow (mapcar #'parse-type-specifier param-specs)
                     (parse-type-specifier return-spec))))

(defun %parse-effect-names-and-row-var (elts)
  "Split ELTS at '|' into (effects . row-var-or-nil) and build a type-effect-row."
  (let* ((pipe-pos  (position-if (lambda (x) (and (symbolp x) (string= (symbol-name x) "|")))
                                  elts))
         (eff-names (if pipe-pos (subseq elts 0 pipe-pos) elts))
         (row-var   (when pipe-pos (parse-type-specifier (nth (1+ pipe-pos) elts)))))
    (make-type-effect-row
     :effects (mapcar (lambda (n) (make-type-effect-op :name n)) eff-names)
     :row-var row-var)))

(defun parse-effect-row-spec (specs)
  "Parse effect labels like (IO) or (IO State | ε) into a type-effect-row."
  (if (and (= (length specs) 1) (consp (first specs)))
      ;; Angle-bracket form: (<IO State | ε>)
      (%parse-effect-names-and-row-var (first specs))
      ;; Flat list: (IO State | ε)
      (%parse-effect-names-and-row-var specs)))


;;; (parse-row-type, parse-constraint-spec, lambda-list parsing,
;;;  typed AST defstructs, and utilities are in parser-typed.lisp
;;;  which loads after this file.)
