;;;; parser.lisp - Type Annotation Parser
;;;;
;;;; Parses CL-style type annotation s-expressions into type-node trees.
;;;;
;;;; Supported syntax:
;;;;   fixnum, string, boolean, symbol, etc.     primitive types
;;;;   (or T1 T2 ...)                            union
;;;;   (and T1 T2 ...)                           intersection
;;;;   (function (PARAMS...) RETURN)             arrow (pure, ω)
;;;;   (-> A B)                                  pure arrow (ω)
;;;;   (->1 A B)                                 linear arrow (1)
;;;;   (->0 A B)                                 erased arrow (0)
;;;;   (-> A B ! IO State)                       effectful arrow
;;;;   (-> A B / <IO | ε>)                       effectful arrow (alt)
;;;;   (values T1 T2 ...)                        product / tuple
;;;;   (list T)                                  List T  (type-app)
;;;;   (forall a T)  or  (∀ a T)                universal quantification
;;;;   (exists a T)                              existential quantification
;;;;   (mu a T)                                  recursive type μa.T
;;;;   (=> (C a) T)                              qualified type
;;;;   (Refine T pred)                           refinement type
;;;;   (Record (l1 T1) ... | ρ)                  row-polymorphic record
;;;;   (Variant (L1 T1) ... | ρ)                 row-polymorphic variant
;;;;   (! 1 T)  /  (!1 T)  /  (!ω T)            graded modal types

(in-package :cl-cc/type)

;;; ─── Conditions ───────────────────────────────────────────────────────────

(define-condition type-parse-error (error)
  ((message :initarg :message :reader type-parse-error-message)
   (source  :initarg :source  :initform nil :reader type-parse-error-source))
  (:report (lambda (c s)
             (format s "Type parse error: ~A" (type-parse-error-message c)))))

(defun type-parse-error (msg &rest args)
  (error 'type-parse-error :message (apply #'format nil msg args)))

;;; ─── Main entry ───────────────────────────────────────────────────────────

(defun parse-type-specifier (spec)
  "Parse SPEC into a type-node."
  (typecase spec
    (null   type-null)
    (symbol (if (member (symbol-name spec) '("?" "_") :test #'string=)
                (make-type-error :message "unknown")   ; ? / _ → gradual typing hole
                (parse-primitive-type spec)))
    (cons   (parse-compound-type spec))
    (t      (type-parse-error "Invalid type specifier: ~S" spec))))

;;; ─── Primitive types ──────────────────────────────────────────────────────
;;; Data table maps groups of equivalent name strings to their type constants.
;;; Package-independent: all comparisons use string= (symbol-name is always uppercase).

(defvar *primitive-type-name-table*
  `((("FIXNUM" "INTEGER" "INT")              . ,type-int)
    (("FLOAT" "SINGLE-FLOAT" "DOUBLE-FLOAT") . ,type-float)
    (("STRING" "SIMPLE-STRING")              . ,type-string)
    (("BOOLEAN" "BOOL")                      . ,type-bool)
    (("SYMBOL")                              . ,type-symbol)
    (("CONS")                                . ,type-cons)
    (("NULL" "NIL")                          . ,type-null)
    (("T" "TOP")                             . ,type-any)
    (("CHARACTER" "CHAR")                    . ,type-char))
  "Alist of (name-strings . type-constant) for primitive type lookup.")

(defun parse-primitive-type (name)
  "Parse a symbol into a primitive type. Dispatches via *primitive-type-name-table*."
  (let* ((sname (symbol-name name))
         (entry (assoc sname *primitive-type-name-table*
                       :test (lambda (s names) (member s names :test #'string=)))))
    (if entry
        (cdr entry)
        (let ((alias (and (boundp '*type-alias-registry*)
                          (gethash name *type-alias-registry*))))
          (if alias
              (parse-type-specifier alias)
              (make-type-primitive :name name))))))

;;; ─── Compound types ───────────────────────────────────────────────────────

(defun parse-compound-type (spec)
  (let ((head (car spec))
        (args (cdr spec)))
    (case head
      ;; (or T1 T2 ...) — union
      ((or)
       (unless args (type-parse-error "or requires at least one type"))
       (make-type-union (mapcar #'parse-type-specifier args)))

      ;; (and T1 T2 ...) — intersection
      ((and)
       (unless args (type-parse-error "and requires at least one type"))
       (make-type-intersection (mapcar #'parse-type-specifier args)))

      ;; (function (PARAMS...) RETURN) — backward-compat function type
      ((function)
       (unless (and (= (length args) 2) (listp (first args)))
         (type-parse-error "function type: (function (PARAMS...) RETURN)"))
       (make-type-arrow (mapcar #'parse-type-specifier (first args))
                        (parse-type-specifier (second args))))

      ;; (values T1 T2 ...) — product / tuple
      ((values)
       (make-type-product :elems (mapcar #'parse-type-specifier args)))

      ;; (cons T1 T2) — backward compat: cons pair as 2-tuple
      ((cons)
       (unless (= (length args) 2)
         (type-parse-error "cons requires 2 types"))
       (make-type-product :elems (mapcar #'parse-type-specifier args)))

      ;; (list T) → (type-app List T)
      ((list)
       (unless (= (length args) 1)
         (type-parse-error "list requires exactly 1 type"))
       (make-type-app :fun  (make-type-primitive :name 'list)
                      :arg  (parse-type-specifier (first args))))

      ;; (vector T) / (array T)
      ((vector simple-vector)
       (unless (= (length args) 1) (type-parse-error "vector requires 1 type"))
       (make-type-app :fun (make-type-primitive :name 'vector)
                      :arg (parse-type-specifier (first args))))
      ((array simple-array)
        (unless (= (length args) 1) (type-parse-error "array requires 1 type"))
        (make-type-app :fun (make-type-primitive :name 'array)
                       :arg (parse-type-specifier (first args))))

       ;; (option T) → (or null T) — nullable/optional type sugar
       ((option)
        (unless (= (length args) 1)
          (type-parse-error "option requires exactly 1 type"))
        (make-type-union (list type-null (parse-type-specifier (first args)))
                         :constructor-name head))

      (otherwise
        (parse-compound-type-extended head args)))))

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

(defun parse-effect-row-spec (specs)
  "Parse effect labels like (IO) or (IO State | ε) into a type-effect-row."
  ;; specs may be a list of symbols or a single <...> form
  (if (and (= (length specs) 1) (consp (first specs)))
      ;; Angle-bracket form: (<IO State | ε>) — treat as tagged list
      (let* ((inner (first specs))
             (pipe-pos (position-if (lambda (x) (and (symbolp x)
                                                      (string= (symbol-name x) "|")))
                                    inner))
             (eff-names (if pipe-pos (subseq inner 0 pipe-pos) inner))
             (row-var   (when pipe-pos (parse-type-specifier (nth (1+ pipe-pos) inner)))))
        (make-type-effect-row
         :effects (mapcar (lambda (n) (make-type-effect-op :name n)) eff-names)
         :row-var row-var))
      ;; Flat list of names: (IO State | ε)
      (let* ((pipe-pos (position-if (lambda (x) (and (symbolp x)
                                                      (string= (symbol-name x) "|")))
                                    specs))
             (eff-names (if pipe-pos (subseq specs 0 pipe-pos) specs))
             (row-var   (when pipe-pos (parse-type-specifier (nth (1+ pipe-pos) specs)))))
        (make-type-effect-row
         :effects (mapcar (lambda (n) (make-type-effect-op :name n)) eff-names)
         :row-var row-var))))


;;; (parse-row-type, parse-constraint-spec, lambda-list parsing,
;;;  typed AST defstructs, and utilities are in parser-typed.lisp
;;;  which loads after this file.)

