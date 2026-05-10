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

(defun %bound-operator-kind (op)
  "Map a bounded-polymorphism operator symbol to :UPPER or :LOWER."
  (when (symbolp op)
    (let ((name (symbol-name op)))
      (cond
        ((member name '("<:" "EXTENDS" "SUBTYPE-OF") :test #'string=) :upper)
        ((member name '(">:" "SUPERTYPE-OF") :test #'string=) :lower)
        (t nil)))))

(defun %parse-bounded-quantifier-var (spec)
  "Parse quantifier variable SPEC, including bounds like (a <: number).
Supported bound operators are <:/extends/subtype-of and >:/supertype-of."
  (cond
    ((symbolp spec)
     (fresh-type-var :name spec))
    ((and (consp spec) (symbolp (first spec)))
     (let ((name (first spec))
           (upper nil)
           (lower nil)
           (bounds (rest spec)))
       (loop while bounds
             do (let ((kind (%bound-operator-kind (first bounds))))
                  (unless (and kind (consp (rest bounds)))
                    (type-parse-error "Invalid bounded type variable: ~S" spec))
                  (case kind
                    (:upper (setf upper (parse-type-specifier (second bounds))))
                    (:lower (setf lower (parse-type-specifier (second bounds)))))
                  (setf bounds (cddr bounds))))
       (fresh-type-var :name name :upper-bound upper :lower-bound lower)))
    (t
     (type-parse-error "Quantifier variable must be a symbol or bounded spec: ~S" spec))))

(defun %parse-protocol-name-type (spec)
  "Parse a protocol designator into a primitive name wrapper."
  (unless (symbolp spec)
    (type-parse-error "protocol name must be a symbol, got ~S" spec))
  (make-type-primitive :name spec))

(defun %parser-head-name-member-p (head table)
  "Return T when HEAD's symbol-name matches a symbol key in TABLE."
  (and (symbolp head)
       (assoc (symbol-name head)
              table
              :test (lambda (name key) (string= name (symbol-name key))))))

(defun %known-advanced-type-atom-p (value)
  "Return T when VALUE is an atomic form that should parse as a type node."
  (and (symbolp value)
       (or (member (symbol-name value) '("?" "_") :test #'string=)
           (assoc (symbol-name value)
                  *primitive-type-name-table*
                  :test (lambda (name names) (member name names :test #'string=)))
           (and (boundp '*type-alias-registry*)
                (gethash value *type-alias-registry*)))))

(defun %advanced-type-form-head-p (head)
  "Return T when HEAD introduces a parseable type form inside advanced payloads."
  (let ((hn (and (symbolp head) (symbol-name head))))
    (or (%parser-head-name-member-p head *parse-compound-multi-arg-table*)
        (%parser-head-name-member-p head *parse-compound-type-app-table*)
        (and hn (assoc hn *arrow-mult-table* :test #'string=))
        (and hn (%binding-quantifier-ctor hn))
        (and hn (member hn '("VALUES" "OPTION" "REFINE" "RECORD" "VARIANT"
                             "FUNCTION" "HAS-SLOTS" "PROTOCOL"
                             "UNSIGNED-BYTE" "SIGNED-BYTE" "INTEGER" "MOD")
                         :test #'string=))
        (and (symbolp head)
             hn
             (> (length hn) 0)
             (char= (char hn 0) #\!))
        (type-advanced-head-p head))))

(defun %parse-advanced-value (value)
  "Parse VALUE into a type node when it clearly denotes a type; otherwise preserve shape."
  (cond
    ((typep value 'type-node) value)
    ((or (null value) (numberp value) (stringp value) (characterp value) (keywordp value))
     value)
    ((%known-advanced-type-atom-p value)
     (parse-type-specifier value))
    ((consp value)
     (cond
       ((%advanced-type-form-head-p (car value))
        (parse-type-specifier value))
       ((listp value)
        (mapcar #'%parse-advanced-value value))
       (t
        (cons (%parse-advanced-value (car value))
              (%parse-advanced-value (cdr value))))))
    (t value)))

(defun %parse-advanced-items (items)
  "Split ITEMS into positional args, keyword properties, and optional evidence."
  (let ((args nil)
        (properties nil)
        (evidence nil))
    (loop while items
          do (let ((item (first items)))
               (cond
                 ((eq item :evidence)
                  (unless (consp (rest items))
                    (type-parse-error "advanced :evidence requires a value"))
                  (setf evidence (%parse-advanced-value (second items))
                        items (cddr items)))
                 ((keywordp item)
                  (unless (consp (rest items))
                    (type-parse-error "advanced property ~S requires a value" item))
                  (push (cons item (%parse-advanced-value (second items))) properties)
                  (setf items (cddr items)))
                 (t
                  (push (%parse-advanced-value item) args)
                  (setf items (rest items))))))
    (values (nreverse args) (nreverse properties) evidence)))

(defun %parse-advanced-feature-form (head args)
  "Parse either (advanced FR-xxxx ...) or a registered representative advanced head."
  (let* ((head-name (and (symbolp head) (symbol-name head)))
         (general-p (and head-name (string= head-name "ADVANCED"))))
    (cond
      (general-p
       (unless args
         (type-parse-error "advanced requires a feature id"))
       (let ((feature-id (canonicalize-type-advanced-feature-id (first args))))
         (unless (lookup-type-advanced-feature feature-id)
           (type-parse-error "Unknown advanced feature id: ~S" (first args)))
         (multiple-value-bind (positional properties evidence)
             (%parse-advanced-items (rest args))
           (make-type-advanced :feature-id feature-id
                               :name head
                               :args positional
                               :properties properties
                               :evidence evidence))))
      (t
       (let ((feature-id (type-advanced-feature-id-for-head head)))
         (unless feature-id
           (type-parse-error "Unknown advanced surface head: ~S" head))
         (multiple-value-bind (positional properties evidence)
             (%parse-advanced-items args)
           (make-type-advanced :feature-id feature-id
                               :name head
                               :args positional
                               :properties properties
                               :evidence evidence)))))))

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

       ;; ─── Registered advanced feature families ────────────────────────
       ((and (symbolp head) (type-advanced-head-p head))
        (%parse-advanced-feature-form head args))

       ;; ─── Arrow forms (-> :omega, ->1 :one, ->0 :zero) ────────────────
       ((and hn (assoc hn *arrow-mult-table* :test #'string=))
        (parse-arrow-type args (cdr (assoc hn *arrow-mult-table* :test #'string=))))

      ;; ─── Binding quantifiers (forall/∀, exists/∃, mu/μ, type-lambda) ──
      ((and hn (%binding-quantifier-ctor hn))
       (unless (= (length args) 2)
         (type-parse-error "~A requires (~A var body)" hn (string-downcase hn)))
        (let* ((ctor (%binding-quantifier-ctor hn))
               (var  (%parse-bounded-quantifier-var (first args)))
               (body (parse-type-specifier (second args))))
          (funcall ctor :var var :body body)))

       ;; ─── Structural shape: (has-slots :x (:y fixnum)) ───────────────
       ((and hn (string= hn "HAS-SLOTS"))
        (unless args
          (type-parse-error "has-slots requires at least one slot requirement"))
        (make-type-record :fields (%coerce-structural-field-specs args)
                          :row-var nil))

       ;; ─── Protocol reference: (protocol drawable) ─────────────────────
       ((and hn (string= hn "PROTOCOL"))
        (unless (= (length args) 1)
          (type-parse-error "protocol requires exactly one protocol name"))
        (make-type-constructor 'protocol
                               (list (%parse-protocol-name-type (first args)))))

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
