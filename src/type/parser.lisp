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
  (cond
    ((null spec)     type-null)
    ((eq spec '?)    (make-type-error :message "unknown"))   ; ? → error sentinel
    ((symbolp spec)  (parse-primitive-type spec))
    ((consp spec)    (parse-compound-type spec))
    (t (type-parse-error "Invalid type specifier: ~S" spec))))

;;; ─── Primitive types ──────────────────────────────────────────────────────

(defun parse-primitive-type (name)
  (case name
    ((fixnum integer int)    type-int)
    ((float single-float double-float) type-float)
    ((string simple-string)  type-string)
    ((boolean bool)          type-bool)
    ((symbol)                type-symbol)
    ((cons)                  type-cons)
    ((null nil)              type-null)
    ((t top)                 type-any)
    ((character char)        type-char)
    (otherwise
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
       (make-type-union :types (mapcar #'parse-type-specifier args)))

      ;; (and T1 T2 ...) — intersection
      ((and)
       (unless args (type-parse-error "and requires at least one type"))
       (make-type-intersection :types (mapcar #'parse-type-specifier args)))

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

      (otherwise
       (parse-compound-type-extended head args)))))

(defun parse-compound-type-extended (head args)
  "Handle all symbol-named compound forms using string comparison for package-independence."
  (let ((hn (and (symbolp head) (symbol-name head))))
    (cond
      ;; ─── Arrow forms ──────────────────────────────────────────────────
      ((and hn (string= hn "->"))   (parse-arrow-type args :omega))
      ((and hn (string= hn "->1"))  (parse-arrow-type args :one))
      ((and hn (string= hn "->0"))  (parse-arrow-type args :zero))

      ;; ─── Universal quantification: (forall a T) or (∀ a T) ───────────
      ((and hn (or (string= hn "FORALL") (string= hn "∀")))
       (unless (= (length args) 2)
         (type-parse-error "forall requires (forall var body)"))
       (let* ((var  (fresh-type-var (first args)))
              (body (parse-type-specifier (second args))))
         (make-type-forall :var var :body body)))

      ;; ─── Existential: (exists a T) ────────────────────────────────────
      ((and hn (or (string= hn "EXISTS") (string= hn "∃")))
       (unless (= (length args) 2)
         (type-parse-error "exists requires (exists var body)"))
       (let* ((var  (fresh-type-var (first args)))
              (body (parse-type-specifier (second args))))
         (make-type-exists :var var :body body)))

      ;; ─── Recursive: (mu a T) ──────────────────────────────────────────
      ((and hn (or (string= hn "MU") (string= hn "μ")))
       (unless (= (length args) 2)
         (type-parse-error "mu requires (mu var body)"))
       (let* ((var  (fresh-type-var (first args)))
              (body (parse-type-specifier (second args))))
         (make-type-mu :var var :body body)))

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

      ;; ─── Graded modal: (! q T) or (!1 T) or (!ω T) ──────────────────
      ((and hn (or (string= hn "!") (string= hn "!1") (string= hn "!ω") (string= hn "!0")))
       (let ((grade (cond ((string= hn "!1")  :one)
                          ((string= hn "!0")  :zero)
                          ((string= hn "!ω")  :omega)
                          (t ; (! q T) form
                           (case (first args)
                             ((1 :one one)    :one)
                             ((0 :zero zero)  :zero)
                             (otherwise :omega)))))
             (inner-spec (if (string= hn "!") (second args) (first args))))
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
  ;; Check for effect annotation via !
  (let* ((bang-pos  (position '! args))
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
             (pipe-pos (position '\| inner))
             (eff-names (if pipe-pos (subseq inner 0 pipe-pos) inner))
             (row-var   (when pipe-pos (parse-type-specifier (nth (1+ pipe-pos) inner)))))
        (make-type-effect-row
         :effects (mapcar (lambda (n) (make-type-effect-op :name n)) eff-names)
         :row-var row-var))
      ;; Flat list of names: (IO State | ε)
      (let* ((pipe-pos (position '\| specs))
             (eff-names (if pipe-pos (subseq specs 0 pipe-pos) specs))
             (row-var   (when pipe-pos (parse-type-specifier (nth (1+ pipe-pos) specs)))))
        (make-type-effect-row
         :effects (mapcar (lambda (n) (make-type-effect-op :name n)) eff-names)
         :row-var row-var))))

;;; ─── Row type parsing ─────────────────────────────────────────────────────

(defun parse-row-type (args kind)
  "Parse (Record (l1 T1) ... | ρ) or (Variant (L1 T1) ... | ρ)."
  (let* ((pipe-pos (position '\| args))
         (field-specs (if pipe-pos (subseq args 0 pipe-pos) args))
         (row-var-spec (when pipe-pos (nth (1+ pipe-pos) args)))
         (row-var (when row-var-spec (parse-type-specifier row-var-spec)))
         (fields (mapcar (lambda (f)
                           (unless (and (consp f) (= (length f) 2))
                             (type-parse-error "Row field must be (label type), got ~S" f))
                           (cons (first f) (parse-type-specifier (second f))))
                         field-specs)))
    (ecase kind
      (:record  (make-type-record  :fields fields :row-var row-var))
      (:variant (make-type-variant :cases  fields :row-var row-var)))))

;;; ─── Constraint spec parsing ──────────────────────────────────────────────

(defun parse-constraint-spec (spec)
  "Parse a typeclass constraint spec like (Num a) into a type-constraint."
  (unless (and (consp spec) (>= (length spec) 2) (symbolp (first spec)))
    (type-parse-error "Constraint must be (ClassName type-arg), got ~S" spec))
  (make-type-constraint :class-name (first spec)
                        :type-arg   (parse-type-specifier (second spec))))

;;; ─── Lambda-list parsing ──────────────────────────────────────────────────

(defun parse-lambda-list-with-types (lambda-list)
  "Parse a typed lambda list like ((x fixnum) y (z string)).
Returns (values param-names param-types) where untyped params get type-any."
  (let (names types)
    (dolist (item lambda-list)
      (cond
        ((and (consp item) (= (length item) 2))
         (push (first item) names)
         (push (parse-type-specifier (second item)) types))
        ((symbolp item)
         (push item names)
         (push type-any types))
        (t (type-parse-error "Invalid lambda-list item: ~S" item))))
    (values (nreverse names) (nreverse types))))

(defun parse-typed-parameter (item)
  "Parse a single possibly-typed parameter (x) or (x fixnum). Returns (name . type)."
  (if (and (consp item) (= (length item) 2) (symbolp (first item)))
      (cons (first item) (parse-type-specifier (second item)))
      (cons item type-any)))

(defun parse-typed-optional-parameter (item)
  "Parse an &optional parameter possibly typed: (x fixnum default) → (name . type)."
  (if (consp item)
      (cons (first item)
            (if (>= (length item) 2)
                (parse-type-specifier (second item))
                type-any))
      (cons item type-any)))

(defun parse-typed-rest-parameter (item)
  "Parse an &rest parameter: x or (x type). Returns (name . type)."
  (parse-typed-parameter item))

;;; ─── Return type extraction ───────────────────────────────────────────────

(defun extract-return-type (body)
  "If BODY starts with a type annotation (declare (return-type T)), extract T."
  (when (and (consp body)
             (consp (first body))
             (eq (caar body) 'declare))
    (let ((decl (cdar body)))
      (when (and decl (consp (car decl))
                 (eq (caar decl) 'return-type))
        (parse-type-specifier (cadar decl))))))

(defun extract-return-type-from-body (body)
  "Extract return type from (declare (return-type ...)) forms in BODY."
  (extract-return-type body))

;;; ─── Typed AST nodes ──────────────────────────────────────────────────────

(defstruct (ast-defun-typed (:constructor make-ast-defun-typed))
  "A defun node with type annotations."
  (name nil :type symbol)
  (params nil :type list)
  (param-types nil :type list)
  (return-type nil)
  (body nil)
  (source-location nil))

(defstruct (ast-lambda-typed (:constructor make-ast-lambda-typed))
  "A lambda node with type annotations."
  (params nil :type list)
  (param-types nil :type list)
  (return-type nil)
  (body nil)
  (env nil)
  (source-location nil))

(defun parse-typed-defun (form)
  "Parse (defun name ((x T) ...) return-type body...) into ast-defun-typed."
  (destructuring-bind (name lambda-list . rest) (cdr form)
    (multiple-value-bind (names types) (parse-lambda-list-with-types lambda-list)
      (let ((return-type (and (not (null rest))
                              (not (consp (first rest)))
                              (not (eq (first rest) 'declare))
                              (parse-type-specifier-maybe (first rest))))
            (body (if (and rest (not (null (extract-return-type-maybe rest))))
                      (cdr rest)
                      rest)))
        (make-ast-defun-typed
         :name name :params names :param-types types
         :return-type (or return-type type-any) :body body)))))

(defun parse-typed-lambda (form)
  "Parse (lambda ((x T) ...) body...) into ast-lambda-typed."
  (destructuring-bind (lambda-list . body) (cdr form)
    (multiple-value-bind (names types) (parse-lambda-list-with-types lambda-list)
      (make-ast-lambda-typed
       :params names :param-types types :return-type type-any :body body))))

(defun parse-typed-lambda-list (lambda-list)
  "Parse a typed lambda list. Returns (values names types)."
  (parse-lambda-list-with-types lambda-list))

(defun parse-type-specifier-maybe (x)
  "Return a type-node if X looks like a type specifier, else nil."
  (when (looks-like-type-specifier-p x)
    (handler-case (parse-type-specifier x)
      (type-parse-error () nil))))

(defun extract-return-type-maybe (forms)
  "Try to extract a return type from the front of FORMS list."
  (extract-return-type forms))

;;; ─── Utility ──────────────────────────────────────────────────────────────

(defvar *lambda-list-keywords*
  '(&optional &rest &key &allow-other-keys &aux)
  "Lambda list keywords to skip during typed parameter parsing.")

(defun make-type-function-from-spec (param-types return-type)
  "Create an arrow type from a list of param types and a return type."
  (make-type-arrow param-types return-type))
