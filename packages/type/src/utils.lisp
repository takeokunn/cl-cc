;;;; utils.lisp — FR-1701/1702/1803/1804/3303/3304 utility type APIs

(in-package :cl-cc/type)

(defstruct frozen-value
  "Runtime wrapper produced by FR-3303 freeze."
  value
  type)

(defun make-type-level-natural (value)
  "Construct a FR-1701 type-level natural number node."
  (unless (and (integerp value) (not (minusp value)))
    (error "Type-level natural must be a non-negative integer, got ~S" value))
  (make-type-advanced :feature-id "FR-1701" :name 'nat :args (list value)))

(defun type-level-natural-p (type)
  "Return T when TYPE is a FR-1701 natural node."
  (and (type-advanced-p type)
       (string= (type-advanced-feature-id type) "FR-1701")
       (member (type-advanced-name type) '(nat known-nat type-plus type-mul) :test #'eq)))

(defun type-level-natural-value (type)
  "Return TYPE's natural value or signal when it is not statically known."
  (cond
    ((and (integerp type) (not (minusp type))) type)
    ((and (type-level-natural-p type)
          (integerp (first (type-advanced-args type))))
     (first (type-advanced-args type)))
    (t (error "Natural value is not statically known: ~S" type))))

(defun known-nat-value (type)
  "KnownNat-style runtime extraction for FR-1701."
  (type-level-natural-value type))

(defun type-plus (left right)
  "Compute FR-1701 type-level natural addition."
  (make-type-level-natural (+ (type-level-natural-value left)
                              (type-level-natural-value right))))

(defun type-mul (left right)
  "Compute FR-1701 type-level natural multiplication."
  (make-type-level-natural (* (type-level-natural-value left)
                              (type-level-natural-value right))))

(defun make-length-indexed-vector-type (length element-type)
  "Construct (vector LENGTH ELEMENT-TYPE) as a static type constructor."
  (make-type-constructor 'vector
                         (list (make-type-level-natural length) element-type)))

(defun make-matrix-type (rows columns element-type)
  "Construct a matrix type indexed by ROWS and COLUMNS."
  (make-type-constructor 'matrix
                         (list (make-type-level-natural rows)
                               (make-type-level-natural columns)
                               element-type)))

(defun matrix-mul-type (left right)
  "Return the result type of multiplying LEFT and RIGHT matrices, or signal on shape mismatch."
  (unless (and (type-constructor-p left)
               (type-constructor-p right)
               (eq (type-constructor-name left) 'matrix)
               (eq (type-constructor-name right) 'matrix))
    (error "matrix-mul-type expects two matrix types"))
  (destructuring-bind (left-rows left-cols left-elem) (type-constructor-args left)
    (destructuring-bind (right-rows right-cols right-elem) (type-constructor-args right)
      (declare (ignore right-rows))
      (unless (= (type-level-natural-value left-cols)
                 (type-level-natural-value (first (type-constructor-args right))))
        (error "Matrix inner dimensions do not match"))
      (unless (type-equal-p left-elem right-elem)
        (error "Matrix element types do not match"))
      (make-type-constructor 'matrix (list left-rows right-cols left-elem)))))

(defun make-type-level-string (value)
  "Construct a FR-1702 type-level string/symbol node."
  (unless (stringp value)
    (error "Type-level string must be a string, got ~S" value))
  (make-type-advanced :feature-id "FR-1702" :name 'symbol-kind :args (list value)))

(defun type-level-string-p (type)
  "Return T when TYPE is a FR-1702 string/symbol-kind node."
  (and (type-advanced-p type)
       (string= (type-advanced-feature-id type) "FR-1702")))

(defun type-level-string-value (type)
  "Return TYPE's type-level string value."
  (cond
    ((stringp type) type)
    ((and (type-level-string-p type)
          (stringp (first (type-advanced-args type))))
     (first (type-advanced-args type)))
    (t (error "String value is not statically known: ~S" type))))

(defun has-field-type (field-name field-type)
  "Construct a FR-1702 named-field requirement."
  (make-type-advanced :feature-id "FR-1702"
                      :name 'has-field
                      :args (list field-name field-type)))

(defun %field-name= (left right)
  (string= (string-upcase (if (symbolp left) (symbol-name left) (princ-to-string left)))
           (string-upcase (if (symbolp right) (symbol-name right) (princ-to-string right)))))

(defun get-field-type (field-name record-type)
  "Return FIELD-NAME's type from RECORD-TYPE or signal a type error."
  (unless (type-record-p record-type)
    (error "get-field-type expects a record type, got ~S" record-type))
  (let ((entry (find field-name (type-record-fields record-type)
                     :key #'car :test #'%field-name=)))
    (unless entry
      (error "Record type does not contain field ~S" field-name))
    (cdr entry)))

(defun template-literal-type (&rest parts)
  "Construct a FR-1702 template literal type by concatenating static PARTS."
  (make-type-level-string
   (apply #'concatenate 'string
          (mapcar (lambda (part)
                    (if (type-level-string-p part)
                        (type-level-string-value part)
                        (princ-to-string part)))
                  parts))))

(defun make-hlist-type (types)
  "Construct a FR-1803 HList type."
  (unless (every (lambda (type) (typep type 'type-node)) types)
    (error "HList elements must be type nodes: ~S" types))
  (make-type-advanced :feature-id "FR-1803" :name 'hlist :args types))

(defun hlist-head-type (hlist-type)
  "Return the head type of HLIST-TYPE."
  (unless (and (type-advanced-p hlist-type)
               (string= (type-advanced-feature-id hlist-type) "FR-1803")
               (type-advanced-args hlist-type))
    (error "Expected non-empty HList type, got ~S" hlist-type))
  (first (type-advanced-args hlist-type)))

(defun hlist-tail-type (hlist-type)
  "Return the HList tail type of HLIST-TYPE."
  (unless (and (type-advanced-p hlist-type)
               (string= (type-advanced-feature-id hlist-type) "FR-1803")
               (type-advanced-args hlist-type))
    (error "Expected non-empty HList type, got ~S" hlist-type))
  (make-hlist-type (rest (type-advanced-args hlist-type))))

(defun %format-directive-type (directive)
  (case directive
    ((#\D #\B #\O #\X) type-int)
    ((#\F #\E #\G #\$) type-float)
    ((#\S #\A) type-string)
    ((#\C) type-char)
    (otherwise nil)))

(defun format-type (control-string)
  "Infer a FR-1804 printf-style function type from CONTROL-STRING."
  (unless (stringp control-string)
    (error "format-type expects a literal control string"))
  (let ((params nil)
        (length (length control-string))
        (index 0))
    (loop while (< index length) do
      (let ((char (char control-string index)))
        (if (char= char #\~)
            (progn
              (incf index)
              (when (< index length)
                (let ((directive (char-upcase (char control-string index))))
                  (cond
                    ((char= directive #\~) nil)
                    ((%format-directive-type directive)
                     (push (%format-directive-type directive) params))
                    (t (error "Unsupported format directive ~~A" directive))))))
            nil))
      (incf index))
    (make-type-arrow (nreverse params) type-string)))

(defun readonly-type (type)
  "Construct a shallow FR-3303 readonly type."
  (make-type-capability :base type :cap 'readonly))

(defun writable-type (type)
  "Construct a writable marker used by readonly subtyping tests."
  (make-type-capability :base type :cap 'writable))

(defun deep-readonly-type (type)
  "Recursively mark TYPE as readonly."
  (cond
    ((type-record-p type)
     (readonly-type
      (make-type-record :fields (mapcar (lambda (field)
                                          (cons (car field) (deep-readonly-type (cdr field))))
                                        (type-record-fields type))
                        :row-var (type-record-row-var type))))
    ((type-union-p type)
     (readonly-type (make-type-union (mapcar #'deep-readonly-type (type-union-types type))
                                     :constructor-name (type-union-constructor-name type))))
    (t (readonly-type type))))

(defun freeze (value type)
  "Return VALUE wrapped with its FR-3303 readonly type."
  (make-frozen-value :value value :type (readonly-type type)))

(defun partial-type (type)
  "Make every record field optional (FR-3304 Partial<T>)."
  (if (type-record-p type)
      (make-type-record :fields (mapcar (lambda (field)
                                          (cons (car field)
                                                (make-type-union (list type-null (cdr field))
                                                                 :constructor-name 'option)))
                                        (type-record-fields type))
                        :row-var (type-record-row-var type))
      (make-type-union (list type-null type) :constructor-name 'option)))

(defun required-type (type)
  "Remove NULL from optional fields (FR-3304 Required<T>)."
  (if (type-record-p type)
      (make-type-record :fields (mapcar (lambda (field)
                                          (cons (car field) (non-nullable-type (cdr field))))
                                        (type-record-fields type))
                        :row-var (type-record-row-var type))
      (non-nullable-type type)))

(defun pick-type (type keys)
  "Select KEYS from a record type (FR-3304 Pick<T,K>)."
  (unless (type-record-p type)
    (error "pick-type expects a record type"))
  (make-type-record :fields (remove-if-not (lambda (field)
                                             (member (car field) keys :test #'%field-name=))
                                           (type-record-fields type))
                    :row-var nil))

(defun omit-type (type keys)
  "Omit KEYS from a record type (FR-3304 Omit<T,K>)."
  (unless (type-record-p type)
    (error "omit-type expects a record type"))
  (make-type-record :fields (remove-if (lambda (field)
                                         (member (car field) keys :test #'%field-name=))
                                       (type-record-fields type))
                    :row-var nil))

(defun exclude-type (union excluded)
  "Remove EXCLUDED from UNION (FR-3304 Exclude<T,U>)."
  (if (type-union-p union)
      (let ((members (remove-if (lambda (member)
                                  (or (type-equal-p member excluded)
                                      (is-subtype-p member excluded)))
                                (type-union-types union))))
        (cond ((null members) type-null)
              ((null (rest members)) (first members))
              (t (make-type-union members :constructor-name (type-union-constructor-name union)))))
      (if (is-subtype-p union excluded) type-null union)))

(defun extract-type (union target)
  "Keep members of UNION assignable to TARGET (FR-3304 Extract<T,U>)."
  (if (type-union-p union)
      (let ((members (remove-if-not (lambda (member) (is-subtype-p member target))
                                    (type-union-types union))))
        (cond ((null members) type-null)
              ((null (rest members)) (first members))
              (t (make-type-union members :constructor-name (type-union-constructor-name union)))))
      (if (is-subtype-p union target) union type-null)))

(defun non-nullable-type (type)
  "Remove NULL from TYPE (FR-3304 NonNullable<T>)."
  (if (type-union-p type)
      (let ((members (remove-if (lambda (member) (type-equal-p member type-null))
                                (type-union-types type))))
        (cond ((null members) type-null)
              ((null (rest members)) (first members))
              (t (make-type-union members :constructor-name (type-union-constructor-name type)))))
      type))

(defun return-type-of (function-type)
  "Extract a function return type (FR-3304 ReturnType<T>)."
  (unless (type-arrow-p function-type)
    (error "return-type-of expects an arrow type"))
  (type-arrow-return function-type))
