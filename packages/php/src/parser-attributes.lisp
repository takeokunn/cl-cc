(in-package :cl-cc/php)

(defstruct php-attribute
  "PHP 8 attribute descriptor preserved as parser metadata."
  name
  args
  target-type)

(defun %php-attribute-metadata (attributes &optional target-type)
  "Return ATTRIBUTES as metadata plist entries, optionally setting TARGET-TYPE."
  (when attributes
    (let ((attrs (if target-type
                     (mapcar (lambda (attribute)
                               (let ((copy (copy-php-attribute attribute)))
                                 (setf (php-attribute-target-type copy) target-type)
                                 copy))
                             attributes)
                     attributes)))
      (list :php-attributes attrs))))

(defun %php-merge-metadata (metadata attributes &optional target-type)
  "Append PHP attribute metadata to existing plist METADATA."
  (append metadata (%php-attribute-metadata attributes target-type)))

(defun %php-attach-attributes-to-node (node attributes &optional target-type)
  "Attach ATTRIBUTES to NODE's AST metadata and return NODE."
  (when (and node attributes (ast-node-p node))
    (setf (ast-imports node)
          (%php-merge-metadata (ast-imports node) attributes target-type)))
  node)

(defun %php-parse-attribute-argument (stream)
  "Parse a single positional or named PHP attribute argument."
  (if (and (member (php-peek-type stream) '(:T-IDENT :T-TYPE :T-KEYWORD) :test #'eq)
           (eq (php-peek-type (cdr stream)) :T-COLON))
      (let ((name (%php-qualified-name-segment-string (php-peek stream))))
        (multiple-value-bind (expr rest _) (php-parse-expr (cddr stream) nil)
          (declare (ignore _))
          (values (list :name name :value expr) rest)))
      (multiple-value-bind (expr rest _) (php-parse-expr stream nil)
        (declare (ignore _))
        (values expr rest))))

(defun %php-parse-attribute-args (stream)
  "Parse optional PHP attribute constructor arguments."
  (if (not (eq (php-peek-type stream) :T-LPAREN))
      (values nil stream)
      (let ((current (cdr stream))
            (args nil))
        (unless (eq (php-peek-type current) :T-RPAREN)
          (loop
            (multiple-value-bind (arg rest) (%php-parse-attribute-argument current)
              (push arg args)
              (setf current rest))
            (cond ((eq (php-peek-type current) :T-COMMA)
                   (setf current (cdr current))
                   (when (eq (php-peek-type current) :T-RPAREN)
                     (return)))
                  (t (return)))))
        (values (nreverse args) (%php-consume-expected :T-RPAREN current)))))

(defun %php-parse-attribute (stream)
  "Parse one PHP attribute name plus optional argument list."
  (multiple-value-bind (name rest) (php-parse-qualified-name stream)
    (multiple-value-bind (args rest2) (%php-parse-attribute-args rest)
      (values (make-php-attribute :name (%php-strip-leading-namespace-separator name)
                                  :args args)
              rest2))))

(defun %php-parse-attribute-group (stream)
  "Parse one #[Attr1(args), Attr2] attribute group."
  (let ((current (%php-consume-expected :T-ATTRIBUTE-OPEN stream))
        (attributes nil))
    (loop
      (multiple-value-bind (attribute rest) (%php-parse-attribute current)
        (push attribute attributes)
        (setf current rest))
      (cond ((eq (php-peek-type current) :T-COMMA)
             (setf current (cdr current))
             (when (eq (php-peek-type current) :T-RBRACKET)
               (return)))
            (t (return))))
    (values (nreverse attributes) (%php-consume-expected :T-RBRACKET current))))

(defun %php-parse-attributes (stream)
  "Parse zero or more PHP 8 attribute groups and return attributes/rest."
  (let ((current stream)
        (attributes nil))
    (loop while (eq (php-peek-type current) :T-ATTRIBUTE-OPEN)
          do (multiple-value-bind (group rest) (%php-parse-attribute-group current)
               (setf attributes (append attributes group)
                     current rest)))
    (values attributes current)))

(defun %php-skip-attributes (stream)
  "Skip PHP 8 attributes from STREAM and return the remaining stream."
  (nth-value 1 (%php-parse-attributes stream)))
