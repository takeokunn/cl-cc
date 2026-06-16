;;;; packages/php/src/parser-interface.lisp — PHP interface declaration parser
(in-package :cl-cc/php)

;;; ─── Interface Registry ──────────────────────────────────────────────────────

(defvar *php-interface-registry* (make-hash-table :test #'equal)
  "Maps interface name symbols to their compile-time descriptors.
Each entry is a plist: (:parents parent-name-list :methods method-sig-list :constants constant-list).")

;;; ─── Interface Registration ──────────────────────────────────────────────────

(defun %php-interface-register (name parent-names method-sigs constants)
  "Record an interface definition in *PHP-INTERFACE-REGISTRY*.
NAME is a symbol. PARENT-NAMES is a list of symbols. METHOD-SIGS is a list
of method signature plists (each with :name, :params, :return-type, :modifiers).
CONSTANTS is a list of slot-def AST nodes representing interface constants."
  (setf (gethash name *php-interface-registry*)
        (list :parents parent-names
              :methods method-sigs
              :constants constants)))

;;; ─── Interface Body Parsing ──────────────────────────────────────────────────

(defun %php-parse-interface-method-sig (stream modifiers attributes)
  "Parse an abstract method signature: [modifiers] function name(params): type;
Returns (values method-sig-plist remaining-stream)."
  (let ((current (cdr stream)))            ; consume :function keyword
    (let ((returns-by-ref (%php-reference-token-p current)))
      (when returns-by-ref
        (setf current (cdr current)))
    (multiple-value-bind (name-tok rest) (php-expect :T-IDENT current)
      (let ((method-name (php-ident-sym (php-tok-value name-tok))))
        (multiple-value-bind (params rest2 param-types param-attributes by-ref-indices
                              param-defaults variadic-param)
            (php-parse-param-list rest)
          (multiple-value-bind (return-type rest3)
              (php-parse-return-type rest2)
            (values (list :name method-name
                          :params params
                          :param-types param-types
                          :param-attributes param-attributes
                          :by-ref-indices by-ref-indices
                          :param-defaults param-defaults
                          :variadic-param variadic-param
                          :return-type return-type
                          :returns-by-ref returns-by-ref
                          :modifiers modifiers
                          :attributes attributes)
                    (php-skip-semis rest3)))))))))

(defun %php-parse-interface-body (stream known-vars)
  "Parse the body of an interface { abstract-methods constants }.
Returns (values method-sigs constants remaining-stream)."
  (let ((current (%php-consume-expected :T-LBRACE stream))
        (method-sigs nil)
        (constants nil))
    (loop
      (setf current (php-skip-semis current))
      (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE))
        (return))
      (multiple-value-bind (attributes after-attributes) (%php-parse-attributes current)
        (setf current after-attributes)
        (multiple-value-bind (modifiers after-modifiers) (%php-parse-visibility-modifiers current)
          (setf current after-modifiers)
          (cond
            ;; abstract method signature: [public] function name(...): type;
            ((and (eq (php-peek-type current) :T-KEYWORD)
                  (eq (php-peek-value current) :function))
             (multiple-value-bind (sig rest2)
                 (%php-parse-interface-method-sig current modifiers attributes)
               (push sig method-sigs)
               (setf current rest2)))
            ;; const [TYPE] NAME = value;
            ((and (eq (php-peek-type current) :T-KEYWORD)
                  (eq (php-peek-value current) :const))
             (multiple-value-bind (slot rest2 extra-slots)
                 (%php-parse-class-constant current modifiers known-vars attributes)
               (push slot constants)
               (dolist (extra-slot extra-slots)
                 (push extra-slot constants))
               (setf current rest2)))
            (t
             (error "PHP interface: unsupported member near token ~S" (php-peek current)))))))
    (values (nreverse method-sigs)
            (nreverse constants)
            (%php-consume-expected :T-RBRACE current))))

;;; ─── Interface Parent (extends) Clause ──────────────────────────────────────

(defun %php-parse-interface-parents (stream)
  "Parse optional 'extends A, B, ...' for interfaces (multiple inheritance).
Returns (values parent-name-list remaining-stream)."
  (if (and stream
           (eq (php-peek-type stream) :T-KEYWORD)
           (eq (php-peek-value stream) :extends))
      (let ((current (cdr stream))
            (parents nil))
        (loop
          (multiple-value-bind (iface-name rest) (php-parse-qualified-name current)
            (push (php-ident-sym (php-resolve-qualified-name iface-name :class)) parents)
            (setf current rest))
          (unless (and current (eq (php-peek-type current) :T-COMMA))
            (return))
          (setf current (cdr current)))
        (values (nreverse parents) current))
      (values nil stream)))

;;; ─── Top-Level Interface Declaration Parser ──────────────────────────────────

(defun %php-parse-interface-decl (stream known-vars)
  "Parse: interface Name [extends A, B] { abstract-methods constants }
Registers the interface in *PHP-INTERFACE-REGISTRY* for compile-time use.
Returns (values nil remaining-stream known-vars) — interfaces are compile-time
only and produce no runtime AST node."
  (multiple-value-bind (name-tok rest) (php-expect :T-IDENT stream)
    (let ((iface-name (php-ident-sym
                       (php-resolve-qualified-name (php-tok-value name-tok) :class))))
      (multiple-value-bind (parent-names rest2)
          (%php-parse-interface-parents rest)
        (multiple-value-bind (method-sigs constants rest3)
            (%php-parse-interface-body rest2 known-vars)
          (%php-interface-register iface-name parent-names method-sigs constants)
          (values (make-ast-defclass :name iface-name
                                     :superclasses parent-names
                                     :slots constants
                                     :php-kind :interface)
                  rest3
                  known-vars))))))
