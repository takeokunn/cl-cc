;;;; packages/php/src/parser-trait.lisp — PHP Trait declaration and use-trait parser
;;;;
;;;; Implements:
;;;;   trait Greetable { method-list }
;;;;   use TraitA, TraitB [{ insteadof/as block }] ;
;;;;
;;;; AST mapping:
;;;;   trait Foo { ... }  → ast-defclass with :php-kind :trait, methods as slot-defs
;;;;   use A, B { ... }   → ast-slot-def with :php-trait-use t / :php-trait-uses list
;;;;                         conflict resolution stored in :php-trait-insteadof / :php-trait-alias

(in-package :cl-cc/php)

;;; ─── Trait Registry ─────────────────────────────────────────────────────────

(defvar *php-trait-registry* (make-hash-table :test #'equal)
  "Maps trait name strings to their method slot-def lists.
Used for compile-time trait application and conflict detection.")

;;; ─── Trait Application Runtime Record ──────────────────────────────────────

(defvar *php-trait-applications* (make-hash-table :test #'equal)
  "Maps class-name strings to lists of trait application plists.
Each plist has :trait-names, :insteadof, :alias entries.")

(defun %php-apply-traits (class-name trait-names insteadof-list alias-list)
  "Record trait usage for CLASS-NAME.
TRAIT-NAMES  — list of trait name symbols being used.
INSTEADOF-LIST — list of (method-sym trait-sym insteadof-sym ...) plists.
ALIAS-LIST   — list of (original-sym alias-sym) plists.
Stores into *php-trait-applications* and copies method slot-defs from
*php-trait-registry* for each trait."
  (let ((class-key (if (symbolp class-name)
                       (symbol-name class-name)
                       class-name)))
    (push (list :trait-names trait-names
                :insteadof  insteadof-list
                :alias      alias-list)
          (gethash class-key *php-trait-applications* nil))
    ;; Copy method definitions into a per-class merged slot list (advisory).
    (dolist (trait-sym trait-names)
      (let* ((trait-key (symbol-name trait-sym))
             (methods   (gethash trait-key *php-trait-registry*)))
        (when methods
          (let* ((existing (gethash class-key *php-trait-applications* nil))
                 (record   (car existing)))
            (setf (getf record :methods)
                  (append (getf record :methods) methods))))))))

;;; ─── Conflict Resolution Block Parser ───────────────────────────────────────
;;;
;;; Parses the optional `{ insteadof/as clauses }` block that follows a
;;; use-trait list.  Returns (values insteadof-list alias-list rest-stream).
;;;
;;; Grammar (simplified):
;;;   conflict-block  ::= '{' conflict-stmt* '}'
;;;   conflict-stmt   ::= qualified '::' name 'insteadof' name (',' name)* ';'
;;;                      | qualified '::' name 'as' [visibility] [alias-name] ';'
;;;                      | name 'as' [visibility] alias-name ';'

(defun %php-parse-insteadof-targets (stream)
  "Consume comma-separated trait names after insteadof.
Returns (values name-sym-list rest)."
  (let ((names nil) (current stream))
    (loop
      (multiple-value-bind (qname rest) (php-parse-qualified-name current)
        (push (php-ident-sym (php-resolve-qualified-name qname :class)) names)
        (setf current rest))
      (unless (eq (php-peek-type current) :T-COMMA)
        (return))
      (setf current (cdr current)))
    (values (nreverse names) current)))

(defun %php-visibility-keyword-p (stream)
  "Return T when the current token is a visibility modifier keyword."
  (and (eq (php-peek-type stream) :T-KEYWORD)
       (member (php-peek-value stream)
               '(:public :protected :private) :test #'eq)))

(defun %php-parse-conflict-clause (stream insteadof-acc alias-acc)
  "Parse one insteadof-or-as clause.
Returns (values updated-insteadof updated-alias rest-stream).

Clause forms:
  TraitA::method insteadof TraitB, TraitC ;
  TraitA::method as alias ;
  TraitA::method as public ;        (visibility-only alias)
  method         as alias ;
  method         as public alias ;
"
  (multiple-value-bind (lhs-name lhs-rest) (php-parse-qualified-name stream)
    (let ((current lhs-rest))
      (cond
        ;; TraitA::method ...
        ((and (eq (php-peek-type current) :T-OP)
              (equal "::" (php-peek-value current)))
         (setf current (cdr current))       ; consume ::
         (multiple-value-bind (method-tok method-rest) (php-expect :T-IDENT current)
           (setf current method-rest)
           (let ((trait-sym  (php-ident-sym (php-resolve-qualified-name lhs-name :class)))
                 (method-sym (php-ident-sym (php-tok-value method-tok))))
             (cond
               ;; TraitA::method insteadof TraitB, TraitC ;
               ((%php-keyword-p current :insteadof)
                (setf current (cdr current))
                (multiple-value-bind (excluded rest2)
                    (%php-parse-insteadof-targets current)
                  (values (cons (list :method method-sym
                                      :from   trait-sym
                                      :exclude excluded)
                                insteadof-acc)
                          alias-acc
                          (php-skip-semis rest2))))
               ;; TraitA::method as [visibility] [alias] ;
               ((%php-keyword-p current :as)
                (setf current (cdr current))
                (let ((vis nil) (alias nil))
                  (when (%php-visibility-keyword-p current)
                    (setf vis (php-peek-value current)
                          current (cdr current)))
                  (when (eq (php-peek-type current) :T-IDENT)
                    (multiple-value-bind (alias-tok rest2) (php-consume current)
                      (setf alias (php-ident-sym (php-tok-value alias-tok))
                            current rest2)))
                  (values insteadof-acc
                          (cons (list :method method-sym
                                      :from   trait-sym
                                      :alias  alias
                                      :vis    vis)
                                alias-acc)
                          (php-skip-semis current))))
               (t
                (error "PHP trait conflict block: expected insteadof or as after ~S::~S"
                       lhs-name (php-tok-value method-tok)))))))
        ;; Simple: method as [visibility] alias ;
        ((%php-keyword-p current :as)
         (setf current (cdr current))
         (let ((method-sym (php-ident-sym (php-resolve-qualified-name lhs-name :function)))
               (vis nil)
               (alias nil))
           (when (%php-visibility-keyword-p current)
             (setf vis (php-peek-value current)
                   current (cdr current)))
           (when (eq (php-peek-type current) :T-IDENT)
             (multiple-value-bind (alias-tok rest2) (php-consume current)
               (setf alias (php-ident-sym (php-tok-value alias-tok))
                     current rest2)))
           (values insteadof-acc
                   (cons (list :method method-sym
                               :from   nil
                               :alias  alias
                               :vis    vis)
                         alias-acc)
                   (php-skip-semis current))))
        (t
         (error "PHP trait conflict block: unrecognized clause near ~S" (php-peek stream)))))))

(defun %php-parse-trait-conflict-block (stream)
  "Parse an optional `{ ... }` conflict-resolution block after a use-trait list.
Returns (values insteadof-list alias-list rest-stream).
If no block is present (semicolon follows), returns empty lists."
  (cond
    ;; No conflict block — simple use A, B ;
    ((eq (php-peek-type stream) :T-SEMI)
     (values nil nil (cdr stream)))
    ;; Conflict block — use A, B { ... }
    ((eq (php-peek-type stream) :T-LBRACE)
     (let ((current (cdr stream))             ; consume {
           (insteadof nil)
           (alias nil))
       (loop
         (setf current (php-skip-semis current))
         (when (or (php-at-eof-p current)
                   (eq (php-peek-type current) :T-RBRACE))
           (return))
         (multiple-value-bind (new-insteadof new-alias rest2)
             (%php-parse-conflict-clause current insteadof alias)
           (setf insteadof new-insteadof
                 alias     new-alias
                 current   rest2)))
       (values (nreverse insteadof)
               (nreverse alias)
               (%php-consume-expected :T-RBRACE current))))
    (t
     (error "PHP parse error: expected ; or { after use-trait list, got ~S"
            (php-peek stream)))))

;;; ─── Use-Trait Statement ────────────────────────────────────────────────────

(defun %php-parse-use-trait-stmt (stream known-vars)
  "Parse `use TraitA, TraitB [{ insteadof/as block }] ;` inside a class body.
STREAM starts after the `use` keyword has been consumed.
Returns (values ast-slot-def rest-stream known-vars).

The returned ast-slot-def carries:
  :php-trait-use    t
  :php-trait-names  list-of-trait-syms
  :php-insteadof    list-of-insteadof-plists
  :php-alias        list-of-alias-plists"
  (declare (ignore known-vars))
  (let ((current stream)
        (trait-syms nil))
    ;; Collect comma-separated trait names.
    (loop
      (multiple-value-bind (qname rest) (php-parse-qualified-name current)
        (push (php-ident-sym (php-resolve-qualified-name qname :class)) trait-syms)
        (setf current rest))
      (unless (eq (php-peek-type current) :T-COMMA)
        (return))
      (setf current (cdr current)))
    (let ((names (nreverse trait-syms)))
      (multiple-value-bind (insteadof alias rest2)
          (%php-parse-trait-conflict-block current)
        (let ((slot (make-ast-slot-def
                     :name (gensym "PHP-USE-TRAIT-")
                     :allocation :class
                     :imports (list :php-trait-use   t
                                    :php-trait-names  names
                                    :php-insteadof    insteadof
                                    :php-alias        alias))))
          (values slot rest2 nil))))))

;;; ─── Trait Declaration Parser ───────────────────────────────────────────────

(defun %php-parse-trait-decl (stream known-vars)
  "Parse `trait Name { method-list }`.
STREAM starts after the `trait` keyword has been consumed.
Stores methods in *php-trait-registry* under the trait name string.
Returns (values ast-defclass rest-stream known-vars).

The returned ast-defclass has :php-kind :trait and methods as slot-defs
with method bodies in their initform (same convention as class methods)."
  (multiple-value-bind (name-tok rest) (php-expect :T-IDENT stream)
    (let* ((trait-name-str (php-tok-value name-tok))
           (trait-sym      (php-ident-sym
                            (php-resolve-qualified-name trait-name-str :class)))
           (current        (%php-consume-expected :T-LBRACE rest))
           (slots          nil))
      (loop
        (setf current (php-skip-semis current))
        (when (or (php-at-eof-p current)
                  (eq (php-peek-type current) :T-RBRACE))
          (return))
        (multiple-value-bind (slot rest2)
            (%php-parse-class-body-member current known-vars)
          (when slot (push slot slots))
          (setf current rest2)))
      (let ((method-slots (nreverse slots)))
        ;; Register methods in the trait registry for compile-time application.
        (setf (gethash (string-upcase trait-name-str) *php-trait-registry*)
              method-slots)
        (values (make-ast-defclass :name       trait-sym
                                    :superclasses nil
                                    :slots      method-slots
                                    :php-kind   :trait)
                (%php-consume-expected :T-RBRACE current)
                known-vars)))))
