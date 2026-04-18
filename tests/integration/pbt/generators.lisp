;;;; tests/pbt/generators.lisp - Specialized Generators for Type System and Binary Testing
;;;
;;; This module provides specialized generators for:
;;; - Type expressions (for type system testing)
;;; - Mach-O structures (for binary generation testing)
;;; - Typed AST nodes (for type inference/checking testing)

(in-package :cl-cc/pbt)

;;; Configuration Variables

(defvar *max-type-depth* 3
  "Maximum depth for recursive type expressions.")

(defvar *max-mach-o-sections* 5
  "Maximum number of sections in generated Mach-O segments.")

;;; Type Expression Generators

(defun gen-primitive-type ()
  "Generate a primitive type specifier."
  (gen-one-of '(fixnum single-float double-float string boolean symbol
                integer number character list cons null
                t)))

(defun gen-type-variable ()
  "Generate a type variable for polymorphism testing (?a, ?b, etc.)."
  (gen-fmap (lambda (c) (intern (format nil "?~A" c) :keyword))
            (gen-one-of '(a b c d e f x y z))))

(defun gen-simple-compound-type ()
  "Generate simple compound types like (or T1 T2), (and T1 T2)."
  (gen-bind
   (gen-one-of '(or and))
   (lambda (op)
     (gen-fmap
      (lambda (types)
        (cons op types))
      (gen-list-of (gen-primitive-type)
                   :min-length 2 :max-length 4)))))

(defun gen-values-type ()
  "Generate (values T1 T2 ...) type for multiple values."
  (gen-fmap
   (lambda (types)
     (cons 'values types))
   (gen-list-of (gen-primitive-type)
                :min-length 0 :max-length 5)))

(defun gen-fn-type-args ()
  "Generate argument types for function type."
  (gen-list-of (gen-type-expr :depth 0)
               :min-length 0 :max-length 4))

(defun gen-fn-type ()
  "Generate function type (arg-types -> return-type)."
  (gen-bind
   (gen-fn-type-args)
   (lambda (args)
     (gen-fmap
      (lambda (ret)
        (list 'function args ret))
      (gen-type-expr :depth 0)))))

(defun gen-array-type ()
  "Generate array type specifiers."
  (gen-bind
   (gen-one-of '(simple-array array vector simple-vector
                 bit-vector simple-bit-vector string simple-string))
   (lambda (base)
     (gen-fmap
      (lambda (dims)
        (if dims
            (list base dims)
            base))
      (gen-one-of '(nil (1) (*) (* *) ((*) (*))))))))

(defun gen-cons-type ()
  "Generate (cons car-type cdr-type) type specifiers."
  (gen-fmap
   (lambda (types)
     (cons 'cons types))
   (gen-tuple (gen-type-expr :depth 0)
              (gen-type-expr :depth 0))))

(defun gen-type-expr (&key (depth *max-type-depth*))
  "Generate random type expressions for testing.

   Generates:
   - Primitives: fixnum, single-float, string, boolean, symbol
   - Compound: (function (T1 T2) R), (or T1 T2), (values T1 T2)
   - Variables: ?a, ?b (for polymorphism testing)"
  (let ((effective-depth (max 0 (min depth *max-type-depth*))))
    (if (or (zerop effective-depth)
            (< (random 100 (%pbt-rng))
               (- 100 (* effective-depth 20))))
        ;; Generate terminal type
        (gen-bind
         (gen-one-of (list 0 1))
         (lambda (choice)
           (case choice
             (0 (gen-primitive-type))
             (1 (gen-type-variable)))))
        ;; Generate compound type with reduced depth
        (let ((reduced-depth (1- effective-depth)))
          (gen-bind
           (gen-one-of (list 0 1 2 3 4 5))
           (lambda (choice)
              (case choice
                (0 (gen-simple-compound-type))
                (1 (gen-values-type))
                (2 (gen-fn-type))
                (3 (gen-array-type))
                (4 (gen-cons-type))
                (5 (gen-fn-type)))))))))

(defun gen-type-specifier ()
  "Generate a full type specifier (alias for gen-type-expr)."
  (gen-type-expr))

;;; Mach-O Structure Generators

;; Mach-O Magic Numbers
(defvar +mh-magic+ #xFEEDFACE "32-bit Mach-O")
(defvar +mh-magic-64+ #xFEEDFACF "64-bit Mach-O")
(defvar +mh-cigam+ #xCEFAEDFE "32-bit Mach-O byte-swapped")
(defvar +mh-cigam-64+ #xCFFAEDFE "64-bit Mach-O byte-swapped")

;; CPU Types
(defvar +cpu-type-x86+ 7)
(defvar +cpu-type-x86-64+ #x01000007)
(defvar +cpu-type-arm+ 12)
(defvar +cpu-type-arm64+ #x0100000C)

;; CPU Subtypes
(defvar +cpu-subtype-x86-all+ 3)
(defvar +cpu-subtype-arm-all+ 0)
(defvar +cpu-subtype-arm64-all+ 0)

;; File Types
(defvar +mh-object+ 1 "Relocatable object file")
(defvar +mh-execute+ 2 "Executable file")
(defvar +mh-fvm+ 3 "Fixed VM shared library")
(defvar +mh-core+ 4 "Core file")
(defvar +mh-preload+ 5 "Preloaded executable")
(defvar +mh-dylib+ 6 "Dynamic library")
(defvar +mh-dylinker+ 7 "Dynamic link editor")
(defvar +mh-bundle+ 8 "Dynamic bundle")

;; Header Flags
(defvar +mh-noundefs+ 1 "No undefined references")
(defvar +mh-dyldlink+ 4 "Dyld will link this")
(defvar +mh-pie+ #x200000 "Position-independent executable")

;; Load Command Types
(defvar +lc-segment+ 1)
(defvar +lc-segment-64+ #x19)
(defvar +lc-symtab+ 2)
(defvar +lc-dysymtab+ #x0B)
(defvar +lc-load-dylib+ #x0C)
(defvar +lc-id-dylib+ #x0D)
(defvar +lc-load-weak-dylib+ #x80000018)
(defvar +lc-uuid+ #x1B)
(defvar +lc-rpath+ #x8000001C)
(defvar +lc-code-signature+ #x1D)
(defvar +lc-reexport-dylib+ #x8000001F)
(defvar +lc-version-min-macosx+ #x24)
(defvar +lc-build-version+ #x32)

;; Segment Flags
(defvar +vm-prot-read+ 1)
(defvar +vm-prot-write+ 2)
(defvar +vm-prot-execute+ 4)

(defstruct (mach-header (:constructor make-mach-header-raw))
  "Structure representing a Mach-O header."
  magic
  cputype
  cpusubtype
  filetype
  ncmds
  sizeofcmds
  flags
  reserved)

(defstruct (mach-segment-command (:constructor make-mach-segment-raw))
  "Structure representing a Mach-O segment command."
  cmd
  cmdsize
  segname
  vmaddr
  vmsize
  fileoff
  filesize
  maxprot
  initprot
  nsects
  flags
  sections)

(defstruct (mach-section (:constructor make-mach-section-raw))
  "Structure representing a Mach-O section."
  sectname
  segname
  addr
  size
  offset
  align
  reloff
  nreloc
  flags
  reserved1
  reserved2
  reserved3)

(defun gen-mach-magic ()
  "Generate valid Mach-O magic numbers."
  (gen-one-of (list +mh-magic+ +mh-magic-64+
                    +mh-cigam+ +mh-cigam-64+)))

(defun gen-mach-cpu-type ()
  "Generate valid CPU types for Mach-O."
  (gen-one-of (list +cpu-type-x86+ +cpu-type-x86-64+
                    +cpu-type-arm+ +cpu-type-arm64+)))

(defun gen-mach-cpu-subtype ()
  "Generate valid CPU subtypes for Mach-O."
  (gen-one-of (list +cpu-subtype-x86-all+
                    +cpu-subtype-arm-all+
                    +cpu-subtype-arm64-all+)))

(defun gen-mach-file-type ()
  "Generate valid Mach-O file types."
  (gen-one-of (list +mh-object+ +mh-execute+
                    +mh-dylib+ +mh-bundle+
                    +mh-preload+ +mh-core+)))

(defun gen-mach-flags ()
  "Generate valid Mach-O header flags."
  (gen-fmap
   (lambda (flag-list)
     (reduce #'logior flag-list :initial-value 0))
   (gen-list-of (gen-one-of (list +mh-noundefs+ +mh-dyldlink+ +mh-pie+ 0))
                :min-length 0 :max-length 3)))

(defun gen-mach-header ()
  "Generate valid Mach-O header for testing."
  (gen-bind
   (gen-mach-magic)
   (lambda (magic)
     (let ((is-64bit (or (= magic +mh-magic-64+)
                         (= magic +mh-cigam-64+))))
       (gen-bind
        (gen-mach-cpu-type)
        (lambda (cpu)
          (gen-fmap
           (lambda (rest)
             (destructuring-bind (subtype filetype flags ncmds sizeofcmds) rest
               (make-mach-header-raw
                :magic magic
                :cputype cpu
                :cpusubtype subtype
                :filetype filetype
                :ncmds ncmds
                :sizeofcmds sizeofcmds
                :flags flags
                :reserved (if is-64bit 0 nil))))
           (gen-tuple (gen-mach-cpu-subtype)
                      (gen-mach-file-type)
                      (gen-mach-flags)
                      (gen-integer :min 1 :max 10)
                      (gen-integer :min 32 :max 4096)))))))))

(defun gen-segment-permissions ()
  "Generate valid segment permissions (rwx)."
  (gen-fmap
   (lambda (perms)
     (reduce #'logior perms :initial-value 0))
    (gen-list-of (gen-one-of (list +vm-prot-read+
                                   +vm-prot-write+
                                   +vm-prot-execute+))
                :min-length 1 :max-length 3)))

(defun gen-segment-name ()
  "Generate valid Mach-O segment names (16 bytes, uppercase)."
  (gen-one-of '("__TEXT" "__DATA" "__LINKEDIT" "__OBJC" "__IMPORT" "__LC_SEGMENT")))

(defun gen-section-name ()
  "Generate valid Mach-O section names."
  (gen-one-of '("__text" "__data" "__bss" "__const" "__cstring"
                "__literal4" "__literal8" "__mod_init_func"
                "__mod_term_func" "__objc_classlist")))

(defun gen-mach-section ()
  "Generate valid Mach-O section for testing."
  (gen-fmap
   (lambda (data)
     (destructuring-bind (sectname segname addr size offset align flags) data
       (make-mach-section-raw
        :sectname sectname
        :segname segname
        :addr addr
        :size size
        :offset offset
        :align align
        :reloff 0
        :nreloc 0
        :flags flags
        :reserved1 0
        :reserved2 0
        :reserved3 nil)))
   (gen-tuple (gen-section-name)
              (gen-segment-name)
              (gen-integer :min 0 :max #xFFFFFF)
              (gen-integer :min 0 :max #xFFFF)
              (gen-integer :min 512 :max #xFFFFF)
              (gen-one-of '(0 1 2 3 4))
              (gen-integer :min 0 :max #xFFFFFFFF))))

(defun gen-mach-segment-command ()
  "Generate valid Mach-O segment command for testing."
  (gen-bind
   (gen-segment-permissions)
   (lambda (maxprot)
     (gen-bind
      (gen-segment-permissions)
      (lambda (initprot)
        (gen-fmap
         (lambda (data)
           (destructuring-bind (segname vmaddr vmsize fileoff filesize sections) data
             (make-mach-segment-raw
              :cmd +lc-segment-64+
              :cmdsize (+ 72 (* 80 (length sections)))  ; 72 base + 80 per section
              :segname segname
              :vmaddr vmaddr
              :vmsize vmsize
              :fileoff fileoff
              :filesize filesize
              :maxprot maxprot
              :initprot initprot
              :nsects (length sections)
              :flags 0
              :sections sections)))
         (gen-tuple (gen-segment-name)
                    (gen-integer :min 0 :max #xFFFFFFFFFF)
                    (gen-integer :min 0 :max #xFFFFFFFFFF)
                    (gen-integer :min 0 :max #xFFFFFFFFFF)
                    (gen-integer :min 0 :max #xFFFFF)
                    (gen-list-of (gen-mach-section)
                                 :min-length 0 :max-length *max-mach-o-sections*))))))))

(defun gen-mach-load-command-type ()
  "Generate Mach-O load command types."
  (gen-one-of (list +lc-segment+ +lc-segment-64+ +lc-symtab+
                    +lc-dysymtab+ +lc-uuid+ +lc-load-dylib+
                    +lc-rpath+ +lc-code-signature+)))

;;; Typed AST Generators

;; Type-annotated AST node structures
(defstruct (typed-ast (:constructor make-typed-ast-raw))
  "Base structure for typed AST nodes."
  node-type
  source-node)

(defstruct (typed-ast-int (:include typed-ast)
                          (:constructor make-typed-ast-int-raw))
  "Typed integer literal."
  value)

(defstruct (typed-ast-float (:include typed-ast)
                            (:constructor make-typed-ast-float-raw))
  "Typed float literal."
  value)

(defstruct (typed-ast-string (:include typed-ast)
                             (:constructor make-typed-ast-string-raw))
  "Typed string literal."
  value)

(defstruct (typed-ast-boolean (:include typed-ast)
                              (:constructor make-typed-ast-boolean-raw))
  "Typed boolean literal."
  value)

(defstruct (typed-ast-var (:include typed-ast)
                          (:constructor make-typed-ast-var-raw))
  "Typed variable reference."
  name)

(defstruct (typed-ast-binop (:include typed-ast)
                            (:constructor make-typed-ast-binop-raw))
  "Typed binary operation."
  op
  lhs
  rhs)

(defstruct (typed-ast-if (:include typed-ast)
                         (:constructor make-typed-ast-if-raw))
  "Typed conditional expression."
  cond
  then
  else)

(defstruct (typed-ast-lambda (:include typed-ast)
                             (:constructor make-typed-ast-lambda-raw))
  "Typed lambda expression with typed parameters."
  params      ; List of (name . type) pairs
  body)

(defstruct (typed-ast-call (:include typed-ast)
                           (:constructor make-typed-ast-call-raw))
  "Typed function call."
  func
  func-type   ; Function type
  args)

(defstruct (typed-ast-let (:include typed-ast)
                          (:constructor make-typed-ast-let-raw))
  "Typed let binding."
  bindings    ; List of (name . (type . expr))
  body)

(defun gen-typed-primitive-value ()
  "Generate a typed primitive value with its type."
  (gen-bind
   (gen-one-of '(fixnum single-float string boolean))
   (lambda (type)
     (case type
       (fixnum (gen-fmap
                (lambda (v)
                  (make-typed-ast-int-raw
                   :node-type 'fixnum
                   :value v))
                (gen-integer :min -1000 :max 1000)))
       (single-float (gen-fmap
                      (lambda (v)
                        (make-typed-ast-float-raw
                         :node-type 'single-float
                         :value v))
                      (gen-float :min -1000.0 :max 1000.0)))
       (string (gen-fmap
                (lambda (v)
                  (make-typed-ast-string-raw
                   :node-type 'string
                   :value v))
                (gen-string :min-length 0 :max-length 20)))
       (boolean (gen-fmap
                 (lambda (v)
                   (make-typed-ast-boolean-raw
                    :node-type 'boolean
                    :value v))
                 (gen-boolean)))))))

(defun gen-typed-terminal ()
  "Generate typed terminal AST nodes (literals and variables)."
  (gen-bind
   (gen-one-of '(0 1 2))
   (lambda (choice)
     (case choice
       (0 (gen-typed-primitive-value))
       (1 (gen-fmap
           (lambda (name)
             (make-typed-ast-var-raw
              :node-type (generate (gen-primitive-type))
              :name name))
           (gen-symbol :package nil :prefix "VAR")))
       (2 (gen-typed-primitive-value))))))

(defun gen-typed-binop ()
  "Generate typed binary operation AST nodes."
  (gen-bind
   (gen-one-of '(+ - * /))
   (lambda (op)
     (gen-fmap
      (lambda (data)
        (destructuring-bind (lhs rhs) data
          (make-typed-ast-binop-raw
           :node-type 'fixnum
           :op op
           :lhs lhs
           :rhs rhs)))
      (gen-tuple (gen-typed-ast-node :depth 1)
                 (gen-typed-ast-node :depth 1))))))

(defun gen-typed-if ()
  "Generate typed if expression AST nodes."
  (gen-fmap
   (lambda (data)
     (destructuring-bind (cond then else) data
       (make-typed-ast-if-raw
        :node-type (typed-ast-node-type then)
        :cond cond
        :then then
        :else else)))
   (gen-tuple (gen-fmap
               (lambda (v)
                 (make-typed-ast-boolean-raw :node-type 'boolean :value v))
               (gen-boolean))
              (gen-typed-ast-node :depth 1)
              (gen-typed-ast-node :depth 1))))

(defun gen-typed-param ()
  "Generate a typed function parameter (name . type)."
  (gen-fmap
   (lambda (data)
     (destructuring-bind (name type) data
       (cons name type)))
   (gen-tuple (gen-symbol :package nil :prefix "ARG")
              (gen-type-expr :depth 0))))

(defun gen-typed-lambda ()
  "Generate typed lambda expression AST nodes."
  (gen-bind
   (gen-list-of (gen-typed-param) :min-length 0 :max-length 3)
   (lambda (params)
     (gen-fmap
      (lambda (body)
        (let ((ret-type (typed-ast-node-type body))
              (arg-types (mapcar #'cdr params)))
          (make-typed-ast-lambda-raw
           :node-type (list 'function arg-types ret-type)
           :params params
           :body (list body))))
      (gen-typed-ast-node :depth 1)))))

(defun gen-typed-call ()
  "Generate typed function call AST nodes."
  (gen-bind
   (gen-typed-lambda)
   (lambda (func)
     (let* ((fn-type (typed-ast-node-type func))
            (arg-types (second fn-type)))
       (gen-fmap
        (lambda (args)
          (make-typed-ast-call-raw
           :node-type (third fn-type)
           :func func
           :func-type fn-type
           :args args))
        (gen-list-of (gen-typed-ast-node :depth 0)
                     :min-length (length arg-types)
                     :max-length (length arg-types)))))))

(defun gen-typed-let ()
  "Generate typed let binding AST nodes."
  (gen-bind
   (gen-list-of (gen-typed-param) :min-length 1 :max-length 3)
   (lambda (bindings)
     (gen-fmap
      (lambda (data)
        (destructuring-bind (values body) data
          (let ((typed-bindings
                  (loop for (name . type) in bindings
                        for val in values
                        collect (cons name (cons type val)))))
            (make-typed-ast-let-raw
             :node-type (typed-ast-node-type body)
             :bindings typed-bindings
             :body (list body)))))
      (gen-tuple (gen-list-of (gen-typed-ast-node :depth 0)
                              :min-length (length bindings)
                              :max-length (length bindings))
                 (gen-typed-ast-node :depth 1))))))

(defun gen-typed-ast-node (&key (depth *max-type-depth*))
  "Generate AST nodes with type annotations.

   Supports:
   - ast-int, ast-binop, ast-if with type annotations
   - ast-lambda with typed parameters
   - ast-call with function types"
  (let ((effective-depth (max 0 (min depth *max-type-depth*))))
    (if (or (zerop effective-depth)
            (< (random 100 (%pbt-rng))
               (- 100 (* effective-depth 15))))
        ;; Generate terminal node
        (gen-typed-terminal)
        ;; Generate recursive node
        (let ((reduced-depth (1- effective-depth)))
          (gen-bind
           (gen-one-of (list 0 1 2 3 4))
           (lambda (choice)
             (case choice
               (0 (gen-typed-binop))
               (1 (gen-typed-if))
               (2 (gen-typed-lambda))
               (3 (gen-typed-call))
               (4 (gen-typed-terminal)))))))))

;;; Utility Functions for Typed AST

(defun typed-ast-to-sexp (node)
  "Convert a typed AST node to an S-expression for debugging."
  (etypecase node
    (typed-ast-int
     (list 'the (typed-ast-node-type node)
           (typed-ast-int-value node)))
    (typed-ast-float
     (list 'the (typed-ast-node-type node)
           (typed-ast-float-value node)))
    (typed-ast-string
     (list 'the (typed-ast-node-type node)
           (typed-ast-string-value node)))
    (typed-ast-boolean
     (list 'the (typed-ast-node-type node)
           (typed-ast-boolean-value node)))
    (typed-ast-var
     (list 'the (typed-ast-node-type node)
           (typed-ast-var-name node)))
    (typed-ast-binop
     (list 'the (typed-ast-node-type node)
           (list (typed-ast-binop-op node)
                 (typed-ast-to-sexp (typed-ast-binop-lhs node))
                 (typed-ast-to-sexp (typed-ast-binop-rhs node)))))
    (typed-ast-if
     (list 'the (typed-ast-node-type node)
           (list 'if
                 (typed-ast-to-sexp (typed-ast-if-cond node))
                 (typed-ast-to-sexp (typed-ast-if-then node))
                 (typed-ast-to-sexp (typed-ast-if-else node)))))
    (typed-ast-lambda
     (list 'the (typed-ast-node-type node)
           (list* 'lambda
                  (mapcar #'car (typed-ast-lambda-params node))
                  (mapcar #'typed-ast-to-sexp (typed-ast-lambda-body node)))))
    (typed-ast-call
     (list 'the (typed-ast-node-type node)
           (cons (typed-ast-to-sexp (typed-ast-call-func node))
                 (mapcar #'typed-ast-to-sexp (typed-ast-call-args node)))))
    (typed-ast-let
     (list 'the (typed-ast-node-type node)
           (list* 'let
                  (mapcar (lambda (b)
                            (list (car b)
                                  (typed-ast-to-sexp (cddr b))))
                          (typed-ast-let-bindings node))
                  (mapcar #'typed-ast-to-sexp (typed-ast-let-body node)))))))

(defun extract-type-from-ast (node)
  "Extract the type from a typed AST node."
  (typed-ast-node-type node))

;;; Example Properties Using New Generators

(in-suite cl-cc-pbt-suite)

;; Type Expression Properties

(defproperty type-expr-is-sexp
    (type-expr (gen-type-expr))
  (or (symbolp type-expr)
      (and (consp type-expr)
           (symbolp (car type-expr)))))

(defproperty fn-type-has-function-symbol
    (fn-type (gen-fn-type))
  (and (consp fn-type)
       (eq (car fn-type) 'function)
       (= (length fn-type) 3)
       (listp (second fn-type))))

(defproperty type-variables-are-keywords
    (type-var (gen-type-variable))
  (and (symbolp type-var)
       (keywordp type-var)
       (char= (char (symbol-name type-var) 0) #\?)))

;; Mach-O Structure Properties

(defproperty mach-header-has-valid-magic
    (header (gen-mach-header))
  (member (mach-header-magic header)
          (list +mh-magic+ +mh-magic-64+ +mh-cigam+ +mh-cigam-64+)))

(defproperty mach-segment-has-valid-permissions
    (segment (gen-mach-segment-command))
  (and (<= 0 (mach-segment-command-maxprot segment) 7)
       (<= 0 (mach-segment-command-initprot segment) 7)))

(defproperty mach-section-count-matches
    (segment (gen-mach-segment-command))
  (= (mach-segment-command-nsects segment)
     (length (mach-segment-command-sections segment))))

;; Typed AST Properties

(defproperty typed-ast-has-type
    (node (gen-typed-ast-node :depth 2))
  (not (null (typed-ast-node-type node))))

(defproperty typed-lambda-has-function-type
    (node (gen-typed-lambda))
  (let ((type (typed-ast-node-type node)))
    (and (consp type)
         (eq (car type) 'function)
         (= (length type) 3))))

(defproperty typed-binop-returns-numeric
    (node (gen-typed-binop))
  (member (typed-ast-node-type node)
          '(fixnum integer single-float double-float number)))
