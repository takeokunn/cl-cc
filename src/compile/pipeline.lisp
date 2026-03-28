;;;; compile/pipeline.lisp - Top-Level Compilation API
(in-package :cl-cc)

(defun compile-expression (expr &key (target :x86_64) type-check)
  (let* ((ctx (make-instance 'compiler-context))
         (expanded-expr (if (typep expr 'ast-node)
                            expr
                            (compiler-macroexpand-all expr)))
         (ast (if (typep expanded-expr 'ast-node)
                  expanded-expr
                  (lower-sexp-to-ast expanded-expr)))
         (inferred-type (when type-check
                          (handler-case (type-check-ast ast)
                            (error (e)
                              (if (eq type-check :strict)
                                  (error e)
                                  (warn "Type check warning: ~A" e)
                                  )))))
         (result-reg (compile-ast ast ctx))
         (instructions (nreverse (ctx-instructions ctx)))
         (full-instructions (append instructions
                                    (list (make-vm-halt
                                                         :reg result-reg))))
         (optimized-instructions (optimize-instructions full-instructions))
         (optimized-program (make-vm-program
                             :instructions optimized-instructions
                             :result-register result-reg)))
    ;; Capture label counter for REPL continuity
    (when *repl-capture-label-counter*
      (setf *repl-capture-label-counter* (ctx-next-label ctx)))
    (make-compilation-result :program optimized-program
                            :assembly (emit-assembly optimized-program :target target)
                            :type (when type-check inferred-type)
                            :cps (if (typep expr 'ast-node)
                                     nil
                                     (handler-case (cps-transform expr)
                                       (error (e) (declare (ignore e)) nil))))))

;;; Standard Library (Higher-Order Functions)
;;; *standard-library-source* is defined in stdlib-source.lisp (loaded before this file).

(defparameter *stdlib-compiled* nil
  "Cache for compiled standard library instructions.")

(defun get-stdlib-forms ()
  "Parse the standard library source into forms."
  (parse-all-forms *standard-library-source*))

(defun parse-source-for-language (source language)
  "Parse SOURCE according to LANGUAGE, returning a list of AST nodes or s-expressions.
:LISP returns s-expressions (compile-toplevel-forms handles lowering).
:PHP calls parse-php-source which returns AST nodes directly."
  (case language
    (:lisp (parse-all-forms source))
    (:php (parse-php-source source))
    (t (error "Unknown language: ~S" language))))

(defun compile-string (source &key (target :x86_64) type-check (language :lisp))
  (let ((forms (parse-source-for-language source language)))
    (if (and (eq language :lisp) (= (length forms) 1))
        (compile-expression (first forms) :target target :type-check type-check)
        ;; Multiple forms (or non-lisp): use compile-toplevel-forms for sequential macro expansion
        (compile-toplevel-forms forms :target target))))

(defun run-string (source &key stdlib)
  "Compile and run SOURCE. When STDLIB is true, include standard library."
  (let* ((*package* (find-package :cl-cc))
         (*accessor-slot-map* (make-hash-table :test #'eq))
         (*defstruct-slot-registry* (make-hash-table :test #'eq))
         (*labels-boxed-fns* nil)
         (result (if stdlib
                     (compile-string-with-stdlib source :target :vm)
                     (compile-string source :target :vm)))
         (program (compilation-result-program result)))
    (run-compiled program)))

(defun compile-string-with-stdlib (source &key (target :x86_64))
  "Compile SOURCE with standard library prepended."
  (let ((stdlib-forms (get-stdlib-forms))
        (user-forms (parse-all-forms source)))
    (compile-toplevel-forms (append stdlib-forms user-forms) :target target)))

(defun our-eval (form)
  "Evaluate FORM by compiling it and running it in the VM.
This is the self-hosting eval — used for compile-time macro expansion
instead of the host CL eval."
  (let* ((result (compile-expression form :target :vm))
         (program (compilation-result-program result)))
    (run-compiled program)))

;;; ─── Self-Hosting Bootstrap ──────────────────────────────────────────────
;;;
;;; Now that compile-expression and run-compiled are available, switch macro
;;; expansion from the host CL eval to our-eval.  From this point on, every
;;; defmacro/macrolet body is compiled and executed by cl-cc's own pipeline —
;;; the fundamental requirement for self-hosting.

(setf *macro-eval-fn* #'our-eval)

;;; ─── REPL Persistent State ────────────────────────────────────────────────
;;;
;;; The REPL accumulates all compiled instructions into a shared pool so that
;;; closures defined in one expression (with entry labels in that expression's
;;; instruction range) remain callable in subsequent expressions.  Each new
;;; compile-string result is appended to *repl-pool-instructions*, and its
;;; labels are inserted into *repl-pool-labels* with a global offset.  Only
;;; the newly-added slice is executed, but the full pool's label table is used
;;; for all label lookups — so cross-call closure invocations work correctly.

(defvar *repl-vm-state* nil
  "Persistent VM state for the interactive REPL.
Reused across form evaluations so that functions and variables defined
in one expression remain accessible in subsequent ones.")

(defvar *repl-accessor-map* nil
  "Persistent accessor map for the REPL.
Accumulates defstruct slot accessor mappings across form evaluations.")

(defvar *repl-pool-instructions* nil
  "Adjustable vector accumulating ALL instructions from the current REPL session.
Enables cross-expression closure calls (body labels remain globally valid).")

(defvar *repl-pool-labels* nil
  "Hash table mapping label names to absolute PCs in *repl-pool-instructions*.")

(defvar *repl-global-vars-persistent* nil
  "Persistent hash table tracking global variable names defined across REPL calls.
When non-nil, this is bound to *repl-global-variables* during compilation
so that variables from (defvar ...) in one REPL call are visible in the next.")

(defvar *repl-defstruct-registry* nil
  "Persistent defstruct slot registry for the REPL.
Accumulates slot info across form evaluations so :include works across calls.")

(defun reset-repl-state ()
  "Reset the REPL persistent state, starting a completely fresh session."
  (setf *repl-vm-state* nil
        *repl-accessor-map* nil
        *repl-pool-instructions* nil
        *repl-pool-labels* nil
        *repl-global-vars-persistent* nil
        *repl-defstruct-registry* nil
        *repl-label-counter* nil))

(defun run-string-repl (source)
  "Compile and run SOURCE using the persistent REPL state.
Unlike run-string, this reuses the VM state (function-registry, class-registry,
heap) across calls so that top-level definitions persist into later expressions.
Cross-expression closure calls work because all instructions share one pool.

Example:
  (run-string-repl \"(defun double (x) (* x 2))\")
  (run-string-repl \"(double 21)\")  ; => 42"
  (unless *repl-vm-state*
    (setf *repl-vm-state*
          (make-instance 'vm-io-state :output-stream *standard-output*)))
  (unless *repl-accessor-map*
    (setf *repl-accessor-map* (make-hash-table :test #'eq)))
  (unless *repl-pool-instructions*
    (setf *repl-pool-instructions*
          (make-array 64 :adjustable t :fill-pointer 0 :element-type t)))
  (unless *repl-pool-labels*
    (setf *repl-pool-labels* (make-hash-table :test #'equal)))
  (unless *repl-global-vars-persistent*
    (setf *repl-global-vars-persistent* (make-hash-table :test #'eq)))
  (unless *repl-defstruct-registry*
    (setf *repl-defstruct-registry* (make-hash-table :test #'eq)))
  (let* ((*package* (find-package :cl-cc))
         (*accessor-slot-map* *repl-accessor-map*)
         (*defstruct-slot-registry* *repl-defstruct-registry*)
         (*labels-boxed-fns* nil)
         ;; Bind persistent globals so compiler-context picks them up
         (*repl-global-variables* *repl-global-vars-persistent*)
         ;; Enable label counter capture so we can persist it
         (*repl-capture-label-counter* t)
         (result (compile-string source :target :vm))
         (program (compilation-result-program result))
         (new-insts (vm-program-instructions program))
         ;; PC where the new code will start in the global pool
         (start-pc (fill-pointer *repl-pool-instructions*)))
    ;; Persist the label counter for the next compilation
    (when (integerp *repl-capture-label-counter*)
      (setf *repl-label-counter* *repl-capture-label-counter*))
    ;; Track any new global variables defined by this compilation
    (dolist (inst new-insts)
      (when (typep inst 'vm-set-global)
        (setf (gethash (vm-global-name inst) *repl-global-vars-persistent*) t)))
    ;; Append new instructions to the shared pool
    (dolist (inst new-insts)
      (vector-push-extend inst *repl-pool-instructions*))
    ;; Merge new labels (with global offset) into the pool label table
    (let ((new-labels (build-label-table new-insts)))
      (maphash (lambda (label local-pc)
                 (setf (gethash label *repl-pool-labels*)
                       (+ start-pc local-pc)))
               new-labels))
    ;; Execute only the new slice, using the full pool for label resolution
    (run-program-slice *repl-pool-instructions* *repl-pool-labels*
                       start-pc *repl-vm-state*)))

;;; ─── Self-Hosting Load ──────────────────────────────────────────────────
;;;
;;; (our-load pathname) reads a file, parses all forms, and compiles+executes
;;; them in the persistent REPL state. This is the key primitive for cl-cc to
;;; load its own source files.

(defun %prescan-in-package (source)
  "Pre-scan SOURCE for an (in-package ...) form and return the package name string.
   Returns nil if not found. Used to set *package* before full parsing so that
   #. read-time eval resolves symbols in the correct package."
  (let ((pos (search "(in-package " source :test #'char-equal)))
    (when pos
      (let* ((start (+ pos (length "(in-package ")))
             (trimmed (string-trim '(#\Space #\Tab) (subseq source start))))
        ;; Handle :pkg, :pkg), "pkg", 'pkg forms
        (cond
          ((and (> (length trimmed) 0) (char= (first (coerce trimmed 'list)) #\:))
           (let ((end (position-if (lambda (c) (or (char= c #\)) (char= c #\Space))) trimmed)))
             (when end (subseq trimmed 1 end))))
          ((and (> (length trimmed) 0) (char= (first (coerce trimmed 'list)) #\"))
           (let ((end (position #\" trimmed :start 1)))
             (when end (subseq trimmed 1 end))))
          (t nil))))))

(defun our-load (pathname &key (verbose nil) (print nil) (if-does-not-exist :error)
                                 (external-format :default))
  "Load a Lisp source file by reading, compiling, and executing each form.
Uses the persistent REPL state so definitions accumulate across forms.
VERBOSE prints the file being loaded. PRINT prints each form's result.
IF-DOES-NOT-EXIST controls behavior when file is missing (:error or nil).
EXTERNAL-FORMAT is accepted but ignored (UTF-8 assumed)."
  (declare (ignore external-format))
  (when (and (eq if-does-not-exist nil) (not (probe-file pathname)))
    (return-from our-load nil))
  (let ((path (namestring (truename pathname))))
    (when verbose
      (format *standard-output* "; Loading ~A~%" path))
    (let ((source (with-open-file (in path :direction :input)
                    (let ((buf (make-string (file-length in))))
                      (read-sequence buf in)
                      buf))))
      ;; Pre-scan for (in-package ...) to set *package* before parsing,
      ;; so #. read-time eval resolves symbols in the correct package.
      (let* ((pkg-name (%prescan-in-package source))
             (pkg (when pkg-name (find-package (string-upcase pkg-name))))
             (*package* (or pkg *package*)))
        ;; Parse all forms and compile/run each through the REPL pipeline
        (let ((forms (parse-all-forms source))
              (last-result nil))
          (flet ((%whitespace-symbol-p (form)
                   (and (symbolp form)
                        (not (null form))
                        (not (keywordp form))
                        (let ((name (symbol-name form)))
                          (and (> (length name) 0)
                               (not (find-if (lambda (c)
                                               (and (graphic-char-p c)
                                                    (not (eql c #\Space))))
                                             name)))))))
            (dolist (form forms last-result)
              (unless (or (%whitespace-symbol-p form)
                          ;; Skip unsupported top-level forms
                          (and (consp form) (member (car form) '(deftype defopcode))))
                (let ((form-str (write-to-string form)))
                  (setf last-result
                        (handler-case (run-string-repl form-str)
                          (error (e)
                            (format *error-output* "; Error loading ~A: ~A~%  Form: ~S~%"
                                    path e form)
                            nil)))
                  (when print
                    (format *standard-output* "~S~%" last-result)))))))))))


(defun run-string-typed (source &key (mode :warn))
  "Compile and run SOURCE with type checking enabled.
   MODE is :WARN (default, log warnings) or :STRICT (signal errors)."
  (let* ((result (compile-string source :target :vm :type-check mode))
         (program (compilation-result-program result)))
    (values (run-compiled program) (compilation-result-type result))))

;;; Native Executable Generation (Mach-O)

(defun %write-native-binary (builder code-bytes output-path)
  "Finalize BUILDER with CODE-BYTES, write Mach-O to OUTPUT-PATH, and mark it executable."
  (cl-cc/binary:add-text-segment builder code-bytes)
  (cl-cc/binary:add-symbol builder "_main" :value 0 :type #x0F :sect 1)
  (let ((mach-o-bytes (cl-cc/binary:build-mach-o builder code-bytes)))
    (cl-cc/binary:write-mach-o-file output-path mach-o-bytes))
  (uiop:run-program (list "chmod" "+x" (namestring output-path)) :ignore-error-status t)
  output-path)

(defun compile-to-native (source &key (arch :x86-64) (output-file "a.out") (language :lisp))
  "Compile SOURCE to a native Mach-O executable.
SOURCE can be a string (single expression) or a list of forms.
ARCH is :X86-64 or :ARM64.
OUTPUT-FILE is the path for the executable.
LANGUAGE is :LISP (default) or :PHP.

Returns the output file path on success."
  (let* ((result (if (stringp source)
                     (compile-string source :target :vm :language language)
                     (compile-expression source :target :vm)))
         (program    (compilation-result-program result))
         (code-bytes (ecase arch
                       (:x86-64 (compile-to-x86-64-bytes program))
                       (:arm64  (compile-to-aarch64-bytes program))))
         (builder    (cl-cc/binary:make-mach-o-builder arch)))
    (%write-native-binary builder code-bytes output-file)))

(defun compile-file-to-native (input-file &key (arch :x86-64) (output-file nil) (language nil))
  "Compile a CL-CC source file to a native Mach-O executable.
INPUT-FILE is the path to the source file.
OUTPUT-FILE defaults to INPUT-FILE with no extension.
LANGUAGE is :LISP or :PHP. When nil, auto-detected from the file extension."
  (let* ((effective-language (or language
                                 (cond ((string= (pathname-type input-file) "php") :php)
                                       (t :lisp))))
         (output (or output-file
                     (make-pathname :type nil :defaults input-file)))
         (source (if (eq effective-language :php)
                     (with-open-file (in input-file :direction :input
                                                    :element-type 'character)
                       (let ((buf (make-string (file-length in))))
                         (read-sequence buf in)
                         buf))
                     (with-open-file (in input-file :direction :input)
                       (let ((forms nil)
                             (*read-eval* nil))
                         (handler-case
                             (loop (push (read in) forms))
                           (end-of-file () nil))
                         (nreverse forms)))))
         (result (if (eq effective-language :php)
                     (compile-string source :target :vm :language :php)
                     (compile-toplevel-forms source :target :vm)))
         (program    (compilation-result-program result))
         (code-bytes (ecase arch
                       (:x86-64 (compile-to-x86-64-bytes program))
                       (:arm64  (compile-to-aarch64-bytes program))))
         (builder    (cl-cc/binary:make-mach-o-builder arch)))
    (%write-native-binary builder code-bytes output)))

;;; Typeclass Macros (Phase 4) — registered here because cl-cc/type loads before compiler

(register-macro 'deftype-class
  (lambda (form env)
    (declare (ignore env))
    ;; Syntax: (deftype-class Name [(:super S1 S2)] (type-param) method-specs...)
    (let* ((class-name (second form))
           (rest (cddr form))
           ;; Check for optional superclass declaration
           (superclasses (when (and (consp (first rest))
                                    (eq (caar rest) :super))
                           (cdar rest)))
           (rest2 (if superclasses (cdr rest) rest))
           (type-param-list (first rest2))
           (type-param (first type-param-list))
           (method-specs (rest rest2))
           (methods-forms nil)
           (default-forms nil))
      (dolist (spec method-specs)
        (let ((method-name (first spec))
              (type-spec (second spec))
              (rest-spec (cddr spec)))
          (push `(cons ',method-name
                       (cl-cc/type:parse-type-specifier ',type-spec))
                methods-forms)
          (when (and rest-spec (eq (first rest-spec) :default))
            (push `(cons ',method-name ,(second rest-spec))
                  default-forms))))
      `(progn
         (cl-cc/type:register-typeclass
          ',class-name
          (cl-cc/type:make-type-class
           :name ',class-name
           :type-param (cl-cc/type:make-type-variable ',type-param)
           :methods (list ,@(nreverse methods-forms))
           :defaults (list ,@(nreverse default-forms))
           :superclasses ',superclasses))
         ',class-name))))

(register-macro 'deftype-instance
  (lambda (form env)
    (declare (ignore env))
    (destructuring-bind (_ class-name type-spec &rest method-impls) form
      (declare (ignore _))
      (let* ((dict-var (intern (format nil "*~A-~A-DICT*"
                                       class-name type-spec)
                               :cl-cc))
             (method-forms
              (mapcar (lambda (impl)
                        (destructuring-bind (name impl-form) impl
                          `(cons ',name ,impl-form)))
                      method-impls)))
        `(progn
           ;; Register in type inference registry
           (cl-cc/type:register-typeclass-instance
            ',class-name
            (cl-cc/type:parse-type-specifier ',type-spec)
            (list ,@method-forms))
           ;; Store dictionary as global variable for VM access
           (defvar ,dict-var (list ,@method-forms))
           ',class-name)))))

