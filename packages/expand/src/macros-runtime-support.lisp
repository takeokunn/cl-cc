(in-package :cl-cc/expand)

;;; Declaration (silently ignored)

(register-macro 'declare
  (lambda (form env)
    (declare (ignore form env))
    nil))

;;; Global declaration (silently ignored — same semantics as declare)

(register-macro 'declaim
  (lambda (form env)
    (declare (ignore form env))
    nil))

;;; Scope with Declarations

(register-macro 'locally
  (lambda (form env)
    (declare (ignore env))
  ;; FR-397: preserve declarations by wrapping in (let () decls body)
  (let* ((forms (cdr form))
         (decls (remove-if-not (lambda (f) (and (consp f) (eq (car f) 'declare))) forms))
         (body  (remove-if (lambda (f) (and (consp f) (eq (car f) 'declare))) forms)))
    (if decls
        (cons 'let (cons nil (append decls body)))
        (cons 'progn body)))))

;; PROGV (FR-102) — dynamic variable binding
;; Uses vm-progv-enter/vm-progv-exit to save and restore global-vars around body.
(register-macro 'progv
  (lambda (form env)
    (declare (ignore env))
  "Bind SYMBOLS to VALUES dynamically for the duration of BODY."
    (let ((symbols (second form))
          (values (third form))
          (body (cdddr form))
          (syms-var (gensym "SYMS"))
          (vals-var (gensym "VALS"))
          (saved-var (gensym "SAVED")))
      (list 'let* (list (list syms-var symbols)
                        (list vals-var values)
                        (list saved-var (list '%progv-enter syms-var vals-var)))
            (list 'unwind-protect
                  (cons 'progn body)
                  (list '%progv-exit saved-var))))))

;;; File I/O

(register-macro 'with-open-file
  (lambda (form env)
    (declare (ignore env))
  "Bind VAR to an open stream for PATH, execute BODY, then close the stream.
   STREAM-SPEC is (var path &rest open-options)."
    (let* ((stream-spec (second form))
           (body (cddr form))
           (var (first stream-spec))
           (path (second stream-spec))
           (options (cddr stream-spec)))
      (list 'let (list (list var (append (list 'open path) options)))
            (list 'unwind-protect (cons 'progn body)
                  (list 'close var))))))

;;; Warning Output

(register-macro 'error
  (lambda (form env)
    (declare (ignore env))
    (let ((datum (second form))
          (args (cddr form)))
      (if (and args (stringp datum))
          (list 'error (cons 'format (cons nil (cons datum args))))
          form))))

(register-macro 'warn
  (lambda (form env)
    (declare (ignore env))
    (let ((fmt (second form))
          (args (cddr form)))
      (list 'progn
            (cons 'format
                  (cons t
                        (cons (concatenate 'string "~&WARNING: "
                                           (if (stringp fmt) fmt "~A"))
                              args)))
            nil))))

;;; Hash Table Utilities

(register-macro 'copy-hash-table
  (lambda (form env)
    (declare (ignore env))
    (let ((ht (second form))
          (ht-var (gensym "HT"))
          (new-var (gensym "NEW"))
          (k-var (gensym "K"))
          (v-var (gensym "V")))
      (list 'let (list (list ht-var ht))
            (list 'let (list (list new-var (list 'make-hash-table :test (list 'hash-table-test ht-var))))
                  (list 'maphash (list 'lambda (list k-var v-var)
                                       (list 'setf (list 'gethash k-var new-var) v-var))
                        ht-var)
                  new-var)))))

;;; Type Coercion

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((package (or (find-package :cffi)
                     (make-package :cffi :use '(:cl)))))
    (export (intern "FOREIGN-FUNCALL" package) package)))

(defun %foreign-funcall-bridge-symbol ()
  "Return the VM bridge symbol used by the FOREIGN-FUNCALL macro."
  (let ((package (or (find-package :cl-cc/vm)
                     (error "CL-CC/VM package is unavailable for FOREIGN-FUNCALL expansion"))))
    (intern "%FOREIGN-FUNCALL" package)))

(defun %expand-foreign-funcall (form)
  "Expand CFFI-compatible FOREIGN-FUNCALL to the VM host bridge."
  (cons (%foreign-funcall-bridge-symbol) (cdr form)))

(register-macro 'foreign-funcall
  (lambda (form env)
    (declare (ignore env))
    (%expand-foreign-funcall form)))

(register-macro (intern "FOREIGN-FUNCALL" (find-package :cffi))
  (lambda (form env)
    (declare (ignore env))
    (%expand-foreign-funcall form)))

(dolist (name '("CFFI:FOREIGN-FUNCALL" "CFFI::FOREIGN-FUNCALL"))
  (register-macro (intern name :cl-cc/expand)
    (lambda (form env)
      (declare (ignore env))
      (%expand-foreign-funcall form))))

(defun %coerce-runtime-symbol-for-form (value type-form)
  "Return the runtime helper symbol in the caller's package when possible."
  (let ((package (or (and (symbolp type-form) (symbol-package type-form))
                     (and (symbolp value) (symbol-package value))
                     (and (consp type-form)
                          (eq (car type-form) 'quote)
                          (symbolp (second type-form))
                          (symbol-package (second type-form)))
                     *package*)))
    (intern "%COERCE-RUNTIME" package)))

(register-macro 'coerce
  (lambda (form env)
    (declare (ignore env))
  ;; FR-630: quoted literal types → direct call; dynamic types → runtime dispatch
  (let ((value (second form))
        (type-form (third form)))
    (if (and (consp type-form) (eq (car type-form) 'quote))
      (let ((type (second type-form)))
        (cond
          ((and (symbolp type) (member type '(string simple-string base-string)))
           (list 'coerce-to-string value))
          ((eq type 'list)
           (list 'coerce-to-list value))
          ((or (and (symbolp type) (member type '(vector simple-vector)))
                (and (consp type) (member (car type) '(vector simple-array array))))
           (list 'coerce-to-vector value))
          ((eq type 'character) (list 'character value))
          ((member type '(float single-float double-float short-float long-float))
           (list 'float value))
          (t (list (%coerce-runtime-symbol-for-form value type-form)
                   value type-form))))
      (list (%coerce-runtime-symbol-for-form value type-form)
            value type-form)))))

;;; Compile-time Evaluation

(defvar *load-time-value-cache* (make-hash-table :test #'equal)
  "Memoizes LOAD-TIME-VALUE expansion results during compiler macroexpansion.")

(defvar *macro-eval-fn*)

;; LOAD-TIME-VALUE — evaluate at compile time, splice in the quoted result.
(register-macro 'load-time-value
  (lambda (call-form env)
    (declare (ignore env))
    (let ((form (second call-form))
          (read-only-p (third call-form)))
      (declare (ignore read-only-p))
      (multiple-value-bind (cached present-p)
          (gethash form *load-time-value-cache*)
        (unless present-p
          (setf cached (funcall *macro-eval-fn* form))
          (setf (gethash form *load-time-value-cache*) cached))
          (list 'quote cached)))))

;;; FR-1206: Module/feature system — *features*, *modules*, provide, require

(register-macro 'provide
  (lambda (form env)
    (declare (ignore env))
  "Mark MODULE-NAME as loaded by pushing its string name onto *modules*."
    (let ((module-name (second form))
          (mod (gensym "MOD")))
      (list 'let (list (list mod (list 'string module-name)))
            (list 'pushnew mod '*modules* :test '(function string=))
            mod))))

(register-macro 'require
  (lambda (form env)
    (declare (ignore env))
  "Load files in PATHNAMES if MODULE-NAME is not already in *modules*."
    (let ((module-name (second form))
          (pathnames (third form))
          (mod (gensym "MOD"))
          (pn  (gensym "PATHS")))
      (list 'let (list (list mod (list 'string module-name))
                       (list pn pathnames))
            (list 'unless (list 'member mod '*modules* :test '(function string=))
                  (list 'if pn
                        (list 'dolist (list 'p pn) (list 'our-load 'p))
                        (list 'warn "Module ~A not loaded" mod)))
            mod))))
