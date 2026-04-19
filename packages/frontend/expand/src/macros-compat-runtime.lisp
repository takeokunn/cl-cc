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
          (t (list '%coerce-runtime value type-form))))
      (list '%coerce-runtime value type-form)))))

;;; Compile-time Evaluation

(defvar *load-time-value-cache* (make-hash-table :test #'equal)
  "Memoizes LOAD-TIME-VALUE expansion results during compiler macroexpansion.")

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
      (setf cached (eval form))
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
