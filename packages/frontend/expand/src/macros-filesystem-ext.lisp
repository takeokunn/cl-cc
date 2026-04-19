(in-package :cl-cc/expand)

;;; ─── ANSI CL Compatibility Stubs — Extension Set ───────────────────────────
;;; PrettyPrint API, readtable API, debug/introspection, and compile-file stubs.
;;; All are ANSI CL compatibility shims; no inter-file dependencies.
;;;
;;; See also: macros-filesystem.lisp (core I/O, bit-array, time/room/break)

;;; ─── pprint-related stubs (FR-357) ───────────────────────────────────────────
;;;
;;; All pprint stubs evaluate their args (for side effects) and delegate
;;; the actual print to prin1. The data table below encodes:
;;;   (name . extra-arg-count-before-object)
;;; where extra args are flags like colon-p / tabsize.

(defun %make-pprint-stub-handler (extra-arg-accessors)
  "Build a pprint stub: eval extra args, then (prin1 object stream)."
  (lambda (form env)
    (declare (ignore env))
    (cons 'progn
          (append (mapcar (lambda (acc) (funcall acc form)) extra-arg-accessors)
                  (list (list 'prin1 (third form) (second form)))))))

(let ((with-two-flags  (list #'fourth #'fifth))
      (with-two-flags+ (list #'fourth #'fifth #'sixth)))
  ;; pprint-fill and pprint-linear share the same expansion
  (let ((handler (%make-pprint-stub-handler with-two-flags)))
    (register-macro 'pprint-fill   handler)
    (register-macro 'pprint-linear handler))
  (register-macro 'pprint-tabular (%make-pprint-stub-handler with-two-flags+)))

;;; Stubs that evaluate N args and return nil (no print delegation needed)
(dolist (spec '((pprint-newline  second third)
                (pprint-tab      second third fourth fifth)
                (pprint-indent   second third fourth)))
  (let* ((name (first spec))
         (accessors (mapcar #'symbol-function (rest spec))))
    (register-macro name
      (lambda (form env)
        (declare (ignore env))
        (cons 'progn (append (mapcar (lambda (f) (funcall f form)) accessors)
                             (list nil)))))))

(register-macro 'pprint-logical-block
  (lambda (form env)
    (declare (ignore env))
    (let* ((spec (second form))
           (body (cddr form))
           (stream-symbol (car spec))
           (rest-spec (cddr spec))
           (prefix (getf rest-spec :prefix))
           (per-line-prefix (getf rest-spec :per-line-prefix))
           (suffix (getf rest-spec :suffix)))
      (cons 'progn (append (list prefix per-line-prefix suffix stream-symbol) body)))))

;;; ─── file-string-length stub (FR-591) ────────────────────────────────────────

(register-macro 'file-string-length
  (lambda (form env)
    (declare (ignore env))
    (let ((stream (second form))
          (object (third form))
          (obj-g (gensym "OBJ")))
      (list 'progn stream
            (list 'let (list (list obj-g object))
                  (list 'if (list 'characterp obj-g)
                        (list 'length (list 'string-to-octets (list 'string obj-g) :encoding :utf-8))
                        (list 'length (list 'string-to-octets obj-g :encoding :utf-8))))))))

;;; ─── stream-external-format stub (FR-562) ────────────────────────────────────

(register-macro 'stream-external-format
  (lambda (form env)
    (declare (ignore env))
    (list 'progn (second form) :default)))

;;; ─── trace / untrace stubs (FR-432) ──────────────────────────────────────────

(register-macro 'trace
  (lambda (form env)
    (declare (ignore env))
    (list 'progn (cons 'cl:trace (cdr form)) nil)))

(register-macro 'untrace
  (lambda (form env)
    (declare (ignore env))
    (list 'progn (cons 'cl:untrace (cdr form)) nil)))

;;; ─── step stub (FR-433) ───────────────────────────────────────────────────────

(register-macro 'step
  (lambda (form env)
    (declare (ignore env))
    (list 'cl:step (second form))))

;;; ─── disassemble stub (FR-576) ───────────────────────────────────────────────

(register-macro 'disassemble
  (lambda (form env)
    (declare (ignore env))
    (list 'progn (list 'cl:disassemble (second form)) nil)))

;;; ─── inspect stub (FR-577) ────────────────────────────────────────────────────

(register-macro 'inspect
  (lambda (form env)
    (declare (ignore env))
    (list 'progn (list 'cl:inspect (second form)) nil)))

;;; ─── apropos / apropos-list (FR-435) — registered as host bridges in vm.lisp ──

;;; ─── ed stub (FR-515) ─────────────────────────────────────────────────────────

(register-macro 'ed
  (lambda (form env)
    (declare (ignore env))
    (list 'progn (second form) nil)))

;;; ─── invoke-debugger stub (FR-557) ───────────────────────────────────────────

(register-macro 'invoke-debugger
  (lambda (form env)
    (declare (ignore env))
    (list 'error (second form))))

;;; ─── compiler-let stub (FR-439) ───────────────────────────────────────────────

(register-macro 'compiler-let
  (lambda (form env)
    (declare (ignore env))
    (cons 'let (cons (second form) (cddr form)))))

;;; ─── read-char-no-hang (FR-568) ──────────────────────────────────────────────

(register-macro 'read-char-no-hang
  (lambda (form env)
    (declare (ignore env))
    (list 'read-char
          (or (second form) '*standard-input*)
          (if (third form) (third form) t)
          (fourth form)
          (fifth form))))

;;; ─── FR-358: Readtable API stubs ─────────────────────────────────────────────
;;; cl-cc uses a fixed built-in lexer; readtables are not user-customizable.
;;; All stubs evaluate their args (for side effects) then return a fixed value.
;;;
;;; Format: (macro-name nargs return-value)
;;; NARGS is how many consecutive form args starting at (second form) to include.

(defparameter *readtable-stub-specs*
  '((readtablep                   1 nil)
    (copy-readtable                2 nil)
    (readtable-case                1 :upcase)
    (get-macro-character           2 (values nil nil))
    (get-dispatch-macro-character  3 nil)
    (set-macro-character           4 t)
    (set-dispatch-macro-character  4 t)
    (make-dispatch-macro-character 3 t)
    (set-syntax-from-char          4 t))
  "ANSI CL readtable API stubs: (name nargs return-value).")

(dolist (spec *readtable-stub-specs*)
  (let* ((name    (first  spec))
         (nargs   (second spec))
         (retval  (third  spec)))
    (register-macro name
      (lambda (form env)
        (declare (ignore env))
        (cons 'progn
              (append (loop for i from 1 to nargs collect (nth i form))
                      (list retval)))))))

;;; ─── FR-589: compile-file stub ───────────────────────────────────────────────

(register-macro 'compile-file
  (lambda (form env)
    (declare (ignore env))
    (let ((pathname (second form))
          (path-var (gensym "PATH")))
      (list 'let (list (list path-var pathname))
            (list 'our-load path-var)
            (list 'values (list 'probe-file path-var) nil nil)))))
