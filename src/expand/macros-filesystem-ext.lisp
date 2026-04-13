(in-package :cl-cc)

;;; ─── ANSI CL Compatibility Stubs — Extension Set ───────────────────────────
;;; PrettyPrint API, readtable API, debug/introspection, and compile-file stubs.
;;; All are ANSI CL compatibility shims; no inter-file dependencies.
;;;
;;; See also: macros-filesystem.lisp (core I/O, bit-array, time/room/break)

;;; ─── pprint-related stubs (FR-357) ───────────────────────────────────────────

(our-defmacro pprint-fill (stream object &optional colon-p at-sign-p)
  "Pretty-print list with fill style (stub — delegates to prin1)."
  `(progn ,colon-p ,at-sign-p (prin1 ,object ,stream)))

(our-defmacro pprint-linear (stream object &optional colon-p at-sign-p)
  "Pretty-print list with linear style (stub — delegates to prin1)."
  `(progn ,colon-p ,at-sign-p (prin1 ,object ,stream)))

(our-defmacro pprint-tabular (stream object &optional colon-p at-sign-p tabsize)
  "Pretty-print list with tabular style (stub)."
  `(progn ,colon-p ,at-sign-p ,tabsize (prin1 ,object ,stream)))

(our-defmacro pprint-newline (kind &optional stream)
  "Emit a conditional newline (stub — ignored in cl-cc)."
  `(progn ,kind ,stream nil))

(our-defmacro pprint-tab (kind column colinc &optional stream)
  "Move to column for tabbing (stub — ignored in cl-cc)."
  `(progn ,kind ,column ,colinc ,stream nil))

(our-defmacro pprint-indent (relative-to n &optional stream)
  "Set indentation (stub — ignored in cl-cc)."
  `(progn ,relative-to ,n ,stream nil))

(our-defmacro pprint-logical-block (spec &rest body)
  "Execute BODY as a logical block for pretty printing (stub)."
  (let* ((stream-symbol (car spec))
         (rest-spec     (cddr spec))  ; skip stream-sym and list
         (prefix        (getf rest-spec :prefix))
         (per-line-prefix (getf rest-spec :per-line-prefix))
         (suffix        (getf rest-spec :suffix)))
    `(progn ,prefix ,per-line-prefix ,suffix ,stream-symbol ,@body)))

;;; ─── file-string-length stub (FR-591) ────────────────────────────────────────

(our-defmacro file-string-length (stream object)
  "Return the number of UTF-8 octets needed to write OBJECT to STREAM."
  (let ((obj-g (gensym "OBJ")))
    `(progn ,stream
       (let ((,obj-g ,object))
         (if (characterp ,obj-g)
             (length (string-to-octets (string ,obj-g) :encoding :utf-8))
             (length (string-to-octets ,obj-g :encoding :utf-8)))))))

;;; ─── stream-external-format stub (FR-562) ────────────────────────────────────

(our-defmacro stream-external-format (stream)
  "Return the external format of STREAM (stub returns :default)."
  `(progn ,stream :default))

;;; ─── trace / untrace stubs (FR-432) ──────────────────────────────────────────

(our-defmacro trace (&rest function-names)
  "Enable tracing for FUNCTION-NAMES via the host SBCL runtime."
  `(progn (cl:trace ,@function-names) nil))

(our-defmacro untrace (&rest function-names)
  "Disable tracing for FUNCTION-NAMES via the host SBCL runtime."
  `(progn (cl:untrace ,@function-names) nil))

;;; ─── step stub (FR-433) ───────────────────────────────────────────────────────

(our-defmacro step (form)
  "Single-step through FORM via the host SBCL runtime."
  `(cl:step ,form))

;;; ─── disassemble stub (FR-576) ───────────────────────────────────────────────

(our-defmacro disassemble (function)
  "Print disassembly of FUNCTION via the host SBCL runtime."
  `(progn (cl:disassemble ,function) nil))

;;; ─── inspect stub (FR-577) ────────────────────────────────────────────────────

(our-defmacro inspect (object)
  "Inspect OBJECT via the host SBCL runtime."
  `(progn (cl:inspect ,object) nil))

;;; ─── apropos / apropos-list (FR-435) — registered as host bridges in vm.lisp ──

;;; ─── ed stub (FR-515) ─────────────────────────────────────────────────────────

(our-defmacro ed (&optional x)
  "Invoke editor on X (stub — no-op in cl-cc)."
  `(progn ,x nil))

;;; ─── invoke-debugger stub (FR-557) ───────────────────────────────────────────

(our-defmacro invoke-debugger (condition)
  "Invoke the debugger on CONDITION (stub — signals error in cl-cc)."
  `(error ,condition))

;;; ─── compiler-let stub (FR-439) ───────────────────────────────────────────────

(our-defmacro compiler-let (bindings &rest body)
  "Bind variables at compile time (stub — acts as let at runtime in cl-cc)."
  `(let ,bindings ,@body))

;;; ─── read-char-no-hang (FR-568) ──────────────────────────────────────────────

(our-defmacro read-char-no-hang (&optional (stream '*standard-input*)
                                  (eof-error-p t) eof-value recursive-p)
  "Return next char if available without waiting, else nil (stub — delegates to read-char)."
  `(read-char ,stream ,eof-error-p ,eof-value ,recursive-p))

;;; ─── FR-358: Readtable API stubs ─────────────────────────────────────────────
;;; cl-cc uses a fixed built-in lexer; readtables are not user-customizable.
;;; These stubs accept args for ANSI CL compatibility without signaling errors.

(our-defmacro readtablep (x)
  "Return nil (cl-cc has no user-defined readtables)."
  `(progn ,x nil))

(our-defmacro copy-readtable (&optional from to)
  "Return nil (cl-cc readtable stub)."
  `(progn ,from ,to nil))

(our-defmacro readtable-case (readtable)
  "Return :upcase (cl-cc always uses upcase)."
  `(progn ,readtable :upcase))

(our-defmacro set-macro-character (char fn &optional non-terminating-p readtable)
  "No-op stub — cl-cc's reader is not user-extensible."
  `(progn ,char ,fn ,non-terminating-p ,readtable t))

(our-defmacro get-macro-character (char &optional readtable)
  "Return (values nil nil) — no user-defined macro chars."
  `(progn ,char ,readtable (values nil nil)))

(our-defmacro set-dispatch-macro-character (disp-char sub-char fn &optional readtable)
  "No-op stub."
  `(progn ,disp-char ,sub-char ,fn ,readtable t))

(our-defmacro get-dispatch-macro-character (disp-char sub-char &optional readtable)
  "Return nil — no user-defined dispatch chars."
  `(progn ,disp-char ,sub-char ,readtable nil))

(our-defmacro make-dispatch-macro-character (char &optional non-terminating-p readtable)
  "No-op stub."
  `(progn ,char ,non-terminating-p ,readtable t))

(our-defmacro set-syntax-from-char (to-char from-char &optional to-readtable from-readtable)
  "No-op stub."
  `(progn ,to-char ,from-char ,to-readtable ,from-readtable t))

;;; ─── FR-589: compile-file stub ───────────────────────────────────────────────

(our-defmacro compile-file (pathname &rest keys)
  ;; Load PATHNAME (cl-cc compiles by loading). Extra keyword args ignored.
  (let ((path-var (gensym "PATH")))
    (declare (ignore keys))
    `(let ((,path-var ,pathname))
       (our-load ,path-var)
       (values (probe-file ,path-var) nil nil))))
