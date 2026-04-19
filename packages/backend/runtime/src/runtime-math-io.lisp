;;;; packages/backend/runtime/src/runtime-math-io.lisp - CL-CC Runtime: Strings, Chars, Symbols,
;;;;   Hash Tables, CLOS, Conditions, and Misc Operations
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;
;;; Contains: rt-make-string, rt-string-*, rt-char-*, rt-symbol-*, rt-intern,
;;; rt-make-hash-table, rt-gethash/sethash/remhash/clrhash/maphash,
;;; rt-defclass, rt-make-instance, rt-slot-*, rt-class-*, rt-register-method,
;;; rt-signal-error, rt-signal, rt-warn-fn, rt-cerror, handler stubs,
;;; rt-boundp/fboundp, rt-random, rt-coerce, rt-read-from-string, rt-read-sexp,
;;; rt-write-to-string, and other misc operations.
;;;
;;; Tag constants, closure/cons structs, predicate macros, list/array, and
;;; complex math (rt-conjugate etc.) are in runtime.lisp (loads before this).
;;; I/O operations (streams, files, format) are in runtime-io.lisp (loads after this).
;;;
;;; Load order: after runtime.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Strings
;;; ------------------------------------------------------------

(defun rt-make-string (len &optional (char #\Space))
  (make-string len :initial-element char))
(defun rt-string-length (s) (length s))
(defun rt-string-ref (s idx) (char s idx))
(defun rt-string-set (s idx c) (setf (char s idx) c))
(define-rt-binary-predicate rt-string=          string=)
(define-rt-binary-predicate rt-string<          string<)
(define-rt-binary-predicate rt-string>          string>)
(define-rt-binary-predicate rt-string<=         string<=)
(define-rt-binary-predicate rt-string>=         string>=)
(define-rt-binary-predicate rt-string-equal-ci  string-equal)
(define-rt-binary-predicate rt-string-lessp     string-lessp)
(define-rt-binary-predicate rt-string-greaterp  string-greaterp)
(define-rt-binary-predicate rt-string-not-equal string-not-equal)
(define-rt-binary-predicate rt-string-not-greaterp string-not-greaterp)
(define-rt-binary-predicate rt-string-not-lessp    string-not-lessp)
(defun rt-string-upcase (s) (string-upcase s))
(defun rt-string-downcase (s) (string-downcase s))
(defun rt-string-capitalize (s) (string-capitalize s))
(defun rt-string-trim (bag s) (string-trim bag s))
(defun rt-string-left-trim (bag s) (string-left-trim bag s))
(defun rt-string-right-trim (bag s) (string-right-trim bag s))
(defun rt-search-string (pattern target) (search pattern target))
(defun rt-subseq (s start &optional end)
  (if end (subseq s start end) (subseq s start)))
(defun rt-concatenate-seqs (type &rest seqs)
  (apply #'concatenate type seqs))

;;; ------------------------------------------------------------
;;; Characters
;;; ------------------------------------------------------------

(defun rt-char (s idx) (char s idx))
(defun rt-char-code (c) (char-code c))
(defun rt-code-char (n) (code-char n))
(define-rt-binary-predicate rt-char-equal-cs      char=)
(define-rt-binary-predicate rt-char-lt-cs         char<)
(define-rt-binary-predicate rt-char-gt-cs         char>)
(define-rt-binary-predicate rt-char-le-cs         char<=)
(define-rt-binary-predicate rt-char-ge-cs         char>=)
(define-rt-binary-predicate rt-char-ne-cs         char/=)
(define-rt-binary-predicate rt-char-equal-ci      char-equal)
(define-rt-binary-predicate rt-char-not-equal-ci  char-not-equal)
(define-rt-binary-predicate rt-char-lessp-ci      char-lessp)
(define-rt-binary-predicate rt-char-greaterp-ci   char-greaterp)
(define-rt-binary-predicate rt-char-not-lessp-ci  char-not-lessp)
(define-rt-binary-predicate rt-char-not-greaterp-ci char-not-greaterp)
(defun rt-char-upcase (c) (char-upcase c))
(defun rt-char-downcase (c) (char-downcase c))
(define-rt-predicate rt-alpha-char-p    alpha-char-p)
(define-rt-predicate rt-digit-char-p    digit-char-p)
(define-rt-predicate rt-alphanumericp   alphanumericp)
(define-rt-predicate rt-upper-case-p    upper-case-p)
(define-rt-predicate rt-lower-case-p    lower-case-p)
(define-rt-predicate rt-both-case-p     both-case-p)
(define-rt-predicate rt-graphic-char-p  graphic-char-p)
(define-rt-predicate rt-standard-char-p standard-char-p)
(defun rt-digit-char (w &optional (radix 10)) (digit-char w radix))
(defun rt-char-name (c) (char-name c))
(defun rt-name-char (s) (name-char s))
(defun rt-parse-integer (s &key (start 0) end (radix 10) junk-allowed)
  (parse-integer s :start start :end end :radix radix :junk-allowed junk-allowed))

;;; ------------------------------------------------------------
;;; Symbols
;;; ------------------------------------------------------------

(defun rt-symbol-name (sym) (symbol-name sym))
(defun rt-intern (name &optional (pkg *package*)) (intern name pkg))
(defun rt-make-symbol (name) (make-symbol name))
(defun rt-gensym (&optional (prefix "G")) (gensym prefix))
(defun rt-symbol-value (sym) (symbol-value sym))
(defun rt-set-symbol-value (sym val) (setf (symbol-value sym) val))
(defun rt-symbol-plist (sym) (symbol-plist sym))
(defun rt-get-prop (sym indicator) (get sym indicator))
(defun rt-put-prop (sym indicator val) (setf (get sym indicator) val))
(defun rt-remprop (sym indicator) (remprop sym indicator))

;;; ------------------------------------------------------------
;;; Hash Tables
;;; ------------------------------------------------------------

(defun rt-make-hash-table (&key (test #'eql) size)
  (if size
      (make-hash-table :test test :size size)
      (make-hash-table :test test)))
(defun rt-gethash (key ht) (gethash key ht))
(defun rt-sethash (key ht val) (setf (gethash key ht) val))
(defun rt-remhash (key ht) (remhash key ht))
(defun rt-clrhash (ht) (clrhash ht))
(defun rt-hash-count (ht) (hash-table-count ht))
(defun rt-hash-test (ht) (hash-table-test ht))
(defun rt-maphash (fn ht) (maphash fn ht))
(defun rt-hash-keys (ht)
  (let (keys) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) ht) keys))
(defun rt-hash-values (ht)
  (let (vals) (maphash (lambda (k v) (declare (ignore k)) (push v vals)) ht) vals))

;;; ------------------------------------------------------------
;;; CLOS
;;; ------------------------------------------------------------

(defun rt-defclass (name direct-supers slots)
  (declare (ignore name direct-supers slots))
  nil)

(defun rt-defclass-from-reg ()
  nil)

(defun rt-make-instance (class &rest initargs)
  (apply #'make-instance class initargs))

(defun rt-make-instance-0 (class)
  (make-instance class))

(defun rt-slot-value (obj slot-name)
  (slot-value obj slot-name))

(defun rt-slot-set (obj slot-name val)
  (setf (slot-value obj slot-name) val))

(defun rt-slot-boundp (obj slot-name)
  (if (slot-boundp obj slot-name) 1 0))

(defun rt-slot-makunbound (obj slot-name)
  (slot-makunbound obj slot-name))

(defun rt-slot-exists-p (obj slot-name)
  (if (slot-exists-p obj slot-name) 1 0))

(defun rt-class-name (class)
  (if (hash-table-p class)
      (gethash :__name__ class)
      (class-name class)))

(defun rt-class-of (obj) (class-of obj))
(defun rt-find-class (name) (find-class name nil))
(defun rt-register-method (gf specs method) (declare (ignore gf specs method)) nil)
(defun rt-call-generic (gf &rest args) (apply gf args))

;;; ------------------------------------------------------------
;;; Conditions / Error Handling
;;; ------------------------------------------------------------

(defun rt-signal-error (condition)
  (error condition))

(defun rt-signal (condition)
  (signal condition))

(defun rt-warn-fn (condition)
  (warn "~A" condition))

(defun rt-cerror (continue-string condition)
  (cerror continue-string "~A" condition))

(defun rt-establish-handler ()
  nil)

(defun rt-remove-handler ()
  nil)

(defun rt-push-handler (type fn)
  (declare (ignore type fn)) nil)

(defun rt-pop-handler ()
  nil)

(defun rt-bind-restart (name fn thunk)
  (declare (ignore name fn))
  (funcall thunk))

(defun rt-invoke-restart (name &rest args)
  (apply #'invoke-restart name args))

;;; ------------------------------------------------------------
;;; Misc
;;; ------------------------------------------------------------

(define-rt-predicate rt-boundp  boundp)
(define-rt-predicate rt-fboundp fboundp)
(defun rt-makunbound (sym) (makunbound sym))
(defun rt-fmakunbound (sym) (fmakunbound sym))
(defun rt-random (n) (random n))
(defun rt-make-random-state (&optional state)
  (if state (make-random-state state) (make-random-state)))
(defun rt-get-universal-time () (get-universal-time))
(defun rt-get-internal-real-time () (get-internal-real-time))
(defun rt-get-internal-run-time () (get-internal-run-time))
(defun rt-read-from-string (s) (read-from-string s))
(defun rt-read-sexp (stream) (read stream))
(defun rt-coerce (x type) (coerce x type))
(defun rt-write-to-string (x) (write-to-string x))
