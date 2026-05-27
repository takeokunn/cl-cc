(in-package :cl-cc/vm)

;;; VM Symbol and Character Instructions
;;;
;;; This file provides VM instructions for symbol manipulation
;;; (symbol-name, make-symbol, intern, gensym, keywordp) and
;;; character predicates (digit-char-p, alpha-char-p, upper/lower-case-p,
;;; char-upcase/downcase, stringp, characterp, parse-integer, alphanumericp).
;;;

;;; ─── Symbol Manipulation Instructions ───────────────────────────────────────

(define-vm-instruction vm-symbol-name (vm-instruction)
  "Get the name string of a symbol."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :symbol-name)
  (:sexp-slots dst src))

(define-vm-instruction vm-make-symbol (vm-instruction)
  "Create an uninterned symbol from a string."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :make-symbol)
  (:sexp-slots dst src))

(define-vm-instruction vm-intern-symbol (vm-instruction)
  "Intern a string as a symbol. Optional package designator."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (pkg nil :reader vm-intern-pkg)
  (:sexp-tag :intern)
  (:sexp-slots dst src pkg))

(define-vm-instruction vm-add-package-local-nickname (vm-instruction)
  "Add a package-local nickname."
  (dst nil :reader vm-dst)
  (pkg nil :reader vm-local-nickname-pkg)
  (nick nil :reader vm-local-nickname-nick)
  (target nil :reader vm-local-nickname-target)
  (:sexp-tag :add-package-local-nickname)
  (:sexp-slots dst pkg nick target))

(define-vm-instruction vm-remove-package-local-nickname (vm-instruction)
  "Remove a package-local nickname."
  (dst nil :reader vm-dst)
  (pkg nil :reader vm-local-nickname-pkg)
  (nick nil :reader vm-local-nickname-nick)
  (target nil :reader vm-local-nickname-target)
  (:sexp-tag :remove-package-local-nickname)
  (:sexp-slots dst pkg nick target))

(define-vm-instruction vm-find-package (vm-instruction)
  "Find a package by designator through the runtime package registry."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :find-package)
  (:sexp-slots dst src))

(define-vm-instruction vm-find-symbol (vm-instruction)
  "Find a symbol by name in a package designator."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (pkg nil :reader vm-find-symbol-pkg)
  (:sexp-tag :find-symbol)
  (:sexp-slots dst src pkg))

(define-vm-instruction vm-gensym-inst (vm-instruction)
  "Generate a unique uninterned symbol."
  (dst nil :reader vm-dst)
  (:sexp-tag :gensym)
  (:sexp-slots dst))

(define-vm-instruction vm-keywordp (vm-instruction)
  "Test if value is a keyword symbol. Returns 1 if true, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :keywordp)
  (:sexp-slots dst src))

(define-simple-instruction vm-symbol-name :unary symbol-name)

(defmethod execute-instruction ((inst vm-symbol-name) state pc labels)
  (declare (ignore labels))
  (let ((result (%vm-maybe-sso-string (symbol-name (vm-reg-get state (vm-src inst))))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-make-symbol :unary make-symbol)

(defun %vm-host-package-designator (designator)
  (if (hash-table-p designator)
      (or (gethash :host-package designator)
          (gethash :name designator))
      designator))

(defun %vm-host-package-local-nickname-function (name)
  (or (find-symbol name :cl)
      (let ((pkg (find-package :sb-ext)))
        (and pkg (find-symbol name pkg)))))

(defun %vm-find-package-local-nickname (name &optional (package *package*))
  (let ((fn (%vm-host-package-local-nickname-function "PACKAGE-LOCAL-NICKNAMES")))
    (when (and fn package)
      (let ((entry (assoc (string name) (funcall fn package) :test #'string=)))
        (when entry (cdr entry))))))

(defmethod execute-instruction ((inst vm-intern-symbol) state pc labels)
  (declare (ignore labels))
  (let* ((name (%vm-host-string (vm-reg-get state (vm-src inst))))
          (pkg-designator (when (vm-intern-pkg inst)
                             (%vm-host-string (vm-reg-get state (vm-intern-pkg inst)))))
         (result
           ;; Try runtime package intern first (self-hosting path via *runtime-intern-fn*)
           (cond
             ((and cl-cc/bootstrap:*runtime-intern-fn* pkg-designator)
              (funcall cl-cc/bootstrap:*runtime-intern-fn* name pkg-designator))
             (cl-cc/bootstrap:*runtime-intern-fn*
              (funcall cl-cc/bootstrap:*runtime-intern-fn* name))
             (pkg-designator
              (intern name (or (%vm-find-package-local-nickname pkg-designator)
                               (find-package pkg-designator))))
             (t
              (intern name)))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-add-package-local-nickname) state pc labels)
  (declare (ignore labels))
  (let* ((pkg-designator (vm-reg-get state (vm-local-nickname-pkg inst)))
         (nickname (vm-reg-get state (vm-local-nickname-nick inst)))
         (target (vm-reg-get state (vm-local-nickname-target inst)))
         (fn (%vm-host-package-local-nickname-function "ADD-PACKAGE-LOCAL-NICKNAME"))
         (result (funcall fn nickname
                          (%vm-host-package-designator target)
                          (%vm-host-package-designator pkg-designator))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-remove-package-local-nickname) state pc labels)
  (declare (ignore labels))
  (let* ((pkg-designator (vm-reg-get state (vm-local-nickname-pkg inst)))
         (nickname (vm-reg-get state (vm-local-nickname-nick inst)))
         (fn (%vm-host-package-local-nickname-function "REMOVE-PACKAGE-LOCAL-NICKNAME"))
         (result (funcall fn nickname (%vm-host-package-designator pkg-designator))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-find-package) state pc labels)
  (declare (ignore labels))
  (let* ((name (vm-reg-get state (vm-src inst)))
         (result (or
                  ;; Package-local nicknames first (ANSI CL semantics)
                  (%vm-find-package-local-nickname name)
                  ;; Runtime package registry (self-hosting path — PRIMARY)
                  (and cl-cc/bootstrap:*runtime-find-package-fn*
                       (funcall cl-cc/bootstrap:*runtime-find-package-fn* name))
                  ;; Host CL fallback (bootstrap phase only)
                  (find-package name))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-find-symbol) state pc labels)
  (declare (ignore labels))
  (let* ((name (string (vm-reg-get state (vm-src inst))))
         (pkg-designator (when (vm-find-symbol-pkg inst)
                           (vm-reg-get state (vm-find-symbol-pkg inst))))
         (runtime-pkg (if (hash-table-p pkg-designator)
                          pkg-designator
                          (and cl-cc/bootstrap:*runtime-find-package-fn*
                               pkg-designator
                               (funcall cl-cc/bootstrap:*runtime-find-package-fn* pkg-designator))))
          (host-pkg-designator (if (hash-table-p pkg-designator)
                                   (or (gethash :name pkg-designator)
                                       (gethash :host-package pkg-designator))
                                   (or (and pkg-designator
                                            (%vm-find-package-local-nickname pkg-designator))
                                       pkg-designator))))
    (multiple-value-bind (sym status)
        (if (hash-table-p runtime-pkg)
            (let* ((table (gethash :symbols runtime-pkg))
                   (exports (gethash :exports runtime-pkg))
                   (found (and table (gethash name table))))
              (if found
                  (values found (if (member found exports :test #'eq) :external :internal))
                  ;; Not found in runtime table — fall back to host CL lookup.
                  ;; The runtime package may mirror a host package via :host-package.
                  (find-symbol name host-pkg-designator)))
            (if host-pkg-designator
                (find-symbol name host-pkg-designator)
                (find-symbol name)))
      (vm-reg-set state (vm-dst inst) sym)
      (setf (vm-values-list state) (list sym status))
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-gensym-inst) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (gensym))
  (values (1+ pc) nil nil))

(define-simple-instruction vm-keywordp :pred1 keywordp)

;;; ─── Character Predicate Instructions ───────────────────────────────────────

;; define-vm-unary-instruction is defined in vm.lisp (available to all vm/ files).

(define-vm-unary-instruction vm-digit-char-p  :digit-char-p  "Test if character is a digit. Returns weight or nil.")
(define-vm-unary-instruction vm-alpha-char-p  :alpha-char-p  "Test if character is alphabetic. Returns 1/0.")
(define-vm-unary-instruction vm-upper-case-p  :upper-case-p  "Test if character is upper case. Returns 1/0.")
(define-vm-unary-instruction vm-lower-case-p  :lower-case-p  "Test if character is lower case. Returns 1/0.")
(define-vm-unary-instruction vm-char-upcase   :char-upcase   "Upcase a character. DST = uppercase of SRC.")
(define-vm-unary-instruction vm-char-downcase :char-downcase "Downcase a character. DST = lowercase of SRC.")
(define-vm-unary-instruction vm-stringp       :stringp       "Test if value is a string. Returns 1/0.")
(define-vm-unary-instruction vm-characterp    :characterp    "Test if value is a character. Returns 1/0.")
(define-vm-unary-instruction vm-parse-integer :parse-integer "Parse an integer from a string. DST = integer value.")
(define-vm-unary-instruction vm-alphanumericp :alphanumericp "Test if character is alphanumeric. Returns 1/0.")

(defmethod execute-instruction ((inst vm-digit-char-p) state pc labels)
  (declare (ignore labels))
  (let* ((ch (vm-reg-get state (vm-src inst)))
         (result (vm-digit-char-p-value ch)))
    (vm-reg-set state (vm-dst inst) (or result nil))
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-alpha-char-p :pred1 vm-alpha-char-p-value)
(define-simple-instruction vm-upper-case-p :pred1 vm-upper-case-p-value)
(define-simple-instruction vm-lower-case-p :pred1 vm-lower-case-p-value)
(define-simple-instruction vm-char-upcase :unary char-upcase)
(define-simple-instruction vm-char-downcase :unary char-downcase)
(define-simple-instruction vm-stringp :pred1 stringp)
(define-simple-instruction vm-characterp :pred1 characterp)
(define-simple-instruction vm-parse-integer :unary parse-integer)
(define-simple-instruction vm-alphanumericp :pred1 vm-alphanumericp-value)

;;; ─── Sleep (FR-681) ─────────────────────────────────────────────────────────

(define-vm-unary-instruction vm-sleep-inst :sleep "Suspend execution for N seconds.")
(define-simple-instruction vm-sleep-inst :unary sleep)
