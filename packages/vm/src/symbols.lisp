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
             ;; In self-host mode, refuse host CL fallback
             (*vm-self-host-mode*
              (error "Package self-hosting required: cannot intern ~S without runtime package registry"
                     name))
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
                  ;; In self-host mode, refuse host CL fallback
                  (and *vm-self-host-mode*
                       (error "Package self-hosting required: package ~S not found in runtime registry"
                              name))
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

;;; ─── Extended Package Operation Instructions (ANSI CL Ch.11) ────────────────

(define-vm-instruction vm-export-inst (vm-instruction)
  "Export symbols from a package."
  (dst nil :reader vm-dst)
  (symbols nil :reader vm-export-symbols)
  (pkg nil :reader vm-export-pkg)
  (:sexp-tag :export)
  (:sexp-slots dst symbols pkg))

(defmethod execute-instruction ((inst vm-export-inst) state pc labels)
  (declare (ignore labels))
  (let* ((symbols (vm-reg-get state (vm-export-symbols inst)))
         (pkg (vm-reg-get state (vm-export-pkg inst)))
         (result #-cl-cc-self-hosting
                 (if (and cl-cc/runtime::*rt-package-registry*
                          (hash-table-p pkg))
                     (cl-cc/runtime::rt-export symbols pkg)
                     (export symbols pkg))
                 #+cl-cc-self-hosting
                 (cl-cc/runtime::rt-export symbols pkg)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-import-inst (vm-instruction)
  "Import symbols into a package."
  (dst nil :reader vm-dst)
  (symbols nil :reader vm-import-symbols)
  (pkg nil :reader vm-import-pkg)
  (:sexp-tag :import)
  (:sexp-slots dst symbols pkg))

(defmethod execute-instruction ((inst vm-import-inst) state pc labels)
  (declare (ignore labels))
  (let* ((symbols (vm-reg-get state (vm-import-symbols inst)))
         (pkg (vm-reg-get state (vm-import-pkg inst)))
         (result #-cl-cc-self-hosting
                 (if (and cl-cc/runtime::*rt-package-registry*
                          (hash-table-p pkg))
                     (cl-cc/runtime::rt-import symbols pkg)
                     (import symbols pkg))
                 #+cl-cc-self-hosting
                 (cl-cc/runtime::rt-import symbols pkg)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-use-package-inst (vm-instruction)
  "Make packages accessible to another package."
  (dst nil :reader vm-dst)
  (packages-to-use nil :reader vm-use-packages)
  (pkg nil :reader vm-use-pkg)
  (:sexp-tag :use-package)
  (:sexp-slots dst packages-to-use pkg))

(defmethod execute-instruction ((inst vm-use-package-inst) state pc labels)
  (declare (ignore labels))
  (let* ((pkgs (vm-reg-get state (vm-use-packages inst)))
         (pkg (vm-reg-get state (vm-use-pkg inst)))
         (result #-cl-cc-self-hosting
                 (if (and cl-cc/runtime::*rt-package-registry*
                          (hash-table-p pkg))
                     (cl-cc/runtime::rt-use-package pkgs pkg)
                     (use-package pkgs pkg))
                 #+cl-cc-self-hosting
                 (cl-cc/runtime::rt-use-package pkgs pkg)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-unuse-package-inst (vm-instruction)
  "Remove packages from the use-list of another package."
  (dst nil :reader vm-dst)
  (packages-to-unuse nil :reader vm-unuse-packages)
  (pkg nil :reader vm-unuse-pkg)
  (:sexp-tag :unuse-package)
  (:sexp-slots dst packages-to-unuse pkg))

(defmethod execute-instruction ((inst vm-unuse-package-inst) state pc labels)
  (declare (ignore labels))
  (let* ((pkgs (vm-reg-get state (vm-unuse-packages inst)))
         (pkg (vm-reg-get state (vm-unuse-pkg inst)))
         (result #-cl-cc-self-hosting
                 (if (and cl-cc/runtime::*rt-package-registry*
                          (hash-table-p pkg))
                     (cl-cc/runtime::rt-unuse-package pkgs pkg)
                     (unuse-package pkgs pkg))
                 #+cl-cc-self-hosting
                 (cl-cc/runtime::rt-unuse-package pkgs pkg)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-shadow-inst (vm-instruction)
  "Create shadowing symbols in a package."
  (dst nil :reader vm-dst)
  (symbol-names nil :reader vm-shadow-names)
  (pkg nil :reader vm-shadow-pkg)
  (:sexp-tag :shadow)
  (:sexp-slots dst symbol-names pkg))

(defmethod execute-instruction ((inst vm-shadow-inst) state pc labels)
  (declare (ignore labels))
  (let* ((names (vm-reg-get state (vm-shadow-names inst)))
         (pkg (vm-reg-get state (vm-shadow-pkg inst)))
         (result #-cl-cc-self-hosting
                 (if (and cl-cc/runtime::*rt-package-registry*
                          (hash-table-p pkg))
                     (cl-cc/runtime::rt-shadow names pkg)
                     (shadow names pkg))
                 #+cl-cc-self-hosting
                 (cl-cc/runtime::rt-shadow names pkg)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-unintern-inst (vm-instruction)
  "Remove a symbol from its home package."
  (dst nil :reader vm-dst)
  (symbol nil :reader vm-unintern-symbol)
  (pkg nil :reader vm-unintern-pkg)
  (:sexp-tag :unintern)
  (:sexp-slots dst symbol pkg))

(defmethod execute-instruction ((inst vm-unintern-inst) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-unintern-symbol inst)))
         (pkg (vm-reg-get state (vm-unintern-pkg inst)))
         (result #-cl-cc-self-hosting
                 (if (and cl-cc/runtime::*rt-package-registry*
                          (hash-table-p pkg))
                     (cl-cc/runtime::rt-unintern sym pkg)
                     (unintern sym pkg))
                 #+cl-cc-self-hosting
                 (cl-cc/runtime::rt-unintern sym pkg)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-package-name-inst (vm-instruction)
  "Get the name of a package."
  (dst nil :reader vm-dst)
  (pkg nil :reader vm-package-name-pkg)
  (:sexp-tag :package-name)
  (:sexp-slots dst pkg))

(defmethod execute-instruction ((inst vm-package-name-inst) state pc labels)
  (declare (ignore labels))
  (let* ((pkg (vm-reg-get state (vm-package-name-pkg inst)))
         (result #-cl-cc-self-hosting
                 (if (hash-table-p pkg)
                     (cl-cc/runtime::rt-package-name pkg)
                     (package-name pkg))
                 #+cl-cc-self-hosting
                 (cl-cc/runtime::rt-package-name pkg)))
    (vm-reg-set state (vm-dst inst)
                (%vm-maybe-sso-string result))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-list-all-packages-inst (vm-instruction)
  "List all registered packages."
  (dst nil :reader vm-dst)
  (:sexp-tag :list-all-packages)
  (:sexp-slots dst))

(defmethod execute-instruction ((inst vm-list-all-packages-inst) state pc labels)
  (declare (ignore labels))
  (let ((result #-cl-cc-self-hosting
                (if cl-cc/runtime::*rt-package-registry*
                    (cl-cc/runtime::rt-list-all-packages)
                    (list-all-packages))
                #+cl-cc-self-hosting
                (cl-cc/runtime::rt-list-all-packages)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

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
