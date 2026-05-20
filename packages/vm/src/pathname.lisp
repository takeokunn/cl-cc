;;;; pathname.lisp -- VM instructions for host-backed ANSI pathnames

(in-package :cl-cc/vm)

;; FR-595: Pathname system
(define-vm-instruction vm-pathname (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :pathname) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-pathname) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:pathname (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-make-pathname (vm-instruction)
  (dst nil :reader vm-dst)
  (host-reg nil :reader vm-mkpn-host-reg)
  (device-reg nil :reader vm-mkpn-device-reg)
  (directory-reg nil :reader vm-mkpn-directory-reg)
  (name-reg nil :reader vm-mkpn-name-reg)
  (type-reg nil :reader vm-mkpn-type-reg)
  (version-reg nil :reader vm-mkpn-version-reg)
  (defaults-reg nil :reader vm-mkpn-defaults-reg))

(defun %vm-append-make-pathname-arg (args key reg state)
  (if reg (append args (list key (vm-reg-get state reg))) args))

(defmethod execute-instruction ((inst vm-make-pathname) state pc labels)
  (declare (ignore labels))
  (let ((args nil))
    (setf args (%vm-append-make-pathname-arg args :host (vm-mkpn-host-reg inst) state))
    (setf args (%vm-append-make-pathname-arg args :device (vm-mkpn-device-reg inst) state))
    (setf args (%vm-append-make-pathname-arg args :directory (vm-mkpn-directory-reg inst) state))
    (setf args (%vm-append-make-pathname-arg args :name (vm-mkpn-name-reg inst) state))
    (setf args (%vm-append-make-pathname-arg args :type (vm-mkpn-type-reg inst) state))
    (setf args (%vm-append-make-pathname-arg args :version (vm-mkpn-version-reg inst) state))
    (setf args (%vm-append-make-pathname-arg args :defaults (vm-mkpn-defaults-reg inst) state))
    (vm-reg-set state (vm-dst inst) (cl:apply #'cl:make-pathname args))
    (values (1+ pc) nil nil)))

;; Pathname accessors
(define-vm-unary-instruction vm-pathname-host :pathname-host "Get pathname host.")
(define-vm-unary-instruction vm-pathname-device :pathname-device "Get pathname device.")
(define-vm-unary-instruction vm-pathname-directory :pathname-directory "Get pathname directory.")
(define-vm-unary-instruction vm-pathname-name :pathname-name "Get pathname name.")
(define-vm-unary-instruction vm-pathname-type :pathname-type "Get pathname type.")
(define-vm-unary-instruction vm-namestring :namestring "Convert to namestring.")
(define-vm-unary-instruction vm-file-namestring :file-namestring "Get file-namestring.")
(define-vm-unary-instruction vm-directory-namestring :directory-namestring "Get directory-namestring.")

(define-simple-instruction vm-pathname-host :unary cl:pathname-host)
(define-simple-instruction vm-pathname-device :unary cl:pathname-device)
(define-simple-instruction vm-pathname-directory :unary cl:pathname-directory)
(define-simple-instruction vm-pathname-name :unary cl:pathname-name)
(define-simple-instruction vm-pathname-type :unary cl:pathname-type)
(define-simple-instruction vm-namestring :unary cl:namestring)
(define-simple-instruction vm-file-namestring :unary cl:file-namestring)
(define-simple-instruction vm-directory-namestring :unary cl:directory-namestring)

(define-vm-instruction vm-merge-pathnames (vm-instruction)
  (dst nil :reader vm-dst) (path-reg nil :reader vm-mpn-path-reg)
  (defaults-reg nil :reader vm-mpn-defaults-reg))

(defmethod execute-instruction ((inst vm-merge-pathnames) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (cl:merge-pathnames (vm-reg-get state (vm-mpn-path-reg inst))
                                  (vm-reg-get state (vm-mpn-defaults-reg inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-enough-namestring (vm-instruction)
  (dst nil :reader vm-dst) (path-reg nil :reader vm-ens-path-reg)
  (defaults-reg nil :reader vm-ens-defaults-reg))

(defmethod execute-instruction ((inst vm-enough-namestring) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (cl:enough-namestring (vm-reg-get state (vm-ens-path-reg inst))
                                    (vm-reg-get state (vm-ens-defaults-reg inst))))
  (values (1+ pc) nil nil))

;; FR-596: Logical pathnames
(define-vm-instruction vm-logical-pathname (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :logical-pathname) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-logical-pathname) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:logical-pathname (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-logical-pathname-translations (vm-instruction)
  (dst nil :reader vm-dst) (host-reg nil :reader vm-lpt-host-reg))

(defmethod execute-instruction ((inst vm-logical-pathname-translations) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (cl:logical-pathname-translations (vm-reg-get state (vm-lpt-host-reg inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-translate-logical-pathname (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src))

(defmethod execute-instruction ((inst vm-translate-logical-pathname) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (cl:translate-logical-pathname (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

;; FR-597: Filesystem operations
(define-vm-instruction vm-probe-file (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :probe-file) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-probe-file) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:probe-file (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-file-write-date (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :file-write-date) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-file-write-date) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:file-write-date (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-directory (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :directory) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-directory) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:directory (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-rename-file (vm-instruction)
  (dst nil :reader vm-dst) (old-reg nil :reader vm-rn-old-reg)
  (new-reg nil :reader vm-rn-new-reg))

(defmethod execute-instruction ((inst vm-rename-file) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (cl:rename-file (vm-reg-get state (vm-rn-old-reg inst))
                              (vm-reg-get state (vm-rn-new-reg inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-delete-file (vm-instruction)
  (src nil :reader vm-src) (:sexp-tag :delete-file) (:sexp-slots src))

(defmethod execute-instruction ((inst vm-delete-file) state pc labels)
  (declare (ignore labels))
  (cl:delete-file (vm-reg-get state (vm-src inst)))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-ensure-directories-exist (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :ensure-directories-exist) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-ensure-directories-exist) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (pathspec created) (cl:ensure-directories-exist (vm-reg-get state (vm-src inst)))
    (vm-reg-set state (vm-dst inst) pathspec)
    (values (1+ pc) created nil)))
