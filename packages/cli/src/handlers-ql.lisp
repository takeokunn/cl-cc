;;;; cli/src/handlers-ql.lisp — Local Quicklisp/ASDF Integration (FR-763)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;
;;; Registry, qlfile/lock parsing, ASDF dependency resolution, and
;;; system-to-native compilation helpers.
;;;
;;; Loaded before handlers.lisp; the main subcommand handlers (%do-install,
;;; %do-compile-system, etc.) call these utilities.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/cli)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :ql)
    (defpackage :ql
      (:use :cl)
      (:export #:quickload #:system-apropos #:update-all-dists))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Local Quicklisp/ASDF integration (FR-763)
;;; ─────────────────────────────────────────────────────────────────────────

(defparameter *quicklisp-client-directory*
  (merge-pathnames #P"quicklisp/" (user-homedir-pathname))
  "Default Quicklisp client directory searched by cl-cc ASDF integration.")

(defparameter *local-project-directories*
  (list (truename #P"./"))
  "Local project directories scanned for .asd files before Quicklisp fallback.")

(defun %cl-cc-registry-path ()
  (merge-pathnames #P".cl-cc-systems.sexp" (user-homedir-pathname)))

(defun %read-system-registry ()
  (let ((path (%cl-cc-registry-path)))
    (if (probe-file path)
        (with-open-file (in path :direction :input)
          (or (read in nil nil) nil))
        nil)))

(defun %write-system-registry (registry)
  (let ((path (%cl-cc-registry-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write registry :stream out :pretty t))
    registry))

(defun %normalize-system-name (thing)
  (string-downcase (string thing)))

(defun %read-asd-forms (path)
  (with-open-file (in path :direction :input)
    (loop for form = (read in nil :eof)
          until (eq form :eof)
          collect form)))

(defun %asd-defsystem-form (path)
  (find-if (lambda (form)
             (and (consp form)
                  (member (car form) '(asdf:defsystem defsystem) :test #'eq)))
           (%read-asd-forms path)))

(defun %plist-value (plist key)
  (loop for (k v) on plist by #'cddr
        when (eq k key) do (return v)))

(defun %asd-system-name (path)
  (let ((form (%asd-defsystem-form path)))
    (or (and form (second form)) (pathname-name path))))

(defun %asd-dependencies (path)
  "Parse PATH's DEFSYSTEM :DEPENDS-ON without invoking Quicklisp shell commands."
  (let ((form (%asd-defsystem-form path)))
    (mapcar #'%normalize-system-name
            (or (and form (%plist-value (cddr form) :depends-on)) nil))))

(defun %register-asd (path)
  (let* ((truename (namestring (truename path)))
         (name (%normalize-system-name (%asd-system-name path)))
         (entry (list :name name :path truename :depends-on (%asd-dependencies path)))
         (registry (remove name (%read-system-registry)
                           :key (lambda (e) (getf e :name))
                           :test #'string=)))
    (%write-system-registry (cons entry registry))
    entry))

(defun %unregister-system (name)
  (let* ((normalized (%normalize-system-name name))
         (old (%read-system-registry))
         (new (remove normalized old :key (lambda (e) (getf e :name)) :test #'string=)))
    (%write-system-registry new)
    (/= (length old) (length new))))

(defun %registry-system-entry (name)
  (find (%normalize-system-name name) (%read-system-registry)
        :key (lambda (e) (getf e :name)) :test #'string=))

(defun %ensure-registered-systems-visible ()
  (dolist (entry (%read-system-registry))
    (let ((path (getf entry :path)))
      (when (probe-file path)
        (pushnew (uiop:pathname-directory-pathname (pathname path))
                 asdf:*central-registry* :test #'equal))))
  (dolist (dir *local-project-directories*)
    (when (probe-file dir)
      (pushnew (truename dir) asdf:*central-registry* :test #'equal)))
  (let ((local (merge-pathnames #P"local-projects/" *quicklisp-client-directory*)))
    (when (probe-file local)
      (pushnew (truename local) asdf:*central-registry* :test #'equal))))

(defun %split-words (line)
  (let ((words nil)
        (start nil))
    (loop for i from 0 below (length line)
          for ch = (char line i)
          do (if (find ch " 	")
                 (when start
                   (push (subseq line start i) words)
                   (setf start nil))
                 (unless start (setf start i)))
          finally (when start (push (subseq line start) words)))
    (nreverse words)))

(defun %parse-qlfile (&optional (path #P"qlfile"))
  "Parse a Bundler-style qlfile into dependency plists."
  (when (probe-file path)
    (with-open-file (in path :direction :input)
      (loop for line = (read-line in nil nil)
            while line
            for trimmed = (string-trim '(#\Space #\Tab) line)
            unless (or (string= trimmed "") (char= (char trimmed 0) #\#))
              collect (let ((words (%split-words trimmed)))
                        (list :source (first words)
                              :name (second words)
                              :args (cddr words)))))))

(defun %read-qlfile-lock (&optional (path #P"qlfile.lock"))
  (when (probe-file path)
    (with-open-file (in path :direction :input)
      (read in nil nil))))

(defun %write-qlfile-lock (entries &optional (path #P"qlfile.lock"))
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write (list :format :cl-cc-qlfile-lock-v1
                 :generated-at (get-universal-time)
                 :entries entries)
           :stream out :pretty t))
  path)

(defun %sha256-file (path)
  "Return a SHA256 hex digest for PATH using the existing codegen helper."
  (cl-cc/codegen:wasm-file-content-hash path :bits 256))

(defun %verify-sha256 (path expected)
  (let ((actual (%sha256-file path)))
    (unless (or (null expected) (string= (string-downcase expected) (string-downcase actual)))
      (error "SHA256 mismatch for ~A: expected ~A, got ~A" path expected actual))
    actual))

(defparameter *download-timeout-seconds* 300
  "Maximum wall-clock seconds allowed for a single file download.")

(defun %download-file (url output &key sha256)
  "Download URL to OUTPUT using curl/fetch and verify optional SHA256."
  (ensure-directories-exist output)
  (let ((timeout (write-to-string *download-timeout-seconds*)))
    (cond
      ((let ((fe (find-symbol "FIND-EXECUTABLE" :uiop))) (and fe (funcall fe "curl")))
       (uiop:run-program (list "curl" "--max-time" timeout "-L" "-f" "-o" (namestring output) url)
                         :output :interactive :error-output :interactive))
      ((let ((fe (find-symbol "FIND-EXECUTABLE" :uiop))) (and fe (funcall fe "fetch")))
       (uiop:run-program (list "fetch" "-T" timeout "-o" (namestring output) url)
                         :output :interactive :error-output :interactive))
      (t (error "No HTTP downloader found (need curl or fetch)"))))
  (%verify-sha256 output sha256)
  output)

(defun %extract-tar-gz (archive directory)
  "Extract ARCHIVE into DIRECTORY with the system tar implementation."
  (ensure-directories-exist (merge-pathnames #P".keep" directory))
  (unless (let ((fe (find-symbol "FIND-EXECUTABLE" :uiop))) (and fe (funcall fe "tar")))
    (error "Cannot extract ~A: tar executable not found" archive))
  (uiop:run-program (list "tar" "-xzf" (namestring archive) "-C" (namestring directory))
                    :output :interactive :error-output :interactive)
  directory)

(defun %quicklisp-dist-url ()
  "Return a conservative Quicklisp dist archive URL used by cl-cc update."
  "https://beta.quicklisp.org/quicklisp/quicklisp.tar")

(defun %quicklisp-update-dists (&optional package)
  "Update Quicklisp metadata when Quicklisp exists, otherwise refresh qlfile.lock."
  (let* ((ql-package (find-package :ql))
         (update (and ql-package (find-symbol "UPDATE-ALL-DISTS" ql-package))))
    (cond
      ((and update (fboundp update)
            (not (eq (symbol-function update) #'ql-update-all-dists)))
       (funcall update :prompt nil))
      (t
       (let* ((entries (%parse-qlfile))
              (selected (if package
                            (remove-if-not (lambda (entry)
                                             (string= (%normalize-system-name (getf entry :name))
                                                      (%normalize-system-name package)))
                                           entries)
                            entries)))
         (%write-qlfile-lock selected))))))

(defun ql-quickload (system &key silent verbose)
  "Compatibility wrapper for ql:quickload used by cl-cc's local registry."
  (declare (ignore verbose))
  (or (%quickload-system-if-available system)
      (progn
        (%ensure-asdf-system system)
        (asdf:load-system system)
        (unless silent (format t "Loaded ~A~%" system))
        system)))

(defun ql-system-apropos (term)
  "Compatibility wrapper for ql:system-apropos over ASDF and the local registry."
  (let ((needle (string-downcase (string term)))
        (results nil))
    (dolist (entry (%read-system-registry))
      (when (search needle (getf entry :name) :test #'char-equal)
        (push (getf entry :name) results)))
    (dolist (system (asdf:registered-systems))
      (let ((name (%normalize-system-name system)))
        (when (search needle name :test #'char-equal)
          (pushnew name results :test #'string=))))
    (sort results #'string<)))

(defun ql-update-all-dists (&key prompt)
  (declare (ignore prompt))
  (%quicklisp-update-dists))

(eval-when (:load-toplevel :execute)
  (setf (fdefinition (intern "QUICKLOAD" :ql)) #'ql-quickload
        (fdefinition (intern "SYSTEM-APROPOS" :ql)) #'ql-system-apropos
        (fdefinition (intern "UPDATE-ALL-DISTS" :ql)) #'ql-update-all-dists))

(defun %quickload-system-if-available (name)
  "Try to load NAME through Quicklisp when Quicklisp is present.
Returns true when Quicklisp successfully loaded the system."
  (let* ((ql-package (find-package :ql))
          (quickload (and ql-package (find-symbol "QUICKLOAD" ql-package))))
    (when (and quickload
               (fboundp quickload)
               (not (eq (symbol-function quickload) #'ql-quickload)))
      (handler-case
          (progn
            (funcall quickload name :silent t)
            t)
        (error () nil)))))

(defun %find-system-or-quickload (name)
  "Find ASDF system NAME, trying registered paths first and Quicklisp second."
  (%ensure-registered-systems-visible)
  (or (asdf:find-system name nil)
      (and (%quickload-system-if-available name)
           (asdf:find-system name nil))))

(defun %ensure-asdf-system (name)
  "Return ASDF system NAME or signal a CLI-friendly missing-system error."
  (or (%find-system-or-quickload name)
      (error "ASDF system not found: ~A. Install it with Quicklisp or register a local .asd file with `cl-cc install <system.asd>`." name)))

(defun %toposort-systems (names)
  (let ((seen (make-hash-table :test #'equal))
        (visiting (make-hash-table :test #'equal))
        (out nil))
    (labels ((deps (name)
               (or (getf (%registry-system-entry name) :depends-on)
                   (ignore-errors
                     (mapcar #'%normalize-system-name
                             (asdf:system-depends-on (asdf:find-system name nil))))
                   nil))
             (visit (name)
               (let ((n (%normalize-system-name name)))
                 (unless (gethash n seen)
                   (when (gethash n visiting)
                     (error "Cyclic ASDF dependency involving ~A" n))
                   (setf (gethash n visiting) t)
                   (dolist (dep (deps n)) (visit dep))
                   (remhash n visiting)
                   (setf (gethash n seen) t)
                   (push n out)))))
      (dolist (name names) (visit name))
      (nreverse out))))

(defun %asdf-component-source-files (component)
  (let ((children (ignore-errors (asdf:component-children component))))
    (if children
        (mapcan #'%asdf-component-source-files children)
        (let ((path (ignore-errors (asdf:component-pathname component))))
          (if (and path (string= (or (pathname-type path) "") "lisp"))
              (list (namestring path))
              nil)))))

(defun %system-source-files (system-name)
  (let ((system (asdf:find-system system-name nil)))
    (when system
      (remove-duplicates (%asdf-component-source-files system) :test #'equal))))

(defun %concatenate-system-source (system-name)
  (%ensure-registered-systems-visible)
  (with-output-to-string (out)
    (dolist (name (%toposort-systems (list system-name)))
      (ignore-errors (asdf:load-system name))
      (dolist (file (%system-source-files name))
        (format out "~%;;; system ~A file ~A~%" name file)
        (write-string (%read-file file) out)
        (terpri out)))))

(defun %compile-system-to-native (system-name output arch compress kwargs &key bolt bolt-profile)
  (let* ((source (%concatenate-system-source system-name))
         (tmp (merge-pathnames (make-pathname :name (format nil "cl-cc-system-~A" (gensym))
                                             :type "lisp")
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
            (with-open-file (out tmp :direction :output :if-exists :supersede :if-does-not-exist :create)
              (write-string source out))
            (apply #'compile-file-to-native (namestring tmp)
                   :arch arch :output-file output :language :lisp :compress compress
                   (append (if bolt (list :bolt t :bolt-profile bolt-profile) nil)
                           kwargs)))
      (ignore-errors (delete-file tmp)))))
