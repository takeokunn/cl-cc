(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; our-load — File loading via run-form-repl
;;;
;;; Contains: %whitespace-symbol-p, %prescan-in-package, our-load,
;;; and VM host-bridge registration for our-load.
;;;
;;; Helper predicates and run-form-repl are in pipeline-repl-load.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun %whitespace-symbol-p (form)
  "T if FORM is a non-NIL, non-keyword symbol whose name has no non-space graphic chars."
  (and (symbolp form) (not (null form)) (not (keywordp form))
       (let ((name (symbol-name form)))
         (and (> (length name) 0)
              (every (lambda (c) (or (not (graphic-char-p c)) (eql c #\Space))) name)))))

(defun %prescan-in-package (source)
  "Pre-scan SOURCE for an (in-package ...) form and return the package name string.
   Returns nil if not found. Used to set *package* before full parsing so that
   #. read-time eval resolves symbols in the correct package."
  (let ((pos (search "(in-package " source :test #'char-equal)))
    (when pos
      (let* ((start (+ pos (length "(in-package ")))
             (trimmed (string-trim '(#\Space #\Tab) (subseq source start))))
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
  "Load a Lisp source file by reading, compiling, and executing each form."
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
      (let* ((pkg-name (%prescan-in-package source))
             (pkg (when pkg-name (find-package (string-upcase pkg-name))))
             (*package* (or pkg *package*))
             (*our-load-host-definition-mode* (%project-source-load-p path))
             (*our-load-current-path* path))
        (let ((forms (parse-all-forms source))
              (last-result nil))
          (dolist (form forms last-result)
            (let* ((package-form-p (and (consp form) (eq (car form) 'in-package)))
                   (whitespace-symbol-p (%whitespace-symbol-p form))
                   (unsupported-p (and (consp form)
                                       (symbolp (car form))
                                       (member (symbol-name (car form)) '("DEFTYPE" "DEFOPCODE")
                                               :test #'string=))))
              (cond
                (package-form-p
                 (let ((pkg (find-package (second form))))
                   (unless pkg
                     (error "Unknown package: ~S" (second form)))
                   (setf *package* pkg)
                   (setf last-result (second form))))
                ((or whitespace-symbol-p unsupported-p)
                 nil)
                (t
                 (setf last-result
                       (handler-case (run-form-repl form)
                         (error (e)
                           (format *error-output* "; Error loading ~A: ~A~%  Form: ~S~%"
                                   path e form)
                           nil)))
                 (when print
                   (format *standard-output* "~S~%" last-result)))))))))))

(eval-when (:load-toplevel :execute)
  (dolist (sym '(run-string-repl our-load))
    (vm-register-host-bridge sym)))
