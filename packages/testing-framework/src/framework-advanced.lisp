;;;; tests/framework-advanced.lisp — CL-CC Test Framework (Advanced Features)
;;;; Parameterized tests, nesting, snapshots, pipeline testing, combinatorial, flaky.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Dynamic Variables for Advanced Features
;;; ------------------------------------------------------------

(defvar *testing-context* nil
  "Stack of nested testing labels, accumulated as a string path.")

(defvar *update-snapshots* nil
  "When t, overwrite snapshot files on assert-snapshot mismatch.")

;;; ------------------------------------------------------------
;;; FR-015 — testing: Nested Sub-Cases
;;; ------------------------------------------------------------

(defmacro testing (label &body body)
  "Run BODY as a named sub-case within the current test.
TAP output will include the context path: outer > inner.
Uses *testing-context* to accumulate nesting depth."
  (let ((ctx-var (gensym "CTX")))
    `(let* ((,ctx-var (if *testing-context*
                          (format nil "~A > ~A" *testing-context* ,label)
                          ,label))
            (*testing-context* ,ctx-var))
       (handler-case
           (progn ,@body)
         (test-failure (c)
           ;; Re-signal with context prepended to the message
           (let ((orig-msg (test-failure-message c)))
             (error 'test-failure
                    :message (format nil "~A~%  context: ~A"
                                     orig-msg ,ctx-var))))))))

;;; ------------------------------------------------------------
;;; FR-016 — assert-snapshot: Snapshot Testing
;;; ------------------------------------------------------------

(defun %snapshot-path (name)
  "Return the full path for snapshot NAME."
  (concatenate 'string *snapshot-dir* name ".snap"))

(defun %read-snapshot (path)
  "Read and return the saved snapshot value from PATH, or return the
 symbol :snapshot-not-found if the file does not exist."
  (handler-case
      (with-open-file (stream path :direction :input)
        (read stream))
    (file-error () :snapshot-not-found)))

(defun %write-snapshot (path value)
  "Write VALUE to the snapshot file at PATH, creating directories as needed."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write value :stream stream)))

(defmacro assert-snapshot (name form)
  "Assert that FORM evaluates to the same value as the saved snapshot NAME.
On first run (no snapshot file), saves the result.
When *update-snapshots* is t, overwrites the saved file with the current value."
  (let ((actual-var  (gensym "ACTUAL"))
        (path-var    (gensym "PATH"))
        (saved-var   (gensym "SAVED")))
    `(let* ((,actual-var ,form)
            (,path-var   (%snapshot-path ,name))
            (,saved-var  (%read-snapshot ,path-var)))
       (cond
         ;; First run: no snapshot yet — save and pass
         ((eq ,saved-var :snapshot-not-found)
          (%write-snapshot ,path-var ,actual-var)
          t)
         ;; Update mode: overwrite snapshot unconditionally
         (*update-snapshots*
          (%write-snapshot ,path-var ,actual-var)
          t)
          ;; Normal run: compare
          ((not (equal ,saved-var ,actual-var))
           (%fail-test (format nil "assert-snapshot ~S mismatch" ,name)
                       :expected ,saved-var
                       :actual   ,actual-var
                       :form     ',form))
          (t t)))))

;;; ------------------------------------------------------------
;;; FR-014 — deftest-each: Parameterized Tests
;;; ------------------------------------------------------------

(defun %deftest-each-check-vars (base-name vars)
  "Validate the binding list for a DEFTEST-EACH form."
  (unless (and (listp vars) (every #'symbolp vars))
    (error "DEFTEST-EACH ~A requires a binding list of symbols after :CASES, got ~S"
           base-name vars))
  vars)

(defun %deftest-each-case-values (base-name vars case-entry)
  "Return CASE-ENTRY values after checking label and arity."
  (unless (and (consp case-entry) (stringp (first case-entry)))
    (error "DEFTEST-EACH ~A case must start with a string label, got ~S"
           base-name case-entry))
  (let ((case-vals (rest case-entry)))
    (unless (= (length vars) (length case-vals))
      (error "DEFTEST-EACH ~A case ~S binds ~D vars but supplies ~D values"
             base-name (first case-entry) (length vars) (length case-vals)))
    case-vals))

(defmacro deftest-each (base-name docstring &rest args)
  "Define one test per entry in CASES.
Each case is a list whose first element is the case label string and
whose remaining elements are bound to the variables in the VARS list.

Syntax:
  (deftest-each base-name
    \"docstring\"
    :cases ((\"label\" val ...) ...)
    (var ...)
    body...)

Generates tests named SOURCE/BASE-NAME [label] for each case so
parameterized tests from different files do not silently collide in the global registry."
  (let* ((cases-pos (position :cases args))
         (cases     (if cases-pos (nth (1+ cases-pos) args) nil))
         (body      (if cases-pos (nthcdr (+ 2 cases-pos) args) args)))
    (unless cases-pos
      (error "DEFTEST-EACH ~A requires a :CASES clause" base-name))
    (unless (listp cases)
      (error "DEFTEST-EACH ~A requires :CASES to be a list, got ~S" base-name cases))
    (destructuring-bind (vars &rest body-forms) body
      (unless body-forms
        (error "DEFTEST-EACH ~A requires at least one body form" base-name))
      (let* ((checked-vars (%deftest-each-check-vars base-name vars))
             (source-id  (pathname-name
                          (or *compile-file-pathname*
                              *load-pathname*
                              *default-pathname-defaults*)))
             (expansions
               (loop for case-entry in cases
                     for case-vals = (%deftest-each-case-values base-name checked-vars case-entry)
                     for case-label = (first case-entry)
                     for test-name = (intern
                                      (format nil "~A/~A [~A]"
                                              (string-upcase source-id)
                                              (symbol-name base-name)
                                              case-label))
                     for bindings = (mapcar #'list checked-vars case-vals)
                     collect `(deftest ,test-name
                                ,docstring
                                (let ,bindings
                                  ,@body-forms)))))
        `(progn ,@expansions)))))

;;; ------------------------------------------------------------
;;; Flaky Detection
;;; ------------------------------------------------------------

(defun %detect-flaky (all-run-results repeat)
  "Given a list of repeat result-lists, find tests with inconsistent status."
  (let ((by-name (make-hash-table)))
    (dolist (run-results all-run-results)
      (dolist (r run-results)
        (let ((name (getf r :name))
              (status (getf r :status)))
          (push status (gethash name by-name)))))
    (let ((flaky '()))
      (maphash (lambda (name statuses)
                 (let ((pass-count (count :pass statuses))
                       (total (length statuses)))
                   (when (and (< pass-count total) (> pass-count 0))
                     (push (list name pass-count total) flaky))))
               by-name)
      (when flaky
        (format t "# Flaky tests detected (inconsistent across ~A runs):~%" repeat)
        (dolist (f flaky)
          (format t "#   ~A: passed ~A/~A runs~%"
                  (first f) (second f) (third f)))))))
