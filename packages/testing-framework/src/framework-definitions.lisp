(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Suite Definition
;;; ------------------------------------------------------------

(defmacro defsuite (name &key description parent (parallel t))
  "Define a test suite. Stores metadata in *suite-registry*.
Use :parallel NIL for suites that must always run sequentially."
  `(progn
     (setf *suite-registry*
           (persist-assoc *suite-registry* ',name
                          (list :name ',name
                                :description ,description
                                :parent ',parent
                                :parallel ,parallel
                                :before-each '()
                                :after-each '())))
     ',name))

(defmacro in-suite (name)
  "Set the current active suite."
  `(setf *current-suite* ',name))

;;; ------------------------------------------------------------
;;; Test Definition
;;; ------------------------------------------------------------

(defun %parse-deftest-body (forms)
  "Parse body forms, extracting optional docstring and keyword args.
Returns (values docstring timeout depends-on tags body-forms)."
  (let ((docstring nil)
        (timeout nil)
        (depends-on nil)
        (tags nil)
        (rest forms))
    (when (and rest (stringp (car rest)))
      (setf docstring (car rest)
            rest (cdr rest)))
    (loop while (and rest (keywordp (car rest)))
          do (let ((key (pop rest))
                   (val (pop rest)))
               (case key
                 (:timeout   (setf timeout val))
                 (:depends-on (setf depends-on val))
                 (:tags      (setf tags val)))))
    (values docstring timeout depends-on tags rest)))

(defmacro deftest (name &body body)
  "Define a test. Syntax:
     (deftest name
       \"optional docstring\"
       :timeout 5
       :depends-on other-test
       :tags '(:tag1)
       body-form...)"
  (multiple-value-bind (docstring timeout depends-on tags body-forms)
      (%parse-deftest-body body)
    `(progn
       (setf *test-registry*
             (persist-assoc *test-registry* ',name
                            (list :name ',name
                                  :fn (lambda () ,@body-forms)
                                  :suite *current-suite*
                                  :timeout ,timeout
                                  :depends-on ',depends-on
                                  :tags ,tags
                                  :docstring ,docstring
                                  :source-file (or *compile-file-pathname* *load-pathname*))))
       ',name)))

;;; ------------------------------------------------------------
;;; Fixtures
;;; ------------------------------------------------------------

(defmacro defbefore (when-spec suite-spec &body body)
  "Register a before-each fixture for the given suite.
   (defbefore :each (suite-name) body...)"
  (declare (ignore when-spec))
  (let ((suite-name (if (listp suite-spec) (car suite-spec) suite-spec)))
    `(let ((entry (persist-lookup *suite-registry* ',suite-name)))
       (when entry
         (let ((new-entry (copy-list entry)))
           (setf (getf new-entry :before-each)
                 (append (getf new-entry :before-each)
                         (list (lambda () ,@body))))
           (setf *suite-registry*
                 (persist-assoc *suite-registry* ',suite-name new-entry)))))))

(defmacro defafter (when-spec suite-spec &body body)
  "Register an after-each fixture for the given suite.
   (defafter :each (suite-name) body...)"
  (declare (ignore when-spec))
  (let ((suite-name (if (listp suite-spec) (car suite-spec) suite-spec)))
    `(let ((entry (persist-lookup *suite-registry* ',suite-name)))
       (when entry
         (let ((new-entry (copy-list entry)))
           (setf (getf new-entry :after-each)
                 (append (getf new-entry :after-each)
                         (list (lambda () ,@body))))
           (setf *suite-registry*
                 (persist-assoc *suite-registry* ',suite-name new-entry)))))))

;;; ------------------------------------------------------------
;;; Skip / Pending
;;; ------------------------------------------------------------

(defun skip (reason)
  "Signal a skip condition with the given reason."
  (signal 'skip-condition :reason reason))

(defun pending (reason)
  "Signal a pending condition with the given reason."
  (signal 'pending-condition :reason reason))
