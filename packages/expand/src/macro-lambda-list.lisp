(in-package :cl-cc/expand)

;;; Lambda List Parsing and Destructuring Helpers
;;;
;;; Shared by macro.lisp, expander-core.lisp, and macros-stdlib.lisp.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter *lambda-list-keyword-transitions*
  '((&optional . :optional)
    (&rest . :rest)
    (&body . :body)
    (&key . :key)
    (&aux . :aux)
    (&environment . :environment))
  "Maps each standard lambda list keyword to its parser state.")

(defstruct lambda-list-info
  "Information about a parsed lambda list."
  required
  optional
  rest
  body
  key-params
  allow-other-keys
  aux
  environment)

(defun parse-lambda-list (lambda-list)
  "Parse LAMBDA-LIST into a lambda-list-info structure.
   Supports: required, &optional, &rest, &body, &key, &allow-other-keys, &aux."
  (let ((state :required)
        (required nil)
        (optional nil)
        (rest nil)
        (body nil)
        (key-params nil)
        (allow-other-keys nil)
        (aux nil)
        (environment nil))
    (do ((current lambda-list (cdr current)))
        ((atom current)
         (when current
           (setf rest current)))
      (let* ((item (car current))
             (transition (cdr (assoc item *lambda-list-keyword-transitions*))))
        (cond
          (transition
           (setf state transition))
          ((eq item '&allow-other-keys)
           (when (eq state :key)
             (setf allow-other-keys t)))
          (t
           (case state
             (:required
              (push item required))
             (:optional
              (cond ((symbolp item)
                     (push (list item nil nil) optional))
                    ((consp item)
                     (let ((name (first item))
                           (default (if (cdr item) (second item) nil))
                           (supplied-p (if (cddr item) (third item) nil)))
                       (push (list name default supplied-p) optional)))
                    (t (error "Invalid &optional parameter: ~S" item))))
             (:rest
              (setf rest item
                    state :after-rest))
             (:body
              (setf body item
                    state :after-body))
             (:key
              (cond ((symbolp item)
                     (let ((keyword (intern (symbol-name item) :keyword)))
                       (push (list (list keyword item) nil nil) key-params)))
                    ((consp item)
                     (let* ((name-spec (first item))
                            (name (if (consp name-spec)
                                      (progn
                                        (unless (= (length name-spec) 2)
                                          (error "Invalid key name spec: ~S" name-spec))
                                        name-spec)
                                      (list (intern (symbol-name name-spec) :keyword) name-spec)))
                            (default (if (cdr item) (second item) nil))
                            (supplied-p (if (cddr item) (third item) nil)))
                       (push (list name default supplied-p) key-params)))
                    (t (error "Invalid &key parameter: ~S" item))))
             (:aux
              (cond ((symbolp item)
                     (push (list item nil) aux))
                    ((consp item)
                     (push (list (first item) (if (cdr item) (second item) nil)) aux))
                    (t (error "Invalid &aux parameter: ~S" item))))
             (:environment
              (setf environment item
                    state :required))
             ((:after-rest :after-body)
              (error "Unexpected parameter after ~A: ~S"
                     (if (eq state :after-rest) "&rest" "&body") item)))))))
    (make-lambda-list-info
     :required (nreverse required)
     :optional (nreverse optional)
     :rest rest
     :body body
     :key-params (nreverse key-params)
     :allow-other-keys allow-other-keys
     :aux (nreverse aux)
     :environment environment)))

(defun %make-gensym-local ()
  "Return a function that generates sequentially-numbered interned symbols."
  (let ((counter 0))
    (lambda (prefix)
      (intern (format nil "~A-~D" prefix (incf counter))))))

(defun %push-required-bindings (required current-arg bindings gensym-local)
  "Accumulate binding forms for REQUIRED parameters.
Returns the updated CURRENT-ARG cursor and BINDINGS list." 
  (dolist (req required)
    (let ((temp (funcall gensym-local "REQ")))
      (push (list temp (list 'car current-arg)) bindings)
      (push (list req temp) bindings)
      (setf current-arg (list 'cdr current-arg))))
  (values current-arg bindings))

(defun %push-optional-bindings (optional current-arg bindings gensym-local)
  "Accumulate binding forms for OPTIONAL parameters.
Returns the updated CURRENT-ARG cursor and BINDINGS list."
  (dolist (opt optional)
    (destructuring-bind (opt-name default supplied-p) opt
      (let ((temp (funcall gensym-local "OPT"))
            (supplied-temp (funcall gensym-local "SUPPLIED")))
        (push (list temp (list 'if current-arg (list 'car current-arg) default)) bindings)
        (push (list opt-name temp) bindings)
        (when supplied-p
          (push (list supplied-temp (list 'not (list 'null current-arg))) bindings)
          (push (list supplied-p supplied-temp) bindings))
        (setf current-arg (list 'cdr current-arg)))))
  (values current-arg bindings))

(defun %push-key-bindings (key-params current-arg bindings gensym-local)
  "Accumulate binding forms for KEY-PARAMS against CURRENT-ARG."
  (dolist (key-spec key-params)
    (destructuring-bind ((keyword name) default supplied-p) key-spec
      (let ((found (funcall gensym-local "FOUND"))
            (val (funcall gensym-local "VAL")))
        (push (list val (list 'getf current-arg keyword default)) bindings)
        (push (list name val) bindings)
        (when supplied-p
          (push (list found
                      (list 'not
                            (list 'eq (list 'getf current-arg keyword :not-found)
                                  :not-found)))
                bindings)
          (push (list supplied-p found) bindings)))))
  bindings)

(defun %push-aux-bindings (aux-specs bindings)
  "Accumulate binding forms for AUX-SPECS into BINDINGS."
  (dolist (aux-spec aux-specs)
    (destructuring-bind (aux-name init) aux-spec
      (push (list aux-name init) bindings)))
  bindings)

(defun %push-destructured-required-bindings (required current-arg bindings gensym-local)
  "Accumulate binding forms for REQUIRED destructuring parameters."
  (dolist (req required)
    (let ((temp (funcall gensym-local "REQ")))
      (push (list temp (list 'car current-arg)) bindings)
      (if (listp req)
          (let ((sub-bindings (destructure-lambda-list req temp)))
            (dolist (b sub-bindings)
              (push b bindings)))
          (push (list req temp) bindings))
      (push (list current-arg (list 'cdr current-arg)) bindings)))
  bindings)

(defun %push-destructured-optional-bindings (optional opt-remaining bindings gensym-local)
  "Accumulate binding forms for OPTIONAL destructuring parameters."
  (dolist (opt optional)
    (destructuring-bind (name default supplied-p) opt
      (let ((temp (funcall gensym-local "OPT"))
            (supplied-temp (funcall gensym-local "SUPPLIED")))
        (push (list temp (list 'if opt-remaining (list 'car opt-remaining) default)) bindings)
        (push (list name temp) bindings)
        (when supplied-p
          (push (list supplied-temp (list 'if opt-remaining t nil)) bindings)
          (push (list supplied-p supplied-temp) bindings))
        (push (list opt-remaining (list 'cdr opt-remaining)) bindings))))
  bindings)

(defun %push-destructured-key-bindings (key-params key-remaining bindings gensym-local)
  "Accumulate binding forms for KEY-PARAMS in destructuring mode."
  (dolist (key-spec key-params)
    (destructuring-bind ((keyword name) default supplied-p) key-spec
      (let ((val-temp (funcall gensym-local "VAL")))
        (push (list val-temp (list 'getf key-remaining keyword default)) bindings)
        (push (list name val-temp) bindings)
        (when supplied-p
          (let ((found-temp (funcall gensym-local "FOUND")))
            (push (list found-temp
                        (list 'not
                              (list 'eq (list 'getf key-remaining keyword :not-found)
                                    :not-found)))
                  bindings)
            (push (list supplied-p found-temp) bindings))))))
  bindings)

(defun generate-lambda-bindings (lambda-list form-var)
  "Generate LET* bindings from LAMBDA-LIST for the macro form in FORM-VAR.
Uses the full destructuring path so macro lambda lists like
  (name (parent) &body body)
work in host-registered expanders as well as simpler flat lambda lists."
  (destructure-lambda-list lambda-list `(cdr ,form-var)))

(defun destructure-lambda-list (pattern arg)
  "Destructure ARG according to PATTERN lambda list.
   Returns an alist of (symbol . binding-form) pairs.
   Supports: required, &optional, &rest, &body, &key, &aux"
  (let ((info (parse-lambda-list pattern))
        (bindings nil)
        (arg-var (gensym "ARG"))
        (remaining-var (gensym "REMAINING"))
        (gensym-local (%make-gensym-local)))
    (push (list arg-var arg) bindings)

      (let ((current-arg arg-var))
        (setf bindings (%push-destructured-required-bindings
                        (lambda-list-info-required info)
                        current-arg bindings gensym-local))

        (push (list remaining-var current-arg) bindings))

      (let ((opt-remaining remaining-var))
        (setf bindings (%push-destructured-optional-bindings
                        (lambda-list-info-optional info)
                        opt-remaining bindings gensym-local))

        (setf remaining-var opt-remaining))

      (when (or (lambda-list-info-rest info)
                (lambda-list-info-body info))
        (let ((rest-sym (or (lambda-list-info-rest info)
                             (lambda-list-info-body info))))
          (push (list rest-sym remaining-var) bindings)))

      (when (lambda-list-info-key-params info)
        (let ((key-remaining remaining-var))
          (setf bindings (%push-destructured-key-bindings
                          (lambda-list-info-key-params info)
                          key-remaining bindings gensym-local))))

      (setf bindings (%push-aux-bindings (lambda-list-info-aux info) bindings))

      (nreverse bindings)))

) ; end eval-when
