(in-package :cl-cc/expand)

;;; Lambda List Parsing and Destructuring Helpers
;;;
;;; Shared by macro.lisp, expander-core.lisp, and macros-stdlib.lisp.

(eval-when (:compile-toplevel :load-toplevel :execute)

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
      (let ((item (car current)))
        (case item
          (&optional (setf state :optional))
          (&rest (setf state :rest))
          (&body (setf state :body))
          (&key (setf state :key))
          (&allow-other-keys
           (when (eq state :key)
             (setf allow-other-keys t)))
          (&aux (setf state :aux))
          (&environment (setf state :environment))
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

(defun generate-lambda-bindings (lambda-list form-var)
  "Generate LET bindings from LAMBDA-LIST for the macro form in FORM-VAR."
  (let ((info (parse-lambda-list lambda-list))
        (bindings nil)
        (temp-counter 0))
    (labels ((gensym-local (prefix)
               (intern (format nil "~A-~D" prefix (incf temp-counter)))))
      (let ((current-arg `(cdr ,form-var)))
        (dolist (req (lambda-list-info-required info))
          (let ((temp (gensym-local "REQ")))
            (push `(,temp (car ,current-arg)) bindings)
            (push `(,req ,temp) bindings)
            (setf current-arg `(cdr ,current-arg))))

        (dolist (opt (lambda-list-info-optional info))
          (destructuring-bind (opt-name default supplied-p) opt
            (let ((temp (gensym-local "OPT"))
                  (supplied-temp (gensym-local "SUPPLIED")))
              (push `(,temp (if ,current-arg (car ,current-arg) ,default)) bindings)
              (push `(,opt-name ,temp) bindings)
              (when supplied-p
                (push `(,supplied-temp (not (null ,current-arg))) bindings)
                (push `(,supplied-p ,supplied-temp) bindings))
              (setf current-arg `(cdr ,current-arg)))))

        (when (or (lambda-list-info-rest info)
                  (lambda-list-info-body info))
          (let ((rest-sym (or (lambda-list-info-rest info)
                              (lambda-list-info-body info))))
            (push `(,rest-sym ,current-arg) bindings)))

        (when (lambda-list-info-key-params info)
          (dolist (key-spec (lambda-list-info-key-params info))
            (destructuring-bind ((keyword name) default supplied-p) key-spec
              (let ((found (gensym-local "FOUND"))
                    (val (gensym-local "VAL")))
                (push `(,val (getf ,current-arg ,keyword ,default)) bindings)
                (push `(,name ,val) bindings)
                (when supplied-p
                  (push `(,found (not (eq (getf ,current-arg ,keyword :not-found)
                                         :not-found))) bindings)
                  (push `(,supplied-p ,found) bindings))))))

        (dolist (aux-spec (lambda-list-info-aux info))
          (destructuring-bind (aux-name init) aux-spec
            (push `(,aux-name ,init) bindings))))

      (nreverse bindings))))

(defun destructure-lambda-list (pattern arg)
  "Destructure ARG according to PATTERN lambda list.
   Returns an alist of (symbol . binding-form) pairs.
   Supports: required, &optional, &rest, &body, &key, &aux"
  (let ((info (parse-lambda-list pattern))
        (bindings nil)
        (arg-var (gensym "ARG"))
        (remaining-var (gensym "REMAINING"))
        (temp-counter 0))
    (labels ((gensym-local (prefix)
               (intern (format nil "~A-~D" prefix (incf temp-counter)))))
      (push (list arg-var arg) bindings)

      (let ((current-arg arg-var))
        (dolist (req (lambda-list-info-required info))
          (let ((temp (gensym-local "REQ")))
            (push `(,temp (car ,current-arg)) bindings)
            (if (listp req)
                (let ((sub-bindings (destructure-lambda-list req temp)))
                  (dolist (b sub-bindings)
                    (push b bindings)))
                (push `(,req ,temp) bindings))
            (push `(,current-arg (cdr ,current-arg)) bindings)))

        (push `(,remaining-var ,current-arg) bindings))

      (let ((opt-remaining remaining-var))
        (dolist (opt (lambda-list-info-optional info))
          (destructuring-bind (name default supplied-p) opt
            (let ((temp (gensym-local "OPT"))
                  (supplied-temp (gensym-local "SUPPLIED")))
              (push `(,temp (if ,opt-remaining (car ,opt-remaining) ,default)) bindings)
              (push `(,name ,temp) bindings)
              (when supplied-p
                (push `(,supplied-temp (if ,opt-remaining t nil)) bindings)
                (push `(,supplied-p ,supplied-temp) bindings))
              (push `(,opt-remaining (cdr ,opt-remaining)) bindings))))

        (setf remaining-var opt-remaining))

      (when (or (lambda-list-info-rest info)
                (lambda-list-info-body info))
        (let ((rest-sym (or (lambda-list-info-rest info)
                            (lambda-list-info-body info))))
          (push `(,rest-sym ,remaining-var) bindings)))

      (when (lambda-list-info-key-params info)
        (let ((key-remaining remaining-var))
          (dolist (key-spec (lambda-list-info-key-params info))
            (destructuring-bind ((keyword name) default supplied-p) key-spec
              (let ((val-temp (gensym-local "VAL")))
                (push `(,val-temp (getf ,key-remaining ,keyword ,default)) bindings)
                (push `(,name ,val-temp) bindings)
                (when supplied-p
                  (let ((found-temp (gensym-local "FOUND")))
                    (push `(,found-temp (not (eq (getf ,key-remaining ,keyword :not-found)
                                                 :not-found))) bindings)
                    (push `(,supplied-p ,found-temp) bindings))))))))

      (dolist (aux-spec (lambda-list-info-aux info))
        (destructuring-bind (name init) aux-spec
          (push `(,name ,init) bindings)))

      (nreverse bindings))))

) ; end eval-when
