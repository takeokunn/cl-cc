;;;; types-extended-routing-types.lisp — Route, api-spec structs and validators
(in-package :cl-cc/type)

(define-condition route-validation-error (error)
  ((detail :initarg :detail :reader route-validation-error-detail))
  (:report (lambda (condition stream)
             (format stream "Invalid route: ~A" (route-validation-error-detail condition)))))

(defstruct (route (:constructor %make-route))
  "A concrete typed route descriptor."
  (method :get)
  (path "/" :type string)
  (parameters nil :type list)
  (request-type nil)
  (response-type nil))

(defstruct api-spec
  "A collection of typed routes."
  (routes nil :type list))

(defparameter +route-methods+ '(:get :post :put :patch :delete :head :options)
  "Supported HTTP methods for typed routes.")

(defun normalize-route-method (method)
  "Normalize METHOD into a keyword route method."
  (cond
    ((keywordp method) method)
    ((symbolp method) (intern (symbol-name method) :keyword))
    ((stringp method) (intern (string-upcase method) :keyword))
    (t method)))

(defun %split-path (path)
  "Split PATH into slash-separated segments."
  (let ((start 0)
        (length (length path))
        segments)
    (loop for index from 0 to length do
      (when (or (= index length) (char= (char path index) #\/))
        (let ((segment (subseq path start index)))
          (unless (string= segment "")
            (push segment segments)))
        (setf start (1+ index))))
    (nreverse segments)))

(defun route-template-parameters (path)
  "Extract ordered placeholder names from PATH."
  (let (parameters)
    (dolist (segment (%split-path path))
      (when (and (> (length segment) 2)
                 (char= (char segment 0) #\{)
                 (char= (char segment (1- (length segment))) #\}))
        (push (intern (string-upcase (subseq segment 1 (1- (length segment)))) :keyword)
              parameters)))
    (nreverse parameters)))

(defun %normalize-route-parameters (parameters)
  "Normalize PARAMETERS into an alist of keyword → type-designator."
  (mapcar (lambda (entry)
            (cond
              ((and (consp entry) (consp (rest entry)) (null (cddr entry)))
               (cons (intern (string-upcase (symbol-name (first entry))) :keyword)
                     (second entry)))
              ((consp entry)
               (cons (intern (string-upcase (symbol-name (car entry))) :keyword)
                     (cdr entry)))
              (t
               (error 'route-validation-error :detail (format nil "malformed parameter ~S" entry)))))
          parameters))

(defun route-valid-p (route)
  "Return T when ROUTE is a valid typed route descriptor."
  (and (route-p route)
       (member (normalize-route-method (route-method route)) +route-methods+ :test #'eq)
       (stringp (route-path route))
       (plusp (length (route-path route)))
       (char= (char (route-path route) 0) #\/)
       (let* ((template-parameters (route-template-parameters (route-path route)))
              (normalized-parameters (%normalize-route-parameters (route-parameters route)))
              (parameter-names (mapcar #'car normalized-parameters)))
         (and (= (length template-parameters) (length parameter-names))
              (equal template-parameters parameter-names)))))

(defun make-route (method path &key parameters request-type response-type)
  "Construct and validate a typed route."
  (let ((route (%make-route :method (normalize-route-method method)
                            :path path
                            :parameters (%normalize-route-parameters parameters)
                            :request-type request-type
                            :response-type response-type)))
    (unless (route-valid-p route)
      (error 'route-validation-error :detail (format nil "~S ~A" method path)))
    route))

(defun %parse-route-parameter (raw-value type-designator)
  "Parse RAW-VALUE according to TYPE-DESIGNATOR."
  (cond
    ((eq type-designator 'integer)
     (handler-case
         (multiple-value-bind (value index) (parse-integer raw-value :junk-allowed t)
           (and (= index (length raw-value)) value))
       (error () nil)))
    ((eq type-designator 'string) raw-value)
    ((eq type-designator 'keyword) (intern (string-upcase raw-value) :keyword))
    (t (when (typep raw-value type-designator) raw-value))))

(defun build-route-path (route parameter-values)
  "Render ROUTE using PARAMETER-VALUES, an alist keyed by parameter name."
  (unless (route-valid-p route)
    (error 'route-validation-error :detail "cannot build an invalid route"))
  (let* ((value-alist (%normalize-route-parameters parameter-values))
         (expected (%normalize-route-parameters (route-parameters route)))
         (segments (%split-path (route-path route))))
    (concatenate
     'string
     "/"
     (format nil "~{~A~^/~}"
             (mapcar (lambda (segment)
                       (if (and (> (length segment) 2)
                                (char= (char segment 0) #\{)
                                (char= (char segment (1- (length segment))) #\}))
                           (let* ((name (intern (string-upcase (subseq segment 1 (1- (length segment)))) :keyword))
                                  (expected-type (cdr (assoc name expected :test #'eq)))
                                  (actual (cdr (assoc name value-alist :test #'eq))))
                             (unless (typep actual expected-type)
                               (error 'route-validation-error
                                      :detail (format nil "parameter ~A expected ~S, got ~S"
                                                      name expected-type actual)))
                             (princ-to-string actual))
                           segment))
                     segments)))))

(defun match-route-path (route path)
  "Return (values T params) when PATH matches ROUTE, otherwise (values NIL NIL)."
  (unless (route-valid-p route)
    (return-from match-route-path (values nil nil)))
  (let ((template-segments (%split-path (route-path route)))
        (path-segments (%split-path path))
        (parameters (%normalize-route-parameters (route-parameters route)))
        results)
    (unless (= (length template-segments) (length path-segments))
      (return-from match-route-path (values nil nil)))
    (loop for template in template-segments
          for actual in path-segments do
      (if (and (> (length template) 2)
               (char= (char template 0) #\{)
               (char= (char template (1- (length template))) #\}))
          (let* ((name (intern (string-upcase (subseq template 1 (1- (length template)))) :keyword))
                 (type-designator (cdr (assoc name parameters :test #'eq)))
                 (parsed (%parse-route-parameter actual type-designator)))
            (unless parsed
              (return-from match-route-path (values nil nil)))
            (push (cons name parsed) results))
          (unless (string= template actual)
            (return-from match-route-path (values nil nil)))))
    (values t (nreverse results))))

(defun api-spec-valid-p (api-spec)
  "Return T when API-SPEC contains only valid, non-duplicate routes."
  (and (api-spec-p api-spec)
       (every #'route-valid-p (api-spec-routes api-spec))
       (let ((seen (make-hash-table :test #'equal)))
         (loop for route in (api-spec-routes api-spec)
               always (let ((key (list (route-method route) (route-path route))))
                        (if (gethash key seen)
                            nil
                            (progn
                              (setf (gethash key seen) t)
                              t)))))))

(defun route-form-valid-p (value)
  "Return T when VALUE is a plausible raw FR-3305 route payload form."
  (and (consp value)
       (member (normalize-route-method (first value)) +route-methods+ :test #'eq)
       (>= (length value) 3)
       (stringp (second value))
       (let* ((path (second value))
              (param-count (length (route-template-parameters path)))
              (tail-count (- (length value) 2)))
         (>= tail-count (max 1 param-count)))))


