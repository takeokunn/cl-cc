;;;; routing.lisp — FR-3305 type-safe routing helpers

(in-package :cl-cc/type)

(defun make-api-type (method path parameter-specs response-type &key request-type)
  "Construct a FR-3305 API type from a typed route descriptor."
  (let ((route (make-route method path
                           :parameters parameter-specs
                           :request-type request-type
                           :response-type response-type)))
    (make-type-advanced :feature-id "FR-3305"
                        :name 'api-type
                        :args (list route))))

(defun api-route-lookup (api-spec method path)
  "Return (values ROUTE PARAMS) when API-SPEC contains a matching METHOD/PATH."
  (let ((normalized-method (normalize-route-method method)))
    (dolist (route (api-spec-routes api-spec) (values nil nil))
      (when (eq normalized-method (route-method route))
        (multiple-value-bind (matched params) (match-route-path route path)
          (when matched
            (return (values route params))))))))

(defun route-response-type-for (api-spec method path)
  "Return the statically declared response type for METHOD/PATH."
  (multiple-value-bind (route params) (api-route-lookup api-spec method path)
    (declare (ignore params))
    (and route (route-response-type route))))
