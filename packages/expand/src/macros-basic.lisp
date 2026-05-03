(in-package :cl-cc/expand)
;;; Built-in Macros for Bootstrap

(defvar *setf-compound-place-handlers* (make-hash-table :test 'eq))

;; CHECK-TYPE macro
(defun %make-type-error (datum expected-type)
  "Construct a TYPE-ERROR condition with DATUM and EXPECTED-TYPE."
  (make-condition 'type-error :datum datum :expected-type expected-type))

(register-macro 'check-type
  (lambda (form env)
    (declare (ignore env))
    (let ((place (second form))
          (type (third form)))
      (list 'unless (list 'typep place (list 'quote type))
            (list 'error (list '%make-type-error place (list 'quote type)))))))

;; SETF macro (simplified)
(our-defmacro setf (place value)
  (cond
    ((symbolp place)
     `(setq ,place ,value))
    ((and (consp place) (member (car place) '(car first)))
     (let ((v (gensym "V")))
       `(let ((,v ,value))
          (rplaca ,(second place) ,v)
          ,v)))
    ((and (consp place) (member (car place) '(cdr rest)))
     (let ((v (gensym "V")))
       `(let ((,v ,value))
          (rplacd ,(second place) ,v)
          ,v)))
    ((and (consp place) (eq (car place) 'nth))
     (let ((v (gensym "V")))
       `(let ((,v ,value))
          (rplaca (nthcdr ,(second place) ,(third place)) ,v)
          ,v)))
     ((and (consp place) (member (car place) '(aref elt)))
      (let ((v (gensym "V")))
        `(let ((,v ,value))
           (aset ,(second place) ,(third place) ,v)
           ,v)))
     ((and (consp place) (eq (car place) 'fill-pointer))
      (let ((v (gensym "V")))
        `(let ((,v ,value))
           (%set-fill-pointer ,(second place) ,v))))
     ((and (consp place) (member (car place) '(symbol-function fdefinition)))
       `(set-fdefinition ,value ,(second place)))
     ((and (consp place) (eq (car place) 'gethash))
      ;; (setf (gethash key table) value) — handled by parser, but macro fallback
      `(setf-gethash ,(second place) ,(third place) ,value))
    ((and (consp place) (eq (car place) 'slot-value))
      ;; (setf (slot-value obj slot) value) — handled by parser, but macro fallback
      `(rt-slot-set ,(second place) ,(third place) ,value))
     ((and (consp place) (eq (car place) 'getf))
       ;; (setf (getf plist indicator) value) — rebuild plist with new value
       (let ((plist-place (second place))
             (indicator (third place))
             (v (gensym "V")))
         `(let ((,v ,value))
            ,(if (symbolp plist-place)
                 `(setq ,plist-place (rt-plist-put ,plist-place ,indicator ,v))
                 `(setf ,plist-place (rt-plist-put ,plist-place ,indicator ,v)))
            ,v)))
    (t
     (let ((handler (and (consp place)
                         (symbolp (car place))
                         (gethash (car place) *setf-compound-place-handlers*))))
       (cond
         (handler (funcall handler place value))
         ((and (consp place) (symbolp (car place)) (= (length place) 2))
          (expand-setf-accessor place value))
         (t
          (error "SETF: Unsupported place ~S" place)))))))

;; PSETQ macro (parallel setq)
(register-macro 'psetq
  (lambda (form env)
    (declare (ignore env))
    (let ((pairs (cdr form)))
      (if (null pairs)
          nil
          (let ((bindings (loop for (var val) on pairs by #'cddr
                                collect (list (gensym (symbol-name var)) val)))
                (vars (loop for (var) on pairs by #'cddr collect var)))
            (append (list 'let bindings)
                    (mapcar (lambda (var binding)
                              (list 'setq var (car binding)))
                            vars bindings)
                    (list nil)))))))

;; MULTIPLE-VALUE-BIND macro
(register-macro 'multiple-value-bind
  (lambda (call-form env)
    (declare (ignore env))
    (let ((vars (second call-form))
          (form (third call-form))
          (body (cdddr call-form))
          (temp (gensym "MVB")))
      (labels ((%nth-bindings (remaining index)
                 (if (null remaining)
                     nil
                     (cons (list (car remaining) (list 'nth index temp))
                           (%nth-bindings (cdr remaining) (+ index 1))))))
        (list 'let (list (list temp (list 'multiple-value-list form)))
              (cons 'let* (cons (%nth-bindings vars 0) body)))))))

;; MULTIPLE-VALUE-SETQ macro
(register-macro 'multiple-value-setq
  (lambda (call-form env)
    (declare (ignore env))
    (let ((vars (second call-form))
          (form (third call-form))
          (temp (gensym "MVS")))
      (labels ((%setq-forms (remaining index)
                 (if (null remaining)
                     nil
                     (cons (list 'setq (car remaining) (list 'nth index temp))
                           (%setq-forms (cdr remaining) (+ index 1))))))
        (cons 'let
              (cons (list (list temp (list 'multiple-value-list form)))
                    (append (%setq-forms vars 0)
                            (list (list 'car temp)))))))))

;; MULTIPLE-VALUE-LIST macro
;; Evaluates FORM (capturing its values side-effect), then drains the internal
;; values buffer via %values-to-list (a VM intrinsic).
(register-macro 'multiple-value-list
  (lambda (call-form env)
    (declare (ignore env))
    (let ((form (second call-form))
          (acc (gensym "MVL-ACC")))
      (list 'let (list (list acc nil))
            (list 'multiple-value-call
                  (list 'lambda '(&rest vals)
                        (list 'dolist '(v vals)
                              (list 'push 'v acc)))
                  form)
            (list 'nreverse acc)))))

;; LIST macro — expands to nested CONS calls; avoids a VM list instruction.
(register-macro 'list
  (lambda (call-form env)
    (declare (ignore env))
    (labels ((%expand-list-args (args)
               (if (null args)
                   nil
                   (cons 'cons
                         (cons (car args)
                               (cons (%expand-list-args (cdr args)) nil))))))
      (%expand-list-args (cdr call-form)))))
