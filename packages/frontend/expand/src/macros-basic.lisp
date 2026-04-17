(in-package :cl-cc/expand)
;;; Built-in Macros for Bootstrap

;; CHECK-TYPE macro
(defun %make-type-error (datum expected-type)
  "Construct a TYPE-ERROR condition with DATUM and EXPECTED-TYPE."
  (make-condition 'type-error :datum datum :expected-type expected-type))

(our-defmacro check-type (place type &optional type-string)
  (declare (ignore type-string))
  `(unless (typep ,place ',type)
     (error (%make-type-error ,place ',type))))

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
    ((and (consp place) (eq (car place) 'gethash))
     ;; (setf (gethash key table) value) — handled by parser, but macro fallback
     `(setf-gethash ,(second place) ,(third place) ,value))
    ((and (consp place) (eq (car place) 'slot-value))
     ;; (setf (slot-value obj slot) value) — handled by parser, but macro fallback
     `(setf-slot-value ,(second place) ,(third place) ,value))
    ((and (consp place) (eq (car place) 'getf))
     ;; (setf (getf plist indicator) value) — rebuild plist with new value
     (let ((v (gensym "V")))
       `(let ((,v ,value))
          (setq ,(second place) (rt-plist-put ,(second place) ,(third place) ,v))
          ,v)))
    (t
     (error "SETF: Unsupported place ~S" place))))

;; PSETQ macro (parallel setq)
(our-defmacro psetq (&rest pairs)
  (when pairs
    (let ((bindings (loop for (var val) on pairs by #'cddr
                          collect (list (gensym (symbol-name var)) val)))
          (vars (loop for (var) on pairs by #'cddr collect var)))
      `(let ,bindings
         ,@(mapcar (lambda (var binding)
                     `(setq ,var ,(car binding)))
                   vars bindings)
         nil))))

;; MULTIPLE-VALUE-BIND macro
(our-defmacro multiple-value-bind (vars form &body body)
  `(multiple-value-call
    (lambda ,vars ,@body)
    ,form))

;; MULTIPLE-VALUE-SETQ macro
(our-defmacro multiple-value-setq (vars form)
  (let ((temp (gensym "MVS")))
    `(let ((,temp (multiple-value-list ,form)))
       ,@(mapcar (lambda (var i)
                   `(setq ,var (nth ,i ,temp)))
                 vars
                 (loop for i below (length vars) collect i))
       (car ,temp))))

;; MULTIPLE-VALUE-LIST macro
;; Evaluates FORM (capturing its values side-effect), then drains the internal
;; values buffer via %values-to-list (a VM intrinsic).
(our-defmacro multiple-value-list (form)
  "Collect all values of FORM into a list."
  (let ((acc (gensym "MVL-ACC")))
    `(let ((,acc nil))
       (multiple-value-call (lambda (&rest vals)
                              (dolist (v vals) (push v ,acc)))
                            ,form)
       (nreverse ,acc))))

;; LIST macro — expands to nested CONS calls; avoids a VM list instruction.
(our-defmacro list (&rest args)
  (if (null args)
      nil
      (reduce (lambda (x acc) `(cons ,x ,acc))
              args :from-end t :initial-value nil)))
