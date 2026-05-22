;;;; optimizer-defunctionalize.lisp --- FR-676 direct-call recovery

(in-package :cl-cc/optimize)

(defun %opt-known-function-designator-p (value)
  (or (and (symbolp value) (not (keywordp value)))
      (and (consp value) (eq (first value) 'function))))

(defun %opt-normalize-function-designator (value)
  (if (and (consp value) (eq (first value) 'function) (= (length value) 2))
      (second value)
      value))

(defun %opt-track-constant (inst constants)
  (cond
    ((typep inst 'vm-const)
     (setf (gethash (vm-dst inst) constants) (vm-value inst)))
    ((typep inst 'vm-move)
     (multiple-value-bind (value foundp) (gethash (vm-src inst) constants)
       (if foundp
           (setf (gethash (vm-dst inst) constants) value)
           (remhash (vm-dst inst) constants))))
    ((opt-inst-dst inst)
     (remhash (opt-inst-dst inst) constants))))

(defun %opt-known-function-reg (reg constants)
  (multiple-value-bind (value foundp) (gethash reg constants)
    (and foundp
         (%opt-known-function-designator-p value)
         (%opt-normalize-function-designator value))))

(defun %opt-known-concrete-list-reg (reg constants)
  (multiple-value-bind (value foundp) (gethash reg constants)
    (and foundp (listp value) value)))

(defun %opt-defunctionalize-apply (inst constants)
  (let ((known-fn (%opt-known-function-reg (vm-func-reg inst) constants)))
    (if (not known-fn)
        inst
        (let* ((args (copy-list (vm-args inst)))
               (last-reg (car (last args)))
               (concrete-tail (and last-reg (%opt-known-concrete-list-reg last-reg constants))))
          (if concrete-tail
              (make-vm-call :dst (vm-dst inst)
                            :func known-fn
                            :args (append (butlast args) concrete-tail))
              inst)))))

(defun %opt-defunctionalize-call (inst constants)
  (let ((known-fn (%opt-known-function-reg (vm-func-reg inst) constants)))
    (if known-fn
        (make-vm-call :dst (vm-dst inst) :func known-fn :args (vm-args inst))
        inst)))

(defun opt-pass-defunctionalize (instructions)
  "FR-676: convert constant-proven higher-order calls to direct vm-call sites."
  (let ((constants (make-hash-table :test #'eq))
        (changed nil)
        (result nil))
    (dolist (inst instructions (if changed (nreverse result) instructions))
      (let ((rewritten (cond
                         ((typep inst 'vm-apply)
                          (%opt-defunctionalize-apply inst constants))
                         ((typep inst 'vm-call)
                          (%opt-defunctionalize-call inst constants))
                         (t inst))))
        (unless (eq rewritten inst) (setf changed t))
        (push rewritten result)
        (%opt-track-constant rewritten constants)))))

(defun opt-defunctionalize-form (form &optional constants)
  "Defunctionalize source-like FUNCALL/APPLY forms when the function is known.
CONSTANTS is an alist mapping variables to constant function designators."
  (labels ((known (x)
             (cond ((%opt-known-function-designator-p x) x)
                   ((symbolp x) (cdr (assoc x constants :test #'eq)))
                   (t nil)))
           (known-call-name (x)
             (let ((value (known x)))
               (and value (%opt-normalize-function-designator value))))
           (walk (x)
             (cond
               ((atom x) x)
               ((and (eq (first x) 'funcall) (known-call-name (second x)))
                (cons (known-call-name (second x)) (mapcar #'walk (cddr x))))
               ((and (eq (first x) 'apply)
                     (known-call-name (second x))
                     (= (length x) 3)
                     (consp (third x))
                     (eq (first (third x)) 'list))
                (cons (known-call-name (second x)) (mapcar #'walk (rest (third x)))))
               ((and (eq (first x) 'sort)
                     (= (length x) 3)
                     (member (third x) (list ''< '(function <)) :test #'equal))
                (list '%sort/fixnum-ascending (walk (second x))))
               (t (mapcar #'walk x)))))
    (walk form)))
