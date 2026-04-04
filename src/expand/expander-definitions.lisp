(in-package :cl-cc)

;;; FR-607: Strip leading docstring from body (string + more forms → skip string)
;;; Returns (stripped-body . docstring-or-nil)
(defun %strip-docstring (body)
  (if (and (stringp (car body)) (cdr body))
      (cdr body)
      body))

(defun %extract-docstring (body)
  "Return the docstring if BODY starts with one (and has more forms), else nil."
  (if (and (stringp (car body)) (cdr body))
      (car body)
      nil))

;; defsetf — register a setf expander for an accessor (FR-355)
;; Short form: (defsetf accessor updater) → (updater args... value)
;; Long form: (defsetf accessor lambda-list (store-var) body...) — partial support
(define-expander-for defsetf (form)
  (let ((accessor (second form)))
    (cond
      ;; Short form: (defsetf accessor updater)
      ((and (= (length form) 3) (symbolp (third form)))
       (let ((updater (third form)))
         (setf (gethash accessor *setf-compound-place-handlers*)
               (lambda (place value)
                 (compiler-macroexpand-all
                  `(,updater ,@(cdr place) ,value))))))
      ;; Long form: (defsetf accessor (args...) (store-var) body...)
      ((and (>= (length form) 5) (listp (third form)) (listp (fourth form)))
       (let* ((lambda-list (third form))
              (store-vars (fourth form))
              (body (cddddr form))
              (store-var (first store-vars)))
         (setf (gethash accessor *setf-compound-place-handlers*)
               (let ((ll lambda-list) (sv store-var) (bd body))
                 (lambda (place value)
                   (let* ((arg-bindings (mapcar #'list ll (cdr place)))
                          (store-binding `((,sv ,value))))
                     (compiler-macroexpand-all
                      `(let (,@arg-bindings ,@store-binding)
                         ,@bd)))))))))
    `(quote ,accessor)))

;; define-setf-expander — register setf expansion function (FR-355)
;; (define-setf-expander accessor (lambda-list) body...)
(define-expander-for define-setf-expander (form)
  (let* ((accessor (second form))
         (lambda-list (third form))
         (body (cdddr form))
         (expander-fn (eval `(lambda ,lambda-list ,@body))))
    (setf (gethash accessor *setf-compound-place-handlers*)
          (let ((fn expander-fn))
            (lambda (place value)
              (multiple-value-bind (temps vals stores store-form access-form)
                  (funcall fn place)
                (declare (ignore access-form))
                (compiler-macroexpand-all
                 `(let (,@(mapcar #'list temps vals)
                        ,@(mapcar (lambda (s) (list s value)) stores))
                    ,store-form)))))))
  `(quote ,(second form)))

;; defconstant — compile-time constants treated as defparameter
(define-expander-for defconstant (form)
  (let ((name (second form))
        (value (third form)))
    (setf (gethash name *constant-table*) value)
    (compiler-macroexpand-all `(defparameter ,name ,value))))

;; setf — unified place dispatcher
(define-expander-for setf (form)
  (let ((len (length form))
        (expand-args (lambda () (cons 'setf (mapcar #'compiler-macroexpand-all (cdr form))))))
    (cond
      ;; (setf a 1 b 2 ...) → (progn (setf a 1) (setf b 2) ...)
      ((and (> len 3) (evenp (1- len)))
       (compiler-macroexpand-all
        `(progn ,@(loop for (place val) on (cdr form) by #'cddr
                        collect `(setf ,place ,val)))))
      ;; (setf var val) → (setq var val)
      ((and (= len 3) (symbolp (second form)))
       (compiler-macroexpand-all `(setq ,(second form) ,(third form))))
      ;; compound place — table lookup first, then generic accessor, then passthrough
      ((and (= len 3) (consp (second form)))
       (let* ((place   (second form))
              (value   (third form))
              (handler (gethash (car place) *setf-compound-place-handlers*)))
         (cond
           (handler (funcall handler place value))
           ((and (symbolp (car place)) (= (length place) 2))
            (expand-setf-accessor place value))
           (t (funcall expand-args)))))
      (t (funcall expand-args)))))

;; make-array — promote to make-adjustable-vector when :fill-pointer/:adjustable given
(define-expander-for make-array (form)
  (if (>= (length form) 4)
      (expand-make-array-form (second form) (cddr form))
      (cons 'make-array (mapcar #'compiler-macroexpand-all (cdr form)))))

;; defstruct — expand to defclass + constructor + predicate
(define-expander-for defstruct (form)
  (compiler-macroexpand-all (expand-defstruct form)))

;; eval-when — phase control (compile-toplevel vs execute/load-toplevel)
(define-expander-for eval-when (form)
  (expand-eval-when-form (second form) (cddr form)))

;; macrolet — local macro bindings scoped to body
(define-expander-for macrolet (form)
  (expand-macrolet-form (second form) (cddr form)))

;; symbol-macrolet — local symbol macro bindings scoped to body (FR-398)
(define-expander-for symbol-macrolet (form)
  (expand-symbol-macrolet-form (second form) (cddr form)))

;; define-symbol-macro — global symbol macro definition (FR-398)
(define-expander-for define-symbol-macro (form)
  (let ((name (second form))
        (expansion (third form)))
    (setf (gethash name *symbol-macro-table*) expansion)
    `(quote ,name)))

;; vector — variadic vector constructor (vector a b c) → #(a b c)
(define-expander-for vector (form)
  (compiler-macroexpand-all `(coerce-to-vector (list ,@(cdr form)))))

;; get-decoded-time — 0-arg form, expands to decode-universal-time call
(define-expander-for get-decoded-time (form)
  (declare (ignore form))
  '(decode-universal-time (get-universal-time)))
