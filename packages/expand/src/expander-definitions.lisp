(in-package :cl-cc/expand)

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

(defun %build-setf-progn-forms (args)
  "Build one SETF form per place/value pair in ARGS."
  (let ((xs args)
        (forms nil))
    (tagbody
     scan
       (if (null xs) (return-from %build-setf-progn-forms (nreverse forms)))
       (setq forms (cons (list 'setf (car xs) (cadr xs)) forms))
       (setq xs (cddr xs))
       (go scan))))

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
                  (cons updater (append (cdr place) (list value))))))))
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
                          (store-binding (list (list sv value))))
                     (compiler-macroexpand-all
                      (cons 'let
                            (cons (append arg-bindings store-binding)
                                  bd))))))))))
    (list 'quote accessor)))

;; define-setf-expander — register setf expansion function (FR-355)
;; (define-setf-expander accessor (lambda-list) body...)
(define-expander-for define-setf-expander (form)
  (let* ((accessor (second form))
         (lambda-list (third form))
         (body (cdddr form))
         (expander-fn (lambda (place)
                        (values-list
                         (funcall *macro-eval-fn*
                                  `(multiple-value-list
                                    ((lambda ,lambda-list ,@body) ',place)))))))
    (setf (gethash accessor *setf-compound-place-handlers*)
          (let ((fn expander-fn))
            (lambda (place value)
              (multiple-value-bind (temps vals stores store-form access-form)
                  (funcall fn place)
                (declare (ignore access-form))
                (compiler-macroexpand-all
                 (list 'let
                       (append (mapcar #'list temps vals)
                               (mapcar (lambda (s) (list s value)) stores))
                       store-form)))))))
  (list 'quote (second form)))

;; defconstant — compile-time constants treated as defparameter
(define-expander-for defconstant (form)
  (let ((name (second form))
        (value (third form)))
    (setf (gethash name *constant-table*) value)
    (compiler-macroexpand-all (list 'defparameter name value))))

;; setf — unified place dispatcher
(define-expander-for setf (form)
  (let ((len (length form)))
    (if (and (> len 3) (evenp (1- len)))
        ;; (setf a 1 b 2 ...) → (progn (setf a 1) (setf b 2) ...)
        (compiler-macroexpand-all
         (cons 'progn (%build-setf-progn-forms (cdr form))))
        (if (and (= len 3) (symbolp (second form)))
            ;; (setf var val) → (setq var val)
            (compiler-macroexpand-all (list 'setq (second form) (third form)))
            (if (and (= len 3) (consp (second form)))
                ;; compound place — table lookup first, then generic accessor, then passthrough
                (let* ((place   (second form))
                       (value   (third form))
                       (handler (gethash (car place) *setf-compound-place-handlers*)))
                  (if handler
                      (funcall handler place value)
                      (if (and (symbolp (car place)) (= (length place) 2))
                          (expand-setf-accessor place value)
                          (cons 'setf (mapcar #'compiler-macroexpand-all (cdr form))))))
                (cons 'setf (mapcar #'compiler-macroexpand-all (cdr form))))))))

;; make-array — preserve keyword calls for backend lowering
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
    (list 'quote name)))

;; vector — variadic vector constructor (vector a b c) → #(a b c)
(define-expander-for vector (form)
  (compiler-macroexpand-all
   (list 'coerce-to-vector (cons 'list (cdr form)))))

;; get-decoded-time — 0-arg form, expands to decode-universal-time call
(define-expander-for get-decoded-time (form)
  (declare (ignore form))
  '(decode-universal-time (get-universal-time)))
