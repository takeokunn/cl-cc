(in-package :cl-cc/compile)

;;; Block and Return-From

(defmethod cps-transform-ast ((node ast-block) k)
  "Transform block with named exit point.
The block creates a catch tag for return-from."
  (let* ((name (ast-block-name node))
         (body (ast-block-body node))
         (result (gensym "RESULT")))
    ;; The continuation for the body is a special one that returns from the block
    (list 'block name
          (cps-transform-sequence body
                                  (list 'lambda (list result)
                                        (list 'return-from name
                                              (list 'funcall k result)))))))

(defmethod cps-transform-ast ((node ast-return-from) k)
  "Transform return-from to exit the block with a value."
  (let* ((name (ast-return-from-name node))
         (value (ast-return-from-value node))
         (v (gensym "VAL")))
    ;; Evaluate the value, then return from the block
    ;; Note: k is ignored since we're doing a non-local exit
    (cps-transform-ast value
                       (list 'lambda (list v)
                             (list 'return-from name v)))))

;;; Tagbody and Go

(defun cps-transform-tagbody-section (forms k)
  "Transform a section of tagbody forms between tags, passing NIL to K when done."
  (%cps-transform-sequence-step forms k (list 'funcall k nil)))

(defmethod cps-transform-ast ((node ast-tagbody) k)
  "Transform tagbody with labeled sections.
Uses a tag table to map tags to their continuations."
  (let* ((tags (ast-tagbody-tags node))
         (tag-k (gensym "TAG-K")))
    (cons 'tagbody
          (append (loop for entry in tags
                        for tag = (car entry)
                        for forms = (cdr entry)
                        for result-k = (if (eq entry (car (last tags))) k tag-k)
                        append (list tag (cps-transform-tagbody-section forms result-k)))
                  (list (list 'funcall k nil))))))

(defmethod cps-transform-ast ((node ast-go) k)
  "Transform go to jump to a tag."
  (let ((tag (ast-go-tag node)))
    ;; k is ignored since go performs a non-local jump
    (declare (ignore k))
    (list 'go tag)))

;;; Catch and Throw

(defmethod cps-transform-ast ((node ast-catch) k)
  "Transform catch with dynamic tag."
  (let* ((tag-expr (ast-catch-tag node))
         (body (ast-catch-body node))
         (tag-v (gensym "TAG"))
         (result (gensym "RESULT")))
    (cps-transform-ast tag-expr
                       (list 'lambda (list tag-v)
                             (list 'catch tag-v
                                   (cps-transform-sequence body
                                                           (list 'lambda (list result)
                                                                 (list 'funcall k result))))))))

(defmethod cps-transform-ast ((node ast-throw) k)
  "Transform throw to unwind to matching catch."
  (let* ((tag-expr (ast-throw-tag node))
         (value (ast-throw-value node))
         (tag-v (gensym "TAG"))
         (val-v (gensym "VAL")))
    (cps-transform-ast tag-expr
                       (list 'lambda (list tag-v)
                             (cps-transform-ast value
                                                (list 'lambda (list val-v)
                                                      (list 'throw tag-v val-v)))))))

;;; Unwind-Protect

(defmethod cps-transform-ast ((node ast-unwind-protect) k)
  "Transform unwind-protect with guaranteed cleanup.
The cleanup forms always run, even on non-local exit."
  (let* ((protected (ast-unwind-protected node))
         (cleanup (ast-unwind-cleanup node))
         (result (gensym "RESULT"))
         (cleanup-result (gensym "CLEANUP")))
    (list 'unwind-protect
          (cps-transform-ast protected
                             (list 'lambda (list result)
                                   (list 'funcall k result)))
          (if cleanup
              (cps-transform-sequence cleanup
                                      (list 'lambda (list cleanup-result)
                                            (list 'declare (list 'ignore cleanup-result))
                                            nil))
              nil))))

;;; Flet and Labels (Local Function Bindings)

(defun cps-transform-fn-binding (binding k-var)
  "Transform a function binding (name params . body) to CPS form."
  (let* ((name (first binding))
         (params (second binding))
         (body (cddr binding)))
    (list name
          (cons 'lambda
                (cons (append params (list k-var))
                      (list (cps-transform-sequence body k-var)))))))

(defun cps-transform-local-fns (form-kw bindings body k)
  "Transform a flet/labels binding group to CPS.
FORM-KW is either 'flet or 'labels; they share identical CPS structure."
  (let ((fn-k (gensym (if (eq form-kw 'flet) "FLET-K" "LABELS-K"))))
    (list form-kw
          (loop for binding in bindings
                collect (cps-transform-fn-binding binding fn-k))
          (cps-transform-sequence body
                                  (list 'lambda (list fn-k)
                                        (list 'funcall k fn-k))))))

(defmethod cps-transform-ast ((node ast-flet) k)
  "Transform flet (non-recursive local functions)."
  (cps-transform-local-fns 'flet (ast-flet-bindings node) (ast-flet-body node) k))

(defmethod cps-transform-ast ((node ast-labels) k)
  "Transform labels (mutually recursive local functions)."
  (cps-transform-local-fns 'labels (ast-labels-bindings node) (ast-labels-body node) k))
