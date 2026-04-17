;;;; macros-plist.lisp — Property list helpers
(in-package :cl-cc/expand)

;;; FR-1201: Property List Macros (getf, remf, get-properties)

(our-defmacro getf (plist indicator &optional default)
  "Return the value for INDICATOR in PLIST, or DEFAULT if not found."
  (let ((pl (gensym "PL")) (ind (gensym "IND")) (found (gensym "FOUND")))
    `(let ((,pl ,plist) (,ind ,indicator))
       (let ((,found (member ,ind ,pl)))
         (if ,found (cadr ,found) ,default)))))

(our-defmacro remf (plist indicator)
  "Remove INDICATOR and its value from PLIST. Returns T if found, NIL otherwise."
  (let ((ind (gensym "IND")) (prev (gensym "PREV")) (cur (gensym "CUR"))
        (found (gensym "FOUND")))
    `(let ((,ind ,indicator) (,prev nil) (,cur ,plist) (,found nil))
       (tagbody
         :loop
         (when ,cur
           (cond
             ((eq (car ,cur) ,ind)
              (setq ,found t)
              (if ,prev
                  (rplacd (cdr ,prev) (cddr ,cur))
                  (setq ,plist (cddr ,cur)))
              (go :done))
             (t
              (setq ,prev ,cur)
              (setq ,cur (cddr ,cur))
              (go :loop))))
         :done)
       ,found)))

;;; %plist-put — non-destructive plist update (used by setf getf expansion)
(our-defmacro %plist-put (plist indicator value)
  (let ((p (gensym "P")) (result (gensym "R")) (found (gensym "F"))
        (k (gensym "K")) (v (gensym "V")))
    `(let ((,p ,plist) (,v ,value) (,result nil) (,found nil))
       (loop while ,p do
         (let ((,k (car ,p)))
           (if (eq ,k ,indicator)
               (progn (push ,indicator ,result) (push ,v ,result) (setf ,found t))
               (progn (push ,k ,result) (push (cadr ,p) ,result)))
           (setf ,p (cddr ,p))))
       (unless ,found
         (push ,v ,result) (push ,indicator ,result))
       (nreverse ,result))))
