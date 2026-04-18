;;;; macros-plist.lisp — Property list helpers
(in-package :cl-cc/expand)

;;; FR-1201: Property List Macros (getf, remf, get-properties)

(register-macro 'getf
  (lambda (form env)
    (declare (ignore env))
    (let ((plist (second form))
          (indicator (third form))
          (default (fourth form))
          (pl (gensym "PL"))
          (ind (gensym "IND"))
          (found (gensym "FOUND")))
      (list 'let (list (list pl plist)
                       (list ind indicator))
            (list 'let (list (list found (list 'member ind pl)))
                  (list 'if found (list 'cadr found) default))))))

(register-macro 'remf
  (lambda (form env)
    (declare (ignore env))
    (let ((plist (second form))
          (indicator (third form))
          (ind (gensym "IND"))
          (prev (gensym "PREV"))
          (cur (gensym "CUR"))
          (found (gensym "FOUND")))
      (list 'let (list (list ind indicator)
                       (list prev nil)
                       (list cur plist)
                       (list found nil))
            (list 'tagbody
                  :loop
                  (list 'when cur
                        (list 'cond
                              (list (list 'eq (list 'car cur) ind)
                                    (list 'setq found t)
                                    (list 'if prev
                                          (list 'rplacd (list 'cdr prev) (list 'cddr cur))
                                          (list 'setq plist (list 'cddr cur)))
                                    '(go :done))
                              (list 't
                                    (list 'setq prev cur)
                                    (list 'setq cur (list 'cddr cur))
                                    '(go :loop))))
                  :done)
            found))))

;;; %plist-put — non-destructive plist update (used by setf getf expansion)
(register-macro '%plist-put
  (lambda (form env)
    (declare (ignore env))
    (let ((plist (second form))
          (indicator (third form))
          (value (fourth form))
          (p (gensym "P"))
          (result (gensym "R"))
          (found (gensym "F"))
          (k (gensym "K"))
          (v (gensym "V")))
      (list 'let (list (list p plist)
                       (list v value)
                       (list result nil)
                       (list found nil))
            (list 'loop 'while p 'do
                  (list 'let (list (list k (list 'car p)))
                        (list 'if (list 'eq k indicator)
                              (list 'progn (list 'push indicator result)
                                    (list 'push v result)
                                    (list 'setf found t))
                              (list 'progn (list 'push k result)
                                    (list 'push (list 'cadr p) result)))
                        (list 'setf p (list 'cddr p))))
            (list 'unless found
                  (list 'push v result)
                  (list 'push indicator result))
            (list 'nreverse result)))))
