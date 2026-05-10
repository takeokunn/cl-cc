;;;; macros-mop-support.lisp — MOP introspection macros + parse-float + reinitialize-instance
(in-package :cl-cc/expand)

;;; FR-523〜528: MOP Introspection Macros
;;; Class objects in the VM are native CL hash tables with keys like
;;; :__name__, :__superclasses__, :__slots__, :__initargs__, etc.
;;; These macros expand inline so user code doesn't need function dispatch.

(register-macro 'class-direct-superclasses
  (lambda (form env)
    (declare (ignore env))
    (let ((class (second form))
          (c (gensym "C")))
      (list 'let (list (list c class))
            (list 'when (list 'hash-table-p c)
                  (list 'gethash :__superclasses__ c))))))

(register-macro 'class-direct-slots
  (lambda (form env)
    (declare (ignore env))
    (let ((class (second form))
          (c (gensym "C")))
      (list 'let (list (list c class))
            (list 'when (list 'hash-table-p c)
                  (list '%class-slot-definitions c :__direct-slots__))))))

(register-macro 'class-slots
  (lambda (form env)
    (declare (ignore env))
    (let ((class (second form))
          (c (gensym "C")))
      (list 'let (list (list c class))
            (list 'when (list 'hash-table-p c)
                  (list '%class-slot-definitions c))))))

(register-macro 'class-direct-default-initargs
  (lambda (form env)
    (declare (ignore env))
    (let ((class (second form))
          (c (gensym "C")))
      (list 'let (list (list c class))
            (list 'when (list 'hash-table-p c)
                  (list 'gethash :__default-initargs__ c))))))

(register-macro 'generic-function-methods
  (lambda (form env)
    (declare (ignore env))
    (let ((gf (second form))
          (gf-v (gensym "GF-V"))
          (ht-v (gensym "HT-V")))
      (list 'let* (list (list gf-v gf)
                        (list ht-v (list 'and (list 'hash-table-p gf-v)
                                         (list 'gethash :__methods__ gf-v))))
            (list 'when ht-v
                  (list 'hash-table-values ht-v))))))

(register-macro 'generic-function-method-combination
  (lambda (form env)
    (declare (ignore env))
    (let ((gf (second form))
          (gf-v (gensym "GF-V")))
      (list 'let (list (list gf-v gf))
            (list 'if (list 'and (list 'hash-table-p gf-v)
                            (list 'gethash :__method-combination__ gf-v))
                  (list 'gethash :__method-combination__ gf-v)
                  '(quote standard))))))

(register-macro 'class-precedence-list
  (lambda (form env)
    (declare (ignore env))
    (let ((class-form (second form))
          (c (gensym "C"))
          (name-v (gensym "NAME-V"))
          (result-v (gensym "RESULT-V"))
          (seen-v (gensym "SEEN-V"))
          (queue-v (gensym "QUEUE-V"))
          (s-v (gensym "S-V"))
          (sht-v (gensym "SHT-V")))
      (list 'let (list (list c class-form))
            (list 'when (list 'hash-table-p c)
                  (list 'let (list (list name-v (list 'gethash :__name__ c)))
                        (list 'when name-v
                              (list 'let (list (list result-v (list 'list name-v))
                                               (list seen-v (list 'list name-v))
                                               (list queue-v (list 'gethash :__superclasses__ c)))
                                    (list 'dolist (list s-v queue-v)
                                          (list 'unless (list 'member s-v seen-v)
                                                (list 'push s-v result-v)
                                                (list 'push s-v seen-v)
                                                (list 'let (list (list sht-v (list 'find-class s-v)))
                                                      (list 'when (list 'hash-table-p sht-v)
                                                            (list 'dolist (list s-v (list 'gethash :__superclasses__ sht-v))
                                                                  (list 'unless (list 'member s-v seen-v)
                                                                        (list 'push s-v result-v)
                                                                        (list 'push s-v seen-v)))))))
                                    (list 'nreverse result-v)))))))))

;;; FR-302: parse-float — not in ANSI CL but requested; implemented via read-from-string

(register-macro 'parse-float
  (lambda (form env)
    (declare (ignore env))
    (let ((string (second form))
          (start (third form))
          (sv (gensym "SV")))
      (list 'let (list (list sv (list 'if start (list 'subseq string start) string)))
            (list 'float (list 'read-from-string sv))))))

;;; FR-1005: reinitialize-instance and shared-initialize

(register-macro '%apply-instance-initargs
  (lambda (form env)
    (declare (ignore env))
    (let ((instance (second form))
          (class-initargs (third form))
          (initargs (fourth form))
          (slot-names (fifth form))
          (inst-v (gensym "INST"))
          (imap-v (gensym "IMAP"))
          (pairs-v (gensym "PAIRS"))
          (slots-v (gensym "SLOTS"))
          (rest-v (gensym "REST"))
          (key-v (gensym "KEY"))
          (value-v (gensym "VALUE"))
          (binding-v (gensym "BINDING"))
          (slot-v (gensym "SLOT")))
      (list 'let (list (list inst-v instance)
                       (list imap-v class-initargs)
                       (list pairs-v initargs)
                       (list slots-v slot-names))
            (list 'loop 'for rest-v 'on pairs-v 'by (list 'function 'cddr)
                  'while (list 'and imap-v (list 'cdr rest-v))
                  'do (list 'let* (list (list key-v (list 'car rest-v))
                                        (list value-v (list 'cadr rest-v))
                                        (list binding-v (list 'assoc key-v imap-v)))
                            (list 'when binding-v
                                  (list 'let* (list (list slot-v (list 'cdr binding-v)))
                                        (list 'when (list 'or (list 'null slots-v)
                                                             (list 'eq slots-v t)
                                                             (list 'member slot-v slots-v))
                                              (list 'setf (list 'gethash slot-v inst-v) value-v))))))
            inst-v))))

(register-macro 'reinitialize-instance
  (lambda (form env)
    (declare (ignore env))
    (let ((instance (second form))
          (initargs (cddr form))
          (inst-v (gensym "INST"))
          (args-v (gensym "ARGS"))
          (class-v (gensym "CLASS"))
          (imap-v (gensym "IMAP")))
      (list 'let* (list (list inst-v instance)
                        (list args-v (cons 'list initargs))
                        (list class-v (list 'gethash :__class__ inst-v))
                        (list imap-v (list 'when (list 'hash-table-p class-v)
                                           (list 'gethash :__initargs__ class-v))))
            (list '%apply-instance-initargs inst-v imap-v args-v)
            inst-v))))

(register-macro 'shared-initialize
  (lambda (form env)
    (declare (ignore env))
    (let ((instance (second form))
          (slot-names (third form))
          (initargs (cdddr form))
          (inst-v (gensym "INST"))
          (slots-v (gensym "SLOTS"))
          (args-v (gensym "ARGS"))
          (class-v (gensym "CLASS"))
          (imap-v (gensym "IMAP")))
      (list 'let* (list (list inst-v instance)
                        (list slots-v slot-names)
                        (list args-v (cons 'list initargs))
                        (list class-v (list 'gethash :__class__ inst-v))
                        (list imap-v (list 'when (list 'hash-table-p class-v)
                                           (list 'gethash :__initargs__ class-v))))
            (list '%apply-instance-initargs inst-v imap-v args-v slots-v)
            inst-v))))
