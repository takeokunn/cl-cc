;;;; macros-compat-mop.lisp — MOP introspection macros + parse-float + reinitialize-instance
(in-package :cl-cc/expand)

;;; FR-523〜528: MOP Introspection Macros
;;; Class objects in the VM are native CL hash tables with keys like
;;; :__name__, :__superclasses__, :__slots__, :__initargs__, etc.
;;; These macros expand inline so user code doesn't need function dispatch.

(register-macro 'class-direct-superclasses
  (lambda (form env)
    (declare (ignore env))
    (let ((class (second form)))
      (with-gensyms (c)
        (list 'let (list (list c class))
              (list 'when (list 'hash-table-p c)
                    (list 'gethash :__superclasses__ c)))))))

(register-macro 'class-direct-slots
  (lambda (form env)
    (declare (ignore env))
    (let ((class (second form)))
      (with-gensyms (c)
        (list 'let (list (list c class))
              (list 'when (list 'hash-table-p c)
                    (list '%class-slot-definitions c)))))))

(register-macro 'class-slots
  (lambda (form env)
    (declare (ignore env))
    (let ((class (second form)))
      (with-gensyms (c)
        (list 'let (list (list c class))
              (list 'when (list 'hash-table-p c)
                    (list '%class-slot-definitions c)))))))

(register-macro 'class-direct-default-initargs
  (lambda (form env)
    (declare (ignore env))
    (let ((class (second form)))
      (with-gensyms (c)
        (list 'let (list (list c class))
              (list 'when (list 'hash-table-p c)
                    (list 'gethash :__default-initargs__ c)))))))

(register-macro 'generic-function-methods
  (lambda (form env)
    (declare (ignore env))
    (let ((gf (second form)))
      (with-gensyms (gf-v ht-v)
        (list 'let* (list (list gf-v gf)
                          (list ht-v (list 'and (list 'hash-table-p gf-v)
                                           (list 'gethash :__methods__ gf-v))))
              (list 'when ht-v
                    (list 'hash-table-values ht-v)))))))

(register-macro 'generic-function-method-combination
  (lambda (form env)
    (declare (ignore env))
    (let ((gf (second form)))
      (with-gensyms (gf-v)
        (list 'let (list (list gf-v gf))
              (list 'if (list 'and (list 'hash-table-p gf-v)
                              (list 'gethash :__method-combination__ gf-v))
                    (list 'gethash :__method-combination__ gf-v)
                    '(quote standard)))))))

(register-macro 'class-precedence-list
  (lambda (form env)
    (declare (ignore env))
    (let ((class-form (second form)))
      (with-gensyms (c name-v result-v seen-v queue-v s-v sht-v)
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
                                      (list 'nreverse result-v))))))))))

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

(defun %apply-instance-initargs (instance class-initargs initargs &optional slot-names)
  "Apply INITARGS to INSTANCE using CLASS-INITARGS.

CLASS-INITARGS is the class descriptor's initarg association list mapping initarg
keywords to slot names. When SLOT-NAMES is NIL, apply all matching initargs.
When SLOT-NAMES is T, also apply all matching initargs. Otherwise only update
slots explicitly listed in SLOT-NAMES. Returns INSTANCE."
  (when (and class-initargs initargs)
    (loop for (key value) on initargs by #'cddr
          while value
          for slot-binding = (assoc key class-initargs)
          for slot-name = (cdr slot-binding)
          when (and slot-binding
                    (or (null slot-names)
                        (eq slot-names t)
                        (member slot-name slot-names)))
            do (setf (gethash slot-name instance) value)))
  instance)

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
