(in-package :cl-cc/expand)
;;;; DEFSTRUCT copy-structure expansion.

;;; FR-555: copy-structure — VM structs are hash-tables with :__class__.
;;; Typed defstructs (:type list/vector) are represented as sequences, so this
;;; must preserve shallow-copy semantics across all registered defstruct
;;; representations using expansion-time metadata.
(register-macro 'copy-structure
  (lambda (form env)
    (declare (ignore env))
    (let ((struct (second form))
          (s (gensym "STRUCT")))
      (list 'let (list (list s struct))
            (cons 'cond
                  (append
                   (loop for name being the hash-keys of *defstruct-slot-registry*
                         using (hash-value slots)
                         for struct-type = (gethash name *defstruct-type-registry*)
                         collect
                         (if struct-type
                             (if (eq struct-type 'list)
                                 (list (list 'and (list 'listp s)
                                             (list 'consp s)
                                             (list 'eq (list 'car s) (list 'quote name)))
                                       (cons 'list
                                             (cons (list 'quote name)
                                                   (loop for _slot in slots
                                                         for idx from 1
                                                         collect (list 'nth idx s)))))
                                 (list (list 'and (list 'vectorp s)
                                             (list '> (list 'length s) 0)
                                             (list 'eq (list 'aref s 0) (list 'quote name)))
                                       (cons 'vector
                                             (cons (list 'quote name)
                                                   (loop for _slot in slots
                                                         for idx from 1
                                                         collect (list 'aref s idx))))))
                             (list (list 'typep s (list 'quote name))
                                   (cons 'let
                                         (cons (list (list 'copy (list 'make-instance (list 'quote name))))
                                               (append
                                                (loop for slot in slots
                                                      for slot-name = (first slot)
                                                      collect (list 'setf
                                                                    (list 'slot-value 'copy (list 'quote slot-name))
                                                                    (list 'slot-value s (list 'quote slot-name))))
                                                (list 'copy)))))))
                   (list (list 't (list 'error "copy-structure: unsupported object ~S" s)))))))))
