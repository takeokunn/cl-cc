;;;; printer-unparse.lisp — type unparsing and type-specifier recognition

(in-package :cl-cc/type)

(defun unparse-type (ty)
  "Convert a type-node back to a type specifier s-expression."
  (typecase ty
    (type-primitive (type-primitive-name ty))
    (type-var
     (if (type-var-name ty)
         (type-var-name ty)
         (intern (format nil "?T~D" (type-var-id ty)))))
    (type-arrow
     `(-> ,@(mapcar #'unparse-type (type-arrow-params ty))
          ,(unparse-type (type-arrow-return ty))))
    (type-product
     `(values ,@(mapcar #'unparse-type (type-product-elems ty))))
    (type-union
     (let ((name (type-union-constructor-name ty)))
       (if name
           `(,name ,@(mapcar #'unparse-type (type-constructor-args ty)))
           `(or ,@(mapcar #'unparse-type (type-union-types ty))))))
    (type-intersection
     `(and ,@(mapcar #'unparse-type (type-intersection-types ty))))
    (type-forall
     `(forall ,(type-var-name (type-forall-var ty))
              ,(unparse-type (type-forall-body ty))))
    (type-app
     (let ((name (type-constructor-name ty))
           (args (type-constructor-args ty)))
       (if name
           `(,name ,@(mapcar #'unparse-type args))
           (list (unparse-type (type-app-fun ty))
                 (unparse-type (type-app-arg ty))))))
    (t ty)))

(defparameter *primitive-type-name-strings*
  '("FIXNUM" "STRING" "BOOLEAN" "SYMBOL" "CONS" "NULL" "T" "FLOAT" "CHARACTER" "CHAR")
  "CL primitive type names recognised as type specifiers.")

(defparameter *shorthand-type-name-strings*
  '("INT" "BOOL" "TOP" "?")
  "Short-form type names recognised as type specifiers.")

(defparameter *composite-type-head-strings*
  '("OR" "AND" "FUNCTION" "VALUES" "CONS" "LIST" "VECTOR" "ARRAY" "OPTION"
    "->" "->0" "->1" "FORALL" "∀" "=>" "EXISTS" "MU" "REFINE" "RECORD" "VARIANT")
  "Head symbols that introduce composite type specifiers.")

(defparameter *atomic-type-name-strings*
  (append *primitive-type-name-strings* *shorthand-type-name-strings*)
  "Union of primitive and shorthand names for single-pass atomic type lookup.")

(defun looks-like-type-specifier-p (spec)
  "True iff SPEC looks like it might be a type specifier.
Uses string= for non-CL symbols to ensure package-independent matching."
  (flet ((sym-name-in (sym table)
           (and (symbolp sym) (member (symbol-name sym) table :test #'string=))))
    (or (and (symbolp spec)
             (or (sym-name-in spec *atomic-type-name-strings*)
                 (lookup-type-alias spec)))
        (and (consp spec)
             (let ((head (car spec)))
               (or (sym-name-in head *composite-type-head-strings*)
                   (and (symbolp head)
                        (char= (char (symbol-name head) 0) #\!))))))))
