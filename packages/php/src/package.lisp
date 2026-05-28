(defpackage :cl-cc/php
  (:use :cl :cl-cc/ast :cl-cc/bootstrap :cl-cc/parse)
  (:export
   #:tokenize-php-source
   #:parse-php-source
   #:parse-php-source-to-cst
   #:%php-array
   #:%php-array-ref
   #:%php-array-set
   #:%php-count
   #:%php-strlen
   #:%php-strtolower
   #:%php-strtoupper
   #:%php-isset
   #:%php-array-key-exists
   #:*php-builtin-map*
   #:php-check-supported-forms))
