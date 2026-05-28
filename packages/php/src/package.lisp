(defpackage :cl-cc/php
  (:use :cl :cl-cc/ast :cl-cc/bootstrap :cl-cc/parse)
  (:export
   #:tokenize-php-source
    #:parse-php-source
    #:parse-php-source-to-cst
    #:+php-null+
    #:%php-array
    #:%php-array-empty-p
    #:%php-array-ref
    #:%php-array-set
    #:%php-eq-loose
    #:%php-eq-strict
    #:%php-null-p
    #:%php-to-number
    #:%php-truthy
    #:%php-value-type
    #:%php-count
    #:%php-strlen
    #:%php-strtolower
   #:%php-strtoupper
   #:%php-isset
   #:%php-array-key-exists
   #:*php-builtin-map*
   #:php-check-supported-forms))
