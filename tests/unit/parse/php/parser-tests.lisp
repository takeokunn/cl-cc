;;;; tests/unit/parse/php/parser-tests.lisp — PHP Parser tests
(in-package :cl-cc/test)
(in-suite cl-cc-suite)

(deftest php-parser-while-lowering
  "while forms lower to a block-based loop AST."
  (let ((ast (first (parse-php-source "<?php while ($x) { echo 1; }"))))
    (assert-true (typep ast 'cl-cc:ast-block))))

(deftest php-parser-foreach-lowering
  "foreach forms lower to a let-based loop AST."
  (let ((ast (first (parse-php-source "<?php foreach ($items as $item) { echo $item; }"))))
    (assert-true (ast-let-p ast))))
