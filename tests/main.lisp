(defpackage cl-cc/tests/main
  (:use :cl :cl-cc :fiveam))
(in-package :cl-cc/tests/main)

(test add-test
      (is (= 2 (cl-cc::add 1 1))))
