(in-package :cl-cc/test)

  
(def-suite control-flow-tests
  :description "Tests for control flow forms: block, return-from, tagbody, go, catch, throw, unwind-protect, setq, quote, the.")
(in-suite control-flow-tests)

;; ----------------------------------------------------------------------------
;; setq tests
;; ----------------------------------------------------------------------------

(test setq-basic
  (is-compile-string "(let ((x 1)) (setq x 42) x)" :target :x86_64)
  (let ((result (run-string "(let ((x 1)) (setq x 42) x)")))
    (is (= result 42))))

(test setq-multiple
  (is-compile-string "(let ((x 1) (y 2)) (setq x (+ x y 10)) x)" :target :x86_64)
  (let ((result (run-string "(let ((x 1) (y 2)) (setq x (+ x y 10)) x)")))
    (is (= result 12))))

;; ----------------------------------------------------------------------------
;; quote tests
;; ----------------------------------------------------------------------------

(test quote-integer
  (is-compile-string "(quote 42)" :target :x86_64)
  (let ((result (run-string "(quote 42)")))
    (is (= result 42))))

(test quote-symbol
  (is-compile-string "(quote foo)" :target :x86_64))

(test quote-list
  (is-compile-string "(quote (1 2 3))" :target :x86_64))

(test quote-nested
  (is-compile-string "(quote (a (b c) d))" :target :x86_64))

;; ----------------------------------------------------------------------------
;; the tests
;; ----------------------------------------------------------------------------

(test the-integer
  (is-compile-string "(the integer 42)" :target :x86_64)
  (let ((result (run-string "(the integer 42)")))
    (is (= result 42))))

(test the-in-expression
  (is-compile-string "(the integer (+ 1 2))" :target :x86_64)
  (let ((result (run-string "(the integer (+ 1 2))")))
    (is (= result 3))))

;; ----------------------------------------------------------------------------
;; block/return-from tests
;; ----------------------------------------------------------------------------

(test block-normal-return
  (is-compile-string "(block foo 42)" :target :x86_64)
  (let ((result (run-string "(block foo 42)")))
    (is (= result 42))))

(test block-early-return
  (is-compile-string "(block foo (return-from foo 10) 20)" :target :x86_64)
  (let ((result (run-string "(block foo (return-from foo 10) 20)")))
    (is (= result 10))))

(test block-nested
  (is-compile-string "(block outer (block inner (return-from inner 5) 10) 20)" :target :x86_64)
  (let ((result (run-string "(block outer (block inner (return-from inner 5) 10) 20)")))
    (is (= result 20))))

;; ----------------------------------------------------------------------------
;; tagbody/go tests
;; ----------------------------------------------------------------------------

(test tagbody-simple
  (is-compile-string "(tagbody start (print 1) (go end) start (print 2) end (print 3) 42)" :target :x86_64)
  (let ((result (run-string "(tagbody start (print 1) (go end) start (print 2) end (print 3) 42)")))
    (is (= result 42))))

;; ----------------------------------------------------------------------------
;; catch/throw tests
;; ----------------------------------------------------------------------------

(test catch-no-throw
  (is-compile-string "(catch 'foo 42)" :target :x86_64)
  (let ((result (run-string "(catch 'foo 42)")))
    (is (= result 42))))

(test unwind-protect-normal
  (is-compile-string "(unwind-protect 42 (print \"cleanup\"))" :target :x86_64)
  (let ((result (run-string "(unwind-protect 42 (print \"cleanup\"))")))
    (is (= result 42))))

(test multiple-value-prog1
  (is-compile-string "(multiple-value-prog1 42 (print 1) (print 2))" :target :x86_64)
  (let ((result (run-string "(multiple-value-prog1 42 (print 1) (print 2))")))
    (is (= result 42))))

;; ----------------------------------------------------------------------------
;; Run control flow tests
;; ----------------------------------------------------------------------------

(defun run-control-flow-tests ()
  (run! 'control-flow-tests))
