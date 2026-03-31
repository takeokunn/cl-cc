(in-package :cl-cc/test)

(defsuite macros-restarts-suite
  :description "Restart protocol expansion tests"
  :parent cl-cc-suite)

(in-suite macros-restarts-suite)

(deftest macros-restarts-lookup-expands
  "FIND-RESTART and COMPUTE-RESTARTS expand to the active restart forms."
  (assert-eq 'let (car (our-macroexpand-1 '(find-restart 'my-restart))))
  (assert-eq 'cl-cc::*%active-restarts* (our-macroexpand-1 '(compute-restarts))))

(deftest macros-restarts-control-expands
  "INVOKE-RESTART, ABORT, and RESTART-NAME expand to control forms."
  (assert-eq 'let* (car (our-macroexpand-1 '(invoke-restart 'my-restart))))
  (assert-eq 'let (car (our-macroexpand-1 '(abort))))
  (assert-eq 'if (car (our-macroexpand-1 '(restart-name r)))))
