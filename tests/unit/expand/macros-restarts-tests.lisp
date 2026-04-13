(in-package :cl-cc/test)

(defsuite macros-restarts-suite
  :description "Restart protocol expansion tests"
  :parent cl-cc-suite)

(in-suite macros-restarts-suite)

(deftest-each macros-restarts-control-expansion
  "Restart control forms expand to their expected top-level operators."
  :cases (("find-restart"    '(find-restart 'my-restart) 'let)
          ("invoke-restart"  '(invoke-restart 'my-restart) 'let*)
          ("abort"           '(abort)                      'let)
          ("restart-name"    '(restart-name r)             'if))
  (form expected-car)
  (assert-eq expected-car (car (our-macroexpand-1 form))))

(deftest macros-restarts-compute-restarts-expands
  "COMPUTE-RESTARTS expands to the active-restarts dynamic variable directly."
  (assert-eq 'cl-cc::*%active-restarts* (our-macroexpand-1 '(compute-restarts))))
