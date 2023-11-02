(defsystem "cl-cc"
  :version "0.1.0"
  :author "takeokunn"
  :license "GPLv3"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :in-order-to ((test-op (test-op "cl-cc/tests"))))

(defsystem "cl-cc/tests"
  :author "takeokunn"
  :license "GPLv3"
  :depends-on (:cl-cc :fiveam)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-cc"
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!)))
