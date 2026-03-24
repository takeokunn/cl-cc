(require :asdf)
(load "../cl-cc.asd")
(asdf:test-system :cl-cc/test)
(sb-ext:exit :code 0)
