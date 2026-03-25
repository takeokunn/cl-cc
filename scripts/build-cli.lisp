;;;; scripts/build-cli.lisp — Build the cl-cc standalone binary
;;;;
;;;; Usage (called by 'make build'):
;;;;   sbcl --non-interactive \
;;;;        --eval '(require :asdf)' \
;;;;        --load cl-cc.asd \
;;;;        --eval '(asdf:load-system :cl-cc/bin)' \
;;;;        --load scripts/build-cli.lisp

(format t "~&; Building cl-cc standalone binary...~%")
(force-output)

(sb-ext:save-lisp-and-die
 "cl-cc"
 :toplevel      #'cl-cc/cli:main
 :executable    t
 :save-runtime-options t)
