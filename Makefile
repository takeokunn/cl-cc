.PHONY: test load build clean

test:
	sbcl --non-interactive \
	     --eval '(require :asdf)' \
	     --load cl-cc.asd \
	     --eval '(asdf:load-system :cl-cc/test)' \
	     --eval '(uiop:symbol-call :cl-cc/test (quote run-tests))'

load:
	sbcl --non-interactive \
	     --eval '(require :asdf)' \
	     --load cl-cc.asd \
	     --eval '(asdf:load-system :cl-cc)'

build:
	sbcl --non-interactive \
	     --eval '(require :asdf)' \
	     --load cl-cc.asd \
	     --eval '(asdf:load-system :cl-cc/bin)' \
	     --load scripts/build-cli.lisp

clean:
	rm -rf *.fasl *.lib *.dex
	find . -name "*.fasl" -delete
