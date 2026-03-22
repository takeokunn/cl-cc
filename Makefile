.PHONY: test load clean

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

clean:
	rm -rf *.fasl *.lib *.dex
	find . -name "*.fasl" -delete
