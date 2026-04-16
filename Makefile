.PHONY: test coverage load build clean

# ─── Common sbcl invocation ──────────────────────────────────────────────────
# All targets below share the same bootstrap: 4GB heap, non-interactive,
# ASDF + cl-cc.asd loaded. Additional --eval forms are appended per target.
SBCL := nix run nixpkgs\#sbcl -- --dynamic-space-size 4096 --non-interactive \
              --eval '(require :asdf)' \
              --load cl-cc.asd

SBCL_IN_TREE_FASL := $(SBCL) --eval '(asdf:disable-output-translations)'

TEST_CACHE_DIR ?= /private/tmp/c

# Canonical test entry point. CLCC_PBT_COUNT defaults to 3 (bounded PBT)
# but is honored from the caller's environment if already set.
define run-test-suite
CLCC_PBT_COUNT=$${CLCC_PBT_COUNT:-3} $(SBCL) \
    --eval '(asdf:load-system :cl-cc/test :force t)' \
    --eval '(uiop:symbol-call :cl-cc/test (quote run-tests))'
endef

# ─── Test target ─────────────────────────────────────────────────────────────
# Public test workflow is intentionally singular: make test.
test:
	$(run-test-suite)

coverage:
	rm -rf /tmp/cl-cc-coverage
	perl -e 'use File::Path qw(remove_tree); use File::Find; my $$root=$$ENV{"HOME"}."/.cache/common-lisp"; exit 0 unless -d $$root; my @targets; find(sub { return unless -d $$_; my $$p=$$File::Find::name; push @targets, $$p if index($$p, "/Users/take/ghq/github.com/takeokunn/cl-cc") >= 0 }, $$root); eval { remove_tree(@targets) if @targets; 1 } or warn $$@;' || true
	find . -name "*.fasl" -delete
	nix run nixpkgs\#sbcl -- --dynamic-space-size 4096 --non-interactive \
	    --eval '(require :asdf)' \
	    --eval '(require :sb-cover)' \
	    --eval '(declaim (optimize (sb-cover:store-coverage-data 3)))' \
	    --load cl-cc.asd \
	    --eval '(asdf:load-system :cl-cc/test :force t)' \
	    --eval '(cl-cc/test:run-suite (quote cl-cc/test::cl-cc-suite) :parallel nil :random nil :warm-stdlib t :coverage t)'

# ─── Other targets ───────────────────────────────────────────────────────────
load:
	$(SBCL_IN_TREE_FASL) --eval '(asdf:load-system :cl-cc)'

build:
	$(SBCL_IN_TREE_FASL) --eval '(asdf:load-system :cl-cc/bin)' \
	        --load scripts/build-cli.lisp

clean:
	rm -rf *.fasl *.lib *.dex
	find . -name "*.fasl" -delete
	rm -rf $(TEST_CACHE_DIR)
