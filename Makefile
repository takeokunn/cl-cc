.PHONY: test load build clean

# ─── Common sbcl invocation ──────────────────────────────────────────────────
# All targets below share the same bootstrap: 4GB heap, non-interactive,
# ASDF + cl-cc.asd loaded. Additional --eval forms are appended per target.
SBCL := sbcl --dynamic-space-size 4096 --non-interactive \
              --eval '(require :asdf)' \
              --load cl-cc.asd

SBCL_IN_TREE_FASL := $(SBCL) --eval '(asdf:disable-output-translations)'

TEST_CACHE_DIR ?= /private/tmp/c

# Canonical test entry point. CLCC_PBT_COUNT defaults to 3 (bounded PBT)
# but is honored from the caller's environment if already set.
define run-test-suite
CLCC_PBT_COUNT=$${CLCC_PBT_COUNT:-3} $(SBCL) \
    --eval '(asdf:load-system :cl-cc/test)' \
    --eval '(uiop:symbol-call :cl-cc/test (quote run-tests))'
endef

# ─── Test target ─────────────────────────────────────────────────────────────
# Public test workflow is intentionally singular: make test.
test:
	$(run-test-suite)

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
