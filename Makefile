.PHONY: test test-fast test-full test-selfhost test-pbt test-all load build clean test-core clean-test-core

# ─── Common sbcl invocation ──────────────────────────────────────────────────
# All targets below share the same bootstrap: 4GB heap, non-interactive,
# ASDF + cl-cc.asd loaded. Additional --eval forms are appended per target.
SBCL := sbcl --dynamic-space-size 4096 --non-interactive \
              --eval '(require :asdf)' \
              --load cl-cc.asd

SBCL_IN_TREE_FASL := $(SBCL) --eval '(asdf:disable-output-translations)'

TEST_CACHE_DIR ?= /private/tmp/c

# Template for running a named test runner function from :cl-cc/test.
# Each test-* target sets RUNNER; `$(run-test-suite)` expands to the full
# sbcl command. CLCC_PBT_COUNT defaults to 3 (bounded PBT) but is honored
# from the caller's environment if already set.
define run-test-suite
CLCC_PBT_COUNT=$${CLCC_PBT_COUNT:-3} $(SBCL_IN_TREE_FASL) \
    --eval '(asdf:load-system :cl-cc/test)' \
    --eval '(uiop:symbol-call :cl-cc/test (quote $(RUNNER)))'
endef

# ─── Test targets ────────────────────────────────────────────────────────────
# Default `make test` is the fast path: excludes slow/selfhost/PBT suites.
test: test-fast

# Per-suite runner selection. Adding a new suite = one line here + one line
# in the shared recipe rule below.
test-fast:     RUNNER := run-tests
test-full:     RUNNER := run-tests-extended
test-selfhost: RUNNER := run-selfhost-tests
test-pbt:      RUNNER := run-pbt-tests
test-all:      RUNNER := run-all-tests

# Shared recipe. Every test-* target expands the same template.
#   test-fast     — excludes slow/selfhost/PBT (matches run-tests 0-arg default)
#   test-full     — everything EXCEPT selfhost/PBT (practical full run)
#   test-selfhost — selfhost integration suite only
#   test-pbt      — property-based suites only
#   test-all      — literally everything (slow; local validation)
test-fast test-full test-selfhost test-pbt test-all:
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
test-core:
	@printf '%s\n' 'test-core is currently disabled; direct SBCL runner is used for stability.'

clean-test-core:
	@true
