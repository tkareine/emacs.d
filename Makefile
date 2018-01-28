SHELL := bash  # required for `help` target

EMACS ?= emacs

TEST_FILES ?= $(wildcard test/*-test.el)

.PHONY: help
help:
	@echo -e '$(subst $(newline),\n,$(help_text))'

.PHONY: test
test:
	$(EMACS) -batch -l ert $(foreach test,$(TEST_FILES),-l $(test)) -f ert-run-tests-batch-and-exit

define newline


endef

define help_text
Targets:

  help     Show this guide.
  test     Run tests.
endef
