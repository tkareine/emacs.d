EMACS ?= emacs

TEST_FILES ?= $(wildcard test/*-test.el)

.PHONY: help
help:
	@bash -c 'echo -e "$(subst $(newline),\n,$(help_text))"'

.PHONY: upgrade-packages
upgrade-packages:
	$(EMACS) --batch -l etc/tk-packages.el -f tk-packages/upgrade-packages

.PHONY: test
test:
	$(EMACS) -Q --batch -L lib -l ert $(foreach test,$(TEST_FILES),-l $(test)) -f ert-run-tests-batch-and-exit

define newline


endef

define help_text
Targets:

  help              Show this guide.
  upgrade-packages  Upgrade installed packages.
  test              Run tests.
endef
