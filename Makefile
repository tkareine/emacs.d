EMACS := emacs

EMACS_BATCH := $(EMACS) --batch

EL_FILES := $(shell find site-lisp -type f -name '*.el')

ELC_FILES := $(patsubst %.el,%.elc, $(EL_FILES))

TEST_FILES := $(wildcard test/*-test.el)

.PHONY: help
help: SHELL := bash
help:
	@echo -e "$(subst $(newline),\n,$(help_text))"

%.elc: %.el
	@echo Compile: $<
	@$(EMACS_BATCH) -L site-lisp -Q -f batch-byte-compile $<

.PHONY: compile
compile: $(ELC_FILES)

.PHONY: upgrade-packages
upgrade-packages:
	$(EMACS_BATCH) \
	  -l etc/tk-network.el \
	  -l etc/tk-packages.el \
	  -f tk-packages/upgrade-packages

.PHONY: recompile-packages
recompile-packages:
	$(EMACS_BATCH) \
	  -l etc/tk-network.el \
	  -l etc/tk-packages.el \
	  -f tk-packages/recompile-packages

.PHONY: test
test:
	$(EMACS_BATCH) -Q -L site-lisp -L site-lisp/tk-support \
	  $(foreach test,$(TEST_FILES),-l $(test)) \
	  -f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	rm -f custom.el
	find site-lisp -name '*.elc' -delete

define newline


endef

define help_text
Targets:

  help                Show this guide
  compile             Compile site-lisp/**/*.el to .elc
  upgrade-packages    Upgrade installed packages
  recompile-packages  Recompile installed packages
  test                Run tests
  clean               Delete compiled files
endef
