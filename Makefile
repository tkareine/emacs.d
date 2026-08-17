EMACS := emacs

EMACS_BATCH := $(EMACS) --batch --load=early-init.el

EL_FILES := $(shell find site-lisp -type f -name '*.el')

ELC_FILES := $(patsubst %.el,%.elc, $(EL_FILES))

TEST_FILES := $(wildcard test/*-test.el)

.PHONY: help
help: SHELL := bash
help:
	@echo -e "$(subst $(newline),\n,$(help_text))"

%.elc: %.el
	@echo Compile: $<
	@$(EMACS_BATCH) \
	  --quick \
	  --directory=site-lisp \
	  --funcall=batch-byte-compile \
	  $<

.PHONY: compile
compile: $(ELC_FILES)

.PHONY: recompile-packages
recompile-packages:
	$(EMACS_BATCH) \
	  --load=etc/tk-network.el \
	  --load=etc/tk-packages.el \
	  --funcall=tk-packages/recompile-packages

.PHONY: reinstall-packages
reinstall-packages: .cache/make/reinstall-packages.sentinel

.cache/make/reinstall-packages.sentinel: elpaca-lock.eld
	rm -fr elpaca/builds
	$(EMACS_BATCH) \
	  --load=etc/tk-network.el \
	  --load=etc/tk-packages.el \
	  --funcall=tk-packages/reinstall-packages
	mkdir -p "$(@D)"
	touch "$@"

.PHONY: reinstall-treesit-language-grammars
reinstall-treesit-language-grammars:
	$(EMACS_BATCH) \
	  --load=etc/tk-network.el \
	  --load=etc/tk-packages.el \
          --eval="(defvar tk-looks/minor-mode-alist '())" \
	  --load=etc/tk-dev.el \
	  --eval='(tk-dev/treesit-install-language-grammars t)'

.PHONY: test
test:
	$(EMACS_BATCH) \
	  --quick \
	  --directory=site-lisp \
	  --directory=site-lisp/tk-support \
	  $(foreach test,$(TEST_FILES),--load=$(test)) \
	  --funcall=ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	rm -f custom.el
	find site-lisp -name '*.elc' -delete

.PHONY: clobber
clobber: clean
	rm -rf .cache eln-cache elpa elpaca tree-sitter

define newline


endef

define help_text
Targets:

  help                                 Show this guide
  compile                              Compile site-lisp/**/*.el to .elc
  recompile-packages                   Recompile installed packages
  reinstall-packages                   Reinstall packages by lockfile
  reinstall-treesit-language-grammars  Reinstall Tree-sitter language grammars
  test                                 Run tests
  clean                                Delete custom.el and compiled files
  clobber                              Clean, then delete downloaded files
endef
