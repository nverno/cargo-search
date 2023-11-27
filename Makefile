emacs ?= emacs
wget  ?= wget

.PHONY: test
all: test
test:
	$(emacs) -Q -batch -L . -l ert -l test/cargo-search-tests.el \
		-f ert-run-tests-batch-and-exit

clean:
	$(RM) *~

distclean: clean
	$(RM) -rf $$(git ls-files --others --ignored --exclude-standard)
