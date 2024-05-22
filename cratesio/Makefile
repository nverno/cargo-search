BIN     = $(CURDIR)/dist/index.js
SRC     = $(wildcard src/*.ts)
PREFIX ?= $(shell echo "$$HOME/.local/bin")

define SCRIPT
#!/usr/bin/env bash
node "$(CURDIR)/dist/index.js" "$$@"
endef
export SCRIPT

.PHONY: install cratesio all
all:
	@

install: cratesio

cratesio: $(PREFIX)/cratesio

$(PREFIX)/cratesio: $(BIN)
	@printf "%s" "$$SCRIPT" > $@
	@chmod +x $@

$(BIN): $(SRC)
	@npm run build


.PHONY: clean distclean
clean:
	$(RM) -r *~

distclean: clean
	$(RM) -rf dist

