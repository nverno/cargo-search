BIN = $(CURDIR)/dist/index.js
SRC = $(wildcard src/*.ts)

install: cratesio

cratesio: $(BIN)
$(BIN): $(SRC)
	@npm run build

.PHONY: clean distclean
clean:
	$(RM) -r *~

distclean: clean
	$(RM) -rf dist

