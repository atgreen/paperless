EMACS ?= emacs
EL := paperless.el
ELC := $(EL:.el=.elc)
PACKAGE_DIR := $(shell cask package-directory)

$(ELC):%.elc:%.el
	cask exec ${EMACS} -L . -Q --batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $<

$(PACKAGE_DIR): Cask
	cask install
	touch $(PACKAGE_DIR)

build: $(PACKAGE_DIR) $(ELC)
