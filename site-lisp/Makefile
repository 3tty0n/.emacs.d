EMACS := emacs
USER_EMACS_DIR := ~/.emacs.d
BATCHFLAG := -L $(USER_EMACS_DIR)/site-lisp -L $(USER_EMACS_DIR)/elpa -batch -f batch-byte-compile

ELISPS := $(shell find . -maxdepth 1 -name "*.el" -type f)
ELCS := $(ELISPS:.el=.elc)

.PHONY: all
all: compile

.PHONY: compile
compile: $(ELCS)

%.elc: %.el
	$(EMACS) $(BATCHFLAG) $<

.PHONY: clean
clean:
	$(RM) $(ELCS)
