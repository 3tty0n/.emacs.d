EMACS := emacs
USER_EMACS_DIR := ~/.emacs.d
LDFLAGS := -L site-lisp/ -L elpa/
BATCHFLAG := -L $(USER_EMACS_DIR)/site-lisp -L $(USER_EMACS_DIR)/elpa

ELISPS := $(shell find . -maxdepth 1 -name "*.el" -type f)
ELCS := $(ELISPS:.el=.elc)

.PHONY: all
all: compile

.PHONY: compile
compile: $(ELCS)

%.elc: %.el
	$(EMACS) -Q --batch -L . $(LDFLAGS) -f batch-byte-compile $<

.PHONY: clean
clean:
	$(RM) $(ELCS)
