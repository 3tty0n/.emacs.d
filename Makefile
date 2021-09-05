EMACS := emacs
USER_EMACS_DIR := ~/.emacs.d
BATCHFLAG := -L site-lisp/ -L elpa/ -L straight/repo/ -batch -f batch-byte-compile

ELISPS :=  init.el my-init.el
ELCS := $(ELISPS:.el=.elc)
SUBDIRS := site-lisp/

all: $(ELCS) $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

%.elc: %.el
	$(EMACS) $(BATCHFLAG) $<

clean:
	$(RM) $(ELCS)
	$(MAKE) -C $(SUBDIRS) clean

.PHONY: all clean $(SUBDIRS)
