EMACS := emacs
USER_EMACS_DIR := ~/.emacs.d
LDFLAGS := -L site-lisp/ -L elpa/

ELISPS :=  init.el my-init.el
ELCS := $(ELISPS:.el=.elc)
SUBDIRS := site-lisp/

all: $(ELCS) $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

%.elc: %.el
	$(EMACS) -Q --batch -L . $(LDFLAGS) -f batch-byte-compile $<

clean:
	$(RM) $(ELCS) session.*
	$(MAKE) -C $(SUBDIRS) clean

.PHONY: all clean $(SUBDIRS)
