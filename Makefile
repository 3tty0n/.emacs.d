EMACS ?= emacs

SRC := $(wildcard lisp/*.el) site-lisp/my-lazy-package.el
ELC := $(SRC:.el=.elc)

.PHONY: all quickstart clean profile

all: $(ELC) quickstart

# Compile against the same load-path and use-package settings as init.el.
%.elc: %.el build.el
	$(EMACS) -Q --batch -l build.el -f batch-byte-compile $<

# Recompile everything when the shared bootstrap changes.
lisp/init-package.elc: site-lisp/my-lazy-package.elc
$(filter-out lisp/init-package.elc,$(filter lisp/%.elc,$(ELC))): lisp/init-package.elc

quickstart:
	$(EMACS) -Q --batch --eval '(progn (setq package-quickstart t) (package-initialize) (package-quickstart-refresh))'

# Startup time (in a real frame; batch mode skips GUI-only setup).
profile:
	$(EMACS) --eval '(run-with-timer 3 nil (lambda () (message "init: %s" (emacs-init-time)) (kill-emacs)))' 2>&1 | grep -a "init:" || true

clean:
	$(RM) $(ELC) package-quickstart.elc
