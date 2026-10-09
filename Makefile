EMACS ?= emacs

ELC := init.elc config.elc site-lisp/my-lazy-package.elc

.PHONY: all quickstart recompile-packages clean profile

all: $(ELC) quickstart

# Compile against the same use-package settings as config.el.
%.elc: %.el build.el
	$(EMACS) -Q --batch -l build.el -f batch-byte-compile $<

config.elc: site-lisp/my-lazy-package.elc

quickstart:
	$(EMACS) -Q --batch --eval '(progn (setq package-quickstart t) (package-initialize) (package-quickstart-refresh))'

# Rebuild installed packages after upgrading Emacs, then refresh autoloads.
recompile-packages:
	$(EMACS) -Q --batch --eval '(progn (require (quote package)) (package-initialize) (package-recompile-all))'
	$(MAKE) quickstart

# Startup time (in a real frame; batch mode skips GUI-only setup).
profile:
	$(EMACS) --eval '(run-with-timer 3 nil (lambda () (message "init: %s" (emacs-init-time)) (kill-emacs)))' 2>&1 | grep -a "init:" || true

clean:
	$(RM) $(ELC) package-quickstart.elc
