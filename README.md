# .emacs.d

Personal Emacs configuration (Emacs 30+; developed on 31).

Installation:

```shell
$ git clone --recursive git@github.com:3tty0n/.emacs.d.git ~/.emacs.d
$ cd ~/.emacs.d && make
```

| Path | Role |
| --- | --- |
| `early-init.el` | GC/UI tweaks, package quickstart |
| `init.el` | load-path and module list |
| `lisp/init-package.el` | use-package bootstrap, lazy installs (`site-lisp/my-lazy-package.el`) |
| `lisp/init-env.el` | cached shell `PATH`, server, terminal mouse |
| `lisp/init-core.el` | editing, history, large-file safeguards |
| `lisp/init-ui.el` | theme, modeline, line numbers, tabs, dashboard |
| `lisp/init-input.el` | Japanese input (ddskk) |
| `lisp/init-completion.el` | vertico / consult / company / projectile |
| `lisp/init-prog.el` | eglot, lsp-mode, flymake, magit, shells |
| `lisp/init-lang.el` | language modes, LaTeX, PDF tools |
| `lisp/init-org.el` | org, markdown, notes, AI tools |
| `lisp/init-mail.el` | mu4e / calendar, loaded when idle |
| `custom.el` | Customize output |

## Build

`make` byte-compiles `lisp/*.el` and regenerates `package-quickstart.el`.
Run it after editing the config or installing/removing packages; stale `.elc`
files are ignored automatically (`load-prefer-newer`).
`make profile` prints the startup time.
