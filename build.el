;;; build.el --- Batch byte-compilation environment -*- lexical-binding: t -*-

;; Loaded with `emacs -Q --batch -l build.el'.  Mirrors the use-package setup
;; of config.el so it compiles against the same settings.

(setq load-prefer-newer t)
(package-initialize)
(add-to-list 'load-path
             (expand-file-name "site-lisp" (file-name-directory
                                            (or load-file-name buffer-file-name))))
(setq use-package-expand-minimally nil
      use-package-enable-imenu-support t
      use-package-always-ensure t)
(require 'use-package)
;; Record dependencies without installing them during compilation.  Compiled
;; declarations call the runtime ensure function selected by config.el.
(require 'my-lazy-package)
(my-lazy-package-mode 1)

;; `after-init-time' is already set when -l files run, which would let
;; my-lazy-package install missing packages during compilation.
(setq my-lazy-package--startup-p t)
