;;; build.el --- Batch byte-compilation environment -*- lexical-binding: t -*-

;; Loaded with `emacs -Q --batch -l build.el'.  Mirrors what init.el sets up so
;; the config compiles against the same load-path and use-package settings.

(setq load-prefer-newer t)
(package-initialize)
(dolist (dir '("lisp" "site-lisp"))
  (add-to-list 'load-path
               (expand-file-name dir (file-name-directory
                                      (or load-file-name buffer-file-name)))))
;; Sets `use-package-expand-minimally' and enables lazy (non-installing) ensure.
(require 'init-package)

;; `after-init-time' is already set when -l files run, which would let
;; my-lazy-package install missing packages during compilation.
(setq my-lazy-package--startup-p t)
