;;; init.el --- Entry point -*- lexical-binding: t -*-

;; The configuration lives in lisp/init-*.el.  Those files are byte-compiled by
;; `make' (see Makefile); `load-prefer-newer' makes Emacs use a source file
;; instead whenever it is newer than its .elc.

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(dolist (dir '("lisp" "site-lisp"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))
(when (eq system-type 'gnu/linux)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp"))

(require 'init-package)
(require 'init-env)
(require 'init-core)
(require 'init-ui)
(require 'init-input)
(require 'init-completion)
(require 'init-lang)
(require 'init-org)
(require 'init-mail)

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;;; init.el ends here
