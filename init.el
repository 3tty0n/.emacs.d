;;; init.el --- Entry point -*- lexical-binding: t -*-

;; The configuration lives in config.el, byte-compiled by `make' (see Makefile).
;; `load-prefer-newer' makes Emacs use config.el whenever it is newer than
;; config.elc.

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(when (eq system-type 'gnu/linux)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp"))

(load (expand-file-name "config" user-emacs-directory) nil 'nomessage)

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;;; init.el ends here
