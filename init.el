;;; init.el --- Entry point -*- lexical-binding: t -*-

;; The configuration lives in lisp/*.el; `make' (see Makefile) concatenates it
;; into config.el and compiles that.  `load-prefer-newer' makes Emacs use
;; config.el instead whenever it is newer than config.elc.

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(dolist (dir '("lisp" "site-lisp"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))
(when (eq system-type 'gnu/linux)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp"))

(defvar my/config-parts
  '(init-package init-env init-core init-ui init-input init-completion
    init-lang init-org init-mail))

;; `make' concatenates lisp/*.el into config.el (one file to grep, one .elc
;; to load).  Use it only when it is newer than every source.
(let ((config (expand-file-name "config.el" user-emacs-directory)))
  (if (and (file-exists-p config)
           (let ((mtime (file-attribute-modification-time (file-attributes config))))
             (seq-every-p (lambda (f)
                         (time-less-p (file-attribute-modification-time (file-attributes f)) mtime))
                       (directory-files (expand-file-name "lisp" user-emacs-directory) t "\\.el\\'"))))
      (load (file-name-sans-extension config) nil 'nomessage)
    (mapc #'require my/config-parts)))

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;;; init.el ends here
