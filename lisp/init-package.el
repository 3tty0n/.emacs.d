;;; init-package.el --- Package management bootstrap -*- lexical-binding: t; -*-

;; Packages are activated before init.el runs (`package-enable-at-startup' and
;; `package-quickstart' in early-init.el), so nothing here calls
;; `package-initialize'.

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)

(eval-and-compile
  ;; Must be set before use-package is loaded.  Minimal expansion keeps the
  ;; byte-compiled code small; the full expansion is only useful when debugging.
  (setq use-package-expand-minimally (bound-and-true-p byte-compile-current-file)
        use-package-enable-imenu-support t))
(require 'use-package)

;; Never contact package archives during startup; install on first use.
(require 'my-lazy-package)
(my-lazy-package-mode 1)

(use-package bind-key)
(use-package diminish)

(use-package package-utils
  :commands (package-utils-upgrade-all package-utils-list-upgrades))

(use-package restart-emacs
  :commands restart-emacs)

(provide 'init-package)
;;; init-package.el ends here
