;;; init-ui.el --- Appearance and windows -*- lexical-binding: t; -*-

(require 'init-core)

(use-package whitespace
  :hook ((prog-mode text-mode conf-mode) . my-enable-whitespace-mode)
  :config
  (defun my-enable-whitespace-mode ()
    (my-enable-unless-large-file #'whitespace-mode))
  (setq whitespace-style '(trailing tabs newline tab-mark newline-mark))
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          ;; WARNING: the mapping below has a problem.
          ;; When a TAB occupies exactly one column, it will display the
          ;; character ?\xBB at that column followed by a TAB which goes to
          ;; the next TAB column.
          ;; If this is a problem for you, please, comment the line below.
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]))))
;; auto-fill
(keymap-global-set "C-c q" #'auto-fill-mode)
(setq-default fill-column 80)
(add-hook 'prog-mode-hook
          (lambda ()
            (my-enable-unless-large-file
             #'display-fill-column-indicator-mode)))

(use-package visual-fill-column
  :ensure t
  :hook
  (visual-line-mode . visual-fill-column-mode))

(use-package visual-fill
  :ensure t
  :defer t)

;; Line numbers: everywhere, except large/remote buffers (see init-core).
(global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook #'my-configure-line-numbers)
(add-hook 'conf-mode-hook #'my-configure-line-numbers)
(set-face-attribute 'line-number nil
                    :foreground "DarkOliveGreen"
                    :background "#131521")
(set-face-attribute 'line-number-current-line nil :foreground "gold")

;; highlight indentation line
(use-package highlight-indent-guides
  :ensure t
  :hook
  (prog-mode . my-enable-highlight-indent-guides-mode)
  :config
  (defun my-enable-highlight-indent-guides-mode ()
    (unless (derived-mode-p 'lisp-data-mode 'lisp-mode)
      (my-enable-unless-large-file #'highlight-indent-guides-mode)))
  (setq highlight-indent-guides-method 'bitmap))

(require 'my-font)
(require 'my-util)

;; Avoid redisplay stalls in minified files with extremely long lines.  Keep
;; the original major mode and editing enabled so this remains a mitigation,
;; not a read-only fallback.
(use-package so-long
  :ensure nil
  :demand t
  :config
  (setq so-long-action 'so-long-minor-mode
        so-long-variable-overrides
        (assq-delete-all 'buffer-read-only so-long-variable-overrides))
  (dolist (mode '(company-mode highlight-indent-guides-mode))
    (add-to-list 'so-long-minor-modes mode))
  (global-so-long-mode 1))


;; Startup screen (icons off = faster paint).  Skipped when files are on argv.
(use-package dashboard
  :ensure t
  :unless command-line-args-left
  :custom
  (dashboard-items '((recents . 10)
                     (projects . 10)
                     (bookmarks . 10)))
  (dashboard-set-heading-icons nil)
  (dashboard-set-file-icons nil)
  (dashboard-set-navigator t)
  (dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook))

;; color theme
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-city-lights t)
  (with-eval-after-load 'org
    (doom-themes-org-config)))

;; mode-line
;; XXX: hit M-x nerd-icons-install-fonts
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package nerd-icons
  :if (display-graphic-p)
  :defer t)
(keymap-global-unset "C-z")

(keymap-global-set "C-z C-z" #'my-suspend-frame)

(defun my-suspend-frame ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))

;; Show tabs corresponding to a window
(use-package tab-bar
  :bind (("C-z C-c" . tab-bar-new-tab)
         ("C-z C-k" . tab-close)
         ("C-z C-n" . tab-next)
         ("C-<tab>" . tab-next)
         ("C-z C-p" . tab-previous))
  :hook (after-init . tab-bar-mode)
  :config
  (setq tab-bar-new-tab-choice "*scratch*"))
(use-package which-key
  :defer 0.5
  :config
  (which-key-setup-minibuffer)
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.05)
  (which-key-mode 1))

(provide 'init-ui)
;;; init-ui.el ends here
