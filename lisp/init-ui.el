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

(use-package treemacs
  :ensure t
  :init
  (advice-add 'treemacs :override #'my/treemacs-project)
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil)
    (add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1))))
  :preface
  (defun my/treemacs-project ()
    "Show only the current project (git root, else `default-directory') in treemacs."
    (interactive)
    (require 'treemacs)                 ; make the let-bound var dynamic
    (let ((treemacs--find-user-project-functions
           (list (lambda () (or (vc-git-root default-directory) default-directory)))))
      (treemacs-add-and-display-current-project-exclusively)))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . my/treemacs-project)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

(use-package treemacs-projectile
  :disabled
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :disabled
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t
  :config
  (with-eval-after-load 'dired
    (treemacs-icons-dired-mode))
  )

(use-package treemacs-magit
  :disabled
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :disabled
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :disabled
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; color theme
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-city-lights t)
  (with-eval-after-load 'treemacs
    (setq doom-themes-treemacs-theme "doom-colors")
    (doom-themes-treemacs-config))
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
