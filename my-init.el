(setq gc-cons-threshold (* 100 1024 1024))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(package-initialize)

(setq byte-compile-warnings '(cl-functions))

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(if (eq system-type "gnu/linux")
    (add-to-list 'load-path "/usr/share/emacs/site-lisp"))

;; Bootstrapping use-package
(eval-when-compile
  (require 'package)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (setq use-package-enable-imenu-support t)
  (require 'use-package))

(use-package bind-key :ensure t)
(use-package diminish :ensure t)

(use-package mouse
  :if (eq system-name 'gnu/linux)
  :init
  (xterm-mouse-mode t)
  (mouse-wheel-mode t))

;; path
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

(when window-system
  (require 'server)
  (unless (eq (server-running-p) 't)
    (server-start)))

(use-package restart-emacs :ensure t)

(use-package esup
  :disabled
  :ensure t
  ;; To use MELPA Stable use ":pin mepla-stable",
  :pin melpa
  :commands (esup))

;; initial window
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((recents   . 10)
                          (projects  . 10)
                          (bookmarks . 10)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (dashboard-setup-startup-hook))

;; hide warnings
(setq warning-minimum-level :emergency)

;; internal
(setq initial-scratch-message "")
(setq compilation-scroll-output t)

;; ignore beep sounds
(setq ring-bell-function 'ignore)

;; delete while spaces when you save a file every time
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; no backup
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default backup-directory-alist '(("" . "~/.emacs.d/.backup")))

;; tab -> space
(setq-default indent-tabs-mode nil)

;; tabsize
(setq-default tab-width 4)

(use-package whitespace
  :init
  (global-whitespace-mode 1)
  :config
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

;; disable whitespace-mode in dired-mode
(add-hook 'dired-mode-hook (lambda () (global-whitespace-mode -1)))

;; revert buffer
(global-auto-revert-mode 1)

;; auto-fill
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(setq-default fill-column 80)

(global-visual-line-mode)

;; add key-bining for recompilation
(global-set-key (kbd "M-c") 'recompile)

;; editting rectangle region
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; Eshell
(use-package eshell
  :defer t
  :init
  (setq ;; eshell-buffer-shorthand t ...  Can't see Bug#19391
        eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t)
  (add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package esh-autosuggest
  :defer t
  :ensure t)

(use-package eshell-prompt-extras
  :ensure t
  :after (eshell)
  :defer t
  :disabled
  :config
  (with-eval-after-load 'esh-opt
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

(use-package eshell-git-prompt
  :ensure t
  :after (eshell)
  :defer t
  :init
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-z
  :after (eshell)
  :ensure t
  :bind ("C-x C-z" . eshell-z))

;; shell
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode -1)))

(use-package vterm
  :ensure t
  :init
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
  :bind
  ("C-x v t" . vterm-other-window)
  :config
  (use-package multi-vterm
    :ensure t
    :bind
    ((",n" . multi-vterm-next)
     (",p" . multi-vterm-prev)
     (",c" . multi-vterm)  )))

;; term

;; shell-pop
(use-package shell-pop
  :ensure t
  :bind
  ("C-t". shell-pop)
  :custom
  (shell-pop-internal-mode "eshell")
  (shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell)))))
  (shell-pop-term-shell "/usr/bin/zsh")
  (shell-pop-window-size 30)
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom"))

(use-package multi-term
  :ensure t
  :defer t
  :init
  (add-hook 'term-mode-hook
            (lambda () (display-line-numbers-mode -1)))
  :config
  (setq multi-term-program "/usr/bin/zsh")
  (defun last-term-buffer (l)
    "Return most recently used term buffer."
    (when l
      (if (eq 'term-mode (with-current-buffer (car l) major-mode))
          (car l) (last-term-buffer (cdr l)))))

  (defun get-term ()
    "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
    (interactive)
    (let ((b (last-term-buffer (buffer-list))))
      (if (or (not b) (eq 'term-mode major-mode))
          (multi-term)
        (switch-to-buffer b)))))

;; saveplace
(save-place-mode 1)
(setq save-place-file (locate-user-emacs-file "places" "~/.emacs.d/.cache/places"))


(defalias 'yes-or-no-p 'y-or-n-p) ; yes-no → y-n

(show-paren-mode t) ; 対応するカッコを強調表示

;; smartparens
(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode)
  :config
  ;; (sp-pair "'" "'" :actions nil)
  (sp-pair "`" "`" :actions nil))

;; scrolling
(use-package smooth-scrolling
  :ensure t
  :init (smooth-scrolling-mode))

(setq scroll-conservatively 10)
(setq scroll-margin 10)

(scroll-bar-mode -1) ; スクロールバーを非表示

(if (version<= "26.0.50" emacs-version)
    (progn
      (global-display-line-numbers-mode)
      ;; (setq left-margin-width 2)
      (defun display-line-numbers-color-on-after-init (frame)
        "Hook function executed after FRAME is generated."
        (unless (display-graphic-p frame)
          (set-face-background
           'line-number
           (plist-get base16-solarized-dark-colors :base01))))
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (display-line-numbers-color-on-after-init frame)))
      ))

(setq inhibit-startup-message t) ; 起動メッセージを非表示
(tool-bar-mode -1) ; ツールバーを非表示
(menu-bar-mode -1) ; メニューバーを非表示

;; set C-h to backspace
(global-set-key (kbd "C-h") 'backward-char)

;; font config
(use-package my-font :load-path "site-lisp")

;; utilities
(use-package my-util :load-path "site-lisp")

;; toggle truncate lines
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)

;; copy & paste
(if (eq system-type 'gnu/linux)
    (setq x-select-enable-clipboard t
          x-select-enable-primary t))

;; undo tree
(use-package undo-tree
  :ensure t
  :defer t
  :init (global-undo-tree-mode))

;; dired
(with-eval-after-load 'dired
  (setq dired-dwim-target t))

(use-package dired-sidebar
  :disabled
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (progn
                (unless (file-remote-p default-directory)
                  (auto-revert-mode))
                (display-line-numbers-mode -1))
              ))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'ascii)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         30)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package neotree :ensure t)
;;; Input method

(setq current-input-method nil)
(setq default-input-method nil)

;; ddskk
(use-package skk
  :ensure ddskk
  :defer t
  :init
  (global-set-key (kbd "C-x C-j") 'skk-mode)
  (global-set-key (kbd "C-x j") 'skk-auto-fill-mode)
  :config
  (use-package viper :init (setq viper-mode -1))

  ;; (setq skk-kutouten-type 'en)

  (setq skk-user-directory "~/.ddskk")
  (setq default-input-method "japanese-skk")
  (setq skk-preload t)
  (setq skk-byte-compile-init-file t)

  (setq skk-show-candidates-always-pop-to-buffer t) ; 変換候補の表示位置

  (setq skk-dcomp-activate t)                       ; 動的補完
  (setq skk-dcomp-multiple-activate t)              ; 動的補完の複数候補表示
  (setq skk-dcomp-multiple-rows 5)                  ; 動的補完の候補表示件数

  (setq skk-egg-like-newline t)
  (setq skk-comp-circulate t)

  (setq skk-egg-like-newline t)                     ; Enterで改行しない
  (setq skk-delete-implies-kakutei nil)             ; ▼モードで一つ前の候補を表示
  (setq skk-show-annotation nil)                    ; Annotation
  (setq skk-use-look t)                             ; 英語補完
  (setq skk-auto-insert-paren nil)
  (setq skk-henkan-strict-okuri-precedence t)

  ;; 動的補完の複数表示群のフェイス
  ;; (set-face-foreground 'skk-dcomp-multiple-face "Black")
  ;; (set-face-background 'skk-dcomp-multiple-face "LightGoldenrodYellow")
  ;; (set-face-bold-p 'skk-dcomp-multiple-face nil)
  ;; 動的補完の複数表示郡の補完部分のフェイス
  ;; (set-face-foreground 'skk-dcomp-multiple-trailing-face "dim gray")
  ;; (set-face-bold-p 'skk-dcomp-multiple-trailing-face nil)
  ;; 動的補完の複数表示郡の選択対象のフェイス
  ;; (set-face-foreground 'skk-dcomp-multiple-selected-face "White")
  ;; (set-face-background 'skk-dcomp-multiple-selected-face "LightGoldenrod4")
  ;; (set-face-bold-p 'skk-dcomp-multiple-selected-face nil)
  ;; 動的補完時に下で次の補完へ
  (define-key skk-j-mode-map (kbd "<down>") 'skk-completion-wrapper))

;; evil-mode
(use-package evil
  :ensure t
  :config
  (setq evil-disable-insert-state-bindings t))

;; color theme
(use-package spacemacs-common
  ;; :disabled
  :ensure spacemacs-theme
  :if (not (display-graphic-p))
  :config
  (load-theme 'spacemacs-dark t))

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :if (display-graphic-p)
  :config
  (load-theme 'doom-city-lights t)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package srcery-theme
  :disabled
  :ensure t
  :if (not (display-graphic-p))
  :config
  (load-theme 'srcery t))

(use-package jetbrains-darcula-theme
  :disabled
  :ensure t
  :if (not (display-graphic-p))
  :config
  (load-theme 'jetbrains-darcula t))

(use-package color-theme-modern
  :ensure t)

;; mode-line
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))


(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
        (after-init . display-time))

;; icon
(use-package all-the-icons
  :ensure t
  :config
  (use-package all-the-icons-dired
    :ensure t
    :init
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

(use-package emojify
  :ensure t
  :init (global-emojify-mode t))

;; ide settings

;; completion

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (use-package company-bibtex :ensure t)
  (use-package company-c-headers :ensure t)
  (use-package company-irony :ensure t)
  (use-package company-quickhelp :ensure t)
  (use-package company-reftex :ensure t)
  (use-package company-irony-c-headers :ensure t)
  (define-key global-map (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (push 'company-preview-common-frontend company-frontends)

  (setq company-require-match 'never)
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-align-annotations t))

(use-package company-statistics
  :ensure t
  :after (company)
  :config
  (setq company-statistics-file my-company-history-file
      company-transformers
      '(company-sort-by-statistics company-sort-by-backend-importance)))

(use-package company-dwim
  :load-path "site-lisp/company-dwim"
  :after (company))

(use-package company-flx
  :ensure t
  :disabled
  :init (company-flx-mode +1)
  :after (company))

(use-package company-quickhelp
  :ensure t
  :init (company-quickhelp-mode)
  :after (company))

(use-package company-box
  :ensure t
  :if (display-graphic-p)
  :after (company)
  :hook (company-mode . company-box-mode))

(setq lsp-keymap-prefix "C-l")

;; lsp
(use-package lsp-mode
  :ensure t
  :hook ((c-mode        . lsp)
         (c++-mode      . lsp)
         (tuareg-mode   . lsp)
         (scala-mode    . lsp)
         (java-mode     . lsp)
         (LaTeX-mode    . lsp)
         (python-mode   . lsp)
         (ruby-mode     . lsp)
         (scala-mode    . lsp)
         (haskell-mode  . lsp)
         (lsp-mode      . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :init (lsp-headerline-breadcrumb-mode)
  :config
  ;; for better performance
  (setq read-process-output-max (* 4 1024 1024)) ;; 4mb

  (setq lsp-headerline-breadcrumb-enable nil)
  ;; disable flymake
  (setq lsp-prefer-flymake nil)

  ;; completion
  ;; (setq lsp-completion-no-cache t)
  ;; (setq lsp-completion-provider :none)

  (setq lsp-response-timeout 5)
  (setq lsp-idle-delay 1)
  (setq lsp-keep-workspace-alive nil)

  (setq lsp-lens-enable nil)

  ;; metals (for scala)
  (use-package lsp-metals :ensure t :after lsp)

  ;; lsp-latex
  (use-package lsp-latex
    :ensure t
    :config
    (setq lsp-latex-forward-search-executable "evince-synctex")
    (setq lsp-latex-forward-search-args '("-f" "%l" "%p" "\"emacsclient +%l %f\""))
    (setq lsp-tex-server 'digestif)
    :after lsp)

  (use-package lsp-java :ensure t :after lsp
    :config (add-hook 'java-mode-hook 'lsp))

  (use-package lsp-haskell :ensure t :after lsp
    :config
    (add-hook 'haskell-mode-hook #'lsp)
    (add-hook 'haskell-literate-mode-hook #'lsp))

  (use-package lsp-pyright
    :disabled
    :hook (python-mode . (lambda ()
                           (require 'lsp-pyright)
                           (lsp))))

  (use-package lsp-python-ms
    :ensure t
    :config
    ;; (setq lsp-python-ms-auto-install-server t)
    (setq lsp-python-ms-executable
          (executable-find
           "~/src/github.com/microsoft/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"))
    ))

;; optionally
(use-package lsp-ui
  :ensure t
  :after lsp
  :init (lsp-ui-mode)
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  (lsp-ui-imenu-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
  (lsp-ui-doc-max-width 180)
  (lsp-ui-doc-max-height 60)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-delay 0.5)
  ;; (lsp-ui-doc-use-webkit t)
  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable t)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-update-mode t)
  ;; (lsp-ui-sideline-delay 0.5)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always

  (lsp-headerline-breadcrumb-enable nil)
  :bind
  ("C-c m" . lsp-ui-imenu)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (add-hook 'after-make-frame-functions-hook 'tabbar-local-mode)
  (add-hook 'after-make-frame-functions-hook (lambda (frame) (set-frame-parameter frame 'tab-bar-lines 0))))

(use-package lsp-treemacs
  :after lsp
  :ensure t
  :commands lsp-treemacs-errors-list
  :config
  ;; for metals
  ;; (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t))

(use-package eglot
  :disabled
  :ensure t
  :init
  (add-hook 'LaTeX-mode-hook 'eglot-ensure))

;; find definitions
(use-package smart-jump
  :ensure t
  :config
  (smart-jump-setup-default-registers))

;; syntax check
(use-package flymake
  :config
  (use-package flymake-diagnostic-at-point
    :ensure t
    :after flymake
    :config
    (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))
  (use-package flymake-python-pyflakes :ensure t))

(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
  :config
  (use-package flycheck-irony :ensure t)
  (use-package flycheck-ocaml :ensure t)
  (use-package flycheck-pos-tip :ensure t)
  (use-package flymake-shellcheck
    :commands flymake-shellcheck-load
    :init
    (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

  ;; (setq flymake-mode -1)
  (if (display-graphic-p)
      (flycheck-pos-tip-mode)))

;; rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  ;; color of parens
  (use-package cl-lib)
  (use-package color)
  (defun rainbow-delimiters-using-stronger-colors ()
    (interactive)
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30))))
  (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors))

(use-package ag :ensure t)

(use-package migemo
  :load-path "site-lisp/migemo"
  :init (migemo-init)
  :config
  ;; cmigemo(default)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))

  ;; Set your installed path
  (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix))

;; helm
(use-package helm
  ;;:disabled
  :ensure t
  :init (helm-mode)
  :bind
  ("C-x C-f" . helm-find-files)
  ("C-x C-m" . helm-recentf)
  ("C-x b"   . helm-mini)
  ("C-s"     . helm-swoop)
  ("C-c s"   . helm-occur)
  ("M-i"     . helm-imenu)
  ("M-x"     . helm-M-x)
  ("M-y"     . helm-show-kill-ring)
  ("C-c y"   . helm-show-kill-ring)
  :config
  (use-package helm-swoop
    :ensure t
    :bind
    ;; (:map helm-swoop-map
    ;;       ("C-r"     . helm-previous-line)
    ;;       ("C-s"     . helm-next-line))
    :config
    ;; If you prefer fuzzy matching
    ;; (setq helm-swoop-use-fuzzy-match t)

    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows t)

    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically))

  ;; enable migemo
  (helm-migemo-mode t)

  (use-package helm-ghq
    :ensure t
    :if (executable-find "ghq")
    :bind ("C-c RET" . helm-ghq))

  (use-package helm-projectile
    :ensure t
    :after (projectile)
    :config
    (helm-projectile-on)
    (setq helm-projectile-fuzzy-match t))

  (use-package helm-lsp
    :ensure t
    :after (lsp)
    :commands helm-lsp-workspace-symbol)

  ;; Please install the_silver_searcher
  (use-package helm-ag
    :ensure t
    :bind
    ("C-x C-g" . helm-ag))

  (use-package helm-ls-git
    :ensure t)

  (use-package helm-ls-hg
    :ensure t)
  ;; Fuzzy matching
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-autoresize-mode t)

  (setq helm-always-two-windows nil)
  (setq helm-display-buffer-default-height 30)
  (setq helm-default-display-buffer-functions '(display-buffer-in-side-window))

  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action))

;; ido
(use-package ido
  :disabled
  :init
  (ido-mode 1)
  (ido-everywhere 1)
  :config
  (setq ido-enable-flex-matching t)

  (use-package swoop
    :ensure t
    :bind
    ("C-s" . swoop)
    ("C-M-s" . swoop-multi)
    ("M-s" . swoop-pcre-regexp)
    ("C-S-s" . swoop-back-to-last-position))

  (use-package ido-completing-read+
    :ensure t
    :after (ido)
    :init (ido-ubiquitous-mode 1))

  (use-package ido-vertical-mode
    :ensure t
    :init (ido-vertical-mode)
    :after (ido)
    :config
    (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

  (use-package smex
    :ensure t
    :after (ido)
    :bind
    ("M-x" . smex)
    ("M-X" . xmex-major-mode-commands)))

;; ivy
(use-package ivy
  :disabled
  :ensure t
  :init (ivy-mode)
  :bind
  ("C-s"     . swiper)
  ("C-c C-s" . swiper-thing-at-point)
  ("<f6>"    . ivy-resume)
  ("M-x"     . counsel-M-x)
  ("M-i"     . counsel-imenu)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f"  . counsel-describe-function)
  ("<f1> v"  . counsel-describe-variable)
  ("<f2> i"  . counsel-info-lookup-symbol)
  ("<f2> u"  . counsel-unicode-char)
  ("C-c g"   . counsel-git)
  ("C-c j"   . counsel-git-grep)
  ("C-c k"   . counsel-ag)
  ("C-x l"   . counsel-locate)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 35)
  (setq ivy-truncate-lines t)
  (setq ivy-wrap t)

  (use-package all-the-icons-ivy
    :ensure t
    :init
    (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

  (use-package all-the-icons-ivy-rich
    :ensure t
    :after (ivy-rich)
    :init (all-the-icons-ivy-rich-mode 1))

  (use-package ivy-rich
    :ensure t
    :init
    (add-hook 'after-init-hook (lambda () (ivy-rich-mode t)))
    :config
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

  (use-package counsel-projectile
    :ensure t
    :after (projectile)
    :init (counsel-projectile-mode)))

(use-package imenu-anywhere
  :ensure t
  :bind
  ("C-." . imenu-anywhere))

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              ("C-;" . projectile-command-map)
              ("s-;" . projectile-command-map))
  :config
  ;; (setq projectile-switch-project-action #'projectile-dired)
  :init (projectile-mode))

;; ace jump
(use-package ace-jump-mode
  :ensure t
  :defer t
  :init
  (autoload 'ace-jump-mode "ace-jump-mode" nil t)
  (bind-key "C-." 'ace-jump-mode))

;; gist
(use-package gist
  :ensure t
  :disabled t)

;; yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :init (yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :ensure t))

(global-unset-key (kbd "C-z"))

(global-set-key (kbd "C-z C-z") 'my-suspend-frame)

(defun my-suspend-frame ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))

;; Show tabs corresponding to a window
(use-package tab-bar
  :if (and (version<= "27.1" emacs-version)
           (eq system-type 'gnu/linux))
  :bind (("C-z C-c" . tab-bar-new-tab)
         ("C-z C-k" . tab-close)
         ("C-z C-n" . tab-next)
         ("C-z C-p" . tab-previous))
  :config
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq xterm-mouse-mode t)
  :init
  (tab-bar-mode))

(use-package elscreen
  :ensure t
  :if (eq system-type 'darwin)
  :init (elscreen-start)
  :config
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  :bind (("C-<tab>" . elscreen-next)))

;; Magit
(use-package magit
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-x g") 'magit)
  :config
  (setq-default magit-auto-revert-mode nil)
  (setq vc-handled-backends '())
  (eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(magit-diff-added ((t (:background "black" :foreground "green"))))
   '(magit-diff-added-highlight ((t (:background "white" :foreground "green"))))
   '(magit-diff-removed ((t (:background "black" :foreground "blue"))))
   '(magit-diff-removed-hightlight ((t (:background "white" :foreground "blue"))))
   '(magit-hash ((t (:foreground "red"))))))

;; for mercurial
(use-package monky
  :ensure t
  :defer t
  :commands monky-status
  :config
  (setq monky-process-type 'cmdserver))

;; for mercurial
(use-package ahg
  :ensure t)

;; Git gutter
(use-package git-gutter
  :ensure t
  :defer t
  :custom
  (git-gutter:ask-p nil)
  (git-gutter:handled-backends '(git hg svn))
  :init
  (global-git-gutter-mode)
  :config
  (defun git-gutter:toggle-popup-hunk ()
    "Toggle git-gutter hunk window."
    (interactive)
    (if (window-live-p (git-gutter:popup-buffer-window))
        (delete-window (git-gutter:popup-buffer-window))
      (git-gutter:popup-hunk))))

(use-package which-key
  :ensure t
  :config
  (which-key-setup-minibuffer)
  (setq which-key-idle-secondary-delay 0))
(add-hook 'after-init-hook #'which-key-mode)

;;; Web

;; Browser
(use-package w3m
  :ensure t
  :defer t
  :config
  (setq w3m-default-display-inline-images t))

;; E-mail
(use-package wl
  :ensure wanderlust
  :defer t
  :commands (wl wl-draft)
  :custom
  (wl-summary-width 95)
  :init
  (display-line-numbers-mode -1)
  :config
  (global-set-key (kbd "<mouse-1>")
                  (lambda (event) (interactive (list last-command-event))
                    (posn-set-point (event-end event))
                    (browse-url (thing-at-point 'url t)))))

(use-package my-mu4e :load-path "~/.mu4e.d")

(use-package twittering-mode
  :ensure t
  :bind (:map twittering-mode-map
              ("F" . twittering-favorite)
              ("C-c C-h" . twittering-home-timeline))
  :hook
  (twittering-mode-hook . (lambda () (display-line-numbers-mode -1)))
  :config
  (defalias 'epa--decode-coding-string 'decode-coding-string)
  (setq twittering-use-master-password t)
  (setq twittering-cert-file "/etc/ssl/certs/ca-certificates.crt")
  (setq twittering-icon-mode t)
  (setq twittering-convert-fix-size 36)
  (setq twittering-use-icon-storage t)
  (setq twittering-status-format
        "%RT{%FACE[bold]{RT}} %i %p %S (%s), %@:
%FOLD[  ]{%T // from %f%L%r%R%QT{
+----
%FOLD[|]{%i %p %S (%s), %@:
%FOLD[  ]{%T // from %f%L%r%R}}
+----}}
 ")
  )

(use-package circe
  :ensure t
  :config
  (setq circe-network-options
        '(("Libera Chat"
           :nick "yuiza__"
           :sasl-username "yuiza__"
           :channels ("#pypy")
           )))
  (require 'lui-logging)
  (enable-lui-logging-globally)
  (setq lui-logging-directory "~/.emacs.d/.circle/logs"))

;;;;; Infra

;;;;; Language

(use-package agda2-mode
  :load-path "/usr/share/agda/emacs-mode/"
  :disabled
  :init
  (add-hook 'agda2-mode-hook '(lambda () (deactivate-input-method)))
  :config
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

;; c
(use-package cc-mode
  :defer t
  :mode ("\\.g4\\'" . antlr-mode)
  :hook
  (asm-mode-hook  . (lambda () (setq-default indent-tabs-mode t)))
  (java-mode-hook . (lambda () (setq c-basic-offset 4
                                     tab-width 4
                                     indent-tabs-mode nil)))
  (c-mode-hook    . (lambda () (setq c-basic-offset 2
                                     tab-always-indent 0
                                     c-auto-newline t
                                     indent-tabs-mode nil)))
  (sh-mode-hook   . (lambda () (setq sh-basic-offset 4
                                     indent-tabs-mode nil))))

;; ocaml
(use-package tuareg
  :ensure t
  :init
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'tuareg-mode-hook #'merlin-eldoc-setup)
  (add-hook 'tuareg-mode-hook #'utop-minor-mode)
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("dune" . dune-mode))
  :config
  (setq tuareg-match-patterns-aligned t)
  (setq tuareg-highlight-all-operators t))

(use-package ocp-indent
  :ensure t
  :after (tuareg)
  :config
  (add-to-list 'tuareg-mode-hook 'ocp-setup-indent))

(use-package dune
  :ensure t)

(use-package merlin
  :ensure t
  :after (tuareg)
  :config
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)
  ;; Enable Flycheck checker
  (flycheck-ocaml-setup)

  (use-package merlin-company
    :ensure t
    :config
    (add-to-list 'company-backends #'merlin-company-backend)))

(use-package merlin-eldoc
  :ensure t
  :after (merlin))

(use-package utop
  :ensure t
  :config
  (setq utop-command "opam config exec -- utop -emacs"))

(use-package ocamlformat
  :ensure t
  :after (tuareg)
  :custom (ocamlformat-enable 'enable-outside-detected-project)
  ;; :hook (before-save . ocamlformat-before-save)
  :config
  (define-key tuareg-mode-map (kbd "C-M-<tab>") #'ocamlformat))

;; Proof General
(use-package proof-general :ensure t)

;; LaTeX
(use-package pdf-tools
  :ensure t
  :if (eq system-type 'gnu/linux)
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :bind ("C-c C-g" . pdf-sync-forward-search)
  :defer t
  :init
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'pdf-view-mode-hook '(lambda () (display-line-numbers-mode -1)))
  (add-hook 'pdf-tools-enabled-hook '(lambda () (auto-revert-mode 1)))
  :config
  (setq mouse-wheel-follow-mouse t)
  (setq pdf-view-resize-factor 1.10))

(use-package languagetool
  :ensure t
  :config
  (global-set-key (kbd "C-c l c") 'languagetool-check)
  (global-set-key (kbd "C-c l d") 'languagetool-clear-buffer)
  (global-set-key (kbd "C-c l p") 'languagetool-correct-at-point)
  (global-set-key (kbd "C-c l b") 'languagetool-correct-buffer)
  (global-set-key (kbd "C-c l l") 'languagetool-set-language)

  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command "~/.languagetool/languagetool-commandline.jar"
        languagetool-server-command "~/.languagetool/languagetool-server.jar"))

(use-package tex
  :ensure auctex
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'display-fill-column-indicator-mode)
  (flycheck-mode -1)

  (require 'tex-site)
  (setq TeX-parse-self t)
  (setq TeX-auto-save t)
  (setq TeX-clean-confirm t)
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-mode t)

  ;; Outline minor mode
  ;; extra outline headers
  (setq TeX-outline-extra
        '(("%chapter" 1)
          ("%section" 2)
          ("%subsection" 3)
          ("%subsubsection" 4)
          ("%paragraph" 5)))

  ;; add font locking to the headers
  (font-lock-add-keywords
   'latex-mode
   '(("^%\\(chapter\\|\\(sub\\|subsub\\)?section\\|paragraph\\)"
      0 'font-lock-keyword-face t)
     ("^%chapter{\\(.*\\)}"       1 'font-latex-sectioning-1-face t)
     ("^%section{\\(.*\\)}"       1 'font-latex-sectioning-2-face t)
     ("^%subsection{\\(.*\\)}"    1 'font-latex-sectioning-3-face t)
     ("^%subsubsection{\\(.*\\)}" 1 'font-latex-sectioning-4-face t)
     ("^%paragraph{\\(.*\\)}"     1 'font-latex-sectioning-5-face t)))

  ;; reftex
  (setq reftex-plug-into-AUCTeX t)

  ;; inverse search
  (setq TeX-source-correlate-mode t)
  (setq-default TeX-source-correlate-start-server t)

  (setq-default TeX-master t)
  (setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape -synctex=1 %S%(PDFout)")))

  ;; Use pdf-tools to open PDF files
  ;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
  ;;       TeX-source-correlate-start-server t)

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (use-package auctex-latexmk
    :ensure t
    :config
    (auctex-latexmk-setup)
    (setq shell-escape-mode t))

  (use-package company-auctex
    :disabled
    :ensure t
    :init
    (company-auctex-init))

  (use-package reftex
    :ensure t
    :config
    (setq reftex-section-levels
          (append '(("frametitle" . -3) ) reftex-section-levels))
    (setq reftex-cite-prompt-optional-args t)
    (setq reftex-plug-into-AUCTeX t)))


;; c/c++
(use-package irony
  :ensure t
  :disabled
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  :config
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-irony-c-headers))

(use-package irony-eldoc
  :ensure t
  :after (irony)
  :init
  (add-hook 'irony-mode-hook #'irony-eldoc))

;; lua
(use-package lua-mode
  :ensure t
  :defer t)

;; scala
;; Enable scala-mode and sbt-mode
(use-package scala-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :ensure t
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Java build tool
(use-package gradle-mode :ensure t
  :config
  (use-package groovy-mode :ensure t))

;; Java CUP
(use-package cup-java-mode
  :load-path "site-lisp/cup-java"
  :mode "\\.cup$")

(use-package nxml
  :mode
  (("\.xml$" . nxml-mode)
   ("\.xls$" . nxml-mode))
  :config
  (setq nxml-child-indent 2)
  (setq nxml-attribute-indent 2)
  (setq nxml-slash-auto-complete-flag t))

(use-package flycheck-gradle :ensure t)

;; haskell
(use-package haskell-mode
  :ensure t
  :defer t)

;; gnuplot
(use-package gnuplot
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.plot" . gnuplot-mode)))

;; PHP
(use-package php-mode :ensure t)

;; Python
(use-package python
  :config
  (defun python-pytest ()
    (interactive)
    (if (and (string-match
              (rx bos "test_")
              (file-name-nondirectory (buffer-file-name)))
             (s-suffix? ".py" (file-name-nondirectory (buffer-file-name))))
        (let (input)
          (while (not input)
            (setq input (read-string
                         "-k:")))
          (let (cmd)
            (if (string= "" input)
                (setq cmd "py.tesst --color=no -rP ")
              (setq cmd (concat (concat "py.test --color=no -rP -k " input) " ")))
            (compile (concat cmd (buffer-file-name)))))
      (py-execute-buffer))))

(use-package pypytrace-mode
  :defer t
  :mode "\\.log\\.txt$")

;; html
(use-package htmlize :ensure t)

;; markdown
(use-package markdown-mode :ensure t)

;; YAML
(use-package yaml-mode
  :ensure t
  :config
  (use-package yaml-imenu
    :ensure t))

(use-package org
  :ensure t
  :defer t
  :bind (("C-c C-q" . org-capture)
         ("C-c C-l" . org-store-link))
  :custom
  (org-use-speed-commands t)
  (org-startup-folded t)
  :config
  (setq org-todo-keywords
        '((sequence "TODO" "DOING" "|" "DONE")))
  (setq org-agenda-files '("~/Dropbox/org/research.org"
                           "~/Dropbox/org/todo.org"
                           "~/Dropbox/org/notes.org"))

  (setq org-capture-templates
        '(("t" "Todo" entry (file"~/Dropbox/org/todo.org")
           "* TODO %?\n %i\n")
          ("T" "ToDo with link" entry (file "~/Dropbox/org/todo.org")
           "* TODO %i%? \n:PROPERTIES: \n:CREATED: %U \n:END: \n %a\n")
          ("n" "Note" entry (file "~/Dropbox/org/notes/inbox.org")
           "* %?\n   %a\n    %T")
          ))

  (use-package ox-latex
    :config
    (add-to-list 'org-latex-classes
                 '("ltjsarticle"
                   "\\documentclass[11pt,a4paper]{ltjsarticle}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
% \\usepackage{newtxtext,newtxmath}
\\usepackage{textcomp}
\\usepackage{graphicx}
\\usepackage{booktabs}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{hyperref}
\\hypersetup{pdfencoding=auto}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
                 '("beamer"
                   "\\documentclass\[presentation\]\{beamer\}"
                   ("\\section\{%s\}" . "\\section*\{%s\}")
                   ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                   ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}"))))

  (setq org-latex-pdf-process '("lualatex --shell-escape --draftmode %f"
                                "lualatex --shell-escape %f"))
  (setq org-latex-default-class "ltjsarticle")

  (use-package org-appear
    :ensure t
    :config
    (add-hook 'org-mode-hook 'org-appear-mode))

  (use-package org-bullets
    :ensure t
    :disabled
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  (use-package org-superstar
    :ensure t
    ;; :disabled
    :init
    (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
    :config
    (setq org-superstar-special-todo-items t))

  (use-package org-pomodoro
    :ensure t
    :defer t
    :after (org)
    :custom
    (org-pomodoro-ask-upon-killing t)
    (org-pomodoro-format "%s")
    (org-pomodoro-short-break-format "%s")
    (org-pomodoro-long-break-format  "%s")
    :custom-face
    (org-pomodoro-mode-line ((t (:foreground "#ff5555"))))
    (org-pomodoro-mode-line-break   ((t (:foreground "#50fa7b"))))
    :hook
    (org-pomodoro-started . (lambda () (notifications-notify
                                        :title "org-pomodoro"
                                        :body "Let's focus for 25 minutes!")))

    (org-pomodoro-finished . (lambda () (notifications-notify
                                         :title "org-pomodoro"
                                         :body "Well done! Take a break.")))
    :config
    :bind (:map org-agenda-mode-map
                ("p" . org-pomodoro))))

(use-package open-junk-file :ensure t)

;; Hightlight TODO
(use-package hl-todo
  :ensure t
  :defer ti
  :hook ((prog-mode . hl-todo-mode)
         (LaTeX-mode . hl-todo-mode))
  :bind
  (:map hl-todo-mode-map
        ("C-c t p" . hl-todo-previous)
        ("C-c t n" . hl-todo-next)
        ("C-c t o" . hl-todo-occur)
        ("C-c t i" . hl-todo-insert))
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF"))))

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
