;;; init-completion.el --- Completion, search, navigation -*- lexical-binding: t; -*-

(require 'init-core)

;; completion
(use-package orderless
  :ensure t
  :defer t
  ;; The style registers itself through the package's autoloads, so the
  ;; package is only loaded when first used.
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package company
  :ensure t
  :defer 0.5
  :bind (("C-M-i" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-filter-candidates)
         ("TAB" . company-complete-common-or-cycle)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-require-match 'never
        company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-backends
        '(company-capf
          company-files
          (company-dabbrev-code company-keywords)
          company-dabbrev))
  (push 'company-preview-common-frontend company-frontends)
  (global-company-mode 1))

(defun my-company-large-buffer-setup ()
  "Make Company manual-only in large or remote buffers."
  (when (my-large-file-p)
    (setq-local company-idle-delay nil
                company-backends '(company-capf))))

(add-hook 'company-mode-hook #'my-company-large-buffer-setup)
;; find definitions
(use-package smart-jump
  :ensure t
  :defer 2
  :config
  (smart-jump-setup-default-registers))
(use-package ag
  :ensure t
  :commands (ag ag-project ag-regexp))

(defun my-filename-upto-parent ()
  "Move to parent directory like \"cd ..\" in find-file."
  (interactive)
  (let ((sep (eval-when-compile (regexp-opt '("/" "\\")))))
    (save-excursion
      (left-char 1)
      (when (looking-at-p sep)
        (delete-char 1)))
    (save-match-data
      (when (search-backward-regexp sep nil t)
        (right-char 1)
        (filter-buffer-substring (point) (line-end-position)
                                 #'delete)))))

(use-package vertico
  :ensure t
  ;; :defer t
  :bind (("C-l" . my-filename-upto-parent))
  :hook (after-init . vertico-mode)
  :custom
  ;; Different scroll margin
  (vertico-scroll-margin 0)
  ;; Show more candidates
  (vertico-count 30)
  ;; Grow and shrink the Vertico minibuffer
  (vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-cycle t)
  )

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package emacs
  :ensure nil
  :init
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt)
        read-extended-command-predicate #'command-completion-default-include-p
        enable-recursive-minibuffers t)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ;; ("C-s"   . consult-line)
         ("C-s" . consult-line-at-point)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  (defun consult-line-at-point ()
    "Search the current buffer, starting from the symbol or word at point."
    (interactive)
    (consult-line (or (thing-at-point 'symbol t)
                      (thing-at-point 'word t))))

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )
(use-package imenu-anywhere
  :bind ("C-c ." . imenu-anywhere))

(use-package projectile
  :ensure t
  :defer 0.8
  :bind-keymap (("C-c p" . projectile-command-map)
                ("C-;" . projectile-command-map)
                ("s-;" . projectile-command-map))
  :config
  (setq projectile-switch-project-action #'projectile-dired
        projectile-enable-caching t)
  (projectile-mode 1))

;; Jump (avy is the maintained successor to ace-jump)
(use-package avy
  ;; M-g f is `consult-flymake'; avy-goto-line is available via M-x.
  :bind ("C-." . avy-goto-char-timer))
;; yasnippet — per-buffer, not global at startup
(use-package yasnippet
  :ensure t
  :hook ((prog-mode text-mode conf-mode) . yas-minor-mode)
  :config
  (yas-reload-all)
  (use-package yasnippet-snippets))

(provide 'init-completion)
;;; init-completion.el ends here
