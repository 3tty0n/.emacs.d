;;; init-prog.el --- LSP, checkers, VCS, shells -*- lexical-binding: t; -*-

(require 'init-core)

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

(use-package eshell-git-prompt
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-z
  :after eshell
  :bind ("C-x C-z" . eshell-z))
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
  (shell-pop-internal-mode "eat")
  (shell-pop-shell-type (quote ("eat" "*eat*" (lambda nil (eshell shell-pop-term-shell)))))
  (shell-pop-term-shell "/usr/bin/zsh")
  (shell-pop-window-size 30)
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom"))
;; lsp
(use-package eglot
  :defer t
  :commands (eglot eglot-ensure)
  :hook ((R-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         ;; (python-mode . eglot-ensure)
         ;; (LaTeX-mode . eglot-ensure)
         )
  :config
  (setq eglot-autoshutdown t
        eglot-report-progress nil
        eglot-send-changes-idle-time 1.0
        eglot-events-buffer-config '(:size 0 :format short)
        eglot-code-action-indications nil
        eglot-ignored-server-capabilities
        '(:documentHighlightProvider
          :inlayHintProvider
          :codeLensProvider
          :colorProvider
          :foldingRangeProvider
          :semanticTokensProvider))

  ;; JSONRPC logging and GUI-side inline UI updates are expensive in large
  ;; workspaces. Keep Eglot usable but quiet by default.
  (remove-hook 'jsonrpc-event-hook #'jsonrpc--log-event)

  (defun my-eglot-lightweight-settings ()
    "Reduce per-keystroke UI work in Eglot buffers."
    (setq-local eldoc-idle-delay 1.0
                eldoc-echo-area-use-multiline-p nil
                company-idle-delay (if (my-large-file-p) nil 0.4)
                company-backends '(company-capf))
    (when (and (my-large-file-p) (fboundp 'flymake-mode))
      (flymake-mode -1))
    (when (fboundp 'flymake-diagnostic-at-point-mode)
      (flymake-diagnostic-at-point-mode -1))
    (when (fboundp 'flycheck-mode)
      (flycheck-mode -1))
    (when (fboundp 'eglot-inlay-hints-mode)
      (eglot-inlay-hints-mode -1)))

  (add-hook 'eglot-managed-mode-hook #'my-eglot-lightweight-settings)

  (add-to-list 'eglot-server-programs
               '(tex-mode "texlab"))
  (add-to-list 'eglot-server-programs
               '(c-mode "ccls"))

  (defun eglot-ccls-inheritance-hierarchy (&optional derived)
    "Show inheritance hierarchy for the thing at point.
If DERIVED is non-nil (interactively, with prefix argument), show
the children of class at point."
    (interactive "P")
    (if-let* ((res (jsonrpc-request
                    (eglot--current-server-or-lose)
                    :$ccls/inheritance
                    (append (eglot--TextDocumentPositionParams)
                            `(:derived ,(if derived t :json-false))
                            '(:levels 100) '(:hierarchy t))))
              (tree (list (cons 0 res))))
        (with-help-window "*ccls inheritance*"
          (with-current-buffer standard-output
            (while tree
              (pcase-let ((`(,depth . ,node) (pop tree)))
                (cl-destructuring-bind (&key uri range) (plist-get node :location)
                  (insert (make-string depth ?\ ) (plist-get node :name) "\n")
                  (make-text-button (+ (line-beginning-position 0) depth) (line-end-position 0)
                                    'action (lambda (_arg)
                                               (interactive)
                                               (find-file (eglot--uri-to-path uri))
                                               (goto-char (car (eglot--range-region range)))))
                  (cl-loop for child across (plist-get node :children)
                           do (push (cons (1+ depth) child) tree)))))))
      (eglot--error "Hierarchy unavailable")))
  )
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
        lsp-log-io nil
        lsp-keep-workspace-alive nil
        lsp-idle-delay 0.8
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-enable-folding nil
        lsp-enable-imenu nil
        lsp-enable-file-watchers nil
        lsp-eldoc-enable-hover nil
        lsp-semantic-tokens-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; (LaTeX-mode      . lsp-deferred)
         (rust-mode       . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (tuareg-mode     . lsp-deferred)
         (js-mode         . lsp-deferred)
         (racket-mode     . lsp-deferred)
         (julia-mode      . lsp-deferred)
         ;; (c-mode . lsp-deferred)
         (python-mode     . lsp-deferred)
         (java-mode       . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  ;; Use flymake like eglot; avoids flycheck "no checker" noise.
  (setq lsp-diagnostics-provider :flymake)
  ;; Keep modeline updates quiet as well; diagnostics remain available through
  ;; Flycheck when it is enabled.
  (with-eval-after-load 'lsp-modeline
    (setq lsp-modeline-code-actions-enable nil
          lsp-modeline-diagnostics-enable nil
          lsp-modeline-workspace-status-enable nil))

  (defun my-lsp-lightweight-settings ()
    "Reduce point/change-triggered UI work in LSP buffers."
    (setq-local eldoc-idle-delay 1.0
                company-idle-delay (if (my-large-file-p) nil 0.4)
                company-backends '(company-capf)))

  (add-hook 'lsp-mode-hook #'my-lsp-lightweight-settings)

  (use-package lsp-pyright
    :ensure t
    :custom (lsp-pyright-langserver-command "pyright")) ;; or basedpyright
  (add-to-list 'lsp-disabled-clients 'semgrep-ls)
  (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("/usr/bin/jdtls"))
      :activation-fn (lsp-activate-on "java")
      :server-id 'jdtls-system))
  (use-package lsp-jedi
    :ensure t)
  (use-package ccls
    :ensure t)
  (use-package lsp-julia
    :ensure t
    :config
    (setq lsp-julia-default-environment "~/.julia/environments/v1.11")
    ))

(use-package lsp-ui :ensure t :commands lsp-ui-mode :after lsp
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-update-mode 'point))
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list :after lsp)

;; optionally if you want to use debugger
(use-package dap-mode :ensure t :after lsp)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(defun my-enable-flymake ()
  "Enable Flymake unless the current buffer is remote or large."
  (my-enable-unless-large-file #'flymake-mode))

(use-package flymake
  :hook (prog-mode . my-enable-flymake)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error))
  :defer t
  :init
  ;; Eglot already waits before sending document changes.  Avoid starting a
  ;; second syntax-check timer almost immediately after every edit.
  (setq flymake-no-changes-timeout 1.0)
  :config
  (use-package flymake-diagnostic-at-point
    :ensure t
    :after flymake
    :hook (flymake-mode . flymake-diagnostic-at-point-mode)))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook (sh-mode . flymake-shellcheck-load))

;; Prefer flymake (eglot/lsp). Keep flycheck available for M-x / language addons,
;; but do not enable it globally — that spams "no checker" on every buffer.
(use-package flycheck
  :ensure t
  :defer t
  :commands (flycheck-mode flycheck-list-errors)
  :config
  (setq flycheck-check-syntax-automatically '(save)
        flycheck-idle-change-delay 2.0)
  (use-package flycheck-pos-tip
    :ensure t
    :if (display-graphic-p)
    :hook (flycheck-mode . flycheck-pos-tip-mode)))
(use-package flycheck-ocaml :defer t)
(use-package flycheck-mypy :defer t)

;; rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (defun my-enable-rainbow-delimiters ()
    (unless (derived-mode-p 'emacs-lisp-mode)
      (my-enable-unless-large-file #'rainbow-delimiters-mode))))
;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-auto-revert-mode nil
        vc-handled-backends '(Git)))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

;; for mercurial
(use-package monky
  :ensure t
  :defer t
  :commands monky-status
  :config
  (setq monky-process-type 'cmdserver))

;; for mercurial
(use-package ahg
  :ensure t
  :defer t)

(provide 'init-prog)
;;; init-prog.el ends here
