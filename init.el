(let ((file-name-handler-alist))
  (load-file (concat user-emacs-directory "my-init.el")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("30b14930bec4ada72f48417158155bc38dd35451e0f75b900febd355cda75c3e" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" default))
 '(package-selected-packages
   '(todoist darcula-theme clues-theme smooth-scrolling merlin-company helm-ls-git monky helm-ag multi-vterm lsp-pyright lsp-haskell lsp-java lsp-latex lsp-metals yasnippet-snippets yaml-imenu which-key wanderlust w3m vterm vscode-icon utop use-package undo-tree twittering-mode spacemacs-theme smartparens smart-jump shell-pop scala-mode sbt-mode restart-emacs rainbow-delimiters proof-general php-mode pdf-tools org-pomodoro org-bullets open-junk-file ocp-indent neotree multi-term mu4e-alert merlin-eldoc magit lua-mode lsp-treemacs lsp-python-ms irony-eldoc imenu-anywhere htmlize hl-todo helm-swoop helm-projectile helm-lsp helm-ghq haskell-mode groovy-mode gradle-mode gnuplot git-gutter flycheck-pos-tip flycheck-ocaml flycheck-irony flycheck-gradle exec-path-from-shell evil esup eshell-z eshell-git-prompt esh-autosuggest emojify elscreen edit-server dune doom-themes doom-modeline diminish ddskk dashboard company-tabnine company-reftex company-quickhelp company-irony-c-headers company-irony company-c-headers company-box company-bibtex company-auctex color-theme-modern auctex-latexmk all-the-icons-dired ahg ag ace-jump-mode))
 '(safe-local-variable-values '((TeX-master . main))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-pomodoro-mode-line ((t (:foreground "#ff5555"))))
 '(org-pomodoro-mode-line-break ((t (:foreground "#50fa7b")))))
