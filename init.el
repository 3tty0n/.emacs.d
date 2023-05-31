(let ((file-name-handler-alist))
  (load-file (concat user-emacs-directory "my-init.el")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "30b14930bec4ada72f48417158155bc38dd35451e0f75b900febd355cda75c3e" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" default))
 '(ignored-local-variable-values '((eval setq-default fill-column 95)))
 '(package-selected-packages
   '(cuda-mode ox-slack smalltalk-mode python-black highlight-indent-guides visual-fill virtual-auto-fill browse-kill-ring marginalia jedi company-fuzzy org-appear org-superstar languagetool todoist darcula-theme clues-theme smooth-scrolling merlin-company helm-ls-git monky helm-ag multi-vterm lsp-pyright lsp-haskell lsp-java lsp-latex lsp-metals yasnippet-snippets yaml-imenu which-key wanderlust w3m vterm vscode-icon utop use-package undo-tree twittering-mode spacemacs-theme smartparens smart-jump shell-pop scala-mode sbt-mode restart-emacs rainbow-delimiters proof-general php-mode pdf-tools org-pomodoro org-bullets open-junk-file ocp-indent neotree multi-term mu4e-alert merlin-eldoc magit lua-mode lsp-treemacs lsp-python-ms irony-eldoc imenu-anywhere htmlize hl-todo helm-swoop helm-projectile helm-lsp haskell-mode groovy-mode gradle-mode gnuplot git-gutter flycheck-ocaml flycheck-irony flycheck-gradle exec-path-from-shell evil esup eshell-z eshell-git-prompt esh-autosuggest emojify elscreen edit-server dune doom-themes doom-modeline diminish ddskk dashboard company-tabnine company-reftex company-quickhelp company-irony-c-headers company-irony company-c-headers company-box company-bibtex company-auctex color-theme-modern auctex-latexmk all-the-icons-dired ahg ag ace-jump-mode))
 '(safe-local-variable-values
   '((TeX-auto-save . t)
     (TeX-parse-self . t)
     (eval setq-default fill-column 95)
     (TeX-command-extra-options . "-halt-on-error -shell-escape --synctex=1")
     (TeX-master . \.\./main\.tex)
     (main . \.\./main\.tex)
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "/usr/local/lib/ocaml")
            (expand-file-name "/usr/lib/ocaml")))
     (TeX-master . main))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((t (:background "black" :foreground "green"))))
 '(magit-diff-added-highlight ((t (:background "white" :foreground "green"))))
 '(magit-diff-removed ((t (:background "black" :foreground "blue"))))
 '(magit-diff-removed-hightlight ((t (:background "white" :foreground "blue"))))
 '(magit-hash ((t (:foreground "red"))))
 '(org-pomodoro-mode-line ((t (:foreground "#ff5555"))) t)
 '(org-pomodoro-mode-line-break ((t (:foreground "#50fa7b"))) t))
