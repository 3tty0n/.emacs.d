;;; init.el --- Entry point -*- lexical-binding: t -*-

(load (expand-file-name "my-init.el" user-emacs-directory) nil 'nomessage)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-jump-mode acp ag ahg all-the-icons auctex-cont-latexmk auctex-latexmk
                   calfw-cal ccls chatgpt claude-code claude-code-ide
                   company-bibtex company-box company-c-headers company-flx
                   company-quickhelp company-reftex consult csv-mode dashboard
                   ddskk diminish doom-modeline doom-themes dune eat ein
                   esh-autosuggest eshell-git-prompt eshell-z ess-R-data-view
                   ess-view evil excorporate exec-path-from-shell
                   fill-column-indicator flycheck-eglot flycheck-gradle
                   flycheck-irony flycheck-mypy flycheck-ocaml flycheck-pos-tip
                   flymake-diagnostic-at-point flymake-python-pyflakes
                   flymake-shellcheck git-gutter gnuplot gradle-mode grip-mode
                   groovy-mode haskell-mode highlight-indent-guides hl-todo
                   htmlize imenu-anywhere js2-mode jupyter languagetool lsp-java
                   lsp-jedi lsp-julia lsp-pyright lsp-ui lua-mode marginalia
                   merlin-eldoc monet monky mu4e-alert obsidian ocamlformat
                   open-junk-file orderless org-appear org-bullets org-pomodoro
                   package-utils pdf-tools php-mode python-black racket-mode
                   rainbow-delimiters sbt-mode scala-mode shell-maker shell-pop
                   smalltalk-mode smart-jump smartparens sml-mode treemacs-magit
                   treemacs-persp treemacs-projectile treemacs-tab-bar
                   typescript-mode undo-tree utop vertico visual-fill
                   visual-fill-column vscode-icon vterm yaml-imenu
                   yasnippet-snippets))
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el")))
 '(safe-local-variable-values '((TeX-master . \./main.tex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((t (:background "black" :foreground "green"))))
 '(magit-diff-added-highlight ((t (:background "white" :foreground "green"))))
 '(magit-diff-removed ((t (:background "black" :foreground "blue"))))
 '(magit-diff-removed-hightlight ((t (:background "white" :foreground "blue"))))
 '(magit-hash ((t (:foreground "red")))))
