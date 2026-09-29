;;; init-org.el --- Org, markup, notes, AI tools -*- lexical-binding: t; -*-

(require 'init-core)

;; html
(use-package htmlize
  :commands (htmlize-buffer htmlize-file htmlize-region))

;; markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
         ("C-c g" . grip-mode)
         :map markdown-mode-command-map
         ("g" . grip-mode))
  :config
  (setq markdown-command
        "pandoc --from=markdown --to=html --standalone --mathjax --highlight-style=pygments"))

;; Live Markdown/Org preview (`C-c g` or Markdown `C-c C-c g`)
(use-package grip-mode
  :ensure t
  :commands grip-mode
  :custom
  (grip-command 'auto)
  (grip-real-time-refresh t)
  ;; Refresh on save only (avoids GitHub API rate limits while typing).
  ;; Set to t for true as-you-type updates if you authenticate grip.
  (grip-update-after-change nil)
  :config
  ;; Prefer in-Emacs webkit when available (opens on the right via display-buffer-alist).
  (when (featurep 'xwidget-internal)
    (setq grip-preview-in-webkit t)))

;; csv
(use-package csv-mode
  :mode "\\.csv\\'")

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :config
  (use-package yaml-imenu))

(use-package org
  :ensure t
  :defer t
  :bind (("C-c C-q" . org-capture)
         ("C-c C-l" . org-store-link)
         :map org-mode-map
         ("C-c g" . grip-mode))
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
          ("m" "Memo" entry (file "~/Dropbox/org/memo.org")
           "* %?\n   %a\n    %T")
          ))

  (setq org-latex-pdf-process '("lualatex --shell-escape --draftmode %f"
                                "lualatex --shell-escape %f"))
  (setq org-latex-default-class "ltjsarticle")

  (use-package org-appear
    :ensure t
    :config
    (add-hook 'org-mode-hook 'org-appear-mode))

  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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

(use-package open-junk-file
  :commands open-junk-file)

;; AI agent
(use-package eat
  :ensure t
  :commands (eat eat-other-window)
  :config
  (setq eat-term-scrollback-size 400000)
  (add-hook 'eat-mode-hook (lambda () (display-line-numbers-mode -1))))
(use-package obsidian
  :ensure t
  :defer t
  :commands (obsidian-capture
             obsidian-follow-link-at-point
             obsidian-jump
             obsidian-insert-link
             obsidian-backlink-jump)
  :config
  (global-obsidian-mode t)
  (obsidian-backlinks-mode t)
  :custom
  ;; location of obsidian vault
  (obsidian-directory "~/Obsidian")
  ;; Default location for new notes from `obsidian-capture'
  (obsidian-inbox-directory "Inbox")
  ;; Useful if you're going to be using wiki links
  (markdown-enable-wiki-links t)

  ;; These bindings are only suggestions; it's okay to use other bindings
  :bind (:map obsidian-mode-map
              ;; Create note
              ("C-c C-n" . obsidian-capture)
              ;; If you prefer you can use `obsidian-insert-wikilink'
              ("C-c C-l" . obsidian-insert-link)
              ;; Open file pointed to by link at point
              ("C-c C-o" . obsidian-follow-link-at-point)
              ;; Open a different note from vault
              ("C-c C-p" . obsidian-jump)
              ;; Follow a backlink for the current file
              ("C-c C-b" . obsidian-backlink-jump)))


(use-package claude-code-ide
  :load-path "site-lisp/claude-code-ide.el"
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(use-package agent-shell
  :ensure t
  :defer t
  :commands agent-shell
  :config
  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :login t))

  (defun my-agent-shell-consult-reveal-fragment ()
    "Expand a collapsed agent-shell fragment so a consult match is visible."
    (when (derived-mode-p 'agent-shell-mode)
      (when-let* ((state (get-text-property (point) 'agent-shell-ui-state))
                  ((map-elt state :collapsed)))
        (agent-shell-ui--toggle-fragment-at-point))))
  (add-hook 'consult-after-jump-hook #'my-agent-shell-consult-reveal-fragment))



;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(provide 'init-org)
;;; init-org.el ends here
