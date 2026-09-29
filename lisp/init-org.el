;;; init-org.el --- Org, markup, notes, AI tools -*- lexical-binding: t; -*-

(require 'init-core)

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

(provide 'init-org)
;;; init-org.el ends here
