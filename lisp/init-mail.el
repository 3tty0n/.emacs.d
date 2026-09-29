;;; init-mail.el --- Mail and calendar (site-specific, loaded after startup) -*- lexical-binding: t; -*-

;; These are large and only needed once the session is up, so they are loaded
;; from an idle timer rather than blocking startup.

(use-package my-mu4e
  :load-path "~/.mu4e.d"
  :defer t)

(use-package my-calendar
  :load-path "~/.my-calendar.d"
  :defer t)

(use-package excorporate-oauth2
  :load-path "site-lisp/excorporate-oauth2"
  :defer t)

(use-package calfw-excorporate
  :load-path "site-lisp/calfw-excorporate"
  :after excorporate-oauth2
  :defer t)

(defun my-load-mail-and-calendar ()
  "Load the mail and calendar configuration."
  (dolist (feature '(my-mu4e my-calendar excorporate-oauth2 calfw-excorporate))
    (with-demoted-errors "Mail/calendar setup: %S"
      (require feature))))

(add-hook 'emacs-startup-hook
          (lambda () (run-with-idle-timer 1 nil #'my-load-mail-and-calendar)))

(provide 'init-mail)
;;; init-mail.el ends here
