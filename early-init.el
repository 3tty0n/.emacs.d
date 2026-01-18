;;; early-init.el -*- lexical-binding: t -*-

(setq gc-cons-threshold (* 100 1024 1024))
(setq gc-cons-percentage 0.6)

(defvar my/saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message (user-login-name))
(setq initial-scratch-message nil)

(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1 1024 1024))
            (setq gc-cons-percentage 0.1)

            (setq file-name-handler-alist my/saved-file-name-handler-alist)

            (message "Emacs loaded in %s." (emacs-init-time))))

(provide 'early-init)
