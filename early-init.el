;;; early-init.el --- Early startup tweaks -*- lexical-binding: t -*-

;; Straight owns packages; never let package.el race it at startup.
(setq package-enable-at-startup nil
      package-quickstart nil
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-compacting-font-caches t
      read-process-output-max (* 1024 1024)
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Avoid UI chrome flash before init (not buffer display settings).
(setq default-frame-alist
      (append '((menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars . nil)
                (horizontal-scroll-bars . nil))
              default-frame-alist))

(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/file-name-handler-alist)
            (message "Emacs loaded in %s." (emacs-init-time))))

(with-eval-after-load 'comp
  (setq native-comp-async-jobs-number 8
        native-comp-speed 3
        native-comp-async-report-warnings-errors 'silent))

(provide 'early-init)
