;;; early-init.el --- Early startup tweaks -*- lexical-binding: t -*-

(setq package-enable-at-startup t
      package-quickstart nil
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-compacting-font-caches t
      read-process-output-max (* 1024 1024)
      ;; Let JIT fontification wait briefly while the user is typing.
      ;; This keeps redisplay/input responsive in large source buffers.
      jit-lock-defer-time 0.2
      jit-lock-stealth-time nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Avoid UI chrome flash before init (not buffer display settings).
(setq default-frame-alist
      (append '((menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars . nil)
                (horizontal-scroll-bars . nil))
              default-frame-alist))

;; emacs-mac: frames created with tool-bar-lines=0 after the first one are
;; often AX-ineligible, so yabai leaves them unmanaged. Flash the tool bar
;; on make-frame (not the initial frame) long enough for yabai to see it.
(defun my/hide-tool-bar (&optional frame)
  (let ((frame (or frame (selected-frame))))
    (when (and (frame-live-p frame) (display-graphic-p frame))
      (set-frame-parameter frame 'tool-bar-lines 0))))

(defun my/yabai-realize-frame (frame)
  (when (and (frame-live-p frame) (display-graphic-p frame))
    (set-frame-parameter frame 'tool-bar-lines 1)
    (run-at-time 0.2 nil #'my/hide-tool-bar frame)))

(add-hook 'after-make-frame-functions #'my/yabai-realize-frame)

(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore before command-line files are visited (after-init runs first).
;; Merge so handlers registered during init (Tramp, etc.) are kept.
(defun my/restore-file-name-handler-alist ()
  (setq file-name-handler-alist
        (delete-dups (append file-name-handler-alist
                             my/file-name-handler-alist))))
(add-hook 'after-init-hook #'my/restore-file-name-handler-alist)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)
            (message "Emacs loaded in %s." (emacs-init-time))))

(with-eval-after-load 'comp
  (setq native-comp-async-jobs-number 8
        native-comp-speed 3
        native-comp-async-report-warnings-errors 'silent))

(provide 'early-init)
