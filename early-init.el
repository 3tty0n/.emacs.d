;;; early-init.el --- Early startup tweaks -*- lexical-binding: t -*-

;; Defer garbage collection while starting; restored in `emacs-startup-hook'.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      read-process-output-max (* 1024 1024))

;; Prefer newer sources over stale .elc files (the config is byte-compiled by
;; `make'), but never inside batch compilation.
(setq load-prefer-newer t)

;; Precomputed autoloads for installed packages; refresh with
;; `make quickstart' (package.el also refreshes it on install/delete).
(setq package-enable-at-startup t
      package-quickstart t)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-compacting-font-caches t
      initial-major-mode 'fundamental-mode
      ;; Skip X resource lookups; nothing here relies on them.
      inhibit-x-resources t)

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

;; File-name handlers (Tramp, jka-compr, ...) slow every `load' during init.
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
            (message "Emacs loaded in %s with %d GCs."
                     (emacs-init-time) gcs-done)))

(with-eval-after-load 'comp
  (setq native-comp-async-jobs-number 8
        native-comp-speed 3
        native-comp-async-report-warnings-errors 'silent))

(provide 'early-init)
;;; early-init.el ends here
