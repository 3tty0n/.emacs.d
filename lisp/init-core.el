;;; init-core.el --- Core editing behaviour -*- lexical-binding: t; -*-

(setq warning-minimum-level :emergency
      initial-scratch-message ""
      compilation-scroll-output t
      ring-bell-function #'ignore
      use-short-answers t
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      ;; Prefer side-by-side splits (preview/PDF/webkit on the right, not below).
      split-width-threshold 120
      split-height-threshold nil)

;; Open preview-like buffers to the right of the source.
(setq display-buffer-alist
      (append
       display-buffer-alist
       '(((or (major-mode . pdf-view-mode)
              (major-mode . xwidget-webkit-mode)
              (major-mode . eww-mode))
          (display-buffer-reuse-mode-window
           display-buffer-in-side-window)
          (mode pdf-view-mode xwidget-webkit-mode eww-mode)
          (side . right)
          (slot . 1)
          (window-width . 0.5))
         ("\\*\\(?:[Xx]widget\\|eww\\|Html.*\\).*\\*"
          (display-buffer-in-side-window)
          (side . right)
          (slot . 1)
          (window-width . 0.5)))))


(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq-default backup-directory-alist
              `(("." . ,(expand-file-name ".backup" user-emacs-directory)))
              indent-tabs-mode nil
              tab-width 4)

(defcustom my-large-file-threshold (* 1024 1024)
  "Files larger than this skip expensive display/checker minor modes."
  :type 'integer
  :group 'my)

(defun my-large-file-p ()
  "Return non-nil when the current buffer is too large for expensive modes."
  (or (and buffer-file-name
           (file-remote-p buffer-file-name))
      (> (buffer-size) my-large-file-threshold)))

(defun my-enable-unless-large-file (mode)
  "Enable MODE unless the current buffer is remote or large."
  (unless (my-large-file-p)
    (funcall mode 1)))

(defun my-configure-line-numbers ()
  "Keep line numbers in normal buffers, but omit them in large buffers."
  (display-line-numbers-mode (if (my-large-file-p) -1 1)))

(defun my-large-buffer-performance-setup ()
  "Disable input-sensitive display features in large or remote buffers."
  (when (my-large-file-p)
    (display-line-numbers-mode -1)
    (when (fboundp 'show-paren-local-mode)
      (show-paren-local-mode -1))
    ;; Company remains available for explicit use, but does not start an
    ;; expensive completion search after every pause in a large buffer.
    (setq-local company-idle-delay nil
                company-backends '(company-capf))
    (setq-local jit-lock-defer-time 0.0)))

(add-hook 'prog-mode-hook #'my-large-buffer-performance-setup)
(add-hook 'conf-mode-hook #'my-large-buffer-performance-setup)
(add-hook 'find-file-hook #'my-large-buffer-performance-setup)

;; revert buffer
(setq auto-revert-verbose nil
      auto-revert-remote-files nil
      auto-revert-avoid-polling t
      auto-revert-interval 5)
(global-auto-revert-mode 1)

;; recompile
(keymap-global-set "M-c" #'recompile)

;; Rectangle editing only.  CUA's register-0 overlay inherits `highlight'
;; and otherwise stays on the pasted replacement until the cursor exits left.
(setq cua-enable-cua-keys nil
      cua-delete-copy-to-register-0 nil)
(cua-mode t)

;; ssh connection
(use-package tramp
  :ensure nil
  :defer t
  :init (setq tramp-default-method "ssh"))

;; encoding / clipboard / misc
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode -1)))

;; saveplace / history
(setq save-place-file (expand-file-name ".cache/places" user-emacs-directory)
      recentf-max-saved-items 200
      ;; Cleaning at mode start stats every entry (slow with Tramp files);
      ;; do it once Emacs has been idle instead.
      recentf-auto-cleanup 60)
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)

(show-paren-mode 1)
(defun my-lisp-disable-paren-scan ()
  "Do not scan matching parens; it walks the whole sexp on every move."
  (setq-local show-paren-data-function #'ignore)
  (if (fboundp 'show-paren-local-mode)
      (show-paren-local-mode -1)
    (setq-local show-paren-mode nil)))
(dolist (hook '(lisp-data-mode-hook lisp-mode-hook emacs-lisp-mode-hook))
  (add-hook hook #'my-lisp-disable-paren-scan))
(when (fboundp 'repeat-mode)
  (repeat-mode 1))

(setq redisplay-skip-fontification-on-input t
      fast-but-imprecise-scrolling t
      bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Auto-pair: no sexp-balance scan (that is what made typing `(` crawl in my-init.el)
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil
      blink-matching-paren nil
      electric-pair-inhibit-predicate
      (lambda (c)
        (or (eq c ?`)
            (electric-pair-conservative-inhibit c))))
(with-eval-after-load 'smartparens
  (smartparens-global-mode -1)
  (show-smartparens-global-mode -1))

(setq scroll-conservatively 10
      scroll-margin 10)

;; set C-h to backspace
(keymap-global-set "C-h" #'backward-char)
(keymap-global-set "C-c t" #'toggle-truncate-lines)

;; copy & paste
(when (eq system-type 'gnu/linux)
  (setq select-enable-clipboard t
        select-enable-primary t))
;; dired
(setq dired-dwim-target t)

;; undo tree
(use-package undo-tree
  :ensure t
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil))

(provide 'init-core)
;;; init-core.el ends here
