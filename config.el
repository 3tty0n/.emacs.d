;;; config.el --- Main configuration -*- lexical-binding: t -*-

;; Byte-compiled by `make'; init.el loads it.

;;;; ==== my-font ====
;;; Commentary:
;;;
;;; Code:

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(with-system gnu/linux
  (add-hook 'window-setup-hook
            (lambda ()
              ;; (set-frame-font "JetBrains Mono Medium 12" nil t)
              (set-face-attribute 'default nil
                                  :family "JetBrains Mono"
                                  :height 110)
              ;; (add-to-list 'default-frame-alist '(font . "Fira Code-10"))
              (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Noto Sans CJK JP")))))

(with-system darwin
  (add-hook 'window-setup-hook
            (lambda ()
              (when (display-graphic-p)
                (set-face-attribute 'default nil
                                  :family "JetBrains Mono"
                                  :height 140)
                ;; (add-to-list 'default-frame-alist '(font . "Fira Code-10"))
                (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Noto Sans CJK JP"))
                ))))


;;;; ==== my-util ====
;;; Commentary:
;;;
;;; Code:

;; window size
;; enlarge
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-=") 'enlarge-window)
;; shrink
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-|") 'shrink-window)

;; hide *compile* buffer
(setq compilation-finish-function
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "1 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*"))
              (message "No Compilation Errors!")))))

(defun remove-newlines-in-region ()
  "Remove all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "" nil t))))

(global-set-key [f8] 'remove-newlines-in-region)

;; http://qiita.com/marcy@github/items/ba0d018a03381a964f24
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "Alpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(95))))

;; window size
(defun set-frame-size-according-to-resolution ()
  "Adjusting the preferred width and resolutions."
  (interactive)
  (if window-system
      (progn
        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 120))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 500)
                                      (frame-char-height)))))))

(add-hook 'window-setup-hook
          (lambda ()
            (when (display-graphic-p)
              (set-alpha 97)
              (set-frame-size-according-to-resolution))))

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))
(define-key global-map "\C-a" 'back-to-indentation-or-beginning)

(defun e-run-command ()
  "Runf external system programs. Dmenu/Rofi-like. Tab/C-M-i to completion n-[b/p] for
walk backward/forward early commands history."
  (interactive)
  (require 'subr-x)
  (start-process "RUN" "RUN" (string-trim-right (read-shell-command "RUN: "))))

(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))

(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

(global-set-key "\C-x@" '(lambda ()
                           (interactive)
                           (split-window-vertically-n 3)))
(global-set-key "\C-x#" '(lambda ()
                           (interactive)
                           (split-window-horizontally-n 3)))

(defun compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)

  ;; This command symbol has a property “'stateIsCompact-p”.
  (let (currentStateIsCompact (bigFillColumnVal 4333999) (deactivate-mark nil))

    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil) ) )

      (if (region-active-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))) )
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil)) ) )

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)) ) ) )

(defun my-copy-simple (beg end)
  "Copy the region to the kill ring, joining wrapped lines.

Paragraph breaks are kept.  Only newlines inside a paragraph are
replaced by spaces."
  (interactive "r")
  (require 'subr-x)
  (let* ((raw (buffer-substring-no-properties beg end))
         (paragraphs (split-string raw "\r?\n\\(?:[ \t]*\r?\n\\)+" t))
         (unfilled (mapcar (lambda (paragraph)
                             (string-trim
                              (replace-regexp-in-string
                               "[ \t]*\r?\n[ \t]*" " " paragraph)))
                           paragraphs))
         (text (string-trim (mapconcat #'identity unfilled "\n\n"))))
    (kill-new text)
    (setq deactivate-mark t)
    (message "Copied %d characters" (length text))))


;;;; ==== init-package ====

;; Packages are activated before init.el runs (`package-enable-at-startup' and
;; `package-quickstart' in early-init.el), so nothing here calls
;; `package-initialize'.

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)

(eval-and-compile
  ;; Must be set before use-package is loaded.  Keep the full expansion even
  ;; when byte-compiling: minimal expansion drops the error handling around
  ;; `require', so one missing package would abort the rest of the config.
  (setq use-package-expand-minimally nil
        use-package-enable-imenu-support t))
(require 'use-package)

;; Never contact package archives during startup; install on first use.
(require 'my-lazy-package)
(my-lazy-package-mode 1)

(use-package bind-key)
(use-package diminish)

(use-package package-utils
  :commands (package-utils-upgrade-all package-utils-list-upgrades))

(use-package restart-emacs
  :commands restart-emacs)


;;;; ==== init-env ====

;; Importing PATH from a login+interactive shell costs ~0.4s, so the result is
;; cached and applied instantly; a background shell refreshes the cache once
;; Emacs is idle.

(eval-when-compile (require 'cl-lib))

(defconst my-shell-env-cache
  (expand-file-name ".cache/shell-env.eld" user-emacs-directory)
  "File caching the environment imported from the user's shell.")

(defconst my-shell-env-variables '("PATH" "MANPATH")
  "Environment variables imported from the user's shell.")

(defun my-shell-env--apply (alist)
  "Set the environment variables in ALIST and refresh `exec-path'."
  (pcase-dolist (`(,name . ,value) alist)
    (setenv name value)
    (when (equal name "PATH")
      (setq exec-path (append (split-string value path-separator t)
                              (list exec-directory))))))

(defun my-shell-env--save (alist)
  "Write ALIST to `my-shell-env-cache'."
  (make-directory (file-name-directory my-shell-env-cache) t)
  (with-temp-file my-shell-env-cache
    (prin1 alist (current-buffer))))

(defun my-shell-env--read ()
  "Return the cached environment alist, or nil."
  (when (file-readable-p my-shell-env-cache)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents my-shell-env-cache)
        (read (current-buffer))))))

(defun my-shell-env--parse (output)
  "Parse the marked OUTPUT of `my-shell-env-refresh' into an alist."
  (when (string-match "<<ENV>>\\(.*\\)<<ENV>>" output)
    (let ((values (split-string (match-string 1 output) "\0")))
      (cl-loop for name in my-shell-env-variables
               for value in values
               unless (string-empty-p value) collect (cons name value)))))

(defun my-shell-env-refresh ()
  "Re-read the shell environment in the background and update the cache."
  (interactive)
  (let ((buffer (generate-new-buffer " *shell-env*"))
        (script (concat "printf '<<ENV>>"
                        (mapconcat (lambda (_) "%s") my-shell-env-variables "\\0")
                        "<<ENV>>'"
                        (mapconcat (lambda (name) (format " \"$%s\"" name))
                                   my-shell-env-variables ""))))
    (make-process
     :name "shell-env" :buffer buffer :noquery t
     :command (list (or (getenv "SHELL") "/bin/sh") "-l" "-i" "-c" script)
     :sentinel
     (lambda (process _event)
       (when (eq (process-status process) 'exit)
         (let ((alist (with-current-buffer buffer
                        (my-shell-env--parse (buffer-string)))))
           (when (and alist (not (equal alist (my-shell-env--read))))
             (my-shell-env--save alist)
             (my-shell-env--apply alist))))
       (unless (process-live-p process)
         (kill-buffer buffer))))))

(defun my-shell-env-setup ()
  "Import the shell environment using the cache when available."
  (if-let* ((alist (my-shell-env--read)))
      (progn
        (my-shell-env--apply alist)
        (run-with-idle-timer 3 nil #'my-shell-env-refresh))
    ;; First run: pay the cost once and remember the result.
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)
    (my-shell-env--save
     (mapcar (lambda (name) (cons name (getenv name))) my-shell-env-variables))))

(when (memq window-system '(mac ns x))
  (add-hook 'after-init-hook #'my-shell-env-setup))

;; Keep exec-path-from-shell available for manual use.
(use-package exec-path-from-shell
  :ensure t
  :defer t)

(when window-system
  (add-hook 'emacs-startup-hook
            (lambda ()
              (require 'server)
              (unless (server-running-p)
                (server-start)))))

(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (keymap-global-set "<mouse-4>" #'scroll-down-line)
  (keymap-global-set "<mouse-5>" #'scroll-up-line))


;;;; ==== init-core ====

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


;;;; ==== init-ui ====


(use-package whitespace
  :hook ((prog-mode text-mode conf-mode) . my-enable-whitespace-mode)
  :config
  (defun my-enable-whitespace-mode ()
    (my-enable-unless-large-file #'whitespace-mode))
  (setq whitespace-style '(trailing tabs newline tab-mark newline-mark))
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          ;; WARNING: the mapping below has a problem.
          ;; When a TAB occupies exactly one column, it will display the
          ;; character ?\xBB at that column followed by a TAB which goes to
          ;; the next TAB column.
          ;; If this is a problem for you, please, comment the line below.
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]))))
;; auto-fill
(keymap-global-set "C-c q" #'auto-fill-mode)
(setq-default fill-column 80)
(add-hook 'prog-mode-hook
          (lambda ()
            (my-enable-unless-large-file
             #'display-fill-column-indicator-mode)))

(use-package visual-fill-column
  :ensure t
  :hook
  (visual-line-mode . visual-fill-column-mode))

(use-package visual-fill
  :ensure t
  :defer t)

;; Line numbers: everywhere, except large/remote buffers (see init-core).
(global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook #'my-configure-line-numbers)
(add-hook 'conf-mode-hook #'my-configure-line-numbers)
(set-face-attribute 'line-number nil
                    :foreground "DarkOliveGreen"
                    :background "#131521")
(set-face-attribute 'line-number-current-line nil :foreground "gold")

;; highlight indentation line
(use-package highlight-indent-guides
  :ensure t
  :hook
  (prog-mode . my-enable-highlight-indent-guides-mode)
  :config
  (defun my-enable-highlight-indent-guides-mode ()
    (unless (derived-mode-p 'lisp-data-mode 'lisp-mode)
      (my-enable-unless-large-file #'highlight-indent-guides-mode)))
  (setq highlight-indent-guides-method 'bitmap))


;; Avoid redisplay stalls in minified files with extremely long lines.  Keep
;; the original major mode and editing enabled so this remains a mitigation,
;; not a read-only fallback.
(use-package so-long
  :ensure nil
  :demand t
  :config
  (setq so-long-action 'so-long-minor-mode
        so-long-variable-overrides
        (assq-delete-all 'buffer-read-only so-long-variable-overrides))
  (dolist (mode '(company-mode highlight-indent-guides-mode))
    (add-to-list 'so-long-minor-modes mode))
  (global-so-long-mode 1))


;; Startup screen (icons off = faster paint).  Skipped when files are on argv.
(use-package dashboard
  :ensure t
  :unless command-line-args-left
  :custom
  (dashboard-items '((recents . 10)
                     (projects . 10)
                     (bookmarks . 10)))
  (dashboard-set-heading-icons nil)
  (dashboard-set-file-icons nil)
  (dashboard-set-navigator t)
  (dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook))

(use-package treemacs
  :ensure t
  :init
  (advice-add 'treemacs :override #'my/treemacs-project)
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           28
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       nil
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil)
    (add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1))))
  :preface
  (defun my/treemacs-project ()
    "Show only the current project (git root, else `default-directory') in treemacs."
    (interactive)
    (require 'treemacs)                 ; make the let-bound var dynamic
    (let ((treemacs--find-user-project-functions
           (list (lambda () (or (vc-git-root default-directory) default-directory)))))
      (treemacs-add-and-display-current-project-exclusively)))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . my/treemacs-project)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

(use-package treemacs-projectile
  :disabled
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :disabled
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t
  :config
  (with-eval-after-load 'dired
    (treemacs-icons-dired-mode))
  )

(use-package treemacs-magit
  :disabled
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :disabled
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :disabled
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; color theme
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-city-lights t)
  (with-eval-after-load 'treemacs
    (setq doom-themes-treemacs-theme "doom-colors")
    (doom-themes-treemacs-config))
  (with-eval-after-load 'org
    (doom-themes-org-config)))

;; mode-line
;; XXX: hit M-x nerd-icons-install-fonts
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package nerd-icons
  :if (display-graphic-p)
  :defer t)
(keymap-global-unset "C-z")

(keymap-global-set "C-z C-z" #'my-suspend-frame)

(defun my-suspend-frame ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))

;; Show tabs corresponding to a window
(use-package tab-bar
  :bind (("C-z C-c" . tab-bar-new-tab)
         ("C-z C-k" . tab-close)
         ("C-z C-n" . tab-next)
         ("C-<tab>" . tab-next)
         ("C-z C-p" . tab-previous))
  :hook (after-init . tab-bar-mode)
  :config
  (setq tab-bar-new-tab-choice "*scratch*"))
(use-package which-key
  :defer 0.5
  :config
  (which-key-setup-minibuffer)
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.05)
  (which-key-mode 1))


;;;; ==== init-input ====


;; No input method until requested (ddskk sets its own below).
(setq current-input-method nil
      default-input-method nil)

;; ddskk
(use-package ddskk
  ;; :ensure ddskk
  :ensure t
  :defer t
  :init
  (keymap-global-set "C-x C-j" #'skk-mode)
  (keymap-global-set "C-x j" #'skk-auto-fill-mode)
  :config
  (use-package viper :init (setq viper-mode -1))

  ;; (setq skk-kutouten-type 'en)

  ;; Turn off AquaSKK

  (setq skk-user-directory "~/.ddskk")
  (setq default-input-method "japanese-skk")
  (setq skk-preload t)
  ;; (setq skk-byte-compile-init-file t)

  (setq skk-show-candidates-always-pop-to-buffer t) ; 変換候補の表示位置

  (setq skk-dcomp-activate t)                       ; 動的補完
  (setq skk-dcomp-multiple-activate t)              ; 動的補完の複数候補表示
  (setq skk-dcomp-multiple-rows 5)                  ; 動的補完の候補表示件数

  (setq skk-egg-like-newline t)
  (setq skk-comp-circulate t)

  (setq skk-egg-like-newline t)                     ; Enterで改行しない
  (setq skk-delete-implies-kakutei nil)             ; ▼モードで一つ前の候補を表示
  (setq skk-show-annotation nil)                    ; Annotation
  (setq skk-use-look t)                             ; 英語補完
  (setq skk-auto-insert-paren nil)
  (setq skk-henkan-strict-okuri-precedence t)

  ;; 動的補完の複数表示群のフェイス
  (set-face-foreground 'skk-dcomp-multiple-face "Black")
  (set-face-background 'skk-dcomp-multiple-face "LightGoldenrodYellow")
  (set-face-attribute 'skk-dcomp-multiple-face nil :weight 'normal)
  ;; 動的補完の複数表示郡の補完部分のフェイス
  (set-face-foreground 'skk-dcomp-multiple-trailing-face "dim gray")
  (set-face-attribute 'skk-dcomp-multiple-trailing-face nil :weight 'normal)
  ;; 動的補完の複数表示郡の選択対象のフェイス
  (set-face-foreground 'skk-dcomp-multiple-selected-face "White")
  (set-face-background 'skk-dcomp-multiple-selected-face "LightGoldenrod4")
  (set-face-attribute 'skk-dcomp-multiple-selected-face nil :weight 'normal)
  ;; 動的補完時に下で次の補完へ
  (keymap-set skk-j-mode-map "<down>" #'skk-completion-wrapper))

;; evil-mode (never enabled here; defer so it is not loaded unless something needs it)
(use-package evil
  :ensure t
  :defer t
  :config
  (setq evil-disable-insert-state-bindings t))


;;;; ==== init-completion ====


;; completion
(use-package orderless
  :ensure t
  :defer t
  ;; The style registers itself through the package's autoloads, so the
  ;; package is only loaded when first used.
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package company
  :ensure t
  :defer 0.5
  :bind (("C-M-i" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-filter-candidates)
         ("TAB" . company-complete-common-or-cycle)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-require-match 'never
        company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-backends
        '(company-capf
          company-files
          (company-dabbrev-code company-keywords)
          company-dabbrev))
  (push 'company-preview-common-frontend company-frontends)
  (global-company-mode 1))

(defun my-company-large-buffer-setup ()
  "Make Company manual-only in large or remote buffers."
  (when (my-large-file-p)
    (setq-local company-idle-delay nil
                company-backends '(company-capf))))

(add-hook 'company-mode-hook #'my-company-large-buffer-setup)
;; find definitions
(use-package smart-jump
  :ensure t
  :defer 2
  :config
  (smart-jump-setup-default-registers))
(use-package ag
  :ensure t
  :commands (ag ag-project ag-regexp))

(defun my-filename-upto-parent ()
  "Move to parent directory like \"cd ..\" in find-file."
  (interactive)
  (let ((sep (eval-when-compile (regexp-opt '("/" "\\")))))
    (save-excursion
      (left-char 1)
      (when (looking-at-p sep)
        (delete-char 1)))
    (save-match-data
      (when (search-backward-regexp sep nil t)
        (right-char 1)
        (filter-buffer-substring (point) (line-end-position)
                                 #'delete)))))

(use-package vertico
  :ensure t
  ;; :defer t
  :bind (("C-l" . my-filename-upto-parent))
  :hook (after-init . vertico-mode)
  :custom
  ;; Different scroll margin
  (vertico-scroll-margin 0)
  ;; Show more candidates
  (vertico-count 30)
  ;; Grow and shrink the Vertico minibuffer
  (vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-cycle t)
  )

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package emacs
  :ensure nil
  :init
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt)
        read-extended-command-predicate #'command-completion-default-include-p
        enable-recursive-minibuffers t)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ;; ("C-s"   . consult-line)
         ("C-s" . consult-line-at-point)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  (defun consult-line-at-point ()
    "Search the current buffer, starting from the symbol or word at point."
    (interactive)
    (consult-line (or (thing-at-point 'symbol t)
                      (thing-at-point 'word t))))

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )
(use-package imenu-anywhere
  :bind ("C-c ." . imenu-anywhere))

(use-package projectile
  :ensure t
  :defer 0.8
  :bind-keymap (("C-c p" . projectile-command-map)
                ("C-;" . projectile-command-map)
                ("s-;" . projectile-command-map))
  :config
  (setq projectile-switch-project-action #'projectile-dired
        projectile-enable-caching t)
  (projectile-mode 1))

;; Jump (avy is the maintained successor to ace-jump)
(use-package avy
  ;; M-g f is `consult-flymake'; avy-goto-line is available via M-x.
  :bind ("C-." . avy-goto-char-timer))
;; yasnippet — per-buffer, not global at startup
(use-package yasnippet
  :ensure t
  :hook ((prog-mode text-mode conf-mode) . yas-minor-mode)
  :config
  (yas-reload-all)
  (use-package yasnippet-snippets))


;;;; ==== init-lang ====


;; Eshell
(use-package eshell
  :defer t
  :init
  (setq ;; eshell-buffer-shorthand t ...  Can't see Bug#19391
        eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t)
  (add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package eshell-prompt-extras
  :ensure t
  :after (eshell)
  :defer t
  :disabled
  :config
  (with-eval-after-load 'esh-opt
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

(use-package eshell-git-prompt
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-z
  :after eshell
  :bind ("C-x C-z" . eshell-z))
(use-package vterm
  :ensure t
  :init
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
  :bind
  ("C-x v t" . vterm-other-window)
  :config
  (use-package multi-vterm
    :ensure t
    :bind
    ((",n" . multi-vterm-next)
     (",p" . multi-vterm-prev)
     (",c" . multi-vterm)  )))

;; term

;; shell-pop
(use-package shell-pop
  :ensure t
  :bind
  ("C-t". shell-pop)
  :custom
  (shell-pop-internal-mode "eat")
  (shell-pop-shell-type (quote ("eat" "*eat*" (lambda nil (eshell shell-pop-term-shell)))))
  (shell-pop-term-shell "/usr/bin/zsh")
  (shell-pop-window-size 30)
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom"))
;; lsp
(use-package eglot
  :defer t
  :commands (eglot eglot-ensure)
  :hook ((R-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         ;; (python-mode . eglot-ensure)
         ;; (LaTeX-mode . eglot-ensure)
         )
  :config
  (setq eglot-autoshutdown t
        eglot-report-progress nil
        eglot-send-changes-idle-time 1.0
        eglot-events-buffer-config '(:size 0 :format short)
        eglot-code-action-indications nil
        eglot-ignored-server-capabilities
        '(:documentHighlightProvider
          :inlayHintProvider
          :codeLensProvider
          :colorProvider
          :foldingRangeProvider
          :semanticTokensProvider))

  ;; JSONRPC logging and GUI-side inline UI updates are expensive in large
  ;; workspaces. Keep Eglot usable but quiet by default.
  (remove-hook 'jsonrpc-event-hook #'jsonrpc--log-event)

  (defun my-eglot-lightweight-settings ()
    "Reduce per-keystroke UI work in Eglot buffers."
    (setq-local eldoc-idle-delay 1.0
                eldoc-echo-area-use-multiline-p nil
                company-idle-delay (if (my-large-file-p) nil 0.4)
                company-backends '(company-capf))
    (when (and (my-large-file-p) (fboundp 'flymake-mode))
      (flymake-mode -1))
    (when (fboundp 'flymake-diagnostic-at-point-mode)
      (flymake-diagnostic-at-point-mode -1))
    (when (fboundp 'flycheck-mode)
      (flycheck-mode -1))
    (when (fboundp 'eglot-inlay-hints-mode)
      (eglot-inlay-hints-mode -1)))

  (add-hook 'eglot-managed-mode-hook #'my-eglot-lightweight-settings)

  (add-to-list 'eglot-server-programs
               '(tex-mode "texlab"))
  (add-to-list 'eglot-server-programs
               '(c-mode "ccls"))

  (defun eglot-ccls-inheritance-hierarchy (&optional derived)
    "Show inheritance hierarchy for the thing at point.
If DERIVED is non-nil (interactively, with prefix argument), show
the children of class at point."
    (interactive "P")
    (if-let* ((res (jsonrpc-request
                    (eglot--current-server-or-lose)
                    :$ccls/inheritance
                    (append (eglot--TextDocumentPositionParams)
                            `(:derived ,(if derived t :json-false))
                            '(:levels 100) '(:hierarchy t))))
              (tree (list (cons 0 res))))
        (with-help-window "*ccls inheritance*"
          (with-current-buffer standard-output
            (while tree
              (pcase-let ((`(,depth . ,node) (pop tree)))
                (cl-destructuring-bind (&key uri range) (plist-get node :location)
                  (insert (make-string depth ?\ ) (plist-get node :name) "\n")
                  (make-text-button (+ (line-beginning-position 0) depth) (line-end-position 0)
                                    'action (lambda (_arg)
                                               (interactive)
                                               (find-file (eglot--uri-to-path uri))
                                               (goto-char (car (eglot--range-region range)))))
                  (cl-loop for child across (plist-get node :children)
                           do (push (cons (1+ depth) child) tree)))))))
      (eglot--error "Hierarchy unavailable")))
  )
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
        lsp-log-io nil
        lsp-keep-workspace-alive nil
        lsp-idle-delay 0.8
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-enable-folding nil
        lsp-enable-imenu nil
        lsp-enable-file-watchers nil
        lsp-eldoc-enable-hover nil
        lsp-semantic-tokens-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; (LaTeX-mode      . lsp-deferred)
         (rust-mode       . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (tuareg-mode     . lsp-deferred)
         (js-mode         . lsp-deferred)
         (racket-mode     . lsp-deferred)
         (julia-mode      . lsp-deferred)
         ;; (c-mode . lsp-deferred)
         (python-mode     . lsp-deferred)
         (java-mode       . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  ;; Use flymake like eglot; avoids flycheck "no checker" noise.
  (setq lsp-diagnostics-provider :flymake)
  ;; Keep modeline updates quiet as well; diagnostics remain available through
  ;; Flycheck when it is enabled.
  (with-eval-after-load 'lsp-modeline
    (setq lsp-modeline-code-actions-enable nil
          lsp-modeline-diagnostics-enable nil
          lsp-modeline-workspace-status-enable nil))

  (defun my-lsp-lightweight-settings ()
    "Reduce point/change-triggered UI work in LSP buffers."
    (setq-local eldoc-idle-delay 1.0
                company-idle-delay (if (my-large-file-p) nil 0.4)
                company-backends '(company-capf)))

  (add-hook 'lsp-mode-hook #'my-lsp-lightweight-settings)

  (use-package lsp-pyright
    :ensure t
    :custom (lsp-pyright-langserver-command "pyright")) ;; or basedpyright
  (add-to-list 'lsp-disabled-clients 'semgrep-ls)
  (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("/usr/bin/jdtls"))
      :activation-fn (lsp-activate-on "java")
      :server-id 'jdtls-system))
  (use-package lsp-java
    ;; The system jdtls client registered above is used instead.  Keep this
    ;; optional package from producing a startup error when it is not installed.
    :disabled
    :ensure t)
  (use-package lsp-jedi
    :ensure t)
  (use-package ccls
    :ensure t)
  (use-package lsp-julia
    :ensure t
    :config
    (setq lsp-julia-default-environment "~/.julia/environments/v1.11")
    ))

(use-package lsp-ui :ensure t :commands lsp-ui-mode :after lsp
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-update-mode 'point))
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list :after lsp)

;; optionally if you want to use debugger
(use-package dap-mode :ensure t :after lsp)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(defun my-enable-flymake ()
  "Enable Flymake unless the current buffer is remote or large."
  (my-enable-unless-large-file #'flymake-mode))

(use-package flymake
  :hook (prog-mode . my-enable-flymake)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error))
  :defer t
  :init
  ;; Eglot already waits before sending document changes.  Avoid starting a
  ;; second syntax-check timer almost immediately after every edit.
  (setq flymake-no-changes-timeout 1.0)
  :config
  (use-package flymake-diagnostic-at-point
    :ensure t
    :after flymake
    :hook (flymake-mode . flymake-diagnostic-at-point-mode)))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook (sh-mode . flymake-shellcheck-load))

;; Prefer flymake (eglot/lsp). Keep flycheck available for M-x / language addons,
;; but do not enable it globally — that spams "no checker" on every buffer.
(use-package flycheck
  :ensure t
  :defer t
  :commands (flycheck-mode flycheck-list-errors)
  :config
  (setq flycheck-check-syntax-automatically '(save)
        flycheck-idle-change-delay 2.0)
  (use-package flycheck-pos-tip
    :ensure t
    :if (display-graphic-p)
    :hook (flycheck-mode . flycheck-pos-tip-mode)))
(use-package flycheck-ocaml :defer t)
(use-package flycheck-mypy :defer t)

;; rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (defun my-enable-rainbow-delimiters ()
    (unless (derived-mode-p 'emacs-lisp-mode)
      (my-enable-unless-large-file #'rainbow-delimiters-mode))))
;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-auto-revert-mode nil
        vc-handled-backends '(Git)))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

;; for mercurial
(use-package monky
  :ensure t
  :defer t
  :commands monky-status
  :config
  (setq monky-process-type 'cmdserver))

;; for mercurial
(use-package ahg
  :ensure t
  :defer t)

;; racket
(use-package racket-mode
  :mode "\\.rkt\\'")

;; julia
(use-package julia-mode
  :mode "\\.jl\\'")

;; c
(use-package cc-mode
  :defer t
  :config (setq c-basic-offset 4)
  :hook
  (asm-mode . (lambda () (setq-default indent-tabs-mode t)))
  (java-mode . (lambda () (setq c-basic-offset 4
                                tab-width 4
                                indent-tabs-mode nil)))
  (c-mode . (lambda () (setq c-basic-offset 4
                             tab-always-indent 0
                             c-auto-newline t
                             indent-tabs-mode nil)))
  (sh-mode . (lambda () (setq sh-basic-offset 4
                              indent-tabs-mode nil))))

;; sml
(use-package sml-mode
  :mode ("\\.sml\\'" "\\.sig\\'"))

;; ocaml
;; ocaml
(use-package tuareg
  :disabled
  :init
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'tuareg-mode-hook #'utop-minor-mode)
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?\\'" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\`dune\\'" . dune-mode))
  :config
  (setq tuareg-match-patterns-aligned t
        tuareg-highlight-all-operators t))

(use-package ocp-indent
  :disabled
  :after tuareg
  :config
  (add-hook 'tuareg-mode-hook #'ocp-setup-indent))

(use-package merlin
  :disabled ;; enable when lsp-ocaml is disabled
  :after tuareg
  :config
  (setq merlin-error-after-save nil)
  (flycheck-ocaml-setup)
  (use-package merlin-company
    :config
    (add-to-list 'company-backends #'merlin-company-backend)))

(use-package merlin-eldoc
  :disabled
  :after merlin)

(use-package ocamlformat
  :after tuareg)


(use-package dune
  :mode (("\\`dune\\'" . dune-mode)
         ("\\`dune-project\\'" . dune-mode)))

(use-package utop
  :commands (utop utop-minor-mode)
  :config
  (setq utop-command "opam exec -- utop -emacs"))


;; LaTeX
(defun my-synctex-forward-search ()
  "Jump from the TeX source at point to the matching PDF location."
  (interactive)
  (require 'pdf-tools)
  (require 'pdf-occur)
  (pdf-tools-install :no-query)
  (require 'pdf-sync)
  (TeX-pdf-tools-sync-view))

(defun my-synctex-backward-search ()
  "Jump from the current PDF view to the matching TeX source."
  (interactive)
  (require 'pdf-sync)
  (pdf-util-assert-pdf-window)
  (let* ((size (pdf-view-image-size))
         (x (/ (float (car size)) 2))
         (y (+ (or (window-vscroll nil t) 0)
               (/ (float (window-body-height nil t)) 2))))
    (pdf-sync-backward-search x y)))

(defun my-synctex-search ()
  "Forward search from TeX, or backward search from a PDF buffer."
  (interactive)
  (cond
   ((derived-mode-p 'pdf-view-mode)
    (my-synctex-backward-search))
   ((derived-mode-p 'TeX-mode 'tex-mode 'LaTeX-mode 'latex-mode)
    (my-synctex-forward-search))
   (t
    (user-error "SyncTeX works only in TeX or PDF buffers"))))

(keymap-global-set "C-c C-g" #'my-synctex-search)

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook ((pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
         (pdf-tools-enabled . auto-revert-mode))
  :config
  (require 'pdf-occur)
  (pdf-tools-install :no-query)
  (require 'pdf-sync)
  (setq mouse-wheel-follow-mouse t
        pdf-view-resize-factor 1.10)
  (keymap-set pdf-view-mode-map "C-c C-g" #'my-synctex-backward-search)
  (keymap-set pdf-sync-minor-mode-map "C-c C-g" #'my-synctex-backward-search)
  (define-key pdf-sync-minor-mode-map [mouse-3] #'pdf-sync-backward-search-mouse)
  (define-key pdf-sync-minor-mode-map [double-mouse-1] #'pdf-sync-backward-search-mouse)
  (define-key pdf-sync-minor-mode-map [C-mouse-1] #'pdf-sync-backward-search-mouse))

(use-package languagetool
  :ensure t
  :defer t
  :config
  (keymap-global-set "C-c l c" #'languagetool-check)
  (keymap-global-set "C-c l d" #'languagetool-clear-buffer)
  (keymap-global-set "C-c l p" #'languagetool-correct-at-point)
  (keymap-global-set "C-c l b" #'languagetool-correct-buffer)
  (keymap-global-set "C-c l l" #'languagetool-set-language)

  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command "~/.languagetool/languagetool-commandline.jar"
        languagetool-server-command "~/.languagetool/languagetool-server.jar"))

(defun my-auctex-latexmk-rc-option ()
  "Return a latexmk option for the nearest project-local .latexmkrc.
Search upward from the AUCTeX master file directory so a project rc file
above a nested TeX master is still honored."
  (or (when-let* ((master-directory (TeX-master-directory))
                  (project-directory
                   (locate-dominating-file master-directory ".latexmkrc"))
                  (rc-file
                   (expand-file-name ".latexmkrc" project-directory)))
        (format "-r %s " (shell-quote-argument rc-file)))
      ""))

(defun my-auctex-cont-latexmk-use-project-rc (command)
  "Make continuous latexmk COMMAND honor the project-local rc file."
  (let ((option (my-auctex-latexmk-rc-option)))
    (if (string= option "")
        command
      (let ((without-pdf
             (replace-regexp-in-string
              "[[:space:]]+-pdf\\(?:[[:space:]]+\\|\\'\\)" " " command)))
        (replace-regexp-in-string
         "\\`latexmk\\(?:[[:space:]]+\\)?"
         (concat "latexmk " option)
         without-pdf t t)))))

(defun my-auctex-latexmk-engine-option ()
  "Return an AUCTeX engine option unless a project rc file controls it."
  (if (not (string= (my-auctex-latexmk-rc-option) ""))
      ""
    (cond
     ((and (eq TeX-engine 'default)
           TeX-PDF-mode
           auctex-latexmk-inherit-TeX-PDF-mode)
      "-pdf ")
     ((and (eq TeX-engine 'xetex)
           TeX-PDF-mode
           auctex-latexmk-inherit-TeX-PDF-mode)
      "-pdf -pdflatex=xelatex ")
     ((eq TeX-engine 'xetex) "-xelatex ")
     ((eq TeX-engine 'luatex) "-lualatex ")
     (t ""))))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook #'display-fill-column-indicator-mode)

  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t
        TeX-parse-self t
        TeX-auto-save t
        TeX-clean-confirm t
        TeX-PDF-mode t
        TeX-master t
        reftex-plug-into-AUCTeX t)
  (setq-default TeX-command-extra-options "-shell-escape")

  (TeX-source-correlate-mode 1)

  (keymap-set TeX-mode-map "C-c C-g" #'my-synctex-forward-search)
  (keymap-set TeX-source-correlate-map "C-c C-g" #'my-synctex-forward-search)
  (with-eval-after-load 'latex
    (keymap-set LaTeX-mode-map "C-c C-g" #'my-synctex-forward-search))

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  ;; Outline minor mode — extra outline headers
  (setq TeX-outline-extra
        '(("%chapter" 1)
          ("%section" 2)
          ("%subsection" 3)
          ("%subsubsection" 4)
          ("%paragraph" 5)))

  (font-lock-add-keywords
   'latex-mode
   '(("^%\\(chapter\\|\\(sub\\|subsub\\)?section\\|paragraph\\)"
      0 'font-lock-keyword-face t)
     ("^%chapter{\\(.*\\)}"       1 'font-latex-sectioning-1-face t)
     ("^%section{\\(.*\\)}"       1 'font-latex-sectioning-2-face t)
     ("^%subsection{\\(.*\\)}"    1 'font-latex-sectioning-3-face t)
     ("^%subsubsection{\\(.*\\)}" 1 'font-latex-sectioning-4-face t)
     ("^%paragraph{\\(.*\\)}"     1 'font-latex-sectioning-5-face t)))

  (use-package auctex-latexmk
    :ensure t
    :config
    (auctex-latexmk-setup)
    (setq auctex-latexmk-inherit-TeX-PDF-mode t)
    (add-to-list 'TeX-expand-list
                 '("%(latexmkrc)" my-auctex-latexmk-rc-option))
    (add-to-list 'TeX-expand-list
                 '("%(latexmk-engine)" my-auctex-latexmk-engine-option))
    (setf (nth 1 (assoc "LatexMk" TeX-command-list))
          "latexmk %(latexmkrc)%(latexmk-engine)%S%(mode) %(file-line-error) %(extraopts) %t"))

  (use-package company-auctex
    :init
    (company-auctex-init))

  (use-package reftex
    :hook (LaTeX-mode . reftex-mode)
    :config
    (setq reftex-section-levels
          (append '(("frametitle" . -3)) reftex-section-levels)
          reftex-cite-prompt-optional-args t
          reftex-plug-into-AUCTeX t))

  (use-package auctex-cont-latexmk
    :bind (:map LaTeX-mode-map
                ("C-c k" . auctex-cont-latexmk-toggle))
    :config
    (advice-add 'auctex-cont-latexmk--compilation-command :filter-return
                #'my-auctex-cont-latexmk-use-project-rc)))

;; lua
(use-package lua-mode
  :mode "\\.lua\\'")

;; scala
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)\\'")

(use-package sbt-mode
  :commands (sbt-start sbt-command)
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Java build tool
(use-package gradle-mode
  :commands gradle-mode
  :config
  (use-package groovy-mode
    :mode "\\.groovy\\'"))

(use-package nxml-mode
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.xls\\'" . nxml-mode))
  :config
  (setq nxml-child-indent 2
        nxml-attribute-indent 2
        nxml-slash-auto-complete-flag t))

(use-package flycheck-gradle
  :defer t)

;; JavaScript
(use-package js2-mode
  :mode "\\.js\\'")

(use-package peg
  :mode "\\.\\(pegjs\\|peg\\)\\'")

;; Typescript
(use-package typescript-mode
  :mode "\\.tsx?\\'")

;; haskell
(use-package haskell-mode
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . haskell-mode)))

;; gnuplot
(use-package gnuplot
  :mode ("\\.plot\\'" . gnuplot-mode))

;; PHP
(use-package php-mode
  :mode "\\.php\\'")

;; Python
(use-package python
  :defer t
  :config
  (defun python-pytest ()
    (interactive)
    (if (and (string-match
              (rx bos "test_")
              (file-name-nondirectory (buffer-file-name)))
             (string-suffix-p ".py" (file-name-nondirectory (buffer-file-name))))
        (let (input)
          (while (not input)
            (setq input (read-string "-k:")))
          (let (cmd)
            (if (string= "" input)
                (setq cmd "py.test --color=no -rP ")
              (setq cmd (concat "py.test --color=no -rP -k " input " ")))
            (compile (concat cmd (buffer-file-name)))))
      (message "Not a pytest file"))))

(use-package ein
  :ensure t
  :defer t
  :config
  (setq ein:worksheet-enable-undo t))
(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))
;; R
(use-package ess
  :ensure t
  :defer t)

(use-package ess-view
  :ensure t
  :after (ess))

(use-package ess-R-data-view
  :ensure t
  :after (ess))

;; smalltalk
(use-package smalltalk-mode
  :ensure t
  :mode ("\.som$" . smalltalk-mode)
  )


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



;;;; ==== init-org ====


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


;;;; ==== init-mail ====

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


;;; config.el ends here
