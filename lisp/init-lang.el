;;; init-lang.el --- Language modes -*- lexical-binding: t; -*-

(require 'init-core)

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

(provide 'init-lang)
;;; init-lang.el ends here
