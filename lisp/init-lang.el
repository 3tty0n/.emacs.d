;;; init-lang.el --- Language modes -*- lexical-binding: t; -*-

(require 'init-core)

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
