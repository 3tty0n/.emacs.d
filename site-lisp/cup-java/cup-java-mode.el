;; -*- Mode: Emacs-Lisp; -*-
;; CUP Parser Generator for Java -- Emacs mode
;; Copyright (C) 2020 by Pierre-Antoine Rouby <contact@parouby.fr>

(setq cup-java-highlights
      `(("\\(\\/\\*\\)\\(.*\\)\\(\\*\\/\\)" . font-lock-comment-face)
        ("\\(\\/\\/\\)\\(.*\\)$"            . font-lock-comment-face)
        (,(string-join '("package" "import" "terminal" "non terminal"
                         "precedence \\(left\\|right\\)" "start with" "parser code"
                         "new" "private" "public" "protected")
                       "\\|")
         . font-lock-keyword-face)
        ("\\(package\\|import\\) \\(.*\\)\\.\\(.*\\);" . (2 font-lock-constant-face))
        ("\\(package\\|import\\) \\(.*\\)\\.\\(.*\\);" . (3 font-lock-type-face))
        ("\\([A-Z_]+\\)[ \\\n,;:)]"         . (1 font-lock-constant-face))
        ("[ \\\t(]\\([A-Z][a-zA-Z_]+\\)"    . (1 font-lock-type-face))
        (":\\([a-z][a-z0-9]*\\)"            . (1 font-lock-variable-name-face))
        ("\\(.*\\)::="                      . (1 font-lock-function-name-face))))

(define-derived-mode cup-java-mode fundamental-mode "cup-java"
  "major mode for editing cup-java language code."
    (setq font-lock-defaults '(cup-java-highlights)))

(provide 'cup-java-mode)
