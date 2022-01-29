;;; font-config.el -- font configuration for me.
;;; Commentary:
;;;
;;; Code:

(if (string-equal system-type "gnu/linux")
    (progn
      (add-to-list 'default-frame-alist '(font . "Fira Code Retina-9"))
      (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Noto Sans CJK JP"))))

(provide 'my-font)
;;; my-font.el ends here
