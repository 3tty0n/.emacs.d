;;; font-config.el -- font configuration for me.
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
                (set-frame-font "Fira Code Retina-14" nil t)))))

(provide 'my-font)
;;; my-font.el ends here
