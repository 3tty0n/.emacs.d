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
  (progn
    (add-to-list 'default-frame-alist '(font . "Fira Code Medium-11"))
    (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Noto Sans CJK JP"))))

(with-system darwin
  (if (display-graphic-p)
      (set-face-attribute 'default nil
                          :family "Fira Code" :height 150 :weight "Retina")))

(provide 'my-font)
;;; my-font.el ends here
