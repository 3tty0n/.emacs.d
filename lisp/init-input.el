;;; init-input.el --- Japanese input -*- lexical-binding: t; -*-

(require 'init-core)

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

(provide 'init-input)
;;; init-input.el ends here
