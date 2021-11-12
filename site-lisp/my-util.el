;;; my-util.el -- Utility functions for me.
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
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))

(set-alpha 95)

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
                     (cons 'height (/ (- (x-display-pixel-height) 200)
                                      (frame-char-height)))))))

(set-frame-size-according-to-resolution)

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
  "Save the current region to the kill ring after stripping extra whitespace and new lines"
  (interactive "r")
  (copy-region-as-kill beg end)
  (with-temp-buffer
    (yank)
    (goto-char 0)
    (while (looking-at "[ \t\n]")
      (delete-char 1))
    (compact-uncompact-block)
    (mark-whole-buffer)
    (kill-region (point-min) (point-max))))

(provide 'my-util)
;;;  my-util.el ends here
